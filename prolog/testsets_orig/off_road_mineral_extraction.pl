% ============================================================================
% CONSTRAINT STORY: off_road_mineral_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_off_road_mineral_extraction, []).

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
 *   constraint_id: off_road_mineral_extraction
 *   human_readable: Off-Road Mineral Extraction Constraint System
 *   domain: environmental/economic/political
 *
 * SUMMARY:
 *   Off-road mineral extraction represents a structural constraint that
 *   asymmetrically distributes extraction costs to powerless agents
 *   (indigenous communities, ecosystems, future generations) while
 *   concentrating benefits to institutional actors (mining corporations,
 *   industrial supply chains). The constraint operates through multiple
 *   suppression mechanisms: legal frameworks that deny land sovereignty,
 *   technological lock-in that makes mineral intensity appear inevitable,
 *   epistemic suppression that naturalizes extraction as development cost,
 *   and temporal suppression that discounts future harms. The theater ratio
 *   (0.58) reflects the performative nature of environmental regulation —
 *   impact assessments and monitoring create appearance of control while
 *   extraction proceeds unchanged. The extractiveness trajectory (0.42→0.68
 *   over 60 years) shows accumulation of extraction as open-access frontier
 *   depletion accelerates extraction speed. This constraint exhibits all
 *   snare signatures: high base extractiveness (0.68), high suppression
 *   (0.72), asymmetric coercion (powerless agents cannot exit), and minimal
 *   coordination function (the system does not solve a collective action
 *   problem; it imposes a unilateral distribution).
 *
 * KEY AGENTS:
 *   - Indigenous Land Communities: Primary victims (powerless/trapped) — structurally excluded from land rights; bear environmental and livelihood costs; no exit options
 *   - Mining Corporations: Primary beneficiaries (institutional/arbitrage) — capture resource rents and supply chain integration; full exit options through arbitrage between jurisdictions
 *   - Ecosystem Integrity: Structural victim (powerless/trapped) — abstract collective good; cannot organize or advocate; bears extraction through habitat loss and pollution
 *   - Future Generations: Temporal victims (powerless/identity_locked) — not yet constituted as agents; trapped by irreversibility of mineral extraction; discount rates render their interests invisible
 *   - Mining-Dependent Governments: Secondary institutional actor (moderate/constrained) — benefit from tax revenue; constrained by capital mobility threat; face sovereignty erosion through extraction leverage
 *   - Environmental Regulators: Degraded institutional mechanism (organized/constrained) — maintain performative assessment and monitoring; captured by industry proximity; constrain through theater rather than enforcement
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — identifies the constraint as systemic extraction with manufactured inevitability framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(off_road_mineral_extraction, 0.68).
domain_priors:suppression_score(off_road_mineral_extraction, 0.72).
domain_priors:theater_ratio(off_road_mineral_extraction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(off_road_mineral_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(off_road_mineral_extraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(off_road_mineral_extraction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(off_road_mineral_extraction, snare).
narrative_ontology:human_readable(off_road_mineral_extraction, "Off-Road Mineral Extraction Constraint System").
narrative_ontology:topic_domain(off_road_mineral_extraction, "environmental/economic/political").

domain_priors:requires_active_enforcement(off_road_mineral_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(off_road_mineral_extraction, mining_corporations).
narrative_ontology:constraint_beneficiary(off_road_mineral_extraction, industrial_supply_chains).
narrative_ontology:constraint_victim(off_road_mineral_extraction, indigenous_land_communities).
narrative_ontology:constraint_victim(off_road_mineral_extraction, ecosystem_integrity).
narrative_ontology:constraint_victim(off_road_mineral_extraction, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS LAND COMMUNITIES (SNARE) — Structurally trapped. Face extraction through resource depletion, environmental degradation, and dispossession. Cannot exit because land rights are securitized by colonial and post-colonial legal frameworks. No meaningful participation in extraction decisions. Bear full cost of ecological collapse and livelihood destruction. Maximum suppression through legal exclusion and economic dependency.
constraint_indexing:constraint_classification(off_road_mineral_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ECOSYSTEM INTEGRITY (SNARE) — Cannot organize, cannot advocate, cannot exit. Bears extraction through habitat loss, pollution, species extinction, and carbon release. Trapped by the asymmetry between extraction speed and ecosystem recovery timescales. Suppression through naturalization: extraction portrayed as inevitable cost of development.
constraint_indexing:constraint_classification(off_road_mineral_extraction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: FUTURE GENERATIONS (SNARE) — Structurally mobile (could theoretically organize now if temporally connected) but identity-locked: their identity is not yet constituted, their voice cannot speak, their exit options are literally nonexistent in present decision-making. Trapped by temporal displacement and the irreversibility of mineral extraction. Suppression through discount rates and present-bias framing that renders future costs invisible.
constraint_indexing:constraint_classification(off_road_mineral_extraction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 4: MINING CORPORATIONS (ROPE) — Institutional beneficiary with full arbitrage options. Experience the constraint as pure coordination: permits structure access, supply chains integrate extraction, markets price minerals. No perceived extraction — the system coordinates their interests perfectly. Can exit individual projects but not the institutional role of extraction. Sees constraint as mutually beneficial coordination mechanism.
constraint_indexing:constraint_classification(off_road_mineral_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MINING-DEPENDENT NATIONAL GOVERNMENTS (TANGLED ROPE) — Constrained by revenue dependency and global capital mobility. Genuinely coordinate resource governance (permit allocation, infrastructure development) but also extract rents through taxation and labor agreements. Face career/sovereignty risk of restricting extraction. Mixed experience: benefit from coordination function but also trapped by the extraction leverage that global corporations hold.
constraint_indexing:constraint_classification(off_road_mineral_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ENVIRONMENTAL REGULATION SYSTEMS (PITON) — Degraded institutional mechanism. Environmental impact assessments, permitting processes, and monitoring are largely performative theater masking continued extraction. Regulatory capture by industry. High theater ratio (0.58) reflects gap between regulatory rhetoric (environmental protection) and actual function (permitting extraction). Regulations persist through institutional inertia despite failure to prevent ecosystem degradation.
constraint_indexing:constraint_classification(off_road_mineral_extraction, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational scale, the constraint is pure extraction of nonrenewable resources with suppression of alternatives. The suppression operates through: (1) technological lock-in (industrial supply chains optimized around mineral-intensive production), (2) institutional capture (regulatory agencies aligned with industry), (3) epistemic suppression (internalized belief that extraction is inevitable for development), (4) future suppression (irreversibility of ore extraction makes alternatives unthinkable after extraction occurs). Mandatrophy resolved: the system exhibits all snare signatures despite attempts to frame it as responsible development.
constraint_indexing:constraint_classification(off_road_mineral_extraction, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(off_road_mineral_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(off_road_mineral_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(off_road_mineral_extraction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(off_road_mineral_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(off_road_mineral_extraction, TR),
    TR >= 0.70.

:- end_tests(off_road_mineral_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but not maximal. Mining corporations extract substantial economic value, but not all value; indigenous communities and ecosystems lose total value. The 0.68 reflects that extraction concentrates enough value to drive the system despite obvious alternatives. Suppression (0.72): High. Legal exclusion of indigenous land claims, technological lock-in of mineral-intensive supply chains, epistemic suppression (internalized belief that extraction is necessary for development), and temporal suppression (future costs discounted) create multiple, reinforcing barriers to exit. No single suppression mechanism is total, but together they are nearly insurmountable. Theater ratio (0.58): Moderate-high. Environmental impact assessments, community consultation processes, and monitoring systems create performative appearance of control and responsible extraction while actual ecosystem degradation continues unchecked. The theater has increased over the 60-year interval as environmental movements have forced more elaborate compliance theater. The extractiveness trajectory shows accumulation: early extraction (0.42) was contained by frontier abundance and weak global demand; current extraction (0.68) reflects saturation of accessible deposits and acceleration of open-access extraction rates.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the full spectrum from Rope (beneficiary) to Snare (victim) to Piton (degraded regulator). Mining corporations experience the constraint as Rope — it purely coordinates their access and supply chains. Mining-dependent governments experience Tangled Rope — they benefit from tax revenue but are constrained by extraction dependency and capital flight threats. Environmental regulators experience Piton — their assessment and monitoring processes are performative theater masking continued extraction. Indigenous communities and ecosystems experience Snare — pure extraction with no coordination benefit and no exit options. Future generations experience Snare from an identity-locked position — they cannot yet speak but will bear irreversible consequences. The analytical observer sees Snare at civilizational scale — the system exhibits all signatures of extractive constraint with suppression of alternatives. This perspectival diversity (Rope→Tangled Rope→Piton→Snare→Snare) is the diagnostic signature of a genuinely asymmetric constraint, not a perspective-dependent phenomenon.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation chain prioritizes the structural relationship (beneficiary/victim) and exit options to compute experienced extractiveness. Indigenous communities are declared victims with trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high experienced chi. Ecosystems are abstract victims with no exit → d = 1.0 → maximum f(d). Mining corporations are beneficiaries with arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative experienced extraction. Mining-dependent governments are mixed: partially beneficiary (tax revenue) and partially victim (sovereignty erosion); they are constrained (not fully mobile) → d ≈ 0.50 → f(d) ≈ 0.65 → moderate experienced extraction. Scope modifier σ(S) = 1.1 for continental-scale extraction amplifies extractiveness further. The perspectival gap reveals itself: beneficiaries see coordination (Rope from corporate perspective), while victims see pure extraction (Snare from powerless perspective). This gap is diagnostic — if all perspectives produced the same classification, the directionality derivation would be suspect.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint classifies as Snare despite attempts to frame it as responsible development. The mandatrophy resolution required showing: (1) HIGH BASE EXTRACTIVENESS (0.68) — economic value flows asymmetrically to mining corporations, not to affected communities or future ecosystem users. (2) HIGH SUPPRESSION (0.72) — exit barriers (legal exclusion, technological lock-in, epistemic suppression, temporal discounting) are structural, not contingent. (3) MINIMAL COORDINATION FUNCTION — the constraint does not solve a shared collective action problem; it imposes a unilateral extraction distribution. (4) MULTIPLE SNARE PERSPECTIVES — not just one victim perceives extraction, but multiple independent victims (indigenous communities, ecosystems, future generations) experience the same asymmetry. The performative theater (environmental assessment, consultation processes, monitoring) initially suggested Piton (degraded function), but the underlying extractiveness and suppression metrics confirm Snare: the theater masks ongoing extraction rather than failing to achieve a genuine coordination goal. Extractiveness > 0.70 triggers mandatrophy requirement; resolution confirms that suppression is structural (not contingent on jurisdiction or corporate choice) and that alternative paths (circular economy, substitution, localization) are artificially suppressed rather than genuinely infeasible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ecosystem_recovery_velocity,
    'Can ecosystem recovery timescales match or exceed mineral extraction timescales at global scale?',
    'Bioaccumulation studies, soil remediation duration measurements, species recolonization timelines, hydrological restoration data from closed mines',
    'If recovery << extraction speed: constraint is permanent Snare (irreversible extraction asymmetry). If recovery ≈ extraction speed: constraint could be Tangled Rope with long regeneration cycle. If recovery >> extraction speed: frame collapses entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ecosystem_recovery_velocity, empirical, 'Comparative timescales of ecosystem recovery vs mineral extraction').

omega_variable(
    technological_substitution_feasibility,
    'What proportion of mineral-intensive industrial supply chains could transition to circular/alternative material pathways without catastrophic efficiency loss?',
    'Material science analysis of substitutes (graphene vs rare earths, bio-based polymers vs mineral plastics), economic modeling of transition costs, historical analogues (lead phase-out, CFC replacement)',
    'If < 20% substitutable: extraction is structurally necessary (Snare justified by developmental necessity). If > 60% substitutable: suppression is artificial choice (Snare becomes policy rather than structural). If > 80% substitutable: the Snare is revealed as manufactured scarcity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_substitution_feasibility, empirical, 'Feasibility of material substitution and circular economy transitions').

omega_variable(
    indigenous_land_rights_enforcement,
    'Under what legal/institutional conditions do indigenous land claims actually prevent extraction (vs performative consultation)?',
    'Comparative analysis of cases with binding consent requirements vs advisory consultation; correlation between legal recognition level and extraction prevention; international court decisions on land sovereignty',
    'If no legal condition prevents extraction: indigenous communities are fully trapped (Snare confirmed). If binding consent mechanisms exist: exit options upgrade from trapped to constrained or mobile (Snare potentially becomes Tangled Rope or Rope). If full sovereignty recognized: constraint becomes bilateral (two separate institutional perspectives, not one power asymmetry).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_land_rights_enforcement, empirical, 'Whether indigenous land rights can structurally prevent extraction').

omega_variable(
    regulatory_capture_mechanism,
    'Is environmental regulation capture a contingent institutional failure or a structural feature of extractive industries?',
    'Historical analysis of regulatory drift post-capture; cross-sector comparison (mining vs oil vs forestry); analysis of whether industry funding of regulators is necessary or contingent',
    'If structural: Piton perspective is permanent degradation (constraint morphs to Snare as regulation fails). If contingent: regulatory reform could upgrade Piton to functional constraint type. If institutional design flaw: new governance models (indigenous co-management, third-party monitoring) could shift classification entirely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, conceptual, 'Whether regulatory capture is inherent to extractive industries').

omega_variable(
    global_supply_chain_decoupling,
    'What happens to extraction demand if global supply chains decouple (localization, deglobalization, consumption reduction)?',
    'Scenario analysis of demand reduction (5%, 25%, 50%); measurement of actual decoupling trends; economic modeling of circular economy adoption rates',
    'If demand inelastic to decoupling: extraction is driven by fundamental needs (Snare structural). If demand elastic: extraction volume is contingent on supply chain choice (Snare becomes Tangled Rope with policy exit). If demand collapsible: the entire constraint can be unwound through economic reorganization (Snare dissolves into Rope or Scaffold).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(global_supply_chain_decoupling, preference, 'Elasticity of extraction demand to global supply chain reorganization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(off_road_mineral_extraction, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(offroad_tr_t0, off_road_mineral_extraction, theater_ratio, 0, 0.32).
narrative_ontology:measurement(offroad_tr_t20, off_road_mineral_extraction, theater_ratio, 20, 0.45).
narrative_ontology:measurement(offroad_tr_t40, off_road_mineral_extraction, theater_ratio, 40, 0.58).
narrative_ontology:measurement(offroad_tr_t60, off_road_mineral_extraction, theater_ratio, 60, 0.65).

% Extraction over time
narrative_ontology:measurement(offroad_be_t0, off_road_mineral_extraction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(offroad_be_t20, off_road_mineral_extraction, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(offroad_be_t40, off_road_mineral_extraction, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(offroad_be_t60, off_road_mineral_extraction, base_extractiveness, 60, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(off_road_mineral_extraction, resource_allocation).
narrative_ontology:boltzmann_floor_override(off_road_mineral_extraction, 0.12).
narrative_ontology:affects_constraint(off_road_mineral_extraction, indigenous_land_sovereignty).
narrative_ontology:affects_constraint(off_road_mineral_extraction, supply_chain_mineral_dependency).
narrative_ontology:affects_constraint(off_road_mineral_extraction, ecosystem_remediation_failure).

% DUAL FORMULATION NOTE:
% Off-road mineral extraction decomposes into three structurally linked constraints: (1) indigenous_land_sovereignty (ε≈0.45, Tangled Rope) — genuine coordination of land use with embedded extraction of sovereignty rights; (2) supply_chain_mineral_dependency (ε≈0.55, Tangled Rope) — supply chains coordinate production with embedded technological lock-in; (3) ecosystem_remediation_failure (ε≈0.72, Snare) — environmental systems extractively consume without regeneration. The present story represents the unified system constraint at the institutional level (mining corporations + governments + regulators). Upstream constraints have lower ε (focused on specific mechanisms); this story integrates all mechanisms and shows how they reinforce extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(off_road_mineral_extraction, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
