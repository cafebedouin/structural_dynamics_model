% ============================================================================
% CONSTRAINT STORY: paxsilica_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paxsilica_framework, []).

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
 *   constraint_id: paxsilica_framework
 *   human_readable: PaxSilica AI and Silicon Governance Framework
 *   domain: geopolitical/technological
 *
 * SUMMARY:
 *   PaxSilica is a proposed international coordination framework for managing
 *   artificial intelligence development and the silicon chips required for
 *   frontier AI systems. Announced by the US as a mechanism for coordinating
 *   with allies on export controls, research standards, and verification
 *   protocols, the framework presents as a classic coordination problem:
 *   preventing an arms race in AI while maintaining unified standards and
 *   preventing defection. However, the structural data reveals a tangled_rope
 *   constraint — genuine coordination benefits for allied nations are
 *   inseparable from asymmetric extraction of non-aligned states. The
 *   constraint demonstrates mandatrophy: framed as multilateral governance,
 *   it operates through unilateral US chip export authority. The
 *   theater_ratio (0.62) reflects the performative multilateral institutions
 *   (UN discussions, international ethics boards, consensus-based standards
 *   committees) that legitimize what is fundamentally a hegemonic control
 *   mechanism. Non-aligned nations experience pure extraction (snare): they
 *   can either accept the framework's terms or develop autonomous chip
 *   fabrication at 5-10x cost over 15-20 years. Allied nations experience
 *   mixed benefits and constraints (tangled_rope): coordination gains are
 *   real, but constrained market access and subordinated autonomy exact a
 *   cost. Alternative chip ecosystems represent a genuine sunset path
 *   (scaffold): if China achieves 7nm fabrication at scale and open-source AI
 *   models converge toward closed-model capability, the extraction mechanism
 *   weakens. The analytical observer risks naturalizing the constraint as
 *   inevitable ('technological concentration is structural'), but the high
 *   theater_ratio and active political enforcement reveal that PaxSilica's
 *   governance form is contingent, not natural law.
 *
 * KEY AGENTS:
 *   - US Government and Administration: Primary beneficiary (institutional/arbitrage) — maintains hegemonic control over global AI development through unilateral chip export authority; benefits from unified alliance coordination
 *   - Non-Aligned Nations and Competing AI Ecosystems: Primary victim (powerless/trapped) — locked out of cutting-edge chip access; must choose between expensive autonomous development or accepting framework terms; includes China, Russia, India, Iran, unaligned African and Southeast Asian states
 *   - Allied Governments (EU, Japan, South Korea, Taiwan, Australia): Secondary beneficiary with extraction (organized/constrained) — gain coordination benefits and market access but constrained in autonomous policy and trade freedom; includes EU as distinct strategic actor
 *   - Western AI Research Institutions and Companies: Beneficiary (institutional/arbitrage) — access to unified standards, export protection, and state-backed research funding; includes OpenAI, Anthropic, Google, Meta, Microsoft
 *   - Allied Semiconductor Manufacturers: Mixed (moderate/mobile) — TSMC, Samsung, Intel benefit from unified standards and protected markets but face constrained sales to non-aligned states and US-directed fab location requirements
 *   - Alternative Chip Development Programs: Victim with exit path (organized/constrained) — China's SMIC and Loongson, India's semiconductor initiatives represent scaffold perspective; building alternative pathways with 10-20 year sunset
 *   - International Governance Institutions: Performative (institutional/arbitrage) — UN committees, World Economic Forum discussions, international AI ethics boards maintain appearance of multilateralism while actual authority derives from unilateral US enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paxsilica_framework, 0.58).
domain_priors:suppression_score(paxsilica_framework, 0.68).
domain_priors:theater_ratio(paxsilica_framework, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paxsilica_framework, extractiveness, 0.58).
narrative_ontology:constraint_metric(paxsilica_framework, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(paxsilica_framework, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paxsilica_framework, tangled_rope).
narrative_ontology:human_readable(paxsilica_framework, "PaxSilica AI and Silicon Governance Framework").
narrative_ontology:topic_domain(paxsilica_framework, "geopolitical/technological").

domain_priors:requires_active_enforcement(paxsilica_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paxsilica_framework, us_hegemonic_position).
narrative_ontology:constraint_beneficiary(paxsilica_framework, allied_semiconductor_manufacturers).
narrative_ontology:constraint_beneficiary(paxsilica_framework, western_ai_research_institutions).
narrative_ontology:constraint_victim(paxsilica_framework, non_aligned_nations).
narrative_ontology:constraint_victim(paxsilica_framework, competing_ai_research_ecosystems).
narrative_ontology:constraint_victim(paxsilica_framework, global_chip_supply_chain_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-ALIGNED NATIONS (SNARE) — Countries outside the US-led alliance (China, Russia, India, Iran, and unaligned states) cannot exit the framework without losing access to cutting-edge AI development infrastructure. No alternative silicon governance system exists. Trapped within the constraint: either accept export controls and verification regimes, or build AI infrastructure at dramatically higher cost with constrained access to leading-edge chips. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96. Pure extraction with no coordination benefit for this group.
constraint_indexing:constraint_classification(paxsilica_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALLIED GOVERNMENTS (TANGLED ROPE) — US allies (EU, Japan, South Korea, Taiwan, Australia) experience mixed coordination and extraction. Framework provides coordination benefit: unified standards reduce transaction costs for semiconductor supply, synchronized export control compliance, and shared AI research governance. But extraction exists: allied nations have constrained autonomy over their own AI and chip policies. EU must align with US strategic objectives; Japan and South Korea cannot independently export to non-aligned states; Taiwan's strategic position is crystallized by the framework. d≈0.58, f(d)≈0.72, σ=1.1 → χ≈0.46. Mixed extraction and coordination.
constraint_indexing:constraint_classification(paxsilica_framework, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: US GOVERNMENT AND WESTERN AI LEADERSHIP (ROPE) — The US and its directly-allied institutions experience the framework as pure coordination. It solves the collective action problem of maintaining technological lead while preventing rival AI development: chip export controls are synchronized, research standards are unified, and verification protocols are shared. Exit options are high (the US can unilaterally enforce controls; Intel, NVIDIA, and TSMC operate with high structural flexibility). d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.07. Net beneficiary; effective extraction is negative.
constraint_indexing:constraint_classification(paxsilica_framework, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL GOVERNANCE THEATER (PITON) — The framework performs multilateralism: United Nations discussions, international AI ethics boards, consensus-based technical standards committees. But the actual authority derives from unilateral US chip export controls and enforcement by allied governments. The multilateral institutions are largely performative — coordination theater maintained for legitimacy while extraction mechanisms operate through state power. theater_ratio=0.62 reflects significant performative content (international forums, consensus rhetoric) alongside functional control (export licenses, verification regimes). d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.03. Institutional actor sees own participation as maintenance of a degraded multilateral order.
constraint_indexing:constraint_classification(paxsilica_framework, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALLIED SEMICONDUCTOR MANUFACTURERS (TANGLED ROPE) — Companies like TSMC, Samsung, Intel experience mixed coordination and extraction. Coordination benefit: unified standards reduce fragmentation, synchronized export controls prevent free-riding by competitors, shared R&D roadmaps reduce duplicative investment. Extraction: constrained ability to sell to non-aligned markets, mandatory compliance with US verification protocols, pressure to locate fabs in allied territory. Exit options are mobile but not fully arbitrage — companies can relocate fabs or shift markets, but not without substantial cost. d≈0.48, f(d)≈0.62, σ=1.1 → χ≈0.40. Strong coordination function with meaningful extraction overlay.
constraint_indexing:constraint_classification(paxsilica_framework, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ALTERNATIVE ECOSYSTEMS (SCAFFOLD) — China's chip development programs (SMIC, Loongson), India's semiconductor initiatives, and other non-aligned AI research are building alternative pathways to reduce dependency on PaxSilica. This is not a permanent escape from the framework but a temporary support structure (scaffold) for emerging alternatives. The sunset is 10-20 years: if non-aligned states achieve 7nm chip fabrication at scale and open-source AI models mature sufficiently, the framework's extraction mechanism weakens. d≈0.68, f(d)≈1.08, σ=1.2 → χ≈0.76. High extraction now, but with visible sunset path. The constraint looks like a snare initially but is actually a temporary extraction mechanism (scaffold) because the exit path is becoming feasible.
constraint_indexing:constraint_classification(paxsilica_framework, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some coordination of high-end chip development may be inevitable: the capital requirements ($20B+ per fab), the concentration of expertise (Taiwan, South Korea), and the dual-use nature of advanced AI (civilian and military applications) create structural constraints on access that no policy framework can eliminate. This perspective sees PaxSilica as formalizing an underlying technological reality. However, the high theater_ratio (0.62) and evidence of active political enforcement contradict the mountain classification — the engine will compute this as a false summit, revealing that technological concentration is real but PaxSilica's *governance structure* is a contingent political choice.
constraint_indexing:constraint_classification(paxsilica_framework, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: EUROPEAN UNION (TANGLED ROPE) — The EU experiences the framework with particular tension. Coordination benefit: unified AI governance reduces fragmentation across 27 member states, synchronized export controls prevent free-riding, alignment with US technology standards maintains interoperability and market access. Extraction: EU digital sovereignty is subordinated to US strategic objectives, European chip manufacturers (limited) face restricted markets, European AI research is constrained by US export control on training data and models. Exit options are constrained but not trapped — EU can theoretically develop independent standards, but at cost of market fragmentation and US retaliation. d≈0.62, f(d)≈0.82, σ=1.0 → χ≈0.48. Significant perspectival gap from US institutional view.
constraint_indexing:constraint_classification(paxsilica_framework, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paxsilica_framework_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(paxsilica_framework, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(paxsilica_framework, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(paxsilica_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(paxsilica_framework, TR),
    TR >= 0.70.

:- end_tests(paxsilica_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): Elevated. The framework extracts significant value from non-aligned states through restricted chip access, forcing expensive autonomous development or subordination to US strategic objectives. But the value is not at maximum (≥0.66 snare threshold) because substantial coordination benefits genuinely exist for allied nations — this is not pure rent-seeking. The extraction is conditional on alliance participation; allies can theoretically defect (at cost). Suppression (0.68): High. Non-aligned states face suppressed alternatives: chip autarky is extremely expensive ($500B+ for a single fab, 15-20 years to reach parity), open-source AI pathways are still inferior to closed models trained on proprietary data, and alternative governance frameworks are nonexistent. But suppression is not total (≤0.90) because China, India, and others ARE developing alternatives — the path is harder, not impossible. Theater ratio (0.62): Moderate-high. The framework performs multilateralism through international consensus-building, ethics boards, and unified standards rhetoric. But 38% of the constraint is functional: actual export controls, technical verification protocols, and coordinated sanctions represent genuine enforcement mechanisms. Theater has increased over the interval as PaxSilica transitioned from proposal to implementation — early framing emphasized coordination and consensus; implementation reveals unilateral US control.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximal perspectival divergence. The US institutional perspective (beneficiary/arbitrage) sees rope — solving the collective action problem of unified AI governance. The allied government perspective (organized/constrained) sees tangled_rope — genuine coordination benefits but constrained autonomy. The non-aligned nation perspective (powerless/trapped) sees snare — pure extraction with no alternatives. The EU strategic autonomy perspective (organized/constrained) sees a different tangled_rope — more constrained than other allies due to geographic proximity to Russia and dependence on US security umbrella. The alternative chip ecosystem perspective (organized/constrained) sees scaffold — temporary extraction mechanism with visible sunset. The international governance theater perspective (institutional/arbitrage) sees piton — performative multilateralism with degraded function. The analytical observer risks mountain (naturalizing as inevitable), but the structural data reveals false summit. These are not different measurements of the same constraint — they are genuinely different structural experiences caused by agents' different positions relative to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   US Government: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiary; defines the constraint's terms and maintains exit options (can unilaterally enforce or withdraw). Non-aligned nations: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — no realistic alternatives within biographical horizon. Allied governments: Mixed + constrained → d≈0.58, f(d)≈0.72. Significant extraction (constrained autonomy) but not maximal (coordination benefits real, exit possible at cost). EU distinct: Victim + constrained → d≈0.62, f(d)≈0.82. More constrained than other allies due to geographic risk and security dependence. Semiconductor manufacturers: Moderate mixed + mobile → d≈0.48, f(d)≈0.62. Can relocate fabs, shift markets, but meaningful costs to doing so. Alternative chip ecosystems: Victim + constrained → d≈0.68, f(d)≈1.08. High extraction (expensive development path) but constrained not trapped (scaling is theoretically possible). International institutions: Minimal agents (institutional/arbitrage) → d≈0.05, f(d)≈-0.12. Theater performers benefit from legitimacy they provide.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint solves the fundamental ambiguity through perspectival analysis. The mandatrophy is: 'Is PaxSilica a coordination mechanism solving a collective action problem (Rope), or an extraction mechanism imposing US hegemony (Snare)?' The answer is: both, simultaneously, from different structural positions. From the US perspective, it is rope (genuine coordination). From the non-aligned perspective, it is snare (extraction). From the allied perspective, it is tangled_rope (mixed). The framework's theater_ratio (0.62) and base_extractiveness (0.58) place it at the border between tangled_rope (0.40 ≤ χ ≤ 0.90) and snare (χ ≥ 0.66), but the classification is resolved by introducing perspectives: the ensemble of perspectives clarifies that the constraint is NOT a unified phenomenon but rather an asymmetric institutional arrangement that produces rope-like experiences for beneficiaries and snare-like experiences for victims. The false summit (mountain from analytical observer) is caught by the contradiction between claimed naturalism and actual theater + enforcement. The piton perspective (international governance theater) is legitimate but secondary — performative elements exist but are not the primary constraint structure. The scaffold perspective (alternative ecosystems) is the key insight: PaxSilica's extraction mechanism decays over 10-20 years as alternatives mature, making it not a permanent snare but a temporary extraction mechanism. This resolves the mandatrophy by showing that the constraint's type is perspectival and temporal, not intrinsic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    chip_autarky_feasibility,
    'Can non-aligned states achieve functional chip autarky (7nm+ fabrication, design capability) within 10-20 years, or is the technological dependency structural and irreducible?',
    'Long-term tracking of Chinese SMIC, Indian semiconductor initiatives, and other non-aligned fabs; measurement of node advancement rates, yield improvements, and independent design ecosystem maturity',
    'If autarky is feasible: scaffold perspective confirmed — alternative ecosystem represents real exit path and PaxSilica becomes temporary extraction mechanism. If autarky is infeasible: snare classification hardens — non-aligned states remain trapped indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chip_autarky_feasibility, empirical, 'Feasibility of non-aligned chip manufacturing autarky').

omega_variable(
    open_source_ai_convergence,
    'Will open-source AI models (llama, mistral, etc.) converge to commercial-grade capability without proprietary training data and frontier compute, or is closed-model advantage structural and persistent?',
    'Benchmarking open-source vs proprietary models; tracking of open-source improvement trajectory; analysis of whether frontier performance requires proprietary datasets',
    'If open-source converges: alternative AI ecosystems become viable without cutting-edge chips; extract mechanism weakens. If closed advantage persists: high-end chips remain critical chokepoint; extraction mechanism hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_ai_convergence, empirical, 'Whether open-source AI can achieve capability parity without frontier compute').

omega_variable(
    alliance_cohesion_under_strain,
    'Will allied nations (EU, Japan, South Korea) maintain strict compliance with PaxSilica export controls if their own AI and semiconductor industries suffer competitive disadvantage, or will defection increase as costs accumulate?',
    'Monitoring of compliance violations, unofficial chip sales, academic collaboration with non-aligned states, regulatory divergence from US standards',
    'If cohesion holds: tangled_rope classification stable, framework enforcement remains credible. If defection increases: extraction mechanism degrades, framework shifts toward piton (performative multilateral theater with weakening enforcement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alliance_cohesion_under_strain, empirical, 'Stability of allied compliance with export control regime').

omega_variable(
    us_hegemonic_sustainability,
    'Can the US sustain unilateral enforcement authority over global chip governance, or will distributed manufacturing and open standards eventually undermine enforcement capacity?',
    'Analysis of semiconductor production concentration, cost of enforcement mechanisms, alternative standards adoption rates, and structural shifts in computing architecture (quantum, neuromorphic, analog)',
    'If US hegemony is sustainable: snare classification hardens for non-aligned states. If enforcement capacity erodes: framework transitions from tangled_rope to piton (degraded theater) or rope (reformed coordination).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(us_hegemonic_sustainability, conceptual, 'Long-term sustainability of US unilateral chip governance authority').

omega_variable(
    military_vs_civilian_separation,
    'Are military-grade and civilian-grade AI applications sufficiently separable through governance to allow technology diffusion without national security risk, or is dual-use inherent and irreducible?',
    'Analysis of actual military AI applications and their computational requirements; assessment of whether export-controlled parameters (chip node, clock speed, batch size) correspond to military vs civilian capability thresholds',
    'If separation is feasible: relaxation of constraints becomes technically defensible; framework could shift toward more permissive coordination. If dual-use is inherent: extraction mechanism (snare) becomes justified by genuine security concerns; classification shifts from political extraction to necessary constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(military_vs_civilian_separation, conceptual, 'Feasibility of separating military and civilian AI through export controls').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paxsilica_framework, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(paxsilica_tr_t0, paxsilica_framework, theater_ratio, 0, 0.48).
narrative_ontology:measurement(paxsilica_tr_t3, paxsilica_framework, theater_ratio, 3, 0.55).
narrative_ontology:measurement(paxsilica_tr_t6, paxsilica_framework, theater_ratio, 6, 0.62).

% Extraction over time
narrative_ontology:measurement(paxsilica_be_t0, paxsilica_framework, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(paxsilica_be_t3, paxsilica_framework, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(paxsilica_be_t6, paxsilica_framework, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paxsilica_framework, enforcement_mechanism).
narrative_ontology:affects_constraint(paxsilica_framework, semiconductor_supply_chain_concentration).
narrative_ontology:affects_constraint(paxsilica_framework, us_technological_hegemony_maintenance).
narrative_ontology:affects_constraint(paxsilica_framework, non_aligned_ai_development_capacity).
narrative_ontology:affects_constraint(paxsilica_framework, allied_strategic_autonomy).

% DUAL FORMULATION NOTE:
% PaxSilica governance framework decomposes into multiple constraint structures: (1) Technical coordination problem (how to set unified AI standards without fragmentation) — ε≈0.08, Rope. (2) Chip supply chain control (managing access to leading-edge fabrication) — ε≈0.72, Snare for non-aligned; Rope for allies. (3) Hegemonic control enforcement (unilateral US authority over framework) — ε≈0.58, Tangled Rope overall. The story treats the ensemble as a single constraint because the three mechanisms are inseparable in practice — attempting to separate them into distinct stories would fragment the structural reality. However, within the perspectival analysis, different observers see different dominance orderings: US sees technical coordination primary; non-aligned see supply chain lock-in primary; allies see hegemonic control enforcement primary. The ε-invariance principle suggests these could be written as separate stories, but the network coupling is so tight that decomposition would lose critical analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paxsilica_framework, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
