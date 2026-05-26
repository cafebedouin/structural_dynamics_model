% ============================================================================
% CONSTRAINT STORY: switching_cost_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_switching_cost_lock_in, []).

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
 *   constraint_id: switching_cost_lock_in
 *   human_readable: Switching Cost Lock-In: QWERTY Keyboard Layout Persistence
 *   domain: technology_history/economic_sociology/path_dependence
 *
 * SUMMARY:
 *   The QWERTY keyboard layout persistence represents a canonical test case
 *   for competing theories of technological lock-in. Is QWERTY dominance a
 *   natural consequence of network effects and path dependence (immutable, no
 *   identifiable beneficiary, mountain perspective), or does it reflect
 *   deliberate enforcement by manufacturers and training institutions who
 *   benefit from standardization (extractive, identifiable beneficiaries,
 *   snare or tangled rope perspective)? The constraint exhibits
 *   characteristics of both: genuine coordination benefits (standardization
 *   solves real problems for manufacturers and schools) coexist with genuine
 *   extraction (users bear switching costs with no exit option). The
 *   empirical question is whether the coordination benefits are primary and
 *   the extraction secondary (Rope), or vice versa (Snare). This turns on
 *   factual disputes: Did manufacturers deliberately suppress alternatives to
 *   maintain lock-in, or did they opportunistically benefit from a
 *   path-dependent outcome they didn't engineer? Is QWERTY genuinely
 *   suboptimal, or was it near-optimal given the constraints it emerged from
 *   (typewriter-era type-bar jam prevention)? Are switching costs genuinely
 *   insurmountable, or are they overstated by advocates for alternative
 *   layouts? The constraint's classification depends critically on resolving
 *   these empirical questions — a case study in how epistemically
 *   under-determined domains (technology history without direct evidence of
 *   strategic decisions) map to structural classification.
 *
 * KEY AGENTS:
 *   - Individual Typist (powerless/trapped): Bears full cost of suboptimal layout through muscle-memory lock-in; no exit option at biographical scale
 *   - Keyboard Manufacturer (institutional/arbitrage): Primary beneficiary of standardization; benefits from simplified production and predictable market
 *   - Training Institution/School (institutional/arbitrage): Secondary beneficiary; benefits from standardized curriculum and predictable student outcomes
 *   - Alternative Layout Developer (moderate/constrained): Victim of network effects and installed-base coordination problem; benefits partly from standardization (knows which layout to optimize for)
 *   - Manufacturer Coalition (powerful/constrained): Collective beneficiary; active enforcement through product ecosystem; constrained by need to maintain industry coordination
 *   - Dvorak Advocate Movement (organized/mobile): Theatrical challenger; maintains advocacy despite zero path to adoption; persists through inertia rather than functional impact
 *   - Analytical Observer (analytical/analytical): Risks naturalizing contingent institutional arrangement as inevitable technological law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(switching_cost_lock_in, 0.38).
domain_priors:suppression_score(switching_cost_lock_in, 0.52).
domain_priors:theater_ratio(switching_cost_lock_in, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(switching_cost_lock_in, extractiveness, 0.38).
narrative_ontology:constraint_metric(switching_cost_lock_in, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(switching_cost_lock_in, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(switching_cost_lock_in, tangled_rope).
narrative_ontology:human_readable(switching_cost_lock_in, "Switching Cost Lock-In: QWERTY Keyboard Layout Persistence").
narrative_ontology:topic_domain(switching_cost_lock_in, "technology_history/economic_sociology/path_dependence").

domain_priors:requires_active_enforcement(switching_cost_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(switching_cost_lock_in, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(switching_cost_lock_in, training_institutions).
narrative_ontology:constraint_victim(switching_cost_lock_in, alternative_layout_developers).
narrative_ontology:constraint_victim(switching_cost_lock_in, typing_efficiency_potential).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TYPIST (SNARE) — Individual users face massive switching costs: decades of muscle memory, unavailable alternative keyboards in public/institutional settings, no economic incentive to retrain. Exit is structurally impossible at biographical timescale. Extraction experienced as total — the typist bears full cost of suboptimal layout with zero exit option.
constraint_indexing:constraint_classification(switching_cost_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE LAYOUT DEVELOPER (TANGLED ROPE) — Faces coordination problem: needs critical mass of users to make alternative layouts viable, but critical mass requires users to incur switching costs. Also experiences genuine benefit from network standardization — if one layout dominates, learning which one to optimize is itself solved. High suppression (network effects, installed base) but some exit option (niche markets, specialized devices). Extraction is real but mixed with coordination benefit.
constraint_indexing:constraint_classification(switching_cost_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: KEYBOARD MANUFACTURER (ROPE) — Experiences QWERTY dominance as pure coordination advantage: manufacturing one layout at scale is cheaper than supporting multiple layouts. Benefits from training institutions teaching QWERTY. This perspective sees the constraint as solving a genuine collective action problem — standardization reduces manufacturer complexity and user confusion. Extraction runs toward this agent (they benefit from standard), but the benefit is genuine coordination surplus, not pure extraction.
constraint_indexing:constraint_classification(switching_cost_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRAINING INSTITUTION (ROPE) — Teaching QWERTY is vastly simpler than teaching multiple layouts: standardized curriculum, standardized textbooks, predictable student outcomes. Benefits from the coordination function of QWERTY dominance. Has arbitrage option (could teach Dvorak, but loses market position). Experiences the constraint as beneficial coordination — standardization solved their pedagogical problem.
constraint_indexing:constraint_classification(switching_cost_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: MANUFACTURER COALITION (TANGLED ROPE) — Collective perspective of firms that benefit from QWERTY dominance as standard. Experiences genuine coordination benefits (standardization reduces product diversity, accelerates market growth) alongside extractive leverage over users and alternative-layout producers. Active enforcement through product ecosystem (most devices ship with QWERTY only). Constrained exit because breaking standard would require industry coordination to shift — individual firm leaving QWERTY loses competitive advantage.
constraint_indexing:constraint_classification(switching_cost_lock_in, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LOCK-IN VIEW (MOUNTAIN) — From civilizational/universal scale, path dependence in technology standards is structurally inevitable: small initial advantages (QWERTY's early adoption in typewriter era) amplify through network effects and training accumulation into irreversible dominance. No agent's choice 'caused' this — it emerged from rational individual decisions that compound into a trap. This perspective naturalizes the outcome as an immutable consequence of network effects. However, the presence of identifiable beneficiaries (manufacturers, training institutions) triggers false-summit evaluation.
constraint_indexing:constraint_classification(switching_cost_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: ALTERNATIVE LAYOUT ADVOCATES (PITON) — Organized groups (Dvorak Society, etc.) maintain advocacy for alternative layouts despite declining adoption and minimal real-world impact. The advocacy is substantially theatrical — it identifies a real problem (QWERTY suboptimal for typing speed/ergonomics) but the proposed solution (switch to Dvorak) has zero path to implementation. The organization persists through inertia and ideological commitment rather than functional effectiveness. Theater ratio high because effort-to-impact ratio is inverted.
constraint_indexing:constraint_classification(switching_cost_lock_in, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(switching_cost_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(switching_cost_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(switching_cost_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(switching_cost_lock_in, TR),
    TR >= 0.70.

:- end_tests(switching_cost_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint exhibits both genuine coordination benefits (standardization solves real problems) and genuine extraction (users bear switching costs). The balance is uncertain — extractiveness could be 0.25 (primarily coordination) if manufacturers and training institutions benefited only opportunistically, or 0.55+ (primarily extraction) if they deliberately enforced lock-in. The 0.38 value reflects the middle estimate pending resolution of the empirical omegas. Suppression (0.52): Moderate-high. Barriers to exit include: installed-base network effects (compatibility advantage), training path-dependence (labor supply trained only in QWERTY), infrastructure lock-in (most devices ship with QWERTY as default, requiring user customization to switch), and institutional coordination (schools teach QWERTY, maintaining steady supply of trained QWERTY users). However, suppression is not total — modern digital devices support alternative layouts at zero cost, and switching is technically trivial. Suppression would be higher (0.70+) if manufactured devices actively blocked alternative layouts; it is moderate because the barrier is now primarily institutional inertia, not technical. Theater ratio (0.58): Moderate-high. The Dvorak advocacy movement maintains visibility and cultural narrative ('QWERTY is suboptimal, Dvorak is faster') despite negligible real-world adoption since the 1930s. The advocacy is substantially theatrical — it identifies a real problem but the proposed solution has zero implementation path. Modern digital keyboard customization (OS-level support for Dvorak) is technically available but sees minimal adoption, suggesting that the switching cost barrier or the performance advantage claim (or both) are overstated relative to the advocacy narrative.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiary and victim perspectives is substantial and reveals the core structural ambiguity. The manufacturer sees coordination (Rope) — standardization genuinely solves production complexity. The typist sees extraction (Snare) — they bear switching costs with no exit. The alternative-layout developer sees mixed coordination and extraction (Tangled Rope) — benefiting from knowing which layout to optimize for while being blocked by network effects. The training institution sees coordination (Rope) — standardized curriculum is genuinely simpler. The organized alternative advocates see their own effort as theatrical (Piton) — they maintain advocacy despite zero impact pathway. The analytical observer risks seeing inevitable natural law (Mountain) — path dependence and network effects as immutable — but the presence of identifiable institutional beneficiaries triggers false-summit evaluation. The gap is diagnostic: if all perspectives produced the same type, the classification would be wrong. The fact that they diverge widely suggests the constraint is genuinely structured by institutional relationships rather than by pure technical inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position relative to the extraction flow. Beneficiaries (manufacturers, training institutions) with arbitrage exit options derive d ≈ 0.10-0.20 (low d = low/negative chi = experienced as coordination, not extraction). Victims (typists) with trapped exit derive d ≈ 0.95 (high d = high chi = maximum experienced extraction). The alternative-layout developer has higher d than beneficiaries but lower than trapped agents, reflecting constrained exit and mixed benefits. Organized agents (advocates) have mobile exit (can walk away from the cause) and derive lower d than the constraint's nominal victims. The power-atom assignment (institutional for manufacturers, powerless for typists) reflects constraint-relative position, not global economic status. A typist may be globally powerful (wealthy, educated) but structurally powerless relative to THIS constraint because they have no exit option that avoids switching costs. A manufacturer may be globally powerful and locally powerful relative to this constraint because they have arbitrage options and benefit from standardization.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exemplifies the mandatrophy in beneficiary-driven versus path-dependent lock-in narratives. The question is not 'which classification is correct' but 'which narrative is empirically true?' If manufacturers deliberately enforced QWERTY to extract switching-cost rents, the constraint is Snare with identifiable beneficiaries and would-be victims. If QWERTY dominance emerged from path-dependent accumulation of rational decisions (early adoption → training supply → network effects → irreversibility) without deliberate enforcement, the constraint is Rope: genuine coordination benefits exceeded extraction costs at each step, but the compound outcome is now near-irreversible. Both narratives are logically coherent. The empirical omegas (beneficiary intentionality, technical feasibility of alternatives, actual switching costs, network-effects necessity) resolve the mandatrophy by determining which narrative the historical record supports. The false-summit mountain (naturalizing contingent institutional arrangements) correctly identifies the risk of treating a path-dependent institutional outcome as a law of nature — but the resolution of whether beneficiaries actively enforced lock-in or merely capitalized on accident determines whether the mountain is genuinely false or merely perspectival.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_artifactuality,
    'Are keyboard manufacturers and training institutions genuine beneficiaries who actively enforced QWERTY dominance, or are they post-hoc identified parties who benefited from a path-dependent accident?',
    'Historical archive analysis: evidence of deliberate lock-in strategy vs. opportunistic benefit-capture. Timeline of manufacturer decisions relative to QWERTY dominance emergence. Decision letters, board minutes, product development strategy documents from 1960s-1980s period when alternative layouts were still technically feasible but were not pursued.',
    'If deliberate enforcement: constraint reclassifies as Snare with identifiable enforcers. If opportunistic capture: constraint is genuinely path-dependent Tangled Rope where beneficiaries emerged without planning. This determines whether false-summit mountain correctly identifies beneficiary-driven lock-in or artificially constructs a causal narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_artifactuality, empirical, 'Whether QWERTY dominance results from deliberate manufacturer strategy or path-dependent emergence').

omega_variable(
    qwerty_optimality_contested,
    'Is QWERTY genuinely suboptimal for typing speed/ergonomics compared to alternatives like Dvorak, or does the empirical evidence support QWERTY''s performance as near-optimal given the constraint landscape it emerged in?',
    'Meta-analysis of typing-speed studies comparing QWERTY to Dvorak (control for training effects, practice time). Ergonomic analysis of hand fatigue and repetitive strain across layouts in controlled studies. Historical analysis of why QWERTY was adopted in typewriter era (type-bar jam prevention, not optimization for human factors).',
    'If QWERTY is genuinely suboptimal: victims (typists) experience real extraction from layout choice. If QWERTY is near-optimal: extraction is illusory or minimal, and the constraint reclassifies downward (Rope rather than Tangled Rope). The Dvorak advocacy narrative depends on contested optimality claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(qwerty_optimality_contested, empirical, 'Whether QWERTY is genuinely suboptimal compared to alternatives').

omega_variable(
    switching_cost_magnitude_timescale,
    'What is the actual switching cost (in time, effort, economic loss) for an individual to transition from QWERTY to an alternative layout at different life stages (childhood, adolescence, adulthood)?',
    'Empirical study of adoption patterns for Dvorak users: time to fluency, productivity loss during transition, long-term speed gains, willingness-to-pay for re-training. Survey data on reasons Dvorak adoption failed despite claimed benefits. Economic analysis of training cost vs. lifetime productivity gain.',
    'If switching cost is low (<100 hours to fluency): exits option should be ''constrained'' rather than ''trapped'' for typists. If switching cost is high (>2000 hours): justifies trapped classification. Magnitude determines whether suppression is genuine structural barrier or theatrical narrative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_magnitude_timescale, empirical, 'Magnitude and timescale of individual switching costs').

omega_variable(
    alternative_layout_technical_feasibility,
    'During which historical periods was technical deployment of alternative layouts (Dvorak, Colemak) actually feasible without massive infrastructure change, and was such deployment ever seriously attempted by manufacturers?',
    'Historical timeline: 1873-1950 (typewriter era) — when was QWERTY choice reversible? 1950-1980 (electromechanical) — when alternatives became technically trivial but were not deployed? 1980-present (digital) — when alternative layouts require zero hardware change but remain non-standard in OS defaults. Evidence of manufacturer product development decisions not to include alternatives.',
    'If alternatives were technically trivial since 1980 but not deployed: evidence of deliberate suppression. If deployment was genuinely costly until recently: evidence of path-dependent accident. This determines whether active enforcement is real or constructed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_layout_technical_feasibility, empirical, 'Technical feasibility window for alternative layout deployment').

omega_variable(
    network_effects_necessity,
    'Is QWERTY dominance maintained by genuine network effects (users benefit from compatibility with other users), or by infrastructure lock-in (devices only ship with QWERTY), or by training path-dependence (labor supply trained only in QWERTY)?',
    'Counterfactual analysis: if alternative layouts were available on all devices at zero cost, would users switch? Survey studies on switching willingness. Test markets (niche devices with Dvorak pre-installed) adoption rates. Analysis of modern digital keyboard customization adoption (how many users customize to Dvorak when OS support exists?)',
    'If network effects dominate: users are rationally trapped (coordination problem). If infrastructure lock-in dominates: this is manufacturer enforcement, not network. If training dependence dominates: this is institutional lock-in. Each mechanism maps to different constraint type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_necessity, empirical, 'Mechanism of QWERTY dominance: network effects vs. infrastructure vs. training path-dependence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(switching_cost_lock_in, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(switch_tr_t0, switching_cost_lock_in, theater_ratio, 0, 0.35).
narrative_ontology:measurement(switch_tr_t30, switching_cost_lock_in, theater_ratio, 30, 0.48).
narrative_ontology:measurement(switch_tr_t60, switching_cost_lock_in, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(switch_be_t0, switching_cost_lock_in, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(switch_be_t30, switching_cost_lock_in, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(switch_be_t60, switching_cost_lock_in, base_extractiveness, 60, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(switching_cost_lock_in, information_standard).
narrative_ontology:affects_constraint(switching_cost_lock_in, technical_standards_lock_in).
narrative_ontology:affects_constraint(switching_cost_lock_in, training_supply_path_dependence).

% DUAL FORMULATION NOTE:
% QWERTY persistence decomposes into two structurally distinct constraints: (1) the coordination benefit of information standardization (Rope) — manufacturers and users genuinely benefit from knowing which keyboard layout is standard; (2) the extraction cost of switching barriers (Tangled Rope) — users bear costs of muscle-memory lock-in while manufacturers benefit from installed-base control. These are linked: coordination benefits would not create extraction if switching were costless. The present story integrates both mechanisms. Upstream constraints (early typewriter era path-dependence, mechanical design constraints favoring QWERTY) led to QWERTY's initial adoption; downstream constraints (training supply, institutional standardization) perpetuate it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(switching_cost_lock_in, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
