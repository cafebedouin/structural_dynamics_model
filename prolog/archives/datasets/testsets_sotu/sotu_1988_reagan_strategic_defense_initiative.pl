% ============================================================================
% CONSTRAINT STORY: sotu_1988_reagan_strategic_defense_initiative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1988_reagan_strategic_defense_initiative, []).

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
 *   constraint_id: sotu_1988_reagan_strategic_defense_initiative
 *   human_readable: Strategic Defense Initiative: Technology-Based Military Deterrence and Arms Reduction Coordination
 *   domain: military/geopolitics/defense_policy
 *
 * SUMMARY:
 *   The Strategic Defense Initiative announced by President Reagan in March
 *   1983 represents a structural pivot in Cold War deterrence architecture.
 *   Rather than accepting mutual assured destruction (MAD) as the basis for
 *   strategic stability, SDI reframes security through technological
 *   superiority in defensive systems. The constraint coordinates multiple
 *   institutional actors — U.S. military planners seeking strategic autonomy,
 *   Soviet planners forced into asymmetric arms competition, arms control
 *   negotiators seeking limits, and domestic budget processes absorbing
 *   development costs. The measurement interval (0-10 years, representing
 *   1983-1993) captures SDI's maximum leverage period: announcement creates
 *   shock to Soviet military planning, forcing accelerated research; Soviet
 *   economic stress mounts; arms control negotiations intensify under
 *   pressure from both U.S. (leveraging SDI advantage) and USSR (seeking
 *   negotiated limits); by interval end, Soviet Union begins collapse (1991)
 *   and SDI transitions to post-Cold War ballistic missile defense regime.
 *   The constraint exhibits all six classification types from different
 *   perspectives, revealing how the same institutional arrangement functions
 *   simultaneously as pure extraction (for trapped Soviet planners), hybrid
 *   coordination-extraction (for U.S. domestic budgets), temporary
 *   coordination (for arms control regime), and theater persistence (for ABM
 *   Treaty framework). The theater ratio rising from 0.55 to 0.68 reflects
 *   increasing performative content as actual technical barriers become
 *   apparent — the narrative of a comprehensive defensive shield becomes
 *   decoupled from actual capability development, yet the program persists.
 *   The extractiveness trajectory (0.42→0.60) shows the constraint
 *   strengthening as costs accumulate: initial announcement creates strategic
 *   shock (low extraction, high coordination signal); technical reality
 *   emerges (extraction rises as both sides recognize infinite arms
 *   competition); by decade end, extraction plateaus as Soviet collapse
 *   removes the primary extracted victim, but the constraint persists through
 *   institutional inertia.
 *
 * KEY AGENTS:
 *   - United States Strategic Planning Establishment: Primary beneficiary (institutional/arbitrage) — SDI reframes deterrence toward technological advantage; achieves strategic autonomy from mutual vulnerability; maintains program leverage despite technical barriers
 *   - Defense Industrial Complex: Secondary beneficiary (institutional/arbitrage) — $32B in R&D contracts (1983-1993); sustained funding stream for defense contractors; creates constituency for program continuation
 *   - Soviet Military Establishment: Primary victim (powerless/trapped) — forced into technological arms race at unsustainable cost; no exit options (withdrawal signals weakness, negotiation insufficient without matching program); maximum extraction from perspective of military planners
 *   - Soviet Arms Control Negotiators: Secondary victim (moderate/constrained) — experience tangled_rope: SDI creates genuine negotiation incentive (forces arms reduction discussion) but also forces acceptance of unfavorable terms (U.S. maintains program as leverage); suppression from both military (match capability) and political (negotiate limits) pressure
 *   - U.S. Domestic Budget Process: Tertiary victim (moderate/constrained) — social spending crowded by SDI costs; experience tangled_rope: SDI addresses genuine security concern (Cold War rivalry) but extraction operates through opportunity cost (health, education, infrastructure underfunding)
 *   - International Arms Control Regime: Organized actor (organized/constrained) — ABM Treaty framework degraded through SDI reinterpretation; but also activated (both sides negotiate limits under SDI pressure); experienced as scaffold with sunset logic
 *   - ABM Treaty Institution: Institutional actor (institutional/arbitrage) — piton classification: maintains symbolic role while functional constraint erodes; theatrical compliance debate masks strategic boundary-testing
 *   - Analytical Observer: Various perspectives — mountain view (naturalizes arms race logic), tangled_rope decomposition (reveals coordination + extraction structure)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1988_reagan_strategic_defense_initiative, 0.58).
domain_priors:suppression_score(sotu_1988_reagan_strategic_defense_initiative, 0.72).
domain_priors:theater_ratio(sotu_1988_reagan_strategic_defense_initiative, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1988_reagan_strategic_defense_initiative, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1988_reagan_strategic_defense_initiative, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sotu_1988_reagan_strategic_defense_initiative, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1988_reagan_strategic_defense_initiative, tangled_rope).
narrative_ontology:human_readable(sotu_1988_reagan_strategic_defense_initiative, "Strategic Defense Initiative: Technology-Based Military Deterrence and Arms Reduction Coordination").
narrative_ontology:topic_domain(sotu_1988_reagan_strategic_defense_initiative, "military/geopolitics/defense_policy").

domain_priors:requires_active_enforcement(sotu_1988_reagan_strategic_defense_initiative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1988_reagan_strategic_defense_initiative, united_states_strategic_autonomy).
narrative_ontology:constraint_beneficiary(sotu_1988_reagan_strategic_defense_initiative, defense_industrial_complex).
narrative_ontology:constraint_beneficiary(sotu_1988_reagan_strategic_defense_initiative, deterrence_credibility).
narrative_ontology:constraint_victim(sotu_1988_reagan_strategic_defense_initiative, soviet_union_military_resources).
narrative_ontology:constraint_victim(sotu_1988_reagan_strategic_defense_initiative, strategic_arms_reduction_timeline).
narrative_ontology:constraint_victim(sotu_1988_reagan_strategic_defense_initiative, domestic_social_spending).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOVIET MILITARY ESTABLISHMENT (SNARE) — Trapped in technological arms race. The SDI announcement forces matching capability development at ruinous cost. No exit option: withdrawal signals strategic weakness; matching drains resources from civilian economy; negotiation insufficient (U.S. maintains program regardless of talks). Maximum experienced extraction. The constraint operates with maximal suppression: Soviet planners face binary choice between unilateral disarmament (unacceptable politically) or resource hemorrhage (economically unsustainable). Theater minimal from this perspective — the threat is existential, not performative.
constraint_indexing:constraint_classification(sotu_1988_reagan_strategic_defense_initiative, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SOVIET ARMS CONTROL NEGOTIATORS (TANGLED ROPE) — Constrained by both military pressure (match SDI or lose deterrence) and political pressure (negotiate arms reduction or face economic collapse). Experience genuine coordination function: SDI creates incentive for negotiating limits. But also experience extraction: the program forces accelerated negotiation on terms favorable to U.S., undermining Soviet negotiating leverage. Suppression moderate-high: career risk of being blamed for losing arms race, but also recognition that negotiation is the only viable path. Theater moderate: some negotiation theater (public positions) but underlying pressure is genuine.
constraint_indexing:constraint_classification(sotu_1988_reagan_strategic_defense_initiative, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: U.S. STRATEGIC PLANNING ESTABLISHMENT (ROPE) — Primary beneficiary. Experiences SDI as solving the Cold War coordination problem: reframe deterrence from mutual vulnerability (MAD) toward technological superiority. Net coordination benefit with asymmetric gain. Exit options abundant (can accelerate, decelerate, or abandon program based on technical progress). Theater significant but subordinate to perceived strategic benefit. Suppression low — political consensus on need for strategic alternatives to MAD provides institutional cover.
constraint_indexing:constraint_classification(sotu_1988_reagan_strategic_defense_initiative, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL ARMS CONTROL REGIME (SCAFFOLD) — Sees SDI as temporary policy (sunset: technology maturation or treaty achievement). The constraint has genuine coordination function: forces both superpowers to negotiate (carrot of deterrence improvement, stick of arms race cost). Theater high: much diplomatic activity, many negotiating rounds, but underlying function is real (arms reduction agreements do happen). Has sunset logic: either SDI succeeds (shifts deterrence architecture, sunset natural) or fails (program abandoned, technical proof incomplete), or is constrained by treaty (ABM Treaty extension/modification). Beneficiaries include arms control advocates seeking verification mechanisms; suppression moderate (some resistance to verification norms, but general acceptance that negotiated limits are necessary).
constraint_indexing:constraint_classification(sotu_1988_reagan_strategic_defense_initiative, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: U.S. DOMESTIC BUDGET PROCESS (TANGLED ROPE) — Experiences both coordination and extraction. Coordination function: SDI redirects resources toward military security, achieving consensus that strategic defense is necessary (responds to genuine threat perception). Extraction function: SDI consumes resources (~$32B over 1983-1993 program lifetime) that could fund social infrastructure, accelerating decline of competing domestic priorities (education, infrastructure, poverty reduction). Constrained by political commitment to the program; suppression moderate (some Congressional opposition but overridden by strategic consensus). Theater moderate-high: public debate frames SDI as defensive/peaceful, while actual function is offensive strategic positioning.
constraint_indexing:constraint_classification(sotu_1988_reagan_strategic_defense_initiative, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ABM TREATY INSTITUTIONAL FRAMEWORK (PITON) — The 1972 Anti-Ballistic Missile Treaty created institutional constraints on strategic defense development. SDI redefines the treaty's role: from absolute prohibition (institutional structure) to negotiation object (institutional theater). The treaty persists in degraded form — technically binding but functionally reinterpreted through SDI research loopholes (testing at sub-threshold scales, exotic technology research). Theater ratio very high (0.75+): much discussion of treaty compliance, technical definitions of 'exotic' vs 'conventional' defenses, but real function (constraining arms race) has atrophied. Maintained through institutional inertia (both superpowers value arms control symbolism) rather than effective constraint.
constraint_indexing:constraint_classification(sotu_1988_reagan_strategic_defense_initiative, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRATEGIC STABILITY NATURAL LAW (MOUNTAIN) — From civilizational/universal perspective, the balance of power is an immutable constraint: if one side seeks superiority, the other must match or accept strategic inferiority. This appears as a natural law of military competition. Perceived accessibility collapse very high (0.88): defense planners cannot imagine alternatives to technological arms racing. Perceived resistance to change very low (0.08): once superiority is perceived as possible, the race is inevitable. However, this classification is a FALSE SUMMIT: the constraint naturalizes policy choice (SDI development) as if it were a law of physics or strategy.
constraint_indexing:constraint_classification(sotu_1988_reagan_strategic_defense_initiative, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / INSTITUTIONAL DECOMPOSITION (TANGLED ROPE) — The SDI is neither a natural law nor pure extraction. It is a structured institutional arrangement that coordinates military strategy (genuine benefit for U.S. deterrence) with extraction (forced Soviet resource expenditure, domestic budget crowding, ABM Treaty degradation). The false summit detector fires because identifiable beneficiaries exist (defense industry, U.S. strategic planners) despite the 'natural law' framing. The true classification is tangled_rope: real coordination function (forces arms control negotiation toward outcome favorable to U.S. security preferences) combined with asymmetric extraction (costs borne by Soviet economy and U.S. domestic priorities). Theater ratio (0.68) reflects that strategic defense is framed as purely defensive while functioning as strategic offense — the 'shield' narrative masks deterrence repositioning.
constraint_indexing:constraint_classification(sotu_1988_reagan_strategic_defense_initiative, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1988_reagan_strategic_defense_initiative_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1988_reagan_strategic_defense_initiative, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1988_reagan_strategic_defense_initiative, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1988_reagan_strategic_defense_initiative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1988_reagan_strategic_defense_initiative, TR),
    TR >= 0.70.

:- end_tests(sotu_1988_reagan_strategic_defense_initiative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. SDI extracts resources from Soviet economy (forced matching) and U.S. domestic spending (budget crowding), but is not pure extraction because genuine coordination function exists (creates negotiating framework for arms reduction). The trajectory from 0.42→0.60 reflects accumulating costs as technical barriers emerge and both sides recognize the race is infinite rather than winnable. Theater ratio (0.68): Moderate-high. Initially high strategic content (genuine capability threat to Soviet deterrence), but degrading toward theater as technical proof remains elusive (promised strategic defense system never materializes at claimed capability levels). By decade 10, performance gap between narrative (comprehensive defensive shield) and reality (limited experimental systems) becomes apparent, yet program persists through institutional momentum. Suppression (0.72): Moderate-high. Soviet planners face binary choice: match or accept inferiority (suppression from military necessity). U.S. domestic alternatives blocked by strategic consensus (suppression from political alignment on Cold War threat). ABM Treaty constraints initially restrictive (0.72 effective suppression for SDI researchers), then eroded through technical reinterpretation (suppression decline toward piton persistence). Claimed type tangled_rope correctly reflects genuine coordination function (forces arms control negotiation, improves U.S. strategic position relative to MAD baseline) combined with asymmetric extraction (costs borne primarily by Soviet economy and U.S. social spending).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival fragmentation. Soviet military planners see maximal extraction (snare) — forced into arms race by technology announcement. Soviet negotiators see mixed coordination-extraction (tangled_rope) — SDI creates negotiation incentive but at unfavorable terms. U.S. strategic planners see pure benefit (rope) — reframe deterrence on favorable terms with asymmetric capability advantage. Arms control advocates see temporary coordination (scaffold) — SDI provides leverage for negotiation on strategic defense limits. U.S. domestic budget process sees mixed coordination-extraction (tangled_rope) — addresses genuine security concern but extracts opportunity cost. ABM Treaty institution sees degraded function (piton) — maintains symbolic role while constraint erodes. Civilizational analytical view initially sees natural law (mountain) — arms race seems inevitable from strategic logic — but decomposition reveals false summit: policy choice (SDI development) is not physical law. The perspectival gaps reveal how the same structural arrangement is experienced as an inescapable trap (Soviet military), a temporary problem (arms control), a strategic advantage (U.S. military), and institutional theater (treaty framework). The mandatrophy resolves by recognizing that all six types are simultaneously true from their respective positions — the constraint is not one thing but a structural arrangement that means different things to different power positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by agent power and exit options. Soviet military (powerless/trapped) experiences d≈0.98 → f(d)≈1.42 (maximal extraction multiplier). Soviet negotiators (moderate/constrained) experience d≈0.65 → f(d)≈1.00 (high extraction, moderate agency). U.S. strategists (institutional/arbitrage) experience d≈0.12 → f(d)≈-0.02 (beneficiary position, negative extraction). U.S. domestic budgets (moderate/constrained) experience d≈0.62 → f(d)≈0.93 (high extraction, constrained exit). Arms control regime (organized/constrained) experiences d≈0.48 → f(d)≈0.57 (mixed, genuine agency). ABM Treaty (institutional/arbitrage) experiences d≈0.25 → f(d)≈0.02 (institutional benefit from treaty persistence, low extraction). The derivation accurately captures the power differentials: powerless victims of arms race feel maximum force; organized agents with some exit agency feel moderate force; institutional beneficiaries feel negative or minimal force. The chi formula χ = ε × f(d) × σ(S) scales this by scope modifier: SDI at global scope (σ=1.2) amplifies extraction for global-scope observers; national scope (σ=1.0) for domestic budget processes; varies by perspective. For Soviet military at global scope: χ ≈ 0.58 × 1.42 × 1.2 ≈ 0.99 (approaching snare threshold χ≥0.66 for this victim). For U.S. strategists at global scope: χ ≈ 0.58 × (-0.02) × 1.2 ≈ -0.01 (beneficiary, negative effective extraction). The directionality logic shows why the constraint functions as extraction for some agents and coordination for others — it is not a property of the constraint itself but of the agent's structural position relative to its asymmetric benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED VIA INSTITUTIONAL DECOMPOSITION: The mandatrophy (coordinate claim seems impossible — is this pure extraction, mixed coordination-extraction, pure coordination, or temporary scaffolding?) resolves by recognizing that all six types are simultaneously true from different institutional positions. The false summit (mountain perspective naturalizing arms race logic) is identified by the presence of beneficiaries (U.S. strategic planners, defense industry). The analytical decomposition reveals: (1) Genuine coordination function: SDI forces both superpowers to negotiate strategic defense limits (rope-like); (2) Asymmetric extraction: U.S. leverages technology advantage to force terms favorable to U.S. security (snare-like for victims); (3) Temporary structure: Arms control agreements eventually constrain SDI scope (scaffold-like); (4) Theater persistence: ABM Treaty framework becomes performative as SDI research erodes effective constraint (piton-like). No single type captures the full constraint — the correct model is the presheaf over eight perspectives showing how the same institutional arrangement means different things to different power positions. The mandatrophy is resolved by abandoning the search for a single classification and embracing the perspectival multiplicity as the true structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_feasibility_threshold,
    'What level of technical success would constitute genuine strategic defense (mountain-level immutable capability) vs. aspirational research program (policy constraint)?',
    'Post-Cold War review of SDI technical achievements; comparison of initial capabilities promised (mid-1980s speeches) vs. actual deployed systems (1990s onward); assessment of what would have been required for claimed 100% intercept capability',
    'If genuine capability emergent: SDI transitions from policy choice to strategic reality (mountain). If perpetual research program: SDI remains policy constraint (tangled_rope). Historical evidence suggests perpetual research (SDI became Ballistic Missile Defense Organization, then Missile Defense Agency — program continues but never achieves promised capabilities, supporting tangled_rope classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technical_feasibility_threshold, empirical, 'Whether SDI technical development produces strategic defense or perpetual research program').

omega_variable(
    arms_reduction_causality,
    'Did SDI force genuine strategic arms reduction (USSR responds to capability threat) or merely accelerate negotiation that would have happened anyway (USSR seeking economic relief)?',
    'Counterfactual analysis: Soviet arms reduction behavior with/without SDI; timeline correlation between SDI announcements and Soviet negotiating position shifts; Soviet internal documents (declassified after Cold War) showing decision calculus',
    'If SDI-forced: coordination function is real (snare victim forced to negotiating table). If inevitable anyway: coordination function is theater (both sides seeking exit from arms race regardless of SDI). If mixed: tangled_rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arms_reduction_causality, empirical, 'Whether SDI causally forced Soviet arms reduction or accelerated inevitable negotiation').

omega_variable(
    domestic_budget_substitution,
    'Would the resources consumed by SDI development have been deployed to social spending (education, infrastructure) or remained in military budget (conventional forces, strategic command infrastructure)?',
    'Budget counterfactual analysis: comparison with alternative military spending scenarios; historical precedent from periods of lower defense spending; Congressional debate records on SDI opportunity costs',
    'If substitution from social spending: domestic victims thesis confirmed. If reallocation within military budget: extraction operates horizontally (between military priorities) not vertically (against civilian needs). If SDI replaces capability gaps elsewhere: extraction logic inverts (SDI as efficient consolidation, not pure addition).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(domestic_budget_substitution, preference, 'Whether SDI resources diverted from social spending or reallocated within military budget').

omega_variable(
    soviet_regime_collapse_causality,
    'Did SDI-driven military spending accelerate Soviet economic collapse (extraction mechanism works) or was Soviet collapse overdetermined by other factors (energy prices, reform failure, systemic inefficiency)?',
    'Soviet economic modeling: counterfactual scenarios of SDI vs. non-SDI Soviet military spending 1985-1991; correlation analysis of defense budgets and economic indicators; Soviet leadership decision documents on arms race sustainability',
    'If SDI-caused: the constraint achieved maximum strategic extraction (collapse of adversarial state). If overdetermined: SDI was one factor among many (extractiveness high but not determining). If exaggerated: SDI became scapegoat narrative (theater masking other causes).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(soviet_regime_collapse_causality, empirical, 'Whether SDI-driven spending accelerated Soviet economic collapse').

omega_variable(
    abm_treaty_violation_intent,
    'Did SDI research deliberately test ABM Treaty boundaries to prepare eventual withdrawal (strategic intent) or genuinely stay within technical compliance thresholds (respecting constraint)?',
    'Review of SDI testing protocols; comparison with explicitly prohibited technologies; analysis of research scaling decisions (why test at particular altitude/speed parameters); declassified DoD strategic planning documents',
    'If deliberate boundary-testing: ABM Treaty piton classification is correct (degraded through intentional technical reinterpretation). If genuine compliance: treaty is functioning constraint (modified rope). If deliberate intent to create pretext for withdrawal: theater very high (treaty theater to justify program).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abm_treaty_violation_intent, conceptual, 'Whether SDI research deliberately tested ABM Treaty boundaries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1988_reagan_strategic_defense_initiative, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sdi_tr_t0, sotu_1988_reagan_strategic_defense_initiative, theater_ratio, 0, 0.55).
narrative_ontology:measurement(sdi_tr_t2, sotu_1988_reagan_strategic_defense_initiative, theater_ratio, 2, 0.62).
narrative_ontology:measurement(sdi_tr_t5, sotu_1988_reagan_strategic_defense_initiative, theater_ratio, 5, 0.68).
narrative_ontology:measurement(sdi_tr_t8, sotu_1988_reagan_strategic_defense_initiative, theater_ratio, 8, 0.7).
narrative_ontology:measurement(sdi_tr_t10, sotu_1988_reagan_strategic_defense_initiative, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(sdi_be_t0, sotu_1988_reagan_strategic_defense_initiative, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sdi_be_t2, sotu_1988_reagan_strategic_defense_initiative, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(sdi_be_t5, sotu_1988_reagan_strategic_defense_initiative, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(sdi_be_t8, sotu_1988_reagan_strategic_defense_initiative, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(sdi_be_t10, sotu_1988_reagan_strategic_defense_initiative, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1988_reagan_strategic_defense_initiative, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1988_reagan_strategic_defense_initiative, abm_treaty_framework).
narrative_ontology:affects_constraint(sotu_1988_reagan_strategic_defense_initiative, soviet_military_modernization_response).
narrative_ontology:affects_constraint(sotu_1988_reagan_strategic_defense_initiative, us_defense_industrial_complex_expansion).
narrative_ontology:affects_constraint(sotu_1988_reagan_strategic_defense_initiative, arms_reduction_treaty_negotiation).

% DUAL FORMULATION NOTE:
% SDI represents a structural pivot in Cold War deterrence. The constraint decomposes into multiple downstream obligations: (1) ABM Treaty reinterpretation (institutional theater); (2) Soviet military response (arms race intensification); (3) Defense contractor R&D expansion (military-industrial growth); (4) Arms reduction negotiation (forces both superpowers to negotiating table). Each downstream constraint has its own ε value reflecting different extraction mechanisms. SDI itself (this story) operates at ε=0.58 as the coordinating mechanism that structures all downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
