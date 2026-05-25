% ============================================================================
% CONSTRAINT STORY: china_advanced_chip_design_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_advanced_chip_design_constraint, []).

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
 *   constraint_id: china_advanced_chip_design_constraint
 *   human_readable: China Advanced Chip Design Constraint
 *   domain: geopolitical/technology/semiconductor
 *
 * SUMMARY:
 *   The constraint on Chinese advanced chip design operates through a
 *   combination of export controls (EDA tools, foundry access restrictions,
 *   IP licensing), talent mobility barriers (visa restrictions, security
 *   screening), and alliance-based technology withholding. This constraint
 *   exhibits multiple structural types depending on the observer's position:
 *   it appears as pure extraction (snare) to trapped researchers and
 *   constrained companies; as legitimate coordination (rope) to beneficiary
 *   US firms; as mixed coordination-extraction (tangled rope) to allied
 *   designers and Chinese government industrial policy; as degraded
 *   enforcement theater (piton) to the export control apparatus itself; and
 *   as a temporary problem with a sunset pathway (scaffold) to actors
 *   building alternative ecosystems. The constraint's extractiveness has
 *   increased from 0.45 to 0.68 over the measurement interval as controls
 *   have tightened and as Chinese investments in alternatives have revealed
 *   the true cost of the gap. Theater ratio remains moderate (0.55) because
 *   the constraint operates through multiple mechanisms — some highly visible
 *   enforcement (entity lists, license denials) and some invisible structural
 *   barriers (IP licensing complexity, foundry access). The constraint is
 *   maintained through sustained geopolitical enforcement rather than through
 *   natural law or inevitable technological asymmetry.
 *
 * KEY AGENTS:
 *   - Chinese Semiconductor Researchers: Primary victims (powerless/trapped) — blocked from access to state-of-the-art design tools and international collaboration; face permanent career limitations within the constraint
 *   - Chinese Semiconductor Companies (SMIC, HiSilicon, Huawei): Secondary victims (moderate/constrained) — can invest in workarounds but face extraction through forced domestic investment, restricted foundry access, and IP licensing costs
 *   - US Semiconductor Ecosystem (Cadence, Synopsys, Intel, TSMC): Primary beneficiaries (institutional/arbitrage) — benefit from market dominance, reduced competition, and supply chain control; experience constraint as coordination mechanism
 *   - Allied Chip Designers (Europe, South Korea, Taiwan): Secondary beneficiaries (organized/constrained) — benefit from reduced Chinese competition but face extraction through alliance compliance and licensing dependencies
 *   - Chinese Government Industrial Policy: State-level actor (powerful/mobile) — coordinates extraction from private sector to fund alternative ecosystems (RISC-V, domestic EDA, SMIC); experiences mixed coordination-extraction dynamic
 *   - Export Control Apparatus (BIS, Commerce Department): Institutional enforcer (institutional/arbitrage) — maintains performative enforcement machinery; actual control efficacy disputed (piton perspective)
 *   - Alternative Chip Ecosystem Development (RISC-V, EDA2.0, SMIC): Organized actors (organized/mobile) — building sunset pathway through domestic tool maturity and foundry advancement; expect constraint weakening over generational timescale
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_advanced_chip_design_constraint, 0.68).
domain_priors:suppression_score(china_advanced_chip_design_constraint, 0.72).
domain_priors:theater_ratio(china_advanced_chip_design_constraint, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_advanced_chip_design_constraint, extractiveness, 0.68).
narrative_ontology:constraint_metric(china_advanced_chip_design_constraint, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(china_advanced_chip_design_constraint, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_advanced_chip_design_constraint, snare).
narrative_ontology:human_readable(china_advanced_chip_design_constraint, "China Advanced Chip Design Constraint").
narrative_ontology:topic_domain(china_advanced_chip_design_constraint, "geopolitical/technology/semiconductor").

domain_priors:requires_active_enforcement(china_advanced_chip_design_constraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_advanced_chip_design_constraint, us_semiconductor_ecosystem).
narrative_ontology:constraint_beneficiary(china_advanced_chip_design_constraint, allied_chip_designers).
narrative_ontology:constraint_victim(china_advanced_chip_design_constraint, chinese_semiconductor_industry).
narrative_ontology:constraint_victim(china_advanced_chip_design_constraint, chinese_research_institutions).
narrative_ontology:constraint_victim(china_advanced_chip_design_constraint, domestic_chinese_tech_companies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHINESE SEMICONDUCTOR RESEARCHERS (SNARE) — Trapped by multilayered export controls, EDA tool access restrictions, and IP licensing barriers. Cannot access state-of-the-art design tools (Cadence, Synopsys) or foundational semiconductor IP without circumventing controls. Career advancement blocked by inability to work on cutting-edge nodes. No legitimate exit pathway within the constraint; continued advancement requires either covert acquisition or permanent career limitation.
constraint_indexing:constraint_classification(china_advanced_chip_design_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CHINESE SEMICONDUCTOR COMPANIES (SNARE) — Constrained by design tool access, talent drain, foundry limitations, and IP licensing costs. Can invest heavily in workarounds (reverse engineering, domestic tool development, acquiring talent) but face sustained extraction: forced to build redundant infrastructure, pay premium prices for restricted goods, and accept permanent technological lag. Exit options exist but carry extreme costs (relocation, complete business restructuring, or accepting inferior product lines).
constraint_indexing:constraint_classification(china_advanced_chip_design_constraint, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: US SEMICONDUCTOR ECOSYSTEM (ROPE) — Benefits from constraint as coordination mechanism: export controls preserve market access, prevent competitive pressure from advanced Chinese designs, and maintain allied supply chain dominance. Experiences the constraint as legitimate coordination among aligned partners rather than extraction — the ecosystem has full exit freedom (could lift controls but chooses not to for strategic coordination).
constraint_indexing:constraint_classification(china_advanced_chip_design_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALLIED CHIP DESIGNERS (NON-US) — Mixed perspective: benefit from reduced Chinese competition and access to US-controlled technology ecosystems, but face extraction through licensing restrictions, supply chain dependencies, and performance limitations imposed through alliance membership. Experience both coordination (access to advanced tools via alliance membership) and extraction (constrained to maintain alliance policy, cannot develop independent advanced designs, cannot access Chinese markets).
constraint_indexing:constraint_classification(china_advanced_chip_design_constraint, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CHINESE GOVERNMENT INDUSTRIAL POLICY (TANGLED ROPE) — State actors experience both coordination and extraction relative to the constraint. Coordination function: unified state capacity to invest in alternative chip ecosystems (SMIC, local tool development, domestic IP) creates genuine collective action toward self-sufficiency. Extraction mechanism: the state must extract from private companies and citizens through subsidies, forced partnerships, IP seizure, and talent mobilization to fund these alternatives. Cannot exit without abandoning industrial policy objectives, but has significant agency and leveraging capacity.
constraint_indexing:constraint_classification(china_advanced_chip_design_constraint, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: EXPORT CONTROL APPARATUS (PITON) — The bureaucratic enforcement machinery (BIS, entity lists, license applications) is largely performative theater: sophisticated actors circumvent controls through gray-market imports, subsidiary companies, and legal arbitrage between jurisdictions. The machinery persists through institutional inertia and political messaging rather than actual enforcement effectiveness. Theater ratio reflects that visible restriction (publicized entity list actions) disguises low actual control efficacy.
constraint_indexing:constraint_classification(china_advanced_chip_design_constraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ALTERNATIVE CHIP ECOSYSTEM DEVELOPMENT (SCAFFOLD) — Chinese state investment in RISC-V, domestic EDA tools (EDA2.0), and fab capacity (SMIC, HiSilicon) represents a structured effort to sunset the constraint through redundant infrastructure. Theater remains moderate because these efforts are genuinely functional (not purely performative). However, the constraint will lose force as alternatives mature — generational timescale for RISC-V and domestic tools to reach feature parity with restricted ecosystem. Sunset clause is implicit: constraint weakens as alternatives become viable.
constraint_indexing:constraint_classification(china_advanced_chip_design_constraint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / PHYSICS VIEW (MOUNTAIN) — From a civilizational perspective, some chip design capability gap is inherent to technological asymmetry: leading-edge semiconductor design requires accumulated infrastructure, talent, supply chains, and institutional knowledge that cannot be instantly replicated. The gap is partly natural (knowledge is sticky). However, the structural data contradicts the mountain classification — the analytical engine will detect this as false naturalization. The gap is maintainable only through active enforcement, not through physical or logical necessity.
constraint_indexing:constraint_classification(china_advanced_chip_design_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_advanced_chip_design_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(china_advanced_chip_design_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_advanced_chip_design_constraint, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_advanced_chip_design_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(china_advanced_chip_design_constraint, TR),
    TR >= 0.70.

:- end_tests(china_advanced_chip_design_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The measurement trajectory (0.45 → 0.58 → 0.68) reflects tightening controls and Chinese response investments. The extractiveness value captures both the direct costs imposed (tool access, IP licensing, foundry delays) and the indirect costs (forced domestic R&D duplication, talent drain, slower product development). At 0.68, the constraint is severe enough to justify snare classification for trapped agents. Suppression (0.72): High. Multiple reinforcing barriers: (1) technical — advanced EDA tools are specialized IP with limited substitutes; (2) legal — export controls backed by sanctions; (3) economic — licensing costs are prohibitive for non-compliant pathways; (4) social — talent mobility restricted through visa and security screening. These mechanisms are not easily surmountable. Theater ratio (0.55): Moderate. The constraint operates through mixed mechanisms: visible enforcement (publicized entity list actions, denied licenses, sanctioned companies) creates theatrical effect, but actual control is substantive (tools are genuinely restricted, foundries genuinely have limited capacity). The moderateness reflects that enforcement is neither fully transparent (actual circumvention rates unknown) nor fully performative (real capability gaps exist).
 *
 * PERSPECTIVAL GAP:
 *   The constraint's classification spans all six types across different observational positions. This reveals fundamental disagreement about what the constraint is: an immutable technological gap that coordinate action around (mountain, false), a coordination mechanism that benefits all parties (rope, US view), an asymmetric mixed arrangement (tangled rope, state/allied view), pure extraction with no coordination value (snare, victim view), a degraded enforcement theater (piton, apparatus view), or a transient problem being solved (scaffold, alternative ecosystem view). No single type is 'correct' — each perspective captures a real aspect of the structure. The engine's role is to make the perspectival divergence explicit and measure it, which enables diagnosis of whether the constraint is actually justified by coordination value or maintained by extraction mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) differs sharply by agent position. Chinese researchers at powerless + trapped: d ≈ 0.95 (full target, maximum f(d) ≈ 1.42). Chinese companies at moderate + constrained: d ≈ 0.75 (high target, f(d) ≈ 1.08). US ecosystem at institutional + arbitrage: d ≈ 0.15 (beneficiary, f(d) ≈ -0.01). Allied designers at organized + constrained: d ≈ 0.55 (symmetric pressure, f(d) ≈ 0.75). Chinese government at powerful + mobile: d ≈ 0.50 (leveraged but constrained, f(d) ≈ 0.65). The directionality spread produces the perspectival gap: high-d agents experience severe effective extraction (χ ≈ ε × f(d) × σ(global) ≈ 0.68 × 1.42 × 1.2 ≈ 1.15 for powerless + trapped); low-d agents experience negative effective extraction (beneficiaries) or symmetric burden (leveraged state actors).
 *
 * MANDATROPHY ANALYSIS:
 *   GEOPOLITICAL CONSTRAINT WITH UNRESOLVED JUSTIFICATION: This constraint resolves mandatrophy at the analytical level by revealing the perspectival core disagreement: Does the constraint serve legitimate coordination (protecting allied supply chains, preventing dual-use leakage) or is it primarily extractive (capturing market advantage, stifling competition)? The US perspective (rope/coordination) and Chinese perspective (snare/extraction) are incommensurable — they cannot both be fully correct. The constraint's mandatrophy is resolved by acknowledging that the justification is *political* (geopolitical competition) rather than *structural* (technological necessity). At ε = 0.68 with suppression = 0.72, the constraint is severe enough to require justification beyond coordination. The evidence that justifies the constraint (dual-use risk, supply chain security) is genuinely present but cannot be cleanly separated from the extractive benefits (market dominance, competitive protection) that accrue to beneficiaries. The engine's detection of this ambiguity — six types across eight perspectives — is the diagnostic output. The constraint is maintainable only through sustained geopolitical enforcement, not through natural coordination advantages.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    control_circumvention_efficacy,
    'What fraction of advanced chip design tools and IP are actually blocked by export controls vs. successfully circumvented through gray markets, re-export, and legal arbitrage?',
    'Intelligence assessment of actual vs. declared technology transfer flows; cross-correlation with Chinese semiconductor capability advances; analysis of entity list effectiveness vs. substitute acquisition routes',
    'If circumvention rate > 60%: controls are primarily theater (piton classification confirmed). If circumvention rate < 20%: controls are effective enforcement (snare classification). At 40-60%: hybrid snare/piton behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_circumvention_efficacy, empirical, 'Actual effectiveness of export controls vs. circumvention routes').

omega_variable(
    alternative_ecosystem_maturation_timeline,
    'At what point do domestic Chinese EDA tools, RISC-V ecosystem, and advanced foundries achieve feature parity with US-controlled alternatives, making the constraint obsolete?',
    'Capability assessment of SMIC node advancement, EDA2.0 tool maturity, HiSilicon design success metrics, and RISC-V market adoption; correlation with generational timescale projection',
    'If maturation timeline < 10 years: scaffold perspective dominates and sunset is structural. If maturation timeline > 20 years: constraint persists beyond generational horizon; scaffold is aspirational. Timeline determines whether to classify as transient (scaffold) or persistent (snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_ecosystem_maturation_timeline, empirical, 'Timeline for alternative ecosystem feature parity with restricted technologies').

omega_variable(
    compliance_vs_coercion_boundary,
    'Is the constraint maintained primarily through voluntary alliance compliance or through coercive sanctions that impose costs on non-compliant actors?',
    'Analysis of voluntary vs. forced compliance by allied chip designers; examination of sanction severity and frequency; measurement of extraction costs imposed on constraint violators',
    'If primarily voluntary (alliance members benefit from participation): stronger rope/scaffold signals. If primarily coercive (sanctions dominate): stronger snare signal. Classification sensitivity to this axis is high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_vs_coercion_boundary, conceptual, 'Whether constraint is maintained by voluntary alliance compliance or coercive sanctions').

omega_variable(
    state_capacity_extraction_costs,
    'How much extraction does the Chinese state levy on private companies and citizens through subsidies, forced partnerships, IP appropriation, and talent mobilization to fund alternative chip ecosystems?',
    'Measurement of state investment levels, private company burden-sharing agreements, IP seizure frequency, and productivity costs from talent mandates; comparison with unsubsidized market chip development costs',
    'If extraction costs exceed alternative ecosystem savings: tangled rope classification weakened. If extraction is sustainable: tangled rope confirmed. High extraction costs signal hidden victims (domestic taxpayers, companies) not directly visible in international trade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_extraction_costs, empirical, 'Domestic extraction costs Chinese state imposes to fund alternative chip ecosystems').

omega_variable(
    talent_circulation_asymmetry,
    'Do brain drain and reverse brain drain balance, or does net talent flow remain permanently asymmetric (more Chinese talent exodus than return)?',
    'Longitudinal tracking of Chinese chip designer emigration vs. repatriation rates; career advancement comparison for equivalent talent in US vs. China positions; survey of return migration incentives',
    'If permanent asymmetry: victim classification for Chinese institutions is robust (talent drain is irreversible extraction). If circulation becomes balanced: constraint weakens because domestic capacity can be built and retained. Talent dynamics are core to why constraint is snare for individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(talent_circulation_asymmetry, empirical, 'Whether talent drain from China to US ecosystem is permanent or reversible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_advanced_chip_design_constraint, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cadvchip_tr_t0, china_advanced_chip_design_constraint, theater_ratio, 0, 0.48).
narrative_ontology:measurement(cadvchip_tr_t5, china_advanced_chip_design_constraint, theater_ratio, 5, 0.52).
narrative_ontology:measurement(cadvchip_tr_t10, china_advanced_chip_design_constraint, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(cadvchip_be_t0, china_advanced_chip_design_constraint, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cadvchip_be_t5, china_advanced_chip_design_constraint, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(cadvchip_be_t10, china_advanced_chip_design_constraint, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_advanced_chip_design_constraint, global_infrastructure).
narrative_ontology:boltzmann_floor_override(china_advanced_chip_design_constraint, 0.18).
narrative_ontology:affects_constraint(china_advanced_chip_design_constraint, semiconductor_supply_chain_vulnerability).
narrative_ontology:affects_constraint(china_advanced_chip_design_constraint, taiwan_fab_dependency).
narrative_ontology:affects_constraint(china_advanced_chip_design_constraint, advanced_ai_training_model_export_controls).

% DUAL FORMULATION NOTE:
% This constraint is upstream of multiple dependent constraints: Taiwan fab dependency, AI training export controls, and semiconductor supply chain vulnerabilities all derive their severity from this constraint. The alternative ecosystem development pathway (RISC-V, domestic EDA, SMIC capacity) will progressively weaken all downstream constraints as it matures. The network exhibits causal hierarchy: breaking or weakening this constraint reduces severity across all dependents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(china_advanced_chip_design_constraint, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
