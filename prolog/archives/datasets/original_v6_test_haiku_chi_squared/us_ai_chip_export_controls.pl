% ============================================================================
% CONSTRAINT STORY: us_ai_chip_export_controls
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_ai_chip_export_controls, []).

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
 *   constraint_id: us_ai_chip_export_controls
 *   human_readable: US Export Controls on Advanced AI Chips to China
 *   domain: geopolitical/technological
 *
 * SUMMARY:
 *   US export controls on advanced AI semiconductor chips represent a complex
 *   geopolitical constraint that simultaneously functions as national
 *   security coordination, market protection, and geostrategic extraction.
 *   The controls began in October 2022 with targeted restrictions on
 *   H100/A100-class GPUs and have escalated through multiple rounds of entity
 *   list additions, end-use restrictions, and allied-nation coordination. The
 *   constraint exhibits different structural properties depending on the
 *   observer's position: to Chinese AI developers, it is a snare—complete
 *   exclusion with no legitimate exit. To US semiconductor firms, it is a
 *   coordination mechanism that protects market dominance and enables
 *   first-mover advantage in AI development. To allied chip exporters
 *   (Netherlands, Taiwan, South Korea), it is a tangled rope: they benefit
 *   from US market protection but are constrained by secondary sanctions
 *   threats if they defect. To the US national security apparatus, it is both
 *   coordination (aligning allies on China containment) and extraction
 *   (consolidating US geostrategic leverage). The theater ratio has increased
 *   over time as the enforcement apparatus has grown more complex—the
 *   original clarity of 'block advanced GPUs to China' has become obscured by
 *   exemptions, re-export rules, and entity list ambiguities. This constraint
 *   forms a constraint family with broader technology decoupling mechanisms
 *   and poses a mandatrophy challenge: classifying it as mountain (inherent
 *   to national security) vs. snare (contingent geopolitical extraction)
 *   depends on whether semiconductor dominance is truly necessary to US
 *   security or merely politically convenient.
 *
 * KEY AGENTS:
 *   - Chinese AI Development Sector: Primary victim (powerless/trapped) — completely excluded from cutting-edge supply; cannot source substitutes without accepting permanent technological lag
 *   - US Semiconductor Industry: Primary beneficiary (institutional/arbitrage) — protected market share; exemption access provides arbitrage opportunities; captures supercritical dominance in AI compute
 *   - US National Security State: Primary enforcer (powerful/mobile) — coordinates alliance alignment; consolidates geostrategic leverage; benefits from decoupling strategy but must manage alliance defection risk
 *   - Non-US Chip Exporters (Netherlands, Taiwan, South Korea): Secondary victim/constrained (organized/constrained) — benefit from US market protection but face secondary sanctions pressure to maintain restrictions
 *   - Global AI Governance Coalition: Analytical observer (analytical/analytical) — sees controls as temporary scaffolding; advocates for permanent governance mechanisms with sunset clause
 *   - US Commerce Department Bureaucracy: Institutional enforcer (institutional/constrained) — manages increasingly complex enforcement apparatus; theater rising as clarity declines
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_ai_chip_export_controls, 0.58).
domain_priors:suppression_score(us_ai_chip_export_controls, 0.72).
domain_priors:theater_ratio(us_ai_chip_export_controls, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_ai_chip_export_controls, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_ai_chip_export_controls, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_ai_chip_export_controls, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_ai_chip_export_controls, tangled_rope).
narrative_ontology:human_readable(us_ai_chip_export_controls, "US Export Controls on Advanced AI Chips to China").
narrative_ontology:topic_domain(us_ai_chip_export_controls, "geopolitical/technological").

domain_priors:requires_active_enforcement(us_ai_chip_export_controls).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_ai_chip_export_controls, us_semiconductor_industry).
narrative_ontology:constraint_beneficiary(us_ai_chip_export_controls, us_national_security_apparatus).
narrative_ontology:constraint_beneficiary(us_ai_chip_export_controls, us_ai_companies).
narrative_ontology:constraint_victim(us_ai_chip_export_controls, chinese_ai_development).
narrative_ontology:constraint_victim(us_ai_chip_export_controls, global_semiconductor_market_efficiency).
narrative_ontology:constraint_victim(us_ai_chip_export_controls, chip_export_countries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHINESE AI DEVELOPMENT (SNARE) — Completely trapped by exclusion from cutting-edge semiconductor supply. No legitimate alternative sourcing; cannot exit without accepting permanent technological disadvantage. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.97. Pure extraction from this structural position.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-US SEMICONDUCTOR EXPORTERS (TANGLED ROPE) — Netherlands (ASML), Taiwan (TSMC), South Korea (Samsung) benefit from US-China friction (customers substitute to non-controlled suppliers) but are constrained by US pressure to adopt compatible restrictions. Coordination function: they align standards; extraction function: US extracts compliance with secondary sanctions threat. d≈0.58, f(d)≈0.75, σ=1.2 → χ≈0.52.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: US SEMICONDUCTOR INDUSTRY (ROPE) — Direct beneficiary. Controls are a coordination mechanism protecting market share and enabling supercritical dominance in AI chip supply. Access to exemptions (NVIDIA's authorization for certain sales) provides arbitrage. d≈0.02, f(d)≈-0.13, σ=1.2 → χ≈-0.09. Net beneficiary through market protection.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: US NATIONAL SECURITY STATE (TANGLED ROPE) — Coordination function: controls align with broader decoupling strategy and alliance building (Japan, South Korea, Taiwan align on semiconductor security). Extraction function: controls consolidate US geostrategic power by weaponizing supply chains. Powerful actors have mobility (can shift targets, adjust definitions of 'advanced') but are constrained by alliance management. d≈0.35, f(d)≈0.30, σ=1.2 → χ≈0.21.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: GLOBAL AI GOVERNANCE COALITION (SCAFFOLD) — International AI safety organizations, export control scholars, and multi-stakeholder forums see controls as temporary enforcement scaffolding for the absence of global AI safety norms. Sunset premise: if shared AI safety standards and verification mechanisms mature (chip watermarking, international AI auditing), export controls become unnecessary. d≈0.45, f(d)≈0.50, σ=1.2 → χ≈0.35. High theater (security theater) but with plausible sunset.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: COMMERCE DEPARTMENT ENFORCEMENT (PITON) — The regulatory apparatus (BIS, EAR lists, license review process) is substantially performative. Primary function was clear in 2022-2023 (prevent advanced GPU supply to China); current function obscured by proliferation of exemptions, entity list vagaries, and re-export ambiguities. Theater_ratio=0.48 reflects increasing gap between stated control breadth and actual enforcement clarity. Maintained by institutional inertia and political commitment, not by technical clarity.
constraint_indexing:constraint_classification(us_ai_chip_export_controls, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_ai_chip_export_controls_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_ai_chip_export_controls, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_ai_chip_export_controls, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_ai_chip_export_controls, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_ai_chip_export_controls, TR),
    TR >= 0.70.

:- end_tests(us_ai_chip_export_controls_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): Moderate-high. The constraint extracts significant value from Chinese AI development through technological exclusion, but is not maximum extraction (0.75+) because: (1) China retains options for slower domestic development and substitution through trusted intermediaries, (2) some US companies benefit from exemptions (NVIDIA's authorization), and (3) the controls may ultimately accelerate Chinese indigenous chip development, reducing long-term extraction value. Suppression (0.72): High. The constraint has substantial coercive force—Chinese firms face criminal penalties for unauthorized acquisition, allied exporters face secondary sanctions if they defect, and re-export detection is actively enforced. However, suppression is not total (0.85+) because: (1) enforcement has gaps (re-export through third countries remains possible), (2) some substitution pathways exist (compute-efficient architectures), and (3) the technical definitions are somewhat ambiguous. Theater ratio (0.48): Moderate. Initially low (clear technical definition of controlled chips), but rising as the enforcement apparatus has become more complex—entity lists, end-use restrictions, and exemptions have created ambiguity that obscures the original coordination goal. The trend toward higher theater reflects Goodhart drift: as specific technical controls are circumvented, enforcement expands to indirect measures with lower functional clarity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence driven by structural position and exit options. Chinese AI developers see a snare (trapped, no alternatives). US semiconductor firms see a rope (coordination that benefits them, arbitrage access). National security apparatus sees a tangled rope (coordination with allies + geostrategic extraction). Allied chip exporters see a tangled rope from a different structural position (constrained by sanctions threats despite coordinating). The US Commerce Department sees a piton (the enforcement apparatus is performative—managing an increasingly complex ruleset that may not achieve the original control objective). The global AI governance coalition sees a scaffold (temporary enforcement mechanism pending maturity of shared AI safety standards). This perspectival spread is wider than typical constraints because the structural asymmetry is geopolitical rather than just institutional—national security interests are incommensurable.
 *
 * DIRECTIONALITY LOGIC:
 *   Chinese AI development: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction from this position. US semiconductor industry: Beneficiary + arbitrage → d≈0.02, f(d)≈-0.13. Net beneficiary. Non-US exporters: Victim + constrained (forced alignment) + beneficiary (market share increase from Chinese substitution elsewhere) → d≈0.58, f(d)≈0.75. Mixed, driven by secondary sanctions threat. US national security: Powerful + mobile → d≈0.35, f(d)≈0.30. Low effective extraction from the beneficiary position because powerful actors have alternatives and can redirect focus. Allied government: Institutional + constrained (treaty obligations, sanctions pressure) → d≈0.42, f(d)≈0.45. Moderate extraction from the constrained side. Commerce Department: Institutional + constrained (political commitment, need to maintain enforcement clarity) → d≈0.48, f(d)≈0.62. Piton classification comes from theater gate, not chi.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (extractiveness > 0.70 threshold not reached, but approaching): The critical ambiguity is whether the controls are (a) legitimate national security coordination mechanisms or (b) disguised economic extraction using security framing. The false summit detection appears in the mountain perspective: claims that 'semiconductor dominance is inherently necessary to US security' naturalize a contingent geopolitical choice. The constraint's mandatrophy resolves by acknowledging that BOTH functions are real: (1) genuine national security concern about unrestricted Chinese AI capability development, AND (2) contingent market protection for US semiconductor firms. These are not mutually exclusive—security coordination and industrial policy often align. The tangled rope classification captures this: the controls have a real coordination function (aligning allies, slowing Chinese capability development, reducing near-term AI arms race escalation) AND a real extraction function (protecting US semiconductor market share, consolidating US AI dominance, extracting geostrategic leverage). The theater rise (0.28 → 0.48 over 4 years) indicates that enforcement is becoming increasingly focused on control for its own sake rather than specific technical objectives—classic Goodhart drift. A durable resolution would require: (1) explicit technical thresholds for what constitutes controlled capability, (2) joint international governance mechanisms (not unilateral US restrictions), and (3) sunset clauses tied to development of verification mechanisms for international AI safety standards. Without these, the constraint will drift toward pure snare (enforcement for control's sake) or piton (theatrical maintenance of security theater).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capability_transfer_threshold,
    'What computational capability threshold justifies export controls as legitimate national security vs. economic protectionism?',
    'Analysis of actual AI capabilities achieved by China with controlled vs. uncontrolled chips; comparison with military advantage thresholds; assessment of whether compute-constrained models achieve materially different capabilities than compute-abundant ones',
    'If threshold is military-grade systems: controls are justified security mechanism (rope/scaffold). If threshold is merely ''competitive with US'': controls are protectionism (snare/tangled rope). Current lack of technical clarity means classification depends on who measures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capability_transfer_threshold, empirical, 'Military vs. competitive AI capability threshold for export control justification').

omega_variable(
    supply_chain_substitution_rate,
    'How quickly can China develop indigenous advanced chip production or substitute through trusted suppliers (Israel, UAE, Singapore intermediaries)?',
    'Technical assessment of Chinese SMIC, TSMC-equivalent development timeline; analysis of actual re-export patterns through third countries; measurement of capability gaps in compute-constrained architectures',
    'If rapid substitution: controls are temporary (scaffold) with compressed sunset. If slow substitution: controls are durable extraction (snare/tangled rope). Current uncertainty means confidence in ''generations-long'' advantage is low.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_substitution_rate, empirical, 'Speed of Chinese substitution and re-export circumvention').

omega_variable(
    alliance_fragmentation_risk,
    'Will allied chip exporters maintain US-compatible restrictions or defect to market-share competition with Chinese buyers?',
    'Tracking of Dutch, South Korean, Taiwanese policy divergence from US; economic analysis of foregone revenue from Chinese customers; political pressure from domestic chip industries in allied nations',
    'If fragmentation accelerates: coordination function collapses (rope becomes snare). If alliance holds: tangled rope classification sustained. Current trajectory shows increasing strain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alliance_fragmentation_risk, empirical, 'Likelihood of allied semiconductor exporters defecting to market-share competition').

omega_variable(
    false_summit_natural_law,
    'Is the claim that ''AI semiconductor dominance is inherently necessary to US security'' a natural law or a contingent institutional arrangement?',
    'Comparative analysis: does US security genuinely require permanent semiconductor hegemony, or do security objectives support a multi-polar chip supply with verification mechanisms? Historical precedent: did past US security require permanent dominance in previous technologies (steel, aviation)?',
    'If natural law (mountain): controls are structural inevitability. If contingent (snare/tangled rope): controls are political choices that could be renegotiated. Most likely this is a false summit naturalizing geopolitical choice as immutable constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether semiconductor dominance is inherent to US security vs. contingent institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_ai_chip_export_controls, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usaichip_tr_t0, us_ai_chip_export_controls, theater_ratio, 0, 0.28).
narrative_ontology:measurement(usaichip_tr_t2, us_ai_chip_export_controls, theater_ratio, 2, 0.38).
narrative_ontology:measurement(usaichip_tr_t4, us_ai_chip_export_controls, theater_ratio, 4, 0.48).

% Extraction over time
narrative_ontology:measurement(usaichip_be_t0, us_ai_chip_export_controls, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(usaichip_be_t2, us_ai_chip_export_controls, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(usaichip_be_t4, us_ai_chip_export_controls, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_ai_chip_export_controls, enforcement_mechanism).
narrative_ontology:affects_constraint(us_ai_chip_export_controls, semiconductor_supply_chain_concentration).
narrative_ontology:affects_constraint(us_ai_chip_export_controls, us_china_technology_decoupling).
narrative_ontology:affects_constraint(us_ai_chip_export_controls, international_ai_governance_void).

% DUAL FORMULATION NOTE:
% The export controls represent a single constraint family member distinct from broader technology decoupling and supply chain concentration issues. Controls are the enforcement mechanism downstream of US-China strategic rivalry (higher-level constraint) but upstream of Chinese substitution capabilities and international governance framework development. The ε=0.58 value reflects the intermediate position: extraction is significant (blocks technology transfer) but not complete (substitution remains possible).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_ai_chip_export_controls, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
