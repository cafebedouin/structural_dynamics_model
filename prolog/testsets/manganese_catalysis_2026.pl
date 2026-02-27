% ============================================================================
% CONSTRAINT STORY: manganese_catalysis_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manganese_catalysis_2026, []).

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
 *   constraint_id: manganese_catalysis_2026
 *   human_readable: Manganese-Formate Fuel Pathway Patent and Licensing Architecture
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The manganese-formate fuel pathway represents a high-efficiency CO2
 *   conversion catalytic system with significant potential for
 *   decarbonization, but its deployment is constrained by a complex patent
 *   and licensing architecture that creates structural extraction alongside
 *   genuine coordination benefits. The constraint exhibits the core tension
 *   of innovation policy: patent protection incentivizes R&D investment and
 *   enables institutional licensing arrangements that accelerate
 *   commercialization, but patent clustering, territorial restrictions, and
 *   licensing fragmentation create temporal delays in global deployment that
 *   impose costs on decarbonization timelines and developing-nation energy
 *   transitions. This story decomposes the natural-law framing ('IP
 *   protection is inherent to innovation') into a contingent institutional
 *   arrangement by showing how different structural positions
 *   (decarbonization imperative, competing researchers, patent holders,
 *   organized nations, licensing apparatus) perceive the same technological
 *   constraint very differently — ranging from an extractive snare on climate
 *   action to a temporary coordination scaffold with a political sunset
 *   clause.
 *
 * KEY AGENTS:
 *   - Patent-Holding Institution: Primary beneficiary (institutional/arbitrage) — controls licensing terms, captures technology transfer revenue, enables market access for partners
 *   - Decarbonization Timeline: Primary victim (powerless/trapped) — climate mitigation windows close with licensing delays; no alternative faster pathway exists
 *   - Competing Research Groups: Secondary victim (moderate/constrained) — gain scientific knowledge but cannot freely commercialize competing or complementary catalysts without navigating IP landscape
 *   - Licensed Commercial Developers: Beneficiary (powerful/arbitrage) — gain exclusive market access; benefit from coordination through institutional licensing
 *   - Compulsory Licensing Coalition: Organized agents (organized/constrained) — developing nations, climate NGOs, emergency-response governments building political pressure for TRIPS flexibilities
 *   - Industrial Licensing Apparatus: Institutional actor (institutional/arbitrage) — maintains patent-pool and licensing frameworks; sees own process as degraded relative to deployment speed requirements
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choices (IP policy) as immutable features of innovation systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manganese_catalysis_2026, 0.38).
domain_priors:suppression_score(manganese_catalysis_2026, 0.48).
domain_priors:theater_ratio(manganese_catalysis_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manganese_catalysis_2026, extractiveness, 0.38).
narrative_ontology:constraint_metric(manganese_catalysis_2026, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(manganese_catalysis_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manganese_catalysis_2026, tangled_rope).
narrative_ontology:human_readable(manganese_catalysis_2026, "Manganese-Formate Fuel Pathway Patent and Licensing Architecture").
narrative_ontology:topic_domain(manganese_catalysis_2026, "technological/economic").

domain_priors:requires_active_enforcement(manganese_catalysis_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manganese_catalysis_2026, patent_holding_institutions).
narrative_ontology:constraint_beneficiary(manganese_catalysis_2026, licensed_commercial_developers).
narrative_ontology:constraint_victim(manganese_catalysis_2026, competing_catalyst_research).
narrative_ontology:constraint_victim(manganese_catalysis_2026, open_source_fuel_cell_development).
narrative_ontology:constraint_victim(manganese_catalysis_2026, developing_nation_energy_transition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DECARBONIZATION TIMELINE (SNARE) — Global climate mitigation depends on rapid deployment of efficient CO2 conversion pathways. The manganese-formate catalyst represents a critical efficiency gain, but patent clustering and licensing fragmentation delay adoption. Developing nations and carbon-constrained sectors cannot wait for patent expiration; they face the extraction of delayed deployment. The decarbonization imperative is trapped — no exit option when the window closes.
constraint_indexing:constraint_classification(manganese_catalysis_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING RESEARCH GROUPS (TANGLED ROPE) — Alternative catalytic pathways (nickel-based, cobalt-based, heterogeneous carbon-supported) benefit from improved understanding of the manganese mechanism. Publication of structural data and reaction kinetics advances the entire field. However, patent restrictions on commercial deployment create asymmetric benefits: discovery groups gain knowledge but cannot deploy without licensing. Constrained exit — cannot freely commercialize without navigating IP landscape.
constraint_indexing:constraint_classification(manganese_catalysis_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PATENT-HOLDING INSTITUTION (ROPE) — Licenser coordinates technology transfer to commercial partners. Licensing agreements enable manufacturers to deploy the catalyst at scale. The institution experiences this as a coordination mechanism: they control the IP, manufacturers gain market access, and the public gains CO2 reduction. Net beneficiary — extraction runs toward this agent through licensing fees and equity stakes.
constraint_indexing:constraint_classification(manganese_catalysis_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPULSORY LICENSING COALITION (SCAFFOLD) — Organized actors (developing nations, climate-focused NGOs, emergency-response governments) view the patent as a temporary bottleneck. TRIPS flexibilities, march-in rights, and climate emergency statutes create a sunset path: if global carbon targets are not met by 2035, patent enforcement becomes politically untenable. International pressure for technology transfer is building coalitions around compulsory licensing. Low effective extraction because organized agents see a structural exit path with a clear temporal horizon.
constraint_indexing:constraint_classification(manganese_catalysis_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INDUSTRIAL LICENSING APPARATUS (PITON) — Traditional patent-pool management and cross-licensing frameworks that worked for mid-20th-century pharmaceuticals and semiconductors are maintaining a licensing ritual for green energy technologies despite increasingly brittle legitimacy. The machinery persists through institutional inertia and network lock-in, but its actual function in accelerating energy transition is degraded. Theater ratio reflects that much licensing negotiation is performative: the underlying technology is known, but legal theater determines deployment speed.
constraint_indexing:constraint_classification(manganese_catalysis_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some IP protection is inherent to innovation incentives: without exclusivity periods, R&D investment in energy catalysis becomes uneconomical at scale. The patent system is a natural law of how advanced technologies disseminate. However, the structural data contradicts this framing — patent clustering and licensing fragmentation are contingent institutional arrangements, not inherent to innovation. The engine's false summit detector will identify this as naturalization of policy choice.
constraint_indexing:constraint_classification(manganese_catalysis_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manganese_catalysis_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(manganese_catalysis_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(manganese_catalysis_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(manganese_catalysis_2026, TR),
    TR >= 0.70.

:- end_tests(manganese_catalysis_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, reflecting that the manganese pathway creates both genuine innovation incentives and temporal deployment delays. The base extractiveness is not as high as a pure IP rent-seeking scenario (which might score 0.65+) because the constraint exhibits real coordination function — institutional licensing does accelerate some commercial deployment relative to academic-only dissemination. However, it is higher than a pure rope (0.05-0.35) because licensing fragmentation, territorial restrictions, and patent clustering create delays that impose global costs. The trajectory over the 6-year measurement window (0.22 → 0.38) reflects increasing tension: as alternative catalysts emerge and climate urgency rises, the opportunity cost of patent-driven delays increases. Suppression (0.48): Moderate-high. Legal barriers (patent enforcement, licensing negotiation requirements, territorial restrictions) create significant obstacles to independent deployment, particularly in developing nations. However, suppression is not maximal (0.70+) because some actors (patent holders, licensed developers) have low barriers, and compulsory licensing mechanisms provide a political bypass route, however uncertain. Theater ratio (0.58): Moderate-high. Patent licensing negotiations for green energy technologies involve substantial performative elements — lengthy legal review, IP due diligence, territory-by-territory negotiation — that are not strictly necessary for technical deployment but are required for legal compliance. The licensing apparatus maintains rituals (patent pools, cross-licensing frameworks) inherited from pharmaceutical and semiconductor industries that have questionable fit to energy transition urgency.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how patent policy can produce radically different experienced types from a single technological system. The patent holder sees rope — they are solving the technology transfer coordination problem. The decarbonization timeline sees snare — the patent creates hard dependency with no exit. Competing researchers see tangled rope — genuine coordination benefits (knowledge spillover) paired with asymmetric extraction (cannot commercialize freely). The organized coalition sees scaffold — a temporary institutional arrangement with a clear political sunset. The licensing apparatus sees piton — its own mechanisms are degraded relative to deployment urgency but persist through institutional inertia. The civilizational observer risks seeing a mountain — innovation incentives are natural laws — but the structural data reveals this as policy choice subject to mandate: the patent system could be configured with shorter exclusivity windows, broader compulsory licensing triggers, or developing-nation carve-outs. The perspectival gap is not about disagreeing on facts but about how temporal urgency (climate mitigation windows), structural position (powerless vs. institutional), and exit options (trapped vs. arbitrage) reshape the experienced constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The chi formula χ = ε × f(d) × σ(S) applies scope scaling because this constraint operates globally but with differentiated access. Global scope (σ=1.2) amplifies perceived extraction for agents who cannot negotiate licensing terms. The decarbonization timeline (powerless/trapped, d≈0.95) experiences high f(d)≈1.42, making effective extraction very high despite moderate base ε — this agent sees the constraint as maximally binding. Patent holders (institutional/arbitrage, d≈0.05) experience negative f(d)≈-0.12, making effective extraction negative (they are subsidized by the system). Competing researchers (moderate/constrained, d≈0.55) experience moderate f(d)≈0.75, making the constraint feel moderately extractive despite moderate ε. The organized coalition (organized/constrained, d≈0.40) experiences f(d)≈0.40, reducing effective extraction because they have agency and perceive an exit path (compulsory licensing).
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint satisfies all tangled rope gates: (1) ε=0.38 is in the range 0.30-0.46 for hybrid constraints; (2) suppression=0.48 is in the required range ≥0.40; (3) beneficiaries (patent-holding institutions, licensed developers) are explicitly declared and correspond to agents who perceive rope classification; (4) victims (decarbonization timeline, competing researchers, developing-nation access) are explicitly declared and correspond to agents who perceive snare or higher-extraction classification; (5) requires_active_enforcement is true because patent enforcement and licensing agreements actively maintain the constraint. The mandatrophy is resolved by showing that the constraint is genuinely hybrid — it solves a real coordination problem (technology transfer from research to commercial deployment) while simultaneously enabling asymmetric extraction (delayed global access, developing-nation premium pricing). Naturalizing this as 'inherent to innovation' (mountain classification) would be false; alternative institutional designs (shorter patents, compulsory licensing thresholds, technology transfer mandates for publicly funded research) would shift the classification toward rope or scaffold. The snare and scaffold perspectives are not errors but legitimate structural observations from different agent positions with different temporal and geographic constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catalyst_superiority_boundary,
    'Is manganese-formate genuinely superior to competing pathways, or does its advantage derive primarily from IP consolidation and early-mover licensing position?',
    'Energy return on investment (EROI) analysis; efficiency comparison across all catalytic pathways at identical deployment scale; decoupling efficiency gains from licensing advantage through patent-free catalyst studies',
    'If genuinely superior: licensing delays represent real coordination costs justified by incentive protection. If advantage is primarily institutional: licensing structure is pure extraction masquerading as innovation incentive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(catalyst_superiority_boundary, empirical, 'Whether manganese superiority is catalytic or institutional').

omega_variable(
    developing_nation_bypass_capacity,
    'Can developing nations deploy alternative catalysts at sufficient efficiency without the patented manganese pathway, or is the technology bottleneck so tight that licensing becomes mandatory?',
    'Performance mapping of cobalt, nickel, and iron catalysts at deployment scale in low-resource settings; cost-benefit analysis of patent licensing vs alternative pathway development timelines',
    'If bypass possible: snare classification is too severe — victims have constrained but real exit options. If bypass is blocked: snare classification is accurate — patent clustering creates hard dependency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developing_nation_bypass_capacity, empirical, 'Whether bypass catalysts are developmentally viable').

omega_variable(
    compulsory_licensing_political_trigger,
    'What carbon-reduction or climate-emergency threshold triggers compulsory licensing enforceability, and are current institutional frameworks sufficient to activate it?',
    'Analysis of TRIPS flexibilities, precedent from pharmaceutical compulsory licensing (COVID-19, generic ARVs), international climate commitment enforcement mechanisms; modeling of political will as a function of carbon target miss',
    'If trigger exists and is clear: scaffold classification is structural reality with real sunset timeline. If trigger is vague or absent: scaffold is aspirational framing and the constraint remains closer to tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compulsory_licensing_political_trigger, empirical, 'Political conditions for compulsory licensing activation').

omega_variable(
    licensing_negotiation_friction_cost,
    'What percentage of formate-pathway deployment delay is due to actual technical barriers vs. patent licensing negotiation cycles?',
    'Case study analysis of time-to-deployment for licensed vs. unlicensed catalytic pathways; measurement of licensing negotiation duration relative to manufacturing scale-up timeline; comparison with pre-patent technology transfer timelines',
    'If negotiation cost is high: theater_ratio is accurate and suppression derives from legal friction rather than technical bottlenecks. If negligible: theater_ratio is overstated and the constraint is closer to pure rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_negotiation_friction_cost, empirical, 'Licensing friction as bottleneck component').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manganese_catalysis_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mng_cat_tr_t0, manganese_catalysis_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mng_cat_tr_t3, manganese_catalysis_2026, theater_ratio, 3, 0.48).
narrative_ontology:measurement(mng_cat_tr_t6, manganese_catalysis_2026, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(mng_cat_be_t0, manganese_catalysis_2026, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(mng_cat_be_t3, manganese_catalysis_2026, base_extractiveness, 3, 0.3).
narrative_ontology:measurement(mng_cat_be_t6, manganese_catalysis_2026, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manganese_catalysis_2026, resource_allocation).
narrative_ontology:affects_constraint(manganese_catalysis_2026, hydrogen_fuel_cell_deployment).
narrative_ontology:affects_constraint(manganese_catalysis_2026, developing_nation_clean_energy_access).
narrative_ontology:affects_constraint(manganese_catalysis_2026, catalytic_pathway_research_portfolio).

% DUAL FORMULATION NOTE:
% The manganese-formate pathway decomposes into two structurally distinct claims: (1) CATALYTIC EFFICIENCY (ε≈0.08, Mountain) — manganese is a high-efficiency CO2-to-formate catalyst from first-principles thermochemistry. This is well-established and benefits all perspectives. (2) PATENT LICENSING ARCHITECTURE (ε≈0.38, Tangled Rope) — institutional IP protection on the manganese pathway creates extraction alongside coordination. The two stories are linked by network.affects_constraints: the catalytic superiority enables the licensing extraction. Without genuine catalytic advantage, the licensing constraint would collapse (false extraction). The decomposition allows analysis of whether delays derive from technical bottlenecks or institutional ones.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manganese_catalysis_2026, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
