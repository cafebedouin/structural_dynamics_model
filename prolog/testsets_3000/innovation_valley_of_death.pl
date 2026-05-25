% ============================================================================
% CONSTRAINT STORY: innovation_valley_of_death
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_innovation_valley_of_death, []).

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
 *   constraint_id: innovation_valley_of_death
 *   human_readable: Innovation Valley of Death: The Chasm Between Research Success and Commercialization
 *   domain: innovation_policy/technology_transfer/entrepreneurship
 *
 * SUMMARY:
 *   The innovation valley of death represents the capital chasm between
 *   successful research/proof-of-concept and commercial viability.
 *   Technologies that function at prototype stage often cannot attract
 *   institutional capital for scale-up because the risk profile is too high
 *   for traditional venture investors yet the opportunity is too unproven for
 *   later-stage capital. This constraint exhibits both genuine coordination
 *   failure (the valley solves the problem of identifying which high-risk
 *   technologies survive) and extractive gatekeeping (concentrated venture
 *   capital captures surplus value during the crossing). The boundary between
 *   these mechanisms is empirically ambiguous — hence the Tangled Rope
 *   classification. The extractiveness has increased over the 10-year
 *   interval (0.38 → 0.52) as capital concentration increased and stage-gate
 *   processes became more performative relative to actual risk assessment.
 *   Theater ratio increased (0.42 → 0.58) as due diligence procedures became
 *   more elaborate without improving selection accuracy, indicating Goodhart
 *   drift: the performance metric (funding round completion, investor
 *   enthusiasm) is substituting for the goal (identifying viable
 *   innovations).
 *
 * KEY AGENTS:
 *   - Early-Stage Founders: Primary victims (powerless/trapped) — face maximum suppression from the capital requirement barrier; no exit without capital
 *   - Novel Technology Developers: Secondary victims (moderate/constrained) — benefit from ecosystem but face high dilution and acquirer pressure at unfavorable terms
 *   - Incumbent Industry: Primary beneficiaries (institutional/arbitrage) — gatekeep adjacent innovation, acquire successful startups at distress valuations, maintain market dominance
 *   - Risk-Averse Capital Allocators: Secondary beneficiaries (institutional/arbitrage) — avoid concentration risk through stage-gating, extract fees at each round, benefit from selection mechanism
 *   - Innovation Policy Coalition: Organized disruptors (organized/mobile) — government venture capital, SBIR/STTR programs, university tech transfer offices building alternative pathways
 *   - Traditional VC Model: Institutional ritual (institutional/constrained) — maintains gatekeeping through performative due diligence; sees own process as degraded but perpetuates it through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent institutional arrangement as inherent to innovation itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(innovation_valley_of_death, 0.52).
domain_priors:suppression_score(innovation_valley_of_death, 0.65).
domain_priors:theater_ratio(innovation_valley_of_death, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(innovation_valley_of_death, extractiveness, 0.52).
narrative_ontology:constraint_metric(innovation_valley_of_death, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(innovation_valley_of_death, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(innovation_valley_of_death, tangled_rope).
narrative_ontology:human_readable(innovation_valley_of_death, "Innovation Valley of Death: The Chasm Between Research Success and Commercialization").
narrative_ontology:topic_domain(innovation_valley_of_death, "innovation_policy/technology_transfer/entrepreneurship").

domain_priors:requires_active_enforcement(innovation_valley_of_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(innovation_valley_of_death, incumbent_industry).
narrative_ontology:constraint_beneficiary(innovation_valley_of_death, risk_averse_capital_allocators).
narrative_ontology:constraint_beneficiary(innovation_valley_of_death, research_institutions_with_legacy_funding).
narrative_ontology:constraint_victim(innovation_valley_of_death, early_stage_founders).
narrative_ontology:constraint_victim(innovation_valley_of_death, novel_technology_developers).
narrative_ontology:constraint_victim(innovation_valley_of_death, innovation_ecosystem_dynamism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY-STAGE FOUNDER (SNARE) — Trapped between demonstrating viability (requires capital) and securing capital (requires demonstrated viability). Faces maximum suppression: institutional investors avoid valley-of-death risk, lending gatekeepers require collateral, talented employees fear equity dilution. No exit option without abandoning the innovation. Bears full extraction cost.
constraint_indexing:constraint_classification(innovation_valley_of_death, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NOVEL TECHNOLOGY DEVELOPER (TANGLED ROPE) — Constrained by resource requirements and technical risk, but benefits from shared infrastructure, knowledge spillovers, and potential exit acquisitions. The valley of death provides genuine coordination (ecosystem support, access to expertise) alongside asymmetric extraction (dilution, acquisition pressure at unfavorable terms). Significant agency but high cost.
constraint_indexing:constraint_classification(innovation_valley_of_death, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT INDUSTRY GATEKEEPERS (ROPE) — Benefits from the valley of death as a coordination mechanism: risky innovations are filtered, disruptive competitors are starved of capital, licensing deals occur at distress prices. Experiences the constraint as pure coordination — risk is distributed, surplus is captured. Net beneficiary with complete exit optionality.
constraint_indexing:constraint_classification(innovation_valley_of_death, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INNOVATION POLICY COALITION (SCAFFOLD) — Organized actors (government venture funds, innovation agencies, university tech transfer offices) see the valley as a temporary market failure with a sunset clause. SBIR/STTR grants, regional innovation hubs, and public venture capital are building parallel pathways that reduce the valley's extraction mechanism. Has agency and perceives an exit strategy through institutional innovation.
constraint_indexing:constraint_classification(innovation_valley_of_death, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: TRADITIONAL VC MODEL (PITON) — The institutional venture capital gatekeeping ritual persists through inertia despite structural obsolescence. Stage-gate funding processes, due diligence theater, and fund-raising performance metrics are largely performative relative to actual innovation success prediction. The model maintains its gate position through institutional lock-in, not through functional superiority. Theater ratio high; functional verification low.
constraint_indexing:constraint_classification(innovation_valley_of_death, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the valley of death may appear as an immutable property of innovation: novel technologies always face capital gaps because uncertainty is irreducible and risk capital is scarce. However, this naturalizes what is structurally a contingent institutional arrangement combining capital scarcity, risk asymmetry, and gatekeeping power concentration. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(innovation_valley_of_death, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(innovation_valley_of_death_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(innovation_valley_of_death, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(innovation_valley_of_death, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(innovation_valley_of_death, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(innovation_valley_of_death, TR),
    TR >= 0.70.

:- end_tests(innovation_valley_of_death_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The valley represents real capital scarcity (legitimate coordination problem) but also real gatekeeping concentration (legitimate extraction mechanism). Capital is rationed not just by fundamental scarcity but by institutional preference for stage-gated risk reduction, which concentrates decision-making power. The 0.52 value reflects that roughly half the capital allocation is justified by risk management and half is unjustified gatekeeping premium. Suppression (0.65): High. Multiple reinforcing barriers lock founders into the valley: institutional investors avoid it, lending gatekeepers require collateral (circular: the valley risk makes collateral unavailable), talent acquisition is difficult (equity dilution signals risk), and strategic partnerships often require equity concessions. The suppression score reflects that barriers are multiple and reinforcing but not totalizing — alternative funding routes exist but are substantially harder. Theater ratio (0.58): Moderate-high. VC due diligence processes include substantial performative elements — term sheet negotiations, founder presentations, investor update theatrics — that don't predictably correlate with innovation success. However, some genuine risk assessment occurs. The 0.58 value reflects that roughly 58% of the gating process is performative ritual while 42% is functional risk evaluation. The trend from 0.42 to 0.58 indicates Goodhart drift: as capital becomes more concentrated, the theater becomes more elaborate.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural phenomenon — the capital gap between prototype and scale — appears as Snare (powerless founder), Tangled Rope (moderate developer), Rope (incumbent beneficiary), Scaffold (policy reformer), Piton (ritual gatekeeper), and Mountain (natural law) depending on observer position. This perspectival range is diagnostic: it confirms that the valley is institutionally contingent, not natural. A true Mountain would produce uniform classification across all perspectives. A true Rope would show consistent benefits for all parties. The divergence indicates mixed extraction and coordination with power asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to capital flow. Founders face maximum directionality (d ≈ 0.95): trapped at the bottleneck, bearing full suppression cost. Technology developers are moderate-to-high (d ≈ 0.70): constrained exit options, mixed beneficiary/victim status. Incumbent firms are low (d ≈ 0.10): arbitrage exit options, full beneficiary status — capital concentration grants them advantageous deal terms. The staged f(d) transformation produces the observed chi pattern: founders experience high effective extraction despite moderate base extractiveness (because f(d) ≈ 1.42 for trapped agents); incumbents experience negative effective extraction (because f(d) ≈ -0.12 for beneficiaries with arbitrage). The geographic scope modifier (national = σ ≈ 1.0) indicates the valley is primarily a domestic ecosystem effect, though global capital flows are increasing this scope modifier.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by disambiguating the innovation valley into two distinct components: (1) capital market friction (coordination problem, legitimate if it's information asymmetry or genuine risk concentration); (2) gatekeeping extraction (power concentration that allows incumbents to suppress adjacent innovation). The Tangled Rope classification holds when both components are present and substantial. The classification would shift to pure Rope if the valley-of-death risk were actually information-driven (would dissolve with better signaling). It would shift to pure Snare if the valley were entirely extractive gatekeeping with no genuine risk coordination. The current evidence supports Tangled Rope with extractiveness trending higher over the measurement interval, suggesting the gatekeeping component is strengthening relative to the coordination component.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_failure_vs_capital_starvation,
    'When a promising technology fails to scale, is it because the technology was unviable (legitimate selection) or because capital starvation prevented it from reaching proof-of-concept (extraction mechanism)?',
    'Post-hoc analysis of failed startups: comparison of survival rates when alternative funding routes (angel investors, crowdfunding, government grants, strategic partnerships) were available vs unavailable; forensic analysis of failures that occurred at specific development stages (seed, Series A, Series B); counterfactual modeling of technologies that succeeded despite early capital constraints',
    'If technology failure dominates: valley is high-barrier-to-entry selection mechanism (extraction is justified). If capital starvation dominates: valley is pure extraction mechanism (classification shifts toward Snare). Mixed outcome: valley is mixed selection and extraction (Tangled Rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_failure_vs_capital_starvation, empirical, 'Attribution of technology failure to viability vs capital starvation').

omega_variable(
    network_effects_vs_gatekeeping,
    'Do concentrated venture capital networks (Silicon Valley, Sand Hill Road, London fintech) provide genuine network effects and knowledge transfer (coordination benefit) or do they primarily enforce gatekeeping power that extracts surplus from founders?',
    'Comparative analysis of startup outcomes in concentrated vs distributed funding ecosystems; measurement of knowledge transfer effectiveness in concentrated vs decentralized networks; analysis of post-acquisition value capture (do founders retain a proportional share of value created or do acquirers extract most gains?); geographic analysis of where successful innovations originate and what funding path they followed',
    'If network effects dominate: gatekeeping power is justified by coordination benefits (Rope classification holds). If gatekeeping dominates: the valley is primarily extractive (Snare classification strengthens). Mixed: Tangled Rope classification confirmed with clear separation of coordination vs extraction components.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_effects_vs_gatekeeping, empirical, 'Whether VC networks provide network effects or enforce gatekeeping extraction').

omega_variable(
    alternative_funding_sustainability,
    'Can alternative funding mechanisms (government grants, corporate venture, strategic angels, crowdfunding, international capital) substitute for traditional VC at the valley-of-death stage, or do they have structural limitations that preserve the valley?',
    'Longitudinal tracking of cohorts funded through alternative mechanisms vs traditional VC; measurement of survival rates, scaling outcomes, and median time-to-exit; analysis of failure modes in each pathway; identification of technologies or sectors where alternatives work vs where traditional VC remains dominant',
    'If alternatives are viable: scaffold perspective is confirmed — the valley has a sunset as alternatives mature. If alternatives are limited to specific sectors: the valley persists for most innovations (classification remains Snare/Tangled Rope). If alternatives are equally extractive: the valley is structural to innovation itself (false mountain confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_funding_sustainability, empirical, 'Whether alternative funding can substitute for traditional VC at valley-of-death stage').

omega_variable(
    founder_extraction_vs_innovation_rent,
    'How much of the founder''s dilution during the valley represents extraction (gatekeeping premium) vs legitimate innovation risk premium (founders accept dilution because the technology is actually high-risk)?',
    'Analysis of founder equity retention across ventures with different risk profiles and different capital sources; comparison of dilution rates for similar-risk innovations funded through alternative vs traditional VC pathways; measurement of success probability relative to dilution amount — if dilution tracks risk accurately (high-risk ventures dilute more), extraction is minimal; if dilution tracks gatekeeping power (concentrated capital sources dilute more regardless of risk), extraction is high',
    'If extraction dominates: suppression is structural (founders have no choice). If risk premium dominates: suppression is justified by actual uncertainty. Mixed outcome clarifies the tangled rope structure — legitimate selection mixed with extractive gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founder_extraction_vs_innovation_rent, empirical, 'Attribution of founder dilution to innovation risk vs extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(innovation_valley_of_death, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ivod_tr_t0, innovation_valley_of_death, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ivod_tr_t5, innovation_valley_of_death, theater_ratio, 5, 0.51).
narrative_ontology:measurement(ivod_tr_t10, innovation_valley_of_death, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(ivod_be_t0, innovation_valley_of_death, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ivod_be_t5, innovation_valley_of_death, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ivod_be_t10, innovation_valley_of_death, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(innovation_valley_of_death, resource_allocation).
narrative_ontology:affects_constraint(innovation_valley_of_death, venture_capital_concentration).
narrative_ontology:affects_constraint(innovation_valley_of_death, technology_transfer_friction).
narrative_ontology:affects_constraint(innovation_valley_of_death, startup_founder_dilution_cycles).

% DUAL FORMULATION NOTE:
% The innovation valley of death decomposes into capital scarcity (coordination problem, Mountain or Rope depending on whether scarcity is real or manufactured) and gatekeeping concentration (extraction mechanism, Snare or Tangled Rope depending on whether gatekeeping is justified by risk assessment or unjustified power). This story represents the combined phenomenon. Decomposition into separate scarcity and gatekeeping stories would require distinct ε values reflecting the measurement basis — scarcity ε ≈ 0.15 (Mountain), gatekeeping ε ≈ 0.65 (Snare). The present story at ε = 0.52 represents the empirical blending of both mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(innovation_valley_of_death, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
