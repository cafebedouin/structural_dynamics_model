% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__mitigation_priority, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_legitimacy__mitigation_priority
 *   human_readable: Mitigation-Priority Climate Legitimacy Frame
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The mitigation-priority frame defines legitimate climate response as
 *   emissions reduction through carbon pricing and technological innovation
 *   while preserving economic growth. Emerging from the 1992 UNFCCC process
 *   and crystallizing in the Kyoto flexibility mechanisms and Paris Agreement
 *   architecture, this frame coordinates global action around net-zero
 *   targets and market-based instruments. It presents itself as pragmatic
 *   coordination (rope) but operates as tangled rope: the coordination
 *   function (aligning mitigation effort globally) is real, but asymmetric
 *   extraction occurs — current elites and green capital capture benefits
 *   while future generations and the climate-vulnerable bear the risks of
 *   technological failure and delayed action. Active enforcement maintains
 *   the frame through IPCC scenario selection, financial regulation (TCFD,
 *   taxonomy), trade rules, and discursive marginalization of alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, 0.68).
domain_priors:suppression_score(climate_response_legitimacy__mitigation_priority, 0.58).
domain_priors:theater_ratio(climate_response_legitimacy__mitigation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, resistance, 0.53).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__mitigation_priority, "Mitigation-Priority Climate Legitimacy Frame").
narrative_ontology:topic_domain(climate_response_legitimacy__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__mitigation_priority, 'a931ccd8-ff09-475a-99d2-da69a3f6ae31').
narrative_ontology:cs_kernel_codification('a931ccd8-ff09-475a-99d2-da69a3f6ae31', distributed).
narrative_ontology:cs_authority_grounding('a931ccd8-ff09-475a-99d2-da69a3f6ae31', extraction).
narrative_ontology:cs_interpretation_layer_present('a931ccd8-ff09-475a-99d2-da69a3f6ae31').
narrative_ontology:cs_reading_relation('a931ccd8-ff09-475a-99d2-da69a3f6ae31', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('a931ccd8-ff09-475a-99d2-da69a3f6ae31', climate_response_legitimacy__degrowth_transformation, influences).
narrative_ontology:cs_axiom('a931ccd8-ff09-475a-99d2-da69a3f6ae31', foundational, technological_decoupling_is_sufficient).
narrative_ontology:cs_axiom_status(technological_decoupling_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('a931ccd8-ff09-475a-99d2-da69a3f6ae31', technological_decoupling_is_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('a931ccd8-ff09-475a-99d2-da69a3f6ae31', foundational, growth_preservation_is_legitimate_imperative).
narrative_ontology:cs_axiom_status(growth_preservation_is_legitimate_imperative, holdable).
narrative_ontology:cs_axiom_grounding('a931ccd8-ff09-475a-99d2-da69a3f6ae31', growth_preservation_is_legitimate_imperative, conventional).
narrative_ontology:cs_reference_frame('a931ccd8-ff09-475a-99d2-da69a3f6ae31', kyoto_paris_market_architecture).
narrative_ontology:cs_drift_state('a931ccd8-ff09-475a-99d2-da69a3f6ae31', post_paris_overshoot_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a931ccd8-ff09-475a-99d2-da69a3f6ae31', '2026-08-15T14:32:00Z').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, current_economic_elites).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, green_technology_sector).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, financial_institutions_carbon_markets).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, oecd_governments).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, global_south_development_aspirations).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, green_growth_feasibility).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, carbon_pricing_efficiency).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, technological_optimism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the policy agenda through think tanks, lobbying, and revolving-door appointments. Benefit from growth-preserving climate policy that protects asset values and profit streams while socializing transition costs. Can relocate capital and operations across jurisdictions to avoid stringent regulation.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, current_economic_elites, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__mitigation_priority, current_economic_elites, beneficiary).

% Capture massive public subsidies, tax credits, and carbon market revenues directed toward renewable deployment, CDR, and efficiency technologies. Their business models depend on the mitigation-priority frame directing capital toward techno-fixes rather than demand reduction. Exit options include pivoting to adjacent sectors or relocating to favorable policy environments.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, green_technology_sector, beneficiary,
    organized, biographical, mobile, global).

% Profit from creating and trading carbon credits, green bonds, transition finance instruments, and climate risk derivatives. The mitigation-priority frame creates entire asset classes. They can arbitrage across regulatory regimes and have the capital mobility to exit unfavorable jurisdictions.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, financial_institutions_carbon_markets, beneficiary,
    institutional, generational, arbitrage, global).

% Administer the policy framework (carbon pricing, innovation subsidies, regulation) and benefit from maintaining economic growth and tax bases while appearing to act on climate. Constrained by voter expectations and international commitments; cannot easily exit the climate governance regime without reputational and trade costs.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, oecd_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__mitigation_priority, oecd_governments, beneficiary).

% Bear the existential risk if technological decoupling fails — locked into whatever climate trajectory current decisions produce. No voice in current decisions, no exit from the planetary system, no ability to organize or retaliate. Their victim status is conditional on the mitigation bet failing, making them a contingent but structurally locked-in victim class.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_legitimacy__mitigation_priority, future_generations).

% Experience immediate climate impacts (extreme heat, flooding, crop failure, displacement) while mitigation-priority policies delay meaningful emissions reduction. Often in global south or marginalized communities within wealthy nations. Trapped by poverty, geography, and political exclusion; cannot migrate easily or influence policy.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, climate_vulnerable_populations, payer,
    powerless, biographical, trapped, global).

% Need carbon space for development but face mitigation-priority pressure to leapfrog fossil fuels with expensive green tech they cannot afford. Structurally excluded from setting the global agenda (no veto in OECD-dominated forums). Constrained exit: can form blocs (G77, AOSIS) but lack enforcement power; some pursue independent fossil development despite pressure.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, global_south_development_aspirations, payer,
    moderate, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__mitigation_priority, global_south_development_aspirations, excluded).

% Advocate for adaptation-priority or degrowth frames, demand reparations and immediate emissions cuts. Excluded from formal decision-making (COP observer status only, no vote). Constrained exit: can mobilize protests, litigation, and direct action but face state repression, media marginalization, and NGO capture. Their exclusion is structural — the mitigation-priority frame defines them as 'unrealistic'.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, climate_justice_movements, excluded,
    organized, biographical, constrained, global).

% Provides the scientific assessment that the mitigation-priority frame selectively cites (e.g., 1.5°C pathways with massive CDR). Scientists within IPCC have diverse views but the institutional output is filtered through government approval processes that favor growth-compatible narratives. Analytical exit: can publish dissenting research but cannot change the policy frame directly.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, ipcc_scientific_body, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global climate action around a single measurable target (net-zero by 2050) using price signals and innovation policy to align decentralized actors without requiring centralized planning or lifestyle change.
% TRANSFER_FUNCTION: Transfers transition costs (carbon taxes, green premiums, stranded asset losses) to current consumers, taxpayers, and workers in carbon-intensive sectors. Transfers climate risk (overshoot, tipping points, CDR failure) to future generations. Transfers profits (subsidies, carbon rents, green finance fees) to green technology sector and financial institutions.
% ABSENT_VOICES: Future generations are temporally excluded — they cannot speak or vote. Global south governments and vulnerable populations are structurally excluded from agenda-setting forums (OECD, G7, IMF, World Bank dominate). Degrowth and adaptation-priority advocates are discursively excluded — labeled 'unrealistic', 'politically impossible', or 'anti-human' in mainstream climate discourse.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority frame vanished overnight, climate policy would reorganize around either adaptation-priority (resilience investment, managed retreat, loss-and-damage finance) or degrowth-transformation (demand reduction, universal basic services, democratic economic planning). The current global architecture of carbon markets, net-zero pledges, and innovation subsidies would dissolve; capital would redirect; international negotiations would shift from 'ambition' to 'survival' or 'justice' frames.
% FOUNDING_PROBLEM: How to achieve meaningful emissions reduction without threatening the legitimacy of the global capitalist order, the growth imperative, or the political power of incumbent elites — i.e., how to make climate action compatible with business-as-usual.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by critical political economy literature (Malm, Hickel, Kallis, Anderson) and by the historical record of climate negotiations (Kyoto flexibility mechanisms, Paris bottom-up architecture, Article 6 carbon markets) — all documented by scholars outside the beneficiary set. Beneficiaries (OECD governments, IEA, green finance) deny this framing, claiming the problem was purely 'how to reduce emissions efficiently'.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_legitimacy__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__mitigation_priority, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the massive risk transfer to future generations (conditional on decoupling failure) and the capture of transition finance by incumbent capital. Suppression (0.58) measures the structural exclusion of adaptation-priority and degrowth frames from decision-making venues — not censorship but agenda-setting power. Theater ratio (0.42) captures the growing gap between net-zero rhetoric and emissions reality: performative pledges increase while atmospheric concentrations accelerate. Accessibility collapse (0.62) reflects how thoroughly the growth-preserving frame has colonized 'serious' climate discourse — alternatives exist but are treated as category errors. Resistance (0.53) is significant from both climate justice movements (demanding more) and fossil interests (demanding less), creating a pincer that the frame manages by absorbing the former's language while delivering the latter's outcomes.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, this is genuine coordination solving a collective action problem (rope). From the payer seats, it is extraction dressed as coordination (snare). From the analytical seat, it is a tangled rope with genuine coordination function but asymmetric risk distribution. The engine computes this divergence; the authored claim (tangled_rope) names the structural reality without adjudicating any single seat's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Agenda-setters (current_economic_elites, oecd_governments) sit at d≈0.15 — they design and benefit from the frame. Beneficiaries (green_tech_sector, financial_institutions) at d≈0.25 — they profit but depend on policy continuity. Payers (climate_vulnerable_populations, global_south) at d≈0.85 — they bear costs with minimal voice. Future_generations at d≈0.95 — trapped, voiceless, bearing existential risk. Observers (ipcc) at d≈0.5 — analytically positioned but institutionally constrained. The derivation follows from power, exit options, and beneficiary/victim declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (making climate action compatible with growth/capitalism) is contested — some argue it was always a cover for delay, others that it was a sincere pragmatic compromise now obsolete. The frame persists because no beneficiary has incentive to change it (elites preserve assets, green tech captures subsidies, finance captures rents) and no victim has power to force change (future generations cannot act, vulnerable populations are excluded). Mandatrophy is unresolved: the coordination function (global mitigation alignment) is real but the growth-preservation mandate has outlived its climate effectiveness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_feasibility,
    'Is absolute decoupling of GDP from emissions at the required rate (7-10% annually) physically and economically feasible without demand reduction?',
    'Empirical test: track whether any major economy achieves sustained absolute decoupling at Paris-compatible rates while growing GDP. Current data shows only relative decoupling or absolute decoupling with outsourced emissions.',
    'If decoupling is infeasible, the mitigation-priority frame is a snare extracting from future generations under false pretenses. If feasible, it is a genuine tangled rope with real coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decoupling_feasibility, empirical, 'Whether the core technological promise of the frame is physically realizable.').

omega_variable(
    cdr_scale_up_risk,
    'Can carbon dioxide removal (CDR) scale to the 5-10 GtCO2/yr assumed in 1.5°C pathways without catastrophic land, water, and energy conflicts?',
    'Monitor CDR deployment (BECCS, DACCS, enhanced weathering) against IPCC pathway requirements; assess land-use competition, energy penalty, and permanence risks.',
    'If CDR cannot scale, net-zero pledges are theatrical and the frame extracts from future generations by pretending overshoot is reversible. This would shift classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cdr_scale_up_risk, empirical, 'Whether the technological backstop the frame depends on is deliverable.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''legitimate climate response'' admit a single coherent framing, or do the three readings (mitigation_priority, adaptation_priority, degrowth_transformation) represent incommensurable paradigms rather than policy options within one framework?',
    'Analyze whether any single governance framework could simultaneously satisfy the core premises of all three readings without logical contradiction.',
    'If readings are incommensurable, the kernel itself is a site of structural contestation, not a stable commitment. This would require modeling the kernel as a distributed authority structure with no adjudicating center.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel admits a unified interpretation or is structurally fragmented.').

omega_variable(
    future_generation_victimhood_conditionality,
    'Are future generations victims only conditionally (if decoupling fails) or structurally (the frame inherently discounts their welfare via discount rates and risk transfer)?',
    'Examine integrated assessment models'' discount rate choices, the treatment of tail risks, and whether any mitigation-priority policy internalizes intergenerational equity as a constraint rather than an aspiration.',
    'If victimhood is structural, the frame is a snare from the intergenerational seat regardless of technological outcomes. If conditional, it is tangled rope with a genuine coordination function that may succeed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(future_generation_victimhood_conditionality, conceptual, 'Whether intergenerational extraction is built into the frame''s architecture or contingent on technological failure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__mitigation_priority, 1992, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crl_mp_tr_t1992, climate_response_legitimacy__mitigation_priority, theater_ratio, 1992, 0.25).
narrative_ontology:measurement(crl_mp_tr_t1997, climate_response_legitimacy__mitigation_priority, theater_ratio, 1997, 0.3).
narrative_ontology:measurement(crl_mp_tr_t2005, climate_response_legitimacy__mitigation_priority, theater_ratio, 2005, 0.33).
narrative_ontology:measurement(crl_mp_tr_t2009, climate_response_legitimacy__mitigation_priority, theater_ratio, 2009, 0.36).
narrative_ontology:measurement(crl_mp_tr_t2015, climate_response_legitimacy__mitigation_priority, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(crl_mp_tr_t2021, climate_response_legitimacy__mitigation_priority, theater_ratio, 2021, 0.4).
narrative_ontology:measurement(crl_mp_tr_t2025, climate_response_legitimacy__mitigation_priority, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(crl_mp_be_t1992, climate_response_legitimacy__mitigation_priority, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement(crl_mp_be_t1997, climate_response_legitimacy__mitigation_priority, base_extractiveness, 1997, 0.42).
narrative_ontology:measurement(crl_mp_be_t2005, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(crl_mp_be_t2009, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2009, 0.52).
narrative_ontology:measurement(crl_mp_be_t2015, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(crl_mp_be_t2021, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2021, 0.64).
narrative_ontology:measurement(crl_mp_be_t2025, climate_response_legitimacy__mitigation_priority, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(crl_mp_su_t1992, climate_response_legitimacy__mitigation_priority, suppression_requirement, 1992, 0.4).
narrative_ontology:measurement(crl_mp_su_t1997, climate_response_legitimacy__mitigation_priority, suppression_requirement, 1997, 0.45).
narrative_ontology:measurement(crl_mp_su_t2005, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement(crl_mp_su_t2009, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2009, 0.5).
narrative_ontology:measurement(crl_mp_su_t2015, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2015, 0.53).
narrative_ontology:measurement(crl_mp_su_t2021, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2021, 0.56).
narrative_ontology:measurement(crl_mp_su_t2025, climate_response_legitimacy__mitigation_priority, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__mitigation_priority, 0.18).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__degrowth_transformation).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, carbon_market_architecture).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, green_innovation_subsidy_regime).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, net_zero_pledge_cascade).

% DUAL FORMULATION NOTE:
% This constraint is one member of the climate_response_legitimacy kernel family. The three readings (mitigation_priority, adaptation_priority, degrowth_transformation) share the kernel 'what counts as legitimate climate response' but instantiate different constraints with different ε, different beneficiary/victim structures, and different types. Mitigation_priority has ε≈0.68 (tangled rope); adaptation_priority likely has lower ε but higher suppression of mitigation discourse; degrowth_transformation likely has high ε from elite perspective but low ε from vulnerable perspective. They are linked here via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_legitimacy__mitigation_priority, institutional, 0.15).
constraint_indexing:directionality_override(climate_response_legitimacy__mitigation_priority, organized, 0.25).
constraint_indexing:directionality_override(climate_response_legitimacy__mitigation_priority, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
