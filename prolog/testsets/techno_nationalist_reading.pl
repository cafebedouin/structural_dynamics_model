% ============================================================================
% CONSTRAINT STORY: techno_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_techno_nationalist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: techno_nationalist_reading
 *   human_readable: Techno-Nationalist Performance Legitimacy
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   A developmental state grounds its legitimacy in achieving technological
 *   self-sufficiency and global leadership in strategic industries defined as
 *   essential for national security and great-power status. The constraint
 *   operates through directed credit allocation, industrial subsidies, export
 *   controls, supply chain resilience mandates, and regulatory protection for
 *   national champions. The arrangement is presented as necessary
 *   coordination to overcome market failures in long-horizon strategic
 *   investment; critics read it as extractive industrial policy that
 *   subordinates consumer welfare and allocative efficiency to regime
 *   performance narratives. This is one reading of the performance_legitimacy
 *   kernel; sibling readings (quantitative_growth_reading,
 *   qualitative_development_reading, livelihood_security_reading) instantiate
 *   different legitimacy claims from the same kernel with different
 *   beneficiary structures and extraction patterns.
 *
 * KEY AGENTS:
 *   - state_planning_apparatus: Agenda-setter (institutional/analytical) — sets priorities, allocates resources, enforces mandates
 *   - defense_adjacent_tech_sectors: Beneficiary (powerful/constrained) — receive directed investment and protection
 *   - national_champion_firms: Beneficiary (institutional/mobile) — state-linked firms in strategic sectors
 *   - consumer_goods_sectors: Payer (organized/constrained) — bear opportunity cost of diverted capital
 *   - market_driven_entrepreneurs: Payer (moderate/constrained) — crowded out by subsidized strategic sectors
 *   - import_dependent_industries: Payer (organized/trapped) — forced to source from higher-cost domestic suppliers
 *   - general_population: Payer/Beneficiary (powerless/trapped) — bear diffuse costs, receive security and status benefits
 *   - foreign_competitors: Excluded (institutional/mobile) — face barriers justified by national security
 *   - development_economists: Observer (analytical/analytical) — measure performance against alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(techno_nationalist_reading, 0.68).
domain_priors:suppression_score(techno_nationalist_reading, 0.72).
domain_priors:theater_ratio(techno_nationalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(techno_nationalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(techno_nationalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(techno_nationalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(techno_nationalist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(techno_nationalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(techno_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(techno_nationalist_reading, "Techno-Nationalist Performance Legitimacy").
narrative_ontology:topic_domain(techno_nationalist_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(techno_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(techno_nationalist_reading, 'f19b281f-ab02-4662-b86d-2eed840e0a28').
narrative_ontology:cs_kernel_codification('f19b281f-ab02-4662-b86d-2eed840e0a28', formalized).
narrative_ontology:cs_authority_grounding('f19b281f-ab02-4662-b86d-2eed840e0a28', extraction).
narrative_ontology:cs_interpretation_layer_present('f19b281f-ab02-4662-b86d-2eed840e0a28').
narrative_ontology:cs_reading_relation('f19b281f-ab02-4662-b86d-2eed840e0a28', techno_nationalist_reading__quantitative_growth_reading, coexists_with).
narrative_ontology:cs_reading_relation('f19b281f-ab02-4662-b86d-2eed840e0a28', techno_nationalist_reading__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('f19b281f-ab02-4662-b86d-2eed840e0a28', techno_nationalist_reading__livelihood_security_reading, coexists_with).
narrative_ontology:cs_axiom('f19b281f-ab02-4662-b86d-2eed840e0a28', foundational, strategic_autonomy_primacy).
narrative_ontology:cs_axiom_status(strategic_autonomy_primacy, holdable).
narrative_ontology:cs_axiom_grounding('f19b281f-ab02-4662-b86d-2eed840e0a28', strategic_autonomy_primacy, instrumental).
narrative_ontology:cs_axiom('f19b281f-ab02-4662-b86d-2eed840e0a28', foundational, market_failure_in_strategic_sectors).
narrative_ontology:cs_axiom_status(market_failure_in_strategic_sectors, holdable).
narrative_ontology:cs_axiom_grounding('f19b281f-ab02-4662-b86d-2eed840e0a28', market_failure_in_strategic_sectors, empirically_contingent).
narrative_ontology:cs_axiom('f19b281f-ab02-4662-b86d-2eed840e0a28', secondary, technological_sovereignty_as_legitimacy_source).
narrative_ontology:cs_axiom_status(technological_sovereignty_as_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('f19b281f-ab02-4662-b86d-2eed840e0a28', technological_sovereignty_as_legitimacy_source, conventional).
narrative_ontology:cs_reference_frame('f19b281f-ab02-4662-b86d-2eed840e0a28', post_colonial_technological_dependence).
narrative_ontology:cs_drift_state('f19b281f-ab02-4662-b86d-2eed840e0a28', contemporary_great_power_competition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f19b281f-ab02-4662-b86d-2eed840e0a28', '').
narrative_ontology:cs_kernel_id(techno_nationalist_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(techno_nationalist_reading, defense_adjacent_tech_sectors).
narrative_ontology:constraint_beneficiary(techno_nationalist_reading, national_champion_firms).
narrative_ontology:constraint_beneficiary(techno_nationalist_reading, state_planning_apparatus).
narrative_ontology:constraint_victim(techno_nationalist_reading, consumer_goods_sectors).
narrative_ontology:constraint_victim(techno_nationalist_reading, market_driven_entrepreneurs).
narrative_ontology:constraint_victim(techno_nationalist_reading, import_dependent_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(techno_nationalist_reading, general_population).
narrative_ontology:constraint_victim(techno_nationalist_reading, general_population).
narrative_ontology:constraint_vindicates(techno_nationalist_reading, strategic_autonomy_doctrine).
narrative_ontology:constraint_vindicates(techno_nationalist_reading, technological_sovereignty_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets industrial policy priorities, allocates subsidies and credit to strategic sectors, enforces export controls and supply chain mandates. Justifies massive directed investment as necessary for national security and great-power competition. Measures success by technological milestones and global market share in strategic industries rather than consumer welfare or allocative efficiency.
narrative_ontology:constraint_stakeholder(techno_nationalist_reading, state_planning_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Receive preferential credit, subsidies, procurement contracts, and protection from foreign competition. Their technological advancement is treated as a public good regardless of commercial viability. They benefit from being designated as strategic and from the constraint's suppression of market-driven allocation that would redirect resources elsewhere.
narrative_ontology:constraint_stakeholder(techno_nationalist_reading, defense_adjacent_tech_sectors, beneficiary,
    powerful, generational, constrained, national).

% Large state-linked firms in semiconductors, telecommunications, aerospace, and advanced manufacturing. Receive directed investment, regulatory protection, and diplomatic support for global expansion. Their success is presented as proof of regime competence; their failures are absorbed by the state rather than triggering exit.
narrative_ontology:constraint_stakeholder(techno_nationalist_reading, national_champion_firms, beneficiary,
    institutional, generational, mobile, global).

% Bear the opportunity cost of capital and talent diverted to strategic sectors. Credit is rationed away from consumer-facing industries; regulatory attention focuses on strategic autonomy rather than consumer protection or market competition. Their growth is subordinated to the technological sovereignty goal.
narrative_ontology:constraint_stakeholder(techno_nationalist_reading, consumer_goods_sectors, payer,
    organized, biographical, constrained, national).

% Face credit constraints, regulatory barriers, and talent competition from subsidized strategic sectors. Their ventures are evaluated by market signals the planning apparatus explicitly overrides. Exit options are limited by capital controls and by the fact that their skills and networks are nationally specific.
narrative_ontology:constraint_stakeholder(techno_nationalist_reading, market_driven_entrepreneurs, payer,
    moderate, biographical, constrained, national).

% Downstream manufacturers and service providers dependent on imported components or technology. Bear the cost of supply chain resilience mandates that require sourcing from higher-cost domestic suppliers. Their competitiveness is sacrificed to the strategic autonomy goal; they cannot exit because their customer base is domestic.
narrative_ontology:constraint_stakeholder(techno_nationalist_reading, import_dependent_industries, payer,
    organized, biographical, trapped, national).

% Bear diffuse costs through higher consumer prices, foregone consumption, and opportunity cost of public investment diverted from social services to industrial policy. Receive the coordination benefit of national security and the symbolic benefit of great-power status. Their voice in the allocation is mediated entirely through the regime's performance legitimacy claim.
narrative_ontology:constraint_stakeholder(techno_nationalist_reading, general_population, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(techno_nationalist_reading, general_population, beneficiary).

% Face export controls, investment restrictions, and market access barriers justified by national security. Their exclusion is the enforcement object: the constraint's suppression machinery exists to prevent them from competing in strategic sectors on the domestic market or accessing critical supply chains.
narrative_ontology:constraint_stakeholder(techno_nationalist_reading, foreign_competitors, excluded,
    institutional, generational, mobile, global).

% Study whether directed industrial policy achieves technological catch-up more efficiently than market-driven allocation, and at what cost to consumer welfare and allocative efficiency. They measure the constraint's performance against alternative development paths and assess whether the strategic autonomy achieved justifies the extraction.
narrative_ontology:constraint_stakeholder(techno_nationalist_reading, development_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of achieving technological self-sufficiency in strategic industries where market signals alone would not direct sufficient investment due to national security externalities and long time horizons.
% TRANSFER_FUNCTION: Moves capital, credit, talent, and regulatory protection from consumer sectors and market-driven allocation to defense-adjacent technology sectors and national champion firms, as the price of pursuing strategic autonomy and great-power status.
% ABSENT_VOICES: Market-driven entrepreneurs whose ventures are crowded out, consumers whose welfare is subordinated to strategic goals, and foreign competitors whose exclusion is the enforcement target. All three groups would argue for market-driven allocation and open competition but are structurally excluded from the legitimacy conversation.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, capital would flow toward consumer-facing sectors with shorter payback periods, national champion firms would face market discipline and many would contract or fail, import-dependent industries would source globally rather than domestically, and the regime would lose its primary performance legitimacy narrative. The economy would reorganize around market signals rather than strategic autonomy.
% FOUNDING_PROBLEM: Post-colonial or post-conflict states facing technological dependence on former imperial powers or geopolitical rivals, where market-driven development would perpetuate subordinate status in the global hierarchy and leave critical supply chains vulnerable to coercion.
% FOUNDING_PROBLEM_CORROBORATION: The state planning apparatus and national security establishment attest the problem is live and intensifying due to great-power competition and supply chain weaponization. Development economists and consumer advocates attest the founding problem has been substantially solved in many sectors and the constraint now extracts rents beyond what strategic autonomy requires; independent economic analysis shows diminishing returns to directed investment and rising opportunity costs. Legislative oversight bodies and international development institutions provide corroboration from outside the benefiting parties.
narrative_ontology:disappearance_verdict(techno_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(techno_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(techno_nationalist_reading, '046e0a40c34cddf4fff29b8c15f632dbdef31b7a',
    'c6d6880c39ec6bdfedde2a1d41cc00211f451559', '2026-06-11',
    'performance_legitimacy_kernel_decomposition', 'agent/example_platform_commission.json',
    'claude-sonnet-4-20250514', 'temperature=1.0').
narrative_ontology:story_seed(techno_nationalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(techno_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(techno_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(techno_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.68 at interval end) because the constraint diverts resources from market-driven allocation to strategic sectors regardless of commercial viability or consumer welfare, and the gap between strategic autonomy achieved and opportunity cost paid is substantial. Suppression is higher (0.72) because the constraint's persistence depends on actively suppressing market signals, preventing capital flight, and excluding foreign competition through export controls and investment restrictions. Theater ratio is moderate (0.42): the strategic autonomy function is real and some technological catch-up is achieved, but a growing share of enforcement activity defends national champions from market discipline and justifies continued extraction through performance narratives that overstate security benefits. Accessibility collapse is moderate (0.48) because alternative development paths exist and are visible in other economies; resistance is substantial (0.58) because consumer sectors, entrepreneurs, and import-dependent industries bear clear costs and contest the allocation. The measurement series shows extraction and theater rising over the interval as initial catch-up gains diminish and the constraint increasingly serves to protect established beneficiaries.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently: from the planning apparatus's position the arrangement is necessary coordination to achieve strategic autonomy and overcome market failures in long-horizon investment; from the constrained payer seats the same structure operates as enforced extraction that subordinates their welfare to regime performance narratives. The beneficiary seats (defense-adjacent sectors, national champions) experience genuine coordination and subsidy. The engine computes this divergence from the structural data; the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The state planning apparatus is the agenda-setter with analytical exit options (can observe the full structure). Defense-adjacent tech sectors and national champion firms are structural beneficiaries (receive directed investment and protection, d near beneficiary end). Consumer goods sectors, market-driven entrepreneurs, and import-dependent industries are targets (bear opportunity costs and regulatory burdens, constrained or trapped exit, d near target end). The general population sits near symmetric but slightly toward target: genuine coordination benefit from national security, but diffuse costs through foregone consumption and the opportunity cost of public investment diverted from social services. Foreign competitors are excluded rather than coordinated.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy risk: the founding problem (technological dependence threatening national security) may be substantially solved in many sectors while the constraint persists to protect established beneficiaries. The measurement series shows rising extraction and theater over time, consistent with a shift from genuine catch-up coordination to rent protection. The contested founding_problem_status and the mismatch between status and disappearance_verdict (if the problem is dead but the world would rearrange on disappearance, the constraint has become a zombie) flag this for investigation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_autonomy_vs_rent_protection,
    'Is the constraint still primarily serving its founding goal of achieving strategic autonomy in critical technologies, or has it shifted to protecting established national champions from market discipline?',
    'Longitudinal analysis of the gap between strategic autonomy achieved (measured by supply chain resilience, technological capabilities, and reduced foreign dependence) and opportunity cost paid (measured by foregone consumer welfare, allocative inefficiency, and crowding out of market-driven sectors). If the gap is widening over time, the constraint is shifting toward rent protection.',
    'If the constraint has shifted to rent protection, the extraction is no longer justified by the coordination function and the arrangement should reclassify toward snare. If strategic autonomy gains still justify the opportunity cost, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_autonomy_vs_rent_protection, empirical, 'Whether the constraint still serves its founding strategic autonomy goal or has become rent protection for national champions.').

omega_variable(
    kernel_reading_under_determination,
    'Is the techno_nationalist_reading the only defensible framing of performance legitimacy in this context, or do alternative readings (quantitative growth, qualitative development, livelihood security) provide equally coherent accounts with different beneficiary structures?',
    'Cross-reading comparison: if the same regime actions and resource flows can be coherently described under multiple readings with different victim sets and extraction patterns, the kernel is under-determined and reading selection is a framing choice rather than a discovered fact.',
    'If alternative readings are equally coherent, the classification is observer-relative and the constraint family (all readings of the performance_legitimacy kernel) should be analyzed as a set rather than any single reading being treated as the true constraint. If the techno_nationalist_reading uniquely fits the observed structure, it is the correct decomposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'Whether the techno_nationalist_reading is the uniquely correct framing or one of several defensible framings of the same kernel.').

omega_variable(
    national_security_externality_magnitude,
    'What is the true magnitude of the national security externality that justifies overriding market signals in strategic sectors?',
    'Counterfactual analysis comparing security outcomes under market-driven allocation versus directed industrial policy, controlling for other factors. Independent security analysis from outside the benefiting parties assessing whether the level of strategic autonomy achieved could have been reached at lower cost through alternative mechanisms (e.g., targeted procurement, R&D subsidies without market protection).',
    'If the security externality is large, more of the measured extraction is justified coordination cost. If the externality is small or could be addressed through less extractive mechanisms, the constraint is over-suppressing market allocation and the extraction is unjustified rent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(national_security_externality_magnitude, empirical, 'The true size of the national security benefit that justifies the opportunity cost of directed industrial policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(techno_nationalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, techno_nationalist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(tech_tr_t0, observed).
narrative_ontology:measurement(tech_tr_t8, techno_nationalist_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement_basis(tech_tr_t8, observed).
narrative_ontology:measurement(tech_tr_t16, techno_nationalist_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement_basis(tech_tr_t16, observed).
narrative_ontology:measurement(tech_tr_t24, techno_nationalist_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement_basis(tech_tr_t24, observed).
narrative_ontology:measurement(tech_tr_t32, techno_nationalist_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement_basis(tech_tr_t32, observed).
narrative_ontology:measurement(tech_tr_t40, techno_nationalist_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(tech_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, techno_nationalist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(tech_be_t0, observed).
narrative_ontology:measurement(tech_be_t8, techno_nationalist_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement_basis(tech_be_t8, observed).
narrative_ontology:measurement(tech_be_t16, techno_nationalist_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(tech_be_t16, observed).
narrative_ontology:measurement(tech_be_t24, techno_nationalist_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement_basis(tech_be_t24, observed).
narrative_ontology:measurement(tech_be_t32, techno_nationalist_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(tech_be_t32, observed).
narrative_ontology:measurement(tech_be_t40, techno_nationalist_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(tech_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, techno_nationalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(tech_su_t0, observed).
narrative_ontology:measurement(tech_su_t8, techno_nationalist_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement_basis(tech_su_t8, observed).
narrative_ontology:measurement(tech_su_t16, techno_nationalist_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement_basis(tech_su_t16, observed).
narrative_ontology:measurement(tech_su_t24, techno_nationalist_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement_basis(tech_su_t24, observed).
narrative_ontology:measurement(tech_su_t32, techno_nationalist_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(tech_su_t32, observed).
narrative_ontology:measurement(tech_su_t40, techno_nationalist_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(tech_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(techno_nationalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(techno_nationalist_reading, quantitative_growth_reading).
narrative_ontology:affects_constraint(techno_nationalist_reading, qualitative_development_reading).
narrative_ontology:affects_constraint(techno_nationalist_reading, livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the performance_legitimacy kernel. The kernel is the regime's claim that legitimacy derives from delivering measurable performance outcomes. The techno_nationalist_reading instantiates this through strategic industry dominance; sibling readings instantiate it through GDP growth (quantitative_growth_reading), human development (qualitative_development_reading), or employment stability (livelihood_security_reading). Each reading has different beneficiaries, victims, and extraction patterns. They are linked because they are alternative framings of the same legitimacy claim, and the regime's choice of which metric to prioritize determines which reading's constraint structure becomes active.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
