% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__livelihood_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__livelihood_security_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: performance_legitimacy__livelihood_security_reading
 *   human_readable: Performance Legitimacy — Livelihood Security Reading
 *   domain: political economy / development planning / state capitalism
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   performance-legitimacy kernel: legitimacy grounded specifically in
 *   livelihood security — employment, healthcare, education, elderly care as
 *   directly felt goods. Under this reading, the state's claim to rule rests
 *   on the household's felt material condition, not on GDP growth statistics
 *   (quantitative_growth_reading), not on structural upgrading toward
 *   innovation-intensive production (qualitative_development_reading), and
 *   not on strategic-industry self-sufficiency (techno_nationalist_reading).
 *   The structural delta this reading produces is real and specific: fiscal
 *   transfer formulas reprioritize consumption support and social insurance
 *   over capital formation, redistribution mechanisms strengthen, and the
 *   coordination function shifts from financing investment-led growth to
 *   financing felt welfare improvement. This has a genuine beneficiary set
 *   (service sectors, households receiving direct transfers) and a genuine
 *   victim set (capital-intensive industrial expansion, local government
 *   infrastructure financing) — the same fiscal capacity cannot fund both an
 *   investment-led and a consumption-led legitimacy claim at the margin,
 *   which is why this reading and the quantitative_growth_reading are in real
 *   tension over resource allocation even though both are packaged under one
 *   performance-legitimacy label in ordinary political discourse.
 *
 * KEY AGENTS:
 *   - central_fiscal_transfer_administrators: agenda-setter administering the reallocation formula
 *   - urban_households and elderly_care_recipients: primary felt beneficiaries of the livelihood-security framing
 *   - capital_goods_manufacturers and local_government_infrastructure_bureaus: structural payers as investment financing is deprioritized
 *   - independent_development_economists: analytical observers assessing whether the reallocation is sustainable or merely deferred crisis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, 0.58).
domain_priors:suppression_score(performance_legitimacy__livelihood_security_reading, 0.62).
domain_priors:theater_ratio(performance_legitimacy__livelihood_security_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__livelihood_security_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__livelihood_security_reading, "Performance Legitimacy — Livelihood Security Reading").
narrative_ontology:topic_domain(performance_legitimacy__livelihood_security_reading, "political economy / development planning / state capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__livelihood_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__livelihood_security_reading, 'be164180-c2b8-4cf6-8673-f2a0f9b596b3').
narrative_ontology:cs_kernel_codification('be164180-c2b8-4cf6-8673-f2a0f9b596b3', distributed).
narrative_ontology:cs_authority_grounding('be164180-c2b8-4cf6-8673-f2a0f9b596b3', extraction).
narrative_ontology:cs_interpretation_layer_present('be164180-c2b8-4cf6-8673-f2a0f9b596b3').
narrative_ontology:cs_reading_relation('be164180-c2b8-4cf6-8673-f2a0f9b596b3', performance_legitimacy__quantitative_growth_reading, coexists_with).
narrative_ontology:cs_reading_relation('be164180-c2b8-4cf6-8673-f2a0f9b596b3', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('be164180-c2b8-4cf6-8673-f2a0f9b596b3', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('be164180-c2b8-4cf6-8673-f2a0f9b596b3', foundational, felt_household_welfare_is_the_legitimacy_referent).
narrative_ontology:cs_axiom_status(felt_household_welfare_is_the_legitimacy_referent, holdable).
narrative_ontology:cs_axiom_grounding('be164180-c2b8-4cf6-8673-f2a0f9b596b3', felt_household_welfare_is_the_legitimacy_referent, conventional).
narrative_ontology:cs_axiom('be164180-c2b8-4cf6-8673-f2a0f9b596b3', secondary, consumption_and_social_insurance_take_fiscal_priority_over_capital_formation).
narrative_ontology:cs_axiom_status(consumption_and_social_insurance_take_fiscal_priority_over_capital_formation, holdable).
narrative_ontology:cs_axiom_grounding('be164180-c2b8-4cf6-8673-f2a0f9b596b3', consumption_and_social_insurance_take_fiscal_priority_over_capital_formation, instrumental).
narrative_ontology:cs_reference_frame('be164180-c2b8-4cf6-8673-f2a0f9b596b3', growth_statistics_legitimacy_baseline).
narrative_ontology:cs_drift_state('be164180-c2b8-4cf6-8673-f2a0f9b596b3', post_investment_led_growth_plateau, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('be164180-c2b8-4cf6-8673-f2a0f9b596b3', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__livelihood_security_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, service_sector_workers).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, urban_households).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, elderly_care_recipients).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, central_fiscal_transfer_administrators).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, capital_goods_manufacturers).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_bureaus).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, construction_and_heavy_industry_workers).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, provincial_debt_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the redistribution formula that channels revenue toward pensions, healthcare subsidies, unemployment insurance, and education transfers. Justifies the reallocation as fulfilling the regime's core promise of tangible welfare improvement, and administers the enforcement — budget approval gates, performance reviews of local officials tied to livelihood indicators — that makes the reallocation stick against competing claims on the same revenue.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, central_fiscal_transfer_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Receive expanded healthcare coverage, subsidized education, and elderly care services that were previously thin or unavailable. Their satisfaction with visible, felt improvements is the direct currency of legitimacy this reading trades in; they have little say in how the transfers are financed and cannot easily exit the jurisdiction that provides them.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, urban_households, beneficiary,
    moderate, biographical, constrained, national).

% Depend entirely on pension adequacy and care infrastructure funded by this reallocation. Have no independent means to generate income or care outside what the state channels to them; the constraint's success or failure is experienced as their material daily condition, not as policy abstraction.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, elderly_care_recipients, beneficiary,
    powerless, biographical, trapped, local).

% Employment expands in healthcare, education, and elder-care staffing as consumption-side spending grows relative to investment-side spending. Can move between service employers more easily than industrial workers can move between capital-intensive employers, giving them a comparatively better exit position even as beneficiaries.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, service_sector_workers, beneficiary,
    moderate, biographical, mobile, national).

% Face reduced state investment demand and tighter credit as fiscal priority shifts from capital formation toward consumption and welfare. Lobby for restored infrastructure spending but are structurally deprioritized whenever livelihood indicators (employment, healthcare access) become the binding legitimacy metric; cannot relocate their capital stock easily.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, capital_goods_manufacturers, payer,
    organized, biographical, constrained, national).

% Historically financed growth and legitimacy through land sales and infrastructure investment; under this reading, their budget allocations and political credit are squeezed as central transfers redirect revenue toward welfare and social spending. Carry existing infrastructure debt they cannot easily service once their investment mandate is deprioritized, and cannot opt out of the reallocation formula set above them.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_bureaus, payer,
    organized, biographical, trapped, regional).

% Bear job losses and wage stagnation as infrastructure and heavy-industrial investment contracts under the consumption-prioritizing reallocation. Have limited retraining pathways into the expanding service sectors and are geographically concentrated in regions built around industrial investment cycles.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, construction_and_heavy_industry_workers, payer,
    powerless, biographical, constrained, regional).

% Hold bonds issued against expected infrastructure-driven revenue growth; as local government investment mandates shrink under the welfare-prioritizing reallocation, repayment capacity weakens and asset values become more uncertain, though their financial sophistication gives them more hedging options than displaced workers.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, provincial_debt_holders, payer,
    powerful, biographical, constrained, national).

% Built careers and local coalitions on investment-driven growth metrics and would object that livelihood-indicator scoring devalues their accumulated political capital, but promotion criteria are set centrally and they have no forum to contest the metric shift once it is adopted.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, provincial_party_officials, excluded,
    organized, biographical, trapped, regional).

% Track whether consumption-led reallocation actually raises aggregate welfare sustainably or merely defers a debt-servicing and investment-shortfall crisis onto local governments and industrial regions. Publish comparative analysis outside the direct interest of either the central administrators or the industrial lobby.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, independent_development_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates fiscal reallocation so that revenue generated by the broader economy is channeled into services citizens directly feel — healthcare, education, elderly care, employment support — solving the real problem that growth without felt livelihood improvement erodes political legitimacy even when aggregate output looks strong.
% TRANSFER_FUNCTION: Moves fiscal capacity and central credit allocation away from capital-intensive infrastructure and industrial investment financed through local government balance sheets, toward consumption subsidies, social insurance, and service-sector employment support administered through central transfer channels.
% ABSENT_VOICES: Provincial party officials and industrial regional coalitions who built legitimacy and careers on investment-led growth are structurally excluded from resetting the metric once the central formula shifts toward livelihood indicators; they can lobby informally but have no formal veto over the reallocation.
% DISAPPEARANCE_RATIONALE: If livelihood-indicator legitimacy were abandoned overnight, fiscal transfers would revert toward infrastructure and investment financing, local government bureaus would regain budget priority, healthcare and elderly-care subsidy growth would stall or reverse, and household consumption expectations built on several years of expanding social services would be sharply disappointed — a visible, politically consequential rearrangement, not a null result.
% FOUNDING_PROBLEM: Rapid GDP growth and infrastructure investment were not translating into felt improvements in ordinary daily life — healthcare access, elder care, secure employment, affordable education — creating a legitimacy gap between aggregate statistics and lived experience that risked eroding political consent.
% FOUNDING_PROBLEM_CORROBORATION: Independent development economists and international welfare-comparison studies attest that service-access and social-insurance gaps relative to GDP level remain real and measurable, corroborating the founding problem from outside the central administrators who benefit from claiming credit for closing it; provincial infrastructure bureaus, while victims of the reallocation, independently corroborate that the underlying livelihood gap was real even as they contest the remedy's cost to their own budgets.
narrative_ontology:disappearance_verdict(performance_legitimacy__livelihood_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__livelihood_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__livelihood_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__livelihood_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__livelihood_security_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__livelihood_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__livelihood_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-substantial (0.58 at interval end) because livelihood-security legitimacy does not eliminate extraction — it redirects it: fiscal capacity previously flowing toward local government infrastructure budgets and industrial investment financing is redirected toward consumption support, and the industrial/local-government payer set bears the reallocation cost with limited recourse. Suppression (0.62) reflects the active enforcement needed to hold the reallocation against provincial officials' entrenched investment-led incentive structures — promotion criteria, budget approval gates, and performance scoring must be actively reset and defended against reversion pressure. Theater ratio rises over the interval (0.20 to 0.42) as some fraction of livelihood-indicator reporting becomes performative — headline social-spending announcements that outpace the actual depth of service delivery on the ground, a normal drift pattern in indicator-driven legitimacy regimes.
 *
 * PERSPECTIVAL GAP:
 *   From the central administrators' seat, this reading is coordination: solving a real legitimacy deficit by redirecting fiscal capacity toward felt welfare. From the infrastructure bureaus' and industrial payers' seat, the identical reallocation reads as extraction of their investment mandate and revenue base to fund a competing legitimacy claim they had no vote in adopting. The engine computes these as structurally different seat classifications from the same base data — this is the seat divergence the tangled_rope classification is meant to capture, not an error to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Central fiscal transfer administrators sit at the agenda-setting position: they do not personally collect the redistributed funds but control the formula and enforcement, giving them low-but-nonzero effective extraction (administrative rent from control, not direct capture). Urban households, elderly care recipients, and service-sector workers are declared beneficiaries with low d — the felt improvement is the entire point of this reading and they are structurally positioned to receive it, though elderly care recipients' trapped exit options keep their dependence total rather than merely favorable. Capital goods manufacturers, infrastructure bureaus, industrial workers, and provincial debt holders are declared victims with high d — the same fiscal envelope that funds livelihood security is drawn from the envelope that would otherwise have funded their sector, and their exit options (constrained to trapped) prevent them from routing around the reallocation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — growth without felt livelihood improvement eroding legitimacy — is still live by the corroboration of outside observers, which distinguishes this from a mandatrophy case where the founding problem has been resolved but the arrangement persists as extraction. The risk this story flags is not that livelihood-security legitimacy has become obsolete, but that its indicator apparatus (theater_ratio rising) could decouple from the underlying felt condition even while the founding problem remains real — a live-problem/rising-theater combination that requires watching, not a dead-problem/zombie-arrangement combination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    livelihood_vs_growth_reading_resource_conflict,
    'When fiscal capacity is genuinely scarce, does the livelihood_security_reading''s consumption-and-welfare prioritization structurally crowd out the quantitative_growth_reading''s investment-led prioritization, or can both be funded simultaneously through debt expansion or efficiency gains?',
    'Track whether periods of livelihood-indicator emphasis coincide with measurable declines in infrastructure investment share of GDP and local government capital budgets, controlling for aggregate fiscal space (debt-to-GDP headroom).',
    'If the readings are genuinely in zero-sum tension over the same fiscal envelope, this confirms the tangled_rope classification''s requirement that identifiable victims (infrastructure bureaus, industrial payers) bear real costs; if fiscal expansion can fund both, the victim set shrinks and the reading looks closer to a pure rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(livelihood_vs_growth_reading_resource_conflict, empirical, 'Whether livelihood-security and growth-led legitimacy readings structurally compete for the same fiscal capacity.').

omega_variable(
    felt_versus_reported_livelihood_improvement,
    'Is the rising theater_ratio in livelihood indicators tracking genuine deceleration in service delivery depth relative to reporting, or is it an artifact of expanding reporting infrastructure that captures previously unmeasured but real improvement?',
    'Independent household survey data on subjective welfare and service access, compared against official livelihood-indicator reporting over the same interval, sourced from outside the central administrators'' own statistical apparatus.',
    'If theater_ratio growth reflects genuine indicator gaming, the reading is drifting toward Goodhart substitution and the coordination function is eroding even while the founding problem remains live; if it reflects improved measurement of real gains, the theater_ratio trend is a measurement artifact rather than a substantive drift signal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(felt_versus_reported_livelihood_improvement, empirical, 'Whether rising theater_ratio reflects genuine indicator gaming or improved measurement capture.').

omega_variable(
    kernel_framing_choice_livelihood_vs_composite,
    'Is livelihood security genuinely a separable legitimacy claim from qualitative_development_reading''s structural-transformation claim, or does household-felt welfare improvement partly derive from the same innovation/efficiency gains the qualitative reading tracks (e.g., productivity gains funding social insurance)?',
    'Examine whether social-insurance funding sources trace primarily to consumption-tax and general-revenue reallocation (supporting separability) versus productivity-driven revenue growth from upgraded industries (supporting entanglement with the qualitative reading).',
    'If substantially entangled, the ε-invariance decomposition into four separate readings may understate a shared causal channel between livelihood_security_reading and qualitative_development_reading; if separable, the four-way decomposition is clean as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice_livelihood_vs_composite, conceptual, 'Whether the livelihood-security and qualitative-development readings are structurally independent or share a revenue-generation mechanism, bearing on the validity of the four-way kernel decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__livelihood_security_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__livelihood_security_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(perf_tr_t4, performance_legitimacy__livelihood_security_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(perf_tr_t8, performance_legitimacy__livelihood_security_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(perf_tr_t12, performance_legitimacy__livelihood_security_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(perf_tr_t16, performance_legitimacy__livelihood_security_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__livelihood_security_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(perf_tr_t24, performance_legitimacy__livelihood_security_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__livelihood_security_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(perf_be_t4, performance_legitimacy__livelihood_security_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(perf_be_t8, performance_legitimacy__livelihood_security_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(perf_be_t12, performance_legitimacy__livelihood_security_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(perf_be_t16, performance_legitimacy__livelihood_security_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__livelihood_security_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(perf_be_t24, performance_legitimacy__livelihood_security_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__livelihood_security_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(perf_su_t4, performance_legitimacy__livelihood_security_reading, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(perf_su_t8, performance_legitimacy__livelihood_security_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(perf_su_t12, performance_legitimacy__livelihood_security_reading, suppression_requirement, 12, 0.57).
narrative_ontology:measurement(perf_su_t16, performance_legitimacy__livelihood_security_reading, suppression_requirement, 16, 0.59).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__livelihood_security_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(perf_su_t24, performance_legitimacy__livelihood_security_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__livelihood_security_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__livelihood_security_reading, 0.12).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language concept 'performance legitimacy' per the ε-invariance principle. Each sibling reading has its own ε, beneficiary/victim structure, and classification, sharing the same underlying kernel (performance_legitimacy) but instantiating structurally distinct fiscal-priority arrangements. The livelihood_security_reading and quantitative_growth_reading are in the most direct resource tension (both draw on the same near-term fiscal envelope); qualitative_development_reading and techno_nationalist_reading draw more on long-horizon capital allocation and industrial policy capacity respectively. All four link to each other via affects_constraints because a shift in one reading's dominance structurally changes resource availability for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
