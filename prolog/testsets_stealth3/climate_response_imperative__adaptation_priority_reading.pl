% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__adaptation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__adaptation_priority_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: climate_response_imperative__adaptation_priority_reading
 *   human_readable: Adaptation-First Climate Response Allocation (Adaptation Priority Reading)
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the adaptation_priority_reading —
 *   of the contested kernel climate_response_imperative. The arrangement
 *   modeled: since the early-2000s adaptation turn, the operative global
 *   climate-response allocation channels deliverable action primarily into
 *   resilience-building and damage reduction in exposed regions, while
 *   emissions reduction operates as a succession of non-binding aspirations
 *   (pledged targets repeatedly missed, finance flowing chiefly to visible
 *   protection projects). The ε referent is THIS standing arrangement — the
 *   actual finance-flow regime — assessed by this reading's own lights; the
 *   reading's endorsed emphasis is the arrangement, not an alternative, so
 *   the referent is stable and ε is invariant within the story. Sibling
 *   readings (mitigation_priority_reading, degrowth_reading) instantiate
 *   different constraints and are separate stories linked via network edges.
 *   Claim/metric independence is maintained: claimed_type is tangled_rope
 *   because the structure genuinely coordinates adaptation resources while
 *   extracting asymmetrically; metrics are authored descriptively without
 *   tuning toward that or any other computed verdict.
 *
 * KEY AGENTS:
 *   - developed_nation_governments: agenda-setting bloc (institutional/constrained) — controls fund governance and sets the adaptation-over-abatement budget signal
 *   - multilateral_climate_finance_institutions: administrator with dual position (institutional/constrained) — runs the lending machinery and collects fees and interest on it
 *   - fossil_fuel_incumbent_industries: primary beneficiary (powerful/mobile) — preserved asset values ride on mitigation staying aspirational
 *   - adaptation_engineering_insurance_sector: contract-flow beneficiary (organized/mobile) — revenue scales with persistent publicly-funded exposure
 *   - exposed_low_income_nations: primary target (powerless/trapped) — pays twice: damages from others' emissions, then debt service for protection
 *   - small_island_developing_states: existential target (powerless/trapped) — buys delay, not survival, with their full diplomatic bandwidth
 *   - future_generations: silent target (powerless/trapped) — inherits accumulated damages and the debts contracted against them
 *   - climate_justice_movements: excluded voice (organized/identity_locked) — drafts responsibility-proportional demands outside the rooms where allocation is decided
 *   - ipcc_assessment_community: analytical observer (analytical/analytical) — quantifies gaps and residual risk; shapes feasibility framing without allocation authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, 0.68).
domain_priors:suppression_score(climate_response_imperative__adaptation_priority_reading, 0.62).
domain_priors:theater_ratio(climate_response_imperative__adaptation_priority_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__adaptation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__adaptation_priority_reading, "Adaptation-First Climate Response Allocation (Adaptation Priority Reading)").
narrative_ontology:topic_domain(climate_response_imperative__adaptation_priority_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__adaptation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__adaptation_priority_reading, '8eb19be5-4d4e-4535-b0e7-eccdd9c287ba').
narrative_ontology:cs_kernel_codification('8eb19be5-4d4e-4535-b0e7-eccdd9c287ba', fixed_text).
narrative_ontology:cs_authority_grounding('8eb19be5-4d4e-4535-b0e7-eccdd9c287ba', lineage).
narrative_ontology:cs_interpretation_layer_present('8eb19be5-4d4e-4535-b0e7-eccdd9c287ba').
narrative_ontology:cs_reading_relation('8eb19be5-4d4e-4535-b0e7-eccdd9c287ba', climate_response_imperative__mitigation_priority_reading, forecloses).
narrative_ontology:cs_reading_relation('8eb19be5-4d4e-4535-b0e7-eccdd9c287ba', climate_response_imperative__degrowth_reading, influences).
narrative_ontology:cs_axiom('8eb19be5-4d4e-4535-b0e7-eccdd9c287ba', foundational, adaptation_primacy_under_lockin).
narrative_ontology:cs_axiom_status(adaptation_primacy_under_lockin, holdable).
narrative_ontology:cs_axiom_grounding('8eb19be5-4d4e-4535-b0e7-eccdd9c287ba', adaptation_primacy_under_lockin, empirically_contingent).
narrative_ontology:cs_axiom('8eb19be5-4d4e-4535-b0e7-eccdd9c287ba', foundational, near_term_protection_over_long_term_abatement).
narrative_ontology:cs_axiom_status(near_term_protection_over_long_term_abatement, holdable).
narrative_ontology:cs_axiom_grounding('8eb19be5-4d4e-4535-b0e7-eccdd9c287ba', near_term_protection_over_long_term_abatement, instrumental).
narrative_ontology:cs_reference_frame('8eb19be5-4d4e-4535-b0e7-eccdd9c287ba', adaptation_first_resilience_regime).
narrative_ontology:cs_drift_state('8eb19be5-4d4e-4535-b0e7-eccdd9c287ba', post_global_stocktake_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8eb19be5-4d4e-4535-b0e7-eccdd9c287ba', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__adaptation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, multilateral_climate_finance_institutions).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, fossil_fuel_incumbent_industries).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, adaptation_engineering_insurance_sector).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, exposed_low_income_nations).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, small_island_developing_states).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dominate the governing bodies of the major climate funds and the shareholder boards of the multilateral development banks, and set the budget signals that determine whether resilience projects or emissions programs get financed. Domestic electors reward visible protection spending over distant abatement, which anchors the priority ordering in place. Exiting the arrangement would mean conceding influence over the finance architecture to rival blocs, so participation is maintained even where officials privately dispute the ordering.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, developed_nation_governments, agenda_setter,
    institutional, biographical, constrained, global).

% Administer adaptation funds and concessional lending windows; their project pipelines, staffing, and fee income depend on a continuous flow of bankable resilience projects. Their appraisal criteria define what counts as fundable adaptation, which steers recipient investment choices. They collect interest and service fees on adaptation loans while presenting themselves as neutral conduits.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, multilateral_climate_finance_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__adaptation_priority_reading, multilateral_climate_finance_institutions, beneficiary).

% Retain bookable reserve values and defer transition capital expenditure for as long as emissions reduction remains a non-binding aspiration. They fund research programs and media narratives that present resilience and damage reduction as the serious, realistic policy, contrasting it with allegedly utopian transformation proposals. Capital mobility lets them shift assets across jurisdictions if any single regulator moves first.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, fossil_fuel_incumbent_industries, beneficiary,
    powerful, biographical, mobile, global).

% Wins design-build-operate contracts for coastal defense, water security, heat-resilient housing, and parametric insurance products aimed at exposed regions. Revenue scales with measured exposure and perceived risk rather than with verified risk reduction, so the sector's commercial interest lies in a large, persistent, publicly funded adaptation pipeline.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, adaptation_engineering_insurance_sector, beneficiary,
    organized, biographical, mobile, continental).

% Must finance protection against damages produced overwhelmingly by other parties' cumulative emissions, receiving adaptation capital chiefly as loans rather than grants. Debt service consumes fiscal space needed for development, which raises exposure further and forces renewed borrowing. Territory cannot be relocated and the climate cannot be exited; the only lever available is bargaining inside rooms whose agendas are set elsewhere.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, exposed_low_income_nations, payer,
    powerless, biographical, trapped, regional).

% Face inundation timelines on which adaptation offers delay rather than survival, yet absorb nearly their entire diplomatic bandwidth defending adaptation-finance terms. They negotiate as a coordinated bloc but hold minimal leverage over fund governance, eligibility criteria, and concessionality, and their existential stakes are discounted in appraisal frameworks built around bankability.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, small_island_developing_states, payer,
    powerless, generational, trapped, regional).

% Inherit both the locked-in warming that adaptation-only response allows to accumulate and the public debts contracted to build partial defenses against it. They hold no seat in any negotiating room; their position appears only as projections embedded in other parties' scenario models, and every year of aspirational mitigation converts directly into their baseline damages.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Organize loss-and-damage claims and responsibility-proportional finance demands across exposed regions. Present at protest spaces, side events, and observer slots, but absent from the closed sessions where allocation rules and eligibility criteria are drafted. Leaving the movement would mean dissolving the networks, commitments, and shared account of the crisis that constitute members' political selves.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, climate_justice_movements, excluded,
    organized, generational, identity_locked, global).

% Produces the assessment reports both camps cite, quantifying adaptation finance gaps, residual risk, and the widening spread between pledged and delivered emissions cuts. Holds no allocation authority, but its scenario framing and feasibility language shape which readings of the imperative read as realistic to negotiators and ministers.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, ipcc_assessment_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__adaptation_priority_reading, fossil_fuel_incumbent_industries).
narrative_ontology:fixing_cost_class(climate_response_imperative__adaptation_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools capital and technical capacity to build protective infrastructure, early-warning systems, and resilient water and agriculture for regions facing warming that is already locked in — solving the real problem of reducing present and near-term harm to exposed populations regardless of the emissions path.
% TRANSFER_FUNCTION: Moves adaptation capital (predominantly as debt instruments) from developed-nation treasuries and multilateral lenders to exposed developing nations; moves avoided costs — deferred decarbonization expenditure and preserved carbon-asset values — to high-emission incumbents in wealthy economies; lands the damages of past emissions on the exposed and on future generations.
% ABSENT_VOICES: Small-island existential claimants and future generations have no seat; loss-and-damage proponents are confined to observer slots while allocation rules are drafted in creditor-dominated committees; climate justice movements would demand responsibility-proportional grant finance and are structurally outside those rooms.
% DISAPPEARANCE_RATIONALE: If the adaptation-first allocation vanished overnight, exposed nations would lose funded protection mid-decade, the adaptation contracting and lending apparatus would collapse, and mitigation politics would immediately resurface as the only remaining response channel — finance flows, negotiation agendas, and incumbent asset expectations would all reorganize.
% FOUNDING_PROBLEM: Recognition in the early 2000s that a quantity of warming was already committed and exposed regions would need protection regardless of mitigation success, requiring dedicated finance and institutions for resilience and damage reduction.
% FOUNDING_PROBLEM_CORROBORATION: IPCC working-group assessments, national meteorological services in exposed states, and actuarial loss data all attest independently that locked-in warming and rising adaptation needs are real — corroboration from outside the benefiting parties. What no outside source attests is that the founding problem justifies demoting mitigation to aspiration; that extension is asserted only by the arrangement's beneficiaries.
narrative_ontology:disappearance_verdict(climate_response_imperative__adaptation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__adaptation_priority_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__adaptation_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_imperative__adaptation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__adaptation_priority_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__adaptation_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__adaptation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the arrangement's cost incidence follows vulnerability rather than responsibility: exposed low-income nations finance defense against damages they did little to cause, chiefly through loans, while the parties most responsible for cumulative emissions defer transformation costs indefinitely. Suppression is substantial (0.62) but structural-discursive rather than physical: the priority ordering persists through fund eligibility criteria, bankability standards, creditor leverage, and a realism framing that delegitimizes transformation demands — alternatives remain technically available at every point (hence accessibility_collapse 0.45, well below mountain levels) but are politically fenced off. Theater is moderate and rising (0.32): seawalls, early-warning systems, and resilient-crop programs are real, but a growing share of activity is National Adaptation Plans, resilience strategies, and pledge ceremonies whose documents outnumber delivered projects. Resistance is high (0.60): G77/AOSIS blocs, loss-and-damage campaigns, and justice movements contest the ordering continuously and won a dedicated fund in 2022, though with terms the creditors still govern. The temporal series run on ONE shared grid (t=0..24, mapped to 2001–2025: Marrakech adaptation-fund genesis to the post-global-stocktake period); every tracked metric is authored at every shared point, so no scalar substitution contaminates earlier times. Gain_flow names fossil_fuel_incumbent_industries because the largest identifiable capture is preserved carbon-asset value from indefinite mitigation deferral — larger than loan fees (finance institutions) and contract margins (engineering sector), both of which also capture shares. fixing_cost is prohibitive for the current agenda-setters: reallocating toward responsibility-proportional grant finance plus funded mitigation would cost incumbent industries and creditor treasuries far more than continuing the arrangement costs them, and the electoral cycle rewards the deferral.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats should compute sharply divergent types from the same structural data. From the finance-institution and treasury seats, the arrangement is prudent risk management: triage under scarcity, bankable projects, measurable lives protected. From the exposed-nation seats, the same structure operates as paying twice — absorbing damages produced by others' emissions, then servicing debt to build defenses against the remainder — with eligibility criteria written by creditors. The analytical seat sees both descriptions as locally sincere: nothing requires any participant to lie for the divergence to be real. The engine computes this divergence; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the engine's derivation: fossil_fuel_incumbent_industries and adaptation_engineering_insurance_sector sit near the beneficiary end (low d) with mobile exit damping χ further — they can reposition capital if pressure mounts. multilateral_climate_finance_institutions carry dual roles; their administrative take is real but modest relative to incumbents' preserved rents. exposed_low_income_nations and small_island_developing_states sit at the full-target end (high d) amplified by trapped exit — no relocation, no climate exit, borrowing dependence — so effective extraction concentrates on them at maximum intensity. future_generations are maximally targeted with zero power and civilizational horizon. developed_nation_governments sit nearer the beneficiary end than their nominal neutrality suggests: they defer transformation costs and win domestic credit for visible protection spending. No directionality_overrides were needed: the structural derivation from declared beneficiaries/victims plus exit options captures every seat accurately, including the dual-positioned finance administrators.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is declared: the founding problem (locked-in warming requiring protection) is live and externally corroborated, so this is not a mandate outliving its function. The tangled_rope classification does two pieces of preventive work. Against snare misclassification: the adaptation function is genuine and verifiable — exposed populations are measurably safer where defenses are built — so the arrangement cannot be dismissed as cover-story-only extraction; the coordination gate is satisfied on real evidence. Against rope misclassification: the cost incidence is sharply asymmetric and actively enforced through creditor-controlled eligibility and loan-not-grant terms, so participant net-benefit symmetry fails and the enforcement requirement is met. The rising theater series is watched but is a symptom, not the test: the cost-asymmetry between who could reorder priorities and what reordering would cost THEM is what holds the classification in place.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_position,
    'This story instantiates only the adaptation_priority_reading of the climate_response_imperative kernel — what structural features change under each sibling reading?',
    'Cross-read the linked sibling stories: compare beneficiary/victim sets, ε, and computed types across the three files; the deltas localize the disagreement to primacy ordering and the transformation-prerequisite question.',
    'Under mitigation_priority_reading, exposed developing nations partially exit the victim set (abatement lowers their future damages) and fossil incumbents enter it; under degrowth_reading, Global North consumers enter the victim set and the adaptation contracting sector loses its demand base — this story''s classification holds only for the adaptation-priority instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_position, conceptual, 'Committer-frame routing: one reading among three of the same kernel; sibling deltas recorded here rather than folded into the constraint.').

omega_variable(
    formal_text_vs_actual_flows_referent,
    'Is ε''s referent the de facto finance-flow arrangement (adaptation effectively primary) or the formal treaty-text arrangement (mitigation formally primary under Article 4 ordering)?',
    'Track the adaptation share of realized climate finance against stated mitigation commitments across successive COP cycles; if binding mechanisms begin coupling the formal layer to actual flows differently than this reading assumes, split the formal-commitment layer into its own constraint story.',
    'Measured on treaty text alone, the arrangement reads mitigation-primary with low current extraction; measured on flows, it reads adaptation-primary with the authored extraction — the two observables describe two different constraints, and conflating them would violate ε-invariance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_text_vs_actual_flows_referent, conceptual, 'Label-disambiguation guard: formal commitments versus effective allocation as candidate second constraint.').

omega_variable(
    debt_adaptation_vicious_circle,
    'Does loan-based adaptation finance form a self-reinforcing extraction loop (debt distress narrows fiscal space, raising exposure and forcing renewed borrowing), or is it transitional subsidy later converted to grant terms?',
    'Track debt-service-to-adaptation-spending ratios and refinancing concessionality for heavily exposed sovereign borrowers across a decade; observe whether terms soften as measured exposure rises.',
    'If self-reinforcing, effective extraction exceeds the authored ε and the arrangement trends toward snare; if transitional, part of the measured extraction is coordination cost and the tangled_rope reading stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_adaptation_vicious_circle, empirical, 'Whether the finance mechanism compounds extraction cyclically for trapped borrowers.').

omega_variable(
    constructed_vs_physical_naturalness,
    'Is adaptation-priority an emergent property of locked-in warming physics — a quasi-natural circumstance no coalition chose — or a constructed allocation sustained by identifiable incumbent interests?',
    'Compare jurisdiction pairs with matched exposure profiles but different incumbent-political configurations; systematic divergence in adaptation/mitigation budget shares indicates construction rather than circumstance.',
    'If constructed, the beneficiary declarations carry the classification toward extraction-weighted verdicts and false-summit-style scrutiny applies to any naturality claim; if circumstantial, part of the arrangement is genuinely indifferent to who defends it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constructed_vs_physical_naturalness, conceptual, 'Naturality ambiguity for the priority ordering itself.').

omega_variable(
    negotiator_acceptance_internalization,
    'Among exposed-nation negotiators and finance officials, is acceptance of the adaptation-first ordering structural (creditor leverage and finance dependence leave no alternative) or internalized (the realist framing absorbed as the only serious position)?',
    'Post-leverage trajectory: if negotiator demands shift toward responsibility-proportional grant finance after securing independent funding capacity (new lender coalitions, levies on shipping or fossil transactions), prior acceptance was structural; if positions persist unchanged after leverage improves, internalization is implicated.',
    'If substantially internalized, effective suppression exceeds the structural measure — the constraint travels with the agents even after exit options open — and the omega resolves toward a higher combined suppression estimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negotiator_acceptance_internalization, empirical, 'Structural versus internalized suppression among target-seat elites.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__adaptation_priority_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__adaptation_priority_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(clim_tr_t4, climate_response_imperative__adaptation_priority_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(clim_tr_t8, climate_response_imperative__adaptation_priority_reading, theater_ratio, 8, 0.23).
narrative_ontology:measurement(clim_tr_t12, climate_response_imperative__adaptation_priority_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(clim_tr_t16, climate_response_imperative__adaptation_priority_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(clim_tr_t20, climate_response_imperative__adaptation_priority_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(clim_tr_t24, climate_response_imperative__adaptation_priority_reading, theater_ratio, 24, 0.32).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 0, 0.46).
narrative_ontology:measurement(clim_be_t4, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(clim_be_t8, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(clim_be_t12, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(clim_be_t16, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(clim_be_t20, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(clim_be_t24, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(clim_su_t4, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 4, 0.56).
narrative_ontology:measurement(clim_su_t8, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(clim_su_t12, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(clim_su_t16, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(clim_su_t20, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(clim_su_t24, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__adaptation_priority_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, degrowth_reading).

% DUAL FORMULATION NOTE:
% Constraint family: climate_response_imperative decomposes into three reading-stories (adaptation_priority_reading, mitigation_priority_reading, degrowth_reading) because the colloquial concept 'climate response' covers structurally distinct allocation regimes with distinct beneficiary/victim sets and distinct ε. This member's ε is authored over the adaptation-primary finance-flow arrangement; the mitigation-priority sibling authors ε over a technology-and-market abatement regime (different victims: incumbents enter its victim set); the degrowth sibling authors ε over the consumption-growth arrangement itself (Global North consumers enter its victim set). Upstream/downstream: this reading structurally influences degrowth (adaptation-first allocation crowds out the fiscal and political space transformation requires) and stands in logical contradiction with mitigation-priority's primacy premise. Each story links the others via network.affects_constraints; cross-family contamination propagates through these edges when any member's purity degrades.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
