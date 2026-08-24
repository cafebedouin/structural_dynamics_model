% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__adaptation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: climate_response_imperative__adaptation_priority_reading
 *   human_readable: Adaptation-Priority Climate Response Imperative
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This constraint story captures the adaptation-priority reading of the
 *   climate response imperative: the claim that climate action should focus
 *   on building resilience and reducing damage in exposed regions, treating
 *   emissions mitigation as aspirational rather than operational. The reading
 *   presents itself as pragmatic coordination — protecting the vulnerable now
 *   — but structurally operates as a tangled rope: it coordinates real
 *   adaptation resources while extracting capital from developing nations
 *   that cannot afford the required investment, and suppresses mitigation
 *   alternatives that would reduce the long-term adaptation burden. The
 *   structural delta is explicit: present-day developing nations enter the
 *   victim set via immediate capital requirements they cannot meet, creating
 *   a vicious circle where those least responsible for historic emissions
 *   bear the highest relative costs.
 *
 * KEY AGENTS:
 *   - wealthy_nations: Primary agenda_setter and beneficiary (institutional/arbitrage) — sets the adaptation-priority frame, avoids mitigation costs, directs finance to own industries
 *   - adaptation_infrastructure_industries: Beneficiary (organized/mobile) — captures adaptation finance streams
 *   - financial_institutions: Beneficiary (institutional/arbitrage) — intermediates and profits from adaptation capital flows
 *   - developing_nations: Primary payer (powerless/trapped) — faces unaffordable adaptation costs, debt traps, conditional finance
 *   - future_generations: Payer (powerless/trapped, non-agent) — inherits deferred mitigation costs and adaptation limits
 *   - vulnerable_populations: Payer (powerless/constrained) — bears frontline impacts, often excluded from adaptation benefits
 *   - mitigation_advocates: Excluded (organized/constrained) — systematically marginalized in this framing
 *   - climate_scientists: Observer (analytical/analytical) — provides evidence base used by all sides
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, 0.72).
domain_priors:suppression_score(climate_response_imperative__adaptation_priority_reading, 0.65).
domain_priors:theater_ratio(climate_response_imperative__adaptation_priority_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__adaptation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__adaptation_priority_reading, "Adaptation-Priority Climate Response Imperative").
narrative_ontology:topic_domain(climate_response_imperative__adaptation_priority_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__adaptation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__adaptation_priority_reading, '756fdfb2-cbfa-49df-acbe-929cc8b65b9d').
narrative_ontology:cs_kernel_codification('756fdfb2-cbfa-49df-acbe-929cc8b65b9d', distributed).
narrative_ontology:cs_authority_grounding('756fdfb2-cbfa-49df-acbe-929cc8b65b9d', extraction).
narrative_ontology:cs_interpretation_layer_present('756fdfb2-cbfa-49df-acbe-929cc8b65b9d').
narrative_ontology:cs_reading_relation('756fdfb2-cbfa-49df-acbe-929cc8b65b9d', climate_response_imperative__mitigation_priority_reading, forecloses).
narrative_ontology:cs_reading_relation('756fdfb2-cbfa-49df-acbe-929cc8b65b9d', climate_response_imperative__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('756fdfb2-cbfa-49df-acbe-929cc8b65b9d', foundational, adaptation_primacy_over_mitigation).
narrative_ontology:cs_axiom_status(adaptation_primacy_over_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('756fdfb2-cbfa-49df-acbe-929cc8b65b9d', adaptation_primacy_over_mitigation, instrumental).
narrative_ontology:cs_axiom('756fdfb2-cbfa-49df-acbe-929cc8b65b9d', secondary, mitigation_as_aspirational).
narrative_ontology:cs_axiom_status(mitigation_as_aspirational, holdable).
narrative_ontology:cs_axiom_grounding('756fdfb2-cbfa-49df-acbe-929cc8b65b9d', mitigation_as_aspirational, conventional).
narrative_ontology:cs_reference_frame('756fdfb2-cbfa-49df-acbe-929cc8b65b9d', inevitable_impacts_require_adaptation).
narrative_ontology:cs_drift_state('756fdfb2-cbfa-49df-acbe-929cc8b65b9d', contemporary_paris_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('756fdfb2-cbfa-49df-acbe-929cc8b65b9d', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__adaptation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, wealthy_nations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, adaptation_infrastructure_industries).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, financial_institutions).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, developing_nations).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, future_generations).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, vulnerable_populations).
narrative_ontology:constraint_vindicates(climate_response_imperative__adaptation_priority_reading, adaptation_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(climate_response_imperative__adaptation_priority_reading, resilience_primacy_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the climate agenda through UNFCCC negotiations and bilateral finance; benefit by avoiding costly mitigation while directing adaptation funding to their own industries and maintaining economic stability.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, wealthy_nations, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__adaptation_priority_reading, wealthy_nations, beneficiary).

% Receive contracts for seawalls, resilient agriculture, early-warning systems, and climate-proofed infrastructure; their business model depends on sustained adaptation funding streams.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, adaptation_infrastructure_industries, beneficiary,
    organized, biographical, mobile, global).

% Structure and intermediate adaptation finance (green bonds, resilience credits, insurance products); collect fees and rents from capital flows directed toward adaptation projects.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, financial_institutions, beneficiary,
    institutional, biographical, arbitrage, global).

% Face immediate capital requirements for adaptation they cannot meet; forced into debt or conditional finance; least responsible for historic emissions but bear highest relative costs; vicious circle of underinvestment and escalating vulnerability.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, developing_nations, payer,
    powerless, biographical, trapped, national).

% Inherit a world where deferred mitigation locks in higher warming; bear the compounded costs of adaptation limits being exceeded; no voice in current negotiations.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, future_generations, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_imperative__adaptation_priority_reading, future_generations).

% Experience frontline impacts (sea-level rise, extreme heat, crop failure); adaptation measures often bypass them due to cost-recovery requirements; displaced or impoverished when adaptation fails.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, vulnerable_populations, payer,
    powerless, biographical, constrained, local).

% Argue for emissions reduction as primary response; structurally marginalized in adaptation-priority framings; their proposals treated as aspirational add-ons rather than core strategy.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, mitigation_advocates, excluded,
    organized, generational, constrained, global).

% Provide the evidence base (IPCC, attribution studies) that both adaptation and mitigation camps cite; their synthesis reports increasingly warn of adaptation limits, but policy uptake lags.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, climate_scientists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes global finance, technology transfer, and planning capacity toward protecting exposed populations and infrastructure from near-term climate impacts; creates a shared framework for vulnerability assessment and resilience investment.
% TRANSFER_FUNCTION: Moves public and private capital from taxpayers and capital markets (predominantly in wealthy nations) into adaptation projects in exposed regions; simultaneously defers mitigation investment, transferring future avoided damages to future generations as unpaid liability.
% ABSENT_VOICES: Future generations (cannot participate), most climate-vulnerable communities (lack representation in finance decisions), and mitigation-advocacy coalitions (systematically sidelined as 'aspirational') are absent from the adaptation-priority decision table.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority imperative vanished overnight, climate finance would reorient toward mitigation-first portfolios, developing nations would demand reparative finance rather than project-based loans, and the political economy of climate action would shift from resilience-building to structural emission reduction — a fundamental rearrangement of institutions, financial flows, and North-South relations.
% FOUNDING_PROBLEM: By the 1990s/2000s it was clear that historic emissions had locked in decades of warming; exposed regions needed immediate protection while mitigation negotiations stalled; adaptation was framed as the only politically feasible, morally urgent response.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR4-WGII (2007) established adaptation necessity but emphasized mitigation as avoiding unmanageable adaptation; developing-nation negotiators (G77, AOSIS) attest the founding problem is live but the response has become extractive; wealthy-nation policymakers (OECD, EU) attest adaptation remains the pragmatic priority — no external corroboration of the 'aspirational mitigation' framing beyond the beneficiary coalition.
narrative_ontology:disappearance_verdict(climate_response_imperative__adaptation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__adaptation_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__adaptation_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_imperative__adaptation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__adaptation_priority_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constraint transfers substantial capital obligations to developing nations while deferring mitigation that would reduce total system cost. Suppression (0.65) is substantial because the adaptation-priority framing actively marginalizes mitigation pathways in finance rules, negotiation agendas, and project eligibility. Theater ratio (0.42) reflects genuine adaptation coordination (early warning, resilient infrastructure) mixed with performative finance pledges that fail to materialize. Accessibility collapse (0.71) is high because once adaptation is framed as the primary response, mitigation alternatives become politically and financially inaccessible. Resistance (0.54) is moderate: developing nations and mitigation advocates resist but lack structural power to shift the frame. Measurements show rising extraction, theater, and suppression over the 30-year interval as the adaptation-priority regime matures.
 *
 * PERSPECTIVAL GAP:
 *   From the wealthy-nation/agenda-setter seat, the constraint appears as necessary coordination: adaptation saves lives now, mitigation is politically impossible. From the developing-nation/payer seat, the same structure is enforced extraction: they pay for a crisis they didn't create while mitigation — which would reduce their long-term burden — is suppressed. The engine computes this divergence from the structural data; the claimed_type (tangled_rope) reflects the author's structural judgment that both coordination and extraction are real.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy nations are structural beneficiaries (d ~ 0.2): they set rules, collect industrial rents, avoid mitigation costs. Adaptation industries and financial institutions are beneficiaries (d ~ 0.25-0.3): they capture finance flows. Developing nations are full targets (d ~ 0.9): trapped, powerless, bear unaffordable costs. Future generations are full targets (d ~ 1.0): trapped across civilizational time. Vulnerable populations are targets (d ~ 0.8): constrained exit, bear impacts. Mitigation advocates are excluded (d ~ 0.7): constrained but not primary extraction targets. Climate scientists are analytical (d ~ 0.5): symmetric observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (locked-in warming requires urgent adaptation) remains live — impacts are accelerating. However, the response has undergone mandatrophy: the adaptation-priority frame persists even as mitigation becomes technically feasible and economically advantageous, because the frame now serves the extraction interests of wealthy nations and their industries. The constraint has not transitioned to a scaffold (no sunset clause) and is not a piton (active enforcement intensifies). It is a tangled rope whose coordination function is real but whose extraction asymmetry is structural and worsening.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_mitigation_boundary,
    'Is the adaptation-priority framing a genuine coordination necessity given locked-in warming, or a strategic cover for wealthy nations to avoid mitigation costs?',
    'Counterfactual analysis: if mitigation finance matched adaptation finance 1:1 from 2000 onward, would adaptation needs be lower today? Empirical test via integrated assessment models with alternative finance histories.',
    'If cover, the constraint reclassifies toward snare (coordination story is pretext). If genuine necessity, tangled_rope stands but extraction asymmetry remains a justice failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_mitigation_boundary, conceptual, 'Whether the coordination function is structurally necessary or politically constructed').

omega_variable(
    capital_requirement_vicious_circle,
    'Does the capital-intensity of adaptation projects create a structural trap where developing nations'' debt burdens increase vulnerability, requiring yet more adaptation finance?',
    'Longitudinal debt-climate vulnerability analysis across developing nations; compare adaptation-loan recipients vs. grant recipients on vulnerability trajectories.',
    'If vicious circle confirmed, extraction is systemic and self-reinforcing — constraint moves toward snare. If finance terms evolve (grants, debt-for-climate swaps), extraction may stabilize.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_requirement_vicious_circle, empirical, 'Whether adaptation finance structure reproduces the vulnerability it claims to reduce').

omega_variable(
    kernel_reading_identity,
    'Does this reading''s identity as ''the pragmatic adaptation priority'' depend on suppressing the mitigation_priority_reading as ''unrealistic'', and would that suppression collapse if mitigation costs fell below adaptation costs?',
    'Track rhetorical shifts in UNFCCC negotiations and finance documents as renewable energy LCOE crosses fossil parity; measure frequency of ''aspirational'' applied to mitigation vs. ''essential'' applied to adaptation.',
    'If suppression is cost-contingent, the constraint''s persistence is fragile — a cost crossover could trigger rapid reclassification. If suppression is identity-constitutive, the reading will persist regardless of cost curves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the reading''s boundary against mitigation is contingent or constitutive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__adaptation_priority_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_response_imperative__adaptation_priority_reading_tr_t0, climate_response_imperative__adaptation_priority_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(climate_response_imperative__adaptation_priority_reading_tr_t6, climate_response_imperative__adaptation_priority_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(climate_response_imperative__adaptation_priority_reading_tr_t12, climate_response_imperative__adaptation_priority_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(climate_response_imperative__adaptation_priority_reading_tr_t18, climate_response_imperative__adaptation_priority_reading, theater_ratio, 18, 0.37).
narrative_ontology:measurement(climate_response_imperative__adaptation_priority_reading_tr_t24, climate_response_imperative__adaptation_priority_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(climate_response_imperative__adaptation_priority_reading_tr_t30, climate_response_imperative__adaptation_priority_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(climate_response_imperative__adaptation_priority_reading_be_t0, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(climate_response_imperative__adaptation_priority_reading_be_t6, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(climate_response_imperative__adaptation_priority_reading_be_t12, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(climate_response_imperative__adaptation_priority_reading_be_t18, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 18, 0.66).
narrative_ontology:measurement(climate_response_imperative__adaptation_priority_reading_be_t24, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(climate_response_imperative__adaptation_priority_reading_be_t30, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(climate_response_imperative__adaptation_priority_reading_su_t0, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(climate_response_imperative__adaptation_priority_reading_su_t6, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(climate_response_imperative__adaptation_priority_reading_su_t12, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 12, 0.54).
narrative_ontology:measurement(climate_response_imperative__adaptation_priority_reading_su_t18, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 18, 0.59).
narrative_ontology:measurement(climate_response_imperative__adaptation_priority_reading_su_t24, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(climate_response_imperative__adaptation_priority_reading_su_t30, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__adaptation_priority_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__adaptation_priority_reading, 0.15).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, climate_finance_architecture).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, loss_and_damage_mechanism).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, technology_transfer_regime).

% DUAL FORMULATION NOTE:
% This reading, mitigation_priority_reading, and degrowth_reading form the climate_response_imperative constraint family. They share the kernel (climate action necessity) but instantiate different constraints with distinct ε, beneficiaries, victims, and classifications. This reading's ε (0.72) is substantially higher than mitigation_priority_reading's expected ε (~0.35) because the capital transfer from South to North is larger than the technology transfer from North to South. Degrowth_reading's ε is structurally distinct (targets Northern consumption).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_imperative__adaptation_priority_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
