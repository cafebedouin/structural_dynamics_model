% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__adaptation_priority, []).

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
 *   constraint_id: climate_response_legitimacy__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Legitimacy Settlement
 *   domain: climate policy/political economy/intergenerational ethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   'climate_response_legitimacy': the adaptation-priority reading, under
 *   which a legitimate climate response accepts the warming trajectory as
 *   given and directs effort toward protecting vulnerable populations through
 *   resilience infrastructure and adaptive capacity. Per the epsilon-referent
 *   rule, the referent of the authored extractiveness is the standing
 *   adaptation-priority arrangement itself, assessed by the reading's own
 *   lights (its core value is protection of the exposed) — never the
 *   mitigation or degrowth arrangements its rivals would install. Even by its
 *   own lights the arrangement under-delivers: it accepts a trajectory whose
 *   impacts outrun what adaptation can deliver, finances protection partly
 *   through recipient debt, and defers compounding costs to generations with
 *   no seat. The claimed type (tangled_rope) and the authored metrics are
 *   independent facts: the claim asserts a genuine coordination core wrapped
 *   around asymmetric extraction; the metrics describe that operation as
 *   measured. Sibling readings are separate constraints linked via
 *   network.affects_constraints, not folded into this one.
 *
 * KEY AGENTS:
 *   - wealthy_developed_nations: primary beneficiary and agenda-setter (institutional/arbitrage) — preserves the development model, controls finance terms
 *   - carbon_intensive_industries: secondary beneficiary (powerful/mobile) — regulatory threat removed, adaptation buildout opens new markets
 *   - adaptation_finance_intermediaries: secondary beneficiary (institutional/mobile) — fees and loan interest scale with flow volume
 *   - low_income_climate_vulnerable_regions: dual-positioned payer/beneficiary (organized/trapped) — receives adaptation flows, absorbs the accepted trajectory's residual damages and the $350B gap
 *   - small_island_developing_states: payer (organized/trapped) — existential exposure adaptation cannot ultimately secure
 *   - future_generations: payer (powerless/trapped) — inherits compounded warming and adaptation debt, no seat
 *   - multilateral_climate_funds: agenda-setter (institutional/constrained) — administers allocation rules and access modalities
 *   - climate_justice_movements: excluded voice (organized/constrained) — rejects the accept-the-trajectory premise, outside decision rooms
 *   - ipcc_assessment_bodies: analytical observer (institutional/analytical) — documents adaptation limits and the finance gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, 0.62).
domain_priors:suppression_score(climate_response_legitimacy__adaptation_priority, 0.52).
domain_priors:theater_ratio(climate_response_legitimacy__adaptation_priority, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__adaptation_priority, "Adaptation-Priority Climate Legitimacy Settlement").
narrative_ontology:topic_domain(climate_response_legitimacy__adaptation_priority, "climate policy/political economy/intergenerational ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__adaptation_priority, 'f16109c6-7cb0-4558-bf57-8d66446d8f17').
narrative_ontology:cs_kernel_codification('f16109c6-7cb0-4558-bf57-8d66446d8f17', formalized).
narrative_ontology:cs_authority_grounding('f16109c6-7cb0-4558-bf57-8d66446d8f17', lineage).
narrative_ontology:cs_interpretation_layer_present('f16109c6-7cb0-4558-bf57-8d66446d8f17').
narrative_ontology:cs_reading_relation('f16109c6-7cb0-4558-bf57-8d66446d8f17', climate_response_legitimacy__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('f16109c6-7cb0-4558-bf57-8d66446d8f17', climate_response_legitimacy__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('f16109c6-7cb0-4558-bf57-8d66446d8f17', foundational, warming_trajectory_treated_as_fixed_background).
narrative_ontology:cs_axiom_status(warming_trajectory_treated_as_fixed_background, holdable).
narrative_ontology:cs_axiom_grounding('f16109c6-7cb0-4558-bf57-8d66446d8f17', warming_trajectory_treated_as_fixed_background, empirically_contingent).
narrative_ontology:cs_axiom('f16109c6-7cb0-4558-bf57-8d66446d8f17', foundational, present_vulnerable_protection_prioritized_over_future_harm_avoidance).
narrative_ontology:cs_axiom_status(present_vulnerable_protection_prioritized_over_future_harm_avoidance, holdable).
narrative_ontology:cs_axiom_grounding('f16109c6-7cb0-4558-bf57-8d66446d8f17', present_vulnerable_protection_prioritized_over_future_harm_avoidance, deontological).
narrative_ontology:cs_reference_frame('f16109c6-7cb0-4558-bf57-8d66446d8f17', trajectory_acceptance_impact_management).
narrative_ontology:cs_drift_state('f16109c6-7cb0-4558-bf57-8d66446d8f17', contemporary_post_paris_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f16109c6-7cb0-4558-bf57-8d66446d8f17', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_developed_nations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, carbon_intensive_industries).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, adaptation_finance_intermediaries).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, low_income_climate_vulnerable_regions).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, small_island_developing_states).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, low_income_climate_vulnerable_regions).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__adaptation_priority, adaptation_deficit_doctrine).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__adaptation_priority, resilience_as_development_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dominate the COP finance agenda and multilateral bank voting shares; the accept-the-trajectory framing lets domestic growth, consumption, and carbon-intensive infrastructure continue without structural transformation. Contribute adaptation pledges well below assessed need while retaining discretion over terms. Warming impacts at their latitudes remain manageable on near-term horizons, so adjusting domestically, relocating capital, or re-framing commitments is cheap relative to what exposed regions face.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, wealthy_developed_nations, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__adaptation_priority, wealthy_developed_nations, agenda_setter).

% An adaptation-first legitimacy settlement removes the existential regulatory threat to their core business: demand for fuels, cement, steel, and cooling continues, and the adaptation buildout itself opens new markets (sea defenses, desalination, hardened grids). Assets can be shifted across jurisdictions; exit from any single regulatory perimeter is available.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, carbon_intensive_industries, beneficiary,
    powerful, immediate, mobile, global).

% Development banks, climate funds, consultancies, and catastrophe-risk instruments collect management fees, advisory contracts, and interest on adaptation lending. A substantial share of adaptation finance arrives as loans rather than grants, converting protection into debt service for recipients. Intermediary income scales with flow volume regardless of whether delivered protection closes the vulnerability gap.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, adaptation_finance_intermediaries, beneficiary,
    institutional, biographical, mobile, global).

% Receive adaptation grants and loans for coastal defenses, early-warning systems, and resilient agriculture, and coordinate as negotiating blocs (G77, V20, African Group) to press for more. Simultaneously absorb the escalating losses the accepted trajectory delivers: the roughly $350 billion annual adaptation gap leaves most identified needs unfunded, residual damages compound with each fraction of a degree, and loan-based finance converts protection into indebtedness. Territory cannot be moved; exit is not available.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, low_income_climate_vulnerable_regions, payer,
    organized, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__adaptation_priority, low_income_climate_vulnerable_regions, beneficiary).

% Face existential exposure to sea-level rise that the accepted trajectory guarantees. Coastal armor and elevation cannot secure state survival at higher warming levels; adaptation purchases time, not safety. Diplomatically effective as AOSIS but materially leveraged only through moral pressure; the practical form of exit is outward migration of populations and eventual dissolution of the state form.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, small_island_developing_states, payer,
    organized, generational, trapped, regional).

% Inherit the compounded warming that deferred mitigation locks in, diminished adaptation headroom (hard adaptation limits arrive sooner at higher trajectories), and the servicing liabilities of today's adaptation debt. Hold no seat in any negotiating forum; their interests enter only through advocacy proxies and constitutional litigation attempts.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Green Climate Fund, Adaptation Fund, and loss-and-damage fund boards allocate adaptation resources, set accreditation requirements, access modalities, and co-financing ratios. Board composition balances donor and recipient states; their procedural rules determine who gets protected, on what terms, and how slowly. They administer the settlement rather than capturing its principal gains.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, multilateral_climate_funds, agenda_setter,
    institutional, generational, constrained, global).

% Reject the accept-the-trajectory premise outright: demand loss-and-damage liability attributable to historical emitters, grant-based rather than loan-based finance, and renewed mitigation ambition. Present in protests, side events, and observer delegations, but outside the ministerial rooms where finance terms and priority framings are actually set.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, climate_justice_movements, excluded,
    organized, biographical, constrained, global).

% Assess adaptation limits, residual damages, and finance gaps in structured assessment cycles. Working Group II findings on hard adaptation limits and the widening adaptation gap provide the evidentiary record that other seats cite, contest, or quietly set aside when it conflicts with the settlement's premises.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, ipcc_assessment_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__adaptation_priority, wealthy_developed_nations).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools resources to build protection where climate impacts land first and hardest: early-warning systems, coastal and flood defenses, drought-tolerant agriculture, heat-health infrastructure. Solves the genuine collective problem that exposed populations cannot self-fund resilience at the required scale.
% TRANSFER_FUNCTION: Moves adaptation finance (pledges, fund allocations, MDB loans, some private capital) from wealthy treasuries toward vulnerable-region projects; simultaneously moves the costs of unmitigated warming onto exposed populations and future generations, and moves avoided-transformation savings into wealthy economies.
% ABSENT_VOICES: Future generations have no seat at all. Climate justice movements and small island states hold formal observer access but lack agenda power over finance terms. Mitigation-first and degrowth advocates are framed out of the 'realistic' conversation by the settlement's definition of legitimacy itself.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority settlement vanished overnight, the climate finance architecture (funds, NAP processes, vulnerability indices, accreditation pipelines), the COP coalition structure, and vulnerability-indexed aid flows would lose their organizing principle; the three readings of the kernel would immediately renegotiate the vacuum, and currently shielded regions would stand unprotected mid-trajectory.
% FOUNDING_PROBLEM: The founding problem was the mitigation deadlock: emission-reduction demands threatened the development aspirations of poor nations and the political feasibility of action in rich ones. Adaptation offered a response that visibly protected people now, required no one to stop growing, and could be framed as both justice and prudence.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: UNEP Adaptation Gap Reports attest the chronic shortfall between adaptation needs and delivered finance; IPCC Working Group II attests hard adaptation limits and rising residual damages; small island states and V20 members attest insufficiency from the receiving seat. No benefiting party attests that the arrangement under-delivers; the insufficiency record comes entirely from external assessment bodies and exposed parties.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_legitimacy__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__adaptation_priority, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.62 at interval end) because the arrangement's largest financial fact is what it avoids: wealthy economies escape transformation costs entirely while the accepted trajectory compounds damages elsewhere; the $350B annual adaptation gap means the protective promise is structurally underfunded, and the loan-share of finance converts protection into recipient debt. Suppression (0.52) is moderate rather than high because the sibling readings are not banned — they remain codified in the same treaty corpus — but the adaptation-first framing sets the agenda, absorbs the scarce bandwidth of political attention and finance, and renders alternatives 'unrealistic'; suppression here is structural (agenda control, resource crowding-out), not coercive prohibition. Theater rises steadily (0.22 to 0.44) as pledge inflation, relabeling of development assistance as adaptation finance, and pilot-project optics substitute for closing the gap. The suppression_requirement series tracks genuine enforcement-capacity change, not mere extraction drift: the settlement's machinery matured from the Bali Action Plan's bargaining-chip status through the Cancun Adaptation Framework, Paris Article 7, the Global Goal on Adaptation work program, and the UAE Framework's measurable targets — codification hardened, and defending the accept-the-trajectory premise against intensifying justice and loss-and-damage pressure required more active maintenance. All three series share one time grid (points 0, 3, 6, 9, 12, 15, 18) so no metric row borrows another's endpoints. Accessibility collapse is low (0.35): understanding the arrangement does not eliminate alternatives, since mitigation and degrowth remain live, treaty-codified competitors. Resistance is elevated (0.55): climate justice campaigns, loss-and-damage fights, AOSIS objections, and youth movements contest the settlement continuously.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute sharply different types from identical structural data. From the wealthy-nation and industry seats the arrangement is genuine coordination they fund and administer: real money flows to real defenses, and nobody forced anyone to accept the bargain. From the trapped payer seats the same structure operates as enforced extraction: protection arrives late, partial, and partly as debt, while the trajectory it accepts guarantees the damages it purports to manage. From the future-generations seat there is no coordination component at all — only imposed cost. The engine computes this divergence from power, exit, and directional position; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the wealthy seats toward the subsidy end: wealthy_developed_nations combine beneficiary role, agenda control, and arbitrage-grade exit (capital mobility, manageable domestic exposure), placing them nearest d=0. carbon_intensive_industries and adaptation_finance_intermediaries likewise derive low d from beneficiary declarations plus mobile exit. Victim declarations drive the exposed seats toward the target end: future_generations (payer, powerless, trapped, no temporal presence) sit nearest d=1; small_island_developing_states approach it through existential trapping. The dual-positioned low_income_climate_vulnerable_regions are the tangled-rope crux: declared in both arrays, their derived d lands mid-high — they receive genuine flows (pulling toward beneficiary) but bear uncompensated residual damages, debt service, and the compounding trajectory (pulling toward target), with trapped exit amplifying the target side. Scope amplification applies modestly: the arrangement operates globally, where verification of delivered protection is hardest.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents two opposite mislabels. Read as pure rope, the arrangement's real deliveries (early-warning coverage, defensive infrastructure, agricultural adaptation) would hide the extraction channel — the preserved development model, the financed gap, the deferred compounding. Read as pure snare, the extraction channel would erase the fact that substantial genuine protection reaches exposed populations and that recipients themselves negotiate for more of it, not less. On mandatrophy: the founding problem (the mitigation deadlock) is contested rather than dead — the deadlock persists, and protection needs are real and growing — so the arrangement has not outlived its function; but its financing mandate has drifted from provision toward performance (rising theater ratio), which is the early signature the lifecycle tracker should watch. No sunset clause exists and none is sought: the reading presents itself as steady state, not transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (adaptation_priority) of the kernel climate_response_legitimacy; what would the sibling readings change structurally if they captured the legitimacy center?',
    'Comparative classification across the linked sibling stories: mitigation_priority would move wealthy nations from beneficiary to payer (transformation costs borne), shrink the victim set to transition losers, and re-time costs from deferred to immediate; degrowth_transformation would add wealthy consumers as payers and dissolve the development-model preservation that anchors this reading''s beneficiary structure.',
    'The beneficiary/victim structure, epsilon, and computed type of this story are valid only within the adaptation-priority reading; a kernel-level verdict favoring a sibling invalidates this story''s directionalities wholesale rather than adjusting them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame position: this story''s structure is reading-relative to the climate_response_legitimacy kernel.').

omega_variable(
    adaptation_hard_limits,
    'At what warming level do hard adaptation limits arrive — the point beyond which resilience infrastructure can no longer protect exposed populations — and how far is the accepted trajectory from that point?',
    'IPCC Working Group II limit assessments, empirical tracking of residual damages versus adaptation investment by region, and observed failure cases (uninsurable coastlines, wet-bulb thresholds exceeded despite heat-action plans).',
    'If hard limits sit near the accepted trajectory, the reading''s protective promise fails structurally and the arrangement collapses from tangled_rope toward snare — coordination cover over pure cost-shifting; if limits remain distant, the coordination core is durable and extraction stays bounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_hard_limits, empirical, 'Whether the accepted trajectory breaches the biophysical ceiling of the reading''s own protective method.').

omega_variable(
    finance_additionality_theater,
    'Is reported adaptation finance additional to pre-existing development assistance, or substantially relabeled and double-counted?',
    'OECD DAC marker audits, independent tracking initiatives (e.g. transparent-finance consortia), and reconciliation of pledged versus disbursed versus genuinely-new flows.',
    'High relabeling would raise the true theater_ratio well above the authored 0.44 and weaken the coordination-function gate supporting the tangled_rope claim; verified additionality would confirm the coordination core and bound theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(finance_additionality_theater, empirical, 'Whether the adaptation finance flow is new money or repackaged aid — the main uncertainty under the theater measurement.').

omega_variable(
    net_position_of_recipient_regions,
    'Are low-income climate-vulnerable regions net beneficiaries or net victims of the arrangement once adaptation receipts are weighed against residual damages, loan service, and opportunity costs of the accepted trajectory?',
    'Region-level net-flow accounting combining adaptation inflows, climate-attributable losses, debt-service schedules on climate finance, and counterfactual damage estimates under a mitigated trajectory.',
    'If recipients are net beneficiaries, the victim declaration overstates extraction and the arrangement sits closer to rope; if net victims (the authored position), the tangled_rope asymmetry is confirmed and deepens as warming compounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_position_of_recipient_regions, empirical, 'The dual-positioned seat''s true sign — the crux of the tangled-rope versus rope boundary.').

omega_variable(
    intergenerational_weighting,
    'What social discount rate legitimately governs the deferred, compounded costs this reading imposes on future generations?',
    'Not resolvable by data alone: depends on the ethical framework adopted (pure-time-preference versus prioritarian intergenerational weights); sensitivity analysis across the defensible range bounds the disagreement.',
    'A near-zero discount rate raises effective extraction toward the future-generations seat dramatically and pushes the arrangement toward snare; a market-rate discount shrinks the future-weighted term and supports the tangled_rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_weighting, preference, 'Value-dependent weighting of the deferred-cost channel; bounded by sensitivity analysis, not settled by evidence.').

omega_variable(
    cs_authority_framing,
    'Is the kernel''s authority structure better framed as lineage (treaty corpus with an interpretive apparatus: Rio to UNFCCC to Kyoto to Paris, interpreted by COP decisions and subsidiary bodies) or as distributed (an under-specified legitimacy criterion with no single adjudicator, producing competing readings without resolution)?',
    'Observe adjudication behavior across disputes: if COP outcomes and treaty-text appeals consistently settle legitimacy contests, the lineage framing holds; if legitimacy contests persist unresolved across cycles with no authoritative ruling, the distributed framing fits better.',
    'Under the distributed framing, interpretation_layer_present would be invalid and the commitment-system classification shifts from a mediated-lineage pattern to a fragmented multi-reader pattern; the reading_relations and drift_state authored here presuppose the lineage frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_authority_framing, conceptual, 'CS-framing under-determination: two coherent authority framings of the same kernel yield different commitment-system classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__adaptation_priority, 0, 19).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__adaptation_priority, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t3, climate_response_legitimacy__adaptation_priority, theater_ratio, 3, 0.25).
narrative_ontology:measurement_basis(clim_tr_t3, observed).
narrative_ontology:measurement(clim_tr_t6, climate_response_legitimacy__adaptation_priority, theater_ratio, 6, 0.29).
narrative_ontology:measurement_basis(clim_tr_t6, observed).
narrative_ontology:measurement(clim_tr_t9, climate_response_legitimacy__adaptation_priority, theater_ratio, 9, 0.33).
narrative_ontology:measurement_basis(clim_tr_t9, observed).
narrative_ontology:measurement(clim_tr_t12, climate_response_legitimacy__adaptation_priority, theater_ratio, 12, 0.37).
narrative_ontology:measurement_basis(clim_tr_t12, observed).
narrative_ontology:measurement(clim_tr_t15, climate_response_legitimacy__adaptation_priority, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t18, climate_response_legitimacy__adaptation_priority, theater_ratio, 18, 0.44).
narrative_ontology:measurement_basis(clim_tr_t18, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__adaptation_priority, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t3, climate_response_legitimacy__adaptation_priority, base_extractiveness, 3, 0.46).
narrative_ontology:measurement_basis(clim_be_t3, observed).
narrative_ontology:measurement(clim_be_t6, climate_response_legitimacy__adaptation_priority, base_extractiveness, 6, 0.5).
narrative_ontology:measurement_basis(clim_be_t6, observed).
narrative_ontology:measurement(clim_be_t9, climate_response_legitimacy__adaptation_priority, base_extractiveness, 9, 0.54).
narrative_ontology:measurement_basis(clim_be_t9, observed).
narrative_ontology:measurement(clim_be_t12, climate_response_legitimacy__adaptation_priority, base_extractiveness, 12, 0.57).
narrative_ontology:measurement_basis(clim_be_t12, observed).
narrative_ontology:measurement(clim_be_t15, climate_response_legitimacy__adaptation_priority, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t18, climate_response_legitimacy__adaptation_priority, base_extractiveness, 18, 0.62).
narrative_ontology:measurement_basis(clim_be_t18, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__adaptation_priority, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t3, climate_response_legitimacy__adaptation_priority, suppression_requirement, 3, 0.34).
narrative_ontology:measurement_basis(clim_su_t3, observed).
narrative_ontology:measurement(clim_su_t6, climate_response_legitimacy__adaptation_priority, suppression_requirement, 6, 0.38).
narrative_ontology:measurement_basis(clim_su_t6, observed).
narrative_ontology:measurement(clim_su_t9, climate_response_legitimacy__adaptation_priority, suppression_requirement, 9, 0.42).
narrative_ontology:measurement_basis(clim_su_t9, observed).
narrative_ontology:measurement(clim_su_t12, climate_response_legitimacy__adaptation_priority, suppression_requirement, 12, 0.46).
narrative_ontology:measurement_basis(clim_su_t12, observed).
narrative_ontology:measurement(clim_su_t15, climate_response_legitimacy__adaptation_priority, suppression_requirement, 15, 0.49).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t18, climate_response_legitimacy__adaptation_priority, suppression_requirement, 18, 0.52).
narrative_ontology:measurement_basis(clim_su_t18, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the kernel 'climate_response_legitimacy'. The colloquial label 'legitimate climate response' conflates three structurally distinct arrangements: this adaptation-priority reading (accept trajectory, protect the exposed), the mitigation-priority reading (cut emissions via innovation and pricing, preserve growth), and the degrowth-transformation reading (dismantle the growth imperative in wealthy nations). Each instantiates a different constraint with its own epsilon, beneficiary/victim structure, and failure modes: this reading's epsilon reflects the adaptation-deficit extraction channel; the mitigation reading's reflects stranded-transition and pace-of-decoupling channels; the degrowth reading's reflects transformation-coercion channels. The upstream/downstream structure runs through shared finance bandwidth: whatever reading captures the legitimacy center allocates the same scarce political and financial resources, so each story links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
