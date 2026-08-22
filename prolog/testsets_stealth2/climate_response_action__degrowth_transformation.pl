% ============================================================================
% CONSTRAINT STORY: climate_response_action__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__degrowth_transformation, []).

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
 *   constraint_id: climate_response_action__degrowth_transformation
 *   human_readable: Degrowth Transformation Reading of Climate Response Action
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The climate_response_action kernel — what a climate response requires —
 *   is contested among three live readings. This story instantiates the
 *   degrowth_transformation reading: climate response requires structural
 *   economic transformation that rejects GDP growth as organizing principle,
 *   prioritizing sufficiency, equity, and reduced material throughput over
 *   technological substitution. Per the kernel-reading ε-referent rule, ε is
 *   authored for the standing arrangement under contest — the
 *   growth-maintaining climate response regime of pledge-and-review
 *   negotiation, carbon markets, and net-zero accounting that leans on future
 *   removals — as this reading assesses it, NOT for the transformation this
 *   reading endorses. From this seat the standing regime coordinates real
 *   mitigation capacity while transferring stabilization costs to parties
 *   with the least responsibility and no seat: remaining carbon budgets are
 *   consumed by continued Northern growth, and net-zero accounting defers
 *   present cuts into claims on future removal. The reading's own demand —
 *   universal basic services, working-time reduction, democratic firm
 *   ownership, redistribution from Northern consumption to Southern
 *   development rights — appears in the six-questions interview and omegas as
 *   the reading's content, not as the ε referent. The claim and the metrics
 *   are authored independently; where the engine's per-seat computation
 *   diverges from the claim, that divergence is the measurement this corpus
 *   exists to take.
 *
 * KEY AGENTS:
 *   - unfccc_cop_process: agenda-setter (institutional / constrained) — administers the pledge-and-review architecture this reading contests; consensus rules bind it to growth-compatible pathways
 *   - global_north_high_consumption_households: primary beneficiary (powerful / constrained) — consumption preserved uncapped; material position tied to the arrangement persisting
 *   - carbon_intensive_industries: beneficiary with agenda-setting secondary role (institutional / arbitrage) — shapes national implementation; delay preserves asset values; hedges across policy futures
 *   - growth_dependent_financial_sector: beneficiary (institutional / constrained) — solvency models assume continued growth; intermediates the growth-compatible response designs
 *   - carbon_removal_developers: beneficiary (organized / mobile) — revenue scales with the pledged-versus-delivered gap the accounting asks them to cover
 *   - future_generations: primary target (powerless / trapped) — inherit depleted budgets and locked-in warming; hold no seat and cannot exit
 *   - global_south_development_populations: target (moderate / trapped) — development space narrowed by a consumed budget; negotiate collectively with limited leverage
 *   - climate_vulnerable_frontline_communities: target (powerless / trapped) — bear present impacts; migration is the only exit and least available to the poorest
 *   - degrowth_policy_coalition: excluded challenger (organized / identity_locked) — the reading's advocates; outside agenda-setting rooms; identity fused with the critique
 *   - ipcc_assessment_community: analytical observer (institutional / analytical) — the assessment body all three readings cite; collects nothing from the arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__degrowth_transformation, 0.78).
domain_priors:suppression_score(climate_response_action__degrowth_transformation, 0.62).
domain_priors:theater_ratio(climate_response_action__degrowth_transformation, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, extractiveness, 0.78).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_action__degrowth_transformation, "Degrowth Transformation Reading of Climate Response Action").
narrative_ontology:topic_domain(climate_response_action__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__degrowth_transformation, '4786e0e1-ba22-4830-a882-3396e04771a1').
narrative_ontology:cs_kernel_codification('4786e0e1-ba22-4830-a882-3396e04771a1', distributed).
narrative_ontology:cs_authority_grounding('4786e0e1-ba22-4830-a882-3396e04771a1', distributed).
narrative_ontology:cs_reading_relation('4786e0e1-ba22-4830-a882-3396e04771a1', climate_response_action__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('4786e0e1-ba22-4830-a882-3396e04771a1', climate_response_action__adaptation_priority, influences).
narrative_ontology:cs_axiom('4786e0e1-ba22-4830-a882-3396e04771a1', foundational, growth_maintenance_incompatible_with_stabilization).
narrative_ontology:cs_axiom_status(growth_maintenance_incompatible_with_stabilization, holdable).
narrative_ontology:cs_axiom_grounding('4786e0e1-ba22-4830-a882-3396e04771a1', growth_maintenance_incompatible_with_stabilization, empirically_contingent).
narrative_ontology:cs_axiom('4786e0e1-ba22-4830-a882-3396e04771a1', foundational, sufficiency_equity_over_technological_substitution).
narrative_ontology:cs_axiom_status(sufficiency_equity_over_technological_substitution, holdable).
narrative_ontology:cs_axiom_grounding('4786e0e1-ba22-4830-a882-3396e04771a1', sufficiency_equity_over_technological_substitution, deontological).
narrative_ontology:cs_reference_frame('4786e0e1-ba22-4830-a882-3396e04771a1', sufficiency_based_transformation_framework).
narrative_ontology:cs_drift_state('4786e0e1-ba22-4830-a882-3396e04771a1', contemporary_net_zero_accounting_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('4786e0e1-ba22-4830-a882-3396e04771a1', '').
narrative_ontology:cs_kernel_id(climate_response_action__degrowth_transformation, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, global_north_high_consumption_households).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, carbon_intensive_industries).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, growth_dependent_financial_sector).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, carbon_removal_developers).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, future_generations).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, global_south_development_populations).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, climate_vulnerable_frontline_communities).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, planetary_boundary_framework).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, ipcc_carbon_budget_accounting).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, decoupling_insufficiency_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the annual negotiation cycle through which national emissions pledges, accounting rules, carbon-market mechanisms, and climate-finance arrangements are set, and administers the pledge-and-review architecture agreed at Paris along with the carbon-market rulebook. Every substantive decision requires near-consensus among close to two hundred parties, which in practice means the agenda must accommodate continued growth-oriented development pathways for large economies. Its leverage over delivery is limited to transparency reviews and reputational pressure.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, unfccc_cop_process, agenda_setter,
    institutional, generational, constrained, global).

% The top-consuming households of high-income economies, whose travel, housing, diets, and goods purchases account for a disproportionate share of global material and energy throughput. The current response architecture caps no one's consumption directly: decarbonization proceeds through supply-side substitution, efficiency, and offsets while living standards are politically guaranteed. Their mortgages, pensions, and employment are tied to continued growth, so their material position depends on the arrangement persisting; voluntary downshifting is available to a minority and does not change the aggregate pathway.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, global_north_high_consumption_households, beneficiary,
    powerful, biographical, constrained, global).

% Fossil fuel producers, heavy industry, aviation, shipping, and industrial agriculture. They supply the energy and materials the current pathway consumes and shape national implementation through lobbying, litigation, campaign finance, and investment cycles. Delay in reducing throughput preserves the value of existing assets; several firms hedge by acquiring renewables and removal ventures, keeping profitable positions under multiple policy futures.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, carbon_intensive_industries, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__degrowth_transformation, carbon_intensive_industries, agenda_setter).

% Banks, asset managers, insurers, and pension systems whose solvency models assume continued growth in the productive base they hold claims on. Response designs compatible with expanding balance sheets — green bonds, transition finance, offset portfolios — are the ones they can intermediate profitably; designs that stabilize or shrink material throughput would impair collateral values and return assumptions across their books.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, growth_dependent_financial_sector, beneficiary,
    institutional, immediate, constrained, global).

% Project developers and technology firms whose revenues come from counting future carbon removals against present emissions under net-zero accounting. Their addressable market scales with the gap between pledged and delivered emission cuts; the larger the shortfall the accounting asks them to cover, the larger their pipeline of contracts, land acquisitions, and subsidies.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, carbon_removal_developers, beneficiary,
    organized, biographical, mobile, global).

% People not yet born who will inherit the atmospheric, ecological, and fiscal consequences of today's throughput. They hold no seat in any negotiation and cannot consent to or refuse what is deferred to them; as the remaining carbon budget shrinks each year, the cuts and removals they must deliver under tighter margins grow. Their interests enter the process only through advocacy by present parties and through legal arguments made on their behalf.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, future_generations, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__degrowth_transformation, future_generations, excluded).

% Populations of low- and middle-income countries, most with minimal historical emissions per capita. The development space remaining available to them has been narrowed by a carbon budget already largely consumed by industrialized economies, while adaptation finance arrives late and small relative to assessed need. Their states negotiate collectively as a bloc but hold limited leverage over the accounting rules that determine who bears which burden.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, global_south_development_populations, payer,
    moderate, generational, trapped, global).

% Communities in river deltas, small islands, arid margins, and flood- and fire-exposed regions already experiencing loss and damage. The response architecture addresses their situation primarily through adaptation funding rounds and loss-and-damage pledges whose disbursement lags assessed need. Migration is the main exit available to them, and it is costly, politically contested at destination, and least available to the poorest households.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, climate_vulnerable_frontline_communities, payer,
    powerless, immediate, trapped, regional).

% Researchers, social movements, and a minority of municipal and parliamentary actors organized around sufficiency, working-time reduction, universal basic services, and post-growth economics. They hold that the response architecture's growth assumptions are themselves the obstacle, and they campaign for post-growth scenarios in assessments, citizens' assemblies, and policy pilots. They gained formal scenario presence in recent assessment rounds and parliamentary hearings but remain outside the agenda-setting core of the negotiation process and hold no enforcement lever; their professional and activist identities are bound to the critique itself.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, degrowth_policy_coalition, excluded,
    organized, generational, identity_locked, global).

% The scientific assessment body whose carbon-budget accounting, scenario literature, and decoupling evidence all three readings of the climate response question cite. It administers nothing and collects nothing from the arrangements it assesses; its periodic assessment cycles reprice the empirical premises on which every reading depends.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, ipcc_assessment_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__degrowth_transformation, global_north_high_consumption_households).
narrative_ontology:fixing_cost_class(climate_response_action__degrowth_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The standing regime coordinates a response to a shared atmospheric commons problem: it negotiates effort-sharing for emissions reduction across nearly two hundred parties, standardizes accounting (inventories, carbon markets, net-zero conventions), and mobilizes climate finance — solving centrally what uncoordinated national action cannot.
% TRANSFER_FUNCTION: Moves mitigation burden and atmospheric space across time and place: present consumption in the Global North is preserved while the costs of stabilization — a depleted carbon budget, locked-in warming, adaptation deficits, and reliance on future removal at scale — are transferred to future generations and to Global South populations with the least historical responsibility.
% ABSENT_VOICES: Future generations have no seat anywhere in the process. Global South populations are present as states but their peoples' development claims are subordinated to growth-maintenance in the consensus rules. Frontline vulnerable communities appear only through adaptation-finance negotiations. Degrowth and sufficiency advocates were outside mainstream scenarios until recently and remain outside the agenda-setting rooms where accounting rules are written.
% DISAPPEARANCE_RATIONALE: Mitigation-priority advocates hold the world would be roughly unchanged: the degrowth reading is marginal in implementation terms and climate policy would proceed on green-growth and adaptation tracks regardless. Degrowth advocates hold the world would rearrange: the sufficiency and equity critique is the only force keeping throughput reduction and development-space questions on the agenda, and its removal would leave net-zero accounting and growth-maintenance unchallenged as the sole frame. The parties dispute which, so the verdict is contested.
% FOUNDING_PROBLEM: The degrowth reading was articulated to solve the problem that growth-maintaining climate response cannot stabilize the climate at a just burden allocation: absolute decoupling of GDP from emissions and material throughput has not occurred at the rates stabilization requires, carbon budgets are finite, and technological substitution plus offset accounting shifts rather than eliminates the burden onto those with no seat.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment carbon-budget accounting and consumption-based emissions data corroborate the biophysical premises from outside the degrowth advocacy set, and European Environment Agency material-footprint series corroborate the decoupling shortfall. Green-growth economists dispute the founding premise's feasibility claim, so the problem's existence is corroborated mainly by scientific assessment bodies rather than by the reading's own movement — and its central premise remains under live empirical contestation.
narrative_ontology:disappearance_verdict(climate_response_action__degrowth_transformation, contested).
narrative_ontology:founding_problem_status(climate_response_action__degrowth_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__degrowth_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_action__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__degrowth_transformation, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__degrowth_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   ε (0.78) is authored for the standing arrangement under contest as the degrowth reading assesses it. Extractiveness is high because the regime's burden allocation transfers stabilization costs to the least-responsible parties: the remaining carbon budget shrinks while Northern consumption continues uncapped, and net-zero accounting converts present inaction into claims on future removal at gigatonne scale. Suppression (0.62) is the active marginalization of post-growth alternatives — scenario exclusion until recent assessment rounds, electoral economics, financial lock-in — and is authored as a raw structural property; only extractiveness is scaled by directionality and scope in the engine's computation. Alternatives remain live (accessibility_collapse 0.35): this reading itself, and adaptation-first positions, are published, organized, and increasingly heard. Theater (0.52): a large share of the regime's observable activity — pledge cycles, voluntary offsets, net-zero declarations without implementing legislation — substitutes accounting for delivered throughput reduction, a share that has grown as the pledge-delivery gap has widened. Resistance (0.70) is sustained: climate movements, Global South negotiating blocs, and the degrowth critique all contest the regime. The claimed type, tangled_rope, is stated from this seat independently of the metrics: the regime's coordination function is real (measurement, finance, some delivered decarbonization) and it is the same structure through which the burden-shift operates. The measurement series run on one shared time grid (t=0,3,6,9,12,15,18) so every metric is authored at every examined point; the t=18 points are marked projected.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter/beneficiary seats compute differently. From the COP process and Northern-consumption seats the regime is a functioning, hard-won coordination achievement — the only machinery that exists, delivering transparency and real if insufficient decarbonization. From the future-generations, Global South, and frontline seats the same machinery operates as a deferral device: consensus rules that preserve growth pathways are experienced as the mechanism by which their claims are postponed. The degrowth coalition seat reads the coordination story as partly cover; the IPCC seat treats the empirical premises on all sides as contestable. Coalition dynamics differentiate the powerless payer seats: Global South populations hold coalition power through their negotiating bloc, which is why they carry moderate rather than powerless power; future generations cannot coalition at all (they are not yet constituents), and frontline communities are dispersed and resource-poor, so their burden sits nearer the full-target end than the bloc-negotiating seat's.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries of the standing arrangement — Northern high-consumption households, carbon-intensive industries, growth-dependent finance, removal developers — derive low d: the regime subsidizes their positions (uncapped consumption, preserved asset values, intermediable green instruments, contract pipelines scaled to the pledge-delivery gap). Victims — future generations, Global South development populations, frontline communities — derive high d: they bear deferred stabilization costs, narrowed development space, and present impacts. Exit modulates within the victim set: trapped exit (no seat, no exit, migration-only) keeps the payer seats near the full-target end, while industry's arbitrage-grade options (diversification, relocation, regulatory capture) dampen its effective burden despite its beneficiary position. Spatial scope is global for nearly every seat, and the engine's scope modifier applies to extractiveness, not to suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying the standing regime as tangled_rope rather than rope prevents its genuine coordination function — measurement, finance, negotiation machinery — from laundering the burden-shift as pure coordination; classifying it as tangled_rope rather than snare preserves the delivered mitigation capacity that a pure-extraction reading would erase, keeping this reading honest about what exists. The degrowth reading's founding problem — that growth-maintaining response cannot deliver just stabilization — is live, so no mandatrophy declaration is authored. The mandatrophy risk this story guards against is the reading's own: a sufficiency critique that hardens into a position maintained performatively after its empirical premises (decoupling feasibility, removal scalability) are resolved, kept alive by coalition identity rather than by the problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the climate_response_action kernel; would instantiating mitigation_priority or adaptation_priority instead change the victim set and burden allocation so much that the classification itself changes?',
    'Comparative generation of the sibling stories against the same referent rules; the disagreement is located in the growth-compatibility premise, so whichever reading a corpus adopts as the standing referent determines whose burden is measured.',
    'If the referent were re-grounded in a sibling reading, the beneficiary/victim ledgers shift (under this reading''s own arrangement, Northern consumers become the burden-bearing seat) and the type computation runs on different structural data entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a contested kernel; siblings would re-key the structural data.').

omega_variable(
    absolute_decoupling_feasibility,
    'Can GDP be absolutely decoupled from emissions and material throughput at the rates climate stabilization requires, or is the decoupling-insufficiency premise of this reading empirically correct?',
    'Consumption-based material footprint and emissions-intensity time series assessed against stabilization-rate benchmarks; natural experiments from national and municipal post-growth policy pilots.',
    'If deep absolute decoupling is demonstrated, this reading''s foundational empirical axiom is overridden and the reading collapses toward mitigation_priority; if not, mitigation_priority''s compatibility premise fails and this reading''s assessment of the standing regime is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_decoupling_feasibility, empirical, 'The empirical crux separating this reading from mitigation_priority.').

omega_variable(
    cdr_scalability,
    'Can carbon dioxide removal scale to the gigatonne levels net-zero accounting assumes, without the throughput reductions this reading demands?',
    'Deployment curves against the removal volumes assumed in national net-zero plans; energy, land, and water footprints of removal at scale.',
    'If removal scales, the standing regime''s deferral to future generations is partly redeemed and this reading''s high ε assessment weakens; if not, net-zero accounting is pure burden-shifting and ε rises further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cdr_scalability, empirical, 'Whether the regime''s removal reliance is redemption or deferral.').

omega_variable(
    political_feasibility_gate,
    'Is the transformation this reading demands implementable through ordinary democratic politics, or only through crisis windows — energy shocks, climate disasters, fiscal crises — that temporarily suspend growth-maintenance?',
    'Comparative politics of post-growth policy adoption; historical analysis of welfare-state expansion and wartime mobilization as precedents for rapid structural change under democratic constraint.',
    'If crisis-gated, the regime''s suppression of alternatives and the demand''s resistance are both stable between crises and the constraint''s dynamics are episodic rather than monotonic; if implementable ordinarily, resistance should fall as institutional footholds spread and the rising suppression series would need re-reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_feasibility_gate, empirical, 'Whether the demand''s political feasibility is ordinary or crisis-gated.').

omega_variable(
    reading_arrangement_extraction_symmetry,
    'Does this reading''s own endorsed arrangement — redistributing consumption space from Northern wealthy populations to Southern development and future generations — carry a burden profile of its own that a Northern-payer seat would classify the way this reading classifies the standing regime?',
    'Generate the mirrored story authored from a Global North high-consumption seat with the degrowth arrangement as the standing referent, and compare computed types across the pair.',
    'If the mirrored story computes as tangled_rope or snare, the disagreement between readings is a seat-index disagreement over burden allocation that evidence alone cannot resolve; if it computes as rope, the reading''s equity claim survives the symmetry test.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_arrangement_extraction_symmetry, preference, 'Symmetry check: the reading''s endorsed arrangement has its own burden allocation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__degrowth_transformation, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_action__degrowth_transformation, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t3, climate_response_action__degrowth_transformation, theater_ratio, 3, 0.41).
narrative_ontology:measurement_basis(clim_tr_t3, observed).
narrative_ontology:measurement(clim_tr_t6, climate_response_action__degrowth_transformation, theater_ratio, 6, 0.44).
narrative_ontology:measurement_basis(clim_tr_t6, observed).
narrative_ontology:measurement(clim_tr_t9, climate_response_action__degrowth_transformation, theater_ratio, 9, 0.46).
narrative_ontology:measurement_basis(clim_tr_t9, observed).
narrative_ontology:measurement(clim_tr_t12, climate_response_action__degrowth_transformation, theater_ratio, 12, 0.48).
narrative_ontology:measurement_basis(clim_tr_t12, observed).
narrative_ontology:measurement(clim_tr_t15, climate_response_action__degrowth_transformation, theater_ratio, 15, 0.5).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t18, climate_response_action__degrowth_transformation, theater_ratio, 18, 0.52).
narrative_ontology:measurement_basis(clim_tr_t18, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_action__degrowth_transformation, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t3, climate_response_action__degrowth_transformation, base_extractiveness, 3, 0.65).
narrative_ontology:measurement_basis(clim_be_t3, observed).
narrative_ontology:measurement(clim_be_t6, climate_response_action__degrowth_transformation, base_extractiveness, 6, 0.68).
narrative_ontology:measurement_basis(clim_be_t6, observed).
narrative_ontology:measurement(clim_be_t9, climate_response_action__degrowth_transformation, base_extractiveness, 9, 0.7).
narrative_ontology:measurement_basis(clim_be_t9, observed).
narrative_ontology:measurement(clim_be_t12, climate_response_action__degrowth_transformation, base_extractiveness, 12, 0.73).
narrative_ontology:measurement_basis(clim_be_t12, observed).
narrative_ontology:measurement(clim_be_t15, climate_response_action__degrowth_transformation, base_extractiveness, 15, 0.76).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t18, climate_response_action__degrowth_transformation, base_extractiveness, 18, 0.78).
narrative_ontology:measurement_basis(clim_be_t18, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_action__degrowth_transformation, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t3, climate_response_action__degrowth_transformation, suppression_requirement, 3, 0.48).
narrative_ontology:measurement_basis(clim_su_t3, observed).
narrative_ontology:measurement(clim_su_t6, climate_response_action__degrowth_transformation, suppression_requirement, 6, 0.5).
narrative_ontology:measurement_basis(clim_su_t6, observed).
narrative_ontology:measurement(clim_su_t9, climate_response_action__degrowth_transformation, suppression_requirement, 9, 0.53).
narrative_ontology:measurement_basis(clim_su_t9, observed).
narrative_ontology:measurement(clim_su_t12, climate_response_action__degrowth_transformation, suppression_requirement, 12, 0.56).
narrative_ontology:measurement_basis(clim_su_t12, observed).
narrative_ontology:measurement(clim_su_t15, climate_response_action__degrowth_transformation, suppression_requirement, 15, 0.59).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t18, climate_response_action__degrowth_transformation, suppression_requirement, 18, 0.62).
narrative_ontology:measurement_basis(clim_su_t18, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__degrowth_transformation, resource_allocation).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, climate_response_action__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, climate_response_action__adaptation_priority).

% DUAL FORMULATION NOTE:
% The colloquial label 'climate response' covers three structurally distinct commitments — what temperature outcome to aim for, by what economic means, and with what burden allocation — and per the ε-invariance principle each reading emits its own constraint story with its own ε, beneficiaries, and victims. This file is the degrowth_transformation reading. The upstream story (mitigation_priority) is the institutionalized default against which this reading defines itself; this reading exerts structural pressure on both siblings — its core premise is incompatible with mitigation's growth-maintenance premise within any single framework, and its throughput-reduction demand reshapes the legitimacy conditions of adaptation-only positions without ruling adaptation investment out. All three family members link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
