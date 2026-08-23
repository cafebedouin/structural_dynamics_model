% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__livelihood_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: performance_legitimacy__livelihood_security_reading
 *   human_readable: Livelihood-Security Reading of Performance Legitimacy
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   A large developmental state runs its rule on a delivery bargain:
 *   continued acquiescence in exchange for tangible, directly experienced
 *   improvements in employment, healthcare, education, and eldercare. Under
 *   this reading of the performance-legitimacy kernel, that bargain is the
 *   operative arrangement: plan targets and cadre evaluations are keyed to
 *   service-delivery and consumption-support outcomes, fiscal priority shifts
 *   from capital-intensive industrial expansion and local infrastructure
 *   spending toward household consumption and the social safety net, and
 *   redistribution machinery strengthens. The bargain performs real
 *   coordination — it aligns a vast administrative apparatus with mass
 *   welfare needs — but its terms are non-negotiable: citizens may petition
 *   about delivery failures individually yet may not collectively renegotiate
 *   the exchange itself, and the constituencies that funded the old growth
 *   model pay for the pivot. Claim/metric independence is preserved: the
 *   story CLAIMS tangled_rope (genuine coordination plus asymmetric
 *   extraction held by active enforcement) while the authored metrics
 *   independently describe moderately high, rising extraction and enforcement
 *   intensity. The engine computes per-seat verdicts from the structural
 *   data; where a computed seat-type diverges from the claim, that divergence
 *   is the datum.
 *
 * KEY AGENTS:
 *   - - governing_party_apparatus: Agenda-setter and principal collector (institutional / identity_locked) — sets delivery targets, evaluates cadres on them, enforces the bargain's non-negotiable terms, receives continued rule as the return
 *   - - urban_households: Primary intended beneficiary with a hidden payment leg (moderate / constrained) — receives jobs, care, schooling; pays with bounded political voice and forgone investment-side returns
 *   - - rural_migrant_workers: Structural payers with partial beneficiary status (powerless / trapped) — supply the labor that makes delivery possible while registration rules ration their access to the promised services
 *   - - capital_intensive_industries: Budget-side payers (organized / constrained) — lose credit and fiscal priority to consumption support under this reading
 *   - - local_governments: Mandate-bearing payers (institutional / constrained) — ordered to expand services on shrinking land revenue and debt ceilings; absorb blame for shortfalls
 *   - - public_service_providers: Delivery-side beneficiaries (organized / constrained) — gain budgets and staffing under the reading's priorities
 *   - - retiree_pensioners: Visible beneficiary constituency (moderate / trapped) — depend on the eldercare and pension expansion the bargain showcases
 *   - - independent_labor_advocates: Excluded voices (powerless / trapped) — would demand collective bargaining and accountability beyond individual petition; kept outside
 *   - - development_policy_analysts: Analytical observers (analytical / analytical) — track fiscal flows and delivery data from outside the bargain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, 0.64).
domain_priors:suppression_score(performance_legitimacy__livelihood_security_reading, 0.68).
domain_priors:theater_ratio(performance_legitimacy__livelihood_security_reading, 0.39).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, theater_ratio, 0.39).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__livelihood_security_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__livelihood_security_reading, "Livelihood-Security Reading of Performance Legitimacy").
narrative_ontology:topic_domain(performance_legitimacy__livelihood_security_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__livelihood_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__livelihood_security_reading, 'd7cc0f02-5799-445b-ba25-1336e292fb34').
narrative_ontology:cs_kernel_codification('d7cc0f02-5799-445b-ba25-1336e292fb34', formalized).
narrative_ontology:cs_authority_grounding('d7cc0f02-5799-445b-ba25-1336e292fb34', practice).
narrative_ontology:cs_interpretation_layer_present('d7cc0f02-5799-445b-ba25-1336e292fb34').
narrative_ontology:cs_reading_relation('d7cc0f02-5799-445b-ba25-1336e292fb34', performance_legitimacy__quantitative_growth_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7cc0f02-5799-445b-ba25-1336e292fb34', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7cc0f02-5799-445b-ba25-1336e292fb34', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_axiom('d7cc0f02-5799-445b-ba25-1336e292fb34', foundational, material_delivery_grounds_rule).
narrative_ontology:cs_axiom_status(material_delivery_grounds_rule, holdable).
narrative_ontology:cs_axiom_grounding('d7cc0f02-5799-445b-ba25-1336e292fb34', material_delivery_grounds_rule, instrumental).
narrative_ontology:cs_axiom('d7cc0f02-5799-445b-ba25-1336e292fb34', foundational, experienced_welfare_outweighs_aggregate_output).
narrative_ontology:cs_axiom_status(experienced_welfare_outweighs_aggregate_output, holdable).
narrative_ontology:cs_axiom_grounding('d7cc0f02-5799-445b-ba25-1336e292fb34', experienced_welfare_outweighs_aggregate_output, empirically_contingent).
narrative_ontology:cs_reference_frame('d7cc0f02-5799-445b-ba25-1336e292fb34', livelihood_delivery_social_contract).
narrative_ontology:cs_drift_state('d7cc0f02-5799-445b-ba25-1336e292fb34', contemporary_demographic_fiscal_strain, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d7cc0f02-5799-445b-ba25-1336e292fb34', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__livelihood_security_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, governing_party_apparatus).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, urban_households).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, public_service_providers).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, retiree_pensioners).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, capital_intensive_industries).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, local_governments).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, rural_migrant_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, rural_migrant_workers).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, urban_households).
narrative_ontology:constraint_vindicates(performance_legitimacy__livelihood_security_reading, performance_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(performance_legitimacy__livelihood_security_reading, responsive_authoritarianism_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the plan priorities and delivery targets that cadres at every level are evaluated against, and enforces the bargain's terms: collective political claims are barred, grievances are channeled into individual petitions, and the delivery record is presented as the proof of good rule. It receives continued rule as the return on the arrangement, and it pays the enforcement and expectation-management costs. It cannot walk away from the bargain without confronting the absence of any alternative legitimacy source it is willing to name; the delivery record has become what the organization is.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, governing_party_apparatus, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__livelihood_security_reading, governing_party_apparatus, beneficiary).

% Receive the bargain's visible returns: expanding insurance coverage, school construction, pension payments, employment in the service economy the pivot favors. They pay on a second ledger that is rarely itemized: political voice is bounded to individual petition, and the consumption-over-investment tilt lowers the wage growth and asset returns the old model delivered. Emigration exists for the skilled minority but is costly and severs family and career.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, urban_households, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__livelihood_security_reading, urban_households, payer).

% Supply the labor that builds the cities and staffs the factories whose output funds the delivery promise, while household-registration rules ration their access to the urban schools, clinics, and pensions the promise describes. They receive targeted transfers when administratively designated poor. Collective bargaining is barred; the fallback is return to village subsistence, which forfeits the income that made migration rational.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, rural_migrant_workers, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__livelihood_security_reading, rural_migrant_workers, beneficiary).

% Lose credit allocation, fiscal priority, and political sponsorship as the reading demotes their claim on resources in favor of consumption support and service budgets. They retain lobbying channels and some overseas relocation options, but the core of their asset base is domestically locked, and the state remains their largest customer and creditor.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, capital_intensive_industries, payer,
    organized, generational, constrained, national).

% Were the agenda-setters of the old land-financed infrastructure boom; under this reading they are handed service-expansion mandates while their land-sale revenue shrinks and debt ceilings tighten. They cannot refuse the mandates, cannot repudiate the debts, and absorb the blame when delivery falls short. Exit within the hierarchy means career termination.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, local_governments, payer,
    institutional, biographical, constrained, regional).

% Hospitals, schools, and eldercare agencies gain budgets, staffing, and construction programs under the reading's priorities. They operate under quantified targets, satisfaction surveys, and inspection regimes; professional autonomy is bounded by the metrics their funding rides on.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, public_service_providers, beneficiary,
    organized, biographical, constrained, national).

% Depend directly on the pension indexation and eldercare expansion the bargain showcases, and form the most politically attentive constituency for its continuation. They have no alternative provider; their children's household finances are the implicit collateral behind the promise.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, retiree_pensioners, beneficiary,
    moderate, biographical, trapped, national).

% Would demand collective bargaining rights, independent inspection of service quality, and accountability mechanisms that survive individual delivery failures. Their networks are suppressed before they consolidate; their absence is what keeps the bargain's terms non-negotiable, since no seated party represents the claim that the exchange itself should be renegotiable.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, independent_labor_advocates, excluded,
    powerless, biographical, trapped, national).

% Track fiscal flows, demographic projections, subnational debt, and survey data from outside the bargain, publishing assessments of whether the delivery promise tracks fiscal and demographic reality. They collect nothing from the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, development_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__livelihood_security_reading, governing_party_apparatus).
narrative_ontology:fixing_cost_class(performance_legitimacy__livelihood_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns administrative capacity and fiscal allocation with mass welfare needs — employment, healthcare, education, eldercare — giving officials at every level a shared evaluative standard and giving households stable expectations about what the state owes them.
% TRANSFER_FUNCTION: Moves fiscal resources from capital-intensive industrial expansion and local infrastructure investment toward household consumption support and service delivery; and moves political voice from citizens to the state, as acquiescence is exchanged for delivered welfare on terms citizens did not set and cannot reopen.
% ABSENT_VOICES: Independent labor organizers, proponents of procedural accountability (electoral or judicial checks on the delivery claim itself), and investment-led-growth fiscal conservatives are outside the conversation. Grievance channels are deliberately individualized — petitions, hotlines, letters — so collective objection never reaches the table where allocation is decided.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would force an immediate substitute basis for acquiescence — intensified nationalism, heavier coercion, or rapid political opening — while fiscal flows snapped back toward investment. Cadre evaluation, budget lines, subnational obligations, and household expectations are all organized around the delivery standard; every one of those arrangements would have to rebuild from zero.
% FOUNDING_PROBLEM: After revolutionary ideology exhausted its mobilizing force, the state needed a durable basis for mass acquiescence without electoral competition: the founding problem was making material improvement the visible, checkable proof of good governance.
% FOUNDING_PROBLEM_CORROBORATION: Party congress reports and planning documents attest liveness from the benefiting side. Outside corroboration: World Bank and IMF fiscal analyses documenting the consumption-versus-investment rebalancing; UN and academic demographic projections showing the eldercare burden outrunning current funding arrangements; published studies of subnational debt and land-revenue decline; and comparative-politics scholarship on performance-based legitimacy treating the problem as open. No external source attests the founding problem is solved.
narrative_ontology:disappearance_verdict(performance_legitimacy__livelihood_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__livelihood_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__livelihood_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__livelihood_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__livelihood_security_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.64: the bargain delivers genuinely, but the exchange terms are set unilaterally — households cannot renegotiate, industrial and local-fiscal constituencies absorb the rebalancing costs, and delivery shortfalls fall hardest on the seats with the least exit. Suppression 0.68: persistence depends on keeping collective claims off the table (organizing barred, grievance individualized) and on containing dissatisfaction as delivery strains; suppression is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness is scaled, by directionality and spatial scope, in the engine's computation. Theater 0.39: delivery is real (coverage expansion, poverty-alleviation transfers, eldercare buildout), but a growing share of activity is performative — satisfaction surveys, model-site tours, statistical embellishment of targets — classic Goodhart drift as delivery metrics became career currency for cadres. Accessibility_collapse 0.5: alternatives (procedural accountability, emigration for the skilled) remain partly imaginable but are closed for most seats. Resistance 0.5: recurring petition waves, localized protests over healthcare and pension shortfalls, and quiet withdrawal from the bargain's expectations. All three series run on one shared time grid (points 0, 2, 4, 6, 8, 10, 12) so no metric row borrows another's end-state values; the trajectories are monotonic rather than cyclical, driven by target-gaming accumulation and enforcement hardening rather than oscillating crisis-and-reconciliation phases.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute divergent verdicts from identical structure. From the governing_party_apparatus seat the arrangement reads as the load-bearing social contract it administers — coordination it cannot survive abandoning, with exit fused into organizational identity. From urban_households and retiree_pensioners it reads as a mostly functional exchange with real, experienced returns. From rural_migrant_workers, capital_intensive_industries, and local_governments the same structure reads as extraction: migrants are rationed out of the promised services by registration rules; industries lose credit priority; local governments are handed unfunded mandates plus the blame for shortfalls. The engine derives these per-seat classifications from the declared roles, power atoms, and exit options — the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (party apparatus, urban households, service providers, pensioners) pull those seats toward the subsidized end of the directionality scale; victim declarations (capital-intensive industries, local governments, migrant workers) push those seats toward the full-target end. Urban households carry a secondary payer leg — bounded voice and indirect costs — so their derived position sits nearer symmetric than pure beneficiary. Migrant workers are listed among victims despite receiving targeted transfers because their net position on the promised services is negative under registration-based rationing, and their trapped exit holds them near the target end. The party apparatus is the gain-flow seat — the arrangement's gains accrue to it as continued rule — but it also pays enforcement and expectation-management costs, which keeps it short of the pure-beneficiary pole. No directionality overrides are authored: the derivation from declared roles plus exit options already separates the seats, and a power-atom-keyed override would misfire across the two institutional payers, which sit at opposite ends.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — non-electoral legitimacy secured through visible delivery — is still live, so this is not a mandatrophy case today: founding_problem_status=live combined with disappearance_verdict=world_rearranges produces no dead-mandate/zombie flag. The trajectory risk runs the other way. The measurement series shows theater and enforcement rising together with extraction — the signature of a bargain drifting toward enforcement-without-delivery. If delivery capacity breaks (the local fiscal ceiling binds, demographics outrun pension funding) while the enforcement machinery persists, founding_problem_status flips to dead, the mismatch consumer flags capture/zombie against the computed theater path, and the classification slides toward snare. Tracking the theater series against objective delivery indicators is the early-warning instrument for that flip.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the livelihood_security_reading of the performance_legitimacy kernel; would instantiating a sibling reading instead produce a different beneficiary/victim structure and a different classification?',
    'Identify episodes where fiscal allocation forces an explicit ranking among readings — stimulus design choosing consumption vouchers versus industrial subsidies, budget cycles pitting eldercare funding against strategic-industry funds — then author the winning reading as its own story and compare seat structures.',
    'Under quantitative_growth_reading the payer seats shift toward household consumption; under techno_nationalist_reading capital-intensive industries flip from victim to beneficiary, likely flipping several seats'' computed types. Only the livelihood reading concentrates victims on the investment side.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Kernel-contest delta: how sibling readings would rearrange this constraint''s seats.').

omega_variable(
    expectation_ratchet_destabilization,
    'Does the bargain destabilize from success — do rising expectations outpace delivery capacity so that persistence erodes even as absolute delivery improves?',
    'Compare subjective satisfaction series against objective delivery indicators (coverage rates, real service volumes); sustained divergence with rising objective delivery indicates a ratchet dynamic.',
    'If the ratchet dominates, the coordination payoff decays while enforcement persists — the drift path toward enforcement-without-delivery and eventually snare-flavored operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expectation_ratchet_destabilization, empirical, 'Whether achievement itself erodes the bargain''s sustainability.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (organization barred, grievance channels individualized) or internalized (a learned conviction that collective voice is futile or illegitimate)?',
    'Post-opening trajectory in comparable polities: proliferation of collective claims once barriers lift indicates structural suppression; persistence of individualized grievance habits after barriers lift indicates internalization.',
    'Internalized suppression travels with citizens past barrier removal, raising effective suppression above the structural measure and hardening the bargain against renegotiation even if enforcement relaxes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism behind the measured suppression.').

omega_variable(
    local_fiscal_capacity_binding,
    'Can subnational governments finance the mandated service expansion given land-revenue decline and debt controls, or does the reading''s mandate exceed fiscal reality?',
    'Consolidated audit of subnational balances, contingent liabilities, and unfunded service obligations against mandated expansion trajectories.',
    'If binding, shortfalls concentrate in the poorest jurisdictions, converting nominal beneficiaries (migrant workers, rural households) into net payers and steepening the extraction asymmetry the classification rests on.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(local_fiscal_capacity_binding, empirical, 'Whether the delivery mandate is fiscally executable at the subnational level.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__livelihood_security_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__livelihood_security_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(perf_tr_t0, observed).
narrative_ontology:measurement(perf_tr_t2, performance_legitimacy__livelihood_security_reading, theater_ratio, 2, 0.21).
narrative_ontology:measurement_basis(perf_tr_t2, observed).
narrative_ontology:measurement(perf_tr_t4, performance_legitimacy__livelihood_security_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement_basis(perf_tr_t4, observed).
narrative_ontology:measurement(perf_tr_t6, performance_legitimacy__livelihood_security_reading, theater_ratio, 6, 0.29).
narrative_ontology:measurement_basis(perf_tr_t6, observed).
narrative_ontology:measurement(perf_tr_t8, performance_legitimacy__livelihood_security_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement_basis(perf_tr_t8, observed).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__livelihood_security_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(perf_tr_t10, observed).
narrative_ontology:measurement(perf_tr_t12, performance_legitimacy__livelihood_security_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement_basis(perf_tr_t12, observed).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__livelihood_security_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(perf_be_t0, observed).
narrative_ontology:measurement(perf_be_t2, performance_legitimacy__livelihood_security_reading, base_extractiveness, 2, 0.46).
narrative_ontology:measurement_basis(perf_be_t2, observed).
narrative_ontology:measurement(perf_be_t4, performance_legitimacy__livelihood_security_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement_basis(perf_be_t4, observed).
narrative_ontology:measurement(perf_be_t6, performance_legitimacy__livelihood_security_reading, base_extractiveness, 6, 0.54).
narrative_ontology:measurement_basis(perf_be_t6, observed).
narrative_ontology:measurement(perf_be_t8, performance_legitimacy__livelihood_security_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(perf_be_t8, observed).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__livelihood_security_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(perf_be_t10, observed).
narrative_ontology:measurement(perf_be_t12, performance_legitimacy__livelihood_security_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement_basis(perf_be_t12, observed).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__livelihood_security_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(perf_su_t0, observed).
narrative_ontology:measurement(perf_su_t2, performance_legitimacy__livelihood_security_reading, suppression_requirement, 2, 0.52).
narrative_ontology:measurement_basis(perf_su_t2, observed).
narrative_ontology:measurement(perf_su_t4, performance_legitimacy__livelihood_security_reading, suppression_requirement, 4, 0.56).
narrative_ontology:measurement_basis(perf_su_t4, observed).
narrative_ontology:measurement(perf_su_t6, performance_legitimacy__livelihood_security_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement_basis(perf_su_t6, observed).
narrative_ontology:measurement(perf_su_t8, performance_legitimacy__livelihood_security_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement_basis(perf_su_t8, observed).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__livelihood_security_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(perf_su_t10, observed).
narrative_ontology:measurement(perf_su_t12, performance_legitimacy__livelihood_security_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement_basis(perf_su_t12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__livelihood_security_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the performance_legitimacy kernel. The colloquial label 'performance legitimacy' conflates four structurally distinct claims about WHAT counts as performance: directly experienced livelihood improvement (this story), headline GDP growth, high-quality structural transformation, and strategic-technological self-sufficiency. Each reading instantiates a different constraint with its own epsilon, beneficiary set, and victim set; this reading's victims (investment-side constituencies) are the techno-nationalist reading's beneficiaries, which is precisely why the readings cannot share one file. Family edges run from this story to all three siblings; upstream/downstream ordering is established per-pair by which reading wins contested budget cycles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
