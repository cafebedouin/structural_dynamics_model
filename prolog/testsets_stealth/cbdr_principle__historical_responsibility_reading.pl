% ============================================================================
% CONSTRAINT STORY: cbdr_principle__historical_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cbdr_principle__historical_responsibility_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: cbdr_principle__historical_responsibility_reading
 *   human_readable: CBDR Historical Responsibility Reading: Binding History-Proportional Reductions Plus Loss/Damage Finance
 *   domain: international_climate_governance/treaty_law/development_economics
 *
 * SUMMARY:
 *   Within the CBDR kernel, this reading holds that common but differentiated
 *   responsibilities require LEGALLY BINDING emissions reductions from
 *   developed nations scaled to their cumulative historical emissions, PLUS
 *   compensatory loss/damage financing for harms already incurred. The
 *   arrangement this reading instantiates has a genuine coordination function
 *   — differentiation is what made universal participation achievable at all
 *   — and a real asymmetric transfer running through the same structure:
 *   developed nations pay twice (abatement effort plus finance), developing
 *   nations receive. That combination of genuine coordination and asymmetric
 *   extraction through one enforced structure is why the claimed type is
 *   tangled_rope. Constraint-family note: the colloquial label 'CBDR'
 *   decomposes into two structurally distinct claims. This story's epsilon
 *   (0.74) describes the binding, history-proportional arrangement, where
 *   obligations are enforceable and the transfer is compensatory and
 *   open-ended. The sibling story
 *   (cbdr_principle__voluntary_commitment_reading) instantiates the
 *   voluntary-NDC arrangement, whose epsilon is lower — obligations are
 *   unenforceable by design and the developed-nation obligation is reframed
 *   as technology-transfer investment rather than compensation. Same kernel,
 *   different constraints, different victim sets, different enforcement
 *   requirements; the files are linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - annex_one_developed_nations: Primary target (institutional/constrained) — bears binding reductions scaled to cumulative history plus loss/damage finance transfers
 *   - developed_nation_taxpayers: Diffuse target (moderate/trapped) — ultimately fund the transfers through public budgets with no individual exit
 *   - carbon_intensive_export_industries: Target with arbitrage exit (powerful/arbitrage) — bears abatement costs at home but can relocate emissions-intensive production outside the obligation perimeter
 *   - least_developed_climate_vulnerable_states: Primary beneficiary (organized/constrained) — receives adaptation finance and loss/damage compensation
 *   - major_emerging_economies: Net beneficiary with growing exposure (powerful/constrained) — collects finance and emissions headroom while its own cumulative share grows
 *   - g77_china_negotiating_bloc: Agenda-shaping beneficiary (organized/constrained) — authors the equity texts the reading rides on
 *   - unfccc_treaty_machinery: Agenda setter (institutional/identity_locked) — administers reporting, review, and compliance; the process is its self-concept
 *   - energy_transition_displaced_workers: Excluded voice (organized/constrained) — bears transition costs with no treaty seat
 *   - ipcc_assessment_community: Analytical observer (analytical/analytical) — attributes cumulative emissions and audits pledge-versus-delivery gaps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, 0.74).
domain_priors:suppression_score(cbdr_principle__historical_responsibility_reading, 0.55).
domain_priors:theater_ratio(cbdr_principle__historical_responsibility_reading, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, theater_ratio, 0.54).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__historical_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__historical_responsibility_reading, "CBDR Historical Responsibility Reading: Binding History-Proportional Reductions Plus Loss/Damage Finance").
narrative_ontology:topic_domain(cbdr_principle__historical_responsibility_reading, "international_climate_governance/treaty_law/development_economics").

domain_priors:requires_active_enforcement(cbdr_principle__historical_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__historical_responsibility_reading, '0e5a2701-2134-47ec-bf28-cd7566e3d1d9').
narrative_ontology:cs_kernel_codification('0e5a2701-2134-47ec-bf28-cd7566e3d1d9', fixed_text).
narrative_ontology:cs_authority_grounding('0e5a2701-2134-47ec-bf28-cd7566e3d1d9', lineage).
narrative_ontology:cs_interpretation_layer_present('0e5a2701-2134-47ec-bf28-cd7566e3d1d9').
narrative_ontology:cs_reading_relation('0e5a2701-2134-47ec-bf28-cd7566e3d1d9', cbdr_principle__voluntary_commitment_reading, coexists_with).
narrative_ontology:cs_axiom('0e5a2701-2134-47ec-bf28-cd7566e3d1d9', foundational, binding_reductions_proportional_to_cumulative_history).
narrative_ontology:cs_axiom_status(binding_reductions_proportional_to_cumulative_history, holdable).
narrative_ontology:cs_axiom_grounding('0e5a2701-2134-47ec-bf28-cd7566e3d1d9', binding_reductions_proportional_to_cumulative_history, deontological).
narrative_ontology:cs_axiom('0e5a2701-2134-47ec-bf28-cd7566e3d1d9', secondary, loss_damage_compensation_is_owed).
narrative_ontology:cs_axiom_status(loss_damage_compensation_is_owed, holdable).
narrative_ontology:cs_axiom_grounding('0e5a2701-2134-47ec-bf28-cd7566e3d1d9', loss_damage_compensation_is_owed, deontological).
narrative_ontology:cs_reference_frame('0e5a2701-2134-47ec-bf28-cd7566e3d1d9', annex_split_with_binding_history_proportional_duties).
narrative_ontology:cs_drift_state('0e5a2701-2134-47ec-bf28-cd7566e3d1d9', post_paris_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0e5a2701-2134-47ec-bf28-cd7566e3d1d9', '').
narrative_ontology:cs_kernel_id(cbdr_principle__historical_responsibility_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, least_developed_climate_vulnerable_states).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, major_emerging_economies).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, g77_china_negotiating_bloc).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, annex_one_developed_nations).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developed_nation_taxpayers).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, carbon_intensive_export_industries).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, polluter_pays_principle).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, historical_cumulative_liability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Industrialized states with the largest cumulative historical emissions. Accept binding economy-wide reduction targets scaled to their cumulative emissions share and transfer public finance for adaptation and loss/damage. They retain formal withdrawal rights and heavy influence over how finance is channeled, but leaving the framework would cost diplomatic standing, expose their exporters to carbon border measures, and cede rule-writing to others.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, annex_one_developed_nations, payer,
    institutional, generational, constrained, global).

% Low-income and geographically exposed states that contributed little to cumulative warming. Receive adaptation finance and, under this reading, loss/damage compensation for harms already incurred. Their bargaining strength comes from moral standing and bloc coordination rather than market weight; walking away would forfeit the finance claims their budget planning increasingly relies on.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, least_developed_climate_vulnerable_states, beneficiary,
    organized, generational, constrained, global).

% Fast-industrializing states with rapidly growing cumulative emissions. Under this reading they take no binding reduction obligations and receive technology transfer plus a large share of climate finance. They also suffer real climate damages and face steady pressure to graduate toward obligations as their cumulative share grows, so their position inside the arrangement is favorable but not static.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, major_emerging_economies, beneficiary,
    powerful, generational, constrained, global).

% The developing-country coalition that drafts and defends the equity language this reading rests on. It sets negotiating agendas around historical responsibility and finance delivery, holds together very different member interests, and its members collectively receive the transfers and avoided obligations the reading assigns.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, g77_china_negotiating_bloc, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__historical_responsibility_reading, g77_china_negotiating_bloc, beneficiary).

% The secretariat, subsidiary bodies, and compliance committee that run reporting, technical expert review, and facilitative compliance processes. The process is the institution's self-concept: its budgets, staff careers, and authority are constituted by administering the framework, and its function cannot be relocated outside the treaty it serves.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, unfccc_treaty_machinery, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Households in industrialized states whose taxes fund the finance transfers and whose energy prices carry abatement costs. They are represented only indirectly through governments that negotiated the obligations, and no individual exit exists from public-budget commitments.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developed_nation_taxpayers, payer,
    moderate, biographical, trapped, national).

% Emissions-intensive traded sectors in developed states such as steel, cement, chemicals, and fuels. They bear compliance and border-adjustment costs at home but can relocate capacity to jurisdictions without binding obligations, taking their emissions with them. This relocation option shapes their posture toward the framework.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, carbon_intensive_export_industries, payer,
    powerful, biographical, arbitrage, continental).

% Workers and communities in fossil-dependent regions of developed states whose livelihoods decline as binding reductions bite. Unions represent them domestically, but they hold no seat in treaty negotiations where the pace of reduction is set, and reskilling or relocation options are limited by age, geography, and housing.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, energy_transition_displaced_workers, excluded,
    organized, biographical, constrained, regional).

% The scientific body that quantifies cumulative historical emissions, attributes observed warming, and audits the distance between pledged and delivered effort. It collects nothing and pays nothing under the arrangement; its assessments serve every other seat as shared factual ground.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, ipcc_assessment_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cbdr_principle__historical_responsibility_reading, major_emerging_economies).
narrative_ontology:fixing_cost_class(cbdr_principle__historical_responsibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the participation problem of global climate mitigation: by scaling obligations to cumulative historical emissions and capacity, it makes agreement rational for states that would otherwise refuse or free-ride, and it pools adaptation and loss/damage finance into channels that private capital does not serve.
% TRANSFER_FUNCTION: Moves two things from industrialized developed states to developing states: mandated emissions-reduction effort (the remaining atmospheric budget is allocated away from late industrializers' historical share) and public finance for adaptation and loss/damage.
% ABSENT_VOICES: Taxpayers and fossil-dependent workers in developed states are present only through governments that already accepted the obligations; future generations of all states have no seat; affected subnational communities enter only through accredited observers. The strongest contemporary objection — that the formula ignores where current emissions now come from — is voiced mainly by developed-state delegations, whose self-interest discounts it.
% DISAPPEARANCE_RATIONALE: Annex differentiation, the climate-finance architecture, G77 negotiating unity, and the equity grammar of every COP decision depend on this allocation formula. Remove it overnight and negotiations revert to the uniform-burden contests that collapsed at Copenhagen; finance flows lose their stated justification and would need an entirely new legitimating basis.
% FOUNDING_PROBLEM: Universal climate cooperation was blocked by radical inequality: the states responsible for most historical warming had the most capacity, while the most exposed states had contributed almost nothing. Equal-percentage burdens were politically impossible for the South and inadequate against the North's historical share.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: IPCC assessment reports document the attribution structure the formula responds to; the 95-0 Byrd-Hagel Senate vote shows developed-state legislatures treated the burden asymmetry as real enough to reject ratification over it; game-theoretic treatments of club goods corroborate that undifferentiated burdens block universal participation. No party disputes that the founding problem existed; the live dispute is whether cumulative history remains the right allocation basis now.
narrative_ontology:disappearance_verdict(cbdr_principle__historical_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(cbdr_principle__historical_responsibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cbdr_principle__historical_responsibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cbdr_principle__historical_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cbdr_principle__historical_responsibility_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cbdr_principle__historical_responsibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cbdr_principle__historical_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74 at interval end) because the arrangement moves both abatement effort and open-ended compensatory finance along one enforced channel, and the finance obligation scales with attribution rather than with marginal service cost. Suppression is moderate (0.55): the regime is formally consensual and withdrawal is legally available, but practical exit is priced by diplomatic isolation, carbon border adjustment exposure, and loss of rule-writing influence — coercion without courts. Theater is elevated and rising (0.54): a growing share of activity is pledge ceremony, relabeled development assistance counted as climate finance, and loan-heavy accounting presented as grant-equivalent support. Accessibility collapse is moderate-low (0.45) because alternative allocation formulas remain live — that is precisely why the kernel is contested. Resistance is high (0.68): Byrd-Hagel, the long US absence from Kyoto, Canada's withdrawal, the US Paris exit, and persistent finance-delivery pushback are all active defense of the payer position. The temporal series run on ONE shared nine-point grid (every tracked metric authored at every point, 1992-2024). Steps in the extractiveness trajectory track identifiable events: Kyoto's binding Annex I targets (t8), the Copenhagen/Green Climate Fund finance era (t16-t20), Paris loss-and-damage Article and enhanced transparency (t24), and the Loss and Damage Fund operationalization plus the NCQG (t32). The suppression_requirement series is authored deliberately: the story specifically traces enforcement-capacity maturation — Marrakesh compliance procedures, Bali MRV, the Paris transparency framework and Article 15 committee, and mounting finance-delivery review pressure — so the enforcement trajectory belongs in the series rather than in the static scalar alone.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda seats should compute differently, and the engine derives that divergence from the structural data rather than from this claim. From the Annex I seat the arrangement is an imposed liability with a formula it never accepted proportionally; from the vulnerable-states seat it is owed justice finally priced; from the treaty-machinery seat it is a growing mandate. Two finer divergences matter. INTER-INSTITUTIONAL: Annex I states and the G77 bloc sit at the same nominal institutional power tier with opposite directionalities — what differentiates them is not power but position relative to the transfer. SAME-LEVEL LATERAL: among developed-state actors, carbon-intensive export industries face the same obligations as domestic-only firms but hold an arbitrage exit (relocation outside the obligation perimeter), which changes both their experienced burden and their political behavior; carbon leakage is the visible signature of that exit differential. Identity-lock note: the treaty machinery's exit is identity_locked in the institutional sense — the organization has become its administering function, so it cannot credibly propose shrinking the framework it embodies.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: least_developed_climate_vulnerable_states and the G77 bloc sit near the full-beneficiary end (they receive transfers and avoided obligations, with constrained exit keeping them inside the structure); developed_nation_taxpayers sit nearest the full-target end (victims with trapped exit and no individual opt-out). Two overrides correct derivations the structural data alone would get wrong. First, major_emerging_economies derive as near-pure beneficiaries (d roughly 0.05-0.10), but they bear real climate damages and face graduation pressure inside the reading's own formula, so d is overridden to 0.22 — net beneficiary, not insulated. Second, annex_one_developed_nations derive as near-full targets (d roughly 0.95+), but they retain control of finance channels through multilateral bank intermediation and share the climate-stability benefit everyone gains from, so d is overridden to 0.85 — heavily targeted, not purely so. Carbon-intensive export industries are victims whose arbitrage exit damps their effective extraction below the trapped-victim value; the derivation captures that damping, so no override is needed. Receipt surface: the largest share of transferred value — both finance flows, which concentrate on middle-income recipients, and the avoided-binding-obligation headroom — accrues to major_emerging_economies, which is why gain_flow names that seat rather than 'diffuse'. Fixing is prohibitive: replacing the formula requires unanimous consent from the seats it benefits, and the one serious attempt (Copenhagen) collapsed the entire negotiating round.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: climate change is unresolved and differentiation remains the price of universal participation, so there is no atrophied mandate to detect and no sunset to expect. The classification discipline matters in both directions here. Calling this a snare would erase the demonstrated coordination achievement — undifferentiated burden-sharing proposals have repeatedly failed to produce universal agreements, while differentiated ones produced Rio, Kyoto, and Paris — and the participation function is real, not cover. Calling it a rope would erase the measured asymmetric transfer, the enforcement machinery that sustains it, and the identifiable payer seats who did not consent to the proportionality formula. Tangled rope holds both facts: the same structure that solves the collective-action problem moves resources asymmetrically and requires active enforcement to hold. The omega on the participation-versus-extraction tradeoff marks exactly where this reading could degrade toward snare if the coordination function fails while the transfer persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading (historical_responsibility_reading) of the cbdr_principle kernel; what structurally changes if the sibling reading (voluntary_commitment_reading) is adopted instead?',
    'Comparative analysis of the two instantiated constraint stories: the sibling removes developed nations from the victim set for binding obligations and compensatory finance, replaces the proportionality basis (cumulative history) with present capability, substitutes technology transfer for compensation, and drops the active-enforcement requirement.',
    'Under the sibling reading, developed nations exit the victim set entirely, developing nations lose their entitlement claim (entering at most a request position), and the constraint''s enforcement machinery becomes unnecessary — the classification of the kernel''s instantiation flips from tangled_rope toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame delta between the two readings of the CBDR kernel.').

omega_variable(
    cumulative_emissions_attribution_basis,
    'Which accounting basis defines ''cumulative historical emissions'' — start year (1850 vs 1950 vs 1990), territorial versus consumption-based accounting, treatment of colonial-era emissions?',
    'Convergence in attribution science (IPCC methodology, cumulative-emissions datasets) or an explicit negotiated accounting rule inside the framework.',
    'Shifts the proportionality formula and therefore the entire extraction distribution; a consumption-based or post-1950 basis pulls major_emerging_economies measurably toward the victim set, while an 1850 territorial basis maximizes the developed-nation share.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cumulative_emissions_attribution_basis, empirical, 'Attribution-methodology dependence of the proportionality formula.').

omega_variable(
    finance_additionality_theater_boundary,
    'How much of reported climate and loss/damage finance is genuinely additional to prior development assistance, versus relabeled aid and loan-heavy accounting booked at face value?',
    'Rigorous OECD DAC-style grant-equivalent accounting audits of climate finance flows against pre-commitment baselines.',
    'Determines whether the theater_ratio trajectory reflects ceremonial overhead around a real transfer or substitution of old money for new; if mostly relabeled, effective extraction falls sharply and the payer-seat grievance weakens correspondingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finance_additionality_theater_boundary, empirical, 'Additionality of the finance transfer versus accounting theater.').

omega_variable(
    differentiation_participation_tradeoff,
    'Is history-proportional differentiation primarily enabling participation (the coordination function) or primarily entrenching bloc-level rent collection (the extraction function)?',
    'Counterfactual comparison with uniform-burden negotiation episodes — the Copenhagen collapse is the closest natural experiment — plus analysis of whether finance delivery tracks need or bloc bargaining strength.',
    'Resolves the tangled_rope-versus-snare boundary for this reading: if the coordination function fails while the transfer persists, the arrangement degrades toward snare; if the transfer tracks need, the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(differentiation_participation_tradeoff, conceptual, 'Relative weight of the participation function versus the transfer function.').

omega_variable(
    emerging_economy_graduation_drift,
    'Will major emerging economies'' cumulative-emissions share grow until the reading''s own formula crosses them into the obligated set, making the reading self-undermining on its original terms?',
    'Updated cumulative-attribution time series and the internal-consistency pressure they generate inside the reading''s proportionality logic.',
    'Victim-set migration over time: either the reading evolves (new tiers, graduation clauses) or it survives only by abandoning the historical principle it was founded on — either path changes the constraint''s beneficiary/victim structure and its classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emerging_economy_graduation_drift, empirical, 'Whether the reading''s own formula eventually obligates its current beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__historical_responsibility_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_hist_resp_tr_t0, cbdr_principle__historical_responsibility_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(cbdr_hist_resp_tr_t0, observed).
narrative_ontology:measurement(cbdr_hist_resp_tr_t4, cbdr_principle__historical_responsibility_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement_basis(cbdr_hist_resp_tr_t4, observed).
narrative_ontology:measurement(cbdr_hist_resp_tr_t8, cbdr_principle__historical_responsibility_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(cbdr_hist_resp_tr_t8, observed).
narrative_ontology:measurement(cbdr_hist_resp_tr_t12, cbdr_principle__historical_responsibility_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement_basis(cbdr_hist_resp_tr_t12, observed).
narrative_ontology:measurement(cbdr_hist_resp_tr_t16, cbdr_principle__historical_responsibility_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement_basis(cbdr_hist_resp_tr_t16, observed).
narrative_ontology:measurement(cbdr_hist_resp_tr_t20, cbdr_principle__historical_responsibility_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement_basis(cbdr_hist_resp_tr_t20, observed).
narrative_ontology:measurement(cbdr_hist_resp_tr_t24, cbdr_principle__historical_responsibility_reading, theater_ratio, 24, 0.47).
narrative_ontology:measurement_basis(cbdr_hist_resp_tr_t24, observed).
narrative_ontology:measurement(cbdr_hist_resp_tr_t28, cbdr_principle__historical_responsibility_reading, theater_ratio, 28, 0.5).
narrative_ontology:measurement_basis(cbdr_hist_resp_tr_t28, observed).
narrative_ontology:measurement(cbdr_hist_resp_tr_t32, cbdr_principle__historical_responsibility_reading, theater_ratio, 32, 0.54).
narrative_ontology:measurement_basis(cbdr_hist_resp_tr_t32, observed).

% Extraction over time
narrative_ontology:measurement(cbdr_hist_resp_be_t0, cbdr_principle__historical_responsibility_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(cbdr_hist_resp_be_t0, observed).
narrative_ontology:measurement(cbdr_hist_resp_be_t4, cbdr_principle__historical_responsibility_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement_basis(cbdr_hist_resp_be_t4, observed).
narrative_ontology:measurement(cbdr_hist_resp_be_t8, cbdr_principle__historical_responsibility_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(cbdr_hist_resp_be_t8, observed).
narrative_ontology:measurement(cbdr_hist_resp_be_t12, cbdr_principle__historical_responsibility_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement_basis(cbdr_hist_resp_be_t12, observed).
narrative_ontology:measurement(cbdr_hist_resp_be_t16, cbdr_principle__historical_responsibility_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement_basis(cbdr_hist_resp_be_t16, observed).
narrative_ontology:measurement(cbdr_hist_resp_be_t20, cbdr_principle__historical_responsibility_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(cbdr_hist_resp_be_t20, observed).
narrative_ontology:measurement(cbdr_hist_resp_be_t24, cbdr_principle__historical_responsibility_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(cbdr_hist_resp_be_t24, observed).
narrative_ontology:measurement(cbdr_hist_resp_be_t28, cbdr_principle__historical_responsibility_reading, base_extractiveness, 28, 0.7).
narrative_ontology:measurement_basis(cbdr_hist_resp_be_t28, observed).
narrative_ontology:measurement(cbdr_hist_resp_be_t32, cbdr_principle__historical_responsibility_reading, base_extractiveness, 32, 0.74).
narrative_ontology:measurement_basis(cbdr_hist_resp_be_t32, observed).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_hist_resp_su_t0, cbdr_principle__historical_responsibility_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(cbdr_hist_resp_su_t0, observed).
narrative_ontology:measurement(cbdr_hist_resp_su_t4, cbdr_principle__historical_responsibility_reading, suppression_requirement, 4, 0.26).
narrative_ontology:measurement_basis(cbdr_hist_resp_su_t4, observed).
narrative_ontology:measurement(cbdr_hist_resp_su_t8, cbdr_principle__historical_responsibility_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement_basis(cbdr_hist_resp_su_t8, observed).
narrative_ontology:measurement(cbdr_hist_resp_su_t12, cbdr_principle__historical_responsibility_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement_basis(cbdr_hist_resp_su_t12, observed).
narrative_ontology:measurement(cbdr_hist_resp_su_t16, cbdr_principle__historical_responsibility_reading, suppression_requirement, 16, 0.41).
narrative_ontology:measurement_basis(cbdr_hist_resp_su_t16, observed).
narrative_ontology:measurement(cbdr_hist_resp_su_t20, cbdr_principle__historical_responsibility_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement_basis(cbdr_hist_resp_su_t20, observed).
narrative_ontology:measurement(cbdr_hist_resp_su_t24, cbdr_principle__historical_responsibility_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement_basis(cbdr_hist_resp_su_t24, observed).
narrative_ontology:measurement(cbdr_hist_resp_su_t28, cbdr_principle__historical_responsibility_reading, suppression_requirement, 28, 0.53).
narrative_ontology:measurement_basis(cbdr_hist_resp_su_t28, observed).
narrative_ontology:measurement(cbdr_hist_resp_su_t32, cbdr_principle__historical_responsibility_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement_basis(cbdr_hist_resp_su_t32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__historical_responsibility_reading, resource_allocation).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, cbdr_principle__voluntary_commitment_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'CBDR'. The label conflates two structurally distinct claims: (1) this story — binding reductions proportional to cumulative historical emissions plus loss/damage compensation, with active enforcement and developed nations in the victim set; (2) cbdr_principle__voluntary_commitment_reading — voluntary nationally determined contributions with technology transfer as the developed-nation obligation, no enforcement requirement, no compensatory victim set. The epsilon values differ widely because bindingness and compensatory framing determine whether the transfer is enforceable and open-ended. The upstream story (this one) supplies the equity grammar that the downstream voluntary reading must answer; each file links the other via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cbdr_principle__historical_responsibility_reading, powerful, 0.22).
constraint_indexing:directionality_override(cbdr_principle__historical_responsibility_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
