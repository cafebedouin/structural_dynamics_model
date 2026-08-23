% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__jurisdictional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__jurisdictional_sovereignty, []).

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
 *   constraint_id: border_control_legitimacy__jurisdictional_sovereignty
 *   human_readable: Jurisdictional-Sovereignty Reading of Border Control Legitimacy
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   A democratic state operates a border-control regime justified not as
 *   sovereign prerogative but as a three-way balance: protection obligations
 *   to the displaced, labor-market needs, and resident public consent, with
 *   enforcement constrained by proportionality and necessity review. This
 *   file instantiates the jurisdictional_sovereignty READING of the contested
 *   border_control_legitimacy kernel: sovereignty is authority to regulate
 *   rights and obligations within territory, and border-closure authority is
 *   not entailed by sovereignty but must be earned through the balance. The
 *   epsilon referent is the standing balanced-constrained arrangement as this
 *   reading assesses it — not the absolute-discretion arrangement the
 *   sovereignty_primary sibling would defend, nor the open-movement default
 *   the freedom_of_movement_primary sibling demands; those are other files.
 *   The reading's signature structural fact is the dual victim set:
 *   enforcement bears down on excluded migrants while admission bears down on
 *   displaced domestic workers, and the regime's legitimacy lives or dies on
 *   holding both harms inside tolerable bounds. The interval indexes years
 *   since the regime's consolidation (t0 approximates 1990, t30 approximates
 *   2020).
 *
 * KEY AGENTS:
 *   - destination_state_governments: agenda setter (institutional/constrained) — administers the balance, answers electorally for both flanks
 *   - destination_state_citizenries: primary beneficiary (organized/constrained) — hold the consent leg
 *   - destination_state_employers: beneficiary (powerful/arbitrage) — receive labor channels tuned to need
 *   - admitted_migrant_workers: beneficiary (moderate/constrained) — enter through the regime's labor leg
 *   - recognized_refugees: beneficiary (moderate/constrained) — receive the protection leg's payoff
 *   - excluded_migrants: primary target (powerless/trapped) — bear denial, interdiction, detention, removal
 *   - displaced_domestic_workers: second target (moderate/constrained) — bear sectoral wage and adjustment pressure
 *   - transit_state_governments: externalized-cost bearer (institutional/constrained) — host and intercept for compensation
 *   - judicial_review_bodies: constraining authority (institutional/analytical) — apply proportionality and necessity tests
 *   - migrant_advocacy_organizations: analytical observer (organized/analytical) — litigate and document the gap
 *   - smuggling_networks: excluded party (organized/arbitrage) — shadow market measuring suppressed demand
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, 0.58).
domain_priors:suppression_score(border_control_legitimacy__jurisdictional_sovereignty, 0.64).
domain_priors:theater_ratio(border_control_legitimacy__jurisdictional_sovereignty, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__jurisdictional_sovereignty, "Jurisdictional-Sovereignty Reading of Border Control Legitimacy").
narrative_ontology:topic_domain(border_control_legitimacy__jurisdictional_sovereignty, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__jurisdictional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__jurisdictional_sovereignty, 'cab9d637-b7a3-4ac4-a939-b015e26526a4').
narrative_ontology:cs_kernel_codification('cab9d637-b7a3-4ac4-a939-b015e26526a4', distributed).
narrative_ontology:cs_authority_grounding('cab9d637-b7a3-4ac4-a939-b015e26526a4', distributed).
narrative_ontology:cs_reading_relation('cab9d637-b7a3-4ac4-a939-b015e26526a4', border_control_legitimacy__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('cab9d637-b7a3-4ac4-a939-b015e26526a4', border_control_legitimacy__freedom_of_movement_primary, coexists_with).
narrative_ontology:cs_axiom('cab9d637-b7a3-4ac4-a939-b015e26526a4', foundational, sovereignty_is_jurisdictional_authority).
narrative_ontology:cs_axiom_status(sovereignty_is_jurisdictional_authority, holdable).
narrative_ontology:cs_axiom_grounding('cab9d637-b7a3-4ac4-a939-b015e26526a4', sovereignty_is_jurisdictional_authority, conventional).
narrative_ontology:cs_axiom('cab9d637-b7a3-4ac4-a939-b015e26526a4', foundational, admission_legitimacy_requires_three_way_balance).
narrative_ontology:cs_axiom_status(admission_legitimacy_requires_three_way_balance, holdable).
narrative_ontology:cs_axiom_grounding('cab9d637-b7a3-4ac4-a939-b015e26526a4', admission_legitimacy_requires_three_way_balance, instrumental).
narrative_ontology:cs_reference_frame('cab9d637-b7a3-4ac4-a939-b015e26526a4', balanced_jurisdictional_authority).
narrative_ontology:cs_drift_state('cab9d637-b7a3-4ac4-a939-b015e26526a4', contemporary_externalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cab9d637-b7a3-4ac4-a939-b015e26526a4', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, destination_state_citizenries).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, destination_state_employers).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, admitted_migrant_workers).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, recognized_refugees).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, displaced_domestic_workers).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, transit_state_governments).
narrative_ontology:constraint_vindicates(border_control_legitimacy__jurisdictional_sovereignty, proportionality_doctrine).
narrative_ontology:constraint_vindicates(border_control_legitimacy__jurisdictional_sovereignty, necessity_constraint_doctrine).
narrative_ontology:constraint_vindicates(border_control_legitimacy__jurisdictional_sovereignty, non_refoulement_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set admission categories and quotas, run asylum procedures, direct border and removal agencies, and answer electorally for enforcement outcomes. They harvest legitimacy from visibly managing the balance and suffer crises when enforcement violates basic rights or when admission outruns public consent. Treaty commitments and judicial review bound their discretion; exit would mean abrogating obligations or defying courts.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, destination_state_governments, agenda_setter,
    institutional, biographical, constrained, national).

% Hold the consent leg of the balance: their acceptance sets the political ceiling on admission. They receive ordered migration, aggregate labor-market protection, and a rights-respecting reputation, while carrying diffuse fiscal and adjustment costs. Emigration exists but is rare and costly, so their operative channel is voting and protest.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, destination_state_citizenries, beneficiary,
    organized, biographical, constrained, national).

% Receive admission channels tuned to labor needs — seasonal schemes, skilled visas — gaining workforce access without bearing enforcement costs. Where admission tightens they can relocate production or recruit abroad; their lobbying shapes quota design in both directions.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, destination_state_employers, beneficiary,
    powerful, biographical, arbitrage, global).

% Enter through the regime's labor channels with legal status tied to sponsors or sectors. They gain wages and residence the regime makes possible but face removal if status lapses; switching employers or sectors is bounded by visa conditions.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, admitted_migrant_workers, beneficiary,
    moderate, biographical, constrained, national).

% Receive protection status through the regime's asylum leg — the concrete payoff of the protection obligation. Status brings residence and eventual naturalization paths but leaves them dependent on adjudication quality and exposed when politics turn; return home is often impossible.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, recognized_refugees, beneficiary,
    moderate, generational, constrained, national).

% Bear the regime's denials: refused visas, rejected claims, interdiction, detention, removal. They have no seat in the consent calculus that excludes them and no lawful alternative route; the remaining options are dangerous irregular movement or indefinite waiting in transit states.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Work in sectors and regions where admitted labor competes directly — wage pressure, scheduling intensity, housing costs. They are citizens with votes and unions, but their specific harm is dispersed and slow, and relocation or retraining carries real costs.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, displaced_domestic_workers, payer,
    moderate, biographical, constrained, national).

% Host stranded movement and run externalized enforcement — interception funding, readmission agreements — in exchange for aid and diplomatic leverage. They absorb the destination states' enforcement burdens and the humanitarian costs of backlog, with limited ability to refuse given financial dependence.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, transit_state_governments, payer,
    institutional, biographical, constrained, regional).

% Apply proportionality and necessity tests to enforcement measures, strike down refoulement-adjacent practices, and define the legal ceiling of the balance. They command no enforcement apparatus of their own; their instrument is finding violations and ordering remedies.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, judicial_review_bodies, agenda_setter,
    institutional, generational, analytical, continental).

% Litigate, monitor detention conditions, and publish the enforcement record from outside the state apparatus. They speak for seats with no voice of their own and measure the gap between the regime's stated balance and its operation.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, migrant_advocacy_organizations, observer,
    organized, generational, analytical, global).

% Sell passage across the closed segments the regime maintains. Every tightening of lawful channels raises their margins; they are barred from any legitimating conversation and persist as the regime's shadow market.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, smuggling_networks, excluded,
    organized, immediate, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__jurisdictional_sovereignty, destination_state_governments).
narrative_ontology:fixing_cost_class(border_control_legitimacy__jurisdictional_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of managing cross-border human movement for a democratic polity: converting three incompatible demands — protection obligations to the displaced, labor-market requirements, and resident public consent — into a single administrable admissions-and-enforcement regime with defined categories, procedures, and limits.
% TRANSFER_FUNCTION: Moves admission opportunities (entry, work authorization, protection status) from an open pool to applicants selected by the three-way balance; moves the costs of denial (interdiction, detention, removal, waiting) onto rejected applicants; moves adjustment costs (sectoral wage pressure, service load) onto resident workers in competing positions; and moves enforcement burdens outward onto transit states via externalization agreements.
% ABSENT_VOICES: Excluded migrants are the structurally absent seat: the consent leg of the balance is held by residents, so those denied entry have no vote in the calculus that excludes them and appear only through advocates and courts. Would-be migrants deterred before reaching any procedure are absent even from the statistics. Smuggling networks are excluded by design; their objection would be self-interested, but their existence measures the demand the balance suppresses.
% DISAPPEARANCE_RATIONALE: If the balanced regime vanished overnight, the space it occupies would refill immediately: either closure regimes (emergency borders, mass interdiction) or open-movement defaults would take its place, labor sectors dependent on admitted workers would contract or reorganize around irregular channels, protection pathways would collapse into ad hoc emergency responses, and transit-state arrangements built on the regime's externalization would dissolve. The consent-protection-labor equilibrium is an operating dependency of contemporary welfare states, not decoration.
% FOUNDING_PROBLEM: Mid-twentieth-century mass displacement and post-colonial labor migration confronted states with a triple bind: humanitarian obligations owed to the displaced, structural labor demand, and democratic publics unwilling to accept unbounded admission. The interwar period had shown that both unmanaged influx and unrestricted exclusion destabilize liberal orders; the founding problem was designing an admissions regime that could honor protection duties and fill labor needs without forfeiting public consent.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: UNHCR global displacement reporting attests the protection leg's continuing scale; ILO and sectoral shortage studies attest the labor leg; long-run survey series (Eurobarometer, national election studies) attest the consent leg's persistence. No single source attests the three-way balance itself — that synthesis is this reading's own contribution and remains contested by the sibling readings; stating that plainly is part of the corroboration.
narrative_ontology:disappearance_verdict(border_control_legitimacy__jurisdictional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__jurisdictional_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__jurisdictional_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_control_legitimacy__jurisdictional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__jurisdictional_sovereignty, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type tangled_rope on structural grounds independent of the metrics: the regime solves a genuine collective-action problem (no democratic polity can run protection, labor, and consent through separate uncoordinated mechanisms without recurring crisis), AND the same structure takes asymmetrically from identifiable groups (excluded migrants pay denial costs through the very procedure that admits others; displaced workers pay adjustment costs through the labor leg), AND persistence requires active enforcement (agencies, detention, removal, externalization). Metrics authored independently as descriptive judgments: extractiveness 0.58 — substantial but capped by the proportionality constraint and the regime's real deliveries to admitted workers and refugees; suppression 0.64 — heavy coercive machinery (interdiction, detention, removal) softened but not eliminated by necessity review; theater_ratio 0.32 — mostly functional adjudication with a growing symbolic-enforcement share aimed at consent maintenance; accessibility_collapse 0.48 — alternatives (irregular movement, smuggling, non-entry) persist and the sibling readings remain live political options, so understanding the constraint does not collapse exits; resistance 0.6 — sustained litigation, advocacy, irregular migration itself, and closure-demand backpressure from below. Suppression is authored as a raw structural property and is not scaled by any context dimension; only extractiveness is scaled downstream by directionality and scope. The measurement series share one grid (t=0..30, step 5) with all three metrics authored at every point; all trajectories rise, modeling enforcement accumulation under a formally stable balance. Electoral cycles superimpose short oscillations on the monotonic trend; the authored grid samples the underlying trend, not the cycle.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the government seat the regime is legitimacy engineering: the same operation that excludes migrants manufactures the consent that keeps the welfare state stable. From the excluded-migrant seat the identical operation is arbitrary gatekeeping with life-scale stakes and no appeal to the consent calculus that decided it. From the displaced-worker seat it is under-protection: the balance's labor leg reads as their wage compression. From the judicial seat it is a rule-of-law object — a set of tests to apply. The engine computes these per-seat classifications from power, exit, and role data; the divergence between the administrator's coordination experience and the trapped payer's extraction experience is the perspectival content, not an authored verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: citizenries (consent served), employers (labor channels), admitted workers and refugees (entry and protection delivered) all sit toward the beneficiary end, with employers' arbitrage exit pushing them furthest down. Victim declarations drive high d: excluded migrants (trapped, powerless) sit nearest the full-target end; displaced domestic workers high but moderated by their civic resources; transit states high — they bear externalized enforcement — though compensation payments temper the derived value. Governments occupy a mixed seat the derivation handles correctly: administering the regime yields legitimacy gains (low-d component) while bearing enforcement costs and crisis exposure (high-d component), netting mid-low. No directionality_overrides are authored: the beneficiary/victim-plus-exit derivation reproduces these relationships without correction, and the override surface keys on power atoms too coarsely to improve on it here. Receipt surface: the regime's gains — consent stability convertible into electoral security — demonstrably accrue to the government seat, so gain_flow names destination_state_governments rather than diffuse; fixing_cost is prohibitive because any unilateral fix (full closure or open movement) detonates one flank of the balance, costing the fixer more than the status quo's frictions.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both failure directions. Reading the regime as pure rope would erase the migrant harm the measurements record — extraction has accumulated steadily (0.42 to 0.58) even as the formal balance stayed constant, exactly the rent-layering pattern the temporal series exists to catch. Reading it as pure snare would erase the genuine functions courts enforce and agencies deliver: non-refoulement holds often enough that the protection leg has real payoff, and labor channels really do match workers to shortages. The R5 genealogy shows the founding problem still live — displacement scale, labor gaps, and consent volatility are all current facts corroborated outside the beneficiary set — so no mandate-atrophy is declared. The forward risk is theater substitution: if symbolic enforcement keeps displacing adjudication (theater_ratio 0.18 to 0.32 and climbing), the balance's functional core thins while its performance thickens, and the regime would drift piton-ward inside a snare-ward gradient. The consent_endogeneity and proportionality_binding_force omegas are the early-warning instruments for that drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the border_control_legitimacy kernel — what changes structurally if a sibling reading is adopted instead?',
    'Track adoption events: constitutional or supranational court rulings entrenching one reading, treaty revisions, and doctrinal convergence in state practice; each adoption event re-instantiates the kernel as a different constraint with a different victim set.',
    'Under sovereignty_primary the victim set collapses to migrants alone and enforcement loses its proportionality constraint (epsilon rises sharply); under freedom_of_movement_primary the enforcement apparatus itself becomes the violation and the balancing function disappears.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Reading-indexed structure: sibling adoption changes victim sets and epsilon.').

omega_variable(
    dual_victim_weighting,
    'How should harms to excluded migrants weigh against harms to displaced domestic workers in the extractiveness assessment?',
    'Comparative harm accounting: longitudinal wage and employment studies for displaced-worker cohorts against measured welfare losses for excluded-migrant populations (income, safety, family unity).',
    'Weighting toward migrant harms raises epsilon and pushes the computed type toward snare; weighting toward citizen harms lowers epsilon and pulls the arrangement toward the sovereignty_primary flavor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_victim_weighting, preference, 'Contestable relative weights of the two acknowledged victim sets.').

omega_variable(
    consent_endogeneity,
    'Is public consent an exogenous constraint the balance must respect, or an endogenous quantity governments manufacture through enforcement spectacle?',
    'Natural experiments where enforcement visibility changed without admission levels changing (symbolic deployment surges): if measured consent tracks visibility rather than admission, consent is being produced, not consulted.',
    'If consent is manufactured, the balancing leg is partly theatrical — theater_ratio is understated and the regime drifts toward snare with a consent-production engine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_endogeneity, empirical, 'Whether the consent leg of the balance is a real limit or a managed output.').

omega_variable(
    proportionality_binding_force,
    'Do proportionality and necessity tests actually bind enforcement, or do they function as rubber-stamp justification?',
    'Measure reversal and compliance rates: share of enforcement measures modified or withdrawn after judicial proportionality review, and post-review recidivism (the same measure reintroduced in altered form).',
    'If the tests do not bind, suppression is effectively unconstrained and effective extraction runs near the sovereignty_primary level; if they bind, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_binding_force, empirical, 'Efficacy of the legal constraints distinguishing this reading from absolute discretion.').

omega_variable(
    externalization_accounting,
    'Do burdens pushed onto transit states (interdiction funding, readmission, stranded populations) count as extraction by this constraint?',
    'Follow the money and the people: audit externalization agreements for who bears interception and hosting costs, and whether transit-state consent is purchasable refusal or structurally compelled.',
    'Counting externalization raises epsilon, widens the victim set, and strengthens the asymmetric-extraction side of the tangled_rope assessment; excluding it treats a core enforcement mechanism as out of scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalization_accounting, conceptual, 'Scope question: whether externalized enforcement belongs inside this constraint''s extraction account.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__jurisdictional_sovereignty, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(bord_tr_t0, observed).
narrative_ontology:measurement(bord_tr_t5, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(bord_tr_t5, observed).
narrative_ontology:measurement(bord_tr_t10, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 10, 0.23).
narrative_ontology:measurement_basis(bord_tr_t10, observed).
narrative_ontology:measurement(bord_tr_t15, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 15, 0.26).
narrative_ontology:measurement_basis(bord_tr_t15, observed).
narrative_ontology:measurement(bord_tr_t20, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(bord_tr_t20, observed).
narrative_ontology:measurement(bord_tr_t25, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 25, 0.3).
narrative_ontology:measurement_basis(bord_tr_t25, observed).
narrative_ontology:measurement(bord_tr_t30, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 30, 0.32).
narrative_ontology:measurement_basis(bord_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(bord_be_t0, observed).
narrative_ontology:measurement(bord_be_t5, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 5, 0.46).
narrative_ontology:measurement_basis(bord_be_t5, observed).
narrative_ontology:measurement(bord_be_t10, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(bord_be_t10, observed).
narrative_ontology:measurement(bord_be_t15, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 15, 0.53).
narrative_ontology:measurement_basis(bord_be_t15, observed).
narrative_ontology:measurement(bord_be_t20, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 20, 0.56).
narrative_ontology:measurement_basis(bord_be_t20, observed).
narrative_ontology:measurement(bord_be_t25, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 25, 0.57).
narrative_ontology:measurement_basis(bord_be_t25, observed).
narrative_ontology:measurement(bord_be_t30, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(bord_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(bord_su_t0, observed).
narrative_ontology:measurement(bord_su_t5, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 5, 0.5).
narrative_ontology:measurement_basis(bord_su_t5, observed).
narrative_ontology:measurement(bord_su_t10, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 10, 0.55).
narrative_ontology:measurement_basis(bord_su_t10, observed).
narrative_ontology:measurement(bord_su_t15, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 15, 0.59).
narrative_ontology:measurement_basis(bord_su_t15, observed).
narrative_ontology:measurement(bord_su_t20, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(bord_su_t20, observed).
narrative_ontology:measurement(bord_su_t25, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 25, 0.63).
narrative_ontology:measurement_basis(bord_su_t25, observed).
narrative_ontology:measurement(bord_su_t30, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 30, 0.64).
narrative_ontology:measurement_basis(bord_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__jurisdictional_sovereignty, resource_allocation).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy__sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy__freedom_of_movement_primary).

% DUAL FORMULATION NOTE:
% 'Border control legitimacy' is a colloquial label covering three structurally distinct constraints — one per reading of the kernel. This file authors epsilon for the balanced-constrained arrangement (dual victim sets, proportionality-bound enforcement, epsilon approximately 0.58). The sovereignty_primary sibling authors epsilon for the absolute-discretion arrangement (single victim set, unconstrained enforcement, much higher epsilon). The freedom_of_movement_primary sibling authors epsilon for the closure arrangement as a rights violation (the enforcement apparatus itself is the harm). Upstream/downstream: the sovereignty_primary reading historically grounds the enforcement infrastructure this reading inherits and constrains; the freedom_of_movement_primary reading supplies the rights-based critique that drives the proportionality tests this reading depends on. Family members link via affects_constraints per the epsilon-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
