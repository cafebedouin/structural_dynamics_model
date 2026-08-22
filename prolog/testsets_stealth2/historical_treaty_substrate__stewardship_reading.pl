% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__stewardship_reading, []).

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
 *   constraint_id: historical_treaty_substrate__stewardship_reading
 *   human_readable: Historical Treaty Substrate — Stewardship Reading (Living Covenant of Shared Territorial Stewardship)
 *   domain: legal/indigenous_law/comparative_constitutional
 *
 * SUMMARY:
 *   The historical treaty covenants between Indigenous nations and settler
 *   states were made as relational pacts: mutual, perpetual commitments to
 *   share territory — 'as long as the sun shines, the grass grows, and the
 *   rivers flow' — with no surrender of the nations' inherent jurisdiction.
 *   The standing arrangement under contest is the treaty regime as actually
 *   operated: the settler state holds territorial jurisdiction, collects
 *   resource revenue, and administers the relationship through its own courts
 *   and ministries, while the covenant's counter-obligations — consent,
 *   shared management of territory and resources — remain largely
 *   unperformed. This story instantiates the stewardship reading of the
 *   contested historical_treaty_substrate kernel; its epsilon is authored for
 *   the standing arrangement by this reading's own lights: a real coexistence
 *   framework whose fulfillment is withheld, not an endorsed alternative. The
 *   sibling readings are separate constraint files linked through the network
 *   block. KEY AGENTS (by structural relationship): - settler_state: primary
 *   beneficiary and agenda setter (institutional/constrained) — holds
 *   jurisdiction, collects revenue, controls interpretation -
 *   signatory_indigenous_nations: primary target (organized/identity_locked)
 *   — bear the arrangement's costs; the covenant is constituted through their
 *   nationhood - settler_resource_industries: secondary beneficiary
 *   (powerful/arbitrage) — receive territorial access under state permits
 *   without dealing with the nations - settler_municipalities: secondary
 *   beneficiary (moderate/constrained) — exist on treaty lands under the
 *   state's title - subnational_resource_ministries: secondary beneficiary
 *   and co-administrator (institutional/constrained) — issue permits, collect
 *   resource revenue - constitutional_courts: analytical observer
 *   (institutional/analytical) — adjudicate the covenant's scope -
 *   international_treaty_bodies: analytical observer (organized/analytical) —
 *   monitor conduct against international instruments -
 *   non_status_indigenous_descendants: excluded voice (powerless/trapped) —
 *   inherit the arrangement's burdens without standing in it
 *
 * KEY AGENTS:
 *   - settler_state: primary beneficiary and agenda setter (institutional/constrained) — holds territorial jurisdiction, collects resource revenue, controls the institutions that interpret the covenant
 *   - signatory_indigenous_nations: primary target (organized/identity_locked) — bear the costs; the covenant is constituted through their nationhood and territory
 *   - settler_resource_industries: secondary beneficiary (powerful/arbitrage) — operate on treaty lands under state permits, mobile capital
 *   - settler_municipalities: secondary beneficiary (moderate/constrained) — exist on treaty lands under the state's title and jurisdiction
 *   - subnational_resource_ministries: secondary beneficiary and co-administrator (institutional/constrained) — issue permits and collect provincial resource revenue
 *   - constitutional_courts: analytical observer (institutional/analytical) — adjudicate the covenant's scope and the honour-of-the-crown standard
 *   - international_treaty_bodies: analytical observer (organized/analytical) — monitor against international Indigenous-rights instruments
 *   - non_status_indigenous_descendants: excluded voice (powerless/trapped) — inherit the arrangement's burdens without standing in its benefits or governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, 0.78).
domain_priors:suppression_score(historical_treaty_substrate__stewardship_reading, 0.6).
domain_priors:theater_ratio(historical_treaty_substrate__stewardship_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__stewardship_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__stewardship_reading, "Historical Treaty Substrate — Stewardship Reading (Living Covenant of Shared Territorial Stewardship)").
narrative_ontology:topic_domain(historical_treaty_substrate__stewardship_reading, "legal/indigenous_law/comparative_constitutional").

domain_priors:requires_active_enforcement(historical_treaty_substrate__stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__stewardship_reading, '917bb458-d3e7-4cef-b482-aa89026248c9').
narrative_ontology:cs_kernel_codification('917bb458-d3e7-4cef-b482-aa89026248c9', distributed).
narrative_ontology:cs_authority_grounding('917bb458-d3e7-4cef-b482-aa89026248c9', distributed).
narrative_ontology:cs_reading_relation('917bb458-d3e7-4cef-b482-aa89026248c9', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('917bb458-d3e7-4cef-b482-aa89026248c9', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_axiom('917bb458-d3e7-4cef-b482-aa89026248c9', foundational, no_surrender_of_inherent_sovereignty).
narrative_ontology:cs_axiom_status(no_surrender_of_inherent_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('917bb458-d3e7-4cef-b482-aa89026248c9', no_surrender_of_inherent_sovereignty, deontological).
narrative_ontology:cs_axiom('917bb458-d3e7-4cef-b482-aa89026248c9', foundational, territorial_stewardship_is_shared_and_perpetual).
narrative_ontology:cs_axiom_status(territorial_stewardship_is_shared_and_perpetual, holdable).
narrative_ontology:cs_axiom_grounding('917bb458-d3e7-4cef-b482-aa89026248c9', territorial_stewardship_is_shared_and_perpetual, deontological).
narrative_ontology:cs_reference_frame('917bb458-d3e7-4cef-b482-aa89026248c9', ratified_coexistence_covenant).
narrative_ontology:cs_drift_state('917bb458-d3e7-4cef-b482-aa89026248c9', contemporary_reconciliation_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('917bb458-d3e7-4cef-b482-aa89026248c9', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, settler_state).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, settler_resource_industries).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, settler_municipalities).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, subnational_resource_ministries).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, signatory_indigenous_nations).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__stewardship_reading, living_treaty_doctrine).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__stewardship_reading, honour_of_the_crown_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds territorial jurisdiction over the treaty lands, collects resource royalties and land revenue, and administers the relationship through its courts and ministries. Made the covenants with solemn public commitments and now controls the institutions that decide what they mean. Its legitimacy claim rests on being a covenant partner; unilaterally repudiating the covenants would carry legitimacy costs at home and abroad, so its options are bounded even though it holds most of the power and most of the gains land with it.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state, agenda_setter,
    institutional, generational, constrained, continental).

% Made the covenants and understand them as perpetual mutual commitments — as long as the sun shines, the grass grows, and the rivers flow. They retain their own governance and law and continue to assert jurisdiction over their territories. They bear the arrangement's running costs: land and resource control held by the state, livelihoods constrained, communities administered under frameworks the state installed. Their relationship to the territory and the covenant is constitutive of who they are as nations; there is no exit from it that leaves the nation intact.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, signatory_indigenous_nations, payer,
    organized, generational, identity_locked, continental).

% Operate forestry, mining, agriculture, and energy projects on treaty lands under permits issued by the state. They receive access to territories without negotiating with the nations whose territories these are, and can relocate capital or shift operations when local conditions turn unfavorable. Their planning runs on investment cycles rather than the covenant's generational terms.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_resource_industries, beneficiary,
    powerful, immediate, arbitrage, regional).

% Sit on treaty lands and depend on the state's title and jurisdiction for their existence — roads, utilities, property tax bases, and legal personality all flow from the standing allocation. They stage land acknowledgments and enter occasional revenue-sharing conversations but hold no independent relationship with the nations. They cannot move.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_municipalities, beneficiary,
    moderate, biographical, constrained, local).

% Administer Crown and provincial lands and issue the permits that open treaty territories to industry. They collect resource revenue into their budgets and set the practical terms of the nations' participation — consultation adequacy, accommodation offers, project approvals. They answer to electorates whose livelihoods depend on the flow they administer.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, subnational_resource_ministries, beneficiary,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__stewardship_reading, subnational_resource_ministries, agenda_setter).

% Adjudicate what the covenants mean. Across recent generations they have articulated principles — the honour of the crown, the living tree, consultation and accommodation — that pull interpretation toward the nations' understanding, while stopping short of recognizing shared jurisdiction. They interpret but do not administer; their remedies are declared rather than governed.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Monitor the settler state's conduct against international instruments on Indigenous rights. They receive petitions and periodic reviews from the nations, publish findings, and exert reputational pressure. They hold no enforcement power inside the state.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, international_treaty_bodies, observer,
    organized, generational, analytical, global).

% Descendants of nations and communities left outside the written treaty lists — through non-signatory status, migration, or administrative reclassification under the state's membership registries. They inherit the arrangement's burdens without standing in its benefits or its governance, and the state's registry, not the nations' own law, determines their recognition. They would contest the allocation if the conversation's boundaries admitted them.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, non_status_indigenous_descendants, excluded,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__stewardship_reading, settler_state).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the framework through which two polities inhabit one territory: allocating land use, securing passage and livelihood, establishing annuity and material obligations, and replacing war with negotiated coexistence. Whatever its fulfillment, the framework itself is what both parties' day-to-day presence on the territory runs through.
% TRANSFER_FUNCTION: Moves territorial jurisdiction, land, and resource revenue from the nations to the settler state and its industries; moves annuities, services, and sporadic consultation accommodations from the state to the nations — a flow the covenant's own terms describe as reciprocal but which the standing operation leaves radically one-sided.
% ABSENT_VOICES: Non-status and Métis descendants excluded from the written treaty lists; Indigenous women, historically excluded from both the negotiations and the governance frameworks the state later installed; and future generations, who inherit the covenant without having been party to its making. All three would contest the allocation's current terms. The first two are kept outside by the state's registries and the settlement's boundaries of recognition; the last are structurally unrepresented anywhere in the arrangement's administration.
% DISAPPEARANCE_RATIONALE: If the treaty substrate vanished overnight, the settler state's root of title to the continent would be orphaned — every property regime, municipal boundary, and resource permit traces to instruments made under the covenant framework. The nations' assertion of jurisdiction would cease to be a claim against a counterparty and become the only remaining order on the territory. Courts, land registries, and the federation's internal division of powers would all need re-founding; the resource economy operating on treaty lands would lose its legal basis in a single stroke.
% FOUNDING_PROBLEM: Two peoples occupying one territory after covenant-making: how to secure peace, define each party's place, and enable the newcomers' settlement without extinguishing the nations' own life on the land. The covenants were made to answer that question with mutual obligation rather than conquest.
% FOUNDING_PROBLEM_CORROBORATION: The nations attest the founding problem is live from outside the benefiting parties — the perpetuity formulas were their own undertaking and they continue to perform and demand them. International human rights bodies corroborate independently, through periodic reviews and petition findings, that coexistence on the covenants' terms remains unresolved. The state's own courts corroborate partially, holding the covenants living and the honour of the crown engaged, while stopping short of the shared-jurisdiction reading. No party outside the state's administration attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__stewardship_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(historical_treaty_substrate__stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__stewardship_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__stewardship_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the state takes territorial jurisdiction, land, and resource revenue without the consent or shared management the covenant promises, while its counter-obligations are performed only marginally. Suppression (0.60) is authored as a raw structural property and is not scaled by power or scope: the enforcement machinery — policing of land defense, injunctions against assertion of jurisdiction, administrative control of reserve land and funding, and doctrinal suppression via extinguishment-friendly interpretation — holds the asymmetry in place. The suppression series traces a real enforcement-capacity arc, which is why suppression_requirement is tracked at all: it built to a peak (0.80) under the consolidated assimilation apparatus, then formally dismantled into a lower but persistent doctrinal-administrative register (0.60) — a rise-and-partial-decline, not a static picture. Theater (0.60) is the interval's most consequential drift: ratification-era maintenance was largely functional (0.15), ceremonial benevolence substituted for obligation through the assimilation era, a brief dip follows the activism-forced engagement of the modern-claims era, and the contemporary reconciliation apparatus — ubiquitous acknowledgment, commemoration, unimplemented calls to action — pushes performance above half of all maintenance activity. Accessibility collapse is 0.50: alternatives (litigation, modern claims processes, international instruments) partially persist, but the property regime built on the standing arrangement forecloses full exit. Resistance is 0.70: sustained litigation volume, blockades, land-back movements, and international advocacy meet the arrangement continuously. All series run on one shared time grid (T=0,25,50,75,100,125,150) so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the agenda-setter seat should compute differently, and the inter-institutional layer diverges too. From the settler state's seat, the arrangement is the coordination framework it built, administers, and honors ceremonially — a functioning covenant with unmet obligations. From the nations' seat, the same structure operates as enforced taking riding a real relationship: the covenant language is genuine but its fulfillment is withheld by force and doctrine — a seat from which the structure may compute as pure taking with the covenant as cover. Between institutional seats, the state, the resource ministries, and the courts experience the arrangement differently: ministries collect and allocate; courts articulate obligations they declare but do not administer; the state bears the legitimacy exposure of the gap between promise and performance. Same-level differentiation appears among the beneficiary seats: the state (institutional) and the industries (powerful) hold different power atoms, but the municipalities and ministries — both mid-power beneficiaries — diverge on exit (immobile place versus constrained administration) and horizon (biographical versus budgetary). The engine computes per-seat classifications from the structural data; the authored claim does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The settler state is the structural beneficiary and the agenda setter: it collects the jurisdiction, the revenue, and the interpretive control, so its derived directionality sits near the beneficiary end — partially damped by the legitimacy cost it carries for the covenant's non-fulfillment, but nowhere near symmetric. The resource industries sit nearest the beneficiary end of all seats: they receive access without bearing covenant obligations and hold arbitrage-grade exit (mobile capital, relocatable operations). Municipalities and resource ministries are beneficiaries with constrained exit — they collect from the standing allocation but cannot relocate. The signatory nations are the targets: they bear the costs (denied jurisdiction, extracted resources, administered governance) and their exit is identity_locked — the covenant and the territory are constitutive of their nationhood, so leaving the relationship is not a live option in any meaningful sense; this places them near the full-target end of the directionality range, and the identity-lock means the classification would not change by relaxing formal barriers alone. The courts and international bodies are analytical observers with no directional stake. The excluded descendants inherit costs without standing — their exclusion is part of the arrangement's operation, not an oversight in the model. No directionality overrides are declared: the beneficiary/victim declarations plus exit options already produce the correct relationships for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline matters here because the arrangement invites two opposite mislabels. Calling it pure taking would erase the genuine coordination function: the covenant framework is load-bearing — it is the root of title for every property regime on the territory, the annuities and reserve protections still flow through it, the courts' living-treaty line enforces real obligations, and coexistence on the territory runs through it. Calling it pure coordination would erase the asymmetry: the same structure that coordinates coexistence also channels the territory's value to one party under enforcement. The founding problem — two peoples inhabiting one territory — is live, so this is not an arrangement maintained by inertia after its function died; it is actively enforced, and the theater rise tracks proxy substitution (reconciliation performance) rather than functional death. Mandatrophy is not resolved: the covenant has not outlived its function — its function is unfulfilled. The temporal series is the diagnostic: rising theater alongside persistent, modernizing taking is the signature of a coordination structure carrying an extraction load it was never meant to bear, not of a dead mandate kept alive for show.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the historical_treaty_substrate kernel — the stewardship_reading, which holds the covenants as relational pacts with no cession of sovereignty and perpetual mutual obligations. Which reading of the kernel governs evaluation of the standing arrangement, and what would each sibling change structurally?',
    'Constitutional adjudication of the covenants'' scope, treaty implementation legislation, and domestication of international Indigenous-rights instruments; the disagreement is located in the covenant''s ontological status — completed transaction, international agreement, or living relational pact.',
    'Under the extinguishment_reading the nations drop out of the jurisdictional beneficiary set and the arrangement reads as a completed property transaction with residual welfare duties (much lower epsilon on this referent). Under the nation_to_nation_reading the same arrangement reads as a breached international agreement with remedies in modern treaty law. Under this reading the arrangement is a live covenant whose fulfillment is withheld — the epsilon authored here.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the treaty kernel governs: extinguishment, nation-to-nation, or stewardship.').

omega_variable(
    oral_promise_kernel_status,
    'Are the oral promises made at covenant-making — the perpetuity formulas, medicine chest clauses, hunting and fishing guarantees — part of the kernel itself, or mere negotiation context outside the binding text?',
    'Treaty commissioners'' records, contemporaneous minutes, and oral-history evidence admitted under the courts'' own treaty-interpretation principles; the disagreement is located in the kernel''s content boundary.',
    'If the oral promises are kernel-content, the covenant''s scope is far larger than the written text and measured shortfall against the full covenant is substantially higher than a text-only baseline; if not, this reading''s reference frame contracts toward the written instruments alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oral_promise_kernel_status, empirical, 'Whether oral promises are kernel-content or negotiation context.').

omega_variable(
    reconciliation_performance_ambiguity,
    'Is the contemporary reconciliation apparatus — land acknowledgments, commemoration, reconciliation frameworks — functional movement toward performing the covenant''s obligations, or performative substitution for them?',
    'Correlate acknowledgment activity with material indicators over time: land returned, revenue shared, jurisdiction recognized, implementation of reconciliation calls to action.',
    'If performative, theater_ratio continues rising and the covenant''s public performance drifts toward theatrical maintenance while the underlying shortfall persists — a degradation trajectory for the relationship''s visible form that would eventually date a type transition in the temporal series.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconciliation_performance_ambiguity, empirical, 'Whether reconciliation activity is functional or theatrical covenant maintenance.').

omega_variable(
    dependency_structural_vs_internalized,
    'Is the nations'' constrained position structural (state control of land administration, funding dependency, denial of jurisdiction, policing of land defense) or partly internalized (governance forms installed under the state''s administration that persist as the communities'' own self-conception)?',
    'Post-recognition governance trajectories: compare capacity recovery among nations that have regained jurisdiction over lands and services against nations still administered under the installed frameworks.',
    'If partly internalized, exit is more constrained than the structural measure suggests and the arrangement''s hold on the nations persists even after formal barriers fall — raising the effective suppression the nations carry beyond the structural scalar.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dependency_structural_vs_internalized, empirical, 'Structural versus internalized mechanism of the nations'' constrained position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__stewardship_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__stewardship_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hist_tr_t25, historical_treaty_substrate__stewardship_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(hist_tr_t50, historical_treaty_substrate__stewardship_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement(hist_tr_t75, historical_treaty_substrate__stewardship_reading, theater_ratio, 75, 0.4).
narrative_ontology:measurement(hist_tr_t100, historical_treaty_substrate__stewardship_reading, theater_ratio, 100, 0.42).
narrative_ontology:measurement(hist_tr_t125, historical_treaty_substrate__stewardship_reading, theater_ratio, 125, 0.52).
narrative_ontology:measurement(hist_tr_t150, historical_treaty_substrate__stewardship_reading, theater_ratio, 150, 0.6).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__stewardship_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(hist_be_t25, historical_treaty_substrate__stewardship_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(hist_be_t50, historical_treaty_substrate__stewardship_reading, base_extractiveness, 50, 0.76).
narrative_ontology:measurement(hist_be_t75, historical_treaty_substrate__stewardship_reading, base_extractiveness, 75, 0.74).
narrative_ontology:measurement(hist_be_t100, historical_treaty_substrate__stewardship_reading, base_extractiveness, 100, 0.71).
narrative_ontology:measurement(hist_be_t125, historical_treaty_substrate__stewardship_reading, base_extractiveness, 125, 0.73).
narrative_ontology:measurement(hist_be_t150, historical_treaty_substrate__stewardship_reading, base_extractiveness, 150, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__stewardship_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(hist_su_t25, historical_treaty_substrate__stewardship_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(hist_su_t50, historical_treaty_substrate__stewardship_reading, suppression_requirement, 50, 0.8).
narrative_ontology:measurement(hist_su_t75, historical_treaty_substrate__stewardship_reading, suppression_requirement, 75, 0.74).
narrative_ontology:measurement(hist_su_t100, historical_treaty_substrate__stewardship_reading, suppression_requirement, 100, 0.68).
narrative_ontology:measurement(hist_su_t125, historical_treaty_substrate__stewardship_reading, suppression_requirement, 125, 0.62).
narrative_ontology:measurement(hist_su_t150, historical_treaty_substrate__stewardship_reading, suppression_requirement, 150, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__stewardship_reading, resource_allocation).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__nation_to_nation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the historical treaties' covers three structurally distinct constraints (constraint family per the epsilon-invariance principle): the extinguishment reading (completed property transaction; nations outside the jurisdictional beneficiary set; lower epsilon on the shared referent), the nation_to_nation reading (breached international agreement; remedies in modern treaty law), and this stewardship reading (living covenant with withheld fulfillment; nations in the beneficiary set for territorial jurisdiction, the state in the obligation set for consent and shared governance). Each story carries its own epsilon, beneficiary structure, and classification over the same standing arrangement; the extinguishment reading currently dominates state practice, which is why the enforcement machinery runs on extinguishment-friendly interpretation while this reading's covenant terms go unperformed. The readings are linked as a family; upstream doctrinal movement in any one reading changes the legitimacy conditions of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
