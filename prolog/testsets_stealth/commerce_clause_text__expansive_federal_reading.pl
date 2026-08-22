% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__expansive_federal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__expansive_federal_reading, []).

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
 *   constraint_id: commerce_clause_text__expansive_federal_reading
 *   human_readable: Expansive Federal Commerce Power (Substantial Aggregate Effects Reading)
 *   domain: constitutional/federalism/legal
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the commerce clause kernel: the
 *   expansive federal reading, under which interstate commerce encompasses
 *   all economic activity with substantial aggregate effects on national
 *   markets. The standing arrangement under contest is the post-1937 regime
 *   in which Congress and federal agencies regulate intrastate activity —
 *   homegrown wheat, homegrown medicine, local motel patronage, insurance,
 *   land use adjacent to waterways — whenever aggregated effects on the
 *   national economy are substantial. The claim and the metrics are
 *   independent authored facts: the reading is CLAIMED here as tangled_rope
 *   because the structure possesses both a genuine coordination function
 *   (interstate externalities, race-to-the-bottom dynamics, fifty-code
 *   transaction costs) and asymmetric extraction (state autonomy and local
 *   variation bear costs while the federal administrative state and national
 *   coalitions collect), sustained by active enforcement (Supremacy Clause
 *   preemption, judicial policing of the boundary, agency implementation).
 *   The engine computes per-seat classifications from the structural data;
 *   divergence between this claim and any computed seat type is the
 *   measurement the corpus exists to take. The sibling readings are separate
 *   constraints in separate files, linked through
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - federal_administrative_agencies: primary beneficiary and administering agenda-setter (institutional/identity_locked) — converts each doctrinal expansion into jurisdiction, dockets, staff, and budget
 *   - - congress: agenda-setter (institutional/mobile) — enacts under the power and writes the findings that invoke it
 *   - - supreme_court: boundary adjudicator (institutional/mobile) — owns the tests, moves the line in both directions, treats its own precedents as binding
 *   - - national_coalition_interest_groups: secondary beneficiary (organized/constrained) — achieves aims in one statute instead of fifty campaigns
 *   - - multistate_corporations: beneficiary with real compliance costs (powerful/arbitrage) — buys uniformity and preemption, retains unmatched access to reshape the terms
 *   - - state_governments: primary target (organized/trapped) — residual police powers yield wherever Congress acts; no lawful exit
 *   - - local_governments: diffuse target (powerless/trapped) — ordinances overridden with no seat at the drafting table
 *   - - regulated_individuals: micro-targets of the aggregation logic (powerless/trapped) — reached only by summing thousands of similar households
 *   - - state_sovereignty_advocates: excluded voice (moderate/constrained) — heard in briefs and dissents, rarely credited in operative doctrine
 *   - - constitutional_scholars: analytical observer (analytical/analytical) — audits the fit between findings and effects from outside the operating institutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, 0.66).
domain_priors:suppression_score(commerce_clause_text__expansive_federal_reading, 0.7).
domain_priors:theater_ratio(commerce_clause_text__expansive_federal_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__expansive_federal_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__expansive_federal_reading, "Expansive Federal Commerce Power (Substantial Aggregate Effects Reading)").
narrative_ontology:topic_domain(commerce_clause_text__expansive_federal_reading, "constitutional/federalism/legal").

domain_priors:requires_active_enforcement(commerce_clause_text__expansive_federal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__expansive_federal_reading, '5fab706d-964c-4fbf-80a4-484f85d6df79').
narrative_ontology:cs_kernel_codification('5fab706d-964c-4fbf-80a4-484f85d6df79', fixed_text).
narrative_ontology:cs_authority_grounding('5fab706d-964c-4fbf-80a4-484f85d6df79', lineage).
narrative_ontology:cs_interpretation_layer_present('5fab706d-964c-4fbf-80a4-484f85d6df79').
narrative_ontology:cs_reading_relation('5fab706d-964c-4fbf-80a4-484f85d6df79', commerce_clause_text__originalist_narrow_reading, forecloses).
narrative_ontology:cs_reading_relation('5fab706d-964c-4fbf-80a4-484f85d6df79', commerce_clause_text__substantial_effects_limited_reading, influences).
narrative_ontology:cs_axiom('5fab706d-964c-4fbf-80a4-484f85d6df79', foundational, aggregate_effects_confer_jurisdiction).
narrative_ontology:cs_axiom_status(aggregate_effects_confer_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('5fab706d-964c-4fbf-80a4-484f85d6df79', aggregate_effects_confer_jurisdiction, empirically_contingent).
narrative_ontology:cs_axiom('5fab706d-964c-4fbf-80a4-484f85d6df79', foundational, national_uniformity_preempts_local_variation).
narrative_ontology:cs_axiom_status(national_uniformity_preempts_local_variation, holdable).
narrative_ontology:cs_axiom_grounding('5fab706d-964c-4fbf-80a4-484f85d6df79', national_uniformity_preempts_local_variation, conventional).
narrative_ontology:cs_reference_frame('5fab706d-964c-4fbf-80a4-484f85d6df79', plenary_national_economic_authority).
narrative_ontology:cs_drift_state('5fab706d-964c-4fbf-80a4-484f85d6df79', contemporary_post_lopez_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5fab706d-964c-4fbf-80a4-484f85d6df79', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__expansive_federal_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, federal_administrative_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, national_coalition_interest_groups).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, multistate_corporations).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, state_governments).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, local_governments).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, regulated_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce the regulations the commerce power authorizes; each doctrinal expansion converts into new rulemaking dockets, enforcement staff, and budget lines. Their missions, career ladders, and institutional self-conceptions are built around jurisdictions that exist only under this reading — narrowing it would abolish offices rather than relocate work. They justify each expansion by pointing to interstate harms their programs address.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_administrative_agencies, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__expansive_federal_reading, federal_administrative_agencies, beneficiary).

% Enacts statutes under the commerce power and writes the findings that invoke it. Electoral cycles reward visible national action over deferred state experimentation, so members reach for federal instruments even where state authority could respond. They can widen or narrow their own statutes' reliance on the power and routinely attach preemption clauses that bind the states below them.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, congress, agenda_setter,
    institutional, biographical, mobile, national).

% Defines the boundary of the power case by case and owns the doctrinal tests — substantial effects, aggregation, nexus. It has moved the line in both directions, restricting in Lopez and Morrison and restoring breadth in Raich, and treats its own precedents as binding constraints on future adjustment. Justices write separately to signal which rival reading of the text they would adopt.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, supreme_court, agenda_setter,
    institutional, generational, mobile, national).

% Civil rights, environmental, labor, and public-health organizations can achieve their aims in one statute instead of fifty separate campaigns. Their membership, funding, and strategy all assume a national venue; a state-by-state alternative exists but multiplies cost and leaves hostile states unreformed, so shifting to purely local strategy is possible only at a heavy discount.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, national_coalition_interest_groups, beneficiary,
    organized, generational, constrained, national).

% Operate across state lines and face one federal compliance regime instead of fifty divergent codes; they lobby for federal floors that preempt stricter state rules and for federal ceilings that cap exposure. They also absorb real compliance costs and occasionally litigate against specific mandates, but their net position favors the arrangement and they retain unmatched access to reshape its terms.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, multistate_corporations, beneficiary,
    powerful, biographical, arbitrage, continental).

% Hold residual police powers that yield wherever Congress acts; federal statutes preempt their preferred policies in covered domains and conditional grants steer their budgets. They are represented in the Congress that binds them, but a state whose median voter disagrees with the national median has no lawful way out — no secession, no nullification, no opting out of federal supremacy. Their remaining levers are litigation, cooperative bargaining, and working to capture the federal agenda itself.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, state_governments, payer,
    organized, generational, trapped, regional).

% Regulate land use, local commerce, and community standards; federal preemption and nationwide injunctions override their ordinances with no seat at the drafting table. Their residents bear the difference between locally tailored rules and national defaults, and a municipality cannot relocate itself out of the arrangement's reach.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, local_governments, payer,
    powerless, biographical, trapped, local).

% Farmers growing grain for their own consumption, patients growing medicine for personal use, small operators whose conduct is reached only by aggregating thousands of similar households into a national market. Each enters the system as a one-off litigant carrying the full cost of a test case, with no collective voice and no practical way to move their activity outside the aggregated market the doctrine constructs.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, regulated_individuals, payer,
    powerless, immediate, trapped, local).

% Scholars, state officials, and litigators who argue the narrow or gated readings of the text; they file briefs, publish, and win occasional victories such as Lopez and Morrison that later doctrine contains. No forum exists where their reading could be adopted wholesale short of constitutional amendment, so their objection is heard but rarely credited in operative results.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, state_sovereignty_advocates, excluded,
    moderate, generational, constrained, national).

% Map the doctrine's evolution, audit the fit between legislative findings and measured effects, and document the reliance interests that raise the cost of doctrinal retreat. They collect no jurisdiction and bear no mandate; their assessments feed judicial opinions and reform proposals from outside the operating institutions.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__expansive_federal_reading, federal_administrative_agencies).
narrative_ontology:fixing_cost_class(commerce_clause_text__expansive_federal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves interstate collective-action problems that state-by-state regulation cannot: cross-border externalities (airsheds, watersheds, financial contagion, disease), races to the bottom as states compete for investment, and the transaction costs of fifty divergent commercial codes confronting a single national market.
% TRANSFER_FUNCTION: Moves regulatory authority — and the discretion, staffing, and agenda control attached to it — from state legislatures and local governments to Congress and federal agencies; imposes compliance obligations on firms and individuals subject to national standards; returns part of the flow to states as conditional grants tied to federal priorities.
% ABSENT_VOICES: Defenders of state autonomy and local self-government sit outside the operative conversation: they litigate case by case and publish, but no forum exists where their reading could be adopted without constitutional amendment. Affected individuals appear only as isolated test-case litigants, never as a class with negotiating presence in the design of the rules that reach them.
% DISAPPEARANCE_RATIONALE: National markets in goods, labor, capital, and information are constituted by the uniform rules this power supplies; civil rights, environmental, and financial enforcement all rest on it. Overnight removal would return the country to concurrent fragmented regulation — interstate externalities unpriced, fifty-code compliance burdens, and a scramble to rebuild national programs state by state or through amendment.
% FOUNDING_PROBLEM: Under the Articles of Confederation, states erected tariff barriers and discriminated against sister-state trade, revolutionary war debts went unserviceable, and no national market could form; the Philadelphia Convention gave Congress power to regulate commerce among the states to dissolve those barriers.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem itself is corroborated well outside any modern beneficiary: Madison's Federalist No. 42, Washington's 1785 circular letter on trade discord, and the ratification-debate records all attest it. On status, the parties split along the kernel's own fault line — federalism scholars across the spectrum agree the original barrier problem is solved, while economists document surviving cross-border externalities and network industries that proponents cite as its transformed descendants. No neutral body certifies either answer.
narrative_ontology:disappearance_verdict(commerce_clause_text__expansive_federal_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__expansive_federal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__expansive_federal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_text__expansive_federal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__expansive_federal_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__expansive_federal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__expansive_federal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.66: the arrangement displaces state regulatory choice across a vast domain, but a large share of what it displaces it replaces with functioning national governance, so the burden is substantial without being confiscatory. Suppression is authored at 0.70 as a RAW structural property — the engine scales only extractiveness (by directionality and scope), never suppression. Suppression is high because the targets' alternatives are legally nullified rather than merely costly: supremacy binds states regardless of consent, and no secession or nullification path exists. Theater ratio rises from 0.15 to 0.38 across the interval: the core regulatory function remains real, but a growing share of the doctrine's activity is ritualized — post-hoc 'substantial effects' findings written to satisfy a test the outcome does not depend on, most visibly in Raich, where the nexus language was performed and then disregarded. Accessibility_collapse is 0.55: once the doctrine is understood, state-regulatory alternatives inside covered domains collapse, but uncovered subjects, cooperative-federalism bargains, and capture of the federal agenda keep partial alternatives alive. Resistance is 0.60: Lopez and Morrison, sustained state litigation, anti-commandeering rulings, and legalization-as-nullification-in-practice show real, recurring pushback that the doctrine absorbs rather than eliminates. The temporal series run on ONE shared grid (1937–2026, eight points, every tracked metric authored at every point). The suppression_requirement series is included because the story specifically tracks enforcement-capacity change: machinery built steadily from 1937, dipped at 1995 (Lopez/Morrison constrained enforcement posture), then hardened again through Raich and the preemption expansion — a ratchet with one attempted reversal, not a cycle.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the federal administrative seat the arrangement is indispensable coordination the institutions built, staffed, and justified case by case; from the state and local seats the same structure operates as enforced subordination of their preferred policies; from the regulated-individual seat it arrives as an incomprehensible reach — a backyard crop aggregated into a national market. Multistate corporations straddle: they pay compliance costs yet buy the uniformity, so their seat sits nearer the beneficiary pole than their cost-bearing alone would suggest. The Court experiences the structure as interpretive stewardship — the same doctrine that reads as extraction from below reads as fidelity to precedent from the bench. The engine computes these divergences from power, exit, and directional data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: federal_administrative_agencies (identity_locked — their missions exist only inside this arrangement, so they cannot arbitrage away and sit deep at the beneficiary end), national_coalition_interest_groups (constrained exit to state-by-state strategy keeps them low), multistate_corporations (arbitrage-grade lobbying exit plus genuine uniformity rents keep them low despite real compliance costs). Victim declarations drive high directionality: state_governments (trapped — no exit amplifies their position toward the full-target end), local_governments (trapped, powerless), regulated_individuals (trapped, powerless, bearing the sharpest per-capita incidence of the aggregation logic). Congress and the Court carry no beneficiary or victim declaration: they administer and adjudicate rather than collect, so their directionality derives from the power-atom fallback — intermediate, reflecting that they bear the arrangement's maintenance costs and legitimacy risks alongside its agenda control. No directionality_overrides are authored: the beneficiary/victim declarations plus exit options already produce the correct ordering, and the shared institutional power atom (agencies, Congress, Court) would make any per-atom override collide across structurally different seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interstate tariff barriers and trade discrimination under the Articles — is dead as originally framed; every party concedes the national market exists. The arrangement persists because it acquired successor functions (externality pricing, network-industry governance, rights enforcement) that its founding text never named. The R5 mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: no zombie flag fires, because the parties genuinely dispute liveness rather than the arrangement persisting past unanimous agreement that its function is gone. The tangled_rope claim is what prevents mandatrophy misclassification in both directions: calling this a pure coordination mechanism erases the documented subordination of state autonomy (the victims array keeps that half visible); calling it pure extraction erases the externality-solving and market-constituting work that would collapse without it (the beneficiaries array and coordination_function keep that half visible). The rising theater_ratio series is the early-warning channel: if findings become fully ritualized while coverage keeps widening, the structure migrates toward extraction-with-coordination-cover, and the temporal data will date that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the expansive federal reading the correct instantiation of the commerce_clause_text kernel, or does one of the sibling readings (originalist_narrow_reading, substantial_effects_limited_reading) better capture the text''s commitment?',
    'Supreme Court doctrine revision, constitutional amendment, or a sustained judicial-scholarly consensus shift; observable in future Commerce Clause holdings that narrow or further extend effects-based jurisdiction.',
    'Adopting originalist_narrow_reading collapses federal jurisdiction over intrastate activity and dissolves most of this arrangement''s cost-bearing surface; adopting substantial_effects_limited_reading retains the coordination core while imposing nexus and non-pretext gates that cut the pretextual margin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the commerce clause kernel governs remains structurally contested; this file authors only the expansive reading.').

omega_variable(
    aggregation_logic_limitlessness,
    'Does the aggregation principle (Wickard, Raich) admit a principled stopping point, or can any activity be aggregated into a national market, making the reading''s coverage effectively unlimited?',
    'Identify a class of activity the Court refuses to aggregate, or formalize aggregation limits in doctrine; comparative analysis of the narrow band of exclusions recognized after Lopez.',
    'If aggregation is unlimited, effective extraction trends toward its ceiling and the arrangement drifts toward pure extraction; if bounded, the reading retains internal limits and the hybrid classification stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_logic_limitlessness, conceptual, 'Whether the aggregation warrant has principled limits or is an unlimited coverage engine.').

omega_variable(
    substantial_effects_pretext_rate,
    'What fraction of congressional ''substantial effects'' findings are genuine rather than pretextual rationalizations for reaching activity Congress wanted to regulate anyway?',
    'Compare enacted legislative findings against independent economic-effect studies and post-enactment effect data; audit the enacting-coalition record for findings written after the regulatory decision was made.',
    'A high pretext rate raises effective extraction above the authored base and increases drift toward pure extraction; a low rate supports the coordination reading of the same structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substantial_effects_pretext_rate, empirical, 'Share of effects findings that are pretextual rather than evidentiary.').

omega_variable(
    cooperative_federalism_offset,
    'How much of the burden borne by state governments is returned through conditional grants, cooperative administration, and delegated implementation authority?',
    'Fiscal-federalism accounting: compute states'' net transfer position under commerce-powered regimes against a counterfactual of state-only regulation of the same subjects.',
    'A large offset lowers net burden on the state seat and softens the asymmetry; a small offset confirms the subordination reading of the state-locality relationship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cooperative_federalism_offset, empirical, 'Net-versus-gross burden position of state governments under the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__expansive_federal_reading, 1937, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cc_expansive_fed_tr_t1937, commerce_clause_text__expansive_federal_reading, theater_ratio, 1937, 0.15).
narrative_ontology:measurement(cc_expansive_fed_tr_t1950, commerce_clause_text__expansive_federal_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement(cc_expansive_fed_tr_t1965, commerce_clause_text__expansive_federal_reading, theater_ratio, 1965, 0.22).
narrative_ontology:measurement(cc_expansive_fed_tr_t1980, commerce_clause_text__expansive_federal_reading, theater_ratio, 1980, 0.26).
narrative_ontology:measurement(cc_expansive_fed_tr_t1995, commerce_clause_text__expansive_federal_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(cc_expansive_fed_tr_t2005, commerce_clause_text__expansive_federal_reading, theater_ratio, 2005, 0.33).
narrative_ontology:measurement(cc_expansive_fed_tr_t2012, commerce_clause_text__expansive_federal_reading, theater_ratio, 2012, 0.35).
narrative_ontology:measurement(cc_expansive_fed_tr_t2026, commerce_clause_text__expansive_federal_reading, theater_ratio, 2026, 0.38).

% Extraction over time
narrative_ontology:measurement(cc_expansive_fed_be_t1937, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1937, 0.3).
narrative_ontology:measurement(cc_expansive_fed_be_t1950, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1950, 0.38).
narrative_ontology:measurement(cc_expansive_fed_be_t1965, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1965, 0.5).
narrative_ontology:measurement(cc_expansive_fed_be_t1980, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement(cc_expansive_fed_be_t1995, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(cc_expansive_fed_be_t2005, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2005, 0.63).
narrative_ontology:measurement(cc_expansive_fed_be_t2012, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2012, 0.64).
narrative_ontology:measurement(cc_expansive_fed_be_t2026, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2026, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(cc_expansive_fed_su_t1937, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1937, 0.4).
narrative_ontology:measurement(cc_expansive_fed_su_t1950, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1950, 0.48).
narrative_ontology:measurement(cc_expansive_fed_su_t1965, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1965, 0.55).
narrative_ontology:measurement(cc_expansive_fed_su_t1980, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1980, 0.62).
narrative_ontology:measurement(cc_expansive_fed_su_t1995, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(cc_expansive_fed_su_t2005, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2005, 0.64).
narrative_ontology:measurement(cc_expansive_fed_su_t2012, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2012, 0.67).
narrative_ontology:measurement(cc_expansive_fed_su_t2026, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2026, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__expansive_federal_reading, resource_allocation).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, originalist_narrow_reading).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Commerce Clause' decomposes into three structurally distinct constraints per the epsilon-invariance principle: this expansive reading, the originalist narrow reading, and the substantial-effects limited reading. Each carries its own epsilon, beneficiary/victim structure, and classification; forcing one story to span all three would make epsilon observer-dependent. The originalist narrow reading is the founding-era baseline the other two depart from; this reading's accumulated precedent generates the reliance interests that create structural downstream pressure on the limited reading's viability (each year of expansive operation raises the cost of adopting nexus gates). Family links run through affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
