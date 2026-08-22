% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__expansive_federal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Expansive Aggregate-Effects Reading of the Commerce Clause
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested commerce-clause
 *   kernel: the expansive aggregate-effects reading under which Congress may
 *   regulate any economic activity, however local in form, if it
 *   substantially affects interstate commerce in the aggregate (the Wickard
 *   v. Filburn line running through Heart of Atlanta Motel and Gonzales v.
 *   Raich). This reading is not the text; it is a doctrinal commitment about
 *   what the text means, held by a specific line of federal judicial and
 *   administrative authority and contested by the originalist and
 *   substantial-effects-limited readings, which are separate constraint
 *   stories. Under this reading, federal regulatory authority over labor,
 *   agriculture, health care, and civil rights rests on treating small
 *   individual transactions as part of a national aggregate — a move that
 *   vindicates national market integration but subordinates state-level
 *   policy variation.
 *
 * KEY AGENTS:
 *   - federal_administrative_agencies: primary beneficiary and agenda-setter — gains jurisdiction over intrastate activity
 *   - national_policy_coherence_advocates: beneficiary — needs uniform national standards
 *   - national_labor_and_environmental_coalitions: beneficiary — relies on federal floor to avoid state races to the bottom
 *   - state_legislatures: primary payer — loses distinct local policy-setting authority
 *   - dissenting_state_governments: payer — bears cost of foreclosed local preference
 *   - originalist_jurists: excluded voice — dissenting doctrinal position without current controlling force
 *   - constitutional_historians: analytical observer of the doctrinal trajectory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, 0.58).
domain_priors:suppression_score(commerce_clause_text__expansive_federal_reading, 0.62).
domain_priors:theater_ratio(commerce_clause_text__expansive_federal_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__expansive_federal_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__expansive_federal_reading, "Expansive Aggregate-Effects Reading of the Commerce Clause").
narrative_ontology:topic_domain(commerce_clause_text__expansive_federal_reading, "constitutional/political").

domain_priors:requires_active_enforcement(commerce_clause_text__expansive_federal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__expansive_federal_reading, '99b6e802-894e-4609-871f-e31c784c860f').
narrative_ontology:cs_kernel_codification('99b6e802-894e-4609-871f-e31c784c860f', fixed_text).
narrative_ontology:cs_authority_grounding('99b6e802-894e-4609-871f-e31c784c860f', lineage).
narrative_ontology:cs_interpretation_layer_present('99b6e802-894e-4609-871f-e31c784c860f').
narrative_ontology:cs_reading_relation('99b6e802-894e-4609-871f-e31c784c860f', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_reading_relation('99b6e802-894e-4609-871f-e31c784c860f', commerce_clause_text__substantial_effects_limited_reading, influences).
narrative_ontology:cs_axiom('99b6e802-894e-4609-871f-e31c784c860f', foundational, aggregate_economic_effects_constitute_commerce).
narrative_ontology:cs_axiom_status(aggregate_economic_effects_constitute_commerce, holdable).
narrative_ontology:cs_axiom_grounding('99b6e802-894e-4609-871f-e31c784c860f', aggregate_economic_effects_constitute_commerce, conventional).
narrative_ontology:cs_axiom('99b6e802-894e-4609-871f-e31c784c860f', secondary, national_economic_uniformity_overrides_state_variation_when_aggregate_effects_substantial).
narrative_ontology:cs_axiom_status(national_economic_uniformity_overrides_state_variation_when_aggregate_effects_substantial, holdable).
narrative_ontology:cs_axiom_grounding('99b6e802-894e-4609-871f-e31c784c860f', national_economic_uniformity_overrides_state_variation_when_aggregate_effects_substantial, instrumental).
narrative_ontology:cs_reference_frame('99b6e802-894e-4609-871f-e31c784c860f', new_deal_crisis_necessity_framework).
narrative_ontology:cs_drift_state('99b6e802-894e-4609-871f-e31c784c860f', contemporary_administrative_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('99b6e802-894e-4609-871f-e31c784c860f', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__expansive_federal_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, federal_administrative_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, national_policy_coherence_advocates).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, national_labor_and_environmental_coalitions).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, state_legislatures).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, local_regulatory_experimentation).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, dissenting_state_governments).
narrative_ontology:constraint_vindicates(commerce_clause_text__expansive_federal_reading, national_market_integration_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_text__expansive_federal_reading, aggregate_effects_jurisprudence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain jurisdiction over economic activity that would otherwise sit with the states — labor conditions, agricultural production, environmental standards, health-care markets — by characterizing it as part of an interstate aggregate. Draft and enforce rules under this reading and have institutional reasons to keep the aggregate-effects doctrine broad, since it is the source of their regulatory reach.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_administrative_agencies, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__expansive_federal_reading, federal_administrative_agencies, agenda_setter).

% Legal scholars, national civil-rights organizations, and unions who need a single national floor (minimum wage, anti-discrimination, workplace safety) rather than fifty different regimes. Benefit directly from a reading that lets Congress reach wholly intrastate activity whenever it aggregates into something with a substantial national effect.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, national_policy_coherence_advocates, beneficiary,
    organized, generational, mobile, national).

% Rely on federal aggregate-effects authority to override state-level races to the bottom on wages, pollution, and workplace conditions. Their bargaining leverage depends on the doctrine staying broad; a narrowing reading would push protective standards back into state legislatures where their leverage is uneven.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, national_labor_and_environmental_coalitions, beneficiary,
    organized, biographical, mobile, national).

% Lose the ability to set distinct local economic policy once an activity is characterized as having substantial aggregate effects on interstate commerce — that characterization is available for almost any recurring, non-trivial economic activity. Can litigate under the anti-commandeering and enumerated-powers doctrines but cannot exit the constitutional structure itself.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, state_legislatures, payer,
    institutional, generational, constrained, national).

% The practice of states adopting divergent economic rules to test policy approaches — variation that a broad national floor displaces once Congress occupies the field. Not an actor itself, but a capacity that atrophies as the range of activity subject to exclusively federal treatment expands.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, local_regulatory_experimentation, payer,
    moderate, generational, trapped, regional).
narrative_ontology:stakeholder_non_agent(commerce_clause_text__expansive_federal_reading, local_regulatory_experimentation).

% States whose electorates prefer a different regulatory balance (lighter agricultural regulation, different labor rules, different environmental thresholds) find their preferred policy foreclosed once the activity is drawn into the interstate-commerce aggregate. They can seek Supreme Court review case by case but bear the compliance cost while litigation proceeds.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, dissenting_state_governments, payer,
    moderate, biographical, constrained, regional).

% Judges and scholars who hold that the constitutional text and founding-era usage support a narrower reading are structurally present in dissent but not in the controlling doctrine once the expansive reading holds a majority on the Court; their objection is preserved in opinions but does not currently govern outcomes.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, originalist_jurists, excluded,
    institutional, civilizational, analytical, national).

% Study the doctrinal trajectory from Gibbons v. Ogden through Wickard v. Filburn to the modern aggregate-effects tests, documenting how each expansion was justified and contested without holding a stake in which reading prevails.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__expansive_federal_reading, federal_administrative_agencies).
narrative_ontology:fixing_cost_class(commerce_clause_text__expansive_federal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows Congress to set uniform national rules for economic activity that, taken individually in any one state, looks local, but which in aggregate materially affects interstate markets — preventing a patchwork of inconsistent state rules from undermining national economic policy (wage floors, agricultural quotas, environmental baselines).
% TRANSFER_FUNCTION: Moves regulatory authority over a wide swath of nominally local economic activity from state legislatures to Congress and federal agencies, and moves the practical capacity to set divergent local policy away from state and local governments toward a single national standard-setter.
% ABSENT_VOICES: State legislators and local constituencies who would have preferred a different regulatory balance rarely get a forum once the aggregate-effects characterization attaches — their objection is doctrinal (raised in litigation) rather than political, and by the time a case is decided the underlying local policy choice has typically already been displaced.
% DISAPPEARANCE_RATIONALE: If the expansive aggregate-effects reading disappeared, wide areas of current federal regulation (federal minimum wage coverage of intrastate businesses, agricultural production quotas on activity never sold across state lines, federal environmental rules reaching purely local land use) would lose their constitutional basis, forcing either a return to state-by-state regulation or a scramble to re-ground federal statutes in narrower commerce theories or other enumerated powers.
% FOUNDING_PROBLEM: The New Deal-era problem of state-level races to the bottom and inconsistent economic regulation undermining a national economy in crisis — Congress needed a way to regulate national economic problems (agricultural overproduction, labor standards, later civil rights) that manifested through millions of individually small, formally local transactions.
% FOUNDING_PROBLEM_CORROBORATION: Federal agencies and national labor/civil-rights coalitions attest the founding problem remains live — national economic integration and civil-rights enforcement still require aggregate-effects authority. Independent constitutional historians and originalist jurists, outside the beneficiary set, attest that the doctrine's continued breadth substantially exceeds what the founding-era crisis required and has become a general-purpose grant of regulatory jurisdiction rather than a crisis-specific remedy.
narrative_ontology:disappearance_verdict(commerce_clause_text__expansive_federal_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__expansive_federal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__expansive_federal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_text__expansive_federal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__expansive_federal_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored moderate-high (0.58) because the reading transfers real, substantial regulatory authority away from states without state consent, but the transfer serves a genuine coordination function (preventing a race-to-the-bottom patchwork) rather than being pure rent extraction — this is why the claim is tangled_rope, not snare. Suppression (0.62) reflects that the reading's persistence depends on continued judicial enforcement (stare decisis, doctrine maintained across many circuits) rather than on voluntary state acquiescence; states that disagree cannot simply opt out. Theater ratio is modest (0.28) — the coordination function (national economic uniformity) is largely real, not performative, though some invocations of aggregate effects on tenuous factual chains (e.g., extending the doctrine to activity with only speculative aggregate impact) function more as jurisdictional cover than genuine coordination need.
 *
 * PERSPECTIVAL GAP:
 *   From the federal agency seat, this reading is the mechanism that lets national economic and civil-rights policy function at all — a rope solving a genuine multi-state coordination failure. From the state-legislature seat, the same doctrine is an enforced subordination of local policymaking capacity that persists regardless of local preference, closer to tangled_rope or even snare depending on how tenuous the aggregate-effects showing is in a given application. The engine should compute these seats differently given the same structural data — agenda-setter/beneficiary seats read low d, payer seats read high d.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal administrative agencies and national coalition beneficiaries get low d — they collect regulatory reach and policy uniformity from the doctrine's operation. State legislatures and dissenting state governments get high d — they surrender policymaking authority they would otherwise hold, with no meaningful exit (a state cannot secede from federal commerce jurisdiction; its only recourse is doctrinal litigation, which is slow and often unsuccessful once the aggregate-effects characterization attaches).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (New Deal-era national economic crisis requiring coordinated response to state-level regulatory failure) is genuinely contested as to whether it remains live in its original form. Classifying this as tangled_rope rather than snare preserves the genuine coordination function it still performs in some domains (interstate pollution, national labor standards) while flagging, through the extraction and suppression metrics, that the doctrine has been extended well past crisis-specific need into a general-purpose jurisdictional grant. A pure mountain or pure rope classification would erase the real state-autonomy costs; a pure snare classification would erase the real coordination benefit national uniformity provides in some domains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregate_effects_reading_vs_original_meaning,
    'Does the expansive aggregate-effects reading track the constitutional text''s original public meaning of ''commerce among the several states,'' or is it a doctrinal construction that has drifted substantially from that meaning under pressure of twentieth-century economic crisis and administrative convenience?',
    'Historical linguistic and legal analysis of founding-era usage of ''commerce,'' cross-checked against the doctrinal trajectory from Gibbons v. Ogden through Wickard v. Filburn and subsequent aggregate-effects cases; comparison with contemporaneous state practice under the Articles of Confederation.',
    'If the expansive reading substantially departs from original meaning, its authority rests on stare decisis and functional necessity rather than textual fidelity — strengthening the case that it operates as a constructed grant of jurisdiction benefiting federal administrative capacity rather than a natural reading of the text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_effects_reading_vs_original_meaning, conceptual, 'Whether the expansive reading is textually grounded or a doctrinal construction serving federal administrative interests.').

omega_variable(
    sibling_reading_foreclosure_structure,
    'Does adopting the expansive aggregate-effects reading as controlling doctrine logically foreclose the originalist narrow reading within the same judicial framework, or can both persist as live positions held by different factions (majority doctrine vs. dissenting opinions)?',
    'Doctrinal analysis of whether current aggregate-effects precedent (Wickard, Raich) is logically compatible with a simultaneous originalist framework, or whether the two premises about what ''commerce'' denotes are mutually exclusive at the level of controlling law.',
    'Determines whether the reading_relations edge to originalist_narrow_reading should be forecloses or coexists_with; this story treats them as coexisting live positions (majority doctrine vs. persistent dissenting jurisprudence) rather than mutually foreclosing, since the narrow reading persists as an active minority position with periodic doctrinal traction (e.g., Lopez, Morrison).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_structure, conceptual, 'Structural relationship between this reading and the originalist sibling — coexistence vs. foreclosure.').

omega_variable(
    state_autonomy_beneficiary_ambiguity,
    'Is state-level policy variation itself a genuine value being extracted from, or is ''state autonomy'' sometimes a proxy label for entrenched local interests (e.g., agricultural cartels, discriminatory labor practices) that the aggregate-effects doctrine was specifically built to override?',
    'Case-by-case historical review of which state policies were displaced by aggregate-effects doctrine (civil rights enforcement, child labor, wage floors) versus which represented genuine local democratic preference with no third-party harm.',
    'If a substantial share of displaced state autonomy was itself extractive of vulnerable in-state populations, some of the measured ''victim'' cost to state legislatures should be discounted — the doctrine''s extraction from states is partly extraction from state-level extraction, not extraction from a neutral policy good.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_autonomy_beneficiary_ambiguity, preference, 'Whether displaced state autonomy is uniformly a genuine value or partly a proxy for entrenched local extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__expansive_federal_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__expansive_federal_reading, theater_ratio, 1937, 0.1).
narrative_ontology:measurement_basis(comm_tr_t1937, observed).
narrative_ontology:measurement(comm_tr_t1955, commerce_clause_text__expansive_federal_reading, theater_ratio, 1955, 0.15).
narrative_ontology:measurement_basis(comm_tr_t1955, observed).
narrative_ontology:measurement(comm_tr_t1970, commerce_clause_text__expansive_federal_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement_basis(comm_tr_t1970, observed).
narrative_ontology:measurement(comm_tr_t1990, commerce_clause_text__expansive_federal_reading, theater_ratio, 1990, 0.24).
narrative_ontology:measurement_basis(comm_tr_t1990, observed).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_text__expansive_federal_reading, theater_ratio, 2005, 0.26).
narrative_ontology:measurement_basis(comm_tr_t2005, observed).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_text__expansive_federal_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(comm_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement_basis(comm_be_t1937, observed).
narrative_ontology:measurement(comm_be_t1955, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1955, 0.42).
narrative_ontology:measurement_basis(comm_be_t1955, observed).
narrative_ontology:measurement(comm_be_t1970, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement_basis(comm_be_t1970, observed).
narrative_ontology:measurement(comm_be_t1990, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1990, 0.53).
narrative_ontology:measurement_basis(comm_be_t1990, observed).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2005, 0.56).
narrative_ontology:measurement_basis(comm_be_t2005, observed).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement_basis(comm_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1937, 0.3).
narrative_ontology:measurement_basis(comm_su_t1937, observed).
narrative_ontology:measurement(comm_su_t1955, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1955, 0.4).
narrative_ontology:measurement_basis(comm_su_t1955, observed).
narrative_ontology:measurement(comm_su_t1970, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement_basis(comm_su_t1970, observed).
narrative_ontology:measurement(comm_su_t1990, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement_basis(comm_su_t1990, observed).
narrative_ontology:measurement(comm_su_t2005, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement_basis(comm_su_t2005, observed).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2024, 0.62).
narrative_ontology:measurement_basis(comm_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__expansive_federal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__originalist_narrow_reading).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the commerce_clause_text kernel, decomposed per the epsilon-invariance principle because the same textual clause supports structurally distinct claims with different beneficiary/victim sets and different epsilon values. expansive_federal_reading (this story, tangled_rope, eps=0.58) authorizes federal reach over any activity with substantial aggregate effects; substantial_effects_limited_reading (eps lower, requires jurisdictional nexus) is a narrower version of the same doctrinal family that this reading's continued dominance keeps under pressure (influences edge); originalist_narrow_reading (eps lowest, limited to cross-border trade and instrumentalities) is a genuinely coexisting minority position rather than one this reading forecloses, since it persists in dissenting opinions and occasional majority resurgence (Lopez, Morrison, NFIB v. Sebelius).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
