% ============================================================================
% CONSTRAINT STORY: cbdr_principle__voluntary_commitment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cbdr_principle__voluntary_commitment_reading, []).

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
 *   constraint_id: cbdr_principle__voluntary_commitment_reading
 *   human_readable: CBDR Voluntary Commitment Reading (Paris NDC Architecture)
 *   domain: international_climate_governance/treaty_law/development_economics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested CBDR kernel: the
 *   voluntary commitment reading codified at Paris, under which common but
 *   differentiated responsibilities are discharged through nationally
 *   determined contributions that no body can compel, with technology
 *   transfer and capacity building as the primary developed-nation
 *   obligation. The standing arrangement under contest, and therefore the
 *   epsilon referent, is this voluntary architecture itself, assessed by the
 *   reading's own lights: it holds that state consent is constitutive of
 *   legitimate obligation and that differentiated duty is discharged through
 *   enabled transition rather than imposed targets. Per the
 *   epsilon-invariance principle, the colloquial label 'CBDR' covers two
 *   structurally distinct claims, and the sibling reading (binding reductions
 *   proportional to historical emissions plus loss-and-damage financing) is a
 *   separate constraint story with its own epsilon, its own victim set, and
 *   its own classification; the two files are linked through the network
 *   edge. The claim/metric gap here is deliberate: the constraint is CLAIMED
 *   as tangled_rope from the authoring seat, while the metrics describe
 *   moderately extractive, actively maintained, slowly hardening operation,
 *   and the engine computes each seat's type from the structural data without
 *   reconciling the claim to the metrics.
 *
 * KEY AGENTS:
 *   - industrialized_states: principal beneficiary and co-agenda-setter (institutional/arbitrage) — avoids binding schedules and liability, controls the discretionary transfer channels
 *   - major_emerging_economies: secondary beneficiary (powerful/mobile) — no binding caps, partial exposure to climate costs, outside options preserved
 *   - climate_vulnerable_developing_states: primary target (organized/trapped) — bears uncompensated adaptation costs, exit impossible
 *   - least_developed_countries: target with partial relief (powerless/trapped) — receives sub-need voluntary finance, bears the residual
 *   - unfccc_secretariat: administrator (institutional/constrained) — runs facilitative review, exists only inside the architecture it services
 *   - fossil_fuel_exporters: indirect beneficiary (powerful/arbitrage) — a regime that cannot compel phase-out protects revenue streams
 *   - youth_climate_movements: excluded voice (moderate/identity_locked) — seeks the justiciable alternative the frame forecloses
 *   - international_climate_law_scholars: analytical observer (analytical/analytical) — tracks the text-practice-court gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__voluntary_commitment_reading, 0.56).
domain_priors:suppression_score(cbdr_principle__voluntary_commitment_reading, 0.5).
domain_priors:theater_ratio(cbdr_principle__voluntary_commitment_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__voluntary_commitment_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__voluntary_commitment_reading, "CBDR Voluntary Commitment Reading (Paris NDC Architecture)").
narrative_ontology:topic_domain(cbdr_principle__voluntary_commitment_reading, "international_climate_governance/treaty_law/development_economics").

domain_priors:requires_active_enforcement(cbdr_principle__voluntary_commitment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__voluntary_commitment_reading, 'dde366eb-33e5-480d-90a4-764685ef95c1').
narrative_ontology:cs_kernel_codification('dde366eb-33e5-480d-90a4-764685ef95c1', fixed_text).
narrative_ontology:cs_authority_grounding('dde366eb-33e5-480d-90a4-764685ef95c1', lineage).
narrative_ontology:cs_interpretation_layer_present('dde366eb-33e5-480d-90a4-764685ef95c1').
narrative_ontology:cs_reading_relation('dde366eb-33e5-480d-90a4-764685ef95c1', cbdr_principle__historical_responsibility_reading, coexists_with).
narrative_ontology:cs_axiom('dde366eb-33e5-480d-90a4-764685ef95c1', foundational, state_consent_grounds_mitigation_obligations).
narrative_ontology:cs_axiom_status(state_consent_grounds_mitigation_obligations, holdable).
narrative_ontology:cs_axiom_grounding('dde366eb-33e5-480d-90a4-764685ef95c1', state_consent_grounds_mitigation_obligations, conventional).
narrative_ontology:cs_axiom('dde366eb-33e5-480d-90a4-764685ef95c1', foundational, technology_transfer_primary_developed_obligation).
narrative_ontology:cs_axiom_status(technology_transfer_primary_developed_obligation, holdable).
narrative_ontology:cs_axiom_grounding('dde366eb-33e5-480d-90a4-764685ef95c1', technology_transfer_primary_developed_obligation, instrumental).
narrative_ontology:cs_reference_frame('dde366eb-33e5-480d-90a4-764685ef95c1', sovereign_consent_pledge_review_framework).
narrative_ontology:cs_drift_state('dde366eb-33e5-480d-90a4-764685ef95c1', post_advisory_opinion_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('dde366eb-33e5-480d-90a4-764685ef95c1', '').
narrative_ontology:cs_kernel_id(cbdr_principle__voluntary_commitment_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, industrialized_states).
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, major_emerging_economies).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, climate_vulnerable_developing_states).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, least_developed_countries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, least_developed_countries).
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, fossil_fuel_exporters).
narrative_ontology:constraint_vindicates(cbdr_principle__voluntary_commitment_reading, state_consent_norm_international_law).
narrative_ontology:constraint_vindicates(cbdr_principle__voluntary_commitment_reading, pledge_and_review_methodology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Co-designed the Paris architecture and remain its principal architects. They determine their own contribution levels, deliver technology transfer and finance through discretionary channels they control, and bear no binding reduction schedule and no liability for loss and damage. Any move toward bindingness requires their consent, which they decline to give; if dissatisfied with the regime they can reshape its effective force through unilateral trade measures such as border carbon adjustments.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, industrialized_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__voluntary_commitment_reading, industrialized_states, agenda_setter).

% Large developing emitters that made differentiation the price of their participation. They accept no binding caps, submit contributions on their own terms, and preserve outside options through South-South blocs and bilateral energy and infrastructure deals. They increasingly suffer domestic climate impacts and contribute voluntary finance, which gives them partial exposure to the costs the structure otherwise externalizes.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, major_emerging_economies, beneficiary,
    powerful, generational, mobile, continental).

% Small island and low-lying coastal states organized as negotiating coalitions. They submit contributions, report transparently, and absorb escalating adaptation costs such as sea defenses, relocation, and disaster recovery largely from their own budgets. They cannot exit the climate system, and withdrawing from the treaty would forfeit their voice without reducing their exposure. Their demand for binding developed-nation reductions and guaranteed compensation is deferred at every cycle.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, climate_vulnerable_developing_states, payer,
    organized, biographical, trapped, regional).

% The poorest states receive adaptation finance and technology through voluntary funds, but inflows run far below assessed need and arrive late. They bear the residual adaptation bill themselves and depend on the regime's discretionary goodwill channels, which makes open opposition to the voluntary frame costly for them.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, least_developed_countries, payer,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__voluntary_commitment_reading, least_developed_countries, beneficiary).

% Administers the enhanced transparency framework, reviews national contribution submissions, convenes the annual conference cycle and the global stocktake, and applies facilitative rather than punitive pressure. Its mandate, budget, and continued existence depend on the voluntary architecture it services, so it has no independent lever to convert review findings into obligations.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, unfccc_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% States and industries whose revenues depend on continued fossil expansion. They benefit from a regime that cannot compel phase-out schedules, work within conferences to keep operative language voluntary and non-prescriptive, and retain the option of diversifying portfolios and alliances if diplomatic pressure intensifies.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, fossil_fuel_exporters, beneficiary,
    powerful, biographical, arbitrage, global).

% Litigants and street movements demanding binding obligations and intergenerational equity. They hold observer status at the conferences but no vote, and the alternative they seek, justiciable climate duties, is precisely what the voluntary frame excludes. Their political identity is fused with the cause, so disengagement is not a live option for them.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, youth_climate_movements, excluded,
    moderate, generational, identity_locked, global).

% Track the gap between treaty text, international court advisory opinions, and observed state practice. They document whether the voluntary frame is hardening into custom, eroding under judicial pressure, or being quietly supplemented by binding obligations arising outside the treaty.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, international_climate_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cbdr_principle__voluntary_commitment_reading, industrialized_states).
narrative_ontology:fixing_cost_class(cbdr_principle__voluntary_commitment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: After the binding top-down Kyoto model failed to achieve universality, the voluntary nationally determined structure solved the participation problem: nearly every state joined a single regime with common transparency rules, a shared stocktake cycle, and a ratchet rhythm, coordinating reporting and review across parties that would not accept negotiated targets.
% TRANSFER_FUNCTION: Moves the setting of mitigation ambition to each state itself; moves technology and capacity-building resources from developed to developing states through non-binding, discretionary channels; and leaves the costs of adapting to realized climate impacts with the vulnerable states that suffer them.
% ABSENT_VOICES: The vulnerable-state blocs object from inside the room and are heard and then set aside each cycle; future generations and the communities already absorbing loss and damage have no seat at all; the historical responsibility position is voiced annually and converted into hortatory fund language rather than obligation.
% DISAPPEARANCE_RATIONALE: If the voluntary contribution structure vanished overnight, the regime would not simply persist: states would reorganize around binding clubs of willing actors, border carbon adjustment regimes would expand to fill the enforcement vacuum, litigation and advisory-opinion obligations would move to the center, and the fragile universality that is the structure's principal product would fragment.
% FOUNDING_PROBLEM: The Kyoto-era failure: binding negotiated targets produced limited ratification, defection, and no universality, so the founding problem was designing a climate regime that the United States, China, and the major emitters would all actually join despite radically divergent responsibilities and capabilities.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: vulnerable-state negotiating statements and least-developed-country submissions attest that universality was purchased by removing bindingness; diplomatic histories of the 2014 United States-China joint announcement and negotiator memoirs from developing-country delegations document the same bargain; academic treaty-design literature treats the participation-versus-stringency tradeoff as the documented design constraint. The vulnerable blocs corroborate that the problem was real while disputing that voluntariness was the only available solution.
narrative_ontology:disappearance_verdict(cbdr_principle__voluntary_commitment_reading, world_rearranges).
narrative_ontology:founding_problem_status(cbdr_principle__voluntary_commitment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cbdr_principle__voluntary_commitment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cbdr_principle__voluntary_commitment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cbdr_principle__voluntary_commitment_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cbdr_principle__voluntary_commitment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cbdr_principle__voluntary_commitment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cbdr_principle__voluntary_commitment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.56: the arrangement delivers real coordination value (near-universal participation, common transparency) while shifting the realized costs of climate impacts onto the states least able to carry them, with the compensating obligation (technology transfer) rendered hortatory and chronically under-delivered against assessed need. Suppression is 0.50 and is authored as a raw structural property, unscaled by power or scope: the binding alternative is not physically coerced away, but consensus rules plus developed-state refusal reliably defeat it inside the regime, and vulnerable states pay a voice-related price for open opposition; exit exists (withdrawal happened) but gains a leaver nothing against the underlying physical exposure. Theater ratio is 0.52: aggregate contributions remain inconsistent with the regime's own stated temperature goal, and a growing share of conference activity is pledge restatement and communiqué production rather than burden movement. Accessibility collapse is 0.45: alternatives persist and multiply outside the frame, including border carbon adjustments, binding clubs, and litigation-driven duty theories. Resistance is 0.60: the vulnerable-state blocs, litigation movements, and now international courts actively contest the voluntary frame every cycle. The constraint requires active enforcement in the specific sense that its persistence depends on continuously expended diplomatic maintenance: consensus management, facilitative-review pressure, mobilized counter-response to withdrawal, and repeated defense of voluntariness against bindingness proposals. All three tracked metric series run on one shared time grid (2015-2025, biennial) so every metric is authored at every examined point; the trajectories show extraction and theater rising together as the adaptation gap widens faster than discretionary transfers close it, and enforcement machinery maturing gently (transparency framework operationalization, global stocktake) without gaining punitive force.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the industrialized seat the arrangement is legitimate sovereign consent plus enabled transition, and its costs are the ordinary price of universality; from the trapped vulnerable-state seat the same structure is procedural fairness laundering substantive cost-shifting, since consent extracted under existential exposure is not consent in the sense the doctrine celebrates; from the secretariat's seat it is a facilitative machine making slow measurable progress; from the excluded movements' seat it is the deliberate foreclosure of the only remedy that would bind. The engine derives these per-seat classifications from the power, exit, and role data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Industrialized states sit nearest the beneficiary end: they collect avoided mitigation costs and avoided liability, and their arbitrage-grade exit (trade measures, club formation) means the structure subsidizes them relative to any binding alternative. Major emerging economies derive low-to-moderate directionality: strong beneficiaries of no-binding-caps, partially damped by domestic climate damage and voluntary finance they now extend. Climate-vulnerable developing states sit near the full-target end, amplified by trapped exit: they cannot leave the climate system, and treaty exit buys silence rather than safety. Least developed countries sit slightly less far along because voluntary fund receipts partially offset their burden, though below assessed need. The secretariat sits near symmetric: it administers and reproduces the structure but collects no extraction rents from it. Fossil fuel exporters derive beneficiary-side directionality through the structure's inability to compel phase-out, though they appear in no beneficiary declaration because their gain is incidental to the differentiation bargain rather than constituted by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, post-Kyoto participation failure, is still live: universality remains fragile, as the withdrawal-and-return episode demonstrated, so the arrangement has not outlived its function and no mandatrophy resolution is declared. The classification prevents mislabeling in both directions: reading the structure as pure coordination (rope) would erase the documented uncompensated adaptation burden and the defeated binding alternative; reading it as pure extraction (snare) would erase the genuine participation good that a binding-only regime failed to produce and that vulnerable states themselves rely on for voice. The live risk this story flags is forward drift: if binding obligations mature outside the frame through courts and border adjustments while conference activity becomes increasingly declaratory, the theater ratio trajectory crosses into piton territory, a voluntary shell performing coordination while the real constraint migrates elsewhere. The rising theater series is the early indicator to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint instantiates the voluntary_commitment_reading of the cbdr_principle kernel; what would the sibling historical_responsibility_reading change structurally if it governed instead?',
    'Comparative authoring of the sibling story against the same referent arrangement, holding the factual record fixed and varying only the obligation structure.',
    'Under the sibling reading, developed nations enter the victim set for binding reductions and loss-and-damage liability while vulnerable states gain enforceable compensation claims; the beneficiary and victim sets substantially invert, and the classification would recompute from different structural data.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: this story is one reading of a contested kernel, and the sibling reading redistributes the victim and beneficiary sets.').

omega_variable(
    differentiation_basis_contest,
    'Is differentiation properly indexed to current capability and national circumstance, as this reading holds, or to cumulative historical emissions, as the sibling reading holds?',
    'Treaty interpretation and the conference negotiation record: which index the parties repeatedly accept in operative decisions, and which index independent equity analyses find defensible.',
    'Determines which states owe what: the capability index concentrates obligations on wealthy states regardless of emissions history; the historical index adds liability for past emissions and reverses the direction and magnitude of the transfer function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(differentiation_basis_contest, conceptual, 'The specific structural element on which the kernel''s readings disagree: the metric of differentiated responsibility.').

omega_variable(
    tech_transfer_substitution_adequacy,
    'Can delivered technology transfer and capacity building actually substitute for binding reductions and compensation in offsetting the burden this structure places on vulnerable states?',
    'Audit of technology mechanism deliveries and adaptation finance flows against independently assessed needs, using adaptation gap accounting rather than pledge accounting.',
    'If substitution is adequate, much of the measured extraction is coordination cost and the rope component dominates; if it is inadequate, the voluntary frame functions as cost-shifting and the extraction component dominates, pushing toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tech_transfer_substitution_adequacy, empirical, 'Whether the reading''s primary developed-nation obligation delivers enough to offset the uncompensated adaptation burden.').

omega_variable(
    consent_legitimacy_or_free_riding_cover,
    'Is voluntariness constitutive of the regime''s legitimacy under the consent-based norms of international law, or is it a cover story enabling free-riding by the largest emitters?',
    'Behavioral test: whether any state has faced material consequence for delivering a contribution inconsistent with its pledge, and whether aggregate ambition rises across stocktake cycles in the absence of compulsion.',
    'If consent grounds legitimacy and the ratchet works, the constraint leans rope; if defection is costless and ambition stagnates, the coordination story is cover and the constraint drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_legitimacy_or_free_riding_cover, conceptual, 'Whether the voluntary form is a legitimacy ground or an extraction enabler.').

omega_variable(
    advisory_opinion_absorption,
    'Will the binding obligations articulated in recent international court advisory opinions be absorbed by the conference interpretive layer without revising the voluntary core, or will they force codification-level revision?',
    'Watch subsequent contribution cycles and facilitative reviews for explicit citation of due-diligence and erga omnes obligations, and whether review findings begin carrying legal consequence.',
    'Absorption preserves the tangled_rope structure with the interpretation layer doing its work; forced revision migrates the constraint toward the sibling reading''s structure and recomputes the victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advisory_opinion_absorption, empirical, 'Whether judicially asserted bindingness is absorbed, deflected, or adopted by the voluntary regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__voluntary_commitment_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_vol_tr_t2015, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement_basis(cbdr_vol_tr_t2015, observed).
narrative_ontology:measurement(cbdr_vol_tr_t2017, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2017, 0.4).
narrative_ontology:measurement_basis(cbdr_vol_tr_t2017, observed).
narrative_ontology:measurement(cbdr_vol_tr_t2019, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2019, 0.43).
narrative_ontology:measurement_basis(cbdr_vol_tr_t2019, observed).
narrative_ontology:measurement(cbdr_vol_tr_t2021, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2021, 0.46).
narrative_ontology:measurement_basis(cbdr_vol_tr_t2021, observed).
narrative_ontology:measurement(cbdr_vol_tr_t2023, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2023, 0.49).
narrative_ontology:measurement_basis(cbdr_vol_tr_t2023, observed).
narrative_ontology:measurement(cbdr_vol_tr_t2025, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2025, 0.52).
narrative_ontology:measurement_basis(cbdr_vol_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(cbdr_vol_be_t2015, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement_basis(cbdr_vol_be_t2015, observed).
narrative_ontology:measurement(cbdr_vol_be_t2017, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2017, 0.44).
narrative_ontology:measurement_basis(cbdr_vol_be_t2017, observed).
narrative_ontology:measurement(cbdr_vol_be_t2019, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2019, 0.47).
narrative_ontology:measurement_basis(cbdr_vol_be_t2019, observed).
narrative_ontology:measurement(cbdr_vol_be_t2021, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2021, 0.5).
narrative_ontology:measurement_basis(cbdr_vol_be_t2021, observed).
narrative_ontology:measurement(cbdr_vol_be_t2023, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2023, 0.53).
narrative_ontology:measurement_basis(cbdr_vol_be_t2023, observed).
narrative_ontology:measurement(cbdr_vol_be_t2025, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2025, 0.56).
narrative_ontology:measurement_basis(cbdr_vol_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_vol_su_t2015, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement_basis(cbdr_vol_su_t2015, observed).
narrative_ontology:measurement(cbdr_vol_su_t2017, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2017, 0.37).
narrative_ontology:measurement_basis(cbdr_vol_su_t2017, observed).
narrative_ontology:measurement(cbdr_vol_su_t2019, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2019, 0.4).
narrative_ontology:measurement_basis(cbdr_vol_su_t2019, observed).
narrative_ontology:measurement(cbdr_vol_su_t2021, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2021, 0.44).
narrative_ontology:measurement_basis(cbdr_vol_su_t2021, observed).
narrative_ontology:measurement(cbdr_vol_su_t2023, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2023, 0.47).
narrative_ontology:measurement_basis(cbdr_vol_su_t2023, observed).
narrative_ontology:measurement(cbdr_vol_su_t2025, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2025, 0.5).
narrative_ontology:measurement_basis(cbdr_vol_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__voluntary_commitment_reading, resource_allocation).
narrative_ontology:affects_constraint(cbdr_principle__voluntary_commitment_reading, cbdr_principle__historical_responsibility_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'CBDR' conflates two structurally distinct claims about the same treaty kernel. This story authors the voluntary-commitment claim (obligation form: nationally determined and non-compellable; differentiation index: current capability; developed-nation duty: technology transfer). The sibling story authors the historical-responsibility claim (obligation form: binding; differentiation index: cumulative historical emissions; developed-nation duty: proportional reductions plus loss-and-damage financing). The two claims carry materially different epsilon values, different victim sets, and different failure modes, so they are modeled as two linked stories rather than one story with a measurement parameter. The edges run both ways analytically: the voluntary reading's codification at Paris changed the sibling's operating environment by rerouting its demands into discretionary funds, while the sibling supplies the moral warrant that the voluntary reading must continuously deflect at every conference cycle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
