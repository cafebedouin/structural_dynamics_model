% ============================================================================
% CONSTRAINT STORY: cbdr_principle__voluntary_commitment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: CBDR Voluntary-Commitment Reading: Nationally Determined Pledges with Technology Transfer as Principal Developed-Nation Duty
 *   domain: international_climate_governance/treaty_law/development_economics
 *
 * SUMMARY:
 *   This story instantiates the voluntary-commitment reading of the CBDR
 *   principle as it operates through the Paris-era architecture: every party
 *   chooses its own contribution, differentiation is expressed as a
 *   developed-nation duty to transfer technology and mobilize finance, and
 *   the COP21 decision text expressly shields loss-and-damage provisions from
 *   liability and compensation. The expected structural delta is realized in
 *   the stakeholder surface: developed nations exit the victim set for
 *   binding emission constraints (they owe no binding schedule and no
 *   assessed compensation), while climate-exposed developing nations enter
 *   it, absorbing adaptation costs without guaranteed compensating flows. Per
 *   the epsilon-invariance principle this is a separate constraint from the
 *   sibling historical-responsibility reading, which assigns developed
 *   nations binding proportional reductions plus loss-and-damage financing;
 *   the two are linked via network.affects_constraints and carry different
 *   epsilons, victim sets, and classifications. Claim and metrics are
 *   independent authored facts: the structure is claimed as tangled_rope
 *   (genuine universal-participation coordination fused with asymmetric cost
 *   allocation requiring active maintenance), while the metrics describe the
 *   arrangement's actual operation as moderately-to-substantially extractive
 *   by this reading's own lights — the reading endorses voluntariness as
 *   necessary yet concedes the adaptation-finance shortfall is real and
 *   uncompensated.
 *
 * KEY AGENTS:
 *   - developed_nations: primary beneficiary ([institutional]/[arbitrage]) — collects avoided binding obligations and avoided compensation; co-authors the liability exclusions
 *   - small_island_states and least_developed_countries: primary payers ([organized]/[trapped], [moderate]/[trapped]) — bear uncompensated adaptation costs with no exit from atmospheric exposure
 *   - frontline_climate_vulnerable_communities: ultimate payers ([powerless]/[trapped]) — absorb losses beneath the state seat with no negotiation seat
 *   - large_emerging_economies: dual-positioned ([powerful]/[constrained]) — flexibility beneficiary and adaptation payer simultaneously
 *   - unfccc_cop_process: agenda setter ([institutional]/[identity_locked]) — administers the pledge cycle it cannot compel
 *   - cleantech_export_industries: secondary beneficiary ([organized]/[arbitrage]) — converts transfer obligation into contracted exports
 *   - future_generations: excluded voice ([powerless]/[trapped]) — inherits the aggregate outcome without representation
 *   - ipcc_assessment_community: analytical observer — external audit of pledge-versus-trajectory gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__voluntary_commitment_reading, 0.53).
domain_priors:suppression_score(cbdr_principle__voluntary_commitment_reading, 0.62).
domain_priors:theater_ratio(cbdr_principle__voluntary_commitment_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, extractiveness, 0.53).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__voluntary_commitment_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__voluntary_commitment_reading, "CBDR Voluntary-Commitment Reading: Nationally Determined Pledges with Technology Transfer as Principal Developed-Nation Duty").
narrative_ontology:topic_domain(cbdr_principle__voluntary_commitment_reading, "international_climate_governance/treaty_law/development_economics").

domain_priors:requires_active_enforcement(cbdr_principle__voluntary_commitment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__voluntary_commitment_reading, '7ccf6e8a-c73b-4e71-95ec-dbb68291e887').
narrative_ontology:cs_kernel_codification('7ccf6e8a-c73b-4e71-95ec-dbb68291e887', fixed_text).
narrative_ontology:cs_authority_grounding('7ccf6e8a-c73b-4e71-95ec-dbb68291e887', lineage).
narrative_ontology:cs_interpretation_layer_present('7ccf6e8a-c73b-4e71-95ec-dbb68291e887').
narrative_ontology:cs_reading_relation('7ccf6e8a-c73b-4e71-95ec-dbb68291e887', cbdr_principle__historical_responsibility_reading, influences).
narrative_ontology:cs_axiom('7ccf6e8a-c73b-4e71-95ec-dbb68291e887', foundational, voluntary_ndc_architecture_sufficient).
narrative_ontology:cs_axiom_status(voluntary_ndc_architecture_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('7ccf6e8a-c73b-4e71-95ec-dbb68291e887', voluntary_ndc_architecture_sufficient, instrumental).
narrative_ontology:cs_axiom('7ccf6e8a-c73b-4e71-95ec-dbb68291e887', foundational, technology_transfer_as_principal_developed_duty).
narrative_ontology:cs_axiom_status(technology_transfer_as_principal_developed_duty, holdable).
narrative_ontology:cs_axiom_grounding('7ccf6e8a-c73b-4e71-95ec-dbb68291e887', technology_transfer_as_principal_developed_duty, conventional).
narrative_ontology:cs_reference_frame('7ccf6e8a-c73b-4e71-95ec-dbb68291e887', nationally_determined_enablement_bargain).
narrative_ontology:cs_drift_state('7ccf6e8a-c73b-4e71-95ec-dbb68291e887', post_advisory_opinion_finance_goal_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7ccf6e8a-c73b-4e71-95ec-dbb68291e887', '').
narrative_ontology:cs_kernel_id(cbdr_principle__voluntary_commitment_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, developed_nations).
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, large_emerging_economies).
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, cleantech_export_industries).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, small_island_states).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, least_developed_countries).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, frontline_climate_vulnerable_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, large_emerging_economies).
narrative_ontology:constraint_vindicates(cbdr_principle__voluntary_commitment_reading, nationally_determined_sovereignty_norm).
narrative_ontology:constraint_vindicates(cbdr_principle__voluntary_commitment_reading, facilitative_multilateralism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes the annual Conference of the Parties and its Paris-era successor body; operates the pledge-submission cycle, the transparency reviews, and the five-yearly global stocktake. Administers by invitation and record-keeping: it can publicize shortfalls but cannot penalize them. Its consensus procedure doubles as the wall that keeps commitments nationally determined, since any single party can block language binding anyone. Restructuring the process would mean losing the only negotiating table where every emitter sits, and the facilitative method is constitutive of what the body understands itself to be.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, unfccc_cop_process, agenda_setter,
    institutional, generational, identity_locked, global).

% Submit self-chosen emission targets and account technology and finance flows to developing countries largely as mobilized private investment and project lending. They negotiated and maintain the express decision-language excluding liability and compensation, and can dilute or delay obligation proposals through consensus. Consequences for missing their own targets are reputational; formal withdrawal remains available after a waiting period. Fiscal relief from owing neither binding reductions nor assessed compensation accrues to their treasuries, while their technology firms gain export markets wherever transfer is funded.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, developed_nations, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__voluntary_commitment_reading, developed_nations, agenda_setter).

% Set their own contribution levels under the same flexibility as everyone else and defended the differentiation that spared them obligatory targets and mandatory finance. Their industrial buildout rides on transferred technology and concessional credit. Simultaneously, heat extremes, glacier-fed river stress, and coastal exposure impose mounting domestic adaptation spending, and diplomatic pressure to contribute to international funds grows with their emissions. Withdrawing from the regime would cost them standing in trade and diplomacy.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, large_emerging_economies, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__voluntary_commitment_reading, large_emerging_economies, payer).

% Coalition of low-lying island governments whose territory, reefs, and freshwater are directly threatened by sea-level rise and storm intensification despite negligible emissions. Their entire protective strategy runs through the multilateral process: adaptation finance, the loss-and-damage fund, and temperature-goal language are the only levers they hold. The fund receives voluntary contributions and owes them nothing guaranteed; they cannot leave the process because no other venue offers protection, and archipelago-scale relocation is not an exit.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, small_island_states, payer,
    organized, generational, trapped, regional).

% Group of low-income countries facing drought, flood, and crop-stress adaptation bills that routinely exceed domestic budgets plus received adaptation finance. Transfers arrive as donor-designed projects rather than entitlements; shortfalls are absorbed as unbuilt infrastructure and unrecovered harvests. They hold group speaking slots in negotiations but little blocking power, and leaving the process would forfeit even the discretionary flows.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, least_developed_countries, payer,
    moderate, immediate, trapped, regional).

% Coastal villages, dryland herders, and smallholder farming households who absorb floods, saltwater intrusion, and failed rains directly. They hold no seat in the negotiations; adaptation money reaches them, when it does, filtered through national plans and donor projects, and losses beyond it are simply carried. Their realistic option set is distress migration, usually internal.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, frontline_climate_vulnerable_communities, payer,
    powerless, immediate, trapped, local).

% Manufacturers and developers of renewable-energy, grid, and efficiency technology based in wealthy economies. Transfer programs, export credits, and blended finance convert the developed-country duty into paid contracts and licensed intellectual property. They favor transfer framed as market creation rather than assessed contribution, and can redirect sales to whichever jurisdictions subsidize fastest.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, cleantech_export_industries, beneficiary,
    organized, biographical, arbitrage, global).

% People who will inherit whatever temperature outcome the pledge cycle aggregates to. Present in the texts only as preambular invocation; no delegation speaks for them, and nothing in the consensus procedure weighs their interest against a sitting government's near-term costs. Their exposure compounds with each shortfall year.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, future_generations, excluded,
    powerless, civilizational, trapped, global).

% Scientific body producing periodic assessments of warming trajectories, adaptation gaps, and the distance between aggregated pledges and stated temperature goals. Its synthesis reports are the standing external audit of whether the voluntary cycle is delivering; it holds no negotiating position, and its findings enter the process only as inputs.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, ipcc_assessment_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cbdr_principle__voluntary_commitment_reading, developed_nations).
narrative_ontology:fixing_cost_class(cbdr_principle__voluntary_commitment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps every emitter inside one universal regime by letting each set its own contribution level: synchronized pledge submission, transparency review, and a five-yearly global stockturn coordinate national planning and reputational pressure around a shared temperature goal, solving the participation problem that defeated binding top-down scheduling.
% TRANSFER_FUNCTION: Moves technology and finance from developed to developing nations as discretionary, project-based flows accounted as mobilized investment; simultaneously moves the cost of climate impacts onto climate-exposed developing nations as domestic adaptation spending and unreimbursed loss, with no guaranteed compensating inflow.
% ABSENT_VOICES: Future generations have no delegation; frontline affected communities are represented only indirectly through state seats that often prioritize different interests; proponents of the historical-responsibility reading attend the negotiations but find liability and compensation language consistently blocked by consensus procedure before it reaches operative text.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would collapse the single universal negotiating table: climate diplomacy would fragment into carbon clubs, border-adjustment blocs, bilateral deals, and litigation venues; transparency reporting and stocktake cycles would cease; vulnerable nations would lose the only forum where their votes count equally; finance and technology channels would reorganize around export control and pure commercial terms.
% FOUNDING_PROBLEM: Rescuing universal climate participation after the binding top-down model failed ratification: the task was to construct an architecture every major emitter would join while nominally preserving differentiation between developed and developing nations.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: IPCC assessment reports and UNEP Emissions Gap reports attest both that universal membership was achieved and that aggregated pledges diverge widely from stated goals; AOSIS and LDC group statements corroborate that participation was purchased at the price of adequacy; the facilitator institutions themselves concede an implementation gap in global stocktake outputs while disputing that it indicts voluntariness.
narrative_ontology:disappearance_verdict(cbdr_principle__voluntary_commitment_reading, world_rearranges).
narrative_ontology:founding_problem_status(cbdr_principle__voluntary_commitment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cbdr_principle__voluntary_commitment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cbdr_principle__voluntary_commitment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cbdr_principle__voluntary_commitment_reading, 0.53, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness (0.53 at interval end) reflects the reading's own concession structure: voluntariness is regarded as legitimate, but the widening gap between delivered and owed transfer-plus-adaptation finance is acknowledged as real, so epsilon sits well above coordination-friction floors yet below the sibling reading's assessment of the same referent. Suppression (0.62) is structural, not physical coercion: consensus procedure, liability-exclusion language, voluntary-capitalization of the loss-and-damage fund, and finance-conditionality close off the binding-schedule and compensation alternatives inside the negotiating space — suppression is authored unscaled, as a raw property. Theater ratio (0.48) tracks pledge inflation: net-zero declarations and long-dated targets have multiplied faster than delivery pathways, though real functions (finance flows, transparency reviews, technology deployment) persist. Accessibility collapse is low-moderate (0.38) because alternatives remain conceivable and partially practiced (carbon clubs, ICJ advisory proceedings, domestic litigation, border adjustment instruments). Resistance (0.58) is substantial and bidirectional: vulnerable-country blocs press for liability language, litigants pursue state-obligation rulings, while parts of the beneficiary coalition resist any hardening. The measurement series share one grid ({0,2,4,6,8,10}); all points are observed, spanning adoption through the recent finance-goal disputes and the advisory-opinion pressure on state obligations. Rising suppression_requirement is the deliberate trace: maintaining voluntariness has demanded progressively thicker procedural defenses as liability pressure grew.
 *
 * PERSPECTIVAL GAP:
 *   The facilitator seat (unfccc_cop_process) experiences the arrangement as its signal achievement — universality preserved, ratchet turning, no party walking away entirely — and should compute something rope-like. The beneficiary seat experiences pragmatic realism: the only deal available, delivering technology markets alongside. The trapped payer seats experience discretionary charity substituting for owed repair: the same pledge cycle that reads as momentum from above reads as attrition from below. Large emerging economies straddle: beneficiaries of flexibility, payers of exposure. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place developed nations near the subsidy end (d near 0.0), amplified toward full beneficiary by arbitrage-grade exit: they can under-deliver, reframe, or withdraw at modest cost while collecting fiscal relief. Cleantech export industries are near-pure collectors. Large emerging economies derive as beneficiaries from their flexibility gain but carry real adaptation exposure, pulling them toward symmetric — a genuinely mixed seat. Small island states, least developed countries, and frontline communities sit near the full-target end, and trapped exit pins them there: no alternative venue, no relocation scale. The administering process derives near symmetric — it neither collects the gains nor bears the costs, and its identity_locked exit marks institutional fusion with the facilitative method rather than material stake. Future generations carry compounding exposure but hold an excluded, commentary-grade seat only. The structural derivation from beneficiaries, victims, power, and exit handles every seat; no directionality overrides were needed.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy confusion is the obvious failure mode for this constraint: a pure-rope reading (universal participation achieved — historic success) and a pure-snare reading (charity relabeled as obligation while impacts compound) are both available and both wrong. Declaring beneficiaries, victims, and active enforcement together forces the tangled_rope evaluation: the coordination function is real (the founding participation problem was genuinely solved — membership is universal where Kyoto's coalition collapsed) and the extraction is real (the most exposed parties pay without entitlement). Founding-problem status is authored contested, not dead: participation remains live and fragile, but adequacy — whether the arrangement still serves the problem it was built for, namely effective mitigation — is precisely what the stocktakes dispute. The contested-status times world_rearranges combination routes the mismatch flag rather than letting either celebratory or condemnatory labels settle. mandatrophy_resolved is deliberately not declared: the mandate has degraded in adequacy, not expired in function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (voluntary_commitment_reading) of the cbdr_principle kernel; what changes structurally if the sibling historical_responsibility_reading is adopted instead?',
    'Cross-story comparative recomputation: adopt the sibling''s obligation set (binding developed-nation reductions proportional to cumulative historical emissions, plus loss-and-damage financing) and recompute seat directionalities over the same arrangement.',
    'Developed nations re-enter the victim set for binding emission constraints; vulnerable developing nations'' uncompensated adaptation burden converts into compensated claims; the arrangement''s epsilon and classification change materially. The disagreement is located in what the differentiation clause obliges: prospective enablement versus retrospective proportional liability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one-of-two-readings status of the CBDR kernel and the sibling reading''s structural delta.').

omega_variable(
    technology_transfer_enforceability,
    'Is technology transfer an enforceable obligation or an operative aspiration — what actually happens when developed-nation delivery falls systematically short?',
    'Track dispute-settlement usage, technology framework review outcomes, and independent audits of finance and transfer accounting; observe whether any consequence attaches to persistent non-delivery.',
    'If unenforceable, the developed-nation side of the bargain is rhetorical, raising effective extraction and pushing the arrangement toward a snare-flavored profile; if consequential, part of measured extraction is ordinary coordination friction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_enforceability, empirical, 'Whether the principal developed-nation duty bites in practice.').

omega_variable(
    participation_binding_counterfactual,
    'Would a binding-schedule architecture have achieved comparable or greater participation, or would it have collapsed as the Kyoto ratification coalition did?',
    'Comparative institutional history of binding-annex ratification dynamics versus universal-pledge entry into force; evidence from climate clubs with binding membership conditions.',
    'If binding architectures reliably collapse, part of this arrangement''s suppression is the price of the only feasible coordination, shifting classification rope-ward; if binding regimes are viable, voluntariness is a choice serving the beneficiary seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(participation_binding_counterfactual, empirical, 'Counterfactual viability of binding alternatives to the voluntary architecture.').

omega_variable(
    adaptation_cost_attribution,
    'How much of the adaptation burden currently falling on climate-exposed developing nations traces to developed-nation cumulative emissions versus subsequent developing-world emission growth?',
    'Attribution-science reconciliation of cumulative-emissions responsibility shares with observed residual damage and adaptation costs.',
    'Recomposes the victim set: a larger own-share attribution lowers the target-side directionality of exposed seats and reduces measured extraction; a dominant developed-share strengthens it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_cost_attribution, empirical, 'Causal share of uncompensated adaptation costs attributable under the differentiation clause.').

omega_variable(
    differentiation_boundary_frozen_annex,
    'Which states count as developed for the transfer obligation — the frozen 1992 annex list or updated capability-and-emission classifications?',
    'Track negotiation text on differentiation reviews and the treatment accorded newly high-income, high-emission parties across successive pledge cycles.',
    'Moves seats between the obligation side and the exposure side; a widened developed set raises total transfer owed and redraws which nations sit in the victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(differentiation_boundary_frozen_annex, conceptual, 'Boundary ambiguity in the differentiation clause''s beneficiary/victim assignment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__voluntary_commitment_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_vol_read_tr_t0, cbdr_principle__voluntary_commitment_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(cbdr_vol_read_tr_t0, observed).
narrative_ontology:measurement(cbdr_vol_read_tr_t2, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2, 0.33).
narrative_ontology:measurement_basis(cbdr_vol_read_tr_t2, observed).
narrative_ontology:measurement(cbdr_vol_read_tr_t4, cbdr_principle__voluntary_commitment_reading, theater_ratio, 4, 0.36).
narrative_ontology:measurement_basis(cbdr_vol_read_tr_t4, observed).
narrative_ontology:measurement(cbdr_vol_read_tr_t6, cbdr_principle__voluntary_commitment_reading, theater_ratio, 6, 0.4).
narrative_ontology:measurement_basis(cbdr_vol_read_tr_t6, observed).
narrative_ontology:measurement(cbdr_vol_read_tr_t8, cbdr_principle__voluntary_commitment_reading, theater_ratio, 8, 0.44).
narrative_ontology:measurement_basis(cbdr_vol_read_tr_t8, observed).
narrative_ontology:measurement(cbdr_vol_read_tr_t10, cbdr_principle__voluntary_commitment_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement_basis(cbdr_vol_read_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(cbdr_vol_read_be_t0, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(cbdr_vol_read_be_t0, observed).
narrative_ontology:measurement(cbdr_vol_read_be_t2, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2, 0.44).
narrative_ontology:measurement_basis(cbdr_vol_read_be_t2, observed).
narrative_ontology:measurement(cbdr_vol_read_be_t4, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 4, 0.47).
narrative_ontology:measurement_basis(cbdr_vol_read_be_t4, observed).
narrative_ontology:measurement(cbdr_vol_read_be_t6, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement_basis(cbdr_vol_read_be_t6, observed).
narrative_ontology:measurement(cbdr_vol_read_be_t8, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(cbdr_vol_read_be_t8, observed).
narrative_ontology:measurement(cbdr_vol_read_be_t10, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement_basis(cbdr_vol_read_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_vol_read_su_t0, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(cbdr_vol_read_su_t0, observed).
narrative_ontology:measurement(cbdr_vol_read_su_t2, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2, 0.5).
narrative_ontology:measurement_basis(cbdr_vol_read_su_t2, observed).
narrative_ontology:measurement(cbdr_vol_read_su_t4, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 4, 0.53).
narrative_ontology:measurement_basis(cbdr_vol_read_su_t4, observed).
narrative_ontology:measurement(cbdr_vol_read_su_t6, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 6, 0.57).
narrative_ontology:measurement_basis(cbdr_vol_read_su_t6, observed).
narrative_ontology:measurement(cbdr_vol_read_su_t8, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement_basis(cbdr_vol_read_su_t8, observed).
narrative_ontology:measurement(cbdr_vol_read_su_t10, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(cbdr_vol_read_su_t10, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__voluntary_commitment_reading, resource_allocation).
narrative_ontology:affects_constraint(cbdr_principle__voluntary_commitment_reading, cbdr_principle__historical_responsibility_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'CBDR' per the epsilon-invariance principle. The label conflates two structurally distinct arrangements arising from one kernel text: this story (voluntary_commitment_reading — self-chosen pledges, transfer-as-enablement, liability excluded) and the sibling (historical_responsibility_reading — binding proportional reductions, loss-and-damage finance as obligation). Their epsilons differ materially over the same referent because the readings assign different obligation sets and therefore different victim sets; forcing one story to span both would require an observable-selection parameter, which the framework forbids. This upstream reading influences the sibling structurally: by becoming the operative architecture (Paris displacing Kyoto's binding annex model), it changed the sibling's operating environment — the historical-responsibility demand now manifests as contest inside a voluntary frame (loss-and-damage fund fights, advisory opinions) rather than as competing treaty text — without foreclosing it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
