% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__adaptation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__adaptation_priority_reading, []).

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
 *   constraint_id: climate_response_imperative__adaptation_priority_reading
 *   human_readable: Adaptation-First Climate Response Arrangement (Adaptation Priority Reading)
 *   domain: climate policy/political economy/intergenerational justice
 *
 * SUMMARY:
 *   The standing global climate response arrangement operates
 *   adaptation-forward: national adaptation plans, early-warning systems,
 *   resilient-infrastructure investment, and a dedicated adaptation-finance
 *   architecture are the operative machinery, while emissions reduction
 *   proceeds through voluntary pledges that are perpetually renewed and
 *   perpetually unmet. This story instantiates the
 *   adaptation_priority_reading of the climate_response_imperative kernel —
 *   the claim that present-day protection of exposed populations is the
 *   operative climate response and mitigation a long-run aspiration. The ε
 *   referent is the standing arrangement itself, assessed by this reading's
 *   own lights: even granting the reading's premise that present protection
 *   deserves priority, the arrangement as it operates nominally prioritizes
 *   adaptation while shifting its costs onto the populations it names —
 *   exposed nations borrow to protect themselves against damages driven
 *   overwhelmingly by others' cumulative emissions, promised finance arrives
 *   at a small fraction of need and mostly as loans, and 'adaptation first'
 *   supplies the justification under which binding mitigation recedes. The
 *   reading's endorsed alternative — an adaptation-first bargain actually
 *   honored, with those responsible financing the protection of the exposed —
 *   is not the referent. KEY AGENTS (by structural relationship):
 *   developed_nation_governments: agenda-setter and beneficiary
 *   (institutional/arbitrage) — controls the finance architecture, collects
 *   mitigation deferral; fossil_fuel_incumbents: primary beneficiary
 *   (institutional/arbitrage) — deferral preserves extraction markets;
 *   adaptation_finance_intermediaries: secondary beneficiary
 *   (institutional/mobile) — collects on adaptation flows;
 *   industrializing_emitter_nations: dual beneficiary/payer
 *   (powerful/constrained); exposed_developing_nations: primary payer
 *   (moderate/trapped); climate_vulnerable_populations: primary payer
 *   (powerless/trapped); future_generations: payer and structurally excluded
 *   (powerless/trapped); loss_and_damage_claimants: excluded claimant bloc
 *   (organized/trapped); climate_science_community: analytical observer
 *   (institutional/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, 0.72).
domain_priors:suppression_score(climate_response_imperative__adaptation_priority_reading, 0.6).
domain_priors:theater_ratio(climate_response_imperative__adaptation_priority_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__adaptation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__adaptation_priority_reading, "Adaptation-First Climate Response Arrangement (Adaptation Priority Reading)").
narrative_ontology:topic_domain(climate_response_imperative__adaptation_priority_reading, "climate policy/political economy/intergenerational justice").

domain_priors:requires_active_enforcement(climate_response_imperative__adaptation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__adaptation_priority_reading, 'fb8b628d-c241-4a79-b6df-f607067313af').
narrative_ontology:cs_kernel_codification('fb8b628d-c241-4a79-b6df-f607067313af', formalized).
narrative_ontology:cs_authority_grounding('fb8b628d-c241-4a79-b6df-f607067313af', distributed).
narrative_ontology:cs_reading_relation('fb8b628d-c241-4a79-b6df-f607067313af', climate_response_imperative__mitigation_priority_reading, influences).
narrative_ontology:cs_reading_relation('fb8b628d-c241-4a79-b6df-f607067313af', climate_response_imperative__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('fb8b628d-c241-4a79-b6df-f607067313af', foundational, present_harms_claim_operative_priority).
narrative_ontology:cs_axiom_status(present_harms_claim_operative_priority, holdable).
narrative_ontology:cs_axiom_grounding('fb8b628d-c241-4a79-b6df-f607067313af', present_harms_claim_operative_priority, deontological).
narrative_ontology:cs_axiom('fb8b628d-c241-4a79-b6df-f607067313af', secondary, mitigation_deferred_not_abandoned).
narrative_ontology:cs_axiom_status(mitigation_deferred_not_abandoned, holdable).
narrative_ontology:cs_axiom_grounding('fb8b628d-c241-4a79-b6df-f607067313af', mitigation_deferred_not_abandoned, instrumental).
narrative_ontology:cs_reference_frame('fb8b628d-c241-4a79-b6df-f607067313af', cbdr_adaptation_first_bargain).
narrative_ontology:cs_drift_state('fb8b628d-c241-4a79-b6df-f607067313af', contemporary_finance_gap_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fb8b628d-c241-4a79-b6df-f607067313af', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__adaptation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, developed_nation_governments).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, adaptation_finance_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, industrializing_emitter_nations).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, exposed_developing_nations).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, future_generations).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, loss_and_damage_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, industrializing_emitter_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the negotiation agenda and the finance architecture through COP consensus management and control of multilateral climate fund boards and disbursement conditionality. Deliver adaptation finance well below pledged scale, count loans and re-labeled aid toward commitments, and resist liability language. Their benefit is deferral of binding mitigation, which preserves carbon-intensive production and consumption at home; their cost is modest adaptation transfers. Exit is cheap: participation can be suspended and resumed at low political price.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, developed_nation_governments, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__adaptation_priority_reading, developed_nation_governments, beneficiary).

% Producers and financiers of coal, oil, and gas whose markets persist because mitigation remains voluntary and indefinitely deferred. They fund lobbying and narrative work that frames present-day adaptation as the responsible climate focus. Every year of deferral is continued revenue; capital is mobile across jurisdictions and portfolios, so no single regulatory turn traps them.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, fossil_fuel_incumbents, beneficiary,
    institutional, biographical, arbitrage, global).

% Multilateral development banks, climate fund implementing entities, and consultancies that channel and implement adaptation projects. They collect fees, margins, and administrative overhead on adaptation flows regardless of whether delivery meets need, and their institutional growth depends on the adaptation-finance architecture continuing in its current form.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, adaptation_finance_intermediaries, beneficiary,
    institutional, biographical, mobile, global).

% Large rapidly industrializing economies that preserve development space under voluntary mitigation while simultaneously facing severe exposure to heat, water stress, and coastal risk, with adaptation costs they increasingly self-finance. They defend the voluntarism that benefits them while demanding finance as payers. Leaving the arrangement would cost them finance access and diplomatic standing.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, industrializing_emitter_nations, beneficiary,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__adaptation_priority_reading, industrializing_emitter_nations, payer).

% Must protect their populations against damages driven overwhelmingly by others' cumulative emissions. Adaptation needs run to tens of billions annually against fiscal capacity a fraction of that; they borrow to build resilience, service the resulting debt, and rebuild after losses. The negotiation forum is their main channel for finance, and exiting it would forfeit even the inadequate flows they currently receive.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, exposed_developing_nations, payer,
    moderate, biographical, trapped, continental).

% Frontline communities in low-lying coastal zones, arid interiors, and flood plains who lose homes, harvests, and lives to intensifying hazards. They cannot exit exposure: migration is partial, costly, and often blocked. They are represented in negotiations only through states that may not prioritize them.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, climate_vulnerable_populations, payer,
    powerless, immediate, trapped, regional).

% People not yet born who will inhabit the warming locked in by deferred mitigation. They bear the compounding costs of every year the arrangement defers emissions cuts. They have no seat in any forum and no exit from the atmosphere they inherit.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, future_generations, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__adaptation_priority_reading, future_generations, excluded).

% Nations and coalitions, including small-island and climate-vulnerable blocs and the litigation movements behind the ITLOS and ICJ advisory proceedings, seeking compensation for damages already incurred. The arrangement's texts expressly exclude liability and compensation bases, and the loss-and-damage fund agreed in 2022 remains minimally capitalized. They attend the negotiations but cannot place liability on the agenda against consensus rules.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, loss_and_damage_claimants, excluded,
    organized, biographical, trapped, continental).

% IPCC and allied researchers who assess the gap between the arrangement's pledges and the emissions trajectory, document the adaptation finance gap, and track the shrinking carbon budget. They hold no agenda-setting power and can only publish the divergence.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, climate_science_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__adaptation_priority_reading, fossil_fuel_incumbents).
narrative_ontology:fixing_cost_class(climate_response_imperative__adaptation_priority_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates protective investment against locked-in warming: national adaptation plans, early-warning systems, resilient infrastructure standards, drought and flood defenses, and a multilateral channel that moves adaptation finance from developed to exposed regions — solving, imperfectly, the problem of how exposed populations protect themselves when hazards can no longer be prevented at the source.
% TRANSFER_FUNCTION: Moves adaptation burdens and residual damages onto exposed developing nations and vulnerable populations — they self-finance resilience, largely through debt, and absorb losses — while moving mitigation obligations into voluntary, indefinitely deferred form, preserving emission-intensive production and its revenues for incumbents and developed economies. A smaller counter-flow of adaptation finance, partly loan-based, moves from developed treasuries through intermediary institutions to adaptation projects.
% ABSENT_VOICES: Future generations have no seat in any forum. Loss-and-damage claimants attend but cannot place liability on the agenda against consensus rules. Frontline communities are represented only through states that may not prioritize them. Adaptation-finance recipients sit on the receiving side of tables whose terms — loan weighting, conditionality, accounting rules — are set by the funders.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the adaptation finance channel, the national adaptation plan process, and the negotiation forum would all collapse: exposed nations would lose even the inadequate flows they currently receive, adaptation projects in the pipeline would halt, and mitigation would lose its voluntary coordination shell entirely. The climate response architecture would have to be rebuilt from zero while hazards intensify.
% FOUNDING_PROBLEM: Built at Rio in 1992 to solve the problem of coordinating a global response to a shared-atmosphere threat across radical development asymmetry: how to divide mitigation effort and finance protection when the emitters of record and the exposed populations are largely different people. The founding bargain was common but differentiated responsibilities, with finance flowing from those who industrialized first to those most exposed.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's liveness is attested from outside the beneficiary set by the IPCC assessment series (documenting the widening response gap), the UNEP Adaptation Gap Reports (documenting the finance shortfall against need), G77+China coordination statements, Climate Vulnerable Forum declarations, and the advisory proceedings small-island states initiated before ITLOS and the ICJ. The agenda-setting parties attest the problem is being addressed; no party disputes that the underlying coordination problem remains unsolved.
narrative_ontology:disappearance_verdict(climate_response_imperative__adaptation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__adaptation_priority_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__adaptation_priority_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_imperative__adaptation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__adaptation_priority_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__adaptation_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__adaptation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72 at interval end) because the arrangement's operative effect is a burden transfer: exposed nations self-finance resilience against damages driven overwhelmingly by others' cumulative emissions, promised finance arrives at a small fraction of need and mostly as loans, and mitigation deferral simultaneously preserves incumbent revenue and developed-nation fiscal space. Suppression (0.60) is structural, not internalized: binding commitments, liability, and non-debt finance are not violently suppressed but foreclosed through consensus rules, the liability carve-out in the Paris text, loan-weighted accounting, and disbursement conditionalities. Theater (0.45) is moderate and rising: real adaptation assets are built, but a growing share of activity is pledge cycles, 'mobilized finance' accounting that counts loans and re-labeled aid, and communiqué language. Accessibility collapse (0.50): exits exist — unilateral adaptation, climate litigation, bilateral finance — but the multilateral channel absorbs and deflects them. Resistance (0.60): G77 bloc discipline, the V20, the litigation wave, and the ITLOS/ICJ advisory proceedings. The measurement series share one time grid (t=0,5,10,15,20,25,30); each point is an inter-cycle mean across COP pledge cycles — the underlying dynamic oscillates (pledge, shortfall, crisis, renewed pledge) but the cross-cycle trend is monotonic accumulation of extraction and theater, with the enforcement requirement rising as litigation and bloc resistance force more active defense of the liability carve-out and the loan-weighted finance structure.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from the same structure. From developed_nation_governments' position the arrangement is a functioning consensus framework that delivers real adaptation while respecting sovereign difference; from exposed_developing_nations' and climate_vulnerable_populations' position the same structure prices their protection onto their own balance sheets while the mitigation that would shrink their exposure stays voluntary. Fossil_fuel_incumbents experience the arrangement as a stable operating environment; loss_and_damage_claimants experience it as a closed door with a queue in front of it. The engine computes per-seat types from power and exit; this divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low d: developed_nation_governments (collect deferral, control the rules), fossil_fuel_incumbents (pure collectors — every deferred year is continued revenue), adaptation_finance_intermediaries (fee-collectors bearing no climate burden). Victim declarations map to high d: exposed_developing_nations and climate_vulnerable_populations (bear costs with no exit from exposure), future_generations (maximal target — no seat, no exit), loss_and_damage_claimants (denied compensation while bearing damages). One override: industrializing_emitter_nations appear in the beneficiary set (voluntary mitigation preserves their development space), so the structural derivation would place them near the beneficiary end (~0.2); their true position is near-symmetric (~0.45) because they simultaneously bear substantial adaptation costs and damages as payers. The override corrects the derivation for this dual position and applies only to the 'powerful' seat they alone hold in this story. Scope amplification is modest here: the arrangement's scope is global, which the engine reflects in effective extraction, but the decisive asymmetry is the directionality spread between trapped victims and arbitrage-grade beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling in both directions. A pure-rope reading — the arrangement's own framing, 'we coordinate protection for the vulnerable' — would erase the burden shift: the finance gap, the loan weighting, and the liability carve-out are not coordination overhead but asymmetric extraction operating through the same structure that coordinates. A pure-snare reading would erase the genuine protective function: early-warning systems, resilient infrastructure, and adaptation planning demonstrably save lives and assets, and the exposed nations themselves demand more of this coordination, not less. The founding problem (coordinating response across development asymmetry) is live, so there is no mandatrophy resolution: the arrangement has not outlived its function — it has drifted from its founding bargain (adaptation financed by those responsible) while retaining its nominal mandate, which is why the drift registers as practice_drift in the commitment structure rather than as obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the adaptation_priority_reading of the climate_response_imperative kernel; the sibling readings (mitigation_priority_reading, degrowth_reading) instantiate different constraints from the same kernel. Which reading governs is politically unresolved — what would change structurally if a sibling governed instead?',
    'Comparative read of the three sibling stories'' structural deltas: each sibling''s victim and beneficiary sets and ε are fixed within its own reading; the contest itself resolves in negotiation and domestic politics, not in this corpus.',
    'Under mitigation_priority_reading, developed_nation_governments and fossil_fuel_incumbents enter the victim set via binding mitigation costs and future_generations moves toward benefit; under degrowth_reading, Global North consumption becomes the target and the deferral beneficiaries dissolve. This reading''s victim set and its tangled_rope classification hold only within the adaptation-priority instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel contest: which reading of the climate_response_imperative governs, and what each sibling reading would structurally change.').

omega_variable(
    adaptation_delivery_adequacy,
    'Is the adaptation component of the standing arrangement delivered at protective scale, or is even this reading''s endorsed priority under-delivered — adaptation in name, loans and re-labeled aid in substance?',
    'UNEP Adaptation Gap Report series; grant-versus-loan composition audits of adaptation finance; delivery-versus-pledge tracking against the Glasgow adaptation-finance doubling commitment.',
    'If delivery is far below even the reading''s own standard, the coordination function is thinner than claimed and the arrangement leans toward snare (adaptation as cover for deferral); if delivery is substantial, the tangled_rope reading with a genuine coordination core is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_delivery_adequacy, empirical, 'Whether the adaptation component is actually delivered at protective scale or functions as under-capitalized cover.').

omega_variable(
    debt_adaptation_vicious_circle,
    'Does adaptation self-financing lock exposed nations into a compounding cycle — borrow to build resilience, service debt, lose fiscal space, deepen exposure — such that each interval raises the burden on those least responsible?',
    'Longitudinal debt-service-to-adaptation-spending ratios for climate-vulnerable states, cross-referenced with hazard-exposure trends and IMF and World Bank debt-sustainability analyses.',
    'Confirms the expected structural delta (least responsible bear highest costs, compounding) and hardens the victim classification of exposed_developing_nations; a non-compounding burden would leave the extraction severe but static.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_adaptation_vicious_circle, empirical, 'Whether the arrangement perpetuates a compounding debt-exposure cycle in exposed nations.').

omega_variable(
    deferral_reversibility,
    'Is the mitigation deferral this arrangement operates reversible — can later mitigation compensate for present deferral — or does deferral lock in warming that later mitigation cannot undo?',
    'Remaining-carbon-budget accounting and tipping-element science; comparison of warming trajectories under immediate versus delayed emissions peaking.',
    'If deferral is irreversible, the reading''s own premise (protection now, mitigation later) is self-undermining: the arrangement it endorses destroys the conditions for the mitigation it defers, and the reading''s endorsement of the arrangement becomes incoherent within its own lights — the deepest internal tension of this instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deferral_reversibility, empirical, 'Whether mitigation deferral under this reading is reversible or locks in irreversible warming.').

omega_variable(
    authority_framing_underdetermination,
    'Is the arrangement''s authority grounded in distributed COP consensus (as declared here) or in the finance-conditionality structure through which developed-nation treasuries and multilateral bank boards actually control adaptation resources?',
    'Trace actual decision points: fund-board voting shares, conditionalities attached to adaptation disbursements, and which parties can block agenda items against consensus rules.',
    'Under the extraction-grounding framing, the commitment-system classification changes (authority grounded in extraction, with negotiation ritual as interpretive layer) and the arrangement reads as a captured rather than distributed structure; the declared distributed framing takes the consensus rules at face value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_framing_underdetermination, conceptual, 'Whether the arrangement''s authority is genuinely distributed consensus or finance-conditionality capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__adaptation_priority_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__adaptation_priority_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_response_imperative__adaptation_priority_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_response_imperative__adaptation_priority_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_response_imperative__adaptation_priority_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_response_imperative__adaptation_priority_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t25, climate_response_imperative__adaptation_priority_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(clim_tr_t25, observed).
narrative_ontology:measurement(clim_tr_t30, climate_response_imperative__adaptation_priority_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement_basis(clim_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(clim_be_t20, observed).
narrative_ontology:measurement(clim_be_t25, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 25, 0.69).
narrative_ontology:measurement_basis(clim_be_t25, observed).
narrative_ontology:measurement(clim_be_t30, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement_basis(clim_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t25, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(clim_su_t25, observed).
narrative_ontology:measurement(clim_su_t30, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement_basis(clim_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__adaptation_priority_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, degrowth_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'climate response' conflates structurally distinct claims about temporal weighting and economic structure; per the ε-invariance principle it decomposes into three readings of one kernel, each a separate constraint with its own ε and victim set. This story is the adaptation-priority member. Unlike the BGS family the three readings are rivals rather than an upstream-downstream chain, but they are linked because the operative arrangement institutionalized under this reading changes the resource environment and legitimacy conditions of the siblings, and each reading's advocates cite the arrangement's operation as evidence within the same negotiation arena.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_imperative__adaptation_priority_reading, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
