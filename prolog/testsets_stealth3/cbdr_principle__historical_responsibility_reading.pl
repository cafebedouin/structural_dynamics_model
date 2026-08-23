% ============================================================================
% CONSTRAINT STORY: cbdr_principle__historical_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: cbdr_principle__historical_responsibility_reading
 *   human_readable: CBDR Historical-Responsibility Reading: Binding Proportional Mitigation Plus Loss/Damage Finance
 *   domain: international_climate_governance/treaty_law/development_economics
 *
 * SUMMARY:
 *   Under the historical-responsibility reading of common but differentiated
 *   responsibilities, developed nations owe binding, quantified emissions
 *   reductions calibrated to their cumulative historical share of atmospheric
 *   appropriation, plus compensatory loss/damage and adaptation finance owed
 *   to exposed developing nations. The arrangement solves a real coordination
 *   problem — differentiated burden-sharing is what made universal climate
 *   participation achievable — while channeling asymmetric costs and
 *   transfers through the same structure: industrialized states bear binding
 *   constraints their competitors escape, and the largest current emitters
 *   remain outside binding targets entirely. This story is one reading of the
 *   cbdr_principle kernel; the sibling voluntary_commitment_reading is
 *   authored as a separate constraint with its own epsilon, victim set, and
 *   classification, per the epsilon-invariance decomposition rule. The two
 *   epsilons differ because the arrangements differ: this reading's
 *   binding-liability structure imposes costs and transfer duties the
 *   voluntary arrangement does not. KEY AGENTS (by structural relationship):
 *   - industrialized_annex_i_nations: Primary target
 *   (institutional/constrained) — bears binding mitigation constraints and
 *   finance obligations - developed_nation_taxpayers_and_industry: Ultimate
 *   cost bearer (organized/constrained) — absorbs compliance costs passed
 *   down from state commitments - climate_vulnerable_developing_nations:
 *   Primary beneficiary (organized/trapped) — receives loss/damage and
 *   adaptation finance under existential exposure - large_emerging_economies:
 *   Secondary beneficiary (powerful/constrained) — exempt from binding
 *   targets while ranking among largest current emitters -
 *   g77_china_negotiating_bloc: Agenda setter and beneficiary
 *   (organized/trapped) — drives the differentiation and finance agenda;
 *   conduits member gains - petrostate_developing_exporters: Opportune
 *   beneficiary (powerful/arbitrage) — exploits developing-country exemptions
 *   while exporting hydrocarbons - unfccc_secretariat: Process administrator
 *   (institutional/identity_locked) — runs the COP and MRV machinery the
 *   obligations ride on - future_generations_all_nations: Excluded voice
 *   (powerless/trapped) — inherits the outcome of whichever reading prevails
 *   - climate_governance_analysts: Analytical observer
 *   (analytical/analytical) — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, 0.53).
domain_priors:suppression_score(cbdr_principle__historical_responsibility_reading, 0.4).
domain_priors:theater_ratio(cbdr_principle__historical_responsibility_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, extractiveness, 0.53).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__historical_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__historical_responsibility_reading, "CBDR Historical-Responsibility Reading: Binding Proportional Mitigation Plus Loss/Damage Finance").
narrative_ontology:topic_domain(cbdr_principle__historical_responsibility_reading, "international_climate_governance/treaty_law/development_economics").

domain_priors:requires_active_enforcement(cbdr_principle__historical_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__historical_responsibility_reading, '525fc09b-23bb-444e-9e87-f313a0eb8eb5').
narrative_ontology:cs_kernel_codification('525fc09b-23bb-444e-9e87-f313a0eb8eb5', formalized).
narrative_ontology:cs_authority_grounding('525fc09b-23bb-444e-9e87-f313a0eb8eb5', lineage).
narrative_ontology:cs_interpretation_layer_present('525fc09b-23bb-444e-9e87-f313a0eb8eb5').
narrative_ontology:cs_reading_relation('525fc09b-23bb-444e-9e87-f313a0eb8eb5', cbdr_principle__voluntary_commitment_reading, forecloses).
narrative_ontology:cs_axiom('525fc09b-23bb-444e-9e87-f313a0eb8eb5', foundational, cumulative_emissions_proportional_liability).
narrative_ontology:cs_axiom_status(cumulative_emissions_proportional_liability, holdable).
narrative_ontology:cs_axiom_grounding('525fc09b-23bb-444e-9e87-f313a0eb8eb5', cumulative_emissions_proportional_liability, deontological).
narrative_ontology:cs_axiom('525fc09b-23bb-444e-9e87-f313a0eb8eb5', foundational, binding_compliance_modality_required).
narrative_ontology:cs_axiom_status(binding_compliance_modality_required, holdable).
narrative_ontology:cs_axiom_grounding('525fc09b-23bb-444e-9e87-f313a0eb8eb5', binding_compliance_modality_required, instrumental).
narrative_ontology:cs_axiom('525fc09b-23bb-444e-9e87-f313a0eb8eb5', secondary, development_rights_constrain_southern_obligations).
narrative_ontology:cs_axiom_status(development_rights_constrain_southern_obligations, holdable).
narrative_ontology:cs_axiom_grounding('525fc09b-23bb-444e-9e87-f313a0eb8eb5', development_rights_constrain_southern_obligations, deontological).
narrative_ontology:cs_reference_frame('525fc09b-23bb-444e-9e87-f313a0eb8eb5', annex_one_binding_liability_framework).
narrative_ontology:cs_drift_state('525fc09b-23bb-444e-9e87-f313a0eb8eb5', post_paris_operative_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('525fc09b-23bb-444e-9e87-f313a0eb8eb5', '').
narrative_ontology:cs_kernel_id(cbdr_principle__historical_responsibility_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, climate_vulnerable_developing_nations).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, large_emerging_economies).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, industrialized_annex_i_nations).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developed_nation_taxpayers_and_industry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, g77_china_negotiating_bloc).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, petrostate_developing_exporters).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, large_emerging_economies).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, polluter_pays_doctrine).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, atmospheric_commons_appropriation_liability).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, common_but_differentiated_responsibilities_text).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Small island states, least-developed countries, and drought- and flood-exposed nations coordinate through AOSIS and the G77 to secure finance for losses and adaptation they did not cause. They are the designated recipients of loss/damage and adaptation fund flows, though disbursements lag pledges by wide margins. They cannot exit the climate system, their territories face existential exposure, and their fiscal capacity to adapt unaided is minimal, so continued access to the finance channel functions as a survival input.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, climate_vulnerable_developing_nations, beneficiary,
    organized, generational, trapped, global).

% Major economies classified as developing in 1992 — China, India, Brazil among them — are exempt from binding quantitative targets under this reading while now ranking among the largest annual emitters. They gain regulatory room to grow and access to technology-transfer and finance channels, while absorbing severe domestic climate impacts and facing intensifying diplomatic pressure to accept targets. Reclassifying themselves as developed would forfeit exemptions and finance eligibility; remaining classified draws mounting scrutiny of the fit between the 1992 map and current emissions.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, large_emerging_economies, beneficiary,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__historical_responsibility_reading, large_emerging_economies, payer).

% OECD and industrialized former-Soviet states carry binding quantitative mitigation obligations calibrated, under this reading, to cumulative historical emissions, plus loss/damage and adaptation finance duties. They negotiate as a loose bloc with internally divergent positions, face recurring domestic political backlash over transfer costs, and can withdraw from individual instruments at reputational and diplomatic cost — as the United States did from Kyoto — but cannot exit the negotiation architecture itself without ceding agenda control over the terms that would replace it.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, industrialized_annex_i_nations, payer,
    institutional, generational, constrained, continental).

% Households and firms in industrialized democracies ultimately absorb the costs their governments commit to: energy-price effects of binding targets, fiscal outlays for international finance, and competitiveness exposure from obligations their competitors do not carry. They hold no treaty seat and are represented only indirectly through national negotiating positions; their principal lever is domestic electoral pressure on the governments that sign.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developed_nation_taxpayers_and_industry, payer,
    organized, biographical, constrained, national).

% The 130-plus-member coalition of developing countries acts as the collective negotiating agent for differentiation and finance. It sets the agenda on historical-responsibility language, loss/damage facility design, and finance quantum, and enforces internal cohesion around the position that finance obligations precede any widening of mitigation duties. Its membership spans the entire development spectrum, so the bloc both drives the obligation structure and channels its gains, held together by the shared insistence that the finance channel stay open.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, g77_china_negotiating_bloc, agenda_setter,
    organized, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__historical_responsibility_reading, g77_china_negotiating_bloc, beneficiary).

% The Bonn-based secretariat organizes the annual COP cycle, maintains the reporting and transparency machinery, services the compliance committees and fund-governance bodies, and publishes the accounting through which obligations are tracked. Its mandate, staffing, and institutional existence are wholly bound up in administering this process; it has no organizational life apart from the convention architecture it services.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, unfccc_secretariat, agenda_setter,
    institutional, generational, identity_locked, global).

% Hydrocarbon-exporting states classified as developing invoke the differentiation structure to avoid mitigation obligations on their own exports while coordinating obstruction of proposals to widen obligations. They command sovereign wealth and market power that give them abundant outside options, and they capture the exemption benefit without bearing comparable climate vulnerability — the sharpest internal contrast inside the beneficiary coalition.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, petrostate_developing_exporters, beneficiary,
    powerful, immediate, arbitrage, regional).

% People not yet born in every country will inherit whatever atmospheric trajectory the prevailing arrangement produces and whatever fiscal liabilities its finance promises accumulate. They hold no seat in any negotiation, cannot veto any provision, and bear the compounded consequences of both inadequate mitigation and unpaid adaptation.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, future_generations_all_nations, excluded,
    powerless, civilizational, trapped, universal).

% Academic researchers, treaty-accountability organizations, and independent finance trackers observe the full structure: pledge-versus-delivery gaps, fund capitalization against assessed need, and the divergence between negotiated texts and operative practice. They publish the accounting that other seats cite and hold no stake in the obligation structure itself.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, climate_governance_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cbdr_principle__historical_responsibility_reading, climate_vulnerable_developing_nations).
narrative_ontology:fixing_cost_class(cbdr_principle__historical_responsibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the atmospheric-commons collective-action problem by allocating differentiated burdens: states with the largest cumulative contribution to accumulated greenhouse gases and the greatest fiscal capacity carry larger mitigation loads and finance adaptation elsewhere, which made universal participation achievable where uniform obligations repeatedly failed.
% TRANSFER_FUNCTION: Moves mitigation effort-share and direct finance (loss/damage compensation and adaptation funding) from industrialized nations to developing nations, calibrated under this reading by cumulative historical emissions rather than current output.
% ABSENT_VOICES: Future generations of all nations hold no seat and cannot object to provisions adopted in their name. Taxpayers and industry in developed democracies are mediated through state positions they do not directly control. Subnational governments and private firms bearing compliance costs have no standing in the treaty process. Their absence matters because unanimity among seated states is achieved partly by excluding the seats that would contest cost incidence.
% DISAPPEARANCE_RATIONALE: If the binding differentiated-obligation structure vanished overnight, designated finance flows to vulnerable nations would collapse, the G77's negotiating cohesion would dissolve into bilateral aid politics, industrialized states would reallocate mitigation effort on efficiency rather than liability grounds, and the entire COP architecture built to service these obligations would lose its organizing subject.
% FOUNDING_PROBLEM: The 1992 deadlock between development rights and atmospheric limits: uniform obligations would have been rejected by the Global South as freezing developmental inequality, while action confined to willing states would have been environmentally inadequate. CBDR reconciled breadth of participation with differentiation of burden.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment reports (outside the beneficiary set) attest the underlying problem — mobilizing adequate global mitigation while accommodating development needs — remains unsolved. OECD and independent climate-finance accounting corroborates the persistent adaptation-finance gap. Vulnerable-nation testimony corroborates it from the recipient side. Developed-nation negotiators dispute whether the 1992 differentiation map still fits current emissions distributions; they dispute the map, not the existence of the founding problem.
narrative_ontology:disappearance_verdict(cbdr_principle__historical_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(cbdr_principle__historical_responsibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cbdr_principle__historical_responsibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cbdr_principle__historical_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cbdr_principle__historical_responsibility_reading, 0.53, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.53: the binding-liability structure imposes mitigation costs and transfer duties on developed nations beyond what an efficiency-only allocation would charge them, but the cumulative-emissions calibration retains a substantive justice warrant, so extraction is substantial rather than total. Suppression is 0.40: the binding modality plus normative lock-in coerce participation, but international enforcement is soft, withdrawal remains available at diplomatic cost, and the negotiation architecture tolerates open dissent — coercion is real but capped. Accessibility_collapse is 0.40 because developed-nation alternatives (minilateral clubs, border carbon adjustment, instrument withdrawal, bloc defection) remain partly open. Resistance is 0.65, reflecting three decades of sustained refusal by key developed states to accept liability-calibrated binding obligations. Theater_ratio is 0.62 and rising monotonically across the interval: after 2015 the reading is increasingly maintained rhetorically — COP declarations, fund announcements, liability language — while operative practice runs on the sibling reading's voluntary machinery. The temporal arc: enforcement ratcheted upward with the Kyoto compliance machinery (1997–2005), decayed after Paris dissolved the binding modality, with a partial recovery when the Loss and Damage Fund agreement (2022) created a new transfer obligation; theater climbed throughout as the gap between declaratory maintenance and operative practice widened. All three tracked series run on one shared ten-point grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute fundamentally different types from identical structural data. From the Annex-I government seat, the arrangement is a treaty regime it helped draft and can formally exit — constrained extraction with institutional power. From the taxpayer-and-industry seat beneath it, the same obligations arrive as costs passed down without a treaty seat of their own — extraction with weaker representation than the state that consented on their behalf. Within the beneficiary coalition the divergence is sharper still: climate-vulnerable nations experience the structure as a lifeline they depend on for survival (trapped beneficiaries), large emerging economies experience it as regulatory room plus diplomatic pressure (dual-positioned), and petrostates experience it as pure exemption captured with arbitrage-grade exit. Same nominal category, three different structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation: industrialized_annex_i_nations and developed_nation_taxpayers_and_industry sit near the full-target end (high d), with the taxpayers' lack of direct representation and constrained exit pushing their effective extraction up relative to their governments'. climate_vulnerable_developing_nations sit near the full-beneficiary end (low d) — trapped exit amplifies effective extraction only for targets, so their trap deepens dependence rather than extraction. large_emerging_economies derive low-to-moderate d: primary beneficiaries whose secondary payer position (severe domestic impacts, future-review exposure) tempers the subsidy. petrostate_developing_exporters derive near-full-beneficiary d with arbitrage exit — the configuration that extracts the most benefit while bearing the least exposure. The g77 bloc derives slightly below symmetric, leaning beneficiary: it administers the agenda and channels gains but its members bear real climate impacts. The secretariat sits near symmetric as a non-collecting administrator. No directionality overrides were needed: the structural declarations plus exit options already differentiate every seat correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 genealogy shows a live founding problem corroborated from outside the beneficiary set, so this is not a resolved-mandatrophy case — the classification must not collapse into either neighboring error. Calling this a snare would erase the genuine coordination achievement: without differentiation there is no universal climate regime at all (the uniform-obligation counterfactual is Copenhagen 2009, which collapsed). Calling it a rope would erase the asymmetric extraction: the same structure that enables participation binds one bloc, exempts the largest current emitters, and channels compensatory transfers whose scale is contested. Tangled rope holds both facts. The measurement series exists to catch the degradation path: if theater continues rising while finance delivery stagnates, the reading drifts toward theatrical maintenance of a displaced arrangement — the piton signature — and the 2015–2026 theater climb is the early warning. The claim/metric gap is deliberate: the arrangement is CLAIMED as tangled rope (structurally true of the liability structure when operative) while the metrics describe an arrangement increasingly sustained by declaration rather than operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the cbdr_principle kernel — what would the sibling voluntary_commitment_reading change structurally if it prevailed?',
    'Comparative classification of the sibling story: the voluntary reading removes developed nations from the victim set for binding constraints and recasts finance as technology-transfer-primary, moving developing nations from finance-recipient seats into transparency-and-review subject seats.',
    'If the sibling prevails, this reading''s victim set empties, its transfer function shrinks to technology flows, and its classification collapses toward rope; if this reading prevails, the sibling''s review-burden asymmetries become the contested surface instead.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which kernel reading this story instantiates and what the sibling reading would restructure.').

omega_variable(
    disagreement_location,
    'Where exactly do the two readings of the CBDR kernel disagree?',
    'Structural comparison of the axioms: the dispute sits in (a) obligation modality — binding quantified commitments versus nationally determined pledges — and (b) calibration basis — cumulative historical emissions versus present capability and technology access.',
    'Each location implies a different remedy path: resolving modality requires compliance-machinery design; resolving calibration requires updated attribution and capacity accounting. Mislocating the dispute produces remedies that address the wrong element.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location, conceptual, 'The specific structural elements on which the sibling readings diverge.').

omega_variable(
    differentiation_map_currency,
    'Does the 1992 developed/developing partition still track actual responsibility and capacity, given that several classified-developing economies now rank among the largest annual emitters?',
    'Updated cumulative-attribution and capacity accounting (historical share of warming attributable per state, current fiscal capacity indices) compared against the treaty partition.',
    'If the map is stale, the extraction asymmetry loses its justice warrant and the reading degrades toward pure transfer politics — supporting reclassification toward snare or accelerating the piton drift; if the map holds on cumulative grounds, the tangled-rope reading is stabilized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(differentiation_map_currency, empirical, 'Whether the founding differentiation map still fits current emissions and capacity distributions.').

omega_variable(
    finance_additionality,
    'Are loss/damage and adaptation flows additional to existing aid budgets, or relabeled development assistance?',
    'OECD DAC accounting audits distinguishing new obligations from rebooked commitments, tracked against fund capitalization versus assessed need.',
    'If flows are largely relabeled, realized extraction is far below the authored measure and the theater ratio understates the gap — the arrangement is closer to declaratory maintenance than the metrics suggest; if additional, the transfer function is real and the tangled-rope classification is anchored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finance_additionality, empirical, 'Whether the reading''s signature finance mechanism delivers new resources or rebrands old ones.').

omega_variable(
    binding_without_sovereign_enforcer,
    'Can international obligations be meaningfully ''binding'' absent supranational enforcement, or is the binding/voluntary distinction that separates this reading from its sibling partly theatrical?',
    'Compliance-outcome comparison across regimes with and without binding modality (Kyoto Annex-I performance versus Paris NDC trajectories), controlling for economic shocks.',
    'If bindingness changes behavior, the reading''s core delta over the sibling is material and the suppression metric is honestly nonzero; if not, the modality dispute is itself theater and both readings converge structurally despite their axiomatic contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_without_sovereign_enforcer, conceptual, 'Whether the binding-modality premise that distinguishes this reading is behaviorally operative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__historical_responsibility_reading, 1992, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_hrr_tr_t1992, cbdr_principle__historical_responsibility_reading, theater_ratio, 1992, 0.18).
narrative_ontology:measurement(cbdr_hrr_tr_t1997, cbdr_principle__historical_responsibility_reading, theater_ratio, 1997, 0.21).
narrative_ontology:measurement(cbdr_hrr_tr_t2001, cbdr_principle__historical_responsibility_reading, theater_ratio, 2001, 0.27).
narrative_ontology:measurement(cbdr_hrr_tr_t2005, cbdr_principle__historical_responsibility_reading, theater_ratio, 2005, 0.31).
narrative_ontology:measurement(cbdr_hrr_tr_t2009, cbdr_principle__historical_responsibility_reading, theater_ratio, 2009, 0.41).
narrative_ontology:measurement(cbdr_hrr_tr_t2012, cbdr_principle__historical_responsibility_reading, theater_ratio, 2012, 0.45).
narrative_ontology:measurement(cbdr_hrr_tr_t2015, cbdr_principle__historical_responsibility_reading, theater_ratio, 2015, 0.52).
narrative_ontology:measurement(cbdr_hrr_tr_t2019, cbdr_principle__historical_responsibility_reading, theater_ratio, 2019, 0.57).
narrative_ontology:measurement(cbdr_hrr_tr_t2022, cbdr_principle__historical_responsibility_reading, theater_ratio, 2022, 0.6).
narrative_ontology:measurement(cbdr_hrr_tr_t2026, cbdr_principle__historical_responsibility_reading, theater_ratio, 2026, 0.62).

% Extraction over time
narrative_ontology:measurement(cbdr_hrr_be_t1992, cbdr_principle__historical_responsibility_reading, base_extractiveness, 1992, 0.28).
narrative_ontology:measurement(cbdr_hrr_be_t1997, cbdr_principle__historical_responsibility_reading, base_extractiveness, 1997, 0.61).
narrative_ontology:measurement(cbdr_hrr_be_t2001, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2001, 0.57).
narrative_ontology:measurement(cbdr_hrr_be_t2005, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2005, 0.59).
narrative_ontology:measurement(cbdr_hrr_be_t2009, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2009, 0.54).
narrative_ontology:measurement(cbdr_hrr_be_t2012, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2012, 0.49).
narrative_ontology:measurement(cbdr_hrr_be_t2015, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2015, 0.46).
narrative_ontology:measurement(cbdr_hrr_be_t2019, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2019, 0.47).
narrative_ontology:measurement(cbdr_hrr_be_t2022, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2022, 0.56).
narrative_ontology:measurement(cbdr_hrr_be_t2026, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2026, 0.53).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_hrr_su_t1992, cbdr_principle__historical_responsibility_reading, suppression_requirement, 1992, 0.24).
narrative_ontology:measurement(cbdr_hrr_su_t1997, cbdr_principle__historical_responsibility_reading, suppression_requirement, 1997, 0.44).
narrative_ontology:measurement(cbdr_hrr_su_t2001, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2001, 0.42).
narrative_ontology:measurement(cbdr_hrr_su_t2005, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(cbdr_hrr_su_t2009, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2009, 0.47).
narrative_ontology:measurement(cbdr_hrr_su_t2012, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2012, 0.44).
narrative_ontology:measurement(cbdr_hrr_su_t2015, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(cbdr_hrr_su_t2019, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2019, 0.39).
narrative_ontology:measurement(cbdr_hrr_su_t2022, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2022, 0.41).
narrative_ontology:measurement(cbdr_hrr_su_t2026, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2026, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__historical_responsibility_reading, resource_allocation).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, voluntary_commitment_reading).

% DUAL FORMULATION NOTE:
% 'Common but differentiated responsibilities' is a single colloquial label covering two structurally distinct claims about what the UNFCCC kernel requires, decomposed per the epsilon-invariance principle. This story authors the historical_responsibility_reading: binding proportional mitigation plus loss/damage finance, developed nations in the victim set, epsilon approximately 0.53. The sibling story voluntary_commitment_reading authors the voluntary-NDC arrangement: no binding victim set, technology transfer as the primary developed-nation duty, materially lower epsilon. The historical link runs through both: the 1992 kernel text licenses both readings, Kyoto operationalized this one, Paris operationalized the sibling, and each reading's advocates cite the same kernel text against the other. Family linkage declared via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
