% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__abolitionist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__abolitionist, []).

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
 *   constraint_id: npt_article_iv_vi_pairing__abolitionist
 *   human_readable: NPT Article IV/VI Pairing — Abolitionist Reading (Humanitarian-Law Assessment)
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   The NPT's Article IV grants non-weapon states an 'inalienable right' to
 *   peaceful nuclear technology; Article VI obliges all parties to pursue
 *   negotiations on cessation of the arms race and complete disarmament. Five
 *   decades on, the five pre-treaty arsenals persist under modernization
 *   programs running decades forward, no disarmament negotiations have been
 *   convened by their owners, and fuel-cycle capabilities have spread under
 *   safeguards that cannot distinguish intent. This story authors the
 *   ABOLITIONIST READING of that arrangement: the standing NPT regime
 *   assessed by humanitarian-law and weapons-prohibition-treaty standards —
 *   the lineage running from St. Petersburg through the Geneva Protocol, the
 *   anti-personnel-mine and cluster-munitions conventions, and the 2017
 *   prohibition treaty. Under those lights the arrangement coordinates real
 *   restraint (verified non-acquisition that has measurably slowed cascade
 *   proliferation) while imposing catastrophic asymmetric costs: risk
 *   accepted by no one, restraint demanded of the many, arsenals kept by the
 *   few, and the one alternative remedy actively suppressed. Per the
 *   epsilon-referent rule, epsilon indexes the standing arrangement as this
 *   reading sees it — never the prohibition regime this reading would
 *   install. Claim and metrics are authored independently: the claimed type
 *   states what this reading holds structurally true; the metrics describe
 *   the arrangement's operation as the record shows. Family decomposition:
 *   the colloquial label 'the NPT bargain' covers three structurally distinct
 *   claims with distinct epsilon values; the sibling readings are separate
 *   files linked through network.affects_constraints. KEY AGENTS (by
 *   structural relationship): - npt_weapon_states: agenda-setting beneficiary
 *   (institutional/arbitrage) — retains arsenals indefinitely, controls
 *   review agenda and Council enforcement - extended_deterrence_ally_states:
 *   protected beneficiary (powerful/constrained) — collects umbrella
 *   security, opposes prohibition remedies - dual_use_program_states:
 *   dual-positioned beneficiary-payer (moderate/constrained) — receives
 *   fuel-cycle access, carries inspection and stigma costs -
 *   threshold_hedging_states: opportunistic beneficiary (moderate/mobile) —
 *   exploits the peaceful/military ambiguity - nonallied_nonnuclear_states:
 *   primary target (organized/trapped) — bears risk and restraint without
 *   commensurate return - tpnw_state_parties: target with suppressed remedy
 *   (organized/trapped) — pursued the alternative, faces coordinated pressure
 *   - nuclear_test_downwind_communities: concentrated human target
 *   (powerless/trapped) — absorbed the testing legacy -
 *   iaea_safeguards_administration: administering agent
 *   (institutional/constrained) — verifies one side of the arrangement only -
 *   international_humanitarian_law_jurists: analytical observer
 *   (analytical/analytical) — sees the full structure
 *
 * KEY AGENTS:
 *   - npt_weapon_states: agenda-setting beneficiary (institutional/arbitrage) — retains arsenals indefinitely, controls review agenda and Council enforcement
 *   - extended_deterrence_ally_states: protected beneficiary (powerful/constrained) — collects umbrella security, opposes prohibition remedies
 *   - dual_use_program_states: dual-positioned beneficiary-payer (moderate/constrained) — receives fuel-cycle access, carries inspection and stigma costs
 *   - threshold_hedging_states: opportunistic beneficiary (moderate/mobile) — exploits the peaceful/military ambiguity
 *   - nonallied_nonnuclear_states: primary target (organized/trapped) — bears risk and restraint without commensurate return
 *   - tpnw_state_parties: target with suppressed remedy (organized/trapped) — pursued the alternative, faces coordinated pressure
 *   - nuclear_test_downwind_communities: concentrated human target (powerless/trapped) — absorbed the testing legacy
 *   - iaea_safeguards_administration: administering agent (institutional/constrained) — verifies one side of the arrangement only
 *   - international_humanitarian_law_jurists: analytical observer (analytical/analytical) — sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, 0.78).
domain_priors:suppression_score(npt_article_iv_vi_pairing__abolitionist, 0.72).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, extractiveness, 0.78).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__abolitionist, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__abolitionist, "NPT Article IV/VI Pairing — Abolitionist Reading (Humanitarian-Law Assessment)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__abolitionist, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__abolitionist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__abolitionist, '7da48a48-5e0c-4576-ae32-54cb5e524d17').
narrative_ontology:cs_kernel_codification('7da48a48-5e0c-4576-ae32-54cb5e524d17', fixed_text).
narrative_ontology:cs_authority_grounding('7da48a48-5e0c-4576-ae32-54cb5e524d17', lineage).
narrative_ontology:cs_interpretation_layer_present('7da48a48-5e0c-4576-ae32-54cb5e524d17').
narrative_ontology:cs_reading_relation('7da48a48-5e0c-4576-ae32-54cb5e524d17', npt_article_iv_vi_pairing__grand_bargain, forecloses).
narrative_ontology:cs_reading_relation('7da48a48-5e0c-4576-ae32-54cb5e524d17', npt_article_iv_vi_pairing__nonproliferation_primary, forecloses).
narrative_ontology:cs_axiom('7da48a48-5e0c-4576-ae32-54cb5e524d17', foundational, weapon_possession_categorically_illegal).
narrative_ontology:cs_axiom_status(weapon_possession_categorically_illegal, holdable).
narrative_ontology:cs_axiom_grounding('7da48a48-5e0c-4576-ae32-54cb5e524d17', weapon_possession_categorically_illegal, deontological).
narrative_ontology:cs_axiom('7da48a48-5e0c-4576-ae32-54cb5e524d17', foundational, no_peaceful_military_program_distinction).
narrative_ontology:cs_axiom_status(no_peaceful_military_program_distinction, holdable).
narrative_ontology:cs_axiom_grounding('7da48a48-5e0c-4576-ae32-54cb5e524d17', no_peaceful_military_program_distinction, empirically_contingent).
narrative_ontology:cs_reference_frame('7da48a48-5e0c-4576-ae32-54cb5e524d17', transitional_disarmament_pact_under_prohibition_norm).
narrative_ontology:cs_drift_state('7da48a48-5e0c-4576-ae32-54cb5e524d17', post_tpnw_contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('7da48a48-5e0c-4576-ae32-54cb5e524d17', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, npt_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, extended_deterrence_ally_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, dual_use_program_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, threshold_hedging_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, nonallied_nonnuclear_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, tpnw_state_parties).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, nuclear_test_downwind_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, dual_use_program_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, threshold_hedging_states).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__abolitionist, nuclear_deterrence_doctrine).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__abolitionist, article_iv_inalienable_right_interpretation).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__abolitionist, step_by_step_incrementalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five governments that built nuclear arsenals before the treaty and retained them under it. They set the review-cycle agenda, hold veto power over Security Council enforcement, and decide whether disarmament negotiations begin. Their arsenals are being modernized through the 2070s under announced programs. They collect deterrence prestige, alliance leadership, and exemption from the verification regime applied to everyone else. Exit for them means giving up the arsenals — the one thing the arrangement lets them keep indefinitely.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, npt_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, npt_weapon_states, beneficiary).

% Governments in NATO, East Asia, and the Pacific that do not possess nuclear weapons but rely on a protector's arsenal for security. They gain defense at no weapons-program cost and lobby against prohibition instruments that would delegitimize the protector's arsenal. Leaving the arrangement means rebuilding independent deterrence or accepting vulnerability, so they stay bound despite objecting to the risk the arsenals impose on their own territories.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, extended_deterrence_ally_states, beneficiary,
    powerful, biographical, constrained, regional).

% Non-weapon governments operating enrichment or reprocessing facilities under safeguards — civil power programs whose fuel-cycle capabilities are identical to weapons-relevant ones. They receive technology access and energy benefits under the peaceful-use clause while carrying inspection burdens, diversion suspicion in crises, and the stigma attached to their capabilities. Giving the capabilities up is economically costly; keeping them draws scrutiny.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, dual_use_program_states, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, dual_use_program_states, payer).

% Governments that maintain or seek fuel-cycle capabilities short of weapons, preserving a breakout option the treaty text does not forbid. They benefit from the ambiguity between peaceful and military programs — the wider the permitted civilian footprint, the cheaper the hedge. Some face sanctions or diplomatic isolation when their hedging becomes conspicuous; none has an incentive to clarify the boundary they exploit, and one predecessor demonstrated that withdrawal is survivable.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, threshold_hedging_states, beneficiary,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, threshold_hedging_states, payer).

% The majority of governments: neither armed nor under another power's umbrella. They forgo weapons, accept inspections, and bear the catastrophic risk the retained arsenals impose on everyone, with no comparable security return. Their collective weight passes resolutions and produced a prohibition treaty, but individually they cannot alter the review agenda or compel negotiations. Leaving the treaty would cost them trade, assistance, and standing while changing nothing about the risk.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nonallied_nonnuclear_states, payer,
    organized, generational, trapped, global).

% Governments that joined the 2017 prohibition treaty as their remedy for the risk imposed on them. Most are treaty-bound non-weapon states that continue attending the older regime's review meetings, where their instrument is treated as divisive and out of order. They face open pressure — alliance members warned against signing, financial institutions lobbied — aimed at keeping their remedy marginal.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, tpnw_state_parties, payer,
    organized, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, tpnw_state_parties, excluded).

% Populations irradiated and displaced by atmospheric and underground testing conducted while the arrangement tolerated such tests: Marshall Islanders, communities near Semipalatinsk and French Polynesia, and downwind populations elsewhere. They carry elevated cancer rates, contaminated land and fisheries, and compensation schemes that were underfunded or capped. They had no seat where the testing decisions were made and no exit from the contamination.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_test_downwind_communities, payer,
    powerless, biographical, trapped, regional).

% The secretariat that verifies non-weapon states' declarations, inspects facilities, and reports diversion findings to the Security Council. Its mandate covers the restraint side of the arrangement only; it has no authority over the arsenals the five retain. Its budget and access depend on member-state goodwill, and its findings become politicized exactly when they matter most.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, iaea_safeguards_administration, agenda_setter,
    institutional, generational, constrained, global).

% Legal scholars, ICRC lawyers, and judges working in the humanitarian-law tradition who assess the arrangement against the rules governing indiscriminate weapons. They produced the 1996 advisory-opinion analysis of the disarmament obligation, the humanitarian-conference evidence base, and the legal commentary underpinning the prohibition treaty. They observe and publish; they command no enforcement.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, international_humanitarian_law_jurists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__abolitionist, npt_weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__abolitionist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the cascade problem: verified mutual restraint that keeps non-weapon states from acquiring arsenals, a common inspection standard, and a standing forum where restraint commitments are recorded — goods none of the restrained states could secure alone.
% TRANSFER_FUNCTION: Moves deterrence capacity, status, and agenda control to the five armed governments; moves restraint obligations, inspection burdens, and catastrophic risk to everyone else; moves fuel-cycle access to compliant and hedging states while dispersing diversion risk across all; moves the disarmament promise forward indefinitely without delivery.
% ABSENT_VOICES: Test-affected communities and hibakusha hold no seat in review conferences; the populations living under the missiles' flight paths never consented to the risk; future generations are represented by no one. Prohibition-treaty parties attend, but their remedy is ruled out of order by agenda controllers aligned with the armed five.
% DISAPPEARANCE_RATIONALE: Overnight disappearance collapses the verification standard, opens immediate acquisition incentives for threshold states, removes the legal frame behind which the armed five operate, and leaves the prohibition treaty as the only operative norm — alliance structures, export controls, and the review machinery would all reorganize within months.
% FOUNDING_PROBLEM: Stop the predicted cascade of new nuclear states in the late 1960s by freezing the club at five, trading verified restraint and promised technology access for a commitment — deliberately undated — that the five would negotiate their arsenals away.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting five: the International Court of Justice's 1996 advisory opinion (unanimous on the existence of the disarmament obligation), the ICRC's and the Red Cross/Red Crescent movement's resolutions on the unacceptable humanitarian consequences, annual General Assembly majorities, and the 122-government vote creating the prohibition treaty all attest that the disarmament half of the founding problem remains unfulfilled; the armed five and their allies attest the prevention half is live and dispute that the disarmament half binds. No source outside the benefiting parties attests that the bargain is functioning as designed.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__abolitionist, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__abolitionist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__abolitionist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__abolitionist, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__abolitionist, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__abolitionist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__abolitionist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.78 at interval end) because the arrangement's costs — catastrophic risk imposed without consent, restraint obligations, inspection burdens — fall on parties receiving no commensurate return, while the five retain arsenals under announced modernization programs extending past mid-century. Suppression (0.72) is authored as a raw structural property, unscaled by power or scope: it reflects the enforcement machinery — export-control regimes, alliance pressure against the prohibition instrument, sanctions following withdrawal, funding leverage over the verifier — that keeps exits closed; the engine scales only extractiveness, by directionality and scope. Theater (0.62) tracks the growing share of review-cycle activity producing declarations rather than negotiations: action plans adopted and shelved, glossaries issued while modernization proceeds. The temporal series share one grid (eight points, 1968–2025) so every metric is authored at every examined time point. The trajectories show a cyclical pattern superimposed on a ratchet: détente episodes (the SALT era, the post-Cold-War opening, the 2010 New START/Prague dip) briefly relax extraction and suppression, then accumulation resumes. The oscillation is itself partially an extraction mechanism — intermittent reinforcement, since each concession renews non-weapon acquiescence without delivering the disarmament endpoint. Accessibility collapse is moderate (0.45) because a functioning alternative framework exists and 122 governments adopted it, yet alliance pressure collapses that alternative for precisely the states whose defection would matter. Resistance is substantial (0.65): the humanitarian initiative, the prohibition treaty, the 1996 advisory opinion, and recurring review-conference revolt are real, organized, and ongoing. Base properties are measured at interval end (2025).
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same text. From the weapon-states' seat the arrangement is a stability mechanism they administer and fund — low effective burden, arbitrage-grade exit, since they alone may reinterpret, modernize, or withhold cooperation at will. From the non-allied non-weapon seat the same structure operates as enforced hierarchy: full obligations, no arsenal, no umbrella, no agenda control — high burden, trapped exit. Prohibition-treaty parties occupy a third position: they exercised the one exit the system nominally permits and were met with coordinated pressure, so their computed burden includes the cost of having objected. Alliance states experience subsidy and bondage simultaneously. The engine derives these divergences from the declared positions and exit options; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Weapon states sit nearest the beneficiary pole: they wrote the asymmetry, collect its returns, and control its maintenance. Alliance states are subsidized but bound — benefits exceed costs, though dependence constrains their exit. Fuel-cycle states and hedgers draw genuine technology value while paying inspection and stigma costs, placing them mid-low. The payer seats cluster near the target pole: non-allied states bear risk and restraint with trapped exit; prohibition parties add the price of a suppressed remedy; test-affected communities are the concentrated human end of the chain — powerless, immovable, harmed irreversibly. The jurists are analytical and take no directional position. No directionality overrides are declared: the beneficiary/victim declarations plus exit options already separate the seats cleanly, and a power-atom-keyed override would misfile the two institutional seats (the armed five and the verifier) that share the 'institutional' atom but sit at opposite poles.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as pure coordination erases the captured half — the five profit concretely and actively enforce the structure, so a rope verdict would launder the hierarchy. Reading it as pure extraction erases the verified-restraint function that has slowed cascade acquisition — a snare verdict would discard the one component worth preserving. The hybrid verdict holds both halves together, which is exactly what the mandate-atrophy question requires: the arrangement's founding design — a temporary disparity pending negotiated disarmament — has atrophied in its disarmament half while its prevention half remains live. A piton reading is excluded by the receipt surface: gains concentrate in a named seat that vigorously maintains the structure, the opposite of nobody-profiting inertia. The founding-problem interview records the split as contested, and the combination of a contested founding problem with a world that would visibly rearrange without the arrangement flags the zombie-transitional character for cross-check against the rising theater trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the abolitionist specification of the Article IV/VI relationship (Article VI mandatory, Article IV risk-conditioned, possession categorically unlawful) the correct reading of the shared treaty text, or do the grand-bargain and nonproliferation-primary readings specify different, equally coherent constraints?',
    'Comparative structural analysis tracking which reading''s predictions match state practice and legal development: prohibition-treaty ratification curves, review-conference outcomes, and litigation results weighed against the siblings'' predictive record.',
    'Adopting the grand-bargain reading would recast non-weapon states as co-authors rather than victims and lower epsilon substantially; adopting the nonproliferation-primary reading would recast weapon states as legitimate authorities and delete the categorical-illegality premise entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the shared npt_article_iv_vi_pairing kernel this constraint instantiates.').

omega_variable(
    article_vi_justiciability,
    'Is Article VI a binding obligation with cognizable breach, or a political aspiration incapable of judicial enforcement?',
    'Judicial treatment: follow-up requests to the International Court of Justice, standing rulings in cases such as the Marshall Islands litigation, and treaty-body practice on disarmament obligations.',
    'A justiciable Article VI converts fifty years of non-performance into cognizable breach, raising effective extraction and pushing the classification toward the snare boundary; a non-justiciable Article VI supports the grand-bargain framing in which the obligation is background condition rather than enforceable duty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability, empirical, 'Whether the disarmament obligation is legally enforceable or purely hortatory.').

omega_variable(
    dual_use_safeguards_adequacy,
    'Can safeguards reduce diversion risk from enrichment and reprocessing to negligible levels, or is the fuel-cycle capability inherently risk-bearing regardless of inspection intensity?',
    'IAEA detection-probability assessments and the historical diversion record: Iraq pre-1991, Libya, Iran''s accumulation sequence, and North Korea''s breakout timeline from safeguards-covered facilities.',
    'If safeguards can neutralize diversion risk, the no-peaceful-military-distinction axiom loses empirical force and Article IV survives this reading''s critique in modified form; if they cannot, the arrangement trends toward the snare boundary as its coordination cover thins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_use_safeguards_adequacy, empirical, 'Whether the dual-use risk this reading condemns is technically manageable or structural.').

omega_variable(
    deterrence_substitutability,
    'Can the armed five''s security requirements be restructured without nuclear deterrence — through negative security assurances, no-first-use doctrines, and regional arrangements — or is arsenal retention load-bearing for their security?',
    'Comparative security studies of extended-deterrence substitution, allied-state responses to assurance changes, and historical episodes of arsenal reduction without security collapse.',
    'If substitutable, arsenal retention lacks justification even on security grounds and the arrangement moves toward the snare boundary; if not, a genuine security good sustains the coordination component and the hybrid classification firms up.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_substitutability, empirical, 'Whether the security function cited to justify retention is separable from the arsenals.').

omega_variable(
    prohibition_norm_trajectory,
    'Will the prohibition-treaty norm achieve broad adherence and reshape the regime''s legitimacy conditions, or remain a minority instrument the armed five can contain?',
    'Ratification-curve extrapolation, financial-sector divestment diffusion, and defection signals among alliance states currently pressured against joining.',
    'Growing universality raises the suppression cost on the armed five and shifts the regime''s normative center of gravity toward this reading; containment entrenches the current structure and validates the suppression measurements as durable rather than transitional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prohibition_norm_trajectory, empirical, 'Trajectory of the alternative remedy this reading treats as authoritative.').

omega_variable(
    intergenerational_risk_attribution,
    'Does catastrophic risk imposed on future generations count as extraction from a present party, or as an unattributable externality outside the victim structure?',
    'Legal-philosophical settlement on intergenerational obligations, drawing on precedents where courts granted future-generations standing in environmental and climate litigation.',
    'Attribution widens the victim set beyond present-day actors and raises epsilon further; non-attribution caps epsilon at harms to presently existing parties and narrows the payer seats to those named here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_risk_attribution, conceptual, 'Whether the arrangement''s longest-lived cost enters the extraction accounting.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__abolitionist, 1968, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1968, 0.3).
narrative_ontology:measurement_basis(npt__tr_t1968, observed).
narrative_ontology:measurement(npt__tr_t1978, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1978, 0.35).
narrative_ontology:measurement_basis(npt__tr_t1978, observed).
narrative_ontology:measurement(npt__tr_t1988, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1988, 0.4).
narrative_ontology:measurement_basis(npt__tr_t1988, observed).
narrative_ontology:measurement(npt__tr_t1995, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1995, 0.48).
narrative_ontology:measurement_basis(npt__tr_t1995, observed).
narrative_ontology:measurement(npt__tr_t2005, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2005, 0.55).
narrative_ontology:measurement_basis(npt__tr_t2005, observed).
narrative_ontology:measurement(npt__tr_t2010, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2010, 0.5).
narrative_ontology:measurement_basis(npt__tr_t2010, observed).
narrative_ontology:measurement(npt__tr_t2017, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2017, 0.58).
narrative_ontology:measurement_basis(npt__tr_t2017, observed).
narrative_ontology:measurement(npt__tr_t2025, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2025, 0.62).
narrative_ontology:measurement_basis(npt__tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1968, 0.55).
narrative_ontology:measurement_basis(npt__be_t1968, observed).
narrative_ontology:measurement(npt__be_t1978, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1978, 0.6).
narrative_ontology:measurement_basis(npt__be_t1978, observed).
narrative_ontology:measurement(npt__be_t1988, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1988, 0.67).
narrative_ontology:measurement_basis(npt__be_t1988, observed).
narrative_ontology:measurement(npt__be_t1995, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1995, 0.71).
narrative_ontology:measurement_basis(npt__be_t1995, observed).
narrative_ontology:measurement(npt__be_t2005, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2005, 0.73).
narrative_ontology:measurement_basis(npt__be_t2005, observed).
narrative_ontology:measurement(npt__be_t2010, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement_basis(npt__be_t2010, observed).
narrative_ontology:measurement(npt__be_t2017, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2017, 0.74).
narrative_ontology:measurement_basis(npt__be_t2017, observed).
narrative_ontology:measurement(npt__be_t2025, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2025, 0.78).
narrative_ontology:measurement_basis(npt__be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1968, 0.45).
narrative_ontology:measurement_basis(npt__su_t1968, observed).
narrative_ontology:measurement(npt__su_t1978, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1978, 0.52).
narrative_ontology:measurement_basis(npt__su_t1978, observed).
narrative_ontology:measurement(npt__su_t1988, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1988, 0.55).
narrative_ontology:measurement_basis(npt__su_t1988, observed).
narrative_ontology:measurement(npt__su_t1995, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement_basis(npt__su_t1995, observed).
narrative_ontology:measurement(npt__su_t2005, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2005, 0.63).
narrative_ontology:measurement_basis(npt__su_t2005, observed).
narrative_ontology:measurement(npt__su_t2010, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement_basis(npt__su_t2010, observed).
narrative_ontology:measurement(npt__su_t2017, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2017, 0.68).
narrative_ontology:measurement_basis(npt__su_t2017, observed).
narrative_ontology:measurement(npt__su_t2025, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement_basis(npt__su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__abolitionist, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__nonproliferation_primary).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'the NPT bargain' decomposes into three structurally distinct claims per the epsilon-invariance principle. This file authors the abolitionist instantiation (epsilon high, assessed by humanitarian-law lights over the standing arrangement). The nonproliferation-primary sibling authors low epsilon (the arrangement as successful security management); the grand-bargain sibling authors intermediate epsilon (an unbalanced but mutually authored exchange). Upstream/downstream structure: the nonproliferation-primary reading is cited as evidentiary support by the grand-bargain reading, while this abolitionist reading exerts legitimacy pressure on both through the prohibition treaty's stigmatization of possession — pressure that changes their operating environment without resolving the textual dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
