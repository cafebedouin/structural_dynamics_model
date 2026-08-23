% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__advisory_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__advisory_coordination_reading, []).

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
 *   constraint_id: wto_dsb_authority__advisory_coordination_reading
 *   human_readable: WTO Dispute Settlement — Advisory Coordination Arrangement
 *   domain: international law / trade governance / institutional legitimacy
 *
 * SUMMARY:
 *   A standing arrangement in international trade governance: when two
 *   members disagree over trade measures — subsidies, dumping margins,
 *   sanitary rules, licensing regimes — they may take the disagreement
 *   through a structured sequence of government-to-government consultations
 *   followed, if unresolved, by an impartial panel that examines the facts
 *   and law and issues findings, which feed back into negotiation until the
 *   parties settle or one side concedes adjustment. Under the
 *   advisory-coordination understanding of this arrangement, the panel's
 *   product is expert assessment offered to the disputants, not a command:
 *   members retain full discretion over what to concede, and follow-through
 *   comes from bargaining weight, market-access interest, and cross-issue
 *   reciprocity rather than institutional compulsion. The arrangement solves
 *   a real coordination problem while distributing its benefits by market
 *   size: the same report that reliably moves a small partner is reliably
 *   shelved when a large partner dislikes it. Small claimants finance
 *   participation and wait years for outcomes priced by someone else's
 *   leverage; large players treat identical reports as an inventory of
 *   negotiating assets. The claim/metric split is deliberate: this story
 *   CLAIMS tangled_rope (genuine settlement coordination wrapped around
 *   power-priced enforcement) while the metrics are authored independently
 *   from the arrangement's observed operation. KEY AGENTS (by structural
 *   relationship): - major_trading_powers: agenda-setting beneficiary bloc
 *   (institutional/arbitrage) — administers the loose mode and collects
 *   discretion plus leverage - us_trade_representative: paradigmatic
 *   discretion-holder (institutional/arbitrage) — keeps unilateral tools warm
 *   and caps the authority the system may claim - small_open_economies:
 *   primary payers (moderate/constrained) — finance and staff participation;
 *   outcomes priced by others' market weight -
 *   import_injured_domestic_industries: payer constituency
 *   (organized/constrained) — wait through long sequences for
 *   leverage-contingent relief - dispute_settlement_apparatus: professional
 *   beneficiary (organized/identity_locked) — secretariat, panelists, and
 *   Geneva legal community fused with the process -
 *   affected_third_party_exporters: excluded voice (moderate/constrained) —
 *   interests traded away in closed settlements -
 *   academic_trade_regime_analysts: analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__advisory_coordination_reading, 0.6).
domain_priors:suppression_score(wto_dsb_authority__advisory_coordination_reading, 0.28).
domain_priors:theater_ratio(wto_dsb_authority__advisory_coordination_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__advisory_coordination_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__advisory_coordination_reading, "WTO Dispute Settlement — Advisory Coordination Arrangement").
narrative_ontology:topic_domain(wto_dsb_authority__advisory_coordination_reading, "international law / trade governance / institutional legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__advisory_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__advisory_coordination_reading, '5c3ea157-ce20-44de-94ce-fd32986136a9').
narrative_ontology:cs_kernel_codification('5c3ea157-ce20-44de-94ce-fd32986136a9', formalized).
narrative_ontology:cs_authority_grounding('5c3ea157-ce20-44de-94ce-fd32986136a9', distributed).
narrative_ontology:cs_reading_relation('5c3ea157-ce20-44de-94ce-fd32986136a9', wto_dsb_authority__binding_referee_reading, forecloses).
narrative_ontology:cs_reading_relation('5c3ea157-ce20-44de-94ce-fd32986136a9', wto_dsb_authority__judicial_activism_reading, forecloses).
narrative_ontology:cs_axiom('5c3ea157-ce20-44de-94ce-fd32986136a9', foundational, member_policy_supremacy_over_findings).
narrative_ontology:cs_axiom_status(member_policy_supremacy_over_findings, holdable).
narrative_ontology:cs_axiom_grounding('5c3ea157-ce20-44de-94ce-fd32986136a9', member_policy_supremacy_over_findings, conventional).
narrative_ontology:cs_axiom('5c3ea157-ce20-44de-94ce-fd32986136a9', secondary, panel_findings_are_settlement_inputs).
narrative_ontology:cs_axiom_status(panel_findings_are_settlement_inputs, holdable).
narrative_ontology:cs_axiom_grounding('5c3ea157-ce20-44de-94ce-fd32986136a9', panel_findings_are_settlement_inputs, instrumental).
narrative_ontology:cs_reference_frame('5c3ea157-ce20-44de-94ce-fd32986136a9', consensus_diplomatic_adjudication).
narrative_ontology:cs_drift_state('5c3ea157-ce20-44de-94ce-fd32986136a9', post_2019_appellate_paralysis, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('5c3ea157-ce20-44de-94ce-fd32986136a9', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, major_trading_powers).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, dispute_settlement_apparatus).
narrative_ontology:constraint_victim(wto_dsb_authority__advisory_coordination_reading, small_open_economies).
narrative_ontology:constraint_victim(wto_dsb_authority__advisory_coordination_reading, import_injured_domestic_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the dispute system's operating mode through consensus control: they draft and renegotiate the procedures, decide by consensus whether reports are adopted, and staff the bodies that issue them. Because their markets are the prizes in most disputes, panel findings become bargaining assets in their hands whether or not follow-through occurs. They can run disputes bilaterally or regionally whenever the multilateral channel inconveniences them, and they periodically defend the loose mode against proposals to harden it.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, major_trading_powers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__advisory_coordination_reading, major_trading_powers, beneficiary).

% Carries the strongest tradition of retaining unilateral discretion: maintains statutory tools for acting outside the multilateral channel, has declined since 2017 to allow the appellate step to function by withholding the appointments it requires, and treats panel conclusions as material for negotiated packages rather than as instructions. Its posture effectively caps how much authority the system can claim.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, us_trade_representative, agenda_setter,
    institutional, biographical, arbitrage, global).

% Bring a caseload disproportionate to their size and finance participation with legal capacity they can barely afford. Outcomes arrive priced by market weight: their findings stick against small partners and dissolve against large ones. Leaving the venue would leave them with raw bilateral exposure to larger markets, so they stay and absorb the costs of a process whose products they cannot enforce.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, small_open_economies, payer,
    moderate, generational, constrained, global).

% Petition their governments for relief from foreign measures such as subsidies, dumping margins, and regulatory hurdles, then wait through multi-year sequences that end in negotiated compromises rather than guaranteed corrections. Their relief depends on whether their government holds leverage over the offending market, not on the strength of the finding produced.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, import_injured_domestic_industries, payer,
    organized, immediate, constrained, national).

% The secretariat legal staff, rostered panelists, and the wider Geneva trade-law community supply the analysis, chair the proceedings, and train each generation of practitioners. Careers, methods, and institutional memory are built around dispute activity continuing; the apparatus advocates the process's centrality and experiences any shrinkage of its mandate as professional loss rather than neutral reallocation.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, dispute_settlement_apparatus, beneficiary,
    organized, biographical, identity_locked, global).

% Firms and sectors in countries not seated at the table whose market conditions shift when two others settle: negotiated packages routinely trade away third-party access or extend discriminatory measures with carve-outs for those who join. They have no standing in the closed settlement conversations and learn the outcomes afterward.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, affected_third_party_exporters, excluded,
    moderate, biographical, constrained, global).

% Track dispute records, settlement terms, and compliance patterns; document how outcomes distribute across member sizes and whether findings predict results or merely decorate positions already taken. They publish outside the bargaining rooms and reach reform debates only indirectly.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, academic_trade_regime_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__advisory_coordination_reading, major_trading_powers).
narrative_ontology:fixing_cost_class(wto_dsb_authority__advisory_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts bilateral trade frictions into a structured sequence — consultations, impartial factual and legal assessment, findings fed back into negotiation — that lowers information costs, depoliticizes technical questions, and gives both sides a face-saving instrument for settlement without any surrender of sovereign control.
% TRANSFER_FUNCTION: Moves negotiating leverage and legitimation: panel findings become bargaining assets deployable or shelvable at will, concessions flow through negotiated packages weighted by market size, and the costs of litigation capacity and waiting time are borne disproportionately by smaller claimants.
% ABSENT_VOICES: Affected third-party exporters and downstream consumers whose market conditions are traded away in closed-door settlements have no seat in the conversation; nor do non-participants in the plurilateral workaround arrangements. They sit outside the room, represented only by capitals that may trade their interests for package advantages.
% DISAPPEARANCE_RATIONALE: If the advisory dispute process vanished overnight, disputes would revert to raw bilateral power plays and unilateral retaliation statutes, small states would lose their only low-cost forum for airing market-access grievances, settlement rates would fall, and trade frictions would escalate more readily into tit-for-tat barriers. The largest powers would manage; the web of negotiated restraint the process scaffolds would not reassemble on its own.
% FOUNDING_PROBLEM: Sovereign equals with no appetite for a supranational judge needed a way to break trade deadlocks: a neutral technical assessment that both sides could accept as honest without accepting it as a command.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: the classical scholarship documenting the GATT system's deliberately diplomatic design, and small-member reform submissions in the DSU review negotiations that describe the same deadlock-breaking purpose while disputing its adequacy for weak claimants. The major powers' own accounts agree but are discounted as self-interested; the external scholarly and small-member attestations carry the provenance.
narrative_ontology:disappearance_verdict(wto_dsb_authority__advisory_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__advisory_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__advisory_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_dsb_authority__advisory_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__advisory_coordination_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__advisory_coordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__advisory_coordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__advisory_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.60 because the coordination product is real but its incidence is skewed: identical findings force adjustment from small partners and evaporate against large ones, so small claimants pay litigation costs and waiting time for outcomes discounted by leverage they do not hold. Suppression is low (0.28) because almost nothing is coerced — members may ignore reports, act unilaterally, or leave — with only soft venue dependence (MFN interdependence, reputational norms) raising exit friction. Theater is substantial (0.50) and rising: filings made for signaling or domestic audiences, findings deployed to decorate positions already taken, and, after the appellate step stopped functioning, appeals launched principally to freeze adverse reports in limbo. Accessibility collapse is low (0.30): unilateral statutes, regional-agreement mechanisms, and plain bilateral pressure remain fully workable alternatives. Resistance is moderate (0.45): a durable reform coalition presses to restore enforceability while defenders of discretion resist hardening, so the mode itself is continuously contested. Active enforcement is required (true) because the loose mode does not hold by default — it is maintained through appointment politics, procedural-review stalemates, and plurilateral counter-builds; unattended, the structure hardens toward bindingness or fragments. The three measurement series share one grid (t = 0, 5, 10, 16, 21, 26, 31): extractiveness creeps upward as enforcement optionality grows, theater accelerates after appellate paralysis, and the suppression series traces the decay of enforcement capacity rather than any intensification of coercion. Endpoint values at t=31 are marked projected; earlier points are observed.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats compute a different arrangement than the agenda-setter seats do. From small_open_economies and import_injured_domestic_industries, the structure is a queue in which justice is priced by market weight: years of procedure ending in whatever the other side's leverage permits. From major_trading_powers and us_trade_representative, the same structure is sovereign convenience: an optional source of ammunition and a shield against supranational command. The apparatus seat experiences neither extraction nor discretion — it experiences a professional home whose centrality is the point. These are not misreadings of one thing; they are the per-seat classifications the engine computes from role, power, and exit data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for major_trading_powers and dispute_settlement_apparatus; victim declarations drive high directionality for small_open_economies and import_injured_domestic_industries. One override is authored: the structural derivation from 'beneficiary plus arbitrage exit' would place the institutional seats near the pure-beneficiary pole (d near 0.1), but their true relationship is dual — they administer the arrangement, collect its discretion dividend, and intermittently sit as targets when their own measures are challenged — so the override sets institutional seats at d = 0.30. Small claimants sit near 0.75: they receive real settlement services (the coordination half) while absorbing the power-pricing (the extraction half), making them heavy targets but not total ones. The apparatus is a near-pure collector. Excluded third-party exporters inherit moderate-high exposure through settlement spillovers they never agreed to.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what keeps both halves visible. Reading the arrangement as a pure rope (its own preferred self-description) would hide the power-priced extraction that small claimants subsidize; reading it as a snare would erase the genuine settlement function that even weak members keep voluntarily paying to use — no one is forced in, and everyone gets something. The founding problem (deadlock-breaking among sovereign equals unwilling to accept a supranational judge) remains live, so no mandatrophy is declared: the arrangement has not outlived its mandate; it is doing what it was built to do, unevenly. The piton risk runs in the opposite direction: if appellate machinery stays broken and findings degrade further into decoration, theater will pass 0.5 durably and the coordination half will thin toward performance — at which point the same structure re-reads as inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dsb_authority_kernel_reading,
    'Which reading of the wto_dsb_authority kernel does the standing arrangement instantiate, and how would the answer restructure this story?',
    'Comparative classification across the three sibling stories (advisory_coordination, binding_referee, judicial_activism): convergence of computed types on one reading, plus behavioral evidence on whether adopted reports carry compliance consequences.',
    'If the binding_referee_reading is accurate, the victim set expands to all members subject to enforced compliance and effective extraction rises with enforcement intensity; if the judicial_activism_reading is accurate, the extraction story shifts onto the legal apparatus manufacturing obligations. This story''s epsilon (0.60) is valid only under the advisory reading''s own lights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dsb_authority_kernel_reading, conceptual, 'Committer structure: this constraint is one reading of a contested kernel; sibling readings change the structural facts, especially the victim set and the compliance-status of findings.').

omega_variable(
    advisory_binding_substance_boundary,
    'Is the standing arrangement substantively advisory (reports as negotiation inputs) or formally binding with decayed enforcement?',
    'Share of disputes closing in negotiated settlement versus implemented rulings; count of reports frozen by appeals into a non-functioning appellate step; compliance trajectories before and after the 2019 appellate stoppage.',
    'Substantively binding implies higher suppression and extraction and a shift of coordination type toward enforcement_mechanism; substantively advisory leaves the authored metrics standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advisory_binding_substance_boundary, empirical, 'Whether advisory substance or binding form with enforcement decay describes the arrangement''s actual operation.').

omega_variable(
    settlement_power_elasticity,
    'How strongly do dispute outcomes track relative market power rather than the merit of the findings?',
    'Regression of dispute outcomes (adjustment, persistence, retaliation) on leverage proxies such as market size and retaliation capacity, controlling for finding strength and case merit indicators.',
    'High elasticity confirms systematic power-pricing and deepens the tangled_rope profile toward snare at the payer seat; low elasticity supports a rope-leaning coordination account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_power_elasticity, empirical, 'Elasticity of settlement outcomes to bilateral leverage versus finding merit.').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel the DSU treaty text (a fixed formal codification carried by an interpretive hierarchy) or the operational diplomacy of member consensus?',
    'Test which framing reproduces observed authority behavior: if authority tracks textual interpretation hierarchies, a fixed_text framing fits; if it tracks consensus bargaining, the formalized/distributed framing authored here fits.',
    'The alternative framing changes the commitment-system classification (fixed_text with an interpretive layer versus formalized/distributed) and would relocate the drift diagnosis from codification_collapse toward authority_erosion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'CS-framing under-determination; chosen framing reflects the advisory reading''s location of authority in member discretion and consensus practice, leaving no designated interpreter above the members.').

omega_variable(
    plurilateral_fragmentation_trajectory,
    'Do the plurilateral workarounds built around the stalled multilateral machinery consolidate into a functioning parallel system or dissolve back into bilateralism?',
    'Membership growth and caseload of the interim appeal arrangements; whether their outputs gain recognition in members'' domestic trade law practice.',
    'Consolidation partially revives binding norms inside a smaller club and shrinks this arrangement''s coordination reach; dissolution entrenches the advisory mode and deepens power-pricing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(plurilateral_fragmentation_trajectory, empirical, 'Trajectory of plurilateral dispute-resolution fragmentation relative to the multilateral channel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__advisory_coordination_reading, 0, 31).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_dsb_adv_tr_t0, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(wto_dsb_adv_tr_t0, observed).
narrative_ontology:measurement(wto_dsb_adv_tr_t5, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement_basis(wto_dsb_adv_tr_t5, observed).
narrative_ontology:measurement(wto_dsb_adv_tr_t10, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement_basis(wto_dsb_adv_tr_t10, observed).
narrative_ontology:measurement(wto_dsb_adv_tr_t16, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement_basis(wto_dsb_adv_tr_t16, observed).
narrative_ontology:measurement(wto_dsb_adv_tr_t21, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 21, 0.38).
narrative_ontology:measurement_basis(wto_dsb_adv_tr_t21, observed).
narrative_ontology:measurement(wto_dsb_adv_tr_t26, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 26, 0.46).
narrative_ontology:measurement_basis(wto_dsb_adv_tr_t26, observed).
narrative_ontology:measurement(wto_dsb_adv_tr_t31, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 31, 0.5).
narrative_ontology:measurement_basis(wto_dsb_adv_tr_t31, projected).

% Extraction over time
narrative_ontology:measurement(wto_dsb_adv_be_t0, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 0, 0.46).
narrative_ontology:measurement_basis(wto_dsb_adv_be_t0, observed).
narrative_ontology:measurement(wto_dsb_adv_be_t5, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(wto_dsb_adv_be_t5, observed).
narrative_ontology:measurement(wto_dsb_adv_be_t10, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement_basis(wto_dsb_adv_be_t10, observed).
narrative_ontology:measurement(wto_dsb_adv_be_t16, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement_basis(wto_dsb_adv_be_t16, observed).
narrative_ontology:measurement(wto_dsb_adv_be_t21, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 21, 0.56).
narrative_ontology:measurement_basis(wto_dsb_adv_be_t21, observed).
narrative_ontology:measurement(wto_dsb_adv_be_t26, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 26, 0.58).
narrative_ontology:measurement_basis(wto_dsb_adv_be_t26, observed).
narrative_ontology:measurement(wto_dsb_adv_be_t31, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 31, 0.6).
narrative_ontology:measurement_basis(wto_dsb_adv_be_t31, projected).

% Suppression requirement over time
narrative_ontology:measurement(wto_dsb_adv_su_t0, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(wto_dsb_adv_su_t0, observed).
narrative_ontology:measurement(wto_dsb_adv_su_t5, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement_basis(wto_dsb_adv_su_t5, observed).
narrative_ontology:measurement(wto_dsb_adv_su_t10, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 10, 0.54).
narrative_ontology:measurement_basis(wto_dsb_adv_su_t10, observed).
narrative_ontology:measurement(wto_dsb_adv_su_t16, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement_basis(wto_dsb_adv_su_t16, observed).
narrative_ontology:measurement(wto_dsb_adv_su_t21, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 21, 0.4).
narrative_ontology:measurement_basis(wto_dsb_adv_su_t21, observed).
narrative_ontology:measurement(wto_dsb_adv_su_t26, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 26, 0.3).
narrative_ontology:measurement_basis(wto_dsb_adv_su_t26, observed).
narrative_ontology:measurement(wto_dsb_adv_su_t31, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 31, 0.24).
narrative_ontology:measurement_basis(wto_dsb_adv_su_t31, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__advisory_coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority__judicial_activism_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'WTO dispute settlement authority' decomposes into three epsilon-distinct constraint stories over one kernel: the binding_referee_reading (treaty-text reading, upstream, highest formal codification, compliance obligations create their own victim structure), this advisory_coordination_reading (practice reading, findings as negotiation inputs, power-priced enforcement), and the judicial_activism_reading (critique reading, parasitic on the binding frame's premise that outputs function as obligations). Each sibling file carries a mirrored note; the upstream formal reading influences the practice reading because treaty text is cited as evidence in every dispute over the mode.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_dsb_authority__advisory_coordination_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
