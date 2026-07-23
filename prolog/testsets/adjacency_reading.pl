% ============================================================================
% CONSTRAINT STORY: adjacency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_adjacency_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: adjacency_reading
 *   human_readable: Constitutionalized Money/Governance Adjacency (Capped Crossing Reading)
 *   domain: constitutional_political_economy/blockchain_governance
 *
 * SUMMARY:
 *   This story instantiates the adjacency reading of the money/governance
 *   coupling kernel: a constitutional design in which wealth held on-chain
 *   confers no direct governance weight, and monetary mechanisms (treasury,
 *   surety, clearing) may only touch governance through enumerated, capped,
 *   audited crossing points. The design's proponents claim this produces an
 *   empty victim set — no stakeholder class accumulates permanent structural
 *   advantage because the ballot itself cannot be bought. But the design's
 *   own objections literature (its 8/9) concedes that residual, uncapped
 *   informal advantages persist: organized capital retains superior
 *   attendance, organizational capacity, and proposal-drafting access, even
 *   while the formal vote-purchase channel is closed. The metrics here are
 *   authored to reflect a genuinely low-but-nonzero extraction picture,
 *   consistent with a design that mostly works as claimed but leaks at its
 *   conceded edges — this is not the fusion reading (which would show high,
 *   direct, wealth-correlated extraction) nor the exile reading (which would
 *   show near-zero extraction but likely far higher friction costs from total
 *   separation).
 *
 * KEY AGENTS:
 *   - core_developer_council: designs and defends the enumerated crossings (institutional/constrained)
 *   - protocol_wide_stakeholders: benefit from non-plutocratic governance (organized/mobile)
 *   - small_token_holders: primary intended beneficiaries of the adjacency principle (powerless/constrained)
 *   - large_capital_holders: formally decoupled from direct vote purchase, retain arbitrage exit (powerful/arbitrage)
 *   - organized_delegate_blocs: retain conceded informal advantages despite formal decoupling (organized/constrained)
 *   - auditors_and_monitors: analytical seat checking whether the caps and audits actually hold (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(adjacency_reading, 0.28).
domain_priors:suppression_score(adjacency_reading, 0.35).
domain_priors:theater_ratio(adjacency_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(adjacency_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(adjacency_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(adjacency_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(adjacency_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(adjacency_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(adjacency_reading, tangled_rope).
narrative_ontology:human_readable(adjacency_reading, "Constitutionalized Money/Governance Adjacency (Capped Crossing Reading)").
narrative_ontology:topic_domain(adjacency_reading, "constitutional_political_economy/blockchain_governance").

domain_priors:requires_active_enforcement(adjacency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(adjacency_reading, '3612b3be-24ca-4c9a-ba78-e6dca4e16ada').
narrative_ontology:cs_kernel_codification('3612b3be-24ca-4c9a-ba78-e6dca4e16ada', formalized).
narrative_ontology:cs_authority_grounding('3612b3be-24ca-4c9a-ba78-e6dca4e16ada', practice).
narrative_ontology:cs_interpretation_layer_present('3612b3be-24ca-4c9a-ba78-e6dca4e16ada').
narrative_ontology:cs_reading_relation('3612b3be-24ca-4c9a-ba78-e6dca4e16ada', money_governance_coupling__fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('3612b3be-24ca-4c9a-ba78-e6dca4e16ada', money_governance_coupling__exile_reading, influences).
narrative_ontology:cs_axiom('3612b3be-24ca-4c9a-ba78-e6dca4e16ada', foundational, wealth_confers_no_direct_vote_weight).
narrative_ontology:cs_axiom_status(wealth_confers_no_direct_vote_weight, holdable).
narrative_ontology:cs_axiom_grounding('3612b3be-24ca-4c9a-ba78-e6dca4e16ada', wealth_confers_no_direct_vote_weight, conventional).
narrative_ontology:cs_axiom('3612b3be-24ca-4c9a-ba78-e6dca4e16ada', secondary, capital_functions_require_enumerated_capped_crossings).
narrative_ontology:cs_axiom_status(capital_functions_require_enumerated_capped_crossings, holdable).
narrative_ontology:cs_axiom_grounding('3612b3be-24ca-4c9a-ba78-e6dca4e16ada', capital_functions_require_enumerated_capped_crossings, instrumental).
narrative_ontology:cs_reference_frame('3612b3be-24ca-4c9a-ba78-e6dca4e16ada', capped_crossing_constitutional_baseline).
narrative_ontology:cs_drift_state('3612b3be-24ca-4c9a-ba78-e6dca4e16ada', post_delegate_bloc_organization_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('3612b3be-24ca-4c9a-ba78-e6dca4e16ada', '').
narrative_ontology:cs_kernel_id(adjacency_reading, money_governance_coupling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(adjacency_reading, protocol_wide_stakeholders).
narrative_ontology:constraint_beneficiary(adjacency_reading, small_token_holders).
narrative_ontology:constraint_beneficiary(adjacency_reading, core_developer_council).
narrative_ontology:constraint_victim(adjacency_reading, large_capital_holders).
narrative_ontology:constraint_victim(adjacency_reading, organized_delegate_blocs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(adjacency_reading, organized_delegate_blocs).
narrative_ontology:constraint_vindicates(adjacency_reading, separation_of_purse_and_vote_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and maintains the enumerated crossing points (the audited surfaces where treasury, surety, or clearing mechanisms are permitted to touch governance decisions) and defends the cap schedule against proposals to widen them. Cannot exit the design without abandoning the constitutional commitment itself.
narrative_ontology:constraint_stakeholder(adjacency_reading, core_developer_council, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from a governance layer that cannot be directly purchased outright; a holder with vastly more capital does not automatically receive proportional voice. They can exit the chain entirely if the adjacency principle is violated in practice, giving them real if costly leverage.
narrative_ontology:constraint_stakeholder(adjacency_reading, protocol_wide_stakeholders, beneficiary,
    organized, generational, mobile, global).

% Hold small amounts of the asset and would be structurally erased under a pure plutocratic coupling; the adjacency principle is the thing standing between them and irrelevance. Their exit option is technically available (sell and leave) but practically thin given limited capital and limited standing to organize elsewhere.
narrative_ontology:constraint_stakeholder(adjacency_reading, small_token_holders, beneficiary,
    powerless, biographical, constrained, global).

% Hold wealth on-chain that confers no direct proportional governance weight; must instead exercise influence through the enumerated, capped, audited crossings (funding proposals, staking to specific capped pools, treasury grant applications) rather than through raw balance-weighted voting. They retain the option to route capital elsewhere if the adjacency constraint is judged too costly, which gives them exit leverage the smaller holders lack.
narrative_ontology:constraint_stakeholder(adjacency_reading, large_capital_holders, payer,
    powerful, biographical, arbitrage, global).

% Coordinate votes and attention across many smaller holders' delegated shares; the objections literature (paper's own 8/9) concedes they retain real informal advantages — disproportionate attendance at governance calls, superior organizational capacity, easier proposal drafting — even though the ballot itself is formally uncoupled from wealth. They benefit from coordination capacity but pay a structural tax in that their capital cannot be converted directly into votes.
narrative_ontology:constraint_stakeholder(adjacency_reading, organized_delegate_blocs, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(adjacency_reading, organized_delegate_blocs, beneficiary).

% Independent bodies (formal audit committees, academic and watchdog analysts) that examine whether the enumerated crossings are actually capped and actually audited in practice, or whether informal wealth-derived influence is leaking through attendance, organization, and proposal-drafting advantages that the design concedes but does not fully close.
narrative_ontology:constraint_stakeholder(adjacency_reading, auditors_and_monitors, observer,
    institutional, generational, analytical, global).

% Advocates for the fusion reading (direct wealth-weighted voting) or the exile reading (total separation with no crossings at all) are not seated in the adjacency design's own governance process; they can only exit to a fork or a competing chain that implements their preferred coupling, rather than argue the point from within this constitution.
narrative_ontology:constraint_stakeholder(adjacency_reading, rival_coupling_advocates, excluded,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(adjacency_reading, diffuse).
narrative_ontology:fixing_cost_class(adjacency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem of preventing pure plutocratic capture of protocol governance while still allowing capital-intensive functions (treasury management, surety bonding, settlement clearing) to interface with governance where genuinely necessary, through named and capped channels rather than an open wealth-to-vote conversion.
% TRANSFER_FUNCTION: Formally, the arrangement transfers nothing from wealth to vote — that is its entire claim. Informally, it channels influence from large, organized capital toward governance outcomes via the conceded-but-uncapped surfaces of attendance, organizational capacity, and proposal drafting, which the design's own objections literature (8/9) acknowledges remain wealth-correlated.
% ABSENT_VOICES: Advocates of the fusion reading (explicit wealth-weighted voting, arguing capital-at-risk should govern capital-at-risk) and advocates of the exile reading (total separation, arguing any enumerated crossing is a crack that widens) are not represented within this constitution's own deliberative process — they exist only as external competitors or forkers.
% DISAPPEARANCE_RATIONALE: If the adjacency principle were removed, the enumerated/capped/audited crossings would either default to open wealth-weighted governance (collapsing into the fusion reading) or governance would have to sever all contact with treasury and surety mechanisms entirely (collapsing into the exile reading). Either direction is a structural reorganization of who governs and how capital-intensive functions are financed and controlled — this is not a cosmetic label.
% FOUNDING_PROBLEM: Early token-voting systems discovered that unrestricted wealth-to-vote conversion produced rapid plutocratic capture: whales bought governance outright, small holders became irrelevant, and protocol treasuries became extraction vehicles for whoever accumulated the most stake. The adjacency design was built to preserve capital's functional role (funding, surety, clearing) while formally decoupling it from the vote.
% FOUNDING_PROBLEM_CORROBORATION: The core developer council and protocol-wide stakeholder base attest the plutocratic-capture problem remains live and the adjacency principle is actively preventing recurrence. Independent auditors and academic governance analysts corroborate that direct vote-buying is largely absent, but note — consistent with the design's own conceded objections 8/9 — that informal wealth-correlated advantages in attendance, organization, and proposal access persist and are not fully audited, which large capital holders themselves point to as evidence the adjacency is more formal than substantive.
narrative_ontology:disappearance_verdict(adjacency_reading, world_rearranges).
narrative_ontology:founding_problem_status(adjacency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(adjacency_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-23',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(adjacency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(adjacency_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(adjacency_reading_tests).
:- end_tests(adjacency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28 at interval end) because the design's central claim — no direct wealth-to-vote conversion — appears to hold structurally; the crossings are genuinely enumerated and capped rather than open conduits. But it is not zero, and it rises modestly over the interval, because the conceded informal advantages (attendance, organization, proposal access) are a real, if diffuse, channel through which capital-correlated influence accumulates over time as organized blocs learn to exploit the edges of the audited surfaces. Suppression is moderate (0.35): the design requires active constitutional maintenance (constant vigilance against proposals to widen the crossings) but does not require coercive exclusion of alternatives — holders can exit to competing chains. Theater ratio is low-moderate (0.22) reflecting that the audit function is mostly substantive but carries some genuine performative element in how compliance with the caps is publicly reported.
 *
 * PERSPECTIVAL GAP:
 *   From the core developer council's seat, this is coordination succeeding at its stated goal: preventing plutocratic capture while preserving capital's legitimate functional role. From the organized delegate blocs' seat, the same structure is experienced as a tax on their coordination capacity — they can influence outcomes but only through channels less direct and more costly than raw balance-weighted voting would be, which the engine should register as low-to-moderate extraction rather than either pure coordination or pure extraction. From the excluded rival-coupling advocates' seat, the entire adjacency framework is itself a contested choice made without their input, visible only as a fork-or-leave decision from outside.
 *
 * DIRECTIONALITY LOGIC:
 *   Small token holders and protocol-wide stakeholders are declared beneficiaries because the adjacency principle's entire function is to protect their voice from being priced out by concentrated capital — they get low d, benefiting from the constraint's operation. Large capital holders and organized delegate blocs are declared payers because they bear the structural cost of not being able to convert capital directly into governance weight, even though they retain some informal influence — they get elevated d, though not maximal, since their exit options (arbitrage for large holders, coordination capacity for blocs) partially offset the constraint's bite. The core developer council sits as agenda_setter rather than beneficiary because it administers the crossings without personally capturing extraction from them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (plutocratic capture via unrestricted wealth-to-vote conversion) remains structurally live in the sense that the underlying incentive to convert capital into governance weight has not disappeared — it has only been rerouted into the conceded informal channels. This is not mandatrophy in the classic sense (the mandate has not become fully obsolete while the institution persists); rather, the design is authored as a live, partially successful containment of an ongoing pressure, with declared residual leakage. The tangled_rope classification (rather than rope) reflects that a coordination function (preventing plutocratic capture) genuinely coexists with asymmetric cost-bearing (organized capital pays a structural tax relative to what raw wealth-weighted voting would give it) requiring active enforcement (the developer council's ongoing defense of the cap schedule) — exactly the hybrid the tangled_rope gate is designed to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empty_victim_set_claim,
    'Is the adjacency design''s claim that no stakeholder class accumulates permanent structural advantage actually true, or does the conceded informal-advantage residue (attendance, organization, proposal access) constitute a de facto victim set that the formal design fails to capture?',
    'Longitudinal audit of governance outcomes correlated with delegate-bloc capital size and organizational capacity, compared against a counterfactual baseline of purely random or capital-blind participation; if outcomes systematically favor organized capital beyond what participation-rate alone would predict, the empty-victim-set claim fails empirically.',
    'If the residue is substantial and growing, this story''s tangled_rope classification would need to shift toward a more extractive reading, potentially converging with aspects of the fusion reading''s outcome even while formally retaining the adjacency reading''s structure — a case of practice drift undermining the codified kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empty_victim_set_claim, empirical, 'Whether the design''s claimed empty victim set survives contact with the conceded informal-advantage residue.').

omega_variable(
    sibling_reading_foreclosure_pressure,
    'Does the adjacency reading''s persistence and apparent partial success create downstream pressure that makes the fusion reading harder to argue for, or does documented leakage at the conceded edges create pressure toward the exile reading''s stricter separation?',
    'Track constitutional amendment proposals and fork rationales across chains implementing each reading; if forks trend toward tightening crossings (exile-ward) rather than loosening them (fusion-ward), this indicates influence rather than mere coexistence.',
    'Determines whether the reading_relations should be characterized as pure coexistence or whether one direction of influence should be strengthened in future revisions of this story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_pressure, conceptual, 'Whether the adjacency reading''s operation structurally pressures the sibling readings rather than merely coexisting with them.').

omega_variable(
    audit_capture_risk,
    'Are the auditors and monitors genuinely independent, or could the audit function itself become captured by the same organized capital that benefits from the conceded informal-advantage channels, thereby laundering informal influence as certified compliance?',
    'Examine funding sources and appointment processes for the audit bodies; cross-check audit findings against independent academic analysis for divergence.',
    'If audit capture is occurring, the theater_ratio is understated and the design''s actual extraction is higher than the audited figures suggest — this would push the classification toward snare at the audit layer specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(audit_capture_risk, empirical, 'Whether the audit function that certifies the caps are holding is itself vulnerable to the same capture it is meant to detect.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(adjacency_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(adja_tr_t0, adjacency_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(adja_tr_t4, adjacency_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(adja_tr_t8, adjacency_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(adja_tr_t12, adjacency_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(adja_tr_t16, adjacency_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(adja_tr_t20, adjacency_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(adja_tr_t24, adjacency_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(adja_be_t0, adjacency_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(adja_be_t4, adjacency_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement(adja_be_t8, adjacency_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(adja_be_t12, adjacency_reading, base_extractiveness, 12, 0.24).
narrative_ontology:measurement(adja_be_t16, adjacency_reading, base_extractiveness, 16, 0.26).
narrative_ontology:measurement(adja_be_t20, adjacency_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(adja_be_t24, adjacency_reading, base_extractiveness, 24, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(adja_su_t0, adjacency_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(adja_su_t4, adjacency_reading, suppression_requirement, 4, 0.31).
narrative_ontology:measurement(adja_su_t8, adjacency_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(adja_su_t12, adjacency_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(adja_su_t16, adjacency_reading, suppression_requirement, 16, 0.34).
narrative_ontology:measurement(adja_su_t20, adjacency_reading, suppression_requirement, 20, 0.345).
narrative_ontology:measurement(adja_su_t24, adjacency_reading, suppression_requirement, 24, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(adjacency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(adjacency_reading, 0.12).
narrative_ontology:affects_constraint(adjacency_reading, fusion_reading).
narrative_ontology:affects_constraint(adjacency_reading, exile_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'money/governance coupling in constitutional blockchain design' per the epsilon-invariance principle. fusion_reading (direct wealth-weighted voting) and exile_reading (total separation, no crossings) are separate constraint stories with their own epsilon values, beneficiary/victim structures, and classifications. Do not average across these three; each is a distinct structural claim about how capital and governance relate, and each should be evaluated independently before any cross-reading comparison is drawn.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
