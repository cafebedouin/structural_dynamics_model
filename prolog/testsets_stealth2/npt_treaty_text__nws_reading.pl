% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nws_reading, []).

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
 *   constraint_id: npt_treaty_text__nws_reading
 *   human_readable: NPT Regime as Read by Nuclear Weapon States: Binding Restraint on NNWS, Discretionary Reciprocity for NWS
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   The Non-Proliferation Treaty is a single persisting commitment (kernel:
 *   npt_treaty_text) that different parties read differently; this file
 *   instantiates ONE reading — the nws_reading — as a clean,
 *   epsilon-invariant constraint. Under this reading, the treaty's operative
 *   content is: Articles II/III bind NNWS to permanent renunciation and
 *   full-scope safeguards with hard enforcement (IAEA inspection, UNSC
 *   referral, sanctions), while Article VI's disarmament commitment ('to
 *   pursue negotiations in good faith ... at an early date') is programmatic
 *   — real in intent, empty in deadline, and backed by no enforcement
 *   machinery whatsoever. The standing arrangement under contest — the
 *   referent for every metric below — is the regime as actually operated
 *   under this reading since entry into force (interval year 0 = 1970; year
 *   55 = 2025). Epsilon is authored for THAT arrangement, not for any
 *   alternative regime this or any sibling reading would install. The regime
 *   retains a genuine coordination core (proliferation-cascade prevention,
 *   transparency) while running a large asymmetric transfer through the same
 *   structure: NNWS pay permanent restraint, inspection burden, and foregone
 *   deterrence; NWS collect arsenal retention, extended-deterrence patronage,
 *   and interpretive control of the reciprocity clause. The claim/metric
 *   relationship is deliberately unreconciled: the reading CLAIMS
 *   tangled_rope and the metrics independently describe substantially
 *   extractive, actively enforced operation — the engine measures any
 *   divergence. KEY AGENTS (by structural relationship): - nws_coalition:
 *   Agenda-setter and primary beneficiary (institutional/arbitrage) — writes
 *   the interpretive settlement, holds the UNSC veto over enforcement,
 *   retains arsenals - compliant_nnws: Primary target (organized/trapped) —
 *   bears permanent restraint, safeguards burden, foregone deterrent option -
 *   extended_deterrence_umbrella_states: Secondary beneficiary with payer
 *   residue (powerful/constrained) — protected by NWS arsenals while carrying
 *   NNWS obligations - iaea_secretariat: Institutional beneficiary
 *   (institutional/constrained) — budget and mandate concentrate on
 *   horizontal verification - threshold_armed_states: Excluded party
 *   (powerful/arbitrage) — stayed outside, armed anyway, cited as evidence of
 *   selective enforcement - tpnw_coalition_states: Excluded voice
 *   (organized/constrained) — formally present at Review Conferences,
 *   structurally unable to move operative decisions -
 *   arms_control_verification_analysts: Analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nws_reading, 0.74).
domain_priors:suppression_score(npt_treaty_text__nws_reading, 0.65).
domain_priors:theater_ratio(npt_treaty_text__nws_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nws_reading, "NPT Regime as Read by Nuclear Weapon States: Binding Restraint on NNWS, Discretionary Reciprocity for NWS").
narrative_ontology:topic_domain(npt_treaty_text__nws_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__nws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nws_reading, '88c79731-406c-4c2d-a353-a273b6aae1e3').
narrative_ontology:cs_kernel_codification('88c79731-406c-4c2d-a353-a273b6aae1e3', fixed_text).
narrative_ontology:cs_authority_grounding('88c79731-406c-4c2d-a353-a273b6aae1e3', extraction).
narrative_ontology:cs_interpretation_layer_present('88c79731-406c-4c2d-a353-a273b6aae1e3').
narrative_ontology:cs_reading_relation('88c79731-406c-4c2d-a353-a273b6aae1e3', npt_treaty_text__nnws_reading, forecloses).
narrative_ontology:cs_reading_relation('88c79731-406c-4c2d-a353-a273b6aae1e3', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('88c79731-406c-4c2d-a353-a273b6aae1e3', foundational, article_vi_programmatic_not_justiciable).
narrative_ontology:cs_axiom_status(article_vi_programmatic_not_justiciable, holdable).
narrative_ontology:cs_axiom_grounding('88c79731-406c-4c2d-a353-a273b6aae1e3', article_vi_programmatic_not_justiciable, conventional).
narrative_ontology:cs_axiom('88c79731-406c-4c2d-a353-a273b6aae1e3', foundational, proliferation_prevention_outranks_symmetry).
narrative_ontology:cs_axiom_status(proliferation_prevention_outranks_symmetry, holdable).
narrative_ontology:cs_axiom_grounding('88c79731-406c-4c2d-a353-a273b6aae1e3', proliferation_prevention_outranks_symmetry, instrumental).
narrative_ontology:cs_reference_frame('88c79731-406c-4c2d-a353-a273b6aae1e3', programmatic_disarmament_compact).
narrative_ontology:cs_drift_state('88c79731-406c-4c2d-a353-a273b6aae1e3', post_tpnw_contemporary, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('88c79731-406c-4c2d-a353-a273b6aae1e3', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nws_coalition).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, extended_deterrence_umbrella_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, iaea_secretariat).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, compliant_nnws).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, extended_deterrence_umbrella_states).
narrative_ontology:constraint_vindicates(npt_treaty_text__nws_reading, nuclear_deterrence_stability_doctrine).
narrative_ontology:constraint_vindicates(npt_treaty_text__nws_reading, horizontal_proliferation_priority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five states that possessed nuclear weapons before the treaty and never accepted its restraints on themselves. They interpret Article VI as discretionary, hold vetoes over Security Council enforcement, steer IAEA priorities as major contributors, and modernize arsenals while demanding universal restraint. Nothing external compels their performance; the interpretive settlement is theirs to maintain or renegotiate.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nws_coalition, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, nws_coalition, beneficiary).

% More than 180 states accepting full-scope safeguards, intrusive inspection, and permanent renunciation of a weapons option. They receive peaceful-cooperation commitments that arrive unevenly and a disarmament process with no deadline. Leaving is demonstrated to be ruinous: the one state that withdrew faced sanctions and isolation, and the supplier cartel conditions all nuclear trade on regime membership. They act collectively in blocs but cannot move operative decisions.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, compliant_nnws, payer,
    organized, generational, trapped, global).

% Allies in Europe and East Asia protected by NWS arsenals they do not own. They gain security without weapons-program costs, but as NNWS they also carry safeguards obligations and hosting burdens, and their security is hostage to the patron's continued arsenal retention — the very thing the regime's disarmament clause nominally retires. Exiting the umbrella means arming under the regime's harshest penalties or accepting reduced protection.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, extended_deterrence_umbrella_states, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, extended_deterrence_umbrella_states, payer).

% Implements the verification machinery. Its budget, staffing, and technical development concentrate overwhelmingly on detecting covert programs in NNWS; disarmament verification remains pilot-scale with no comparable mandate. The agency grows with the horizontal mission and does not set the allocation it administers.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, iaea_secretariat, beneficiary,
    institutional, generational, constrained, global).

% States that never joined the treaty and built arsenals outside it. They face supplier-group restrictions but no treaty obligations and no inspection regime. Their existence is the standing exhibit NNWS cite that enforcement is selective — the rules bound those inside while the armed outsiders remained untouched.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, threshold_armed_states, excluded,
    powerful, generational, arbitrage, regional).

% A bloc of NNWS pursuing the Humanitarian Initiative and the 2017 prohibition treaty. They attend every Review Conference and table proposals for enforceable disarmament timelines, but operative consensus requires the NWS, so their core demand never enters the decided text. Their remedy operates in a parallel instrument the regime's beneficiaries refuse to join.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, tpnw_coalition_states, excluded,
    organized, generational, constrained, global).

% Academic and policy analysts tracking compliance records, safeguards budgets, and interpretive drift across the interval. They publish outside the negotiation rooms and have documented the widening gap between the regime's reciprocity language and its enforcement practice.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, arms_control_verification_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nws_reading, nws_coalition).
narrative_ontology:fixing_cost_class(npt_treaty_text__nws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents proliferation cascades and provides mutual transparency: states facing the prospect of dozens of new nuclear powers coordinate on a single verified restraint norm, with a shared inspection apparatus converting suspicion about programs into auditable fact.
% TRANSFER_FUNCTION: Moves permanent restraint (foregone weapons options), sovereignty (inspection access), and verification costs from NNWS into the collective regime, while moving security goods — arsenal retention, extended-deterrence patronage, and control over what the reciprocity clause means — to the NWS and their protégés.
% ABSENT_VOICES: Threshold armed states sit outside the treaty entirely; downwind and test-affected populations (Marshall Islands communities, Semipalatinsk region residents) have no seat anywhere in the process despite bearing its historical costs; TPNW-supporting states are physically present but structurally unable to place enforceable timelines on the operative agenda.
% DISAPPEARANCE_RATIONALE: If the regime vanished overnight, the supplier cartel loses its legal anchor, the inspection apparatus dissolves, latent-capable states reassess hedging within months, and alliance structures built on extended deterrence lose their non-proliferation justification — the security architecture of roughly 190 states reorganizes around whatever bilateral arrangements rush in to fill the vacuum.
% FOUNDING_PROBLEM: In the 1960s the forecast was twenty-five nuclear powers within a decade; the treaty was built to freeze the club near its existing five members while committing the armed five to stop the arms race and negotiate disarmament — a two-sided problem written into one text.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the ICJ's 1996 advisory opinion (unanimous on the existence of an obligation to pursue negotiations), SIPRI arsenal and modernization time series, UNIDIR and academic verification studies, and the NNWS-bloc working papers across Review Conference records all attest that the non-proliferation half remains live while the disarmament half has no operative schedule. The NWS attest only the non-proliferation half; no source inside the beneficiary set attests the disarmament half is live, and none outside disputes that finding.
narrative_ontology:disappearance_verdict(npt_treaty_text__nws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nws_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_text__nws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nws_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nws_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__nws_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__nws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74 at interval end) because the transfer is decoupled from performance: the NNWS side of the bargain is fully specified and fully enforced, the NWS side is unspecified and unenforced, and the gap has widened monotonically — 1995 indefinite extension extracted permanence from NNWS in exchange for disarmament steps (CTBT, FMCT, 'principled and systematic' process) that were subsequently abandoned one by one, while arsenals modernized. Suppression (0.65) is a raw structural property of the constraint, unscaled by power or scope in the engine's computation: it reflects the real coercive closure of NNWS alternatives (DPRK withdrawal triggering sanctions, NSG supply conditioning, Additional Protocol intrusion) — and it is deliberately lower than extractiveness because the regime's hold on NNWS relies substantially on locked-in membership rather than continuous active coercion. Theater ratio (0.55) tracks the growing share of regime activity that is performative: Review Conference consensus theater, 'unequivocal undertaking' language, P5 process statements — activity that reproduces the appearance of reciprocity without constraining arsenals — while functional activity concentrates almost entirely on horizontal safeguards. Accessibility_collapse (0.55) is moderate: alternatives do not vanish (threshold states stayed out and armed; DPRK exited) but they collapse sharply for states already inside the supplier-controlled system. Resistance (0.58) is real and recurring — NAM bloc pressure, the Humanitarian Initiative, the 2017 TPNW — but has not altered the operative structure, which is itself diagnostic of enforced asymmetry. The measurement series run on ONE shared time grid ({0,10,20,30,40,50,55}) with every tracked metric authored at every point; the trajectories are monotonic, not cyclical — the drivers (indefinite extension, Additional Protocol, collapse of the 13 practical steps, TPNW counter-mobilization, New START lapse) accumulate rather than oscillate, so no intermittent-reinforcement dynamic is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the agenda-setter seat compute very different types from the same structural data. From the nws_coalition seat, the arrangement is a legitimate security public good it underwrites: it enforces restraint it considers essential, interprets its own obligations as conditional on a security environment no one else controls, and experiences the regime as successful coordination it built. From the compliant_nnws seat, the same structure operates as enforced extraction: a permanent, inspected renunciation exchanged for a reciprocity clause whose only enforcement mechanism is rhetorical. The umbrella states sit between — genuine security beneficiaries who nonetheless carry payer obligations and watch the patron's arsenal grow. The engine derives this per-seat divergence from the power/exit/role data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   The nws_coalition declares as beneficiary with arbitrage-grade exit: it wrote the interpretive settlement, faces no external compulsion, and can renegotiate what it dislikes — this places it nearest the beneficiary end (low d, damped or inverted effective extraction). The compliant_nnws declare as victims with trapped exit: withdrawal is demonstrably punished and the supplier system is conditioned on membership — this places them nearest the target end (high d, amplified effective extraction, further amplified by the regime's global scope making verification selectively expensive for the inspected). The extended_deterrence_umbrella_states carry a dual declaration (beneficiary with payer residue): they receive the security good without owning its costs but also carry NNWS safeguards burdens, placing them mid-low on the target axis. The iaea_secretariat collects budget and mandate growth (beneficiary-direction) but bears mandate risk and no discretion over the allocation it administers, keeping it well short of capturer status. Threshold and TPNW seats are excluded rather than coordinated — their exclusion is part of what the enforcement asymmetry maintains. No directionality_overrides are authored: the beneficiary/victim declarations plus exit options already differentiate every seat the derivation needs to distinguish, and a power-atom-keyed override would misfire across same-power seats with different structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem had two halves: stop the forecast proliferation cascade (live — dozens of latent weapons states were predicted; the regime helped hold the count near the original five-plus-outliers) and halt the arms race toward disarmament (dead in operative terms — no timeline has ever been met, arsenals are modernizing, and the last bilateral ceiling agreement is lapsing). Because the founding problem is CONTESTED rather than dead, and disappearance_verdict is world_rearranges (supplier controls, verification infrastructure, and restraint expectations all depend on the regime), the mismatch consumer finds no zombie flag: this is not a piton coasting on a vanished mandate. The classification discipline cuts both ways: reading the regime as pure snare would erase the genuine collective-action good (cascade prevention) that gives the extraction its cover and its durability; reading it as pure rope would erase the enforced, widening asymmetry that distinguishes it from voluntary standards. Tangled_rope is the structural truth this reading generates: coordination function present, extraction asymmetric, enforcement active. The mandatrophy risk to watch is the reverse direction — if the non-proliferation half also decays (verification failure cascade, withdrawal contagion), the regime converts to piton: theatrical Review Conferences administering a constraint nobody can profit from maintaining and nobody is hurt enough to fix.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_legal_status,
    'Is Article VI a binding legal obligation with determinate content (the nnws_reading) or a programmatic aspiration without enforcement (this reading)? This constraint is ONE reading of kernel npt_treaty_text; the sibling reading assigns the opposite legal status to the same clause.',
    'Evolution of ICJ jurisprudence and VCLT good-faith interpretation doctrine applied to the negotiating record (ENDC travaux, 1995 Extension Conference decision record); a court or Review Conference ruling that Article VI carries justiciable content would resolve it.',
    'If Article VI is binding-with-content, the NWS become persistent violators rather than discretionary agenda-setters, the victim/beneficiary structure inverts around breach rather than asymmetry, and effective extraction rises further; if aspirational, this tangled_rope classification stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_legal_status, conceptual, 'Committer omega: which reading of the NPT kernel governs Article VI''s legal force.').

omega_variable(
    early_date_drafting_intent,
    'Was the ''at an early date'' vagueness in Article VI a deliberate constructive ambiguity designed to permit indefinite non-performance, or a necessary drafting compromise to secure ratification by both blocs?',
    'Declassified ENDC negotiating records, delegations'' internal instructions, and contemporaneous diplomatic correspondence comparing drafted alternatives.',
    'Designed ambiguity supports a snare-leaning reading (coordination language as cover from inception); necessity compromise supports genuine tangled_rope (real bargain degraded later by drift).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(early_date_drafting_intent, empirical, 'Whether the load-bearing ambiguity was engineered extraction or bargaining cost.').

omega_variable(
    safeguards_allocation_politics,
    'Is the concentration of IAEA verification resources on horizontal proliferation (NNWS programs) a technical necessity — counting warheads is easier than detecting covert programs — or a politically steered allocation reflecting NWS influence over the agency?',
    'Comparative budget analysis of safeguards versus disarmament-verification line items over time, cross-referenced with Board of Governors voting patterns and major-contributor earmarking.',
    'Political steering confirms the extraction channel runs through the verification apparatus itself; technical necessity would relocate part of the measured asymmetry to genuine cost differences, lowering effective extraction attributable to interpretive control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safeguards_allocation_politics, empirical, 'Whether the verification-budget asymmetry is technical or captured.').

omega_variable(
    withdrawal_threshold_coupling,
    'How does the sibling withdrawal_threshold_reading interact with this reading''s stability — does a high Article X threshold (regime-stability priority) reinforce the NWS reading''s enforcement asymmetry by closing the exit that would otherwise discipline NWS interpretive control?',
    'Counterfactual analysis of NNWS bargaining leverage under low-versus-high withdrawal thresholds, using the DPRK withdrawal episode and threatened withdrawals as natural experiments.',
    'If the high threshold is load-bearing for this reading, liberalizing exit would convert part of the measured suppression into credible bargaining leverage for NNWS and could shift the classification toward a more symmetric tangled_rope; if not, the asymmetry survives exit reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_threshold_coupling, conceptual, 'Structural coupling between this reading and the Article X threshold sibling.').

omega_variable(
    deterrence_stability_warrant,
    'Does continued NWS arsenal retention actually produce the strategic stability that this reading cites to justify asymmetric enforcement, or does modernization and arms-race recurrence undercut the instrumental warrant?',
    'Systematic review of crisis-instability literature, near-miss records, and arms-race time series across the interval.',
    'If the stability warrant fails empirically, the reading''s instrumental axiom loses its grounding, the vindicated proposition loses its evidentiary base, and the coordination-side justification for the asymmetry weakens — pushing the classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_stability_warrant, empirical, 'Empirical status of the deterrence-stability claim underwriting the asymmetry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nws_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_nws_reading_tr_t0, npt_treaty_text__nws_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(npt_nws_reading_tr_t0, observed).
narrative_ontology:measurement(npt_nws_reading_tr_t10, npt_treaty_text__nws_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(npt_nws_reading_tr_t10, observed).
narrative_ontology:measurement(npt_nws_reading_tr_t20, npt_treaty_text__nws_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement_basis(npt_nws_reading_tr_t20, observed).
narrative_ontology:measurement(npt_nws_reading_tr_t30, npt_treaty_text__nws_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(npt_nws_reading_tr_t30, observed).
narrative_ontology:measurement(npt_nws_reading_tr_t40, npt_treaty_text__nws_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement_basis(npt_nws_reading_tr_t40, observed).
narrative_ontology:measurement(npt_nws_reading_tr_t50, npt_treaty_text__nws_reading, theater_ratio, 50, 0.5).
narrative_ontology:measurement_basis(npt_nws_reading_tr_t50, observed).
narrative_ontology:measurement(npt_nws_reading_tr_t55, npt_treaty_text__nws_reading, theater_ratio, 55, 0.55).
narrative_ontology:measurement_basis(npt_nws_reading_tr_t55, observed).

% Extraction over time
narrative_ontology:measurement(npt_nws_reading_be_t0, npt_treaty_text__nws_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(npt_nws_reading_be_t0, observed).
narrative_ontology:measurement(npt_nws_reading_be_t10, npt_treaty_text__nws_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(npt_nws_reading_be_t10, observed).
narrative_ontology:measurement(npt_nws_reading_be_t20, npt_treaty_text__nws_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(npt_nws_reading_be_t20, observed).
narrative_ontology:measurement(npt_nws_reading_be_t30, npt_treaty_text__nws_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(npt_nws_reading_be_t30, observed).
narrative_ontology:measurement(npt_nws_reading_be_t40, npt_treaty_text__nws_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement_basis(npt_nws_reading_be_t40, observed).
narrative_ontology:measurement(npt_nws_reading_be_t50, npt_treaty_text__nws_reading, base_extractiveness, 50, 0.72).
narrative_ontology:measurement_basis(npt_nws_reading_be_t50, observed).
narrative_ontology:measurement(npt_nws_reading_be_t55, npt_treaty_text__nws_reading, base_extractiveness, 55, 0.74).
narrative_ontology:measurement_basis(npt_nws_reading_be_t55, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt_nws_reading_su_t0, npt_treaty_text__nws_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(npt_nws_reading_su_t0, observed).
narrative_ontology:measurement(npt_nws_reading_su_t10, npt_treaty_text__nws_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(npt_nws_reading_su_t10, observed).
narrative_ontology:measurement(npt_nws_reading_su_t20, npt_treaty_text__nws_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(npt_nws_reading_su_t20, observed).
narrative_ontology:measurement(npt_nws_reading_su_t30, npt_treaty_text__nws_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(npt_nws_reading_su_t30, observed).
narrative_ontology:measurement(npt_nws_reading_su_t40, npt_treaty_text__nws_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(npt_nws_reading_su_t40, observed).
narrative_ontology:measurement(npt_nws_reading_su_t50, npt_treaty_text__nws_reading, suppression_requirement, 50, 0.63).
narrative_ontology:measurement_basis(npt_nws_reading_su_t50, observed).
narrative_ontology:measurement(npt_nws_reading_su_t55, npt_treaty_text__nws_reading, suppression_requirement, 55, 0.65).
narrative_ontology:measurement_basis(npt_nws_reading_su_t55, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, nnws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, withdrawal_threshold_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the NPT grand bargain' covers multiple structurally distinct claims and is decomposed per the epsilon-invariance principle into a constraint family linked by network edges. This file is the nws_reading (epsilon 0.74, tangled_rope: enforced asymmetry with a live coordination core). The nnws_reading is a separate constraint assigning opposite legal status to Article VI — its epsilon for the same referent is authored from the breached-reciprocity reading and its victim/beneficiary structure differs. The withdrawal_threshold_reading isolates the Article X exit question, which this reading treats as settled in favor of regime stability. Family members cite one another: NWS interpretive control upstream shapes the withdrawal-threshold battleground downstream, and the nnws_reading's breach claim draws its force from the asymmetry this reading operationalizes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
