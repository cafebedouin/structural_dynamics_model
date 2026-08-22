% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__dropping_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__dropping_reading, []).

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
 *   constraint_id: total_war_reachability_boundary__dropping_reading
 *   human_readable: Total War Reachability Boundary — Dropping Reading (Deterrence as Maintained Coordination Equilibrium)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This story instantiates the DROPPING reading of the
 *   total_war_reachability_boundary kernel: total war fell sharply in
 *   probability after the consolidation of mutual vulnerability, but it
 *   remains reachable — the boundary is a managed, actively maintained
 *   coordination equilibrium, not a feature of nature and not an irreversible
 *   contraction of the feasible set. The referent for epsilon is the standing
 *   deterrence arrangement itself (arsenals, alert postures, alliance
 *   umbrellas, command machinery) as this reading assesses it: a structure
 *   that solves a real collective problem for the great powers while
 *   transferring existential risk onto populations who never consented and
 *   fiscal resources into a protected industrial base. The sibling readings —
 *   contraction_reading (winnable total war left the feasible set entirely)
 *   and contingent_reachability_reading (reachability is technology-dependent
 *   and the current lull is reversible inertia) — are separate constraint
 *   stories with their own epsilon values and are linked via
 *   network.affects_constraints; nothing about them is averaged into this
 *   file.
 *
 * KEY AGENTS:
 *   - nuclear_weapons_states: agenda setter and principal beneficiary (institutional/identity_locked) — administers the equilibrium, collects credibility, budgets, and status; partially self-exposed through mutual targeting
 *   - populations_under_nuclear_threat: primary target (powerless/trapped) — bears existential risk on all sides without consent, including inside the nuclear states
 *   - defense_industrial_contractors: secondary beneficiary (powerful/mobile) — collects the fiscal flow, retains portfolio mobility
 *   - extended_deterrence_allies: sheltered beneficiary (organized/constrained) — buys security below cost, pays in basing and exposure
 *   - downwind_test_communities: historical target (powerless/trapped) — already-incurred harm, partial compensation
 *   - anti_nuclear_movements: excluded challenger (organized/constrained) — won an instrument the principals boycott
 *   - nonnuclear_npt_signatories: bargain payer (organized/constrained) — surrendered options against a deferred promise
 *   - deterrence_theorists: analytical observer (analytical/analytical) — models the structure, bears none of it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, 0.58).
domain_priors:suppression_score(total_war_reachability_boundary__dropping_reading, 0.62).
domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__dropping_reading, tangled_rope).
narrative_ontology:human_readable(total_war_reachability_boundary__dropping_reading, "Total War Reachability Boundary — Dropping Reading (Deterrence as Maintained Coordination Equilibrium)").
narrative_ontology:topic_domain(total_war_reachability_boundary__dropping_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__dropping_reading, 'd769f872-050e-4fca-ada2-8ec7efe6760d').
narrative_ontology:cs_kernel_codification('d769f872-050e-4fca-ada2-8ec7efe6760d', distributed).
narrative_ontology:cs_authority_grounding('d769f872-050e-4fca-ada2-8ec7efe6760d', distributed).
narrative_ontology:cs_reading_relation('d769f872-050e-4fca-ada2-8ec7efe6760d', total_war_reachability_boundary__contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('d769f872-050e-4fca-ada2-8ec7efe6760d', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_axiom('d769f872-050e-4fca-ada2-8ec7efe6760d', foundational, total_war_probability_dropped_not_eliminated).
narrative_ontology:cs_axiom_status(total_war_probability_dropped_not_eliminated, holdable).
narrative_ontology:cs_axiom_grounding('d769f872-050e-4fca-ada2-8ec7efe6760d', total_war_probability_dropped_not_eliminated, empirically_contingent).
narrative_ontology:cs_axiom('d769f872-050e-4fca-ada2-8ec7efe6760d', foundational, deterrence_equilibrium_is_actively_maintained).
narrative_ontology:cs_axiom_status(deterrence_equilibrium_is_actively_maintained, holdable).
narrative_ontology:cs_axiom_grounding('d769f872-050e-4fca-ada2-8ec7efe6760d', deterrence_equilibrium_is_actively_maintained, empirically_contingent).
narrative_ontology:cs_axiom('d769f872-050e-4fca-ada2-8ec7efe6760d', secondary, existential_risk_transfer_sustains_credibility).
narrative_ontology:cs_axiom_status(existential_risk_transfer_sustains_credibility, holdable).
narrative_ontology:cs_axiom_grounding('d769f872-050e-4fca-ada2-8ec7efe6760d', existential_risk_transfer_sustains_credibility, instrumental).
narrative_ontology:cs_reference_frame('d769f872-050e-4fca-ada2-8ec7efe6760d', deterred_but_reachable_coordination_equilibrium).
narrative_ontology:cs_drift_state('d769f872-050e-4fca-ada2-8ec7efe6760d', contemporary_multipolar_erosion, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('d769f872-050e-4fca-ada2-8ec7efe6760d', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, defense_industrial_contractors).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, extended_deterrence_allies).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, populations_under_nuclear_threat).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, downwind_test_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, nonnuclear_npt_signatories).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__dropping_reading, rational_deterrence_theory).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__dropping_reading, mutual_vulnerability_stability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain survivable arsenals, alert postures, and command-and-control machinery; issue declaratory policy; hold launch authority. Collect budget shares, diplomatic weight, and alliance leadership premised on arsenal stewardship. Their own cities sit under rival targeting, though continuity-of-government planning shelters the decision-making layer specifically. Relinquishing the arsenal would mean surrendering great-power standing built over decades — the weapons and the status have grown into each other.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nuclear_weapons_states, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__dropping_reading, nuclear_weapons_states, beneficiary).

% Build and maintain delivery systems, warheads, and command infrastructure under cost-plus and modernization contracts. Revenue tracks threat assessments and modernization cycles. Pivot toward conventional or space portfolios is possible, but the nuclear account is a stable, politically protected revenue floor.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, defense_industrial_contractors, beneficiary,
    powerful, biographical, mobile, continental).

% Trade host-basing and alignment for shelter under another state's arsenal, gaining security without bearing full weapons-program costs, and accepting that their territory hosts forward targets. Leaving the umbrella means indigenous weapons programs or accommodation with rivals — both generation-length projects.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, extended_deterrence_allies, beneficiary,
    organized, biographical, constrained, regional).

% Live in targeted zones on every side of every rivalry, including inside the nuclear states themselves. Hostage status was never offered to a ballot. Exit means relocating cities, which changes targeting lists only marginally; the threat follows the geography of industrial civilization and passes to children.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, populations_under_nuclear_threat, payer,
    powerless, generational, trapped, global).

% Lived near atmospheric and underground test sites — Nevada, Semipalatinsk, the Pacific atolls — absorbing fallout-related illness, contamination, and displacement. Compensation regimes arrived late and partial. The exposure is already incurred; exit is retrospective and incomplete.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, downwind_test_communities, payer,
    powerless, biographical, trapped, regional).

% Organize for abolition and non-use instruments; won a prohibition treaty that the arsenal states boycott. Operate outside the councils where posture and employment decisions are made; influence arrives indirectly through public opinion, litigation, and coalition politics.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, anti_nuclear_movements, excluded,
    organized, generational, constrained, global).

% Forgoed weapons options under a bargain trading nonproliferation for eventual disarmament; attend review cycles where the disarmament ledger is perennially deferred. Bear umbrella dependence or unprotected proximity to rival arsenals in the meantime.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nonnuclear_npt_signatories, payer,
    organized, generational, constrained, global).

% Model crisis stability, escalation ladders, and signaling; produce the analyses that posture debates cite. Hold no launch authority and sit under no targeting; their stake is reputational and intellectual.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, deterrence_theorists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__dropping_reading, nuclear_weapons_states).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__dropping_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the recurring great-power problem of preventive-war incentive and crisis escalation: mutual recognized vulnerability makes initiating total war irrational for every party simultaneously, converting a recurring war-or-peace decision into a standing mutual-restraint equilibrium that all major rivals can verify well enough to rely on.
% TRANSFER_FUNCTION: Moves existential risk onto civilian populations on all sides (consent never sought), fiscal resources from publics to arsenal maintenance and the defense industrial base, and survival decision-rights to a handful of command authorities; returns security rents and status to the nuclear states and shelter to allied protectorates.
% ABSENT_VOICES: The populations under threat — including citizens of the nuclear states themselves — hold no seat; no electorate ever voted to be a hostage, and command structures are designed to exclude them from the decision they bear. Anti-nuclear movements and non-nuclear NPT signatories demanding the disarmament leg are formally audible but structurally sidelined: their instrument exists, their demand is deferred indefinitely.
% DISAPPEARANCE_RATIONALE: If the deterrence arrangement vanished overnight, great-power crisis behavior would rearrange around rearmament races, war-fighting doctrines, and renewed preventive-war incentives; alliances built on the umbrella would dissolve or re-arm; budgets, basing maps, and command structures across every continent presuppose it. Nothing about the current great-power order survives its removal intact.
% FOUNDING_PROBLEM: Prevent the recurrence of industrial-scale total war after 1945 — first attempted through monopoly and counterforce denial, then consolidated as mutual vulnerability once both blocs acquired survivable second-strike forces.
% FOUNDING_PROBLEM_CORROBORATION: Declassified crisis archives (ExComm recordings, Soviet Politburo records) and the working consensus of Cold War historians outside any single government attest that the founding problem — recurrent great-power crisis with total-war escalatory potential — was real and remains live; adversary-side archives independently corroborate the mutual fear. Beneficiary-government attestation is explicitly not treated as dispositive, and the arsenal states themselves dispute how far the arrangement's necessity extends to its current scale.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__dropping_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__dropping_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__dropping_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_reachability_boundary__dropping_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__dropping_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__dropping_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_reachability_boundary__dropping_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58: substantial but entangled with a real coordination achievement — eighty years without great-power war is the largest coordination payoff on record, yet the same structure holds every industrial society hostage and channels protected budgets to a permanent industrial constituency. Suppression (0.62) reflects active enforcement machinery — alert postures, secrecy regimes, alliance discipline, nonproliferation pressure — rather than participant preference; it is authored as a raw structural property and is deliberately NOT scaled by power or scope (only extractiveness is scaled, by the engine, through directionality and scope). Theater (0.25) is low-moderate: the submarines and warheads are real, but declaratory policy, civil-defense messaging, and abolition rhetoric carried during modernization carry a growing performative share. Accessibility_collapse (0.5) and resistance (0.45) fit a contested construct: disarmament, minimum-deterrence, and no-first-use alternatives remain visible and were never fully foreclosed, and the prohibition-treaty campaign met real — if boycotted — institutional success. The measurement series run on ONE shared grid (t=0..65, mapped to 1960-2025) with all three metrics authored at every point. The series trace a full cycle: buildup and overkill peak (t=20), detente and arms-control relief (t=30-40), post-Cold War trough (t=40), and renewal amid arms-control collapse (t=50-65). The oscillation is driven by external geopolitical cycles (threat inflation, arms-control waves) rather than being itself the extraction mechanism, though threat-inflation phases do extract fiscally. Base_properties are measured at t=65, on the elevated side of the cycle — noted so end-state values are not read as steady-state.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the engine owns that computation. From the agenda-setter seat the arrangement presents as a successfully managed equilibrium — the coordinating party experiences mostly its benefit stream, with tail risk externalized to populations and buffered for itself by continuity-of-government planning. From the payer seats the identical structure presents as enforced risk imposition: an arrangement they fund, inhabit, and die in if it fails, administered without their consent. From the excluded seats (movements, NPT majority) the structure presents as maintained coercion whose coordination story defers its promised exit. Same referent, same epsilon, radically different effective extraction per seat — that divergence is the measurement, not a defect to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (nuclear_weapons_states, defense_industrial_contractors, extended_deterrence_allies) derive low d for those seats; victim declarations (populations_under_nuclear_threat, downwind_test_communities) derive d near the full-target end, amplified by trapped exit and, for the populations, global scope. One override is declared: power_atom institutional at d=0.2. The structural derivation reads nuclear_weapons_states as near-pure beneficiary from the beneficiary declaration alone, but mutual vulnerability means the nuclear states' own cities sit under rival targeting — partial self-exposure that lifts true d above the pure-beneficiary end. The override is safe to scope because nuclear_weapons_states is the only institutional seat in this story. Anti-nuclear movements inherit no clean derivation (they are excluded rather than declared) and are left to the engine's fallback; their membership in the threatened-population class is recorded narratively rather than forced into the victim array, which names affected groups, not advocacy organizations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing a recurrence of industrial-scale total war — is live: great powers with total-war capacity still face one another, so the mandate has not outlived its function and mandatrophy is not resolved. The tangled_rope classification earns its keep in both directions here. Reading the arrangement as pure rope (pure coordination) would erase the hostages: the equilibrium works precisely because existential risk is transferred onto non-consenting populations, which is asymmetric extraction running through the same structure that coordinates. Reading it as pure snare would erase the achievement: unlike a snare, the structure demonstrably solves the problem it names, alternatives were never fully suppressed, and its beneficiaries include sheltered allies who pay far less than they would otherwise. The hybrid category holds both facts without letting either cover story absorb the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is the dropping_reading of kernel total_war_reachability_boundary; what would the sibling readings (contraction_reading, contingent_reachability_reading) change structurally if adopted instead?',
    'Comparative read across the three linked stories: contraction adoption drives epsilon toward negligible (nothing reachable, nothing extracted to maintain) and pushes toward mountain/piton profiles; contingency adoption raises variance and re-keys classification to technological trajectory rather than maintained enforcement.',
    'Classification of the deterrence arrangement is reading-indexed; cross-reading comparison is valid only over the shared referent, never by averaging epsilon across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: one of three live readings of the reachability-boundary kernel.').

omega_variable(
    second_strike_survivability_durability,
    'Will second-strike forces remain survivable under counterforce refinement, cyber operations against command-and-control, hypersonic delivery compression, and expanding missile defense?',
    'Technical force-survivability assessment: track penetration aids, dispersal, and command redundancy against projected counterforce and defense capabilities over the next two decades.',
    'If survivability erodes, the mutual-vulnerability premise underlying the coordination equilibrium weakens, crisis instability rises, and this reading''s ''actively maintained equilibrium'' characterization drifts toward the contingent reading''s fragility thesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(second_strike_survivability_durability, empirical, 'Durability of the survivability premise on which the equilibrium rests.').

omega_variable(
    peace_attribution_ambiguity,
    'How much of the post-1945 great-power peace is attributable to the deterrence arrangement rather than to confounders — economic interdependence, institutional learning, war''s changing profitability?',
    'Comparative-counterfactual analysis: great-power dyads matched on trade intensity and institutionalization but differing on nuclear overlay; archival study of crisis decisions where deterrence logic competed with other restraints.',
    'If much of the peace persists without deterrence, the arrangement''s genuine coordination function shrinks, its extraction share rises, and the profile drifts from tangled_rope toward snare; if deterrence carries the peace, the coordination credit stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peace_attribution_ambiguity, conceptual, 'Attribution of the long peace between constructed equilibrium and background conditions.').

omega_variable(
    umbrella_shelter_vs_dependence_control,
    'Do extended-deterrence allies receive net shelter, or does umbrella dependence operate partly as leverage over their foreign policy?',
    'Alliance bargaining records, burden-sharing disputes, and cases where umbrella states overridden ally preferences (or failed to), assessed against counterfactual autonomous-defense costs.',
    'If dependence functions substantially as control, the victim set widens beyond declared populations, effective extraction on the organized seat rises, and the asymmetry side of the tangled-rope profile sharpens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(umbrella_shelter_vs_dependence_control, empirical, 'Whether the allied beneficiary seat is purely sheltered or partly coerced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__dropping_reading, 0, 65).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twrb_drop_tr_t0, total_war_reachability_boundary__dropping_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(twrb_drop_tr_t10, total_war_reachability_boundary__dropping_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(twrb_drop_tr_t20, total_war_reachability_boundary__dropping_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(twrb_drop_tr_t30, total_war_reachability_boundary__dropping_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(twrb_drop_tr_t40, total_war_reachability_boundary__dropping_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(twrb_drop_tr_t50, total_war_reachability_boundary__dropping_reading, theater_ratio, 50, 0.21).
narrative_ontology:measurement(twrb_drop_tr_t60, total_war_reachability_boundary__dropping_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(twrb_drop_tr_t65, total_war_reachability_boundary__dropping_reading, theater_ratio, 65, 0.25).

% Extraction over time
narrative_ontology:measurement(twrb_drop_be_t0, total_war_reachability_boundary__dropping_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(twrb_drop_be_t10, total_war_reachability_boundary__dropping_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(twrb_drop_be_t20, total_war_reachability_boundary__dropping_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(twrb_drop_be_t30, total_war_reachability_boundary__dropping_reading, base_extractiveness, 30, 0.54).
narrative_ontology:measurement(twrb_drop_be_t40, total_war_reachability_boundary__dropping_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(twrb_drop_be_t50, total_war_reachability_boundary__dropping_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(twrb_drop_be_t60, total_war_reachability_boundary__dropping_reading, base_extractiveness, 60, 0.56).
narrative_ontology:measurement(twrb_drop_be_t65, total_war_reachability_boundary__dropping_reading, base_extractiveness, 65, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(twrb_drop_su_t0, total_war_reachability_boundary__dropping_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(twrb_drop_su_t10, total_war_reachability_boundary__dropping_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(twrb_drop_su_t20, total_war_reachability_boundary__dropping_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(twrb_drop_su_t30, total_war_reachability_boundary__dropping_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(twrb_drop_su_t40, total_war_reachability_boundary__dropping_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(twrb_drop_su_t50, total_war_reachability_boundary__dropping_reading, suppression_requirement, 50, 0.57).
narrative_ontology:measurement(twrb_drop_su_t60, total_war_reachability_boundary__dropping_reading, suppression_requirement, 60, 0.61).
narrative_ontology:measurement(twrb_drop_su_t65, total_war_reachability_boundary__dropping_reading, suppression_requirement, 65, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__dropping_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contingent_reachability_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'total war became unreachable / the nuclear taboo' decomposes into three structurally distinct claims per the epsilon-invariance principle. contraction_reading asserts the feasible set closed entirely (epsilon near-negligible; nothing left to enforce). dropping_reading (this file) asserts probability dropped while reachability persists under active maintenance (substantial epsilon; tangled_rope). contingent_reachability_reading asserts the boundary is technology-dependent and the current lull is reversible inertia (classification keyed to technological trajectory). The upstream claim in each direction supplies the evidentiary warrant the downstream claim cites; all three are linked via network.affects_constraints and share the referent (the standing deterrence arrangement) while authoring distinct epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_reachability_boundary__dropping_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
