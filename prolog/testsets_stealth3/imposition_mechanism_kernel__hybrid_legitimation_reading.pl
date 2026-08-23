% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__hybrid_legitimation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__hybrid_legitimation_reading, []).

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
 *   constraint_id: imposition_mechanism_kernel__hybrid_legitimation_reading
 *   human_readable: Imperial Exemplarity Plus Institutional Incentives: The Hybrid Legitimation Mechanism
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   A reforming or conquering dynasty introduces novel norms — sumptuary
 *   rules, ritual calendars, scripts, registration duties — into a
 *   tradition-bound population. Under the hybrid legitimation reading
 *   instantiated here, the norms become legitimate through two joined
 *   channels: the emperor performs the norm personally, transferring
 *   charismatic authority from the imperial person to the practice, and an
 *   incentive machinery (office, examination eligibility, honors, patronage)
 *   recruits ambitious elites as paid diffusion channels. Adoption is
 *   therefore stratified — elites first, masses later — enforcement costs
 *   stay moderate because ambition does most of the policing, and legitimacy
 *   attaches to the norm via the center's charisma rather than via prior
 *   popular demand or bare coercion. Epsilon's referent is this standing
 *   hybrid arrangement as this reading assesses it: the historical
 *   exemplarity-plus-incentive complex itself, not the climb or override
 *   alternatives. This file is one member of a three-file constraint family
 *   decomposing the imposition_mechanism_kernel; the sibling readings are
 *   separate constraints with their own epsilon values, linked via
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   imperial_court: agenda-setting beneficiary (institutional/constrained) —
 *   performs exemplarity, administers incentives, collects the loyalty
 *   dividend - early_elite_adopters: primary rent-collecting beneficiary
 *   (organized/arbitrage) — converts court proximity into office and
 *   patronage - mid_rank_officials: dual-positioned payer-beneficiary
 *   (moderate/constrained) — purchases career continuity with conformity and
 *   enforces downward - late_adopter_masses: primary target
 *   (powerless/trapped) — bears conformity costs after the settlement is fait
 *   accompli - displaced_tradition_holders: identity-locked target
 *   (powerless/identity_locked) — absorbs delegitimation of inherited
 *   practice - comparative_historians: analytical observer
 *   (analytical/analytical) — sees the full structure including the kernel
 *   contest
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.58).
domain_priors:suppression_score(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.34).
domain_priors:theater_ratio(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__hybrid_legitimation_reading, "Imperial Exemplarity Plus Institutional Incentives: The Hybrid Legitimation Mechanism").
narrative_ontology:topic_domain(imposition_mechanism_kernel__hybrid_legitimation_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__hybrid_legitimation_reading, '6547a125-7430-4d19-9e0d-9450f3281555').
narrative_ontology:cs_kernel_codification('6547a125-7430-4d19-9e0d-9450f3281555', distributed).
narrative_ontology:cs_authority_grounding('6547a125-7430-4d19-9e0d-9450f3281555', distributed).
narrative_ontology:cs_reading_relation('6547a125-7430-4d19-9e0d-9450f3281555', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('6547a125-7430-4d19-9e0d-9450f3281555', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('6547a125-7430-4d19-9e0d-9450f3281555', foundational, legitimacy_flows_from_exemplary_center).
narrative_ontology:cs_axiom_status(legitimacy_flows_from_exemplary_center, holdable).
narrative_ontology:cs_axiom_grounding('6547a125-7430-4d19-9e0d-9450f3281555', legitimacy_flows_from_exemplary_center, empirically_contingent).
narrative_ontology:cs_axiom('6547a125-7430-4d19-9e0d-9450f3281555', foundational, incentive_channels_convert_prestige_into_adoption).
narrative_ontology:cs_axiom_status(incentive_channels_convert_prestige_into_adoption, holdable).
narrative_ontology:cs_axiom_grounding('6547a125-7430-4d19-9e0d-9450f3281555', incentive_channels_convert_prestige_into_adoption, empirically_contingent).
narrative_ontology:cs_reference_frame('6547a125-7430-4d19-9e0d-9450f3281555', charismatic_exemplarity_incentive_hybrid).
narrative_ontology:cs_drift_state('6547a125-7430-4d19-9e0d-9450f3281555', contemporary_microhistorical_turn, gap(axiom_overriding, minor, true)).
narrative_ontology:cs_created_at('6547a125-7430-4d19-9e0d-9450f3281555', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, early_elite_adopters).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, late_adopter_masses).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, displaced_tradition_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, mid_rank_officials).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, mid_rank_officials).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_charisma_doctrine).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__hybrid_legitimation_reading, exemplarity_confers_normativity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the norm by performing it — dress, ritual, speech, calendar — and backs the performance with offices, examination eligibility, honors, and patronage for those who follow. Once the performance begins, the dynasty cannot stop without surrendering its claim to moral leadership, so the court is bound to keep exemplifying norms it may privately find burdensome. Collects deference, administrative reach, and the ability to steer culture without garrisons.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court, agenda_setter,
    institutional, generational, constrained, continental).

% Watch the court closely and move first: adopting the norm early converts proximity to imperial favor into appointments, marriages, and patronage networks. Their visible success is the demonstration that pulls the next tier along. Because their asset is responsiveness rather than conviction, they can switch allegiance to a successor norm or a rival patron when signals change.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, early_elite_adopters, beneficiary,
    organized, biographical, arbitrage, continental).

% Must display the norm to remain eligible for posting and promotion, and must enforce it downward on clerks and households in their charge. Compliance buys career continuity and a share of distributable rewards; the price is discarding familiar practice and absorbing the resentment of those they police.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, mid_rank_officials, payer,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, mid_rank_officials, beneficiary).

% Meet the norm as accomplished fact, years after the court and elites settled it. Conformity obligations arrive attached to taxes, registration, market access, and household respectability. They receive none of the early-adopter rewards; their options are compliance, clandestine persistence, or removal to peripheral districts where enforcement thins.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, late_adopter_masses, payer,
    powerless, generational, trapped, continental).

% Custodians of the rites, crafts, and lineages the new norm displaces. Their authority rests on knowledge the center has ruled obsolete; students, clients, and income drain away as conformity spreads. They were never consulted when the legitimacy bargain was struck at court, and leaving the practice would erase the identity that organizes their lives, so most persist in shrinking enclaves.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, displaced_tradition_holders, payer,
    powerless, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, displaced_tradition_holders, excluded).

% Reconstruct the sequence from edicts, ledgers, tomb inscriptions, and adoption records; date acts of exemplarity against adoption curves and incentive grants. From this seat the whole mechanism — court performance, elite arbitrage, mass conformity — is visible at once, including the places where the record contradicts every available account of it.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, comparative_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__hybrid_legitimation_reading, early_elite_adopters).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__hybrid_legitimation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns a vast, heterogeneous population on novel norms at far lower cost than universal coercion: the emperor's example supplies a common focal point that settles what is now correct, and institutional incentives recruit self-interested elites as diffusion channels, so compliance propagates through ambition rather than garrison.
% TRANSFER_FUNCTION: Moves legitimacy from the imperial person to the norm (symbolic transfer outward); moves offices, honors, and patronage from the center to early elite adopters (material transfer downward); moves conformity costs — abandoned practice, retrained habit, policed households — onto late adopters and displaced tradition-holders.
% ABSENT_VOICES: Displaced tradition-holders and the mass of eventual conformists were structurally absent: the legitimacy transaction was concluded between court and early adopters before the wider population met the norm as settled fact. Their objection — that the new norm's acceptance was arranged rather than grown — survives only obliquely, in suppression edicts and migration records.
% DISAPPEARANCE_RATIONALE: Without the exemplarity-plus-incentive mechanism, dynastic norm campaigns stall at the court boundary: no focal point forms, early-adopter rewards vanish, and diffusion proceeds only by open coercion or not at all. Examination eligibility, honor hierarchies, and the dynasty's claim to moral leadership all presuppose the mechanism; removing it overnight would force either reversion to local traditions or overt violent imposition.
% FOUNDING_PROBLEM: A conquering or reforming dynasty inherits a tradition-bound population with no mass communication: how does a novel norm — ritual, dress, script, registration — become simply how things are done without stationing an enforcer behind every household?
% FOUNDING_PROBLEM_CORROBORATION: Comparative historiography outside any benefiting party corroborates both the problem's reality and the dispute over its solution: adoption-timeline studies of Tang sumptuary reform, the Roman imperial cult, and Meiji ceremonial westernization document elite-first adoption curves, while the endogenous-climb school attributes the same curves to prior popular uptake and the override school to unrecorded coercion — no school speaks for the court, and none settles the question.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__hybrid_legitimation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__hybrid_legitimation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__hybrid_legitimation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (interval end) because conformity costs are broad-based — every household eventually complies — while rewards concentrate on the small early-adopter tier; the gap widens as courts learn to reuse the machinery across successive norm campaigns (rising base_extractiveness series). Suppression is 0.34: enforcement is predominantly positive inducement with targeted penalties for visible defiance, and the suppression_requirement series shows the characteristic hump — capacity built during the stratified-adoption phase (peak 0.46 at midpoint), then partially retired as the norm internalizes. Theater_ratio 0.30 and rising: early exemplarity is functional signaling, but later performances rehearse norms already settled, drifting toward ceremony. Accessibility_collapse 0.38: private dissent, delayed compliance, and peripheral residence remain available, but public counter-practice collapses once the norm is understood as imperial-endorsed. Resistance 0.42: tradition-holders resist in fragmented, localized ways while elites comply eagerly, so opposition never concentrates. All three series share one time grid (points 0-30, normalized decades of a representative dynastic campaign). Fixing_cost is prohibitive for the only seat that could act: dismantling the mechanism would forfeit the dynasty's norm-steering capacity and its own legitimacy instrument simultaneously, a cost exceeding any benefit the court could expect.
 *
 * PERSPECTIVAL GAP:
 *   The court and early-adopter seats should compute coordination-flavored classifications: they animate the mechanism, collect its rewards, and experience it as an instrument they wield. The late-adopter and tradition-holder seats should compute extraction-flavored classifications: they were conscripted into a settlement concluded above their heads and bear its recurring costs without its rewards. Mid-rank officials straddle — paying conformity and collecting rewards in the same motion. The engine computes these divergent per-seat types from the power, exit, and role data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (imperial_court, early_elite_adopters) drive those seats toward the beneficiary end of d; victim declarations (late_adopter_masses, displaced_tradition_holders) drive theirs toward the target end. Exit modulation sharpens the split: arbitrage-grade exit keeps early adopters nearest the subsidy end, while identity_lock pins tradition-holders near the full-target end even where physical exit technically exists. One override is authored: the institutional seat (imperial_court) is raised from its derived beneficiary-side value (~0.08) to 0.18, because the charisma economy converts the court from pure beneficiary into partly captive participant — once exemplarity begins, the dynasty must keep performing or forfeit the steering instrument, a continuing cost the plain beneficiary derivation cannot see. No other override is needed; the derivation tracks the declared structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against both mislabels. Reading the mechanism as pure rope would ignore that its rewards concentrate on early movers while its costs diffuse across late adopters — an asymmetry requiring active enforcement to hold. Reading it as snare would ignore the genuine coordination achievement: society-wide norm alignment at a fraction of coercive cost, with real exits left open. On mandatrophy: the mechanism's founding mandate (diffuse novel norms without garrisons) is periodically renewed by each new dynastic campaign, so the arrangement does not atrophy into inertial performance — theater_ratio rises within campaigns but resets as new norms re-engage real diffusion work. The extractive layering accumulates instead: courts refine the timing of exemplarity and incentive release to widen early-adopter rents, which is why base_extractiveness trends upward across the interval while the coordination function stays live. The R5 mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: the dispute is over whether the founding problem was ever solved this way, not over whether it existed, so no zombie flag is expected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'Does the hybrid reading, rather than endogenous climb or exogenous override, correctly characterize how the examined norms gained legitimacy?',
    'Fine-grained adoption timelines cross-referenced against dated acts of imperial exemplarity and incentive-grant records: adoption consistently preceding exemplarity supports the climb reading; adoption absent incentives despite exemplarity supports the override reading; adoption tracking the joined signature supports this reading.',
    'Resolution reallocates epsilon and type across the three family files: if climb dominates, this file''s measured asymmetry reflects manufactured consensus and drops toward rope; if override dominates, suppression rises sharply and the family re-centers on the coercion file.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_allocation, empirical, 'Which reading of the imposition-mechanism kernel the adoption record actually supports.').

omega_variable(
    charisma_incentive_separability,
    'Is elite-first adoption driven by the emperor''s symbolic example, or would the incentive package alone have produced the same curve?',
    'Compare cases where incentive grants preceded acts of exemplarity against cases where they followed; contrast with incentive-only administrations lacking a charismatic center.',
    'If incentives suffice alone, the charisma component is garnish and theater_ratio is understated; if charisma is load-bearing, the mechanism''s coordination content is higher than its extraction profile alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charisma_incentive_separability, empirical, 'Whether the symbolic and material channels of the hybrid mechanism are separable.').

omega_variable(
    stratification_design_ambiguity,
    'Is elite-first sequencing an emergent diffusion property or a deliberately engineered reward schedule?',
    'Court correspondence and edict drafting records revealing whether adoption-order advantages were anticipated and priced at design time.',
    'Deliberate design raises effective extraction toward the snare boundary; emergent sequencing keeps the mechanism rope-dominant with incidental asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratification_design_ambiguity, conceptual, 'Whether the stratified adoption curve reflects design or emergence.').

omega_variable(
    modern_recurrence_extension,
    'Does the hybrid mechanism recur in contemporary states and platforms — visible exemplars joined to participation incentives — extending the constraint past its imperial referent?',
    'Apply the same adoption-timeline tests to modern norm campaigns (public-health exemplars, platform creator economies) and compare curve shapes against the imperial baseline.',
    'Recurrence would justify treating this as a persistent governance technology rather than a historical episode, raising its scope weighting and connecting it to modern platform-governance constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modern_recurrence_extension, empirical, 'Whether the mechanism generalizes beyond its historical instantiation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__hybrid_legitimation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(impo_tr_t5, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(impo_tr_t10, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(impo_tr_t15, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(impo_tr_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(impo_tr_t25, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(impo_tr_t30, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(impo_be_t5, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 5, 0.43).
narrative_ontology:measurement(impo_be_t10, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(impo_be_t15, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(impo_be_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(impo_be_t25, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement(impo_be_t30, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(impo_su_t5, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(impo_su_t10, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(impo_su_t15, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 15, 0.46).
narrative_ontology:measurement(impo_su_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(impo_su_t25, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 25, 0.37).
narrative_ontology:measurement(impo_su_t30, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 30, 0.34).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__hybrid_legitimation_reading, identity_coordination).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'how imposed norms gained legitimacy' covers three structurally distinct claims with different epsilon profiles: bottom-up climb (low enforcement, low asymmetry), coercive override (high suppression, concentrated extractor), and the hybrid legitimation complex authored here (moderate enforcement, stratified rents). Each is a separate file; this file links both siblings. The hybrid reading sits structurally between the poles and cites the same adoption records each pole cites, so coupling between family members runs through shared evidence rather than causal dependency. None of the three readings logically forecloses another — mixed cases exist empirically — which is why all reading_relations are coexists_with.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_mechanism_kernel__hybrid_legitimation_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
