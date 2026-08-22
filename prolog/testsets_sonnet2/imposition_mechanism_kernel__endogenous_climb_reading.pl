% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__endogenous_climb_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: imposition_mechanism_kernel__endogenous_climb_reading
 *   human_readable: Endogenous Climb Reading of Norm Legitimation (Bottom-Up Adoption Preceding State Mandate)
 *   domain: historical_sociology/state_formation
 *
 * SUMMARY:
 *   This story instantiates the endogenous_climb reading of the
 *   imposition_mechanism_kernel: a contested historical-sociological claim
 *   about how a specific new social norm acquired legitimacy. In this
 *   reading, informal, decentralized adoption by communities and local norm
 *   entrepreneurs preceded any state pronouncement; the eventual state
 *   mandate merely codified an already-widespread practice. Enforcement costs
 *   are low, adoption is fast because it solves a real coordination problem,
 *   and resistance is minimal because most of the governed population had
 *   already voluntarily complied before the mandate existed. This is a
 *   Rope-shaped reading: the state functions as a coordinator ratifying
 *   consensus, not a coercer manufacturing compliance. The sibling readings —
 *   exogenous_override (state coercion imposed the norm, legitimacy flows
 *   from the monopoly on violence) and hybrid_legitimation (symbolic
 *   authority transfer plus institutional incentive, neither pure climb nor
 *   pure override) — describe structurally different mechanisms with
 *   different ε values and are NOT part of this constraint; they are separate
 *   stories linked via network edges.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__endogenous_climb_reading, 0.18).
domain_priors:suppression_score(imposition_mechanism_kernel__endogenous_climb_reading, 0.12).
domain_priors:theater_ratio(imposition_mechanism_kernel__endogenous_climb_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, resistance, 0.14).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__endogenous_climb_reading, rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__endogenous_climb_reading, "Endogenous Climb Reading of Norm Legitimation (Bottom-Up Adoption Preceding State Mandate)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__endogenous_climb_reading, "historical_sociology/state_formation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__endogenous_climb_reading, 'e342856f-9859-4006-a559-0d70b10d670f').
narrative_ontology:cs_kernel_codification('e342856f-9859-4006-a559-0d70b10d670f', distributed).
narrative_ontology:cs_authority_grounding('e342856f-9859-4006-a559-0d70b10d670f', practice).
narrative_ontology:cs_interpretation_layer_present('e342856f-9859-4006-a559-0d70b10d670f').
narrative_ontology:cs_reading_relation('e342856f-9859-4006-a559-0d70b10d670f', imposition_mechanism_kernel__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('e342856f-9859-4006-a559-0d70b10d670f', imposition_mechanism_kernel__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('e342856f-9859-4006-a559-0d70b10d670f', foundational, legitimacy_precedes_and_produces_mandate).
narrative_ontology:cs_axiom_status(legitimacy_precedes_and_produces_mandate, holdable).
narrative_ontology:cs_axiom_grounding('e342856f-9859-4006-a559-0d70b10d670f', legitimacy_precedes_and_produces_mandate, empirically_contingent).
narrative_ontology:cs_axiom('e342856f-9859-4006-a559-0d70b10d670f', secondary, state_authority_is_ratifying_not_originating).
narrative_ontology:cs_axiom_status(state_authority_is_ratifying_not_originating, holdable).
narrative_ontology:cs_axiom_grounding('e342856f-9859-4006-a559-0d70b10d670f', state_authority_is_ratifying_not_originating, empirically_contingent).
narrative_ontology:cs_reference_frame('e342856f-9859-4006-a559-0d70b10d670f', community_originated_customary_practice).
narrative_ontology:cs_drift_state('e342856f-9859-4006-a559-0d70b10d670f', state_codification_moment, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e342856f-9859-4006-a559-0d70b10d670f', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, early_adopter_communities).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, local_norm_entrepreneurs).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, the_state_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__endogenous_climb_reading, late_adopting_communities).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__endogenous_climb_reading, bottom_up_legitimation_thesis).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__endogenous_climb_reading, state_as_ratifier_not_originator).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Practiced the new norm voluntarily before any state pronouncement, gaining local prestige and coordination benefits from being first movers. Their continued practice is what state mandate eventually ratifies; they are not compelled by the mandate because they were already doing the thing it names.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, early_adopter_communities, beneficiary,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__endogenous_climb_reading, early_adopter_communities, agenda_setter).

% Merchants, clergy, or guild leaders who modeled and promoted the emerging practice within their communities ahead of any state involvement. They accumulate social capital and influence from being credited as originators; if the practice fails to spread they simply revert to prior norms at low cost.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, local_norm_entrepreneurs, agenda_setter,
    moderate, biographical, mobile, local).

% Observes the norm's diffusion and, once adoption is widespread, issues formal mandate to codify what is already practiced. Gains legitimacy and administrative simplicity by appearing to lead a change it in fact ratified; enforcement costs are minimal because compliance already exists.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, the_state_apparatus, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__endogenous_climb_reading, the_state_apparatus, observer).

% Communities slower to adopt the norm face mild social and eventually administrative pressure to conform once mandate arrives, but the pressure is modest because the norm is already broadly accepted by the time it reaches them; their main cost is the friction of behavioral change, not coercive punishment.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, late_adopting_communities, payer,
    moderate, biographical, constrained, regional).

% A minority preferring the prior practice are not consulted in the informal process by which the new norm climbs to legitimacy; by the time state mandate arrives their objection has already been rendered moot by widespread popular acceptance, and they have no forum in which the change was ever debated.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, traditionalist_holdouts, excluded,
    powerless, biographical, constrained, local).

% Chronicle the sequence of events and can attest whether state decree preceded or followed observable popular practice, based on dated administrative records versus dated accounts of community behavior.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, court_historians, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine coordination problem: many dispersed actors needed a shared practice (a calendar convention, a dress norm, a commercial standard) and adoption spread because it solved local coordination problems for those who adopted it, with the state later supplying uniform codification to iron out residual variance.
% TRANSFER_FUNCTION: Moves prestige and coordination benefit to early adopters and norm entrepreneurs; moves administrative simplicity and retrospective legitimacy to the state, which claims credit for a change it observed rather than caused. Late adopters pay a modest conformity cost as the practice becomes universal.
% ABSENT_VOICES: Traditionalist holdouts who preferred prior practice were never part of the informal diffusion process and have no venue in which their preference was weighed; by the time the state mandate exists to object to, the popular question is already settled.
% DISAPPEARANCE_RATIONALE: If the mandate vanished, most practice would continue unchanged since it precedes and does not depend on state codification — but the residual holdout communities and cross-regional disputes the mandate resolves would resurface, and administrative uniformity would erode at the margins where voluntary adoption had not yet fully converged.
% FOUNDING_PROBLEM: Dispersed communities needed a shared practice to solve local coordination problems (trade, timekeeping, social signaling) faster than any central authority could design and impose one from above.
% FOUNDING_PROBLEM_CORROBORATION: Court historians and comparative administrative records corroborate the sequence — dated local practice antedates dated state decree in the surviving record — providing an attestation from outside the state apparatus and outside the early-adopter communities that stand to gain credit for originating the change.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__endogenous_climb_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).
:- end_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18 at interval end) because the norm-holders who benefit (early adopters, norm entrepreneurs) largely bear no cost — most costs fall lightly on late adopters as ordinary conformity friction, not extraction. Suppression is correspondingly low (0.12): there is no coercive apparatus enforcing the norm against a resistant population, because the population was not resistant by the time the state acted. Accessibility collapse is moderate (0.35) rather than near-total: alternative practices did not vanish overnight but eroded gradually as the coordination benefit of the new norm made holding out increasingly costly for isolated non-adopters. Resistance is correspondingly low (0.14) since informal diffusion selects for practices with low resistance by construction — practices that meet strong resistance don't climb.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters and norm entrepreneurs sit near the full-beneficiary end: they gain prestige and coordination advantage, face negligible enforcement, and retain mobile exit throughout. The state apparatus is also directionally a beneficiary here — it gains legitimacy and administrative ease from appearing to have led what it in fact ratified. Late adopters carry a modest payer role but with constrained (not trapped) exit, since the social pressure they face is the ordinary pressure of a majority practice, not state coercion. Traditionalist holdouts are excluded from the informal diffusion process entirely rather than being extracted from through this constraint's specific mechanism — their situation is a byproduct of consensus formation, not a targeted transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as pure extraction: because state mandate followed rather than preceded adoption, there is no point at which coercive apparatus was built to manufacture compliance that wasn't already occurring. Framing this identical historical sequence as a Snare (as the exogenous_override sibling reading would, if applied to the same events) would require treating the state's ratifying decree as the originating cause of compliance, which this reading's chronology denies. The founding_problem/disappearance mismatch check is instructive here: founding_problem_status is 'live' (coordination problem persists) and disappearance_verdict is 'world_rearranges' but only partially — the mandate's removal would matter less than in a coercive reading, since most practice is self-sustaining independent of state enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diffusion_sequence_evidentiary_basis,
    'Is the historical record sufficient to establish that popular adoption genuinely preceded state mandate, or is this sequencing itself a retrospective narrative constructed after the fact by beneficiaries of the endogenous-climb story (the state, seeking legitimacy, and early adopters, seeking credit)?',
    'Cross-reference dated administrative records (mandate issuance dates) against independent dated evidence of practice (private correspondence, trade records, archaeological or material evidence of the practice predating decree) from sources with no stake in either the state''s or the early adopters'' narrative.',
    'If independent dating shows the practice did NOT clearly precede the mandate, this reading''s core premise collapses and the kernel resolves toward exogenous_override or hybrid_legitimation instead — the classification as Rope depends entirely on the sequencing claim holding up.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffusion_sequence_evidentiary_basis, empirical, 'Whether the bottom-up sequencing claim central to this reading is independently verifiable or a constructed retrospective narrative.').

omega_variable(
    kernel_framing_underdetermination,
    'Is a single historical episode of norm legitimation genuinely separable into three distinct mechanistic readings (endogenous climb, exogenous override, hybrid legitimation), or does the underlying historical process actually combine elements of all three in ways that resist clean decomposition?',
    'Fine-grained regional and temporal disaggregation of the historical record: if different regions or social strata show different sequencing (some climbing, some coerced, some hybrid), the kernel itself may be mis-specified as a single contest rather than a composite of parallel, coexisting mechanisms operating on different populations simultaneously.',
    'If the mechanisms coexist regionally rather than compete as alternative full descriptions, each reading may be locally true for a subset of the governed population rather than globally true or false for the whole episode — this would argue for further decomposition into region-indexed constraint stories rather than three global readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the three-way kernel contest correctly partitions the phenomenon or whether finer regional decomposition is needed.').

omega_variable(
    natural_law_vs_constructed_legitimation_narrative,
    'Does the low-extraction, low-suppression profile of this reading reflect a genuinely benign coordination process, or does ''the state as coordinator not coercer'' function as a legitimating myth that beneficiaries (the state apparatus, early adopters) have incentive to promote regardless of the underlying facts?',
    'Compare state''s contemporaneous internal records (if any survive) discussing the practice before public mandate — do they show active promotion/pressure efforts inconsistent with pure observation-and-ratification, or genuine surprise/discovery of an existing consensus?',
    'Evidence of active pre-mandate state promotion would shift beneficiary status of the_state_apparatus from passive ratifier toward active agenda-setter, pushing the classification toward hybrid_legitimation or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_legitimation_narrative, empirical, 'Whether the state''s coordinator framing is a genuine description or a self-serving legitimation narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__endogenous_climb_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(impo_tr_t8, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 8, 0.07).
narrative_ontology:measurement(impo_tr_t16, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 16, 0.09).
narrative_ontology:measurement(impo_tr_t24, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement(impo_tr_t32, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 32, 0.13).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(impo_be_t8, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 8, 0.1).
narrative_ontology:measurement(impo_be_t16, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 16, 0.13).
narrative_ontology:measurement(impo_be_t24, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 24, 0.15).
narrative_ontology:measurement(impo_be_t32, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 32, 0.17).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 40, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(impo_su_t8, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 8, 0.06).
narrative_ontology:measurement(impo_su_t16, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 16, 0.08).
narrative_ontology:measurement(impo_su_t24, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 24, 0.09).
narrative_ontology:measurement(impo_su_t32, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 32, 0.11).
narrative_ontology:measurement(impo_su_t40, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 40, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of imposition_mechanism_kernel, decomposed per the ε-invariance principle: the natural-language claim 'how did this norm gain legitimacy' conflates three structurally distinct causal claims with different ε values (low here, high in exogenous_override, moderate/mixed in hybrid_legitimation). Each reading is authored as its own constraint with its own beneficiary/victim structure and classification; none averages or hedges across the others. Linked bidirectionally in the sibling files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
