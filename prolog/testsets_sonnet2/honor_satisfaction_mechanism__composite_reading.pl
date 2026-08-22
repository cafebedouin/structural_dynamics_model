% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__composite_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__composite_reading
 *   human_readable: Honor-Satisfaction Mechanism (Composite Multi-Mechanism Reading)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This is the composite reading of the honor-satisfaction kernel: rather
 *   than a single cause of dueling's disappearance (declining frequency, or a
 *   wholesale cognitive foreclosure), this reading holds that at least four
 *   structurally distinct mechanisms operated concurrently and unevenly
 *   across regions and classes — the state's assertion of a monopoly on
 *   legitimate adjudicated violence (criminalization, prosecution of
 *   seconds), the rise of bourgeois professional norms that substituted
 *   litigation and reputational management for combat, insurance-like courts
 *   of honor and arbitration boards that certified 'satisfaction'
 *   procedurally, and an eventual category-shift that recast dueling itself
 *   as criminal deviance rather than honorable practice. Each mechanism
 *   captured a different constituency and left different populations
 *   (provincial gentry, junior military officers) caught in gaps between
 *   overlapping, incompletely-reaching substitute institutions. The claimed
 *   type is tangled_rope: there is a genuine coordination function (some
 *   legitimate way to resolve reputational injury must exist) but the
 *   multiple substitute mechanisms also extract — in fees, deference,
 *   prosecutorial discretion, and unequal institutional access — from those
 *   they nominally serve.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, 0.61).
domain_priors:suppression_score(honor_satisfaction_mechanism__composite_reading, 0.58).
domain_priors:theater_ratio(honor_satisfaction_mechanism__composite_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__composite_reading, "Honor-Satisfaction Mechanism (Composite Multi-Mechanism Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__composite_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__composite_reading, 'd2bc447b-c7f8-46af-ad54-b7c3f079edf5').
narrative_ontology:cs_kernel_codification('d2bc447b-c7f8-46af-ad54-b7c3f079edf5', distributed).
narrative_ontology:cs_authority_grounding('d2bc447b-c7f8-46af-ad54-b7c3f079edf5', distributed).
narrative_ontology:cs_reading_relation('d2bc447b-c7f8-46af-ad54-b7c3f079edf5', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2bc447b-c7f8-46af-ad54-b7c3f079edf5', honor_satisfaction_mechanism__contraction_reading, influences).
narrative_ontology:cs_axiom('d2bc447b-c7f8-46af-ad54-b7c3f079edf5', foundational, satisfaction_function_is_institutionally_divisible).
narrative_ontology:cs_axiom_status(satisfaction_function_is_institutionally_divisible, holdable).
narrative_ontology:cs_axiom_grounding('d2bc447b-c7f8-46af-ad54-b7c3f079edf5', satisfaction_function_is_institutionally_divisible, empirically_contingent).
narrative_ontology:cs_axiom('d2bc447b-c7f8-46af-ad54-b7c3f079edf5', foundational, unequal_institutional_reach_produces_residual_victim_class).
narrative_ontology:cs_axiom_status(unequal_institutional_reach_produces_residual_victim_class, holdable).
narrative_ontology:cs_axiom_grounding('d2bc447b-c7f8-46af-ad54-b7c3f079edf5', unequal_institutional_reach_produces_residual_victim_class, empirically_contingent).
narrative_ontology:cs_reference_frame('d2bc447b-c7f8-46af-ad54-b7c3f079edf5', code_duello_peer_administered_satisfaction).
narrative_ontology:cs_drift_state('d2bc447b-c7f8-46af-ad54-b7c3f079edf5', early_twentieth_century_institutional_settlement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d2bc447b-c7f8-46af-ad54-b7c3f079edf5', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, state_judicial_monopolists).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, bourgeois_professional_class).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, honor_insurance_arbiters).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, dueling_code_arbiters).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, challenged_gentlemen).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, junior_officers).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, provincial_notables_excluded_from_new_codes).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__composite_reading, state_monopoly_on_legitimate_violence).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__composite_reading, bourgeois_respectability_as_civic_virtue).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts, prosecutors, and legislators progressively criminalize dueling and reroute honor disputes into libel law, criminal assault statutes, and civil defamation suits. They collect legitimacy and case volume by supplanting private settlement with state adjudication, and enforce this by prosecuting duelists and seconds even where local juries are reluctant to convict.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, state_judicial_monopolists, agenda_setter,
    institutional, generational, arbitrage, national).

% Rising merchants, lawyers, and administrators promote norms of self-restraint, litigation, and reputational management through print and professional association rather than physical risk. They benefit because these substitute mechanisms favor those with money and legal access over those with only inherited martial-honor capital, and they can walk away from the old code entirely without loss of standing.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, bourgeois_professional_class, beneficiary,
    organized, generational, mobile, national).

% Codes of honor committees, courts of honor, and informal arbitration boards emerge to certify apologies, mediate disputes, and formally declare a man's honor 'satisfied' without combat. They profit in social capital and institutional durability by inserting themselves as the necessary intermediary, effectively insuring participants against violent escalation while charging in reputational deference and procedural compliance.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, honor_insurance_arbiters, beneficiary,
    organized, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__composite_reading, honor_insurance_arbiters, agenda_setter).

% Seconds and code-of-honor authorities who still administer the shrinking pool of actual duels retain gatekeeping authority over an increasingly rarefied practice, deriving status from expertise in a ritual most of society has already reclassified as a criminal anachronism.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, dueling_code_arbiters, beneficiary,
    organized, biographical, constrained, regional).

% Men whose honor is impugned face a shrinking menu of legitimate responses: fight and risk prosecution and social recategorization as a criminal or a relic, or submit to courts and insurance-like arbitration mechanisms that impose costs (legal fees, public apology, deference to arbiters) they did not choose and that may not restore standing among peers who still hold the older code.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, challenged_gentlemen, payer,
    moderate, biographical, constrained, regional).

% Military subculture retains dueling obligations longer than civilian life, so junior officers are structurally trapped between institutional criminalization (court-martial, cashiering) and regimental peer pressure that treats refusal to fight as disqualifying — caught in the gap between the mechanisms that are supposedly replacing dueling and a peer institution that has not caught up.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, junior_officers, payer,
    powerless, immediate, trapped, national).

% Rural gentry without access to metropolitan courts of honor, insurance-style arbitration boards, or bourgeois professional networks find their traditional path to satisfaction closed off by law while the substitute institutions remain geographically and socially inaccessible to them, leaving them with no legitimate mechanism at all.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, provincial_notables_excluded_from_new_codes, payer,
    powerless, biographical, trapped, local).

% Reconstruct court records, insurance-arbitration ledgers, dueling manuals, and criminal prosecution statistics across jurisdictions to trace how satisfaction was actually achieved once combat was foreclosed by multiple simultaneous mechanisms rather than any single one.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a socially legible way to resolve an insult or reputational injury without descending into unregulated private violence — some mechanism must exist for signaling that a grievance has been addressed and normal social relations can resume.
% TRANSFER_FUNCTION: Moves the authority to certify 'satisfaction' away from the dueling code's peer-administered violence and distributes it across the state (criminal/civil courts), bourgeois professional associations (reputational management), and honor-insurance-like arbitration boards (procedural mediation) — each capturing a portion of the social and financial capital formerly concentrated in the code duello's seconds and arbiters.
% ABSENT_VOICES: Provincial and lower-gentry claimants whose disputes fall outside metropolitan court jurisdiction, professional associational reach, and arbitration-board catchment are left with no legitimate outlet at all — they are structurally excluded from every substitute mechanism simultaneously and are rarely represented in the legal and print records historians rely on.
% DISAPPEARANCE_RATIONALE: If all four mechanisms (state monopoly, bourgeois norms, insurance-style arbitration, and category-shift stigmatization) vanished simultaneously, the vacuum left by the code duello's collapse would have to be filled by some other satisfaction mechanism — a wholesale reversion to private violence, an entirely different institutional substitute, or an unresolved epidemic of unaddressed grievances; social life around reputation and insult would visibly reorganize.
% FOUNDING_PROBLEM: How can a society without a functioning state monopoly on legitimate violence, or with widely dispersed and unequal access to courts, allow gentlemen (and later a broader class) to resolve reputational injuries without either endless private feuding or total loss of standing through public humiliation?
% FOUNDING_PROBLEM_CORROBORATION: State officials and bourgeois reformers attest the founding problem is essentially dead — modern law and civil society fully absorb the function. Military historians and social historians studying rural and lower-gentry populations, working from court-martial records and provincial correspondence outside the beneficiary institutions, attest the underlying problem of unaddressed status injury persisted well past formal dueling's decline precisely because the substitute mechanisms did not reach everyone equally.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__composite_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__composite_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_mechanism__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.34 to 0.61) as more institutional layers accrete on top of one another rather than cleanly replacing the code duello — the composite reading's core empirical claim is that these mechanisms overlapped and compounded rather than substituted cleanly, so aggregate extraction (fees, prosecutorial costs, unequal access) rose even as combat frequency fell. Theater ratio rises correspondingly (0.18 to 0.42) as courts of honor and arbitration boards increasingly perform the function of 'satisfaction' without underlying resolution for populations they don't reach. Suppression rises (0.31 to 0.58) tracking the state's increasing prosecutorial capacity against dueling specifically, which is a genuinely different mechanism from the bourgeois-norm or insurance-arbitration channels and is why this reading requires multiple, not single, causal threads.
 *
 * DIRECTIONALITY LOGIC:
 *   State judicial monopolists and the professional/arbitration classes are structural beneficiaries — each captures authority, fees, or legitimacy that the code duello's private mechanism formerly held entirely with seconds and arbiters. Challenged gentlemen, junior officers, and provincial notables are targets: the first two face a widening but incomplete menu of legitimate responses, and the third is left with essentially no functioning mechanism as old and new systems both recede from their social position. Junior officers receive an explicit trapped/powerless designation because military subculture retention of the code lags civilian legal change, producing a genuine double-bind rather than symmetric cost-sharing.
 *
 * MANDATROPHY ANALYSIS:
 *   The composite reading resists collapsing into either 'coordination succeeded' (rope) or 'pure extraction by new gatekeepers' (snare) by insisting the founding problem — legitimate resolution of status injury — remained partially live for populations the new mechanisms didn't reach, while being substantially resolved for the classes who built and controlled the substitute institutions. Tangled rope captures this: real coordination function for the classes served, real extraction (fee capture, unequal access, criminalized residual practice) for those left in the gaps.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_independence_vs_convergence,
    'Were the four mechanisms (state monopoly, bourgeois norms, insurance-arbitration, category-shift) genuinely causally independent processes that happened to co-occur, or were they mutually reinforcing expressions of a single underlying shift in class power that the composite reading is artificially disaggregating?',
    'Comparative institutional history tracing whether the mechanisms emerged in different jurisdictions with different sequencing and different sponsoring classes — independent emergence with variable sequencing would support genuine plurality; uniform sequencing across contexts would support a single underlying cause described four ways.',
    'If the mechanisms are genuinely independent, the composite reading''s tangled_rope classification holds with multiple distinct beneficiary/victim structures overlaid. If they are expressions of one process, this reading collapses toward the contraction_reading''s single-mechanism account and the extraction analysis should be unified rather than distributed across four institutional beneficiary sets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_independence_vs_convergence, conceptual, 'Whether the composite reading''s multi-mechanism claim is structurally distinct from a single-cause account described in four vocabularies.').

omega_variable(
    provincial_gap_population_size,
    'How large, numerically and proportionally, was the population of provincial notables and lower gentry left without access to any of the four substitute mechanisms?',
    'Quantitative analysis of court-martial records, provincial correspondence archives, and regional newspaper accounts of unresolved honor disputes, cross-referenced against metropolitan court and arbitration-board catchment maps.',
    'A large excluded population would substantially raise the confidence in this reading''s victim-class claim and support tangled_rope over a purer rope reading of institutional succession; a small or negligible population would weaken the composite reading''s distinctiveness relative to the decline_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(provincial_gap_population_size, empirical, 'The empirical size of the population excluded from all substitute honor-satisfaction mechanisms.').

omega_variable(
    state_monopoly_natural_vs_constructed,
    'Is the state''s assumption of monopoly authority over legitimate violence (vindicated here as a proposition) a natural consequence of state formation, or a constructed extraction of authority that happened to align with, and benefit from, honor-dispute resolution specifically?',
    'Comparative state-formation literature examining whether monopolization of legitimate violence proceeded uniformly across domains (criminal law generally) or was accelerated specifically in the honor-dispute domain by identifiable state actors seeking jurisdictional expansion.',
    'If the monopoly is a general feature of state formation applied incidentally to dueling, the state-monopoly beneficiary claim is weaker (background process, not targeted extraction). If jurisdictional expansion was specifically pursued in this domain, the beneficiary claim strengthens and the tangled_rope classification for the state-facing sub-mechanism is more secure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_monopoly_natural_vs_constructed, conceptual, 'Whether state monopolization of honor-dispute resolution reflects general state formation or targeted jurisdictional capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__composite_reading, 1780, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1780, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1780, 0.18).
narrative_ontology:measurement(hono_tr_t1810, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1810, 0.24).
narrative_ontology:measurement(hono_tr_t1840, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1840, 0.31).
narrative_ontology:measurement(hono_tr_t1870, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1870, 0.37).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1900, 0.4).
narrative_ontology:measurement(hono_tr_t1920, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1920, 0.42).

% Extraction over time
narrative_ontology:measurement(hono_be_t1780, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1780, 0.34).
narrative_ontology:measurement(hono_be_t1810, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1810, 0.42).
narrative_ontology:measurement(hono_be_t1840, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1840, 0.51).
narrative_ontology:measurement(hono_be_t1870, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1870, 0.57).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(hono_be_t1920, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1920, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1780, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1780, 0.31).
narrative_ontology:measurement(hono_su_t1810, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1810, 0.4).
narrative_ontology:measurement(hono_su_t1840, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1840, 0.48).
narrative_ontology:measurement(hono_su_t1870, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1870, 0.53).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1900, 0.56).
narrative_ontology:measurement(hono_su_t1920, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1920, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__composite_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__contraction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the honor_satisfaction_mechanism kernel. The decline_reading treats dueling's disappearance as gradual frequency reduction to fringe status (lower claimed extraction, weaker enforcement claim). The contraction_reading treats it as sudden category-level cognitive foreclosure (near-mountain-like accessibility_collapse, minimal ongoing enforcement once the category shift completes). This composite_reading claims higher and rising extractiveness because it holds that multiple institutions concurrently captured portions of the satisfaction function rather than the practice simply fading or a single conceptual shift occurring — the beneficiary set here (four distinct institutional actors) has no analog in the other two readings, which is the structural delta that justifies treating these as separate constraints rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
