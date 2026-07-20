% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__functional_protection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__functional_protection_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: combatant_status_definition__functional_protection_reading
 *   human_readable: Common Article 3 Functional Protection Floor
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   The functional protection reading of combatant status holds that Common
 *   Article 3 to the Geneva Conventions applies to all persons detained in
 *   armed conflict regardless of combatant classification, providing a
 *   universal floor of humane treatment and fair trial rights. This reading
 *   removes status determination as a precondition for protection, rendering
 *   the constraint operative by factual situation alone. In the kernel
 *   family, it coexists with the state-centric reading (which reserves full
 *   combatant privileges for state military personnel meeting Article 4
 *   criteria) and the national liberation reading (which extends combatant
 *   status to anti-colonial movements under AP I Article 1(4)). The
 *   structural delta is low extractiveness for detainees and the elimination
 *   of status gatekeeping.
 *
 * KEY AGENTS:
 *   - detained_persons: Primary beneficiary (powerless/trapped) â receive status-independent protections by factual situation
 *   - state_detaining_parties: Primary payer/target (institutional/constrained) â bear compliance costs and sovereignty constraints
 *   - non_state_armed_groups: Secondary payer (organized/constrained) â bound to humane treatment without state infrastructure
 *   - international_criminal_courts: Analytical observer (institutional/analytical) â adjudicate violations and interpret scope
 *   - icrc: Analytical observer (institutional/analytical) â monitors detention and promotes the functional protection reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__functional_protection_reading, 0.08).
domain_priors:suppression_score(combatant_status_definition__functional_protection_reading, 0.35).
domain_priors:theater_ratio(combatant_status_definition__functional_protection_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__functional_protection_reading, mountain).
narrative_ontology:human_readable(combatant_status_definition__functional_protection_reading, "Common Article 3 Functional Protection Floor").
narrative_ontology:topic_domain(combatant_status_definition__functional_protection_reading, "legal/international_humanitarian_law").

domain_priors:emerges_naturally(combatant_status_definition__functional_protection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__functional_protection_reading, 'e6fd2c98-3cd2-436b-8a95-d07650175454').
narrative_ontology:cs_kernel_codification('e6fd2c98-3cd2-436b-8a95-d07650175454', formalized).
narrative_ontology:cs_authority_grounding('e6fd2c98-3cd2-436b-8a95-d07650175454', lineage).
narrative_ontology:cs_interpretation_layer_present('e6fd2c98-3cd2-436b-8a95-d07650175454').
narrative_ontology:cs_reading_relation('e6fd2c98-3cd2-436b-8a95-d07650175454', combatant_status_definition__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('e6fd2c98-3cd2-436b-8a95-d07650175454', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_axiom('e6fd2c98-3cd2-436b-8a95-d07650175454', foundational, status_independent_humanitarian_floor).
narrative_ontology:cs_axiom_status(status_independent_humanitarian_floor, holdable).
narrative_ontology:cs_axiom_grounding('e6fd2c98-3cd2-436b-8a95-d07650175454', status_independent_humanitarian_floor, deontological).
narrative_ontology:cs_axiom('e6fd2c98-3cd2-436b-8a95-d07650175454', foundational, universal_fair_trial_guarantee).
narrative_ontology:cs_axiom_status(universal_fair_trial_guarantee, holdable).
narrative_ontology:cs_axiom_grounding('e6fd2c98-3cd2-436b-8a95-d07650175454', universal_fair_trial_guarantee, deontological).
narrative_ontology:cs_reference_frame('e6fd2c98-3cd2-436b-8a95-d07650175454', geneva_convention_minimum_floor).
narrative_ontology:cs_drift_state('e6fd2c98-3cd2-436b-8a95-d07650175454', contemporary_counterterrorism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e6fd2c98-3cd2-436b-8a95-d07650175454', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__functional_protection_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, detained_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(combatant_status_definition__functional_protection_reading, state_detaining_parties).
narrative_ontology:constraint_victim(combatant_status_definition__functional_protection_reading, non_state_armed_groups).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, customary_international_law_universality).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, elementary_considerations_of_humanity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons held by parties to armed conflicts who receive baseline protections of humane treatment and fair trial rights without needing to prove combatant status, civilian status, or any other legal classification. They cannot opt out of detention or the protections; the constraint applies to them by factual situation alone.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, detained_persons, beneficiary,
    powerless, immediate, trapped, universal).

% States that detain persons in armed conflict and must provide food, medical care, humane quarters, and fair trials regardless of detainee status. Their sovereignty over military and security policy is constrained by this universal floor, and they bear the operational and political costs of compliance, including refraining from interrogation methods they might otherwise employ.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, state_detaining_parties, payer,
    institutional, generational, constrained, global).

% Organized armed groups in non-international conflicts that detain persons and are legally bound to the same humane treatment standards as states, despite lacking state infrastructure and often operating clandestinely. Their operational security practices are constrained by the prohibition on torture and summary execution.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, non_state_armed_groups, payer,
    organized, biographical, constrained, national).

% International tribunals and the International Criminal Court that adjudicate violations of Common Article 3, interpret its scope, and enforce the functional protection reading through individual criminal responsibility for war crimes. They sit outside the beneficiary-payer dynamic and assess compliance from an analytical seat.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, international_criminal_courts, observer,
    institutional, generational, analytical, global).

% The International Committee of the Red Cross monitors detention conditions, promotes the functional protection reading through its Customary International Humanitarian Law study, and engages bilaterally with states and non-state actors to secure access to detainees. It neither benefits from nor pays for the constraint's operation.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, icrc, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universally applicable minimum standard of humane treatment in armed conflict, removing the need for costly and often politicized status determinations before protections attach, thereby coordinating expectations among all parties to any conflict about the baseline treatment of detainees.
% TRANSFER_FUNCTION: Moves obligations of humane treatment, medical care, and fair trial guarantees from detaining authorities to detained persons based solely on the factual situation of detention, without requiring any status-based gatekeeping or reciprocal claim.
% ABSENT_VOICES: States operating secret detention and rendition programs; intelligence services advocating for enhanced interrogation exceptions; non-state armed groups that reject international legal frameworks entirely; detainees in undisclosed facilities whose existence is denied by detaining authorities.
% DISAPPEARANCE_RATIONALE: The disappearance of this constraint would remove the absolute floor of the law of armed conflict. Every detention regime would need to reconstruct its legal basis for humane treatment; international criminal law would lose its foundational war crime definitions for non-international conflicts; and the reciprocal expectation of minimum treatment that stabilizes asymmetric conflicts would dissolve, forcing a complete rearrangement of military detention practice and human rights monitoring.
% FOUNDING_PROBLEM: In the first half of the twentieth century, persons held in non-international armed conflicts and non-state actors in international conflicts were systematically denied legal protections, subjected to torture and summary execution, because no treaty bound states to treat them humanely and no status classification covered them.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of the Spanish Civil War and post-World War II colonial conflicts document the absence of protections. The ICRC's 1952 Commentary and the ICTY TadiÄ Jurisdiction Decision (1995) corroborate the ongoing need for a status-independent floor from outside the detainee beneficiary seat. The UN Secretary-General and regional human rights bodies independently attest that the problem persists in contemporary conflicts.
narrative_ontology:disappearance_verdict(combatant_status_definition__functional_protection_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__functional_protection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__functional_protection_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(combatant_status_definition__functional_protection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__functional_protection_reading, 0.08, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__functional_protection_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, ExtMetricName, E),
    domain_priors:suppression_score(combatant_status_definition__functional_protection_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(combatant_status_definition__functional_protection_reading),
    narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(combatant_status_definition__functional_protection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.08 because the constraint does not extract from detainees; it imposes obligations on detaining authorities. The minimal extraction reflects the compliance burden on authorities, which is low relative to total military capacity but real in terms of constrained interrogation and detention practices. Suppression is authored at 0.35 because the constraint's persistence against state security interests requires active legal enforcement through international criminal tribunals, universal jurisdiction prosecutions, and diplomatic pressure; it is not self-executing in the absence of enforcement infrastructure. Accessibility collapse is 0.92 because once the norm is understood, no lawful alternative to humane treatment remains available â the legal space for torture or summary execution collapses completely. Resistance is 0.10 because no state openly repudiates Common Article 3; resistance is covert, framed as interpretation rather than rejection. Theater ratio at 0.25 reflects the post-2001 emergence of legal fictions (enemy combatant designations, black sites, enhanced interrogation memos) that perform compliance with Article 3 while violating its substance. The measurement series share one time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The detaining authority seat experiences the constraint as an externally imposed limitation on sovereignty and military necessity â a coercive legal burden that increases compliance costs and restricts operational discretion. The detainee seat experiences the same constraint as a baseline entitlement that should require no enforcement. The analytical seats (courts, ICRC) see the constraint as a foundational norm whose breach triggers individual criminal responsibility. The engine computes these divergent seat types from the same structural data: low power plus trapped exit produces a beneficiary-classification for detainees; institutional power plus constrained exit produces a target-classification for states.
 *
 * DIRECTIONALITY LOGIC:
 *   Detained persons are structural beneficiaries (d near 0.0): the constraint exists to transfer protections to them unconditionally. State and non-state detaining parties are structural targets (d near 1.0): their operational freedom is constrained, and they bear the costs of compliance. International courts and the ICRC sit near 0.5 (analytical/symmetric): they neither benefit from nor pay for the constraint but administer its interpretation. The directionality derivation from beneficiary declarations places detainees at the subsidy end and detaining authorities at the extraction end.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mandatrophy mislabeling by separating the Article 3 floor from the higher-status combatant privilege debates. The state-centric and national liberation readings contest POW status and combatant privilege â questions involving asymmetric extraction and coordination. The functional protection reading refuses to let those higher-order disputes dissolve the minimum floor. Classifying the floor as mountain while the status-determination machinery above it operates as tangled rope prevents the error of treating the entire combatant-status architecture as uniformly extractive or uniformly coordinative. The floor remains live even when the superstructure is contested or has atrophied into piton-like performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ihl_mountain_ambiguity,
    'Is Common Article 3 a genuinely irreducible norm emerging from universal human dignity, or a constructed legal constraint that happens to benefit detainees while constraining state sovereignty?',
    'Comparative analysis of compliance patterns across failed states and asymmetric conflicts: if the constraint persists even where enforcement and reciprocity break down, it approaches mountain-like status; if compliance collapses without enforcement, it is constructed.',
    'If constructed, the mountain claim is a false summit and the constraint reclassifies toward rope or tangled rope; if genuinely irreducible, the beneficiary-triggered FSM is a false positive and the mountain claim holds despite declared beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ihl_mountain_ambiguity, conceptual, 'Whether the Article 3 floor is natural law or constructed treaty norm').

omega_variable(
    enforcement_vs_internalization,
    'Does the constraint persist through external enforcement (international courts, sanctions, diplomatic pressure) or through internalization by military legal cultures and command responsibility structures?',
    'Measure detention practice variance between jurisdictions with and without strong external enforcement; if variance is low, internalization dominates and suppression is lower than structurally measured.',
    'High internalization would lower measured suppression and support mountain or rope classification; pure external enforcement would raise suppression and suggest a more coercively maintained constraint type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_internalization, empirical, 'External enforcement versus norm internalization mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__functional_protection_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(combatant_status_functional_tr_t0, combatant_status_definition__functional_protection_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(combatant_status_functional_tr_t15, combatant_status_definition__functional_protection_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(combatant_status_functional_tr_t30, combatant_status_definition__functional_protection_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(combatant_status_functional_tr_t45, combatant_status_definition__functional_protection_reading, theater_ratio, 45, 0.18).
narrative_ontology:measurement(combatant_status_functional_tr_t55, combatant_status_definition__functional_protection_reading, theater_ratio, 55, 0.28).
narrative_ontology:measurement(combatant_status_functional_tr_t65, combatant_status_definition__functional_protection_reading, theater_ratio, 65, 0.3).
narrative_ontology:measurement(combatant_status_functional_tr_t75, combatant_status_definition__functional_protection_reading, theater_ratio, 75, 0.25).

% Extraction over time
narrative_ontology:measurement(combatant_status_functional_be_t0, combatant_status_definition__functional_protection_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(combatant_status_functional_be_t15, combatant_status_definition__functional_protection_reading, base_extractiveness, 15, 0.05).
narrative_ontology:measurement(combatant_status_functional_be_t30, combatant_status_definition__functional_protection_reading, base_extractiveness, 30, 0.06).
narrative_ontology:measurement(combatant_status_functional_be_t45, combatant_status_definition__functional_protection_reading, base_extractiveness, 45, 0.07).
narrative_ontology:measurement(combatant_status_functional_be_t55, combatant_status_definition__functional_protection_reading, base_extractiveness, 55, 0.08).
narrative_ontology:measurement(combatant_status_functional_be_t65, combatant_status_definition__functional_protection_reading, base_extractiveness, 65, 0.08).
narrative_ontology:measurement(combatant_status_functional_be_t75, combatant_status_definition__functional_protection_reading, base_extractiveness, 75, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(combatant_status_functional_su_t0, combatant_status_definition__functional_protection_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(combatant_status_functional_su_t15, combatant_status_definition__functional_protection_reading, suppression_requirement, 15, 0.25).
narrative_ontology:measurement(combatant_status_functional_su_t30, combatant_status_definition__functional_protection_reading, suppression_requirement, 30, 0.2).
narrative_ontology:measurement(combatant_status_functional_su_t45, combatant_status_definition__functional_protection_reading, suppression_requirement, 45, 0.35).
narrative_ontology:measurement(combatant_status_functional_su_t55, combatant_status_definition__functional_protection_reading, suppression_requirement, 55, 0.55).
narrative_ontology:measurement(combatant_status_functional_su_t65, combatant_status_definition__functional_protection_reading, suppression_requirement, 65, 0.5).
narrative_ontology:measurement(combatant_status_functional_su_t75, combatant_status_definition__functional_protection_reading, suppression_requirement, 75, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__national_liberation_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'combatant status' conflates three structurally distinct constraints: (1) the functional protection floor (this story), which applies universally regardless of status; (2) the state-centric reading, which restricts privileged combatant status to state militaries; and (3) the national liberation reading, which extends privileged status to certain non-state conflicts. These readings share a kernel but have different epsilon values, beneficiary structures, and failure modes. They are modeled as separate linked stories, not as one constraint with measurement parameters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
