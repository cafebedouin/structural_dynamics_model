% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__contraction_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor Definition Excluding Violence (Contraction Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the CONTRACTION READING of the
 *   honor_violence_legitimacy kernel: the claim that dueling became
 *   structurally unthinkable because the very concept of honor was redefined
 *   to exclude violence. This is not a story about dueling declining due to
 *   external costs (that is the DROP READING) nor about both forces operating
 *   simultaneously (that is the COMPOSITE READING). Here, the conceptual
 *   shift IS the constraint — honor's internal logic contracted, expelling
 *   violence from its legitimate repertoire. The constraint presents as a
 *   mountain (conceptual necessity) but carries beneficiaries (state, honor
 *   custodians) whose interests aligned with the redefinition, triggering FSM
 *   scrutiny.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.12).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.25).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, mountain).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor Definition Excluding Violence (Contraction Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:emerges_naturally(honor_violence_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, 'cc1ced46-db0b-4361-b3a6-4d6386d71760').
narrative_ontology:cs_kernel_codification('cc1ced46-db0b-4361-b3a6-4d6386d71760', distributed).
narrative_ontology:cs_authority_grounding('cc1ced46-db0b-4361-b3a6-4d6386d71760', practice).
narrative_ontology:cs_interpretation_layer_present('cc1ced46-db0b-4361-b3a6-4d6386d71760').
narrative_ontology:cs_reading_relation('cc1ced46-db0b-4361-b3a6-4d6386d71760', honor_violence_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('cc1ced46-db0b-4361-b3a6-4d6386d71760', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('cc1ced46-db0b-4361-b3a6-4d6386d71760', foundational, honor_essentially_nonviolent).
narrative_ontology:cs_axiom_status(honor_essentially_nonviolent, holdable).
narrative_ontology:cs_axiom_grounding('cc1ced46-db0b-4361-b3a6-4d6386d71760', honor_essentially_nonviolent, deontological).
narrative_ontology:cs_axiom('cc1ced46-db0b-4361-b3a6-4d6386d71760', secondary, legitimate_honor_satisfaction_requires_legal_process).
narrative_ontology:cs_axiom_status(legitimate_honor_satisfaction_requires_legal_process, holdable).
narrative_ontology:cs_axiom_grounding('cc1ced46-db0b-4361-b3a6-4d6386d71760', legitimate_honor_satisfaction_requires_legal_process, conventional).
narrative_ontology:cs_reference_frame('cc1ced46-db0b-4361-b3a6-4d6386d71760', aristocratic_violence_honor).
narrative_ontology:cs_drift_state('cc1ced46-db0b-4361-b3a6-4d6386d71760', long_nineteenth_century, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('cc1ced46-db0b-4361-b3a6-4d6386d71760', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, honor_definition_custodians).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, state_legal_monopoly).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, traditional_duelists).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, honor_is_incompatible_with_private_violence).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, legal_monopoly_on_violence_is_honorable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Aristocratic codes, judicial bodies, and intellectual elites who authored the redefinition of honor from 'readiness for violence' to 'moral integrity and legal standing.' They control the symbolic capital that makes the new definition authoritative and benefit from the monopoly on legitimate status-conferral.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, honor_definition_custodians, agenda_setter,
    institutional, generational, analytical, continental).

% The emerging sovereign state apparatus that claims exclusive legitimate violence. The redefinition of honor to exclude private violence removes a rival claimant to legitimate force and aligns aristocratic honor with state law, reducing enforcement costs.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, state_legal_monopoly, beneficiary,
    institutional, generational, arbitrage, continental).

% Aristocratic and military men whose honor identity was constituted through the capacity and willingness to duel. The redefinition renders their core practice illegitimate, stripping status from their primary identity investment. Exit means abandoning the honor framework that structured their social world.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, traditional_duelists, payer,
    organized, biographical, constrained, national).

% Jurists and legislators who codified the new honor definition into law (dueling bans, satisfaction-of-honor-through-courts). They observe the constraint's operation from the institutional seat that translates conceptual change into legal force.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, legal_reformers, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of private violence by providing a shared, non-violent definition of honor that allows status disputes to be resolved through legal and social processes rather than lethal encounters. The constraint coordinates expectations: what counts as honorable conduct, how insults are answered, how status is maintained without killing.
% TRANSFER_FUNCTION: Transfers the legitimate resolution of honor disputes from private violence (dueling) to public legal processes (courts, codes of conduct, state-sanctioned satisfaction). The 'cost' transferred is the aristocratic monopoly on honor-enforcement; the 'gain' is state monopoly on legitimate violence and a pacified public sphere.
% ABSENT_VOICES: The traditional duelists themselves — the class whose identity was most invested in the old definition — were not consulted in the redefinition. Their objection would have been that honor without the capacity for violence is not honor at all, but mere reputation. They are structurally excluded because the new definition defines them out of the conversation.
% DISAPPEARANCE_RATIONALE: If the conceptual constraint vanished overnight — if honor were redefined back to include violence — dueling would not instantly return (material conditions have changed), but the symbolic barrier to its legitimacy would collapse. The legal prohibitions would lose their moral foundation, and the state's claim that its violence monopoly is 'honorable' would face a rival claimant. The aristocratic class would regain a legitimate path to status-enforcement outside state channels.
% FOUNDING_PROBLEM: The founding problem was the escalating lethality and social disruption of aristocratic dueling culture in early modern Europe (16th-18th centuries), where honor disputes killed disproportionate numbers of military-elite men, undermined state authority, and made noble status a death sentence. The arrangement was built to preserve the aristocracy as a ruling class by decoupling its honor from self-destructive violence.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary state-builders (Frederick the Great, Louis XIV's intendants, Prussian reformers) explicitly cited dueling's drain on military manpower and challenge to sovereign authority. Modern historians (Kiernan, McAleer, Banks) corroborate from outside the benefiting parties that the redefinition served state-building and aristocratic survival simultaneously — the 'honor custodians' and 'state' were not distinct beneficiaries but a fused elite.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_violence_legitimacy__contraction_reading),
    narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Low extractiveness (0.12) and suppression (0.25) at interval end reflect that by 1900 the constraint operates as a settled conceptual fact — no one advocates dueling, the alternative (legal honor) is fully institutionalized. High accessibility_collapse (0.92) and low resistance (0.08) match a mountain profile: once the redefinition is understood, the old conception of honor-as-violence-readiness becomes cognitively inaccessible. Theater ratio (0.18) is low but non-zero: residual performative dueling (student mensur, ceremonial challenges) persists as theater. The measurement series shows the constraint's formation phase (1600-1750) where extraction and suppression were higher as the redefinition was actively enforced against resistant duelists, then a long tail of conceptual consolidation.
 *
 * PERSPECTIVAL GAP:
 *   From the custodian/state seat, the constraint looks like a natural evolution of honor toward its true essence (moral integrity). From the duelist seat, it looks like a hostile takeover of the honor concept by state power. The engine's per-seat classification will capture this: mountain for beneficiaries/observers, snare or tangled_rope for payers. The FSM signature will flag the mountain claim with beneficiaries present.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor definition custodians and state legal monopoly are beneficiaries (d ~0.1-0.2): they gain symbolic authority and enforcement monopoly from the constraint. Traditional duelists are payers (d ~0.8-0.9): their identity-capital is expropriated by the redefinition; exit is constrained because their honor identity is fused with the old definition (identity_locked dynamics). Legal reformers are observers (d ~0.5). The constraint's mountain-like appearance from the beneficiary/observer seats contrasts with the extractive experience of the payer seat — the engine computes this divergence from the structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (dueling's lethality threatening aristocratic survival and state authority) remains LIVE — the constraint has not outlived its function. The redefinition successfully pacified the aristocracy and consolidated state violence monopoly. Mandatrophy is not resolved; the constraint continues to serve its coordination function. However, the FSM-relevant question persists: was the conceptual contraction a genuine discovery of honor's true nature, or a constructed redefinition that benefited identifiable powers? The omega variables capture this ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_naturalness_vs_construction,
    'Is the exclusion of violence from honor a genuine conceptual discovery (honor''s true nature is non-violent) or a constructed redefinition serving identifiable power interests?',
    'Counterfactual genealogy: trace whether the non-violent honor concept has pre-existing roots in the tradition (Stoic, Christian, juridical) that were independently authoritative, or whether it was assembled ad hoc to solve the dueling crisis. Cross-kernel comparison with drop_reading and composite_reading on the same evidence base.',
    'If constructed, the mountain claim is a false summit — the constraint is a tangled_rope (coordination + extraction) or snare (pure extraction) disguised as natural law. FSM signature would reclassify. If genuine discovery, mountain stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_naturalness_vs_construction, conceptual, 'Whether honor''s non-violent definition is discovered or constructed — the core FSM ambiguity.').

omega_variable(
    fsme_bias_in_legacy_sources,
    'Do our historical sources overrepresent the contraction_reading because the beneficiaries (state, custodians) controlled the archive?',
    'Source criticism: identify surviving duelist voices (memoirs, correspondence, pamphlets) that contest the redefinition. Assess whether the ''consensus'' on honor''s new definition is an artifact of archival power.',
    'If the consensus is archivally manufactured, the constraint''s low resistance and high accessibility_collapse are artifacts of suppression, not naturalness. Mountain certification would be invalid.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fsme_bias_in_legacy_sources, empirical, 'Archival bias toward the winning reading''s self-justification.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the contraction_reading logically foreclose the drop_reading within a single explanatory framework, or do they coexist as competing partial explanations?',
    'Formalize the logical structure: if honor''s definition contracted (contraction), can dueling simultaneously remain ''legitimate but rare'' (drop)? The drop_reading requires honor''s definition to be stable; contraction_reading requires it to change. These are logically incompatible in a single framework — forecloses relation is justified.',
    'Confirms reading_relations: contraction_reading forecloses drop_reading. Composite_reading is a meta-reading that holds both as sequential or domain-specific, not simultaneous in the same domain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Logical relationship between contraction and drop readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 1600, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1600, honor_violence_legitimacy__contraction_reading, theater_ratio, 1600, 0.45).
narrative_ontology:measurement(hono_tr_t1650, honor_violence_legitimacy__contraction_reading, theater_ratio, 1650, 0.38).
narrative_ontology:measurement(hono_tr_t1700, honor_violence_legitimacy__contraction_reading, theater_ratio, 1700, 0.3).
narrative_ontology:measurement(hono_tr_t1750, honor_violence_legitimacy__contraction_reading, theater_ratio, 1750, 0.24).
narrative_ontology:measurement(hono_tr_t1800, honor_violence_legitimacy__contraction_reading, theater_ratio, 1800, 0.2).
narrative_ontology:measurement(hono_tr_t1850, honor_violence_legitimacy__contraction_reading, theater_ratio, 1850, 0.18).
narrative_ontology:measurement(hono_tr_t1900, honor_violence_legitimacy__contraction_reading, theater_ratio, 1900, 0.18).

% Extraction over time
narrative_ontology:measurement(hono_be_t1600, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1600, 0.35).
narrative_ontology:measurement(hono_be_t1650, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1650, 0.28).
narrative_ontology:measurement(hono_be_t1700, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1700, 0.22).
narrative_ontology:measurement(hono_be_t1750, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1750, 0.18).
narrative_ontology:measurement(hono_be_t1800, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1800, 0.14).
narrative_ontology:measurement(hono_be_t1850, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1850, 0.13).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1900, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1600, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1600, 0.65).
narrative_ontology:measurement(hono_su_t1650, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1650, 0.55).
narrative_ontology:measurement(hono_su_t1700, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1700, 0.42).
narrative_ontology:measurement(hono_su_t1750, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1750, 0.33).
narrative_ontology:measurement(hono_su_t1800, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1800, 0.28).
narrative_ontology:measurement(hono_su_t1850, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1850, 0.26).
narrative_ontology:measurement(hono_su_t1900, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1900, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__contraction_reading, 0.08).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the honor_violence_legitimacy kernel into three readings with distinct ε values and structural profiles. Contraction_reading (this file) has low ε, high conceptual naturalness, mountain claim — the internalist explanation. Drop_reading has moderate ε, externalist explanation. Composite_reading has higher ε, overdetermined explanation. They are linked via affects_constraints to enable contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_violence_legitimacy__contraction_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
