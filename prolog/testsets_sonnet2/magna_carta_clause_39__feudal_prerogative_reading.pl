% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__feudal_prerogative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__feudal_prerogative_reading, []).

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
 *   constraint_id: magna_carta_clause_39__feudal_prerogative_reading
 *   human_readable: Clause 39 as Baronial Procedural Privilege Within Feudal Hierarchy
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This story instantiates the feudal_prerogative_reading of the Clause 39
 *   kernel: the clause is read as a narrow procedural settlement between the
 *   crown and the baronial/free-tenant class, coordinating an end to armed
 *   revolt by guaranteeing that men of that class could not be seized,
 *   imprisoned, or dispossessed except by lawful judgment of their peers or
 *   the law of the land. Under this reading, 'liber homo' (free man) is a
 *   term of art restricted to the propertied and titled — it does not
 *   contemplate villeins, the unfree, or women outside the baronial class as
 *   protected parties. Extraction against the protected class is low (the
 *   clause genuinely constrains the crown's discretion over barons); but the
 *   arrangement is a tangled rope, not a pure rope, because the same
 *   settlement that protects the baronial class leaves the crown's arbitrary
 *   authority over everyone else fully intact, and the clause requires active
 *   baronial and royal enforcement (peer courts, the barons' own military
 *   leverage) to hold. This is a distinct constraint from the
 *   liberal_due_process_reading (which reads 'liber homo' as proto-universal
 *   and finds high extraction in ANY arbitrary detention regardless of class)
 *   and from the originalist_limitation_reading (which reads the clause as
 *   bounded strictly to the specific 1215 abuses documented against the
 *   rebelling barons, without generalizing to a standing procedural principle
 *   at all). All three share the same text; ε and the victim set differ
 *   sharply by reading, per the ε-invariance principle — hence three separate
 *   constraint files linked by network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__feudal_prerogative_reading, 0.28).
domain_priors:suppression_score(magna_carta_clause_39__feudal_prerogative_reading, 0.62).
domain_priors:theater_ratio(magna_carta_clause_39__feudal_prerogative_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__feudal_prerogative_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__feudal_prerogative_reading, "Clause 39 as Baronial Procedural Privilege Within Feudal Hierarchy").
narrative_ontology:topic_domain(magna_carta_clause_39__feudal_prerogative_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__feudal_prerogative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__feudal_prerogative_reading, '93caf15f-8860-4e51-9da6-a3c6c9771af0').
narrative_ontology:cs_kernel_codification('93caf15f-8860-4e51-9da6-a3c6c9771af0', fixed_text).
narrative_ontology:cs_authority_grounding('93caf15f-8860-4e51-9da6-a3c6c9771af0', lineage).
narrative_ontology:cs_interpretation_layer_present('93caf15f-8860-4e51-9da6-a3c6c9771af0').
narrative_ontology:cs_reading_relation('93caf15f-8860-4e51-9da6-a3c6c9771af0', magna_carta_clause_39__liberal_due_process_reading, coexists_with).
narrative_ontology:cs_reading_relation('93caf15f-8860-4e51-9da6-a3c6c9771af0', magna_carta_clause_39__originalist_limitation_reading, influences).
narrative_ontology:cs_axiom('93caf15f-8860-4e51-9da6-a3c6c9771af0', foundational, liber_homo_denotes_baronial_free_tenant_class).
narrative_ontology:cs_axiom_status(liber_homo_denotes_baronial_free_tenant_class, holdable).
narrative_ontology:cs_axiom_grounding('93caf15f-8860-4e51-9da6-a3c6c9771af0', liber_homo_denotes_baronial_free_tenant_class, conventional).
narrative_ontology:cs_axiom('93caf15f-8860-4e51-9da6-a3c6c9771af0', foundational, feudal_hierarchy_is_legitimate_background_order).
narrative_ontology:cs_axiom_status(feudal_hierarchy_is_legitimate_background_order, overridden).
narrative_ontology:cs_axiom_grounding('93caf15f-8860-4e51-9da6-a3c6c9771af0', feudal_hierarchy_is_legitimate_background_order, conventional).
narrative_ontology:cs_reference_frame('93caf15f-8860-4e51-9da6-a3c6c9771af0', baronial_class_privilege_settlement).
narrative_ontology:cs_drift_state('93caf15f-8860-4e51-9da6-a3c6c9771af0', post_feudal_tenure_collapse, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('93caf15f-8860-4e51-9da6-a3c6c9771af0', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, landed_barons).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, free_tenants_in_chief).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, the_crown).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, villeins_and_unfree_peasants).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, women_outside_baronial_class).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, royal_officials_subject_to_baronial_judgment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, the_crown).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Extracted the clause from King John at Runnymede to guarantee that men of their own rank could not be imprisoned, dispossessed, or destroyed except by lawful judgment of their peers or the law of the land. They administer this protection through baronial courts and peer judgment, and they benefit both as the protected class and as the ones who enforce the standard against the crown and against each other.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, landed_barons, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__feudal_prerogative_reading, landed_barons, agenda_setter).

% Concedes a narrow procedural check on arbitrary seizure of baronial persons and lands in exchange for ending the immediate baronial revolt and preserving the broader structure of royal supremacy over everyone outside the baronial class. The concession costs the crown some discretion over nobles but leaves its authority over villeins, towns, and the general population untouched — a cheap trade that stabilizes the regime.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, the_crown, beneficiary,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__feudal_prerogative_reading, the_crown, payer).

% Hold land directly of the crown and fall within the protected class described by 'liber homo' (free man) as read in this feudal context. They gain the same judgment-by-peers guarantee as the great barons, though with less practical leverage to enforce it against royal encroachment.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, free_tenants_in_chief, beneficiary,
    organized, generational, constrained, national).

% Constitute the overwhelming majority of the population and are excluded from the clause's protection by its own terms in this reading — a villein has no peers within the class the clause defines, and remains subject to seizure, disseisin, and arbitrary judgment at their lord's discretion. The clause's existence changes nothing about their exposure to the manorial and royal power that governs their daily lives.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, villeins_and_unfree_peasants, payer,
    powerless, biographical, trapped, local).

% Structurally absent from the 'free man' category the clause protects in this reading; their legal standing runs through fathers, husbands, or lords rather than through the clause's guarantee. The document was not drafted with them as a contemplated party.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, women_outside_baronial_class, payer,
    powerless, biographical, trapped, local).

% Sheriffs and crown agents who previously seized baronial property or persons on the king's instruction now face a check: baronial peer judgment can find such seizures unlawful. Their operational latitude against the baronial class specifically is curtailed, though their authority over everyone else is untouched.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, royal_officials_subject_to_baronial_judgment, payer,
    moderate, biographical, constrained, national).

% Examine the 1215 text, the immediate political context of baronial revolt, and later reissues to determine whether the clause was, at inception, a narrow class privilege or an inchoate universal principle later expanded by interpretation. Their scholarship is contested territory between this reading and its liberal and originalist siblings.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__feudal_prerogative_reading, landed_barons).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__feudal_prerogative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a truce between the crown and the baronial class: it establishes a peer-judgment procedure that lets barons trust the crown will not arbitrarily seize their persons or estates, in exchange for the barons ceasing armed revolt and continuing to supply the crown with military and financial support.
% TRANSFER_FUNCTION: Moves a specific procedural guarantee — protection from seizure without lawful judgment — to the baronial and free-tenant class, while leaving the crown's arbitrary authority over villeins, women outside the baronial class, and the unfree population fully intact. Nothing is transferred to those outside the protected class; if anything, the crown's practical latitude to extract from the unprotected population is undisturbed, so the clause functions as an internal settlement among the propertied and titled.
% ABSENT_VOICES: Villeins, unfree peasants, and women outside the baronial class would object that 'liber homo' excludes them from any real protection, but they are not parties to the Runnymede negotiation and leave no documentary trace of dissent from this arrangement; their absence is total, not merely underrepresented.
% DISAPPEARANCE_RATIONALE: If this clause, read narrowly, vanished, the baronial class would lose a specific procedural check against arbitrary crown seizure and would likely revert to the pre-1215 pattern of ad hoc negotiated protection or renewed revolt — a real rearrangement for that class. But for the overwhelming majority of the population outside the protected class, the clause's disappearance changes nothing observable, since it never governed their exposure to arbitrary power in the first place. The verdict is contested because it depends entirely on which class's world you are asking about.
% FOUNDING_PROBLEM: King John had been seizing baronial lands, imprisoning nobles, and disinheriting them without any process the barons recognized as legitimate, provoking armed rebellion; the clause was built to solve the specific problem of unchecked royal action against the propertied and titled class whose military and financial cooperation the crown needed.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the beneficiary class (e.g., scholarship tracing the 1215 baronial articles and comparing them to earlier charters of liberties) corroborate that the specific 1215 crisis — a king seizing baronial holdings without peer process — no longer exists as a live threat to any modern polity; the crown's arbitrary seizure power over titled nobles, in the form the clause addressed, has been extinguished by the collapse of feudal tenure itself, not by the clause's continued operation. No corroborating source contends the 13th-century baronial grievance remains an active problem today.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__feudal_prerogative_reading, contested).
narrative_ontology:founding_problem_status(magna_carta_clause_39__feudal_prerogative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__feudal_prerogative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_clause_39__feudal_prerogative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__feudal_prerogative_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).
:- end_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction against the protected baronial class is low (0.28 by 1300) and rises only slowly as feudal tenure itself erodes and the crown finds new levers; suppression is comparatively high (0.62-0.70) because the clause's benefit for the baronial class depends on the ongoing threat of baronial military coordination and peer-court enforcement against a crown that would otherwise seize at will. Theater ratio is low and rising slowly (0.10 to 0.20): the peer-judgment machinery was functionally real at inception, with modest performative drift as the baronial class itself changed composition. All three tracked metrics are authored on the shared 1215-1300 grid.
 *
 * DIRECTIONALITY LOGIC:
 *   The barons and free tenants-in-chief are the primary beneficiaries — the clause was drafted for them and by their pressure, so directionality sits near the beneficiary end (constraint subsidizes their security of tenure and person). The crown is a secondary beneficiary in this reading: it pays a narrow cost (reduced discretion over the baronial class) to buy a much larger benefit (ending the revolt, preserving unrestricted authority over the rest of the population) — hence its role is coded beneficiary/payer dual. Villeins, unfree peasants, and women outside the baronial class are structurally outside the protected term 'liber homo' in this reading, so they are victims not because the clause actively extracts from them, but because it does nothing to check the arbitrary power still exercised over them while conspicuously legitimizing the arrangement as a rights-bearing document. Royal officials experience a genuine, if narrow, constraint on their latitude specifically against the baronial class.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — King John's specific pattern of seizing baronial estates without peer process — is dead: feudal tenure of the kind the clause addressed no longer exists anywhere. Under this reading the clause's mandate has plainly outlived its function for the baronial class it was written for, since that class itself has dissolved into the modern polity; whatever protective work the clause still does is inherited by a much larger population than it was drafted to cover, which is precisely the interpretive expansion this reading refuses to grant. Reading Clause 39 narrowly prevents mislabeling a class-restricted settlement as evidence of a universal coordination function it never possessed at inception — that mislabeling is exactly the work the liberal_due_process_reading performs, and the two readings are engineered to diverge on this point.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liber_homo_scope_ambiguity,
    'Did the 1215 drafters and signatories understand ''liber homo'' (free man) as a term restricted to the baronial and free-tenant class, or as a category with latent universalizing potential later realized through reinterpretation?',
    'Comparative philological and legal-historical analysis of contemporaneous charters of liberties, the Articles of the Barons, and 13th-century usage of ''liber homo'' in analogous continental instruments, cross-checked against the composition of parties actually present at Runnymede.',
    'If the term was understood narrowly at inception (supporting this reading), the clause''s low-extraction, class-restricted classification stands as historically grounded. If latent universalizing intent can be established, this reading becomes harder to sustain as the ''original'' reading and shifts toward being one contested interpretation among equals rather than the historically primary one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liber_homo_scope_ambiguity, conceptual, 'Whether ''free man'' was drafted as a narrow class term or a term with latent universal scope.').

omega_variable(
    kernel_reading_divergence_locus,
    'Where exactly does this reading''s classification diverge from the liberal_due_process_reading — is it in the scope of the protected class (who counts as ''liber homo''), the function of ''law of the land'' (procedural floor vs. substantive limit), or both?',
    'This is committer structure, not resolvable by new evidence about 1215 alone — it requires declaring which interpretive commitment each reading holds fixed. Documented here per Rule 2 rather than folded into this constraint''s own ε.',
    'The divergence locus determines whether the two readings are compatible at different levels of description (this reading describing 1215 practice, the liberal reading describing later doctrinal accretion) or genuinely incompatible claims about the same historical fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence_locus, conceptual, 'Locating the precise structural disagreement between this reading and the liberal_due_process_reading.').

omega_variable(
    crown_net_beneficiary_status,
    'Is the crown better modeled as a net beneficiary of this settlement (buying stability cheaply) or as a genuine payer that lost meaningful discretionary power over a militarily significant class?',
    'Analysis of subsequent royal behavior (reissues, John''s repudiation attempt, Henry III''s re-confirmations) to see whether the crown treated the concession as binding or as a temporary concession to be reversed when leverage returned.',
    'If the crown treated it as a genuine binding loss, its dual beneficiary/payer coding understates its cost; if it treated it as a reversible concession (as John''s immediate repudiation suggests), the crown''s beneficiary coding is the more accurate primary role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crown_net_beneficiary_status, empirical, 'Whether the crown''s concession was a real transfer of power or a reversible tactical retreat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__feudal_prerogative_reading, 1215, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1225, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1225, 0.12).
narrative_ontology:measurement(magn_tr_t1237, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1237, 0.14).
narrative_ontology:measurement(magn_tr_t1250, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1250, 0.16).
narrative_ontology:measurement(magn_tr_t1275, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1275, 0.18).
narrative_ontology:measurement(magn_tr_t1300, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1300, 0.2).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1215, 0.15).
narrative_ontology:measurement(magn_be_t1225, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1225, 0.18).
narrative_ontology:measurement(magn_be_t1237, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1237, 0.2).
narrative_ontology:measurement(magn_be_t1250, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1250, 0.23).
narrative_ontology:measurement(magn_be_t1275, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1275, 0.26).
narrative_ontology:measurement(magn_be_t1300, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1300, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1215, 0.7).
narrative_ontology:measurement(magn_su_t1225, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1225, 0.68).
narrative_ontology:measurement(magn_su_t1237, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1237, 0.66).
narrative_ontology:measurement(magn_su_t1250, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1250, 0.64).
narrative_ontology:measurement(magn_su_t1275, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1275, 0.63).
narrative_ontology:measurement(magn_su_t1300, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1300, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__feudal_prerogative_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints emitted from the single kernel magna_carta_clause_39, per the ε-invariance principle (a colloquial label — 'Clause 39' — covering structurally distinct claims about scope and function). This reading (feudal_prerogative_reading) authors low extractiveness (0.28) against a narrow protected class of barons and free tenants-in-chief. The liberal_due_process_reading authors substantially higher extractiveness against a universal population subject to arbitrary detention. The originalist_limitation_reading authors the narrowest scope of all, refusing even this reading's generalization beyond the specific 1215 abuses. All three share the same text and interval but diverge in beneficiary/victim structure, ε, and claimed type; they are linked here and must each document the relationship in their own commentary.narrative_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
