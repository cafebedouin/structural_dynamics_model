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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: Magna Carta Clause 39 â Feudal Prerogative Reading
 *   domain: constitutional/legal/historical
 *
 * SUMMARY:
 *   This is the feudal-prerogative reading of Magna Carta Clause 39,
 *   interpreting the text as a narrow guarantee of procedural security for
 *   elite feudal peers within an established hierarchical order. The clause
 *   is read not as a universal right against arbitrary power, but as a
 *   reciprocal compact between Crown and baronage that preserves traditional
 *   authority over non-elites. The kernel is contested: the liberal
 *   due-process reading universalizes the protection, while the originalist
 *   reading limits it to documented 1215 abuses. This story instantiates only
 *   the feudal reading.
 *
 * KEY AGENTS:
 *   - Baronial peers: Primary beneficiaries and agenda-setters (powerful/mobile) â gain procedural security against arbitrary royal seizure.
 *   - Crown: Primary payer and secondary beneficiary (institutional/constrained) â loses arbitrary prerogative over peers but preserves hierarchy over unfree subjects.
 *   - Unfree subjects: Excluded (powerless/trapped) â denied protection, structurally absent from charter framing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__feudal_prerogative_reading, 0.25).
domain_priors:suppression_score(magna_carta_clause_39__feudal_prerogative_reading, 0.35).
domain_priors:theater_ratio(magna_carta_clause_39__feudal_prerogative_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__feudal_prerogative_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__feudal_prerogative_reading, "Magna Carta Clause 39 â Feudal Prerogative Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__feudal_prerogative_reading, "constitutional/legal/historical").

domain_priors:requires_active_enforcement(magna_carta_clause_39__feudal_prerogative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__feudal_prerogative_reading, 'a387e404-58ec-4610-8528-06604f3ed525').
narrative_ontology:cs_kernel_codification('a387e404-58ec-4610-8528-06604f3ed525', fixed_text).
narrative_ontology:cs_authority_grounding('a387e404-58ec-4610-8528-06604f3ed525', lineage).
narrative_ontology:cs_interpretation_layer_present('a387e404-58ec-4610-8528-06604f3ed525').
narrative_ontology:cs_reading_relation('a387e404-58ec-4610-8528-06604f3ed525', magna_carta_clause_39__liberal_due_process_reading, coexists_with).
narrative_ontology:cs_reading_relation('a387e404-58ec-4610-8528-06604f3ed525', magna_carta_clause_39__originalist_limitation_reading, influences).
narrative_ontology:cs_axiom('a387e404-58ec-4610-8528-06604f3ed525', foundational, procedural_rights_are_privilege_of_free_men).
narrative_ontology:cs_axiom_status(procedural_rights_are_privilege_of_free_men, holdable).
narrative_ontology:cs_axiom_grounding('a387e404-58ec-4610-8528-06604f3ed525', procedural_rights_are_privilege_of_free_men, conventional).
narrative_ontology:cs_axiom('a387e404-58ec-4610-8528-06604f3ed525', secondary, crown_prerogative_limited_only_vis_a_vis_peers).
narrative_ontology:cs_axiom_status(crown_prerogative_limited_only_vis_a_vis_peers, holdable).
narrative_ontology:cs_axiom_grounding('a387e404-58ec-4610-8528-06604f3ed525', crown_prerogative_limited_only_vis_a_vis_peers, conventional).
narrative_ontology:cs_reference_frame('a387e404-58ec-4610-8528-06604f3ed525', feudal_hierarchical_order).
narrative_ontology:cs_drift_state('a387e404-58ec-4610-8528-06604f3ed525', early_modern_state_formation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a387e404-58ec-4610-8528-06604f3ed525', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, baronial_peers).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, crown).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, crown).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__feudal_prerogative_reading, feudal_privilege_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collectively compelled King John to accept limits on arbitrary imprisonment. They gain a guarantee that they may not be seized or imprisoned except by lawful judgment of peers or the law of the land. Their exit options include armed rebellion and feudal non-compliance, though the charter is preferred to civil war.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, baronial_peers, agenda_setter,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__feudal_prerogative_reading, baronial_peers, beneficiary).

% Bound by the charter to refrain from arbitrarily imprisoning free men. Bears the cost of constrained prerogative over the peerage, though the clause simultaneously preserves Crown authority over unfree subjects and the broader feudal hierarchy.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, crown, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__feudal_prerogative_reading, crown, beneficiary).

% Not covered by the clause's protections; remain subject to arbitrary lordly and royal power without recourse to peer judgment. Structurally excluded from the feudal counsel that produced the charter and would demand equivalent protections if present.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, unfree_subjects, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents arbitrary royal seizure of feudal magnates, solving the collective-action problem among barons who individually cannot resist the Crown but collectively can enforce a mutual non-seizure guarantee.
% TRANSFER_FUNCTION: Transfers security of person and property from the Crown's arbitrary prerogative to the baronial peerage, in exchange for continued feudal loyalty and political stability.
% ABSENT_VOICES: Unfree subjects and non-baronial freeholders are excluded from the counsel; they would demand equivalent procedural protections but are structurally absent from the charter's framing.
% DISAPPEARANCE_RATIONALE: Without the clause, arbitrary royal imprisonment of barons resumes, trust collapses, and the feudal political settlement of 1215 unravels into renewed civil war.
% FOUNDING_PROBLEM: King John's arbitrary seizures, imprisonments, and disseisins of barons (e.g., the de Braose affair, post-Bouvines retribution) destroyed elite trust and provoked baronial rebellion.
% FOUNDING_PROBLEM_CORROBORATION: Chroniclers Roger of Wendover and Matthew Paris, writing outside the baronial beneficiary class, record the arbitrary imprisonments; later legal historians (Stubbs, Maitland) corroborate the historical context from an analytical seat.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__feudal_prerogative_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__feudal_prerogative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__feudal_prerogative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_clause_39__feudal_prerogative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__feudal_prerogative_reading, 0.25, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored low (0.25) because the constraint's immediate effect is protective rather than predatory: it limits Crown power rather than extracting from a broad population. Suppression is moderate (0.35) reflecting the hierarchical enforcement that excludes non-peers. Theater_ratio rises over the interval (0.15 to 0.45) as the clause becomes increasingly ceremonial after the feudal order decays, even as the text is ritually invoked. Accessibility_collapse is moderate (0.60): for barons, alternatives to arbitrary royal power collapse once the charter is granted; for unfree subjects, alternatives were never accessible. Resistance is low-moderate (0.30): the Crown resisted at Runnymede but acquiesced under baronial pressure.
 *
 * PERSPECTIVAL GAP:
 *   The baronial seat experiences the clause as a hard-won liberty guarantee (low effective extraction, possibly negative); the Crown seat experiences it as a coerced concession of divine prerogative (high directionality); the unfree seat experiences it as an active exclusion that locks them out of procedural protection. The engine computes these divergences from the structural data without adjudicating the 'true' meaning of the text.
 *
 * DIRECTIONALITY LOGIC:
 *   Baronial peers are declared beneficiaries with mobile exit (rebellion), yielding a low directionality toward the constraint. The Crown is declared victim (payer) with constrained exit, yielding high directionality. Unfree subjects are excluded and trapped, but are not direct victims of the clause's operation (they are excluded from its benefits rather than extracted by its mechanism); their high directionality is implicit via the hierarchical order the clause vindicates.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâarbitrary royal imprisonment of baronsâwas substantially solved by the charter and by the later institutionalization of common-law process. Under the feudal reading, the mandate is dead by the early modern period: the clause persists as ceremonial invocation (rising theater_ratio) long after the feudal order it served has dissolved. The classification as tangled_rope (rather than piton) is supported by the continued, if ceremonial, enforcement and the persistent exclusionary function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer,
    'Is this constraint the feudal-prerogative reading of the Magna Carta Clause 39 kernel, and how would classification change under the liberal due-process or originalist sibling readings?',
    'Comparative analysis of the sibling constraint stories; the engine computes per-reading classification independently.',
    'Under the liberal reading, victim set broadens to all individuals, extractiveness likely rises, and classification may shift toward rope or scaffold; under the originalist reading, the constraint may collapse to a narrow historical rope or piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer, conceptual, 'Committer-frame uncertainty: this is one reading of a contested kernel.').

omega_variable(
    crown_dual_position,
    'Does the Crown''s dual role as constrained payer and hierarchical beneficiary make the asymmetric extraction of this clause negligible or merely obscured?',
    'Disaggregate Crown revenue and authority pre- and post-1215: if net extraction from the Crown is near zero because the clause buys baronial peace, the constraint trends toward rope; if net extraction is positive, tangled_rope holds.',
    'Reclassification to rope would follow if the Crown''s benefit (stable rule) fully offsets its cost (limited prerogative); retention of tangled_rope reflects the persistent exclusion of unfree subjects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crown_dual_position, empirical, 'Whether the Crown is genuinely a net payer or a concealed beneficiary.').

omega_variable(
    free_man_scope_ambiguity,
    'Does ''free man'' in the clause encompass only immediate feudal tenants-in-chief or a broader gentry, and does this ambiguity alter the victim set?',
    'Historical lexical analysis of ''liber homo'' in Angevin and early Plantagenet legal usage.',
    'A broader scope narrows the excluded class and reduces asymmetry; a narrower scope entrenches elite privilege and strengthens the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_man_scope_ambiguity, empirical, 'Scope ambiguity of the beneficiary class in the feudal reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__feudal_prerogative_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(magn_tr_t50, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(magn_tr_t100, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 100, 0.25).
narrative_ontology:measurement(magn_tr_t150, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 150, 0.3).
narrative_ontology:measurement(magn_tr_t200, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 200, 0.35).
narrative_ontology:measurement(magn_tr_t250, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 250, 0.4).
narrative_ontology:measurement(magn_tr_t300, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 300, 0.45).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(magn_be_t50, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement(magn_be_t100, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 100, 0.24).
narrative_ontology:measurement(magn_be_t150, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 150, 0.26).
narrative_ontology:measurement(magn_be_t200, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 200, 0.28).
narrative_ontology:measurement(magn_be_t250, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 250, 0.3).
narrative_ontology:measurement(magn_be_t300, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 300, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(magna_carta_clause_39__feudal_prerogative_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__feudal_prerogative_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
