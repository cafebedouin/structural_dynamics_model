% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__originalist_limitation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__originalist_limitation_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: magna_carta_clause_39__originalist_limitation_reading
 *   human_readable: Magna Carta Clause 39 (Originalist Limitation Reading)
 *   domain: constitutional_law
 *
 * SUMMARY:
 *   Magna Carta's Clause 39 (also 'Clause 29' in 1215) states: 'No free man
 *   shall be taken or imprisoned or disseised of his freehold or liberties or
 *   free customs, nor shall he be outlawed or banished, or in any way
 *   destroyed, save by the lawful judgment of his peers or by the law of the
 *   land.' The originalist reading interprets this as addressing SPECIFIC
 *   documented abuses by King John in 1215: arbitrary imprisonment, seizure
 *   of baronial lands without feudal due process, and excessive fines used as
 *   instruments of extortion. This reading constrains the clause to its
 *   feudal context and the magnates who negotiated it. It is one of three
 *   coherent readings of the same kernel text, each with different structural
 *   consequences for victim set, extractiveness, and the scope of the right.
 *
 * KEY AGENTS:
 *   - magnate_barons: powerful beneficiaries seeking procedural protection against arbitrary royal action
 *   - king_john: institutional payer bearing formal constraint on prerogative through written charter
 *   - english_nobility: later beneficiaries who inherit and enforce the procedural limit
 *   - later_common_lawyers: excluded voices who reframe the clause as universal due process
 *   - american_framers: excluded analytical seat citing the clause as precedent for individual rights
 *   - originalist_legal_scholars: beneficiary observers maintaining textual historical specificity as method
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__originalist_limitation_reading, 0.38).
domain_priors:suppression_score(magna_carta_clause_39__originalist_limitation_reading, 0.22).
domain_priors:theater_ratio(magna_carta_clause_39__originalist_limitation_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__originalist_limitation_reading, rope).
narrative_ontology:human_readable(magna_carta_clause_39__originalist_limitation_reading, "Magna Carta Clause 39 (Originalist Limitation Reading)").
narrative_ontology:topic_domain(magna_carta_clause_39__originalist_limitation_reading, "constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__originalist_limitation_reading, '23bec6b6-27a4-4bea-bf59-059502911bd9').
narrative_ontology:cs_kernel_codification('23bec6b6-27a4-4bea-bf59-059502911bd9', fixed_text).
narrative_ontology:cs_authority_grounding('23bec6b6-27a4-4bea-bf59-059502911bd9', lineage).
narrative_ontology:cs_interpretation_layer_present('23bec6b6-27a4-4bea-bf59-059502911bd9').
narrative_ontology:cs_reading_relation('23bec6b6-27a4-4bea-bf59-059502911bd9', magna_carta_clause_39__magna_carta_clause_39_liberal_due_process_reading, coexists_with).
narrative_ontology:cs_reading_relation('23bec6b6-27a4-4bea-bf59-059502911bd9', magna_carta_clause_39__magna_carta_clause_39_feudal_prerogative_reading, influences).
narrative_ontology:cs_axiom('23bec6b6-27a4-4bea-bf59-059502911bd9', foundational, historical_scope_determinism).
narrative_ontology:cs_axiom_status(historical_scope_determinism, holdable).
narrative_ontology:cs_axiom_grounding('23bec6b6-27a4-4bea-bf59-059502911bd9', historical_scope_determinism, empirically_contingent).
narrative_ontology:cs_axiom('23bec6b6-27a4-4bea-bf59-059502911bd9', foundational, originalism_as_method).
narrative_ontology:cs_axiom_status(originalism_as_method, holdable).
narrative_ontology:cs_axiom_grounding('23bec6b6-27a4-4bea-bf59-059502911bd9', originalism_as_method, instrumental).
narrative_ontology:cs_reference_frame('23bec6b6-27a4-4bea-bf59-059502911bd9', feudal_procedural_constraint).
narrative_ontology:cs_drift_state('23bec6b6-27a4-4bea-bf59-059502911bd9', seventeenth_century_common_law_reframing, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('23bec6b6-27a4-4bea-bf59-059502911bd9', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, magnate_barons).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, english_nobility).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, originalist_legal_scholars).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, king_john).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__originalist_limitation_reading, textual_originalism).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__originalist_limitation_reading, historical_specificity_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__originalist_limitation_reading, narrow_procedural_remedy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated Clause 39 in 1215 to secure procedural protections against arbitrary royal actions (unjust seizure of lands, excessive fines, denial of feudal due process). Benefited from formal written constraint on king's prerogative within feudal hierarchy. Their exit option was armed rebellion; Clause 39 provided a contractual alternative.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, magnate_barons, beneficiary,
    powerful, biographical, constrained, national).

% Conceded Clause 39 under military pressure and baronial threat of succession withdrawal. Paid the constraint through formal limitation of arbitrary imprisonment, land seizure, and excessive fines in documented feudal contexts. Retained full prerogative outside the clause's narrow scope.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, king_john, payer,
    institutional, biographical, mobile, national).

% Inherited the 1215 protections in reissues (1217, 1225) and enforced them as procedural limits within the feudal system. Clause 39 constrained their own capacity to mimic arbitrary royal action and raised cost of internal baronial conflicts, stabilizing the landed hierarchy.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, english_nobility, beneficiary,
    powerful, generational, constrained, national).

% By the 16th-17th centuries, common lawyers read Clause 39 as establishing universal due process rights. They were not in the 1215 room; their reframing expanded the clause's scope beyond the documented baronial grievances and feudal context that the originalist reading treats as the constraint's actual boundary.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, later_common_lawyers, excluded,
    organized, generational, analytical, national).

% Cited Clause 39 (via Coke's gloss) as precedent for individual constitutional rights in the 1780s. The originalist reading disputes whether this citation captured the clause's actual historical function or projected later meanings onto a feudal-era contract.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, american_framers, excluded,
    powerful, biographical, analytical, global).

% Maintain that textual historical specificity is the proper interpretive method. Their position is vindicated by treating Clause 39 as bounded by 1215 documented abuses, not expanded retroactively to support modern rights doctrines. They benefit from methodological consistency and interpretive authority.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, originalist_legal_scholars, beneficiary,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__originalist_limitation_reading, originalist_legal_scholars, observer).

% The liberal reading (separate constraint story) reads Clause 39 as establishing universal procedural guarantees against arbitrary state power. From the originalist perspective, the liberal reading projects modern values onto a feudal contract; from the liberal perspective, the originalist reading undershoots the clause's true universal scope.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, liberal_rights_tradition, excluded,
    organized, civilizational, analytical, global).

% The feudal reading (separate constraint story) emphasizes that Clause 39 preserves the king's hierarchical authority while narrowly formalizing feudal procedures. From the originalist perspective, this reading captures the narrow procedural scope but misses the clause's function as limiting documented abuses.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, feudal_prerogative_tradition, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Formalizes feudal dispute-resolution procedures: when the king acts against a baron's lands, rights, or person, he must use documented procedural due process rather than arbitrary will. Solves the collective-action problem of baronial vulnerability to capricious royal action within the hierarchical feudal order.
% TRANSFER_FUNCTION: Moves formal authority to issue binding writs of due process from informal royal discretion to a written charter that barons can invoke and enforce through collective action. The payment is the king's loss of unfettered prerogative; the transfer is constraint on arbitrary seizure and excessive fines documented in 1215 grievances.
% ABSENT_VOICES: Common lawyers and later equity jurisprudence, which would reframe the clause's scope from 1215 documented abuses to universal procedural rights. American constitutional scholars, who would cite Clause 39 as precedent for individual liberty. Medieval English commoners, merchants, and unfree persons, who are excluded from the clause's protection entirely and whose grievances against the crown are not addressed.
% DISAPPEARANCE_RATIONALE: If Clause 39 and its procedural constraints vanished, barons would lose documented legal recourse against arbitrary royal action; they would resort to armed rebellion or succession contests as the sole check on prerogative. The 1215 settlement depended on the written constraint existing to channel baronial grievances into procedural remedy rather than civil war.
% FOUNDING_PROBLEM: King John's documented abuses: arbitrary imprisonment and seizure of baronial lands without feudal due process, excessive reliefs and fines imposed as instruments of extortion, denial of wardship and marriage rights according to feudal custom. The charter addresses specific 1215 grievances in the feudal relationship between king and magnate.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary 1215 sources document King John's extortionate reliefs, arbitrary seizures, and feudal irregularities (Magna Carta itself, Roger of Wendover, Matthew Paris). Later reissues (1217, 1225) confirm the clause's persistence as a procedural check. Originalist historians (J.C. Holt, David Carpenter) outside the benefiting tradition corroborate the clause as response to documented 1215 abuses; liberal historians acknowledge the abuse-response structure but argue the principle generalizes beyond 1215 context.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__originalist_limitation_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__originalist_limitation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__originalist_limitation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(magna_carta_clause_39__originalist_limitation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__originalist_limitation_reading_tests).
:- end_tests(magna_carta_clause_39__originalist_limitation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at 1215) because the constraint limits specific documented royal abuses but does not establish universal rights or check all prerogative. The clause is narrow: it addresses land seizure, imprisonment, and excessive fines in feudal contexts, not governance of commons, trade, or peasants. Suppression is low (0.22) because the constraint is not maintained by force but by baronial collective action and written reissue — once agreed, barons enforce it through litigation and succession contests rather than coercion. Theater rises sharply (0.05 to 0.38 by year 600) as the clause is cited and glossed for purposes increasingly divorced from 1215 context (common-law expansion, American constitutional founding), but then reverts (0.18 at year 800) as originalist scholarship clarifies the clause's actual historical function. Suppression remains flat because the procedural mechanism (written constraint on arbitrary action) does not require escalating enforcement — it solves the problem once, unless the constraint is reframed to address new domains.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist seat, the constraint is a rope: barons benefit from formal limitation of documented abuses, the king pays by losing unfettered prerogative, and no party is victimized. From the liberal seat (separate constraint story), the same text is a mountain: universal procedural rights against arbitrary state power, benefiting all individuals. From the feudal seat (separate constraint story), the same text is a snare: the king grants narrow procedural formality while preserving core prerogative, and the barons remain bound within the feudal hierarchy. The originalist reading claims that the first perspective is structurally true — the clause's actual function was specific to 1215 grievances — while acknowledging that later readings project new meanings onto the text.
 *
 * DIRECTIONALITY LOGIC:
 *   Magnate barons are the beneficiaries (they negotiated the constraint, benefit from procedural protection, would suffer its loss). King John is the payer (loses unfettered prerogative, must observe feudal due process). Later nobility inherit the beneficiary status. Common lawyers and American framers are EXCLUDED, not payers — they reframe the clause without being parties to the 1215 negotiation. Originalist scholars are beneficiaries in a secondary sense: their interpretive method (textual originalism) is vindicated by treating the clause as bounded by historical specificity. No structured victim set exists in this reading — there are no excluded commons or peasants whose grievances the clause suppresses (that is a critique within the liberal reading, which frames the clause as promising universal rights while delivering only baronial privilege). The absence of a victim group is structural to the originalist reading's claim: the constraint solves a feudal coordination problem without creating new extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy by maintaining that the founding problem (1215 abuses) remains live — barons continue to invoke Clause 39 in disputes with the crown through the 13th century, and later reissues confirm its persistence as a procedural check. The theater-ratio rise (year 200–600) reflects the clause being cited for purposes beyond 1215 (common-law expansion, constitutional precedent), not atrophy of the original function. The theater eventually stabilizes (year 800) as originalist scholarship clarifies the clause's scope. If the founding problem had died (the crown ceased arbitrary seizures, feudal procedures became routine, the clause ceased to constrain any actual royal action), the rising theater would signal theatrical maintenance — a piton. Instead, the originalist reading argues the constraint remains functionally tied to its 1215 purpose because feudal due-process violations continue until the feudal system itself degrades.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_scope_vs_principle_generalization,
    'Does the clause''s actual historical function — addressing 1215 documented abuses — establish its logical scope, or does the principle it instantiates (procedural constraint on arbitrary action) logically generalize to all subjects and state actions?',
    'Textual evidence from 1215 sources and reissue charters; common-law judicial citations and reasoning; comparative analysis with later universal rights doctrines. The resolution turns on whether the generalization is implicit in the 1215 text or projected by later interpreters.',
    'If historical scope is definitive (originalist position holds), the constraint is bounded and moderate in extractiveness; if the principle generalizes, the constraint becomes a universal mountain (liberal reading) and the originalist reading undershoots its scope. This is the core committer disagreement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_scope_vs_principle_generalization, conceptual, 'Whether Clause 39''s scope is fixed by 1215 historical context or determined by the universal principle it articulates.').

omega_variable(
    interpretive_method_vindication,
    'Is textual originalism (fixing meaning by historical authorial intent and documented context) the correct method for constitutional interpretation, or does it miss the clause''s evolving significance across centuries?',
    'Philosophical debate over interpretive methodology; empirical examination of whether the originalist method produces stable, predictable, and defensible constraint classifications across a corpus of similar cases; comparison with other methodologies (living constitutionalism, common-law tradition) on consistency metrics.',
    'The originalist reading itself is vindicated (as a matter of method) if originalism proves robust; if the method fails on methodological grounds, the reading''s authority erodes even if its historical facts remain accurate. This is not about whether the reading is right, but whether the method that produces it is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_method_vindication, preference, 'Whether originalist methodology is justified as the grounds for constitutional interpretation.').

omega_variable(
    feudal_prerogative_boundary,
    'Does Clause 39 genuinely constrain the king''s documented abuses, or does it formalize hierarchy while leaving core prerogative intact (the feudal reading''s claim)?',
    'Examination of post-1215 royal behavior and judicial enforcement of the clause; analysis of whether kings violated the clause with impunity or whether the clause actually modified their behavior; comparison of fealty disputes and feudal conflicts before and after 1215 to measure behavioral change.',
    'If Clause 39 materially changes royal behavior (constrains abuses), the originalist reading''s claim of a rope holds; if kings routinely violate it and enforcement is theatrical, the feudal reading''s claim of formalized prerogative (snare-like) is supported. This is an empirical question about the clause''s actual effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feudal_prerogative_boundary, empirical, 'Whether Clause 39 materially constrains documented King John abuses or merely formalizes feudal hierarchy.').

omega_variable(
    reading_divergence_foundation,
    'Why do the three readings coexist despite textual identity? Is the divergence rooted in ambiguity in the 1215 text, or in deliberate reframing by later parties pursuing different interests?',
    'Linguistic and textual analysis of the original clause; historical tracing of when each reading emerges (feudal in immediate aftermath, liberal with common lawyers 16th century, originalist with modern scholarship); examination of whether earlier readers of the text could have arrived at later readings or whether they require extrinsic conceptual tools (universal rights doctrine, originalism as a methodology).',
    'If the text is genuinely ambiguous (multiple readings are all defensible from 1215), then coexistence reflects textual under-determination. If later readings require extrinsic conceptual frameworks, then the divergence reflects methodological choice, not textual content. This frames whether the readings are co-valid or sequentially replaced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_divergence_foundation, empirical, 'Whether reading divergence stems from textual ambiguity or from later parties'' conceptual projections.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__originalist_limitation_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(magn_tr_t0, observed).
narrative_ontology:measurement(magn_tr_t100, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 100, 0.08).
narrative_ontology:measurement_basis(magn_tr_t100, observed).
narrative_ontology:measurement(magn_tr_t200, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 200, 0.12).
narrative_ontology:measurement_basis(magn_tr_t200, observed).
narrative_ontology:measurement(magn_tr_t400, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 400, 0.28).
narrative_ontology:measurement_basis(magn_tr_t400, observed).
narrative_ontology:measurement(magn_tr_t600, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 600, 0.38).
narrative_ontology:measurement_basis(magn_tr_t600, observed).
narrative_ontology:measurement(magn_tr_t800, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 800, 0.18).
narrative_ontology:measurement_basis(magn_tr_t800, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(magn_be_t0, observed).
narrative_ontology:measurement(magn_be_t100, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 100, 0.42).
narrative_ontology:measurement_basis(magn_be_t100, observed).
narrative_ontology:measurement(magn_be_t200, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 200, 0.45).
narrative_ontology:measurement_basis(magn_be_t200, observed).
narrative_ontology:measurement(magn_be_t400, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 400, 0.48).
narrative_ontology:measurement_basis(magn_be_t400, observed).
narrative_ontology:measurement(magn_be_t600, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 600, 0.52).
narrative_ontology:measurement_basis(magn_be_t600, observed).
narrative_ontology:measurement(magn_be_t800, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 800, 0.38).
narrative_ontology:measurement_basis(magn_be_t800, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(magn_su_t0, observed).
narrative_ontology:measurement(magn_su_t100, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 100, 0.2).
narrative_ontology:measurement_basis(magn_su_t100, observed).
narrative_ontology:measurement(magn_su_t200, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 200, 0.19).
narrative_ontology:measurement_basis(magn_su_t200, observed).
narrative_ontology:measurement(magn_su_t400, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 400, 0.18).
narrative_ontology:measurement_basis(magn_su_t400, observed).
narrative_ontology:measurement(magn_su_t600, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 600, 0.25).
narrative_ontology:measurement_basis(magn_su_t600, observed).
narrative_ontology:measurement(magn_su_t800, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 800, 0.22).
narrative_ontology:measurement_basis(magn_su_t800, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__originalist_limitation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_clause_39__originalist_limitation_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39_liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39_feudal_prerogative_reading).

% DUAL FORMULATION NOTE:
% Magna Carta Clause 39 is a contested kernel with three structurally distinct constraint readings. This file instantiates the ORIGINALIST LIMITATION READING, bounded by 1215 documented abuses (King John's arbitrary seizures, excessive fines, denial of feudal due process). The liberal_due_process_reading expands the scope to universal procedural rights against arbitrary state power. The feudal_prerogative_reading emphasizes that the clause formalizes feudal hierarchy while preserving core royal prerogative. All three readings derive from identical source text; their structural differences (victim set, scope, type) arise from interpretive method. Each reading is published as a separate constraint story with its own ε, stakeholders, and six-questions answers. The kernel_context field in each story explains the relationship to siblings. Network edges link all three members of the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
