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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: magna_carta_clause_39__feudal_prerogative_reading
 *   human_readable: Magna Carta Clause 39 — Feudal Prerogative Reading
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   This constraint instantiates the feudal-prerogative reading of Magna
 *   Carta Clause 39. Under this reading, Clause 39 ('No free man shall be
 *   taken, imprisoned, disseiséd of his freehold, liberties, or customs, nor
 *   passed upon, nor condemned, but by lawful judgment of his peers or by the
 *   law of the land') does NOT establish universal individual rights against
 *   arbitrary state power. Instead, it formalizes narrow procedural
 *   protections for the feudal elite (nobility and church) against caprice
 *   within their own hierarchy. The clause preserves the crown's prerogative;
 *   it merely channels the exercise of that prerogative through established
 *   feudal procedure and baronial counsel. Victims and free tenants remain
 *   outside its scope entirely. This reading emphasizes the elite status of
 *   'free men' in 1215 (roughly 10–15% of the population), the language
 *   'peers' (meaning social equals, not universal peers), and the term 'law
 *   of the land' as customary feudal practice, not universal principle. The
 *   constraint is CLAIMED as a rope (coordination among feudal elites) and
 *   the metrics describe low extractiveness, minimal suppression, and modest
 *   theater — consistent with a negotiated procedure among parties with
 *   recognized grievances and exit options.
 *
 * KEY AGENTS:
 *   - crowned_monarch: Crown retains ultimate prerogative; Clause 39 narrows only the MODE of exercising it (must follow procedure, must respect baronial counsel)
 *   - baronial_nobility: Primary beneficiary; gains recognized procedure for resolving peer disputes and demanding judgment by customary law rather than royal caprice
 *   - church_hierarchy: Secondary beneficiary; gains protection of ecclesiastical property within feudal procedure
 *   - free_tenants_and_merchants: Outside the scope of Clause 39 in this reading; remain subject to crown authority not constrained by the clause
 *   - royal_justices: Institutional beneficiary; gain authority and status as the regular channel for feudal judgment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__feudal_prerogative_reading, 0.28).
domain_priors:suppression_score(magna_carta_clause_39__feudal_prerogative_reading, 0.15).
domain_priors:theater_ratio(magna_carta_clause_39__feudal_prerogative_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__feudal_prerogative_reading, rope).
narrative_ontology:human_readable(magna_carta_clause_39__feudal_prerogative_reading, "Magna Carta Clause 39 — Feudal Prerogative Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__feudal_prerogative_reading, "constitutional/legal/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__feudal_prerogative_reading, '5b589729-a844-4afe-aac5-1e05de08465a').
narrative_ontology:cs_kernel_codification('5b589729-a844-4afe-aac5-1e05de08465a', fixed_text).
narrative_ontology:cs_authority_grounding('5b589729-a844-4afe-aac5-1e05de08465a', lineage).
narrative_ontology:cs_interpretation_layer_present('5b589729-a844-4afe-aac5-1e05de08465a').
narrative_ontology:cs_reading_relation('5b589729-a844-4afe-aac5-1e05de08465a', magna_carta_clause_39__liberal_due_process_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b589729-a844-4afe-aac5-1e05de08465a', magna_carta_clause_39__originalist_limitation_reading, coexists_with).
narrative_ontology:cs_axiom('5b589729-a844-4afe-aac5-1e05de08465a', foundational, feudal_hierarchy_legitimate_and_stable).
narrative_ontology:cs_axiom_status(feudal_hierarchy_legitimate_and_stable, holdable).
narrative_ontology:cs_axiom_grounding('5b589729-a844-4afe-aac5-1e05de08465a', feudal_hierarchy_legitimate_and_stable, conventional).
narrative_ontology:cs_axiom('5b589729-a844-4afe-aac5-1e05de08465a', foundational, narrow_standing_doctrine).
narrative_ontology:cs_axiom_status(narrow_standing_doctrine, overridden).
narrative_ontology:cs_axiom_grounding('5b589729-a844-4afe-aac5-1e05de08465a', narrow_standing_doctrine, deontological).
narrative_ontology:cs_axiom('5b589729-a844-4afe-aac5-1e05de08465a', secondary, procedural_constraint_preserves_prerogative).
narrative_ontology:cs_axiom_status(procedural_constraint_preserves_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('5b589729-a844-4afe-aac5-1e05de08465a', procedural_constraint_preserves_prerogative, instrumental).
narrative_ontology:cs_reference_frame('5b589729-a844-4afe-aac5-1e05de08465a', feudal_elite_procedure_and_consent).
narrative_ontology:cs_drift_state('5b589729-a844-4afe-aac5-1e05de08465a', contemporary_post_enlightenment_legal_frame, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('5b589729-a844-4afe-aac5-1e05de08465a', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, crowned_monarch).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, baronial_nobility).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, church_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the Charter as a concession to baronial pressure; retains de facto prerogative to judge disputes among nobility and free tenants through established feudal procedure. Clause 39 limits only the modes of judgment (not by caprice, not by force), not the crown's authority to judge. The procedural constraint preserves hierarchical order by narrowing which grievances can be brought and by whom.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, crowned_monarch, agenda_setter,
    institutional, generational, trapped, national).

% Gain a recognized procedure for resolving disputes among themselves and with the crown without immediate royal caprice or military seizure. They can demand judgment 'by the law of the land' — meaning by established feudal custom and baronial consent, not arbitrary royal will. This is coordination among equals (peers), not a constraint on royal authority over commoners. Exit option exists: baronial coalition can threaten rebellion; the Charter is its institutional expression.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, baronial_nobility, beneficiary,
    powerful, generational, mobile, national).

% The clause's scope in this reading does not extend to them; they remain subject to royal will outside this narrowly procedural protection. Clause 39 addresses only those with standing to be judged at all — essentially the nobility and major church officials. Free tenants have no formal seat in the conflict this clause resolves; they are outside the 'law of the land' it establishes.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, free_tenants_and_merchants, observer,
    moderate, biographical, constrained, national).

% Gains protection of ecclesiastical property and due process (canon law procedure) in disputes with the crown — a narrow procedural guarantee within the existing feudal structure. The church benefits from explicit recognition that it cannot be despoiled arbitrarily; but this is negotiated privilege, not universal right.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, church_hierarchy, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__feudal_prerogative_reading, church_hierarchy, observer).

% Administer the newly formalized procedures; the constraint binds them to established law rather than ad-hoc royal command. Their authority is reinforced (they become the regular channel for judgment) even as their latitude is narrowed.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, royal_justices, agenda_setter,
    powerful, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a recognized feudal procedure for resolving property and status disputes among the nobility and between the crown and nobility through customary law and baronial counsel, replacing unpredictable royal seizure or military force.
% TRANSFER_FUNCTION: Moves authority over dispute resolution from arbitrary royal caprice into the hands of justices bound by established feudal custom and the need for baronial consensus on precedent. The clause transfers from 'royal will' to 'law of the land,' but 'law of the land' remains narrowly feudal custom, not universal principle.
% ABSENT_VOICES: Peasants, unfree tenants, and the urban commons have no seat at the negotiation; they are outside the scope of this procedural protection entirely. The clause is negotiated between crown and elite peers; lower orders have no formal voice and no formal claim to its protections.
% DISAPPEARANCE_RATIONALE: If Clause 39 and its procedural guarantee vanished, the nobility would lose the negotiated security it provides against arbitrary royal judgment; baronial coalitions would lose institutional standing to demand judgment by counsel; the crown would retain uncontested capacity to seize baronial holdings without formality. The feudal hierarchy would reorganize — likely back toward more direct royal assertion and military resolution of disputes.
% FOUNDING_PROBLEM: Baronial resentment of King John's arbitrary seizure of noble holdings, summary judgment without counsel, and violation of customary feudal obligation and consent procedures. Nobles demanded a formalized procedure they could invoke to recover property or challenge arbitrary judgment.
% FOUNDING_PROBLEM_CORROBORATION: Chroniclers of the 1215 baronial revolt (Roger of Wendover, Matthew Paris) attest that arbitrary judgment and property seizure by the crown were the immediate grievances driving rebellion. Modern feudal historians outside any benefiting party confirm that customary procedure and baronial consent were the expected norms the crown had violated. The problem the clause addresses — arbitrary judgment among elites — remains live within the feudal structure itself, though the structure itself has since been superseded.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__feudal_prerogative_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__feudal_prerogative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__feudal_prerogative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_clause_39__feudal_prerogative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__feudal_prerogative_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is low (0.28) because the constraint does NOT extract from the beneficiaries; it coordinates among them. The crown retains prerogative and does not lose revenue or authority — it gains institutional legitimacy and stability by proceduralizing noble disputes. Suppression is very low (0.15) because the constraint reflects negotiated agreement among parties with real exit options (the baronial coalition that forced the charter). Theater is modest (0.22) because the procedural apparatus is functional — justices do apply feudal custom, baronial counsel is genuinely sought — but grows slightly over time as the constraint becomes more theatrical (repeated reissues, invocations as ceremonial appeals to tradition) and less genuinely negotiated. The measurement series shows extractiveness and theater drifting upward slightly over the 100-unit interval (roughly two centuries), reflecting the slow routinization and ritualization of the clause; suppression remains stable because the constraint never required active coercion — it was born of negotiation. No single metric shows dramatic shift because the feudal hierarchy itself is the stable referent; only later readings (liberal and originalist) reframe the clause as something more or different.
 *
 * PERSPECTIVAL GAP:
 *   The crown's perspective: Clause 39 is a minor concession that actually STRENGTHENS royal authority by institutionalizing it through justices and procedure, and by pacifying rebellious barons. From the crown's seat, extractiveness is near zero — they lose nothing, they gain legitimacy. The baronial perspective: Clause 39 is a hard-won procedural guarantee that prevents arbitrary seizure and forces the crown to justify judgment by customary law and peer counsel. From their seat, extractiveness is also near zero — they gain security without bearing new burdens. The commoner's perspective (not a stakeholder in this reading): Clause 39 is not for them; they remain subject to whatever the crown and nobles decide. The engine computes per-seat classification from structural data: the beneficiary seats should compute as rope or mountain (low extraction, genuine coordination); the crown should also compute as a beneficiary (gains legitimacy). The absence of a victim class is the key structural marker: this reading has no structural extraction because no party is forced to bear costs against their will. This is why feudal-prerogative differs from liberal-due-process — the latter reading attempts to universalize the clause, which creates an extraction claim (the crown's prerogative over commoners is now named an injustice, creating a victim set and raising extractiveness). But that is a different constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The feudal-prerogative reading does not generate directionality gradients because there is no extraction relationship. Every stakeholder benefits from the procedural clarity: the crown from legitimacy, nobles from protection, justices from authority, the church from property security. The commoner is outside the frame, not victimized by it — they are simply not party to this particular coordination. If one were to force a directionality calculation, the crown would have d near 0.5 (symmetric: it gains legitimacy but loses unilateral caprice). Nobles would have d near 0.2 (beneficiary: they gain protection). Church would have d near 0.1 (beneficiary). Justices near 0.35 (moderate beneficiary: they gain authority but are bound by procedure). Commoners would have d undefined in this reading because they are not stakeholders in it — they do not appear in the constraint's operative scope. The liberal and originalist readings would change these calculations dramatically by introducing commoners as victims and reframing the crown's prerogative as extractive.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy in the feudal-prerogative reading. The founding problem (arbitrary baronial judgment) remains live within its scope (elite disputes), and the constraint remains functional for its intended purpose (procedurally binding the crown and justices, coordinating noble consensus). The clause is invoked repeatedly, reissued, and treated as legitimate within the feudal frame — it is not persisting by mere inertia. Mandatrophy would emerge only in readings that attempt to universalize the clause beyond the feudal elite (liberal reading) and then watch it atrophy as the feudal hierarchy itself collapses. In this reading, the constraint is still functional and its founding problem is still live within its narrow scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universalization_boundary,
    'Does Clause 39''s phrase ''free man'' refer only to the feudal elite (nobility, major church, wealthy merchants with recognized standing), or does it implicitly extend to all subjects over time as societies developed?',
    'Historical analysis of the clause''s invocation and reissue: if consistently applied only to elite disputes through the 13th-16th centuries, the narrow reading is supported; if gradually applied to broader populations or explicitly extended by jurists, the universalization process is documented and the liberal reading becomes historically posterior (not original).',
    'If the universalization is posterior (not present in 1215 thinking), then the feudal-prerogative reading is correct and the liberal reading is a later reinterpretation, not a reading of the original clause. If universalization is implicit or early, the liberal reading''s claim to the clause is stronger. This controls whether the constraint is a Rope (feudal reading) or a Snare (liberal reading, if extractiveness recomputes when victims are added).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universalization_boundary, empirical, 'Historical scope of ''free man'' in the clause''s application.').

omega_variable(
    reading_foreclosure_or_coexistence,
    'Are the feudal-prerogative and liberal-due-process readings logically foreclosed or coexisting within medieval and early-modern jurisprudence?',
    'Study of how different parties (monarchs, nobility, common lawyers, church) invoked and interpreted Clause 39 over centuries. If some consistently used it as elite procedure and others as universal principle, and both uses were intelligible and advanced within the same institutions, they coexist; if one interpretation directly contradicted the other such that a jurist could not hold both, they foreclose.',
    'If they coexist, both readings are live and valid constraint stories, linked via network.affects_constraints as siblings of the same kernel. If one forecloses the other, the foreclosing reading should declare that relation in cs_structure.reading_relations. Current assessment: coexistence (late medieval and early modern jurisprudence held both, and institutional space allowed both to be advanced by different factions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_or_coexistence, conceptual, 'Whether the feudal and liberal readings are logically compatible or mutually exclusive interpretations of the same text.').

omega_variable(
    commoner_victim_vs_outside_scope,
    'Are commoners VICTIMS of the feudal-prerogative constraint (i.e., harmed by their exclusion from its protections), or are they simply OUTSIDE its scope (neither benefiting nor victimized because the constraint does not apply to them)?',
    'Examine whether commoners themselves claimed the clause as a right or whether only elites invoked it. If commoners later claimed standing under the clause and were denied it, victimhood is evidenced. If commoners were never parties to disputes the clause governed, they are simply outside scope. The distinction controls whether suppression (exclusion by force) is measured or whether the constraint is simply not the mechanism that governs commoner treatment.',
    'If commoners are victims (harmed by exclusion from protections), extractiveness recomputes upward (a victim set is present), and the constraint approaches Snare territory. If they are outside scope, extractiveness remains low (no extraction from non-parties). This reading assumes ''outside scope''; if empirical analysis shows commoners were victimized by exclusion, the reading must be revised or a separate liberal-reading constraint should be generated to capture the different structural situation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commoner_victim_vs_outside_scope, empirical, 'Whether commoners are victimized by or merely outside the scope of this constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__feudal_prerogative_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(magn_tr_t12, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(magn_tr_t25, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 25, 0.2).
narrative_ontology:measurement(magn_tr_t50, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement(magn_tr_t75, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 75, 0.23).
narrative_ontology:measurement(magn_tr_t100, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(magn_be_t12, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 12, 0.26).
narrative_ontology:measurement(magn_be_t25, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 25, 0.27).
narrative_ontology:measurement(magn_be_t50, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(magn_be_t75, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 75, 0.29).
narrative_ontology:measurement(magn_be_t100, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 100, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(magn_su_t12, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 12, 0.13).
narrative_ontology:measurement(magn_su_t25, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 25, 0.14).
narrative_ontology:measurement(magn_su_t50, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 50, 0.15).
narrative_ontology:measurement(magn_su_t75, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 75, 0.15).
narrative_ontology:measurement(magn_su_t100, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 100, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__feudal_prerogative_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_clause_39__feudal_prerogative_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel magna_carta_clause_39. The feudal-prerogative reading interprets Clause 39 as narrowly coordinating elite procedural protections within a stable feudal hierarchy. Sibling readings — liberal-due-process and originalist-limitation — interpret the same text differently and generate different constraint stories with different ε, beneficiary/victim structures, and classifications. Each reading is a distinct constraint; they are linked through this network field and through kernel_context in commentary. The constraint family decomposition follows the ε-invariance principle: same text, different readings, different ε → different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
