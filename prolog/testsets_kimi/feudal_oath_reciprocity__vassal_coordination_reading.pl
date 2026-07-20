% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__vassal_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__vassal_coordination_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: feudal_oath_reciprocity__vassal_coordination_reading
 *   human_readable: Feudal Oath as Reciprocal Charter Coordination
 *   domain: medieval_political_economy/legal_history
 *
 * SUMMARY:
 *   This constraint instantiates the vassal_coordination_reading of the
 *   feudal_oath_reciprocity kernel: the feudal oath read as a fixed-text,
 *   bilateral coordination mechanism that bounds reciprocal obligations
 *   between lord and vassal, enforced by charter rather than by unilateral
 *   lordly will. It is authored as a low-Îµ rope with no structural victim,
 *   reflecting the view that the charter text provides mutual enforceability.
 *   The committer frame (Rules 1â4) is routed through cs_structure and
 *   omega variables; sibling readings (lord_extraction_reading,
 *   ecclesiastical_mediation_reading) are treated as separate constraints
 *   linked in the kernel family.
 *
 * KEY AGENTS:
 *   - feudal_lords (agenda_setter, powerful, constrained exit â bound by charter text to fixed obligations toward vassals)
 *   - feudal_vassals (beneficiary, moderate, constrained exit â receive protection and bounded land tenure in exchange for limited service)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__vassal_coordination_reading, 0.18).
domain_priors:suppression_score(feudal_oath_reciprocity__vassal_coordination_reading, 0.15).
domain_priors:theater_ratio(feudal_oath_reciprocity__vassal_coordination_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__vassal_coordination_reading, rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__vassal_coordination_reading, "Feudal Oath as Reciprocal Charter Coordination").
narrative_ontology:topic_domain(feudal_oath_reciprocity__vassal_coordination_reading, "medieval_political_economy/legal_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__vassal_coordination_reading, '6e8b4c7d-48c2-4786-a895-70a0be2395ec').
narrative_ontology:cs_kernel_codification('6e8b4c7d-48c2-4786-a895-70a0be2395ec', fixed_text).
narrative_ontology:cs_authority_grounding('6e8b4c7d-48c2-4786-a895-70a0be2395ec', lineage).
narrative_ontology:cs_interpretation_layer_present('6e8b4c7d-48c2-4786-a895-70a0be2395ec').
narrative_ontology:cs_reading_relation('6e8b4c7d-48c2-4786-a895-70a0be2395ec', feudal_oath_reciprocity__lord_extraction_reading, forecloses).
narrative_ontology:cs_reading_relation('6e8b4c7d-48c2-4786-a895-70a0be2395ec', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('6e8b4c7d-48c2-4786-a895-70a0be2395ec', foundational, bilateral_reciprocal_charter_limit).
narrative_ontology:cs_axiom_status(bilateral_reciprocal_charter_limit, holdable).
narrative_ontology:cs_axiom_grounding('6e8b4c7d-48c2-4786-a895-70a0be2395ec', bilateral_reciprocal_charter_limit, conventional).
narrative_ontology:cs_axiom('6e8b4c7d-48c2-4786-a895-70a0be2395ec', foundational, mutual_enforceability_by_text).
narrative_ontology:cs_axiom_status(mutual_enforceability_by_text, holdable).
narrative_ontology:cs_axiom_grounding('6e8b4c7d-48c2-4786-a895-70a0be2395ec', mutual_enforceability_by_text, conventional).
narrative_ontology:cs_reference_frame('6e8b4c7d-48c2-4786-a895-70a0be2395ec', reciprocal_charter_obligation).
narrative_ontology:cs_drift_state('6e8b4c7d-48c2-4786-a895-70a0be2395ec', late_feudal_centralization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6e8b4c7d-48c2-4786-a895-70a0be2395ec', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, feudal_vassals).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, feudal_lords).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the fief and holds the dominant position in the oath relationship, yet is explicitly bound by charter text to fixed, limited obligations toward the vassal. Grants protection, land use rights, and justice in exchange for military service and counsel, with the same charter serving as a source of legitimacy for both parties.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, feudal_lords, agenda_setter,
    powerful, generational, constrained, regional).

% Receives protection, land tenure, and access to the lord's court in return for military service and counsel. The charter text fixes the upper bound of obligations, providing a predictable, reciprocal framework that limits arbitrary lordly demands and gives the vassal a documented basis for resisting excess.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, feudal_vassals, beneficiary,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In a decentralized political order lacking a reliable central enforcement apparatus, the feudal oath creates a stable, bilateral commitment device that coordinates the provision of protection and land tenure with the provision of military service and counsel, binding both parties to fixed obligations.
% TRANSFER_FUNCTION: Moves military service and counsel from vassals to lords, and protection, land use rights, and justice from lords to vassals, all within bounds fixed by charter text rather than by unilateral will.
% ABSENT_VOICES: Peasants, non-feudal merchants, and allodial landholders are structurally outside the oath and lack standing to contest its terms; their exclusion is a feature of the broader feudal order, not of this specific reciprocal instrument.
% DISAPPEARANCE_RATIONALE: The feudal oath is the central commitment mechanism of the lord-vassal relationship; without it, the decentralized coordination of protection and military service would collapse into endemic default and predation, forcing a shift to alternate arrangements such as cash mercenary contracts or embryonic central taxation.
% FOUNDING_PROBLEM: In the post-Carolingian collapse of centralized authority, armed local strongmen and dependent warriors needed a binding mechanism to secure reciprocal obligations in the absence of a reliable state enforcement apparatus.
% FOUNDING_PROBLEM_CORROBORATION: Carolingian administrative chronicles and monastic historians attest the collapse of central authority; modern legal historians corroborate the vacuum of public enforcement that the oath was built to fill, independent of the immediate beneficiary interests of lords or vassals.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__vassal_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__vassal_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__vassal_coordination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__vassal_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__vassal_coordination_reading, 0.18, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).
:- end_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the charter text fixes obligations and prevents arbitrary increases; suppression is low (0.15) because the mechanism operates through mutual consent and legal text rather than coercive exclusion of alternatives. Theater_ratio is very low (0.08) because charter enforcement is functional, not performative. Accessibility_collapse is moderate (0.30) because while the feudal bond is costly to exit, alternatives such as allodial holding or mercenary service exist but are expensive. Resistance is low (0.10) because both parties are net beneficiaries within this reading. The measurement series shares a single time grid so that every metric is aligned at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   Under this reading, both the lord and vassal seats are net beneficiaries of the coordination, and both are constrained by the same charter text. The engine should compute both seats within the rope regime, with any modest directionality difference arising from the lord's greater power rather than from asymmetric extraction. Divergence would appear only if one seat had arbitrage-grade exit while the other was trapped; here both are constrained by the feudal bond.
 *
 * DIRECTIONALITY LOGIC:
 *   Both lords and vassals are declared as beneficiaries in base_properties, driving the directionality derivation toward the beneficiary end (low d) for both agents. The vassal's military service is bounded by charter text, so what might appear as extraction in the lord_extraction_reading is here treated as the symmetric cost of coordination. No victim is declared, so the engine derives no high-d target seat from structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring victim declarations for snare or tangled_rope and by keeping the rope claim independent of the metrics. The low extractiveness and suppression scores, combined with the absence of declared victims and the presence of bilateral beneficiaries, should keep the computed classification within rope territory. If the lord's power advantage were treated as unbounded extraction, the same facts would drift toward snare; the charter-text bounding is the structural feature that blocks that drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charter_text_vs_lordly_power,
    'Does the charter text actually constrain lordly power in practice, or does the power asymmetry make the text a decorative coordination story masking extraction?',
    'Archaeological and charter evidence measuring the frequency and success of vassal appeals against lordly demands beyond charter bounds.',
    'If the text rarely constrained actual practice, this reading''s Îµ is too low and the constraint collapses toward the lord_extraction_reading or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_text_vs_lordly_power, empirical, 'Whether charter reciprocity was operative or decorative in practice.').

omega_variable(
    kernel_reading_foreclosure_validity,
    'Does the vassal_coordination_reading''s core premise of fixed bilateral obligation logically foreclose the lord_extraction_reading, or do medieval legal sources show both interpretations coexisting within the same jurisdictional framework?',
    'Legal-historical analysis of contemporary jurisprudential commentary to see if a single court or treatise held both maximal lordly right and bounded vassal obligation simultaneously.',
    'If both coexisted in one framework, the forecloses relation should be downgraded to coexists_with or influences, altering the kernel-family contamination model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_validity, conceptual, 'Logical relation between coordination and extraction readings of the same oath.').

omega_variable(
    feudal_reciprocity_temporal_drift,
    'Did the reciprocal charter obligations remain stable over the interval, or did practice drift toward lordly extraction despite the fixed text?',
    'Temporal charter analysis tracking the evolution of service demands and the frequency of vassal resistance or renegotiation across the high medieval period.',
    'If practice drifted substantially while the text remained fixed, the constraint at interval end is better modeled as piton or tangled_rope than rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feudal_reciprocity_temporal_drift, empirical, 'Drift between charter text and feudal practice over the high medieval period.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__vassal_coordination_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(feud_tr_t20, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement(feud_tr_t40, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 40, 0.07).
narrative_ontology:measurement(feud_tr_t60, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 60, 0.08).
narrative_ontology:measurement(feud_tr_t80, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 80, 0.08).
narrative_ontology:measurement(feud_tr_t100, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(feud_be_t20, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(feud_be_t40, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(feud_be_t60, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 60, 0.16).
narrative_ontology:measurement(feud_be_t80, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 80, 0.17).
narrative_ontology:measurement(feud_be_t100, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 100, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(feudal_oath_reciprocity__vassal_coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% The feudal_oath_reciprocity kernel decomposes into three structurally distinct constraints: vassal_coordination_reading (low-Îµ rope, charter-bounded reciprocity), lord_extraction_reading (high-Îµ extraction, unbounded lordly right), and ecclesiastical_mediation_reading (sacramental limit on secular obligation). Each reading has a distinct Îµ, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
