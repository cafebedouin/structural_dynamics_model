% ============================================================================
% CONSTRAINT STORY: maat_order_principle__reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__reciprocity_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: maat_order_principle__reciprocity_reading
 *   human_readable: Ma'at Reciprocity Reading â Pharaoh Obligated to Justice
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the reciprocity_reading of the
 *   maat_order_principle kernel. In this reading, Ma'at is not merely a
 *   divine mandate flowing unchecked through the Pharaoh (as in the
 *   divine_mandate_reading), nor a fully distributed responsibility across
 *   all social stations (as in the distributed_maintenance_reading). Rather,
 *   Ma'at imposes genuine mutual obligations: the ruler must deliver justice,
 *   stability, and proper resource distribution to the subject populace, and
 *   failure to do so justifies resistance or withdrawal of support. This
 *   creates a coordination mechanism with a bounded extraction ceiling, where
 *   the populace is both coordinated (receives justice and protection) and
 *   extracted from (taxes, corvÃ©e). The structural asymmetry between the
 *   identity-locked Pharaoh and the constrained peasantry generates divergent
 *   seat experiences.
 *
 * KEY AGENTS:
 *   - Pharaoh: Primary agenda-setter (institutional/identity_locked) â embodies Ma'at and is obligated to provide justice and redistribution; cannot exit the role without dynastic collapse.
 *   - Priestly institution: Secondary agenda-setter and beneficiary (institutional/constrained) â interprets Ma'at, manages temple redistribution, and gains economic endowments.
 *   - Subject populace: Primary beneficiary (powerless/constrained) â receives judicial protection, famine relief, and stability in exchange for compliance.
 *   - Tax-bearing peasantry: Primary payer (powerless/constrained) â bears agricultural taxation and corvÃ©e labor that funds the apparatus.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__reciprocity_reading, 0.45).
domain_priors:suppression_score(maat_order_principle__reciprocity_reading, 0.55).
domain_priors:theater_ratio(maat_order_principle__reciprocity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__reciprocity_reading, "Ma'at Reciprocity Reading â Pharaoh Obligated to Justice").
narrative_ontology:topic_domain(maat_order_principle__reciprocity_reading, "ancient_history/political_philosophy/religious_studies").

domain_priors:requires_active_enforcement(maat_order_principle__reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__reciprocity_reading, '19a5bd4f-c768-4b78-bfe5-6dea8cd9c132').
narrative_ontology:cs_kernel_codification('19a5bd4f-c768-4b78-bfe5-6dea8cd9c132', distributed).
narrative_ontology:cs_authority_grounding('19a5bd4f-c768-4b78-bfe5-6dea8cd9c132', lineage).
narrative_ontology:cs_interpretation_layer_present('19a5bd4f-c768-4b78-bfe5-6dea8cd9c132').
narrative_ontology:cs_reading_relation('19a5bd4f-c768-4b78-bfe5-6dea8cd9c132', maat_order_principle__divine_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('19a5bd4f-c768-4b78-bfe5-6dea8cd9c132', maat_order_principle__distributed_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('19a5bd4f-c768-4b78-bfe5-6dea8cd9c132', foundational, mutual_obligation_between_ruler_and_ruled).
narrative_ontology:cs_axiom_status(mutual_obligation_between_ruler_and_ruled, holdable).
narrative_ontology:cs_axiom_grounding('19a5bd4f-c768-4b78-bfe5-6dea8cd9c132', mutual_obligation_between_ruler_and_ruled, deontological).
narrative_ontology:cs_axiom('19a5bd4f-c768-4b78-bfe5-6dea8cd9c132', foundational, failed_justice_justifies_resistance).
narrative_ontology:cs_axiom_status(failed_justice_justifies_resistance, holdable).
narrative_ontology:cs_axiom_grounding('19a5bd4f-c768-4b78-bfe5-6dea8cd9c132', failed_justice_justifies_resistance, deontological).
narrative_ontology:cs_reference_frame('19a5bd4f-c768-4b78-bfe5-6dea8cd9c132', pharaonic_reciprocal_obligation).
narrative_ontology:cs_drift_state('19a5bd4f-c768-4b78-bfe5-6dea8cd9c132', late_period_observation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('19a5bd4f-c768-4b78-bfe5-6dea8cd9c132', '').
narrative_ontology:cs_kernel_id(maat_order_principle__reciprocity_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, subject_populace).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, priestly_institution).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, tax_bearing_peasantry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the kingdom as the living apex of the Ma'at order. Obligated to adjudicate justly, maintain territorial stability, and ensure proper redistribution of grain and labor. Gains dynastic legitimacy from successful performance but cannot exit the role without collapsing the institutional identity of kingship itself.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, pharaoh, agenda_setter,
    institutional, generational, identity_locked, national).

% Interprets Ma'at through ritual and textual tradition, legitimates the ruler's adherence to cosmic order, manages temple redistribution networks, and receives land endowments and offerings. Their authority and prosperity depend on the continued credibility of the reciprocity framework.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, priestly_institution, agenda_setter,
    institutional, generational, constrained, national).

% Comprises non-elite households who receive judicial oversight, famine relief, and infrastructure maintenance from the redistributive state in exchange for compliance. Their continued acceptance of the order is conditional on the ruler's fulfillment of Ma'at obligations.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, subject_populace, beneficiary,
    powerless, biographical, constrained, national).

% Agricultural producers who bear the primary burden of taxation and corvÃ©e labor that funds the state and temple apparatus. They receive some stability and judicial protection, but the net flow is extractive and intensifies when the reciprocity norm weakens.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, tax_bearing_peasantry, payer,
    powerless, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for social order in an agrarian river-valley civilization by linking political authority to cosmic justice, creating legitimacy conditions for centralized redistribution, dispute resolution, and large-scale labor coordination.
% TRANSFER_FUNCTION: Moves agricultural surplus and labor from rural producers to the state and temple apparatus, and moves judicial protection, stability, and redistributed grain back to the subject populace, contingent on the ruler maintaining Ma'at.
% ABSENT_VOICES: Foreign captives, nomadic groups operating outside the Nile valley agrarian system, and landless laborers are structurally excluded from the reciprocal bargain; they experience the arrangement as pure extraction but are not in the legitimating conversation.
% DISAPPEARANCE_RATIONALE: If the Ma'at reciprocity framework vanished, the ideological legitimacy of the redistributive state would collapse, tax compliance would falter, temple economies would detach from royal authority, and the central apparatus would fragment into local patronage networks or foreign domination.
% FOUNDING_PROBLEM: How to legitimize centralized extraction of agricultural surplus and labor in a recurrently fragile agrarian ecology while preventing ruler predation, peasant flight, and dynastic instability.
% FOUNDING_PROBLEM_CORROBORATION: Modern Egyptology and anthropological archaeology attest the problem was partially solved by genuine redistribution and partially by ideological cover for extraction. No contemporary ancient source outside the benefiting priestly and scribal classes independently corroborates the reciprocity framing; peasant voices survive only in complaint literature.
narrative_ontology:disappearance_verdict(maat_order_principle__reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__reciprocity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__reciprocity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(maat_order_principle__reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__reciprocity_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__reciprocity_reading_tests).
:- end_tests(maat_order_principle__reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the redistributive and judicial institutions are real and functional, but surplus extraction remains substantial. Suppression is moderate (0.55) because ideological enforcement and administrative tracking are strong yet imperfect; documented peasant flight and work stoppages show limits. Theater ratio is moderate-low (0.30) because judicial and redistribution mechanisms genuinely operate, though ritual performance amplifies legitimacy beyond functional necessity. Accessibility collapse is 0.60 because alternatives (local strongmen, foreign rule, nomadic exit) exist but are ideologically devalued. Resistance is 0.40 because strikes, flight, and regional withdrawal are attested but episodic rather than continuous. The measurement series share a single time grid so all metrics are authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The Pharaoh seat experiences the constraint as a generational duty that legitimates rule and demands constant resource expenditure; the tax-bearing peasantry experiences it as a structural extraction whose reciprocity is periodically honored in the breach. The priestly seat experiences it as both a genuine cosmological principle and an economic endowment. These divergences are structurally driven by exit options (identity-locked versus constrained) and by the split between beneficiary and victim declarations for different powerless seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The subject populace and priestly institution are declared beneficiaries (low directionalities, subsidized by the constraint through stability and endowments). The tax-bearing peasantry are declared victims (high directionality, targeted extraction). Pharaoh sits as agenda-setter with identity-locked exit; the constraint does not subsidize him through direct resource flow but through legitimacy. No directionality override is required because the structural derivation captures the asymmetry: beneficiaries receive protection, the payer bears the labor tax, and the agenda-setter's locked identity amplifies his exposure to the constraint's demands without making him a target in the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The reciprocity reading prevents mislabeling the arrangement as pure extraction (snare) by acknowledging the genuine coordination functionâredistribution, justice, and legitimacyâand by noting the extraction ceiling imposed by the reciprocity norm. Conversely, it prevents mislabeling it as pure coordination (rope) by naming an identifiable victim group (tax-bearing peasantry) and the active enforcement required to maintain surplus extraction. The Tangled Rope classification captures this duality without collapsing it into either pole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_norm_empirical_reality,
    'Was the reciprocity norm a functional constraint on royal extraction or primarily an ideological justification that occasionally failed?',
    'Comparative archaeological analysis of redistribution versus extraction ratios across Old, Middle, and New Kingdom sites; textual analysis of complaint literature versus royal propaganda.',
    'If redistribution was minimal, the constraint computes more snare-like; if substantial and consistent, more rope-like. This resolves the true extraction ceiling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_norm_empirical_reality, empirical, 'Whether Ma''at reciprocity was enforced in practice or mainly ideological').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (administrative corvÃ©e tracking, border control, grain storage monitoring) or internalized (the peasantry''s acceptance of cosmic order as natural and unchangeable)?',
    'Post-exit trajectory analysis: do fugitive peasants or those in times of state collapse retain compliance patterns, or does resistance immediately escalate?',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggestsâthe target carries the suppression even when external enforcement weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in agrarian obedience').

omega_variable(
    kernel_reading_relation,
    'Does the reciprocity reading foreclose the divine_mandate reading within a single ancient Egyptian theological framework, or can they be held as context-dependent by the same priestly institution?',
    'Philological analysis of texts that simultaneously assert Pharaoh as the source of Ma''at and subject to Ma''at; identification of single-authorship contexts where both claims appear.',
    'If co-holdable, the forecloses relation is too strong and should be influences or coexists_with, altering the kernel''s structural topology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_relation, conceptual, 'Logical compatibility of reciprocity and divine mandate readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__reciprocity_reading, 0, 3000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_reciprocity_tr_t0, maat_order_principle__reciprocity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(maat_reciprocity_tr_t500, maat_order_principle__reciprocity_reading, theater_ratio, 500, 0.25).
narrative_ontology:measurement(maat_reciprocity_tr_t1000, maat_order_principle__reciprocity_reading, theater_ratio, 1000, 0.3).
narrative_ontology:measurement(maat_reciprocity_tr_t1500, maat_order_principle__reciprocity_reading, theater_ratio, 1500, 0.35).
narrative_ontology:measurement(maat_reciprocity_tr_t2000, maat_order_principle__reciprocity_reading, theater_ratio, 2000, 0.33).
narrative_ontology:measurement(maat_reciprocity_tr_t2500, maat_order_principle__reciprocity_reading, theater_ratio, 2500, 0.4).
narrative_ontology:measurement(maat_reciprocity_tr_t3000, maat_order_principle__reciprocity_reading, theater_ratio, 3000, 0.45).

% Extraction over time
narrative_ontology:measurement(maat_reciprocity_be_t0, maat_order_principle__reciprocity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(maat_reciprocity_be_t500, maat_order_principle__reciprocity_reading, base_extractiveness, 500, 0.4).
narrative_ontology:measurement(maat_reciprocity_be_t1000, maat_order_principle__reciprocity_reading, base_extractiveness, 1000, 0.45).
narrative_ontology:measurement(maat_reciprocity_be_t1500, maat_order_principle__reciprocity_reading, base_extractiveness, 1500, 0.5).
narrative_ontology:measurement(maat_reciprocity_be_t2000, maat_order_principle__reciprocity_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(maat_reciprocity_be_t2500, maat_order_principle__reciprocity_reading, base_extractiveness, 2500, 0.55).
narrative_ontology:measurement(maat_reciprocity_be_t3000, maat_order_principle__reciprocity_reading, base_extractiveness, 3000, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(maat_reciprocity_su_t0, maat_order_principle__reciprocity_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(maat_reciprocity_su_t500, maat_order_principle__reciprocity_reading, suppression_requirement, 500, 0.5).
narrative_ontology:measurement(maat_reciprocity_su_t1000, maat_order_principle__reciprocity_reading, suppression_requirement, 1000, 0.55).
narrative_ontology:measurement(maat_reciprocity_su_t1500, maat_order_principle__reciprocity_reading, suppression_requirement, 1500, 0.6).
narrative_ontology:measurement(maat_reciprocity_su_t2000, maat_order_principle__reciprocity_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(maat_reciprocity_su_t2500, maat_order_principle__reciprocity_reading, suppression_requirement, 2500, 0.65).
narrative_ontology:measurement(maat_reciprocity_su_t3000, maat_order_principle__reciprocity_reading, suppression_requirement, 3000, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__reciprocity_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
