% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__expansive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__expansive_reading, []).

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
 *   constraint_id: lausanne_minority_protections__expansive_reading
 *   human_readable: Lausanne Treaty's Expansive Protections for Religious Minorities
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This constraint represents the 'expansive reading' of the Lausanne
 *   Treaty's protections for non-Muslim minorities in Turkey. This reading
 *   emphasizes the guarantee of functional continuity for pre-1923 religious
 *   governance, including institutional self-administration, property rights,
 *   and the right to clergy formation via theological schools. It frames
 *   these as essential for the survival of minority communities, rather than
 *   merely individual worship rights. This is one reading of a contested
 *   kernel, with other readings offering more restrictive or
 *   internationally-focused interpretations.
 *
 * KEY AGENTS:
 *   - religious_minority_institutions: Primary beneficiary (organized/constrained)
 *   - religious_minority_communities: Primary beneficiary (powerless/identity_locked)
 *   - turkish_state: Agenda setter (institutional/constrained)
 *   - nationalist_factions_within_turkey: Payer (organized/constrained)
 *   - guarantor_states_eu_bodies: Observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__expansive_reading, 0.15).
domain_priors:suppression_score(lausanne_minority_protections__expansive_reading, 0.4).
domain_priors:theater_ratio(lausanne_minority_protections__expansive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__expansive_reading, rope).
narrative_ontology:human_readable(lausanne_minority_protections__expansive_reading, "Lausanne Treaty's Expansive Protections for Religious Minorities").
narrative_ontology:topic_domain(lausanne_minority_protections__expansive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__expansive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__expansive_reading, '96992e70-4526-409e-8105-913f68235ddd').
narrative_ontology:cs_kernel_codification('96992e70-4526-409e-8105-913f68235ddd', fixed_text).
narrative_ontology:cs_authority_grounding('96992e70-4526-409e-8105-913f68235ddd', lineage).
narrative_ontology:cs_interpretation_layer_present('96992e70-4526-409e-8105-913f68235ddd').
narrative_ontology:cs_reading_relation('96992e70-4526-409e-8105-913f68235ddd', lausanne_minority_protections__restrictive_reading, forecloses).
narrative_ontology:cs_reading_relation('96992e70-4526-409e-8105-913f68235ddd', lausanne_minority_protections__guarantor_reading, coexists_with).
narrative_ontology:cs_axiom('96992e70-4526-409e-8105-913f68235ddd', foundational, minority_institutional_autonomy_guaranteed).
narrative_ontology:cs_axiom_status(minority_institutional_autonomy_guaranteed, holdable).
narrative_ontology:cs_axiom_grounding('96992e70-4526-409e-8105-913f68235ddd', minority_institutional_autonomy_guaranteed, conventional).
narrative_ontology:cs_axiom('96992e70-4526-409e-8105-913f68235ddd', secondary, theological_education_right_for_continuity).
narrative_ontology:cs_axiom_status(theological_education_right_for_continuity, holdable).
narrative_ontology:cs_axiom_grounding('96992e70-4526-409e-8105-913f68235ddd', theological_education_right_for_continuity, conventional).
narrative_ontology:cs_reference_frame('96992e70-4526-409e-8105-913f68235ddd', post_lausanne_status_quo).
narrative_ontology:cs_drift_state('96992e70-4526-409e-8105-913f68235ddd', contemporary_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('96992e70-4526-409e-8105-913f68235ddd', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__expansive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, religious_minority_institutions).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, religious_minority_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lausanne_minority_protections__expansive_reading, nationalist_factions_within_turkey).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rely on these protections for their legal status, property rights, and ability to train clergy. Their functional continuity and cultural preservation are directly tied to the treaty's enforcement.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, religious_minority_institutions, beneficiary,
    organized, generational, constrained, national).

% Their cultural and religious identity is preserved through the continuity of their institutions, which are protected by the treaty. For many, their self-concept is fused with their community and its traditions; exit means abandoning their heritage.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, religious_minority_communities, beneficiary,
    powerless, generational, identity_locked, national).

% As a signatory to the Lausanne Treaty, the state is legally bound to uphold these protections. However, it faces internal political pressure to interpret the treaty more restrictively, balancing international obligations with domestic sovereignty concerns.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, turkish_state, agenda_setter,
    institutional, generational, constrained, national).

% Bear the 'cost' of the state's adherence to the expansive interpretation, as it limits their preferred policies of national unity and secularism. They actively lobby for a restrictive reading of the treaty, viewing expansive minority rights as an anachronism or a threat to national cohesion.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, nationalist_factions_within_turkey, payer,
    organized, biographical, constrained, national).

% Monitor Turkey's compliance with the Lausanne Treaty and broader human rights obligations. They apply diplomatic pressure, issue reports, and can invoke international legal mechanisms when violations of minority rights are perceived.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, guarantor_states_eu_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the continued existence and self-governance of pre-1923 religious minority institutions within the Turkish state, ensuring their cultural and religious continuity by guaranteeing specific rights.
% TRANSFER_FUNCTION: Transfers the right to institutional self-administration, property ownership, and theological education to religious minority institutions, thereby limiting the state's ability to impose general laws that would undermine these specific protections.
% ABSENT_VOICES: Hardline nationalist legal scholars and political factions within Turkey, who advocate for a purely secular, unitary state, are structurally present in domestic discourse but excluded from the international framing of the treaty as a protective instrument. They would argue for a purely domestic interpretation of minority rights.
% DISAPPEARANCE_RATIONALE: If these protections vanished overnight, religious minority institutions would lose their legal basis for self-administration, property, and education. This would likely lead to their dissolution or absorption into state control, fundamentally altering the social and religious landscape for these communities and potentially triggering international condemnation.
% FOUNDING_PROBLEM: To establish a framework for the protection of non-Muslim minorities in the newly formed Republic of Turkey, ensuring their cultural and religious continuity and preventing further population displacement after the collapse of the Ottoman Empire and population exchanges.
% FOUNDING_PROBLEM_CORROBORATION: Religious minority leaders, international human rights organizations, and historical scholars consistently attest to the ongoing relevance of these protections for the survival of minority communities and the stability of regional relations. They argue that the underlying problem of minority protection remains pertinent.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__expansive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__expansive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__expansive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(lausanne_minority_protections__expansive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__expansive_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__expansive_reading_tests).
:- end_tests(lausanne_minority_protections__expansive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because this reading frames the constraint as a protection and guarantee, not a mechanism for extraction from beneficiaries. Suppression is moderate (0.4) as it requires active and consistent state compliance against internal pressures. The theater ratio is low (0.1) because the protections, when upheld, are genuinely functional for the survival of minority institutions. Accessibility collapse is moderate (0.4) as alternatives for minorities (e.g., operating informally) exist but are significantly less secure. Resistance is low (0.2) from the perspective of the constraint's function, as beneficiaries actively support its enforcement; resistance primarily comes from those who oppose the constraint's expansive interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The Turkish state, as the agenda-setter, may perceive these protections as a constraint on its sovereignty, while religious minority communities view them as an existential lifeline. Nationalist factions within Turkey see the expansive reading as an imposition, whereas guarantor states view it as a fundamental international obligation. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious minority institutions and communities are clear beneficiaries (low d) as the constraint directly protects their rights and ensures their continuity. The Turkish state, while the enforcer, is also structurally constrained by these international obligations (d near symmetric, leaning slightly towards target due to internal pressures). Nationalist factions within Turkey are indirect targets/payers (high d) as the constraint limits their preferred policies. Guarantor states are analytical observers (d near 0.5).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expansive_vs_restrictive_interpretation,
    'Is the Lausanne Treaty''s intent primarily to guarantee broad institutional autonomy for minorities, or is it limited to individual worship rights?',
    'Analysis of historical diplomatic records, subsequent state practice, and rulings by international courts or human rights bodies on similar treaty provisions.',
    'If a restrictive interpretation gains dominance, the constraint would reclassify towards a Snare or Tangled Rope for minority institutions, with significantly higher extraction and suppression, as their institutional existence would be precarious.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(expansive_vs_restrictive_interpretation, conceptual, 'Ambiguity regarding the scope of Lausanne Treaty protections.').

omega_variable(
    domestic_vs_international_enforcement,
    'To what extent are the Lausanne protections solely a matter of domestic Turkish law and interpretation, versus being subject to international supervision and enforcement by guarantor states or human rights mechanisms?',
    'Precedent from international legal challenges, diplomatic interventions by guarantor states, and the Turkish state''s response to such external pressures.',
    'If enforcement is deemed purely domestic, the constraint''s effective suppression for minority institutions would be higher, as their recourse options would be severely limited. If international enforcement is robust, it acts as a counter-pressure, potentially lowering effective suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_vs_international_enforcement, empirical, 'Ambiguity over the primary enforcement mechanism for Lausanne protections.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__expansive_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1990, lausanne_minority_protections__expansive_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(laus_tr_t1995, lausanne_minority_protections__expansive_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(laus_tr_t2000, lausanne_minority_protections__expansive_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(laus_tr_t2005, lausanne_minority_protections__expansive_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(laus_tr_t2010, lausanne_minority_protections__expansive_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(laus_tr_t2015, lausanne_minority_protections__expansive_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(laus_tr_t2020, lausanne_minority_protections__expansive_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(laus_be_t1990, lausanne_minority_protections__expansive_reading, base_extractiveness, 1990, 0.12).
narrative_ontology:measurement(laus_be_t1995, lausanne_minority_protections__expansive_reading, base_extractiveness, 1995, 0.13).
narrative_ontology:measurement(laus_be_t2000, lausanne_minority_protections__expansive_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(laus_be_t2005, lausanne_minority_protections__expansive_reading, base_extractiveness, 2005, 0.14).
narrative_ontology:measurement(laus_be_t2010, lausanne_minority_protections__expansive_reading, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement(laus_be_t2015, lausanne_minority_protections__expansive_reading, base_extractiveness, 2015, 0.15).
narrative_ontology:measurement(laus_be_t2020, lausanne_minority_protections__expansive_reading, base_extractiveness, 2020, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1990, lausanne_minority_protections__expansive_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(laus_su_t1995, lausanne_minority_protections__expansive_reading, suppression_requirement, 1995, 0.37).
narrative_ontology:measurement(laus_su_t2000, lausanne_minority_protections__expansive_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement(laus_su_t2005, lausanne_minority_protections__expansive_reading, suppression_requirement, 2005, 0.39).
narrative_ontology:measurement(laus_su_t2010, lausanne_minority_protections__expansive_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(laus_su_t2015, lausanne_minority_protections__expansive_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(laus_su_t2020, lausanne_minority_protections__expansive_reading, suppression_requirement, 2020, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__expansive_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'lausanne_minority_protections' kernel. Other readings, such as 'restrictive_reading' and 'guarantor_reading', offer alternative interpretations of the treaty's scope and enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
