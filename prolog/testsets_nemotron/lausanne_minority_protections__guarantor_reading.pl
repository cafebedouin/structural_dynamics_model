% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__guarantor_reading, []).

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
 *   constraint_id: lausanne_minority_protections__guarantor_reading
 *   human_readable: Lausanne Minority Protections — Guarantor State Diplomacy Pathway
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   The Lausanne Treaty (1923) minority protections establish an
 *   internationally supervised framework where guarantor states (originally
 *   France, UK, Italy, Japan) and later European human rights mechanisms
 *   provide diplomatic and legal oversight of Turkey's treatment of its
 *   non-Muslim minorities. This reading emphasizes the procedural-diplomatic
 *   pathway: the treaty creates a standing invitation for external scrutiny
 *   and interstate diplomacy, but lacks automatic enforcement. It functions
 *   as a scaffold — a transitional coordination mechanism meant to prevent
 *   great power conflict over minority persecution, with the expectation that
 *   domestic rule of law would eventually render external supervision
 *   unnecessary. The sunset clause is implicit in the treaty's logic:
 *   supervision persists only while the founding problem (interstate conflict
 *   risk) persists.
 *
 * KEY AGENTS:
 *   - minority_communities_lausanne: Primary beneficiary (moderate/constrained) — receives diplomatic protection but cannot enforce
 *   - turkish_state_sovereignty_claim: Primary payer (institutional/constrained) — bears diplomatic costs, resists external adjudication
 *   - guarantor_states: Agenda setter / secondary beneficiary (powerful/mobile) — wields diplomatic leverage, gains conflict-prevention coordination
 *   - european_human_rights_bodies: Agenda setter (institutional/analytical) — provides authoritative interpretation, institutional legitimacy depends on treaty relevance
 *   - expansive_reading_advocates: Excluded (moderate/identity_locked) — maximalist claims break diplomatic consensus
 *   - restrictive_reading_advocates: Excluded (institutional/mobile) — rejects supervisory premise, tactically flexible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__guarantor_reading, 0.18).
domain_priors:suppression_score(lausanne_minority_protections__guarantor_reading, 0.32).
domain_priors:theater_ratio(lausanne_minority_protections__guarantor_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__guarantor_reading, scaffold).
narrative_ontology:human_readable(lausanne_minority_protections__guarantor_reading, "Lausanne Minority Protections — Guarantor State Diplomacy Pathway").
narrative_ontology:topic_domain(lausanne_minority_protections__guarantor_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__guarantor_reading).
narrative_ontology:has_sunset_clause(lausanne_minority_protections__guarantor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__guarantor_reading, 'e45ac1e8-e4c3-45eb-a417-142cfe77d8b7').
narrative_ontology:cs_kernel_codification('e45ac1e8-e4c3-45eb-a417-142cfe77d8b7', formalized).
narrative_ontology:cs_authority_grounding('e45ac1e8-e4c3-45eb-a417-142cfe77d8b7', lineage).
narrative_ontology:cs_interpretation_layer_present('e45ac1e8-e4c3-45eb-a417-142cfe77d8b7').
narrative_ontology:cs_reading_relation('e45ac1e8-e4c3-45eb-a417-142cfe77d8b7', lausanne_minority_protections__expansive_reading, coexists_with).
narrative_ontology:cs_reading_relation('e45ac1e8-e4c3-45eb-a417-142cfe77d8b7', lausanne_minority_protections__restrictive_reading, coexists_with).
narrative_ontology:cs_axiom('e45ac1e8-e4c3-45eb-a417-142cfe77d8b7', foundational, international_supervision_operationalizes_treaty_obligations).
narrative_ontology:cs_axiom_status(international_supervision_operationalizes_treaty_obligations, holdable).
narrative_ontology:cs_axiom_grounding('e45ac1e8-e4c3-45eb-a417-142cfe77d8b7', international_supervision_operationalizes_treaty_obligations, conventional).
narrative_ontology:cs_axiom('e45ac1e8-e4c3-45eb-a417-142cfe77d8b7', foundational, guarantor_state_diplomacy_is_primary_enforcement_mechanism).
narrative_ontology:cs_axiom_status(guarantor_state_diplomacy_is_primary_enforcement_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('e45ac1e8-e4c3-45eb-a417-142cfe77d8b7', guarantor_state_diplomacy_is_primary_enforcement_mechanism, conventional).
narrative_ontology:cs_reference_frame('e45ac1e8-e4c3-45eb-a417-142cfe77d8b7', treaty_diplomatic_supervision_framework).
narrative_ontology:cs_drift_state('e45ac1e8-e4c3-45eb-a417-142cfe77d8b7', contemporary_european_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e45ac1e8-e4c3-45eb-a417-142cfe77d8b7', '2026-08-03T14:30:00Z').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, minority_communities_lausanne).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, guarantor_states).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, european_human_rights_bodies).
narrative_ontology:constraint_victim(lausanne_minority_protections__guarantor_reading, turkish_state_sovereignty_claim).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__guarantor_reading, international_supervision_of_minority_rights).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__guarantor_reading, diplomatic_enforcement_of_treaty_obligations).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__guarantor_reading, supranational_human_rights_mechanisms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Greek Orthodox, Armenian, and Jewish communities in Turkey whose institutional continuity depends on international recognition of their Lausanne Treaty rights. They can invoke European Court of Human Rights rulings and guarantor state diplomatic pressure, but cannot independently enforce treaty compliance. Their exit options are constrained by demographic decline and political marginalization.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, minority_communities_lausanne, beneficiary,
    moderate, biographical, constrained, regional).

% The Turkish Republic asserts exclusive domestic jurisdiction over minority affairs under general Turkish law. It bears the diplomatic and reputational costs of international supervision, periodic condemnations by European bodies, and guarantor state interventions. It cannot fully exit the treaty framework without major geopolitical consequences, but it resists external adjudication through domestic legal restructuring.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, turkish_state_sovereignty_claim, payer,
    institutional, generational, constrained, national).

% France, UK, Italy, Japan (original Lausanne signatories) plus later EU members. They possess diplomatic leverage to raise minority rights in bilateral and multilateral forums. They benefit from the treaty's function as a managed coordination mechanism that prevents minority crises from escalating into interstate conflicts. Their exit is mobile — they can calibrate diplomatic pressure up or down based on broader strategic interests.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, guarantor_states, agenda_setter,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, guarantor_states, beneficiary).

% European Court of Human Rights, Committee of Ministers, Venice Commission. They provide the authoritative interpretive layer that translates Lausanne's vague protections into binding judgments. They do not collect rents but their institutional legitimacy depends on the treaty system's continued relevance. Their position is analytical — they observe and adjudicate rather than extract or pay.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, european_human_rights_bodies, agenda_setter,
    institutional, generational, analytical, continental).

% Minority community leaders and legal scholars who argue Lausanne guarantees full institutional autonomy (property, clergy formation, self-administration). They are excluded from the guarantor reading's narrower diplomatic pathway because their maximalist claims exceed what guarantor states are willing to enforce. Their identity is fused to the expansive reading — professional and communal legitimacy depends on maintaining the maximalist position.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, expansive_reading_advocates, excluded,
    moderate, biographical, identity_locked, regional).

% Turkish state legal apparatus and aligned scholars who read Lausanne as limited to individual worship. They are excluded from the guarantor reading's international supervision framework because they reject its premise. Their position is mobile — they can pivot between restrictive interpretation and tactical concessions without identity cost, as their role is institutional function rather than communal survival.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, restrictive_reading_advocates, excluded,
    institutional, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a managed diplomatic pathway for minority rights disputes that prevents escalation to interstate conflict, replacing ad hoc great power intervention with a standing treaty supervision mechanism.
% TRANSFER_FUNCTION: Transfers adjudicative authority over minority rights from exclusive domestic jurisdiction to a shared international-diplomatic arena. Moves political capital from Turkish state sovereignty to guarantor states and European bodies. Moves existential security from minority communities' domestic vulnerability to international legal recognition.
% ABSENT_VOICES: The expansive reading advocates (minority communal leadership demanding full institutional autonomy) and restrictive reading advocates (Turkish state apparatus asserting exclusive domestic jurisdiction) are structurally excluded from the guarantor reading's diplomatic compromise. The expansive voices are excluded because their maximalism would break the diplomatic consensus; the restrictive voices are excluded because they reject the supervisory premise entirely. Both would object to the guarantor reading's middle position but are not seated at its negotiation table.
% DISAPPEARANCE_RATIONALE: If the guarantor reading's diplomatic pathway vanished, minority communities would lose their only functioning external recourse; Turkey would face no structured diplomatic pressure on minority rights; guarantor states would lose a calibrated leverage tool; European bodies would lose a major treaty-based docket. The system would revert to ad hoc great power intervention or complete domestic closure — a significant rearrangement.
% FOUNDING_PROBLEM: Post-Ottoman settlement required preventing minority persecution from triggering interstate wars among the great powers, while respecting Turkish sovereignty enough to secure the treaty's ratification. The Lausanne Treaty's minority provisions (Articles 37-45) created a supervised but non-autonomous framework — international guarantee without institutional self-rule.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Treaty text itself and the 1923 diplomatic record (League of Nations archives). The 'contested' status is corroborated by: (1) minority communities who argue the founding problem persists because persecution continues in new forms; (2) Turkish state which argues the founding problem is resolved because the Ottoman collapse context is gone and modern Turkey is a rule-of-law state; (3) guarantor states which maintain the problem is live but managed. No single party's attestation is decisive.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__guarantor_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__guarantor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(lausanne_minority_protections__guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__guarantor_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__guarantor_reading_tests).
:- end_tests(lausanne_minority_protections__guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint primarily creates a diplomatic pathway rather than extracting resources — the 'cost' to Turkey is reputational and sovereignty-constraining, not material extraction. Suppression is moderate (0.32) because Turkey actively resists through domestic legal restructuring, but the constraint's persistence does not depend on crushing alternatives — the diplomatic pathway coexists with domestic law. Theater ratio is elevated (0.45) because the supervisory mechanism has become increasingly performative: European Court judgments on minority rights (e.g., Bozcaada/Kimyada, Vakiflar) are often unimplemented or minimally implemented, while the diplomatic ritual continues. Accessibility collapse is low (0.25) because domestic legal reform remains a genuine alternative pathway — Turkey could resolve the supervision by upgrading domestic protections. Resistance is moderate-high (0.55) because the Turkish state actively contests the supervisory framework's legitimacy, not merely its application.
 *
 * PERSPECTIVAL GAP:
 *   The guarantor states and European bodies experience this as a functioning coordination scaffold (low extraction, genuine diplomatic utility). The minority communities experience it as a fragile lifeline — real but insufficient protection. The Turkish state experiences it as an infringement on sovereignty that it must manage but cannot eliminate. The expansive reading advocates experience it as a betrayal — the diplomatic compromise abandoned their institutional autonomy claims. The restrictive reading advocates experience it as an illegitimate imposition they tolerate only tactically. The engine will compute these as divergent seat classifications from the shared structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Guarantor states and European bodies are structural beneficiaries (d near 0.0) — they gain diplomatic coordination and institutional relevance without bearing costs. Minority communities are partial beneficiaries (d ~0.3) — they receive protection but remain demographically vulnerable and politically marginalized. The Turkish state is the primary target (d ~0.8) — it bears the full diplomatic and reputational burden of supervision. The expansive advocates are identity-locked excluded (d ~0.9) — their maximalist position makes the guarantor reading's compromise feel like extraction. The restrictive advocates are mobile excluded (d ~0.2) — they can engage or disengage tactically.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold's founding problem (preventing great power conflict over minority persecution) has partially mutated. The original interstate war risk has receded, but new forms of minority marginalization persist. The constraint persists because: (1) guarantor states retain the diplomatic tool; (2) European bodies need the docket for institutional legitimacy; (3) minority communities have no alternative recourse; (4) Turkey prefers managed supervision to unpredictable ad hoc pressure. This is mandatrophy in the precise sense: the original coordination function (great power conflict prevention) has attenuated, but the constraint remains because no party bears enough concentrated cost to dismantle it and no party gains enough concentrated benefit to transform it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    guarantor_will_persistence,
    'Will guarantor states maintain diplomatic pressure on minority rights when it conflicts with broader strategic interests (NATO, energy, migration)?',
    'Track diplomatic interventions on Lausanne minority provisions vs. other bilateral agenda items over 5-year windows. Measure correlation between guarantor state pressure and minority rights outcomes.',
    'If guarantor will is contingent on broader interests, the scaffold''s coordination function is parasitic on external alignment — the constraint collapses when interests diverge. If guarantor will is structurally embedded, the scaffold has independent persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guarantor_will_persistence, empirical, 'Whether the scaffold''s agenda-setter has structural commitment or tactical flexibility').

omega_variable(
    supervision_vs_autonomy_boundary,
    'Is the diplomatic supervision pathway structurally compatible with the expansive reading''s institutional autonomy claims, or does accepting supervision foreclose autonomy?',
    'Analyze European Court jurisprudence: does the Court''s Article 9/Protocol 1 reasoning treat minority institutional autonomy as a necessary component of religious freedom, or as a separable political question?',
    'If compatible, the guarantor reading could evolve toward the expansive reading via judicial interpretation. If incompatible, the readings are forked — the diplomatic pathway''s proceduralism structurally limits substantive autonomy claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supervision_vs_autonomy_boundary, conceptual, 'Whether the diplomatic scaffold can bridge to institutional autonomy or constitutes a ceiling').

omega_variable(
    mandatrophy_transition_trigger,
    'What would constitute the ''sunset condition'' where domestic rule of law renders international supervision unnecessary?',
    'Define measurable benchmarks: (a) domestic court recognition of minority institutional legal personality; (b) property restitution framework operational for 10+ years; (c) theological education pathway legally recognized; (d) zero adverse ECHR judgments on minority rights for 5 consecutive years.',
    'Without defined sunset criteria, the scaffold drifts into piton — permanent diplomatic theater without transition logic. With criteria, the scaffold''s provisional nature becomes operational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_transition_trigger, preference, 'Whether the scaffold has an operational sunset condition or has become inertial').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__guarantor_reading, 1923, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lausanne_guarantor_tr_t1923, lausanne_minority_protections__guarantor_reading, theater_ratio, 1923, 0.25).
narrative_ontology:measurement(lausanne_guarantor_tr_t1950, lausanne_minority_protections__guarantor_reading, theater_ratio, 1950, 0.35).
narrative_ontology:measurement(lausanne_guarantor_tr_t1975, lausanne_minority_protections__guarantor_reading, theater_ratio, 1975, 0.42).
narrative_ontology:measurement(lausanne_guarantor_tr_t1990, lausanne_minority_protections__guarantor_reading, theater_ratio, 1990, 0.48).
narrative_ontology:measurement(lausanne_guarantor_tr_t2005, lausanne_minority_protections__guarantor_reading, theater_ratio, 2005, 0.44).
narrative_ontology:measurement(lausanne_guarantor_tr_t2024, lausanne_minority_protections__guarantor_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(lausanne_guarantor_be_t1923, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1923, 0.12).
narrative_ontology:measurement(lausanne_guarantor_be_t1950, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement(lausanne_guarantor_be_t1975, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1975, 0.18).
narrative_ontology:measurement(lausanne_guarantor_be_t1990, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement(lausanne_guarantor_be_t2005, lausanne_minority_protections__guarantor_reading, base_extractiveness, 2005, 0.19).
narrative_ontology:measurement(lausanne_guarantor_be_t2024, lausanne_minority_protections__guarantor_reading, base_extractiveness, 2024, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(lausanne_guarantor_su_t1923, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1923, 0.4).
narrative_ontology:measurement(lausanne_guarantor_su_t1950, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1950, 0.38).
narrative_ontology:measurement(lausanne_guarantor_su_t1975, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1975, 0.35).
narrative_ontology:measurement(lausanne_guarantor_su_t1990, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(lausanne_guarantor_su_t2005, lausanne_minority_protections__guarantor_reading, suppression_requirement, 2005, 0.32).
narrative_ontology:measurement(lausanne_guarantor_su_t2024, lausanne_minority_protections__guarantor_reading, suppression_requirement, 2024, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__guarantor_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__guarantor_reading, 0.1).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections__expansive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, european_convention_human_rights_supervision).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, turkey_eu_accession_process).

% DUAL FORMULATION NOTE:
% This is the guarantor_reading of the lausanne_minority_protections kernel family. The expansive_reading claims substantive institutional autonomy guarantees; the restrictive_reading claims only individual worship rights. This reading claims a diplomatic-procedural pathway. The three readings share the same treaty text but instantiate different constraints with different ε values, beneficiary structures, and enforcement logics. The guarantor reading's ε (0.18) is lower than the expansive reading's (estimated >0.4) because it does not claim resource transfers for institutional autonomy, only diplomatic oversight. The restrictive reading's ε is near-zero for Turkey but high for minorities (denial of protections).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lausanne_minority_protections__guarantor_reading, institutional, 0.75).
constraint_indexing:directionality_override(lausanne_minority_protections__guarantor_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
