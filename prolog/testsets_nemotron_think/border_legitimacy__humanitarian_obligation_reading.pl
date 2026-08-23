% ============================================================================
% CONSTRAINT STORY: border_legitimacy__humanitarian_obligation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__humanitarian_obligation_reading, []).

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
 *   constraint_id: border_legitimacy__humanitarian_obligation_reading
 *   human_readable: Humanitarian Obligation Reading of Border Legitimacy
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   This constraint story instantiates the humanitarian_obligation_reading of
 *   the border_legitimacy kernel. It reads the 1951 Refugee Convention and
 *   its 1967 Protocol as establishing a genuine but bounded obligation:
 *   states must admit and protect those fleeing persecution and disaster, but
 *   retain legitimate authority to exclude general economic migrants. The
 *   reading produces a bifurcated victim set — recognized refugees gain
 *   protection while economic migrants, irregular migrants, and
 *   climate-displaced persons bear the costs of categorical exclusion. The
 *   constraint operates as a tangled rope: it coordinates genuine protection
 *   (refugee regime) while extracting through asymmetric exclusion (economic
 *   migrant denial). Active enforcement (asylum adjudication, detention,
 *   removal, deterrence) is required to maintain the distinction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, 0.45).
domain_priors:suppression_score(border_legitimacy__humanitarian_obligation_reading, 0.55).
domain_priors:theater_ratio(border_legitimacy__humanitarian_obligation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__humanitarian_obligation_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__humanitarian_obligation_reading, "Humanitarian Obligation Reading of Border Legitimacy").
narrative_ontology:topic_domain(border_legitimacy__humanitarian_obligation_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__humanitarian_obligation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__humanitarian_obligation_reading, 'df26e126-299f-446a-9705-4f54ae7bbe6b').
narrative_ontology:cs_kernel_codification('df26e126-299f-446a-9705-4f54ae7bbe6b', formalized).
narrative_ontology:cs_authority_grounding('df26e126-299f-446a-9705-4f54ae7bbe6b', lineage).
narrative_ontology:cs_interpretation_layer_present('df26e126-299f-446a-9705-4f54ae7bbe6b').
narrative_ontology:cs_reading_relation('df26e126-299f-446a-9705-4f54ae7bbe6b', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('df26e126-299f-446a-9705-4f54ae7bbe6b', border_legitimacy__freedom_of_movement_reading, coexists_with).
narrative_ontology:cs_axiom('df26e126-299f-446a-9705-4f54ae7bbe6b', foundational, persecution_triggers_admission_obligation).
narrative_ontology:cs_axiom_status(persecution_triggers_admission_obligation, holdable).
narrative_ontology:cs_axiom_grounding('df26e126-299f-446a-9705-4f54ae7bbe6b', persecution_triggers_admission_obligation, conventional).
narrative_ontology:cs_axiom('df26e126-299f-446a-9705-4f54ae7bbe6b', foundational, economic_motivation_excludes_protection).
narrative_ontology:cs_axiom_status(economic_motivation_excludes_protection, holdable).
narrative_ontology:cs_axiom_grounding('df26e126-299f-446a-9705-4f54ae7bbe6b', economic_motivation_excludes_protection, conventional).
narrative_ontology:cs_axiom('df26e126-299f-446a-9705-4f54ae7bbe6b', secondary, non_refoulement_as_jus_cogens).
narrative_ontology:cs_axiom_status(non_refoulement_as_jus_cogens, holdable).
narrative_ontology:cs_axiom_grounding('df26e126-299f-446a-9705-4f54ae7bbe6b', non_refoulement_as_jus_cogens, conventional).
narrative_ontology:cs_reference_frame('df26e126-299f-446a-9705-4f54ae7bbe6b', postwar_refugee_regime_founding).
narrative_ontology:cs_drift_state('df26e126-299f-446a-9705-4f54ae7bbe6b', contemporary_mixed_migration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('df26e126-299f-446a-9705-4f54ae7bbe6b', '').
narrative_ontology:cs_kernel_id(border_legitimacy__humanitarian_obligation_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, states).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, international_organizations).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, recognized_refugees).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, economic_migrants).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, irregular_migrants).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, climate_displaced_persons).
narrative_ontology:constraint_vindicates(border_legitimacy__humanitarian_obligation_reading, refugee_protection_regime).
narrative_ontology:constraint_vindicates(border_legitimacy__humanitarian_obligation_reading, non_refoulement_principle).
narrative_ontology:constraint_vindicates(border_legitimacy__humanitarian_obligation_reading, persecution_based_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the refugee/economic migrant distinction through asylum adjudication systems, border controls, and removal procedures. Benefit from sovereign control over territory and population while claiming compliance with international obligations. Collect political legitimacy from both restrictionist and humanitarian constituencies.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, states, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain access to protection, non-refoulement, and durable solutions (resettlement, integration, return) contingent on meeting the persecution/disaster criterion. Their protection is real but conditional on successful status determination; failed claimants are reclassified as economic migrants and lose all protections.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, recognized_refugees, beneficiary,
    powerless, biographical, trapped, global).

% Bear the full costs of categorical exclusion: denied legal pathways, exposed to dangerous irregular routes, subject to detention and removal, excluded from labor protections. Their migration drivers (poverty, inequality, lack of opportunity) are structurally similar to persecution drivers but legally disqualifying. No effective exit from the exclusion.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, economic_migrants, payer,
    powerless, biographical, constrained, global).

% Caught in the enforcement apparatus that the humanitarian/sovereignty distinction requires: detention, expedited removal, criminalization of movement, denial of basic services. Includes both failed asylum seekers and those who never accessed the system. The distinction's enforcement machinery falls heaviest here.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, irregular_migrants, payer,
    powerless, immediate, trapped, global).

% Falling outside the persecution/disaster framework (disaster = sudden-onset, not slow-onset climate change), they have no protection pathway. Growing category with zero legal recognition under current reading. Their exclusion is structural, not incidental.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, climate_displaced_persons, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__humanitarian_obligation_reading, climate_displaced_persons, excluded).

% UNHCR, IOM, and treaty bodies derive mandate, funding, and operational relevance from administering the refugee protection regime. They shape interpretation of the Convention, monitor compliance, and coordinate state responses. Their institutional survival depends on the distinction's persistence.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, international_organizations, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__humanitarian_obligation_reading, international_organizations, agenda_setter).

% Implement the distinction through asylum screening, detention, removal, and deterrence operations. Receive resources and legal authority from the enforcement requirement. Institutional culture and mission legitimacy tied to 'managing' the boundary between deserving and undeserving migrants.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, border_enforcement_agencies, agenda_setter,
    institutional, biographical, constrained, national).

% Monitor, litigate, and advocate for expanded protection categories and procedural fairness. Occupy a structural position between the regime's stated values and its exclusionary operation. Can influence interpretation but cannot alter the foundational distinction.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, advocacy_ngos, observer,
    moderate, biographical, mobile, global).

% Scholars, courts, and policy analysts who evaluate the regime's coherence, effectiveness, and justice. See the full structure: the coordination function (protection), the extraction function (categorical exclusion), and the enforcement machinery linking them.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured, legally binding framework for protecting persons fleeing persecution and disaster, replacing ad hoc charity with state obligations and international supervision, while preserving state control over general migration.
% TRANSFER_FUNCTION: Transfers protection resources (asylum, non-refoulement, integration support, resettlement slots) from states and international organizations to recognized refugees. Transfers exclusion costs (dangerous journeys, detention, removal, precarious status, denied rights) to economic migrants, irregular migrants, and climate-displaced persons.
% ABSENT_VOICES: Economic migrants (the global poor seeking survival), climate-displaced persons (slow-onset environmental collapse), stateless persons (no state to claim them), and future generations facing uninhabitable territories. They are structurally excluded from the protection conversation because the distinction defines them out of its moral universe.
% DISAPPEARANCE_RATIONALE: If the humanitarian obligation vanished overnight, the 1951 Refugee Convention and its protocol would lose operative force. States would revert to pure sovereignty over admission (sovereignty_reading). The international protection infrastructure (UNHCR, asylum systems, resettlement) would collapse. Millions currently protected would face immediate refoulement. The global migration order would reorganize around unilateral state discretion.
% FOUNDING_PROBLEM: Post-WWII displacement crisis (40+ million displaced in Europe) required a structured protection system that balanced moral obligation with state consent, avoiding both open borders and total abandonment of refugees.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR founding documents and 1951 Convention travaux préparatoires corroborate the founding problem as stated. However, sovereignty_reading proponents argue the problem was always a pretext for Western migration control; freedom_of_movement_reading proponents argue the problem was misdiagnosed — the real crisis was border violence, not displacement. No consensus outside the humanitarian regime's own institutional memory.
narrative_ontology:disappearance_verdict(border_legitimacy__humanitarian_obligation_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__humanitarian_obligation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__humanitarian_obligation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_legitimacy__humanitarian_obligation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__humanitarian_obligation_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__humanitarian_obligation_reading_tests).
:- end_tests(border_legitimacy__humanitarian_obligation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the protection function is real and resource-intensive — states and IOs transfer tangible protections to refugees. But extraction is structurally significant: the persecution/economic migrant distinction excludes the global poor from legal mobility, forcing them into irregular channels where they face exploitation, detention, and death. Suppression (0.55) reflects the enforcement apparatus needed to maintain the distinction: border controls, carrier sanctions, detention centers, expedited removal, deterrence policies. Theater ratio (0.30) captures the growing gap between protection rhetoric and deterrence practice — externalization agreements, pushbacks, and 'safe third country' rules perform compliance while evading obligation. Accessibility collapse (0.50) and resistance (0.55) are moderate: alternatives exist (irregular migration, smuggling, legal challenges) but are costly and dangerous; resistance comes from migrants, NGOs, and some courts but is fragmented.
 *
 * PERSPECTIVAL GAP:
 *   From the state seat, the constraint is a rope — a coordination solution to the refugee protection problem that preserves sovereignty. From the refugee seat, it is a mountain — protection is a non-negotiable right once status is recognized. From the economic migrant seat, it is a snare — the distinction is a cover for exclusion, enforced by violence. From the analytical seat, it is a tangled rope — genuine coordination hybridized with asymmetric extraction. The engine computes this divergence from the declared structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   States are agenda_setters with arbitrage-grade exit — they write the rules, control enforcement, and can denounce treaties (though rarely do). Recognized refugees are beneficiaries but trapped — protection is conditional on a status determination they cannot control. Economic migrants, irregular migrants, and climate-displaced persons are payers with trapped or constrained exit — they bear exclusion costs with no legal pathway. International organizations are dual-positioned: beneficiaries of the regime's mandate, agenda_setters of its interpretation. Border enforcement agencies are agenda_setters implementing the distinction. Advocacy NGOs and analytical observers see the full structure but lack structural power to alter it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-WWII European displacement) is historically resolved, but the regime treats it as live. The humanitarian obligation reading uses the founding problem to justify a permanent institutional structure that now governs a qualitatively different global displacement landscape (climate change, inequality-driven migration, protracted conflicts). Mandatrophy is unresolved: the regime persists by expanding its operational scope (IDPs, stateless persons) while narrowing its protection scope (deterrence, externalization, safe third country). The coordination function (protection) atrophies while the extraction function (exclusion) intensifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing_ambiguity,
    'Does the humanitarian_obligation_reading represent a genuine structural interpretation of the border_legitimacy kernel, or is it a strategic framing that stabilizes the sovereignty_reading by offering a bounded concession?',
    'Genealogical analysis of the 1951 Convention drafting history: whether the persecution/economic distinction was a sincere moral compromise or a deliberate limitation preserving state control over the global poor.',
    'If strategic framing, the reading''s claimed coordination function is partly performative — the distinction serves to legitimate the broader exclusionary regime. Classification would shift toward snare from the economic migrant seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_ambiguity, conceptual, 'Whether the reading''s framing is sincere interpretation or strategic stabilization of sovereignty.').

omega_variable(
    persecution_economic_distinction_stability,
    'Is the persecution/economic migrant distinction structurally stable, or does it collapse under climate displacement, mixed migration flows, and structural violence that blurs the categories?',
    'Empirical tracking of asylum recognition rates, complementary protection expansion, and climate displacement jurisprudence. If the distinction requires ever-more-complex legal fictions to maintain, it is collapsing.',
    'If collapsing, the constraint''s theater ratio rises (more performance to maintain the distinction) and extraction intensifies (more people pushed into irregularity). The tangled rope classification becomes unstable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(persecution_economic_distinction_stability, empirical, 'Structural stability of the core categorical distinction under contemporary displacement drivers.').

omega_variable(
    climate_displacement_category_gap,
    'Does the reading''s exclusion of slow-onset climate displacement from ''disaster'' protection constitute a structural gap that will force reclassification, or a manageable boundary that the interpretation layer can absorb?',
    'Monitor UNHCR guidance, regional treaty developments (e.g. Cartagena Declaration, Kampala Convention), and climate litigation. If protection pathways emerge without Convention revision, the interpretation layer absorbs the gap. If not, the gap becomes a structural contradiction.',
    'If unabsorbable, the reading''s claimed_type becomes unstable — the coordination function fails for a growing population, exposing the extraction function. Omega resolution could trigger reclassification toward snare or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_displacement_category_gap, empirical, 'Whether the climate displacement gap is absorbable by the regime''s interpretation layer or constitutes a structural contradiction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__humanitarian_obligation_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(borhum_tr_t1951, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1951, 0.1).
narrative_ontology:measurement(borhum_tr_t1967, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(borhum_tr_t1980, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(borhum_tr_t1990, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(borhum_tr_t2001, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2001, 0.32).
narrative_ontology:measurement(borhum_tr_t2015, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(borhum_tr_t2024, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(borhum_be_t1951, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1951, 0.25).
narrative_ontology:measurement(borhum_be_t1967, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1967, 0.3).
narrative_ontology:measurement(borhum_be_t1980, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1980, 0.38).
narrative_ontology:measurement(borhum_be_t1990, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(borhum_be_t2001, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2001, 0.45).
narrative_ontology:measurement(borhum_be_t2015, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement(borhum_be_t2024, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(borhum_su_t1951, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1951, 0.3).
narrative_ontology:measurement(borhum_su_t1967, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1967, 0.35).
narrative_ontology:measurement(borhum_su_t1980, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(borhum_su_t1990, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(borhum_su_t2001, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2001, 0.6).
narrative_ontology:measurement(borhum_su_t2015, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(borhum_su_t2024, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__humanitarian_obligation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_legitimacy__humanitarian_obligation_reading, 0.12).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__freedom_of_movement_reading).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, asylum_adjudication_systems).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, migration_deterrence_infrastructure).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, climate_displacement_protection_gap).

% DUAL FORMULATION NOTE:
% This constraint is one member of the border_legitimacy constraint family (kernel_id: border_legitimacy). The three readings (humanitarian_obligation_reading, sovereignty_reading, freedom_of_movement_reading) instantiate different constraints from the same kernel. They differ in ε (this reading: moderate 0.45; sovereignty_reading: low ~0.15; freedom_of_movement_reading: high ~0.75), victim sets (this reading: bifurcated; sovereignty_reading: all non-citizens; freedom_of_movement_reading: all mobility-restricted persons), and coordination functions. Linked via network.affects_constraints for contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_legitimacy__humanitarian_obligation_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
