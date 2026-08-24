% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__colonial_census_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__colonial_census_reading, []).

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
 *   constraint_id: jati_practice_norm__colonial_census_reading
 *   human_readable: Jati Categories Frozen by Colonial Census Administration
 *   domain: social/political/religious
 *
 * SUMMARY:
 *   The British colonial census (1860s–1940s) transformed fluid,
 *   context-dependent jati boundaries into fixed, enumerated schedules for
 *   administrative legibility. What was a coordination norm subject to
 *   continuous local renegotiation became a legal-administrative fact
 *   enforced by revenue, criminal, and recruitment law. The constraint is
 *   claimed as tangled_rope: it solved a genuine coordination problem
 *   (imperial governance at scale) while extracting autonomy from subject
 *   communities through active enforcement (census operations, criminal
 *   tribes legislation, legal codification). The beneficiaries are the
 *   colonial administrative apparatus; the victims are the communities whose
 *   boundaries were frozen, especially mobile and interstitial groups.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, 0.58).
domain_priors:suppression_score(jati_practice_norm__colonial_census_reading, 0.72).
domain_priors:theater_ratio(jati_practice_norm__colonial_census_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__colonial_census_reading, tangled_rope).
narrative_ontology:human_readable(jati_practice_norm__colonial_census_reading, "Jati Categories Frozen by Colonial Census Administration").
narrative_ontology:topic_domain(jati_practice_norm__colonial_census_reading, "social/political/religious").

domain_priors:requires_active_enforcement(jati_practice_norm__colonial_census_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__colonial_census_reading, '899d9610-8c2a-473d-8e85-8ea23c92f7ad').
narrative_ontology:cs_kernel_codification('899d9610-8c2a-473d-8e85-8ea23c92f7ad', formalized).
narrative_ontology:cs_authority_grounding('899d9610-8c2a-473d-8e85-8ea23c92f7ad', extraction).
narrative_ontology:cs_interpretation_layer_present('899d9610-8c2a-473d-8e85-8ea23c92f7ad').
narrative_ontology:cs_reading_relation('899d9610-8c2a-473d-8e85-8ea23c92f7ad', jati_practice_norm__orthodox_textual_reading, influences).
narrative_ontology:cs_reading_relation('899d9610-8c2a-473d-8e85-8ea23c92f7ad', jati_practice_norm__localized_practice_reading, forecloses).
narrative_ontology:cs_axiom('899d9610-8c2a-473d-8e85-8ea23c92f7ad', foundational, census_categories_are_administrative_constructs_not_scriptural_facts).
narrative_ontology:cs_axiom_status(census_categories_are_administrative_constructs_not_scriptural_facts, holdable).
narrative_ontology:cs_axiom_grounding('899d9610-8c2a-473d-8e85-8ea23c92f7ad', census_categories_are_administrative_constructs_not_scriptural_facts, empirically_contingent).
narrative_ontology:cs_axiom('899d9610-8c2a-473d-8e85-8ea23c92f7ad', secondary, colonial_state_legibility_requires_frozen_social_boundaries).
narrative_ontology:cs_axiom_status(colonial_state_legibility_requires_frozen_social_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('899d9610-8c2a-473d-8e85-8ea23c92f7ad', colonial_state_legibility_requires_frozen_social_boundaries, instrumental).
narrative_ontology:cs_reference_frame('899d9610-8c2a-473d-8e85-8ea23c92f7ad', precolonial_negotiated_boundaries).
narrative_ontology:cs_drift_state('899d9610-8c2a-473d-8e85-8ea23c92f7ad', post_independence_constitutional_settlement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('899d9610-8c2a-473d-8e85-8ea23c92f7ad', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__colonial_census_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, colonial_administration).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, imperial_revenue_service).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, british_legal_codifiers).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, subject_communities).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, jati_councils).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, mobile_labor_groups).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, interstitial_occupational_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, jati_councils).
narrative_ontology:constraint_vindicates(jati_practice_norm__colonial_census_reading, administrative_legibility_as_governance_prerequisite).
narrative_ontology:constraint_vindicates(jati_practice_norm__colonial_census_reading, fixed_category_taxation_efficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces census categories that freeze fluid jati boundaries into fixed schedules for revenue extraction, legal administration, and military recruitment. Collects the administrative efficiency gains directly through reduced transaction costs of governance.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, colonial_administration, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, colonial_administration, beneficiary).

% Receives predictable, standardized tax yields from communities now locked into fixed occupational categories. The categorical freeze eliminates the negotiation overhead of dealing with fluid local arrangements.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, imperial_revenue_service, beneficiary,
    institutional, generational, arbitrage, continental).

% Produce the legal codes (e.g., Criminal Tribes Act, caste schedules) that transform administrative categories into enforceable law. Their professional authority and career advancement depend on the categorization project's continuation.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, british_legal_codifiers, beneficiary,
    organized, biographical, mobile, continental).

% Previously negotiated their social boundaries through local practice, marriage, and occupational mobility. Now find their internal diversity compressed into a single census category that determines tax liability, legal status, and access to colonial patronage. Exit requires either accepting the imposed category or facing criminalization.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, subject_communities, payer,
    organized, generational, constrained, regional).

% Traditional bodies that managed boundary negotiation. Some gain recognized authority under the new system (beneficiary), but lose the flexibility to adapt boundaries to changing circumstances (payer). Their legitimacy becomes dependent on colonial recognition.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, jati_councils, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, jati_councils, beneficiary).

% Groups whose livelihoods required seasonal occupational shifting (e.g., pastoralists, forest produce gatherers, itinerant artisans). The census freeze criminalizes their mobility by assigning them a single fixed category. No exit without abandoning their way of life.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, mobile_labor_groups, payer,
    powerless, immediate, trapped, regional).

% Communities that straddled multiple occupational niches or served as intermediaries between jatis. The census forces them into a single category, destroying their economic niche and social bridging function. No administrative pathway to represent their actual situation.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, interstitial_occupational_groups, payer,
    powerless, immediate, trapped, local).

% Brahminical interpreters who claim scriptural authority over varna/jati order. The colonial census bypasses them, substituting administrative fiat for textual exegesis. They would object to the displacement of their interpretive monopoly but are locked into a framework where resistance means irrelevance.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, orthodox_textual_authorities, excluded,
    organized, civilizational, constrained, continental).

% Village-level authorities who historically managed boundary fluidity through customary negotiation. The census renders their role obsolete by replacing negotiated practice with fixed schedules. They are not consulted; their knowledge is treated as raw data for colonial categorization.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, localized_practice_elders, excluded,
    moderate, biographical, constrained, local).

% Analyze the census as a constitutive intervention that produced the 'caste system' as a colonial-modern formation. They see the full structural transformation but hold no leverage over the historical constraint's operation.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, postcolonial_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the colonial state with a legible, enumerable population for taxation, law enforcement, and military recruitment — replacing negotiated, context-dependent social boundaries with standardized administrative categories.
% TRANSFER_FUNCTION: Moves autonomy over social categorization from local communities and traditional authorities to the colonial administrative apparatus; moves fiscal predictability and governance legibility from the population to the imperial state.
% ABSENT_VOICES: Mobile labor groups and interstitial occupational communities were structurally excluded — their ways of life were illegible to the census logic and they had no representation in the categorization process. Orthodox textual authorities and localized practice elders were displaced but not consulted.
% DISAPPEARANCE_RATIONALE: If the colonial census categorization vanished overnight, the fixed schedules undergirding land revenue, criminal law, recruitment, and affirmative action would collapse. Communities would revert to negotiated boundaries, but the postcolonial state has inherited and amplified the categorical architecture — the rearrangement would be contested and incomplete.
% FOUNDING_PROBLEM: The East India Company and later Crown administration faced illegible, negotiated social landscapes that resisted standardized taxation, legal uniformization, and military recruitment quotas. They needed a fixed classification to make the population governable at scale.
% FOUNDING_PROBLEM_CORROBORATION: Colonial administrative records (census reports, revenue manuals, legislative debates) explicitly document the governance illegibility problem as the driver. Postcolonial scholars (Dirks, Cohn, Appadurai) corroborate from outside the beneficiary set that the 'problem' was colonial state-building, not an indigenous demand for fixed categories. No non-colonial source attests that communities sought this freeze.
narrative_ontology:disappearance_verdict(jati_practice_norm__colonial_census_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__colonial_census_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__colonial_census_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jati_practice_norm__colonial_census_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__colonial_census_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__colonial_census_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__colonial_census_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the transfer of categorization authority from communities to state, with moderate but persistent extraction of autonomy. Suppression (0.72) is high because the constraint's persistence depended on active enforcement — census operations, criminalization of mobility, legal penalties for category non-conformity. Theater ratio (0.38) captures the genuine coordination function (governance legibility) mixed with performative scientific racism (ethnographic classification as objective knowledge). Accessibility collapse (0.68) is substantial: once a community was assigned a census category, alternatives (petitioning for reclassification, maintaining fluid practice) were severely constrained. Resistance (0.55) is moderate: communities petitioned, evaded, and strategically performed categories, but the administrative machinery was overwhelming.
 *
 * PERSPECTIVAL GAP:
 *   From the colonial administrator's seat, the census is genuine coordination (rope-like) — it solves the illegibility problem. From the mobile labor group's seat, it is a snare — their way of life is criminalized. From the jati council's seat, it is a tangled rope — they gain colonial recognition but lose adaptive capacity. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Colonial administration and revenue service are structural beneficiaries (d near 0.0) — they collect the legibility gains and face arbitrage-grade exit (could reform or abandon the system). Subject communities, jati councils, mobile groups, and interstitial groups are targets (d near 1.0) — they bear the autonomy loss with constrained to trapped exit. Jati councils are dual-positioned: some gained recognized authority (beneficiary) but lost flexibility (payer). Orthodox textual authorities and localized elders are excluded — displaced from their interpretive role but not direct targets of extraction. Postcolonial historians are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial governance illegibility) is dead — the British Empire is gone. But the categorical architecture persists in postcolonial India's reservation system, criminal law, and political mobilization. This is mandatrophy: the constraint outlived its founding function and was repurposed. The coordination function (governance legibility) remains but the beneficiary shifted from colonial to postcolonial state. The extraction now serves democratic redistribution claims rather than imperial revenue — but the frozen categories remain the same.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colonial_constitutive_vs_revelatory,
    'Did the colonial census constitute the modern caste system by freezing fluid boundaries, or did it merely reveal/reify a pre-existing scriptural order that was already structurally fixed?',
    'Comparative analysis of pre-colonial inscriptions, traveler accounts, and regional variation in boundary fluidity versus post-census rigidity. If pre-colonial records show widespread boundary negotiation and occupational mobility, the constitutive thesis holds.',
    'If constitutive, the constraint''s extraction is the creation of the rigid system itself — a tangled_rope where the coordination function (legibility) is inseparable from the extraction (autonomy loss). If revelatory, the extraction is only the enforcement of an already-fixed order — closer to a rope with enforcement overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_constitutive_vs_revelatory, empirical, 'Whether the census created or revealed the fixed category structure.').

omega_variable(
    postcolonial_beneficiary_shift,
    'Does the postcolonial state''s use of the same categories for affirmative action transform the constraint''s extraction profile from imperial revenue to democratic redistribution, or does it merely redirect the same frozen architecture?',
    'Analysis of whether reservation policies have increased boundary fluidity (through socioeconomic mobility within categories) or further entrenched the census categories as political identities.',
    'If redistribution increases fluidity, the constraint drifts toward scaffold (transitional support with sunset logic). If it entrenches categories as political capital, the constraint remains tangled_rope with shifted beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(postcolonial_beneficiary_shift, conceptual, 'Whether postcolonial repurposing changes the constraint''s structural type.').

omega_variable(
    mobile_groups_suppression_mechanism,
    'For mobile labor groups and interstitial communities, was suppression primarily structural (criminalization, legal penalties) or did it include internalized identity fixation (communities coming to see themselves through the census category)?',
    'Ethnographic and oral history research on whether groups internalized census categories as self-identification versus maintaining parallel self-understandings. Post-exit trajectory: if groups retain fluid self-conception after legal constraints lift, suppression was primarily structural.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint''s extraction includes identity capture that persists after formal enforcement ends. This would elevate the constraint toward snare dynamics for these groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mobile_groups_suppression_mechanism, empirical, 'Structural vs. internalized suppression for the most vulnerable victim groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__colonial_census_reading, 1860, 1947).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_census_tr_t1860, jati_practice_norm__colonial_census_reading, theater_ratio, 1860, 0.2).
narrative_ontology:measurement(jati_census_tr_t1881, jati_practice_norm__colonial_census_reading, theater_ratio, 1881, 0.28).
narrative_ontology:measurement(jati_census_tr_t1901, jati_practice_norm__colonial_census_reading, theater_ratio, 1901, 0.33).
narrative_ontology:measurement(jati_census_tr_t1921, jati_practice_norm__colonial_census_reading, theater_ratio, 1921, 0.38).
narrative_ontology:measurement(jati_census_tr_t1931, jati_practice_norm__colonial_census_reading, theater_ratio, 1931, 0.4).
narrative_ontology:measurement(jati_census_tr_t1947, jati_practice_norm__colonial_census_reading, theater_ratio, 1947, 0.38).

% Extraction over time
narrative_ontology:measurement(jati_census_be_t1860, jati_practice_norm__colonial_census_reading, base_extractiveness, 1860, 0.35).
narrative_ontology:measurement(jati_census_be_t1881, jati_practice_norm__colonial_census_reading, base_extractiveness, 1881, 0.48).
narrative_ontology:measurement(jati_census_be_t1901, jati_practice_norm__colonial_census_reading, base_extractiveness, 1901, 0.55).
narrative_ontology:measurement(jati_census_be_t1921, jati_practice_norm__colonial_census_reading, base_extractiveness, 1921, 0.58).
narrative_ontology:measurement(jati_census_be_t1931, jati_practice_norm__colonial_census_reading, base_extractiveness, 1931, 0.6).
narrative_ontology:measurement(jati_census_be_t1947, jati_practice_norm__colonial_census_reading, base_extractiveness, 1947, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(jati_census_su_t1860, jati_practice_norm__colonial_census_reading, suppression_requirement, 1860, 0.55).
narrative_ontology:measurement(jati_census_su_t1881, jati_practice_norm__colonial_census_reading, suppression_requirement, 1881, 0.65).
narrative_ontology:measurement(jati_census_su_t1901, jati_practice_norm__colonial_census_reading, suppression_requirement, 1901, 0.7).
narrative_ontology:measurement(jati_census_su_t1921, jati_practice_norm__colonial_census_reading, suppression_requirement, 1921, 0.72).
narrative_ontology:measurement(jati_census_su_t1931, jati_practice_norm__colonial_census_reading, suppression_requirement, 1931, 0.73).
narrative_ontology:measurement(jati_census_su_t1947, jati_practice_norm__colonial_census_reading, suppression_requirement, 1947, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__colonial_census_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__colonial_census_reading, 0.12).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__localized_practice_reading).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, postcolonial_reservation_architecture).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, criminal_tribes_act_legacy).

% DUAL FORMULATION NOTE:
% Kernel jati_practice_norm decomposes into three readings with divergent ε: colonial_census_reading (tangled_rope, ε≈0.58) — census as constitutive intervention; orthodox_textual_reading (mountain-claimed, ε≈0.15) — scriptural order as natural law; localized_practice_reading (rope, ε≈0.25) — fluid negotiation as coordination. The colonial reading's enforcement apparatus structurally influences both siblings: it displaced orthodox textual authority (influences) and suppressed localized practice (forecloses within colonial framework).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jati_practice_norm__colonial_census_reading, organized, 0.15).
constraint_indexing:directionality_override(jati_practice_norm__colonial_census_reading, moderate, 0.65).
constraint_indexing:directionality_override(jati_practice_norm__colonial_census_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
