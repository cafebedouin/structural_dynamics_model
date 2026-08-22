% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__international_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__international_regime, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ost_article_ii_non_appropriation__international_regime
 *   human_readable: OST Article II Non-Appropriation — International Regime Reading
 *   domain: international_space_law/treaty_interpretation/commons_governance
 *
 * SUMMARY:
 *   This constraint story captures the 'international regime' reading of OST
 *   Article II's non-appropriation principle. Unlike the
 *   extraction-permissive reading (which allows private ownership of
 *   extracted resources) and the commons-conservation reading (which
 *   prohibits de facto appropriation via extraction), this reading holds that
 *   Article II deliberately defers the appropriation question to a future
 *   multilateral regime under Article XI. The constraint operates as a
 *   scaffold: it provides temporary coordination (freedom of use without
 *   sovereignty claims) while the regime is negotiated, but the negotiation
 *   has stalled for decades due to zero-sum distributional conflict.
 *   First-mover firms and space-capable states benefit from the regulatory
 *   grey zone; developing states and late arrivals bear the opportunity cost.
 *   The constraint's sunset clause is the unresolved regime negotiation
 *   itself — it was meant to be transitional but has persisted for 50+ years.
 *
 * KEY AGENTS:
 *   - early_mover_space_firms: Primary beneficiary (powerful/arbitrage) — operates in grey zone, extracts first-mover rents
 *   - space_capable_states: Agenda setter / secondary beneficiary (institutional/analytical) — hosts firms, blocks regime, gains strategic revenue
 *   - developing_states: Primary payer / excluded (moderate/constrained) — lacks access, depends on regime for equity
 *   - late_arrival_space_actors: Payer (moderate/constrained) — enters after best positions taken, faces legal uncertainty
 *   - common_heritage_claimants: Excluded non-agent (organized/identity_locked) — doctrinal commitment to prohibition, cannot exit
 *   - international_lawyers_scholars: Observer (analytical/analytical) — produces competing readings, no direct stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, 0.15).
domain_priors:suppression_score(ost_article_ii_non_appropriation__international_regime, 0.3).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__international_regime, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, extractiveness, 0.15).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__international_regime, scaffold).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__international_regime, "OST Article II Non-Appropriation — International Regime Reading").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__international_regime, "international_space_law/treaty_interpretation/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__international_regime).
narrative_ontology:has_sunset_clause(ost_article_ii_non_appropriation__international_regime).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__international_regime, '16d2bc1d-8895-40dc-b478-bd8c429d0483').
narrative_ontology:cs_kernel_codification('16d2bc1d-8895-40dc-b478-bd8c429d0483', formalized).
narrative_ontology:cs_authority_grounding('16d2bc1d-8895-40dc-b478-bd8c429d0483', lineage).
narrative_ontology:cs_interpretation_layer_present('16d2bc1d-8895-40dc-b478-bd8c429d0483').
narrative_ontology:cs_reading_relation('16d2bc1d-8895-40dc-b478-bd8c429d0483', ost_article_ii_non_appropriation__extraction_permissive, coexists_with).
narrative_ontology:cs_reading_relation('16d2bc1d-8895-40dc-b478-bd8c429d0483', ost_article_ii_non_appropriation__commons_conservation, coexists_with).
narrative_ontology:cs_axiom('16d2bc1d-8895-40dc-b478-bd8c429d0483', foundational, article_xi_regime_mandatory).
narrative_ontology:cs_axiom_status(article_xi_regime_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('16d2bc1d-8895-40dc-b478-bd8c429d0483', article_xi_regime_mandatory, conventional).
narrative_ontology:cs_axiom('16d2bc1d-8895-40dc-b478-bd8c429d0483', foundational, no_unilateral_appropriation_authority).
narrative_ontology:cs_axiom_status(no_unilateral_appropriation_authority, holdable).
narrative_ontology:cs_axiom_grounding('16d2bc1d-8895-40dc-b478-bd8c429d0483', no_unilateral_appropriation_authority, conventional).
narrative_ontology:cs_axiom('16d2bc1d-8895-40dc-b478-bd8c429d0483', secondary, regime_delay_not_authorization).
narrative_ontology:cs_axiom_status(regime_delay_not_authorization, holdable).
narrative_ontology:cs_axiom_grounding('16d2bc1d-8895-40dc-b478-bd8c429d0483', regime_delay_not_authorization, conventional).
narrative_ontology:cs_reference_frame('16d2bc1d-8895-40dc-b478-bd8c429d0483', ost_article_xi_regime_anticipation).
narrative_ontology:cs_drift_state('16d2bc1d-8895-40dc-b478-bd8c429d0483', contemporary_artemis_accords_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('16d2bc1d-8895-40dc-b478-bd8c429d0483', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, early_mover_space_firms).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, space_capable_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, developing_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, late_arrival_space_actors).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, common_heritage_claimants).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__international_regime, international_regime_principle).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__international_regime, equitable_sharing_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate in regulatory grey zone with first-mover advantage; extract resources without clear legal title; lobby against binding regime that would impose royalties; can relocate operations or restructure corporate entities if regime turns unfavorable
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, early_mover_space_firms, beneficiary,
    powerful, biographical, arbitrage, global).

% Host and regulate early-mover firms; block regime negotiations that would constrain national champions; benefit from tax revenue and strategic capabilities; can withdraw from treaty regime entirely (Art. XVI) but face diplomatic costs
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, space_capable_states, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__international_regime, space_capable_states, beneficiary).

% Lack independent access to space resources; depend on equitable sharing regime for benefit; excluded from de facto appropriation by technology gap; can only exit by building indigenous capacity (decades) or through regime negotiation
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, developing_states, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__international_regime, developing_states, excluded).

% Enter market after best orbits/resources claimed; face higher costs and legal uncertainty; would benefit from clear regime with licensing but blocked by zero-sum negotiation; exit options limited to niche markets or partnership with incumbents
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, late_arrival_space_actors, payer,
    moderate, biographical, constrained, global).

% Advocate Moon Agreement / common heritage regime; claim moral/legal standing as representatives of humankind; identity fused to prohibition of appropriation; cannot exit without abandoning core doctrinal commitment; blocked by non-ratification of Moon Agreement by space powers
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, common_heritage_claimants, excluded,
    organized, civilizational, identity_locked, universal).
narrative_ontology:stakeholder_non_agent(ost_article_ii_non_appropriation__international_regime, common_heritage_claimants).

% Interpret Article II/Article XI interplay; produce competing readings; no direct stake in extraction; career incentives align with maintaining interpretive relevance; exit is changing research focus
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, international_lawyers_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides temporary legal certainty for space resource activities while multilateral regime is negotiated; prevents unilateral territorial claims; creates focal point for regime negotiations by establishing that the appropriation question remains open
% TRANSFER_FUNCTION: Transfers regulatory certainty and first-mover rents to early-mover firms and space-capable states; transfers opportunity cost and legal risk to developing states, late arrivals, and common heritage claimants; regime delay transfers value from future benefit-sharing to present extractors
% ABSENT_VOICES: Future generations who inherit the resource base and regulatory architecture; indigenous peoples with cosmological claims to celestial bodies; non-spacefaring states excluded from UN COPUOS consensus process; these voices are structurally absent because the constraint operates at the level of states and firms with current capacity
% DISAPPEARANCE_RATIONALE: If the international regime reading vanished overnight, either the extraction-permissive reading would fill the vacuum (enabling unchecked appropriation by first movers) or the conservation reading would dominate (blocking all resource activity); the legal vacuum itself is the constraint's product — its disappearance forces resolution one way or another
% FOUNDING_PROBLEM: Cold War deadlock over celestial sovereignty: neither superpower would accept the other's territorial claims, but both wanted freedom of action for resource activities; Article II/Article XI compromise froze the question for future resolution
% FOUNDING_PROBLEM_CORROBORATION: The Cold War bipolar structure that produced the compromise is gone; space-capable states and firms (the benefiting parties) attest the regime negotiation is still live, but independent scholars (Frans von der Dunk, Stephan Hobe, UN COPUOS working group reports) confirm the founding superpower deadlock is dead — the constraint persists without its founding problem
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__international_regime, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__international_regime, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__international_regime, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__international_regime, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__international_regime, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__international_regime_tests).
:- end_tests(ost_article_ii_non_appropriation__international_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.15) because the constraint itself does not extract — it creates a legal vacuum that enables extraction by first movers. The extraction is indirect, mediated by the absence of a regime. Suppression is moderate (0.3): the constraint does not actively coerce but structurally suppresses regime formation by giving space-capable states veto power and first movers incentive to delay. Theater ratio is moderate (0.2): the regime negotiation process continues (UN COPUOS working groups, Hague Working Group) but produces no binding output — performative maintenance of the transitional frame. Accessibility collapse is moderate (0.4): alternatives (Moon Agreement, national licensing regimes) exist but lack universal participation. Resistance is high (0.65): developing states actively push for regime; Moon Agreement advocates resist normalization of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Early-mover firms and space-capable states are structural beneficiaries: they collect rents and strategic advantage from the regulatory vacuum (d ≈ 0.1-0.2). Developing states and late arrivals are payers: they bear opportunity costs and legal risk without compensatory benefit (d ≈ 0.7-0.8). Common heritage claimants are identity-locked excluded parties: their doctrinal commitment makes exit unthinkable, but they lack structural power to change the constraint (d ≈ 0.9). Space-capable states as agenda-setters have analytical exit (can withdraw from treaty) but generational time horizon makes this costly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Cold War sovereignty deadlock) is dead, but the constraint persists — classic mandatrophy. The scaffold was meant to sunset when the international regime materialized; the regime never materialized because the distributional conflict is zero-sum (space powers won't accept binding benefit-sharing; developing states won't accept regime without it). The constraint now functions as a piton-in-disguise: performative regime negotiations maintain the transitional frame while de facto appropriation proceeds. The international_regime reading itself is the mandatrophic artifact — it presents the constraint as still-transitional when the transition has failed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_viability,
    'Is a binding international regime for space resource benefit-sharing structurally possible given current zero-sum distributional conflict?',
    'Track UN COPUOS Working Group on Space Resources progress; monitor whether space-capable states make concessions on benefit-sharing or developing states accept voluntary guidelines',
    'If regime is impossible, the scaffold has no sunset — it becomes a permanent regulatory vacuum (piton) or collapses into extraction-permissive norm (customary law). If regime emerges, the scaffold fulfills its transitional function',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regime_viability, empirical, 'Whether the scaffold''s sunset condition can ever be satisfied').

omega_variable(
    customary_law_formation,
    'Is state practice (national licensing laws, Artemis Accords) crystallizing into customary international law that resolves the appropriation question without a treaty regime?',
    'Analyze state practice and opinio juris: count of national space resource laws, their convergence/divergence, ICJ or ITLOS advisory proceedings, UNGA resolutions',
    'If customary law favors extraction-permissive reading, the international_regime reading is overtaken by events; if customary law remains indeterminate, the scaffold persists',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_formation, empirical, 'Whether customary law bypasses the treaty regime pathway').

omega_variable(
    reading_boundary_ambiguity,
    'Where exactly does the international_regime reading''s boundary lie — does it permit interim national licensing, or does it require complete regulatory abstinence until the regime exists?',
    'Textual analysis of Article II + Article XI + Vienna Convention rules; ICJ advisory opinion on whether national licensing constitutes ''appropriation by use or occupation''',
    'If the reading permits interim licensing, it converges with extraction_permissive in practice; if it requires abstinence, it converges with commons_conservation. The boundary determines whether this reading is a distinct scaffold or a rhetorical cover for one sibling',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Whether the international_regime reading has a coherent operational boundary distinct from its siblings').

omega_variable(
    committer_structure_kernel_reading,
    'How does the kernel-reading structure of this constraint affect its classification — is the ''international_regime'' reading a genuine scaffold, or a rhetorical position that masks the extraction_permissive outcome?',
    'Compare the operational behavior of states/firms invoking this reading: do they actively negotiate a regime, or do they use the reading to legitimize extraction while blocking regime progress?',
    'If the reading is instrumental cover for extraction_permissive, the constraint''s effective type shifts toward snare/tangled_rope; if genuine, it remains scaffold with failed sunset',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Whether this reading''s committer structure (kernel ost_article_ii_non_appropriation, siblings extraction_permissive/commons_conservation) masks extractive function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__international_regime, 1967, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(ost__tr_t1984, ost_article_ii_non_appropriation__international_regime, theater_ratio, 1984, 0.15).
narrative_ontology:measurement(ost__tr_t2000, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(ost__tr_t2015, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(ost__tr_t2020, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(ost__tr_t2025, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2025, 0.2).
narrative_ontology:measurement(ost__tr_t2030, ost_article_ii_non_appropriation__international_regime, theater_ratio, 2030, 0.2).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1967, 0.05).
narrative_ontology:measurement(ost__be_t1984, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 1984, 0.08).
narrative_ontology:measurement(ost__be_t2000, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2000, 0.1).
narrative_ontology:measurement(ost__be_t2015, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2015, 0.12).
narrative_ontology:measurement(ost__be_t2020, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2020, 0.14).
narrative_ontology:measurement(ost__be_t2025, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2025, 0.15).
narrative_ontology:measurement(ost__be_t2030, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 2030, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 1967, 0.15).
narrative_ontology:measurement(ost__su_t1984, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 1984, 0.2).
narrative_ontology:measurement(ost__su_t2000, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2000, 0.25).
narrative_ontology:measurement(ost__su_t2015, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2015, 0.28).
narrative_ontology:measurement(ost__su_t2020, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2020, 0.3).
narrative_ontology:measurement(ost__su_t2025, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2025, 0.3).
narrative_ontology:measurement(ost__su_t2030, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 2030, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__international_regime, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__international_regime, 0.1).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, moon_agreement_1979).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, artemis_accords_section_10).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, national_space_resource_laws_cluster).

% DUAL FORMULATION NOTE:
% This constraint is one member of the OST Article II constraint family (kernel: ost_article_ii_non_appropriation). The three readings (international_regime, extraction_permissive, commons_conservation) share the same treaty text but instantiate different constraints with different ε values, beneficiary/victim structures, and effective types. This reading's ε (0.15) is lower than extraction_permissive (high, direct rent extraction) but higher than commons_conservation (near-zero, prohibitive). The scaffold structure here is distinct: it coordinates non-appropriation pending regime, whereas extraction_permissive coordinates extraction under national law, and commons_conservation coordinates prohibition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ost_article_ii_non_appropriation__international_regime, institutional, 0.15).
constraint_indexing:directionality_override(ost_article_ii_non_appropriation__international_regime, powerful, 0.2).
constraint_indexing:directionality_override(ost_article_ii_non_appropriation__international_regime, moderate, 0.75).
constraint_indexing:directionality_override(ost_article_ii_non_appropriation__international_regime, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
