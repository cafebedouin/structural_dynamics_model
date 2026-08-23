% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__hybrid_effective_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__hybrid_effective_control_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unclos_maritime_sovereignty__hybrid_effective_control_reading
 *   human_readable: UNCLOS Hybrid Effective Control Reading: Graduated Maritime Sovereignty
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint story captures the hybrid effective control reading of
 *   UNCLOS maritime sovereignty: natural features generate full EEZ and
 *   territorial sea entitlements per Article 121; artificial features on
 *   submerged features or low-tide elevations generate only 500m safety zones
 *   under Article 60/80, but may mature into territorial claims through
 *   prolonged effective control absent challenge. The reading occupies the
 *   middle ground between the strict geographic reading (Art 121 is
 *   exhaustive; artificial features never generate zones) and the expansive
 *   construction reading (effective occupation of any feature generates
 *   zones). The constraint is claimed as tangled_rope — genuine coordination
 *   (clarifying zones, safety regimes) combined with asymmetric extraction
 *   (construction-capable states convert facts on the ground into law). The
 *   metrics reflect intermediate extractiveness rising over three decades as
 *   construction accelerated, suppression hardening as enforcement presence
 *   expanded, and theater increasing as legal arguments become more
 *   performative relative to functional coordination.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.45).
domain_priors:suppression_score(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.65).
domain_priors:theater_ratio(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__hybrid_effective_control_reading, "UNCLOS Hybrid Effective Control Reading: Graduated Maritime Sovereignty").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__hybrid_effective_control_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__hybrid_effective_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__hybrid_effective_control_reading, '6c55357f-99f0-47d1-9a35-148389f47ad4').
narrative_ontology:cs_kernel_codification('6c55357f-99f0-47d1-9a35-148389f47ad4', formalized).
narrative_ontology:cs_authority_grounding('6c55357f-99f0-47d1-9a35-148389f47ad4', lineage).
narrative_ontology:cs_interpretation_layer_present('6c55357f-99f0-47d1-9a35-148389f47ad4').
narrative_ontology:cs_reading_relation('6c55357f-99f0-47d1-9a35-148389f47ad4', unclos_maritime_sovereignty__strict_geographic_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c55357f-99f0-47d1-9a35-148389f47ad4', unclos_maritime_sovereignty__expansive_construction_reading, coexists_with).
narrative_ontology:cs_axiom('6c55357f-99f0-47d1-9a35-148389f47ad4', foundational, graduated_sovereignty_by_feature_origin_and_duration).
narrative_ontology:cs_axiom_status(graduated_sovereignty_by_feature_origin_and_duration, holdable).
narrative_ontology:cs_axiom_grounding('6c55357f-99f0-47d1-9a35-148389f47ad4', graduated_sovereignty_by_feature_origin_and_duration, conventional).
narrative_ontology:cs_axiom('6c55357f-99f0-47d1-9a35-148389f47ad4', foundational, effective_control_matures_artificial_features_absent_challenge).
narrative_ontology:cs_axiom_status(effective_control_matures_artificial_features_absent_challenge, holdable).
narrative_ontology:cs_axiom_grounding('6c55357f-99f0-47d1-9a35-148389f47ad4', effective_control_matures_artificial_features_absent_challenge, conventional).
narrative_ontology:cs_reference_frame('6c55357f-99f0-47d1-9a35-148389f47ad4', unclos_1982_maritime_regime).
narrative_ontology:cs_drift_state('6c55357f-99f0-47d1-9a35-148389f47ad4', post_arbitration_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6c55357f-99f0-47d1-9a35-148389f47ad4', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_power_projectors).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, excluded_claimant_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, major_naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, commercial_shipping_interests).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__hybrid_effective_control_reading, graduated_sovereignty_by_feature_origin).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__hybrid_effective_control_reading, effective_control_matures_artificial_features).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_article_121_interpretation_flexibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with industrial-scale dredging, construction, and logistical capacity to build artificial islands on submerged features (e.g., China in the South China Sea). They set the facts on the ground by constructing features, then invoke prolonged effective control to mature claims. They control the timeline and scale of construction; exit for them means abandoning strategic investments, not legal vulnerability.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_states, beneficiary).

% States with sustained coast guard and naval presence to enforce safety zones and administer artificial features (e.g., China Coast Guard, potentially others). They benefit from the legal ambiguity that lets enforcement presence substitute for title. Their exit is mobile — they can redeploy assets — but they gain strategic depth from the constraint's maturation pathway.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_power_projectors, beneficiary,
    powerful, biographical, mobile, regional).

% States with competing maritime claims but lacking construction capacity and sustained enforcement presence (e.g., Philippines, Vietnam, Malaysia in the South China Sea). They bear the cost of lost exclusive economic zone and territorial sea entitlements when artificial features mature into claims. Their exit options are constrained: legal arbitration (slow, unenforceable against non-participants), diplomatic protest (ignored), or military confrontation (disproportionate risk).
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants, payer,
    moderate, biographical, constrained, regional).

% States with claims to features now occupied or constructed upon by others, excluded from the maturation pathway because they lack effective control. They pay through diminished maritime space and foregone resources. Exit is constrained by the same power asymmetry — they cannot replicate the construction+enforcement formula.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, excluded_claimant_states, payer,
    moderate, biographical, constrained, regional).

% Tribunals (ITLOS, ICJ, Annex VII arbitral tribunals), legal scholars, and treaty bodies that interpret UNCLOS. They see the full structure: the treaty text (strict geographic), the subsequent practice (hybrid), and the claimant assertions (expansive). They neither collect nor pay but their interpretations shape the constraint's legitimacy trajectory.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_legal_community, observer,
    analytical, generational, analytical, global).

% States with global naval reach (primarily the United States) that conduct freedom of navigation operations (FONOPs) challenging excessive maritime claims. They benefit from the hybrid reading's ambiguity — it lets them challenge both expansive construction claims and excessive straight baselines without conceding a strict geographic reading that might limit their own operational flexibility. They collect strategic freedom of action, not maritime territory.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, major_naval_powers, observer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__hybrid_effective_control_reading, major_naval_powers, beneficiary).

% Global shipping firms and flag states that benefit from predictable maritime zones and safety zones around artificial features. They gain navigational clarity from the 500m safety zone regime. Their exit is mobile — they reroute — but they prefer stable, recognized zones over contested ambiguity.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, commercial_shipping_interests, beneficiary,
    organized, biographical, mobile, global).

% Low-lying island states (e.g., Pacific atoll nations) whose maritime entitlements are existentially threatened by sea-level rise and by the precedent that artificial construction can generate maritime zones. They have no construction capacity, no enforcement presence, and no viable exit — their territory and EEZ are physically disappearing while the constraint legitimizes artificial feature claims by powerful states.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, small_island_developing_states, excluded,
    powerless, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a graduated maritime entitlement regime that distinguishes natural features (full EEZ/territorial sea) from artificial features (500m safety zones), while providing a maturation pathway whereby prolonged effective control without challenge can convert artificial features into territorial claims. This coordinates navigation rights, resource allocation, and dispute management by giving all parties a legal framework to assess competing claims.
% TRANSFER_FUNCTION: Transfers potential maritime jurisdiction from militarily weaker claimants to construction-capable states through the maturation pathway: artificial features built by powerful states on disputed features generate 500m safety zones immediately, and if unchallenged over time, mature into full territorial claims — effectively transferring ocean space from claimants who cannot contest effectively to those who can build and hold.
% ABSENT_VOICES: Indigenous coastal communities whose traditional fishing grounds are enclosed by safety zones; small island developing states facing existential maritime loss from both sea-level rise and the artificial-feature precedent; environmental stakeholders (coral reef ecosystems destroyed by construction); and future generations who inherit a legal regime that rewards construction over geography.
% DISAPPEARANCE_RATIONALE: If the hybrid reading vanished, maritime claims would reorganize along one of two poles: (1) strict geographic reading — only natural features generate zones, artificial features generate nothing, rolling back all maturation claims; or (2) expansive construction reading — any effective occupation of any feature generates full zones, accelerating construction races. The hybrid reading structures the current contest; its removal forces a binary choice that rearranges every claimant's legal position.
% FOUNDING_PROBLEM: UNCLOS Article 121 left unresolved whether artificial features on submerged banks or low-tide elevations could ever generate maritime zones, and how to regulate the intersection of geographic entitlement and effective control. The founding problem was preventing unlimited territorialization of artificial features while acknowledging that prolonged, uncontested administration creates expectations and stability interests that pure geography ignores.
% FOUNDING_PROBLEM_CORROBORATION: The strict geographic reading is corroborated by UNCLOS drafting history (original negotiators' intent per the 1982 Convention records) and the 2016 Philippines v. China arbitral award (which held artificial features cannot generate territorial sea). The hybrid reading is corroborated by subsequent state practice (1994–present) where multiple states have asserted safety zones around artificial features and some have claimed maturation. The expansive construction reading is corroborated by claimant state declarations (e.g., China's 2014 position paper, Vietnam's 2012 Law of the Sea). No single reading has consensus corroboration outside its proponent set.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__hybrid_effective_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__hybrid_effective_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).
:- end_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is intermediate: the constraint transfers meaningful maritime space but not total extraction — weaker claimants retain some zones around natural features, and the 500m limit on artificial features caps immediate extraction. Suppression (0.65) is higher because the constraint's operation depends on naval/coast guard enforcement to maintain safety zones and prevent challenge; weaker claimants face coercive exclusion. Theater (0.35) is moderate and rising: legal arguments about 'historic rights,' 'effective administration,' and 'peaceful development' increasingly serve to dress power projection in legal language. Accessibility collapse (0.55) reflects that alternatives exist (arbitration, diplomatic negotiation, code of conduct talks) but are structurally constrained by power asymmetry. Resistance (0.60) is substantial: weaker claimants resist through legal proceedings, diplomatic coalitions, and limited counter-construction, but face diminishing returns.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seat (construction-capable states) experiences this as genuine coordination: they built the features, they administer them, the safety zones serve navigation safety, and maturation reflects stability. The payer seats (weaker claimants) experience the same structure as enforced extraction: the safety zones are the leading edge of territorialization, the maturation pathway rewards whoever builds fastest and holds longest, and the 500m limit is a procedural hurdle, not a substantive cap. The engine computes this divergence from the structural data — the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Construction-capable states and regional power projectors are structural beneficiaries (d near 0.0–0.2): they set the construction timeline, control enforcement, and capture the maturation pathway. Militarily weaker claimants and excluded claimant states are structural payers (d near 0.8–0.9): they bear the jurisdictional transfer, have constrained exit, and cannot replicate the formula. Major naval powers sit near symmetric (d ~0.5): they gain operational freedom from ambiguity but pay in strategic instability. The international legal community is analytical (d = 0.5 by definition). Small island developing states are trapped payers (d ~0.95): they face existential loss with zero exit. The derivation chain from beneficiary/victim declarations + power + exit produces these directionalities without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (regulating artificial features while acknowledging effective control) remains contested. The constraint has not resolved into pure coordination (maturation pathway is actively used for expansion) nor pure extraction (the 500m limit and natural feature protections still operate). Mandatrophy is unresolved: the coordination function (legal clarity) is real but the extraction function (power-based maturation) has grown. The constraint is not a piton — it is actively maintained and expanded, not inertially preserved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maturation_pathway_legal_validity,
    'Is the maturation pathway (prolonged effective control converting artificial features into territorial claims) legally valid under UNCLOS, or does it constitute creeping annexation disguised as prescription?',
    'Authoritative interpretation by a competent tribunal addressing whether Article 121(3) (''rocks which cannot sustain human habitation or economic life of their own shall have no exclusive economic zone or continental shelf'') implicitly bars artificial features from ever generating full zones, regardless of duration of control; or whether subsequent practice under Article 31(3)(b) VCLT can modify the treaty''s geographic taxonomy.',
    'If legally invalid, the hybrid reading''s extraction component is unlawful and the constraint collapses toward strict geographic reading (lower ε, Mountain-like for natural features only). If valid, the hybrid reading stands as a legitimate evolutionary interpretation (intermediate ε, Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maturation_pathway_legal_validity, conceptual, 'Whether the core maturation mechanism is legal evolution or disguised conquest.').

omega_variable(
    safety_zone_coordination_vs_cover,
    'Is the 500m safety zone regime a genuine coordination function (navigation safety, resource protection) or a performative cover for the maturation pathway''s territorial expansion?',
    'Empirical analysis of safety zone enforcement: ratio of genuine safety interventions (collision prevention, environmental protection) to exclusionary enforcement (blocking fishing, survey, navigation by non-claimants). Comparison with safety zones around natural features and offshore installations.',
    'If primarily cover, theater_ratio is understated and the constraint trends toward snare. If genuine coordination, the tangled_rope classification holds with real coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(safety_zone_coordination_vs_cover, empirical, 'Whether the declared coordination function is real or performative.').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Does the distinction between natural and artificial features reflect a natural legal boundary (geography as destiny) or a constructed political choice that benefits construction-capable states?',
    'Comparative analysis of treaty drafting history, state practice, and the physics of feature formation: whether ''natural formation'' is a determinate category or a contested boundary (e.g., reclaimed land, assisted natural accretion, climate-change-driven feature emergence).',
    'If the natural/artificial distinction is constructed, the constraint''s claim to coordinate around a natural boundary is false — it coordinates around a power-structured category. This would support false summit detection if the constraint were claimed as mountain (it is not, but the ambiguity matters for the natural feature sub-constraint).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Whether the feature-type taxonomy is natural law or political construction.').

omega_variable(
    prescription_vs_acquiescence_threshold,
    'What duration and quality of ''absent challenge'' constitutes the maturation threshold — and who bears the burden of challenging?',
    'Analysis of international law on prescription/acquiescence: required duration, nature of challenge (diplomatic protest vs. legal proceedings vs. physical opposition), and whether the burden falls on the weaker claimant to continuously contest or on the constructing state to affirmatively prove title.',
    'If the threshold is low and burden on weaker claimants, extraction is higher (easier maturation). If high and burden on constructor, extraction is lower (maturation is exceptional).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prescription_vs_acquiescence_threshold, conceptual, 'The operational threshold for the maturation pathway and its distributive consequences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_hybrid_ec_tr_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(unclos_hybrid_ec_tr_t0, observed).
narrative_ontology:measurement(unclos_hybrid_ec_tr_t6, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 6, 0.23).
narrative_ontology:measurement_basis(unclos_hybrid_ec_tr_t6, observed).
narrative_ontology:measurement(unclos_hybrid_ec_tr_t12, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(unclos_hybrid_ec_tr_t12, observed).
narrative_ontology:measurement(unclos_hybrid_ec_tr_t18, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 18, 0.31).
narrative_ontology:measurement_basis(unclos_hybrid_ec_tr_t18, observed).
narrative_ontology:measurement(unclos_hybrid_ec_tr_t24, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement_basis(unclos_hybrid_ec_tr_t24, observed).
narrative_ontology:measurement(unclos_hybrid_ec_tr_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement_basis(unclos_hybrid_ec_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(unclos_hybrid_ec_be_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(unclos_hybrid_ec_be_t0, observed).
narrative_ontology:measurement(unclos_hybrid_ec_be_t6, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement_basis(unclos_hybrid_ec_be_t6, observed).
narrative_ontology:measurement(unclos_hybrid_ec_be_t12, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement_basis(unclos_hybrid_ec_be_t12, observed).
narrative_ontology:measurement(unclos_hybrid_ec_be_t18, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 18, 0.42).
narrative_ontology:measurement_basis(unclos_hybrid_ec_be_t18, observed).
narrative_ontology:measurement(unclos_hybrid_ec_be_t24, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement_basis(unclos_hybrid_ec_be_t24, observed).
narrative_ontology:measurement(unclos_hybrid_ec_be_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement_basis(unclos_hybrid_ec_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(unclos_hybrid_ec_su_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(unclos_hybrid_ec_su_t0, observed).
narrative_ontology:measurement(unclos_hybrid_ec_su_t6, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement_basis(unclos_hybrid_ec_su_t6, observed).
narrative_ontology:measurement(unclos_hybrid_ec_su_t12, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement_basis(unclos_hybrid_ec_su_t12, observed).
narrative_ontology:measurement(unclos_hybrid_ec_su_t18, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 18, 0.62).
narrative_ontology:measurement_basis(unclos_hybrid_ec_su_t18, observed).
narrative_ontology:measurement(unclos_hybrid_ec_su_t24, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 24, 0.64).
narrative_ontology:measurement_basis(unclos_hybrid_ec_su_t24, observed).
narrative_ontology:measurement(unclos_hybrid_ec_su_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement_basis(unclos_hybrid_ec_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__hybrid_effective_control_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.15).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, south_china_sea_maritime_disputes).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, freedom_of_navigation_operations_regime).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_article_121_interpretation).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, maritime_boundary_delimitation_practice).

% DUAL FORMULATION NOTE:
% This constraint (hybrid_effective_control_reading) forms a constraint family with strict_geographic_reading and expansive_construction_reading, all readings of the unclos_maritime_sovereignty kernel. The strict reading has near-zero ε (mountain-like); this reading has intermediate ε (tangled_rope); the expansive reading has high ε (snare-like). They are linked because the same treaty text and state practice generate all three interpretations, and claimant states invoke them strategically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
