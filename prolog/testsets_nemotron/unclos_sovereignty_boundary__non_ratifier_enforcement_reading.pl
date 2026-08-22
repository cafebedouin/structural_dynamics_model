% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__non_ratifier_enforcement_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
 *   human_readable: Customary FON Enforcement by Non-Ratifier Naval Powers
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint story captures the 'non-ratifier enforcement reading' of
 *   the UNCLOS sovereignty boundary kernel: the claim that freedom of
 *   navigation (FON) principles exist as customary international law
 *   independent of UNCLOS ratification, and are enforceable by naval presence
 *   (primarily US FONOPs). The constraint structurally decouples from the
 *   treaty text: the enforcer (US) is not a party, the legal basis (customary
 *   law) is asserted unilaterally, and the enforcement mechanism (naval
 *   power) operates outside the treaty's dispute settlement system. Naval
 *   powers gain regulatory exemption and operational freedom; coastal states
 *   asserting EEZ exclusivity bear the cost of constrained sovereignty. The
 *   reading coexists with but structurally pressures the strict EEZ reading
 *   and the historical rights reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.42).
domain_priors:suppression_score(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.68).
domain_priors:theater_ratio(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "Customary FON Enforcement by Non-Ratifier Naval Powers").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '9e9e5f2b-c71b-4cba-8dd0-31e8d1bf8fcb').
narrative_ontology:cs_kernel_codification('9e9e5f2b-c71b-4cba-8dd0-31e8d1bf8fcb', formalized).
narrative_ontology:cs_authority_grounding('9e9e5f2b-c71b-4cba-8dd0-31e8d1bf8fcb', lineage).
narrative_ontology:cs_interpretation_layer_present('9e9e5f2b-c71b-4cba-8dd0-31e8d1bf8fcb').
narrative_ontology:cs_reading_relation('9e9e5f2b-c71b-4cba-8dd0-31e8d1bf8fcb', unclos_sovereignty_boundary__strict_eez_reading, coexists_with).
narrative_ontology:cs_reading_relation('9e9e5f2b-c71b-4cba-8dd0-31e8d1bf8fcb', unclos_sovereignty_boundary__historical_rights_reading, coexists_with).
narrative_ontology:cs_axiom('9e9e5f2b-c71b-4cba-8dd0-31e8d1bf8fcb', foundational, customary_fon_independent_of_unclos_ratification).
narrative_ontology:cs_axiom_status(customary_fon_independent_of_unclos_ratification, holdable).
narrative_ontology:cs_axiom_grounding('9e9e5f2b-c71b-4cba-8dd0-31e8d1bf8fcb', customary_fon_independent_of_unclos_ratification, conventional).
narrative_ontology:cs_axiom('9e9e5f2b-c71b-4cba-8dd0-31e8d1bf8fcb', foundational, naval_presence_legitimate_enforcement_of_customary_law).
narrative_ontology:cs_axiom_status(naval_presence_legitimate_enforcement_of_customary_law, holdable).
narrative_ontology:cs_axiom_grounding('9e9e5f2b-c71b-4cba-8dd0-31e8d1bf8fcb', naval_presence_legitimate_enforcement_of_customary_law, instrumental).
narrative_ontology:cs_reference_frame('9e9e5f2b-c71b-4cba-8dd0-31e8d1bf8fcb', pre_unclos_customary_fon_regime).
narrative_ontology:cs_drift_state('9e9e5f2b-c71b-4cba-8dd0-31e8d1bf8fcb', post_unclos_ratification_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9e9e5f2b-c71b-4cba-8dd0-31e8d1bf8fcb', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, naval_powers_non_ratifiers).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_shipping_industry).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_asserting_eez_exclusivity).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, small_coastal_states_without_navies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major naval powers (primarily the United States) that have not ratified UNCLOS but assert and enforce freedom of navigation (FON) claims through operational challenges (FONOPs). They benefit from open maritime access without accepting treaty obligations on deep seabed mining, environmental standards, or dispute settlement. They set the operational agenda by conducting FONOPs and defining what constitutes 'excessive maritime claims.' Their exit is arbitrage-grade: they can shift patrol patterns, reallocate assets, or adjust claim thresholds at will.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, naval_powers_non_ratifiers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, naval_powers_non_ratifiers, agenda_setter).

% Commercial shipping companies and flag states that rely on predictable, open transit routes for global trade. They benefit from FON enforcement that prevents coastal states from imposing tolls, notification requirements, or restrictive innocent passage regimes in EEZs. Their exit is constrained: they can reroute ships but cannot escape the maritime domain; they depend on naval powers for the security umbrella but have no direct control over FONOP policy.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_shipping_industry, beneficiary,
    organized, biographical, constrained, global).

% Coastal states (e.g., China, Brazil, India, Indonesia) that interpret UNCLOS Article 56-58 as granting exclusive rights over economic activities and security regulation in their EEZs, including the right to restrict foreign military surveys and intelligence collection. They experience FONOPs as violations of sovereign rights and face pressure to either acquiesce (losing exclusive control) or escalate (risking confrontation with superior naval forces). Their exit is constrained: they can file diplomatic protests, invoke dispute settlement (if parties to UNCLOS), or build anti-access capabilities, but cannot unilaterally change the power asymmetry.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_asserting_eez_exclusivity, payer,
    organized, biographical, constrained, regional).

% Developing coastal states lacking naval capacity to enforce their own EEZ claims or resist FONOPs by major powers. They are doubly extracted: they cannot fully exploit their EEZ resources due to capacity gaps, and their attempts to regulate EEZ access (e.g., for environmental protection or resource conservation) are overridden by FON assertions from powerful states. They have no credible exit: they cannot challenge naval powers, cannot opt out of the maritime domain, and depend on the same legal order for their own claims.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, small_coastal_states_without_navies, payer,
    powerless, generational, trapped, local).

% States parties to UNCLOS, international tribunals (ITLOS, ICJ), and the UN Secretariat that monitor compliance, interpret treaty provisions, and adjudicate disputes. They observe the tension between treaty-based EEZ regimes and customary FON claims enforced outside the treaty framework. Their role is analytical and procedural: they do not collect rents from the constraint nor bear its direct costs, but their interpretations shape the legitimacy landscape.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_parties_compliance_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a default rule of open access for international navigation and overflight in maritime zones beyond the territorial sea, preventing a patchwork of coastal state restrictions that would fragment global maritime commerce and naval mobility.
% TRANSFER_FUNCTION: Transfers regulatory authority over EEZ activities (military surveys, intelligence collection, resource regulation) from coastal states to the operational discretion of naval powers conducting FONOPs, backed by the threat of naval confrontation.
% ABSENT_VOICES: Small island developing states (SIDS) and least developed coastal states that lack both naval capacity and legal resources to participate in international dispute settlement. Their preferences for stronger EEZ resource control and environmental regulation are structurally excluded from the FON enforcement calculus.
% DISAPPEARANCE_RATIONALE: If the non-ratifier FON enforcement constraint vanished overnight, coastal states would immediately expand EEZ regulatory regimes — requiring prior notification for military activities, restricting surveys, imposing transit fees — and the global commons character of EEZs would contract. Naval powers would lose their primary legal-operational justification for presence in contested waters. The maritime legal order would reorganize around coastal state exclusivity.
% FOUNDING_PROBLEM: Post-WWII fragmentation of maritime claims (territorial sea breadth, continental shelf rights, high seas freedoms) threatened to enclose the oceans and impede both global trade and naval mobility of the great powers. The 1958 Geneva Conventions and later UNCLOS III sought to codify a balance; the US non-ratification of UNCLOS (1994) froze its FON claims in customary law, creating a dual-track system.
% FOUNDING_PROBLEM_CORROBORATION: The US State Department and DoD attest the founding problem (maritime claim fragmentation) remains live, citing expanding excessive claims. Coastal states and UNCLOS proponents (including ITLOS judges, UN DOALOS) attest the founding problem was substantially solved by UNCLOS's comprehensive regime and the non-ratifier enforcement track now functions as great power privilege, not system maintenance. Independent scholarship (e.g., Klein, 'Maritime Security and the Law of the Sea'; Roach & Smith, 'Excessive Maritime Claims') supports the shifted-function reading from outside the beneficiary set.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).
:- end_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects that the constraint transfers meaningful regulatory authority from coastal states to naval powers, but the coordination function (open global navigation) is real and valuable — it is not pure extraction. Suppression (0.68) is high because persistence depends on active naval enforcement (FONOPs) and the credible threat of escalation; alternatives (coastal state EEZ regimes) are suppressed by power asymmetry, not by participant consent. Theater (0.28) is moderate: FONOPs have genuine operational and signaling functions, but a growing share of activity performs 'rule of law' theater while advancing great power access. Accessibility collapse (0.35) is moderate: coastal states have developed partial countermeasures (anti-access/area denial, legal counter-narratives, regional coalitions) but cannot fully exit the constraint. Resistance (0.55) is significant: diplomatic protests, UNCLOS dispute settlement filings (e.g., Philippines v. China, though distinct), and military modernization programs all push back.
 *
 * PERSPECTIVAL GAP:
 *   From the naval power seat, the constraint is a Rope: it solves a genuine collective action problem (open seas) with minimal coercive overhead relative to the value of global trade and naval mobility. From the coastal state seat, it is a Snare: the coordination story is cover for great power privilege; persistence depends on coercion and suppression of alternatives (EEZ regulatory autonomy). From the small coastal state seat, it is a Snare with no exit. The engine computes this seat divergence from the structural data; the claimed_type (tangled_rope) captures the hybrid reality that the constraint IS both a real coordination mechanism AND an asymmetric extraction vehicle.
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers (non-ratifiers) are structural beneficiaries: they collect operational freedom and regulatory exemption without treaty obligations (d near 0.0). Global shipping is a secondary beneficiary with constrained exit (d ~ 0.2-0.3). Coastal states asserting EEZ exclusivity are targets: they bear regulatory displacement and face enforcement pressure (d ~ 0.7-0.8). Small coastal states without navies are deeply trapped targets (d ~ 0.9). Observers (UNCLOS parties, tribunals) sit at analytical (d = 0.5). The derivation chain from beneficiary/victim declarations + power + exit produces this gradient; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing ocean enclosure) was live in 1958-1982. By 1994, UNCLOS provided a comprehensive treaty framework that largely solved it. The non-ratifier enforcement track persisted because it benefited the most powerful naval actor. Mandatrophy is unresolved: the constraint's original coordination function has been largely absorbed by the treaty regime, but the enforcement machinery continues because it now serves a different function (great power operational freedom). The dual-track system (treaty + customary enforcement by non-party) is the mandatrophy artifact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_formation_mechanism,
    'Does the customary international law of freedom of navigation genuinely exist independent of UNCLOS, or is it a projection of naval power that uses ''customary law'' as a cover story?',
    'Comparative analysis of state practice and opinio juris from non-ratifiers vs. parties; ICJ/ITLOS jurisprudence on customary law formation post-UNCLOS; whether non-ratifier FONOPs are accepted as law-creating by a broad coalition or only tolerated due to power.',
    'If customary law is genuine and widely accepted, the constraint leans toward Rope (coordination with broad legitimacy). If it is a power projection cover, the constraint is a Snare/Tangled Rope with extraction as primary function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_formation_mechanism, conceptual, 'Ontological status of the customary FON claim — law or power?').

omega_variable(
    coordination_extraction_separability,
    'Can the genuine coordination function (open navigation for global commerce) be separated from the extraction function (naval power regulatory exemption), or are they structurally fused?',
    'Counterfactual: if naval powers accepted UNCLOS obligations but retained FONOP rights, would coastal states still resist? If yes, the functions are separable; if no, the extraction is the price of coordination.',
    'If separable, the constraint could be refactored into a pure coordination mechanism (treaty-based) plus a separate extraction claim. If fused, the tangled_rope classification is structurally necessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extraction components can be institutionally decoupled.').

omega_variable(
    committer_structure_kernel_reading,
    'This constraint is one reading (non_ratifier_enforcement_reading) of the contested kernel unclos_sovereignty_boundary. What would the sibling readings change structurally?',
    'Trace the structural delta: strict_eez_reading places coastal states as beneficiaries (exclusive EEZ rights) and naval powers as victims (constrained operations); historical_rights_reading places historical claimants as beneficiaries and UNCLOS parties as victims. The kernel''s constraint family structure is the unit of analysis for cross-reading contamination.',
    'If this reading forecloses or influences siblings, the kernel''s internal dynamics drive classification drift. If all three coexist, the kernel is a stable multi-reading field.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer structure: kernel_id=unclos_sovereignty_boundary, reading_id=non_ratifier_enforcement_reading, siblings=strict_eez_reading, historical_rights_reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tr_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1982, 0.15).
narrative_ontology:measurement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tr_t1994, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1994, 0.18).
narrative_ontology:measurement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tr_t2001, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2001, 0.22).
narrative_ontology:measurement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tr_t2010, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tr_t2016, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2016, 0.27).
narrative_ontology:measurement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tr_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_be_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1982, 0.25).
narrative_ontology:measurement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_be_t1994, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1994, 0.32).
narrative_ontology:measurement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_be_t2001, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2001, 0.35).
narrative_ontology:measurement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_be_t2010, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_be_t2016, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2016, 0.41).
narrative_ontology:measurement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_be_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_su_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1982, 0.45).
narrative_ontology:measurement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_su_t1994, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1994, 0.52).
narrative_ontology:measurement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_su_t2001, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2001, 0.58).
narrative_ontology:measurement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_su_t2010, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_su_t2016, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2016, 0.65).
narrative_ontology:measurement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_su_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.12).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__historical_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is the non_ratifier_enforcement_reading of the unclos_sovereignty_boundary kernel. It structurally influences the strict_eez_reading by providing the enforcement counterweight that makes EEZ exclusivity contestable, and influences the historical_rights_reading by offering an alternative customary-law basis that bypasses treaty text. The three readings form a constraint family linked by the kernel's contested sovereignty boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
