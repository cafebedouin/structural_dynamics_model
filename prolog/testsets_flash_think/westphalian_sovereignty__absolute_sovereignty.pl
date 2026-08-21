% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__absolute_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__absolute_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__absolute_sovereignty
 *   human_readable: Absolute State Sovereignty (Westphalian Reading)
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This constraint represents the 'absolute sovereignty' reading of the
 *   Westphalian principle, asserting that states possess unconditional
 *   authority over their domestic affairs and external interference is
 *   categorically illegitimate. While framed as a cornerstone of
 *   international order, this reading is increasingly contested by human
 *   rights norms and doctrines like the Responsibility to Protect (R2P). The
 *   constraint is claimed as a 'tangled_rope' because it provides a
 *   coordination function (preventing interstate conflict) but also enables
 *   significant extraction (shielding repressive regimes from
 *   accountability).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, 0.55).
domain_priors:suppression_score(westphalian_sovereignty__absolute_sovereignty, 0.75).
domain_priors:theater_ratio(westphalian_sovereignty__absolute_sovereignty, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, extractiveness, 0.55).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__absolute_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__absolute_sovereignty, "Absolute State Sovereignty (Westphalian Reading)").
narrative_ontology:topic_domain(westphalian_sovereignty__absolute_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__absolute_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__absolute_sovereignty, '40367f49-7caa-4672-8f0d-df7e5ca1c062').
narrative_ontology:cs_kernel_codification('40367f49-7caa-4672-8f0d-df7e5ca1c062', formalized).
narrative_ontology:cs_authority_grounding('40367f49-7caa-4672-8f0d-df7e5ca1c062', lineage).
narrative_ontology:cs_interpretation_layer_present('40367f49-7caa-4672-8f0d-df7e5ca1c062').
narrative_ontology:cs_reading_relation('40367f49-7caa-4672-8f0d-df7e5ca1c062', westphalian_sovereignty__conditional_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('40367f49-7caa-4672-8f0d-df7e5ca1c062', westphalian_sovereignty__graduated_sovereignty, coexists_with).
narrative_ontology:cs_axiom('40367f49-7caa-4672-8f0d-df7e5ca1c062', foundational, state_non_interference_absolute).
narrative_ontology:cs_axiom_status(state_non_interference_absolute, holdable).
narrative_ontology:cs_axiom_grounding('40367f49-7caa-4672-8f0d-df7e5ca1c062', state_non_interference_absolute, deontological).
narrative_ontology:cs_axiom('40367f49-7caa-4672-8f0d-df7e5ca1c062', secondary, domestic_jurisdiction_exclusive).
narrative_ontology:cs_axiom_status(domestic_jurisdiction_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('40367f49-7caa-4672-8f0d-df7e5ca1c062', domestic_jurisdiction_exclusive, conventional).
narrative_ontology:cs_reference_frame('40367f49-7caa-4672-8f0d-df7e5ca1c062', post_westphalian_order).
narrative_ontology:cs_drift_state('40367f49-7caa-4672-8f0d-df7e5ca1c062', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('40367f49-7caa-4672-8f0d-df7e5ca1c062', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, sovereign_states).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, human_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, liberal_democracies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Primary proponents and enforcers of the principle, benefiting from the non-interference shield that allows them to manage internal affairs without external scrutiny or intervention. They actively defend this interpretation in international forums.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, sovereign_states, agenda_setter,
    institutional, generational, constrained, global).

% Rely heavily on this principle to legitimize their rule and suppress dissent, shielding them from international accountability for human rights abuses. Their political identity and survival are often tied to the absolute right to self-determination and non-interference.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes, beneficiary,
    powerful, biographical, identity_locked, national).

% Bear the direct costs of state repression, with no legitimate avenue for external intervention due to the absolute sovereignty principle. Their suffering is often invisible or deemed 'internal affairs'.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression, payer,
    powerless, immediate, trapped, national).

% Expend significant effort to challenge the principle, as it directly impedes their ability to protect vulnerable populations and hold states accountable. They pay in terms of limited efficacy and moral burden.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, human_rights_advocates, payer,
    organized, biographical, constrained, global).

% Are bound by the principle in their mandates (e.g., UN Charter Article 2(7)), often limiting their ability to act in cases of internal repression. This creates internal tension and ongoing debate within these bodies.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, international_organizations, observer,
    institutional, generational, analytical, global).

% Often uphold the principle for general international stability but face moral and political pressure to intervene in cases of severe human rights abuses, creating a dilemma. They pay a cost in terms of moral inconsistency or political capital when they do intervene or refrain from doing so.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, liberal_democracies, payer,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, liberal_democracies, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To maintain international order by establishing clear boundaries of state authority and preventing interstate conflict that might arise from interference in domestic affairs.
% TRANSFER_FUNCTION: Transfers the unconditional right to self-determination and non-interference to the state, while placing the burden of internal governance (including potential repression) solely on its population, without external recourse.
% ABSENT_VOICES: Domestic populations under repressive regimes are structurally excluded from the international conversation about sovereignty; if present, they would advocate for conditional sovereignty or legitimate external intervention.
% DISAPPEARANCE_RATIONALE: If the principle of absolute state sovereignty vanished overnight, the international system would face constant claims for intervention based on internal affairs, potentially leading to widespread conflict, a redefinition of statehood, and a chaotic period of global reorganization.
% FOUNDING_PROBLEM: To prevent endless wars of religion and dynastic succession by establishing clear, mutually recognized boundaries of state authority and non-interference in the internal affairs of other states.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international relations scholars corroborate the original intent of the Westphalian system to establish stability. Critics (human rights groups, some liberal states) contest its current relevance, arguing that the founding problem is substantially altered by modern human rights norms; legislative hearings and independent legal analyses from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__absolute_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__absolute_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__absolute_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(westphalian_sovereignty__absolute_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__absolute_sovereignty, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__absolute_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__absolute_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.55) because this reading allows states to inflict severe costs on their populations without external check, effectively extracting human rights and self-determination. Suppression is also high (0.75) as the principle actively delegitimizes and suppresses any attempts at external intervention or internal dissent that seeks external support. The theater ratio is moderate (0.3) as the principle is often invoked performatively to deflect criticism, even when its original coordinating function is less relevant than its extractive shield. The increasing trend in extractiveness and suppression reflects the growing tension between absolute sovereignty and evolving human rights norms over the past decades.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of authoritarian regimes, absolute sovereignty is a legitimate defense of national self-determination and a bulwark against neo-colonialism. From the perspective of repressed populations and human rights advocates, it is a legalistic cover for atrocities and a barrier to justice. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing it as a 'rope' or 'scaffold' for stability, and victims experiencing it as a 'snare'.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states and authoritarian regimes are clear beneficiaries, as the principle grants them a non-interference shield (low d). Domestic populations under repression and human rights advocates are targets, bearing the costs of unchecked state power and limited avenues for redress (high d). International organizations and liberal democracies occupy more complex positions, often caught between upholding the principle for stability and responding to humanitarian crises.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_purpose_ambiguity,
    'Is the primary purpose of absolute sovereignty to ensure international stability by preventing interstate conflict, or to shield states (especially authoritarian ones) from accountability for domestic actions?',
    'Empirical analysis of state behavior: if states consistently invoke the principle only when facing human rights scrutiny, it suggests a shielding function. If invoked broadly to prevent all forms of interference, it supports the stability function.',
    'If primarily a shield, the constraint''s extractiveness is higher and its coordination function is more theatrical. If primarily for stability, the extractiveness is a necessary cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_purpose_ambiguity, empirical, 'Ambiguity in the core function of absolute sovereignty.').

omega_variable(
    intervention_legitimacy_ambiguity,
    'When, if ever, does the severity of domestic human rights violations override the principle of non-interference, making external intervention legitimate?',
    'Development of international customary law and state practice, or a UN Security Council resolution establishing clear criteria for humanitarian intervention without state consent.',
    'If a clear override mechanism is established, the absolute sovereignty reading would be foreclosed or substantially weakened, leading to a reclassification towards conditional or graduated sovereignty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intervention_legitimacy_ambiguity, conceptual, 'The threshold for legitimate external intervention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__absolute_sovereignty, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1970, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(west_tr_t1980, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(west_tr_t1990, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(west_tr_t2000, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2000, 0.27).
narrative_ontology:measurement(west_tr_t2010, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2010, 0.29).
narrative_ontology:measurement(west_tr_t2020, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2020, 0.3).

% Extraction over time
narrative_ontology:measurement(west_be_t1970, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(west_be_t1980, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement(west_be_t1990, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1990, 0.51).
narrative_ontology:measurement(west_be_t2000, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2000, 0.53).
narrative_ontology:measurement(west_be_t2010, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2010, 0.54).
narrative_ontology:measurement(west_be_t2020, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2020, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1970, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(west_su_t1980, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(west_su_t1990, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(west_su_t2000, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(west_su_t2010, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(west_su_t2020, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__absolute_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__graduated_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'westphalian_sovereignty' kernel. Its ε value and structural properties differ significantly from 'conditional_sovereignty' and 'graduated_sovereignty', necessitating separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
