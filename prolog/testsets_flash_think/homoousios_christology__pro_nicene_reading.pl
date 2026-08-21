% ============================================================================
% CONSTRAINT STORY: homoousios_christology__pro_nicene_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__pro_nicene_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: homoousios_christology__pro_nicene_reading
 *   human_readable: Christ is Homoousios with the Father (Pro-Nicene Reading)
 *   domain: historical_theology/ecclesiastical_politics/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the pro-Nicene reading of the homoousios
 *   (consubstantial) Christology, which asserts that Christ is of identical
 *   divine substance with the Father. This reading became the official
 *   doctrine of the Roman Imperial Church, enforced through ecclesiastical
 *   councils and imperial power. It is one reading of the broader
 *   'homoousios_christology' kernel, which was intensely contested in the 4th
 *   century. The metrics reflect the high extraction and suppression involved
 *   in establishing this orthodoxy, despite its claimed coordination function
 *   for church unity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, 0.78).
domain_priors:suppression_score(homoousios_christology__pro_nicene_reading, 0.88).
domain_priors:theater_ratio(homoousios_christology__pro_nicene_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__pro_nicene_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__pro_nicene_reading, "Christ is Homoousios with the Father (Pro-Nicene Reading)").
narrative_ontology:topic_domain(homoousios_christology__pro_nicene_reading, "historical_theology/ecclesiastical_politics/commitment_systems").

domain_priors:requires_active_enforcement(homoousios_christology__pro_nicene_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__pro_nicene_reading, 'e0a2b0df-91ab-48fb-9a31-b1cb571a0253').
narrative_ontology:cs_kernel_codification('e0a2b0df-91ab-48fb-9a31-b1cb571a0253', formalized).
narrative_ontology:cs_authority_grounding('e0a2b0df-91ab-48fb-9a31-b1cb571a0253', lineage).
narrative_ontology:cs_interpretation_layer_present('e0a2b0df-91ab-48fb-9a31-b1cb571a0253').
narrative_ontology:cs_reading_relation('e0a2b0df-91ab-48fb-9a31-b1cb571a0253', homoousios_christology__arian_reading, forecloses).
narrative_ontology:cs_reading_relation('e0a2b0df-91ab-48fb-9a31-b1cb571a0253', homoousios_christology__semi_arian_reading, forecloses).
narrative_ontology:cs_axiom('e0a2b0df-91ab-48fb-9a31-b1cb571a0253', foundational, christ_is_homoousios_with_father).
narrative_ontology:cs_axiom_status(christ_is_homoousios_with_father, holdable).
narrative_ontology:cs_axiom_grounding('e0a2b0df-91ab-48fb-9a31-b1cb571a0253', christ_is_homoousios_with_father, theological).
narrative_ontology:cs_axiom('e0a2b0df-91ab-48fb-9a31-b1cb571a0253', secondary, divine_unity_of_father_and_son).
narrative_ontology:cs_axiom_status(divine_unity_of_father_and_son, holdable).
narrative_ontology:cs_axiom_grounding('e0a2b0df-91ab-48fb-9a31-b1cb571a0253', divine_unity_of_father_and_son, theological).
narrative_ontology:cs_reference_frame('e0a2b0df-91ab-48fb-9a31-b1cb571a0253', nicene_orthodoxy_established).
narrative_ontology:cs_drift_state('e0a2b0df-91ab-48fb-9a31-b1cb571a0253', post_chalcedon_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e0a2b0df-91ab-48fb-9a31-b1cb571a0253', '').
narrative_ontology:cs_kernel_id(homoousios_christology__pro_nicene_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, imperial_church_hierarchy).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, nicene_theologians).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, roman_emperor).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, arian_theologians).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, semi_arian_theologians).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, dissenting_laity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, orthodox_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The bishops and clergy who championed the Nicene creed, benefiting from the theological unity and imperial backing it provided. They actively enforced the doctrine through councils, anathemas, and administrative power, consolidating their authority.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, imperial_church_hierarchy, agenda_setter,
    institutional, generational, constrained, global).

% The intellectual architects and defenders of the homoousios doctrine, whose careers, influence, and theological legacy were affirmed and advanced by its adoption and enforcement. They gained status and patronage within the imperial church.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, nicene_theologians, beneficiary,
    powerful, biographical, constrained, global).

% The political authority that convened councils, ratified decrees, and provided the coercive force (exile, confiscation) to enforce theological conformity. Benefited from a unified church as a pillar of imperial stability, viewing theological dissent as political instability.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, roman_emperor, agenda_setter,
    institutional, generational, constrained, global).

% The proponents of the view that Christ was created and subordinate to the Father. They faced anathema, exile, and suppression of their writings and communities, bearing the full cost of the Nicene enforcement. Their theological position was foreclosed by the Nicene creed.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, arian_theologians, payer,
    powerless, biographical, trapped, global).

% Those who advocated for Christ being 'of similar substance' (homoiousios) rather than 'identical substance' (homoousios). Despite seeking a compromise, their position was ultimately rejected and suppressed by the ascendant Nicene orthodoxy, leading to similar penalties as the Arians.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, semi_arian_theologians, payer,
    powerless, biographical, trapped, global).

% Ordinary Christians who adhered to non-Nicene Christologies, often due to local traditions or the influence of their bishops. They faced social exclusion, loss of church privileges, and sometimes direct persecution, making exit from the dominant theological framework extremely costly to their identity and community.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, dissenting_laity, payer,
    powerless, immediate, identity_locked, local).

% Christians who accepted the Nicene creed, benefiting from the perceived theological stability, unity, and imperial favor of the established church. Their faith was affirmed, and they avoided the penalties faced by dissenters.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, orthodox_laity, beneficiary,
    moderate, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_christology__pro_nicene_reading, imperial_church_hierarchy).
narrative_ontology:fixing_cost_class(homoousios_christology__pro_nicene_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a unified, orthodox theological understanding of Christ's divinity, thereby ensuring doctrinal coherence and maintaining the unity and stability of the Christian church across the Roman Empire.
% TRANSFER_FUNCTION: Transfers theological authority, political legitimacy, and social control to the Nicene faction and the imperial church, while extracting theological conformity, loyalty, and suppression of dissent from all subjects.
% ABSENT_VOICES: Early Christian communities with diverse Christological views that predated the imperial consolidation, or those geographically distant from imperial control (e.g., Persian Christians). They would have argued for a broader theological pluralism but were systematically excluded from the imperial councils and their views suppressed.
% DISAPPEARANCE_RATIONALE: If the homoousios doctrine and its imperial enforcement had vanished overnight, the theological landscape of early Christianity would have remained far more diverse, potentially leading to a different political and ecclesiastical structure for the Roman Empire, or even its fragmentation along theological lines. The unity of the church and empire was deeply intertwined with this doctrinal settlement.
% FOUNDING_PROBLEM: Widespread and intense theological disagreement regarding the nature of Christ, which threatened the unity and stability of the nascent Christian church and, by extension, the Roman Empire that had adopted Christianity as its official religion.
% FOUNDING_PROBLEM_CORROBORATION: Historical records from church councils (Nicaea, Constantinople), imperial edicts, and theological writings from both Nicene and non-Nicene factions attest to the severity of the Christological controversies and the imperial desire for unity. Independent historians and theologians corroborate the historical reality of the doctrinal disputes and their political implications.
narrative_ontology:disappearance_verdict(homoousios_christology__pro_nicene_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__pro_nicene_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__pro_nicene_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(homoousios_christology__pro_nicene_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__pro_nicene_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__pro_nicene_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_christology__pro_nicene_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high because the enforcement of homoousios involved significant costs for dissenters, including anathema, exile, and loss of property and status. Suppression is even higher, reflecting the active and often violent coercion used by the imperial church and state to eliminate alternative Christologies. The theater ratio is moderate, as genuine theological debate existed, but the public performance of orthodoxy, anathema, and imperial decrees played a crucial role in maintaining control and projecting an image of unified authority. The increasing values over the interval reflect the hardening of enforcement and the accumulation of costs as the Nicene position consolidated its power.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Imperial Church Hierarchy and Nicene Theologians, the homoousios doctrine was a necessary 'rope' for theological truth and church unity. From the perspective of Arian and Semi-Arian Theologians, it was a 'snare' designed to eliminate their legitimate theological positions and consolidate power. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Imperial Church Hierarchy and Nicene Theologians are clear beneficiaries, gaining authority and status from the doctrine's triumph. The Roman Emperor also benefits from the political stability a unified church provides. Arian and Semi-Arian Theologians, along with Dissenting Laity, are the primary targets, bearing the full brunt of suppression and extraction. Orthodox Laity are beneficiaries of perceived stability but also bear indirect costs of enforcement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_truth_vs_political_power,
    'To what extent was the triumph of homoousios a genuine theological consensus, versus a politically enforced outcome driven by imperial desire for unity?',
    'Analysis of theological arguments independent of imperial patronage, and counterfactual historical analysis of how Christology might have evolved without state intervention.',
    'If primarily political, the constraint''s extractiveness and suppression are more purely extractive; if primarily theological, a greater portion of these metrics might be attributed to the genuine costs of establishing truth, shifting the classification closer to a Tangled Rope with a stronger coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_truth_vs_political_power, conceptual, 'Ambiguity of theological vs. political drivers for doctrinal enforcement.').

omega_variable(
    identity_lock_strength_for_laity,
    'How deeply was the identity of the ''dissenting_laity'' fused with their non-Nicene Christologies, making exit from the imperial church unthinkable even under severe pressure?',
    'Sociological and historical studies of local community resilience and the persistence of ''underground'' theological traditions despite official suppression.',
    'If identity-lock was very strong, the effective suppression for these groups was even higher than structural measures suggest, as they carried the constraint internally. If weaker, the structural suppression was the primary barrier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength_for_laity, empirical, 'Structural vs. internalized suppression for dissenting laity.').

omega_variable(
    mandate_obsolescence_of_unity,
    'Did the ''founding problem'' of church unity become obsolete once the empire was Christianized, turning ongoing enforcement into pure extraction?',
    'Historical analysis of the actual threats to imperial stability from theological dissent post-Constantine, versus the perceived threats used to justify continued suppression.',
    'If the problem became obsolete, the constraint shifts closer to a Snare or Piton, as its coordination function atrophied while extraction persisted. If the threat remained live, its Tangled Rope classification is more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_obsolescence_of_unity, empirical, 'Whether the original mandate for unity remained relevant over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__pro_nicene_reading, 325, 385).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_christology__pro_nicene_reading, theater_ratio, 325, 0.3).
narrative_ontology:measurement(homo_tr_t335, homoousios_christology__pro_nicene_reading, theater_ratio, 335, 0.35).
narrative_ontology:measurement(homo_tr_t345, homoousios_christology__pro_nicene_reading, theater_ratio, 345, 0.4).
narrative_ontology:measurement(homo_tr_t355, homoousios_christology__pro_nicene_reading, theater_ratio, 355, 0.42).
narrative_ontology:measurement(homo_tr_t365, homoousios_christology__pro_nicene_reading, theater_ratio, 365, 0.44).
narrative_ontology:measurement(homo_tr_t385, homoousios_christology__pro_nicene_reading, theater_ratio, 385, 0.45).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_christology__pro_nicene_reading, base_extractiveness, 325, 0.6).
narrative_ontology:measurement(homo_be_t335, homoousios_christology__pro_nicene_reading, base_extractiveness, 335, 0.68).
narrative_ontology:measurement(homo_be_t345, homoousios_christology__pro_nicene_reading, base_extractiveness, 345, 0.72).
narrative_ontology:measurement(homo_be_t355, homoousios_christology__pro_nicene_reading, base_extractiveness, 355, 0.75).
narrative_ontology:measurement(homo_be_t365, homoousios_christology__pro_nicene_reading, base_extractiveness, 365, 0.77).
narrative_ontology:measurement(homo_be_t385, homoousios_christology__pro_nicene_reading, base_extractiveness, 385, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_christology__pro_nicene_reading, suppression_requirement, 325, 0.7).
narrative_ontology:measurement(homo_su_t335, homoousios_christology__pro_nicene_reading, suppression_requirement, 335, 0.78).
narrative_ontology:measurement(homo_su_t345, homoousios_christology__pro_nicene_reading, suppression_requirement, 345, 0.82).
narrative_ontology:measurement(homo_su_t355, homoousios_christology__pro_nicene_reading, suppression_requirement, 355, 0.85).
narrative_ontology:measurement(homo_su_t365, homoousios_christology__pro_nicene_reading, suppression_requirement, 365, 0.87).
narrative_ontology:measurement(homo_su_t385, homoousios_christology__pro_nicene_reading, suppression_requirement, 385, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__pro_nicene_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
