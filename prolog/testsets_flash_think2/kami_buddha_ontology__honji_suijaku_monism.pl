% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__honji_suijaku_monism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__honji_suijaku_monism, []).

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
 *   constraint_id: kami_buddha_ontology__honji_suijaku_monism
 *   human_readable: Honji Suijaku Monism (Kami as Buddhist Traces)
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   The 'Honji Suijaku Monism' constraint describes the dominant medieval
 *   Japanese theological doctrine that asserted kami (indigenous deities)
 *   were phenomenal traces (suijaku) or provisional manifestations of
 *   original Buddhist grounds (honji), such as buddhas and bodhisattvas. This
 *   framework provided a systematic way to integrate Shinto into a Buddhist
 *   cosmology, effectively subordinating Shinto deities and institutions to
 *   Buddhist ones. The constraint was actively enforced by powerful Buddhist
 *   institutions, often with state backing, and persisted for centuries until
 *   the Meiji Restoration.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, 0.8).
domain_priors:suppression_score(kami_buddha_ontology__honji_suijaku_monism, 0.75).
domain_priors:theater_ratio(kami_buddha_ontology__honji_suijaku_monism, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, extractiveness, 0.8).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__honji_suijaku_monism, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__honji_suijaku_monism, "Honji Suijaku Monism (Kami as Buddhist Traces)").
narrative_ontology:topic_domain(kami_buddha_ontology__honji_suijaku_monism, "religious_studies/philosophy_of_religion/japanese_cultural_history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__honji_suijaku_monism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__honji_suijaku_monism, '65ca5284-9964-40d3-b557-8fb8946d3a4a').
narrative_ontology:cs_kernel_codification('65ca5284-9964-40d3-b557-8fb8946d3a4a', formalized).
narrative_ontology:cs_authority_grounding('65ca5284-9964-40d3-b557-8fb8946d3a4a', lineage).
narrative_ontology:cs_interpretation_layer_present('65ca5284-9964-40d3-b557-8fb8946d3a4a').
narrative_ontology:cs_reading_relation('65ca5284-9964-40d3-b557-8fb8946d3a4a', kami_buddha_ontology__domain_partition, forecloses).
narrative_ontology:cs_reading_relation('65ca5284-9964-40d3-b557-8fb8946d3a4a', kami_buddha_ontology__incoherent_bundle, forecloses).
narrative_ontology:cs_axiom('65ca5284-9964-40d3-b557-8fb8946d3a4a', foundational, buddha_nature_as_ultimate_ground).
narrative_ontology:cs_axiom_status(buddha_nature_as_ultimate_ground, holdable).
narrative_ontology:cs_axiom_grounding('65ca5284-9964-40d3-b557-8fb8946d3a4a', buddha_nature_as_ultimate_ground, theological).
narrative_ontology:cs_axiom('65ca5284-9964-40d3-b557-8fb8946d3a4a', foundational, kami_as_provisional_manifestations).
narrative_ontology:cs_axiom_status(kami_as_provisional_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('65ca5284-9964-40d3-b557-8fb8946d3a4a', kami_as_provisional_manifestations, theological).
narrative_ontology:cs_reference_frame('65ca5284-9964-40d3-b557-8fb8946d3a4a', unified_buddhist_cosmology).
narrative_ontology:cs_drift_state('65ca5284-9964-40d3-b557-8fb8946d3a4a', meiji_restoration_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('65ca5284-9964-40d3-b557-8fb8946d3a4a', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, buddhist_institutions).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, imperial_court_aristocracy).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, buddhist_scholars_theologians).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, shinto_institutions).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, shinto_priests_practitioners).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, kokugaku_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, lay_practitioners_syncretic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promoted and systematized the honji suijaku doctrine, integrating kami into their cosmology and thereby consolidating their authority and resources. They benefited from the unified religious framework and the ontological subordination of Shinto.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, buddhist_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, buddhist_institutions, beneficiary).

% Were ontologically subordinated and often institutionally absorbed by Buddhist temples. Their independent theological development was stifled, and their resources were often diverted to Buddhist entities. Exit meant challenging a deeply entrenched religious-political order.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, shinto_institutions, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, shinto_institutions, excluded).

% Developed and refined the honji suijaku theory, gaining intellectual prestige and institutional support for their systematic efforts. Their careers and influence were tied to the dominance of this interpretive framework.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, buddhist_scholars_theologians, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, buddhist_scholars_theologians, agenda_setter).

% Often served in syncretic temple-shrine complexes, with their rituals and deities interpreted through a Buddhist lens. Their traditional roles and independent spiritual authority were diminished, but their livelihoods were tied to the existing system.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, shinto_priests_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Benefited from the theological coherence and political stability offered by a unified religious system. They often patronized Buddhist institutions and endorsed the honji suijaku framework, which helped legitimize their rule.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, imperial_court_aristocracy, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, imperial_court_aristocracy, agenda_setter).

% Found a coherent and accessible religious worldview that integrated their reverence for local kami with the broader Buddhist cosmology, offering a sense of spiritual unity and reducing cognitive dissonance. They could choose to emphasize either aspect but generally accepted the synthesis.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, lay_practitioners_syncretic, beneficiary,
    moderate, biographical, mobile, local).

% Emerged later as a movement advocating for a 'pure' Shinto, rejecting Buddhist influence and the honji suijaku framework. They faced intellectual and institutional resistance from the dominant Buddhist establishment, bearing the cost of challenging the prevailing orthodoxy.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, kokugaku_scholars, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, kokugaku_scholars, excluded).

% Analyze the historical development and impact of honji suijaku, examining its role in Japanese religious and political history without being bound by its theological claims. They provide an external, critical perspective on its function and persistence.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, modern_secular_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a comprehensive theological framework that integrated indigenous kami worship with the imported Buddhist cosmology, offering a unified religious worldview for the Japanese populace and ruling elite.
% TRANSFER_FUNCTION: Transferred ontological priority, interpretive authority, and often institutional resources from Shinto deities and shrines to Buddhist buddhas/bodhisattvas and temples, establishing a hierarchical religious order.
% ABSENT_VOICES: Early Shinto purists or those who maintained a strict dualistic view of kami and buddhas were marginalized or absorbed; later, nascent Kokugaku scholars who sought to restore Shinto's independence were actively resisted by the dominant Buddhist establishment.
% DISAPPEARANCE_RATIONALE: If the honji suijaku doctrine and its institutional enforcement had vanished overnight during its peak, the entire religious-political landscape of Japan would have been fundamentally reorganized. Shinto shrines would have regained autonomy, Buddhist temples would have lost significant influence, and the unified worldview that underpinned much of medieval Japanese society would have fractured, leading to widespread theological and institutional realignment.
% FOUNDING_PROBLEM: The problem of reconciling indigenous Japanese kami worship with the increasingly dominant and sophisticated Buddhist cosmology, which arrived from the continent and offered a comprehensive philosophical and soteriological system.
% FOUNDING_PROBLEM_CORROBORATION: While Buddhist institutions historically attested that the problem of religious coherence was live, modern secular historians and later Shinto revivalist movements (like Kokugaku) attest that the 'problem' was largely a construct to justify Buddhist dominance, and that the original issue of reconciliation was superseded by a framework of subordination. The Meiji government's Shinbutsu-bunri (separation of kami and buddhas) policy in 1868 formally declared the problem 'dead' by dismantling the syncretic structures.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__honji_suijaku_monism, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__honji_suijaku_monism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__honji_suijaku_monism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kami_buddha_ontology__honji_suijaku_monism, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__honji_suijaku_monism, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the doctrine systematically transferred ontological priority, interpretive authority, and often material resources from Shinto to Buddhist entities. Suppression is high due to the active institutional and political pressure to conform to this unified worldview, marginalizing alternative interpretations. Theater ratio is low because the doctrine was a genuinely held and actively propagated theological system, not merely performative. Accessibility collapse is high for those seeking independent Shinto theological development. Resistance was moderate, emerging notably with the Kokugaku movement, but largely suppressed until the Meiji era.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Buddhist institutions and scholars, honji suijaku was a successful coordination mechanism, providing a coherent and sophisticated religious system. From the perspective of Shinto institutions and priests, it was a highly extractive and suppressive framework that diminished their autonomy and identity. The Imperial Court often viewed it as a beneficial tool for political and social stability.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutions, scholars, and the Imperial Court were primary beneficiaries, gaining authority, resources, and a stable worldview. Shinto institutions and priests were clear victims, experiencing ontological and institutional subordination. Lay practitioners were beneficiaries of the coordinated worldview but also indirectly paid through the loss of independent Shinto traditions. Kokugaku scholars, as a later resistance movement, were payers who bore the cost of challenging the dominant narrative.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_institutional_extraction,
    'To what extent was the extraction primarily ontological (a philosophical claim about reality) versus institutional (a power dynamic between religious organizations)?',
    'Detailed historical analysis of resource flows, political patronage, and the actual autonomy of Shinto shrines before and after the doctrine''s dominance, compared with the philosophical arguments for subordination.',
    'If primarily institutional, the constraint''s extractiveness is more directly attributable to power dynamics; if primarily ontological, it highlights the extractive potential of theological frameworks themselves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_vs_institutional_extraction, empirical, 'Distinguishing the nature of extraction in a theological context.').

omega_variable(
    degree_of_internalized_subordination,
    'How deeply was the honji suijaku framework internalized by Shinto priests and practitioners, leading to self-suppression, versus external institutional coercion?',
    'Analysis of personal diaries, local shrine records, and folk religious practices for evidence of resistance or alternative interpretations, even under official pressure.',
    'If internalized, the effective suppression was higher and more pervasive than external measures suggest; if primarily external, removing institutional coercion would have led to faster re-emergence of independent Shinto thought.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(degree_of_internalized_subordination, empirical, 'Assessing the balance between structural and internalized suppression.').

omega_variable(
    theological_coherence_vs_historical_contingency,
    'Was the dominance of honji suijaku primarily due to its inherent theological coherence and explanatory power, or to the historical-political ascendancy of Buddhism in Japan?',
    'Comparative theological analysis with other syncretic traditions globally, alongside counterfactual historical modeling of Japanese religious development without strong state patronage of Buddhism.',
    'If coherence-driven, its ''rope'' function is stronger; if contingency-driven, its ''snare'' or ''tangled_rope'' aspects (extraction via power) are more pronounced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_coherence_vs_historical_contingency, conceptual, 'Understanding the drivers of the doctrine''s historical dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__honji_suijaku_monism, 1000, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t1000, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1000, 0.05).
narrative_ontology:measurement(kami_tr_t1150, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1150, 0.08).
narrative_ontology:measurement(kami_tr_t1300, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1300, 0.1).
narrative_ontology:measurement(kami_tr_t1450, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1450, 0.12).
narrative_ontology:measurement(kami_tr_t1600, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(kami_tr_t1750, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1750, 0.15).
narrative_ontology:measurement(kami_tr_t1868, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1868, 0.2).

% Extraction over time
narrative_ontology:measurement(kami_be_t1000, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1000, 0.6).
narrative_ontology:measurement(kami_be_t1150, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1150, 0.7).
narrative_ontology:measurement(kami_be_t1300, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1300, 0.78).
narrative_ontology:measurement(kami_be_t1450, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1450, 0.82).
narrative_ontology:measurement(kami_be_t1600, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1600, 0.85).
narrative_ontology:measurement(kami_be_t1750, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1750, 0.8).
narrative_ontology:measurement(kami_be_t1868, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1868, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t1000, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1000, 0.5).
narrative_ontology:measurement(kami_su_t1150, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1150, 0.6).
narrative_ontology:measurement(kami_su_t1300, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1300, 0.7).
narrative_ontology:measurement(kami_su_t1450, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1450, 0.78).
narrative_ontology:measurement(kami_su_t1600, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1600, 0.82).
narrative_ontology:measurement(kami_su_t1750, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1750, 0.75).
narrative_ontology:measurement(kami_su_t1868, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1868, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__honji_suijaku_monism, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__domain_partition).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__incoherent_bundle).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'kami_buddha_ontology' kernel. It represents the honji suijaku monistic interpretation, which asserts ontological identity and Buddhist priority. It is linked to sibling readings that offer alternative interpretations of the kami-buddha relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
