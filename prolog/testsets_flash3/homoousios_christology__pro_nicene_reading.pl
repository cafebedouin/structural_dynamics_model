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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: homoousios_christology__pro_nicene_reading
 *   human_readable: Christ is Homoousios with the Father (Pro-Nicene Reading)
 *   domain: historical_theology/ecclesiastical_politics/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the Pro-Nicene reading of Christ's
 *   consubstantiality (homoousios) with the Father, as established and
 *   enforced by the Council of Nicaea (325 CE) and subsequent councils,
 *   culminating in Chalcedon (451 CE). It is a foundational theological claim
 *   that became a political instrument for imperial-ecclesiastical unity. The
 *   constraint is a Tangled Rope because it genuinely coordinated a
 *   theological problem (unity of doctrine) but did so through asymmetric
 *   extraction and active suppression of dissenting views, benefiting the
 *   imperial church and its aligned bishops while victimizing Arian and
 *   Semi-Arian clergy and laity. The kernel context identifies this as one
 *   reading of the broader 'homoousios_christology' kernel, with
 *   'arian_reading' and 'semi_arian_reading' as sibling interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, 0.85).
domain_priors:suppression_score(homoousios_christology__pro_nicene_reading, 0.92).
domain_priors:theater_ratio(homoousios_christology__pro_nicene_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__pro_nicene_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__pro_nicene_reading, "Christ is Homoousios with the Father (Pro-Nicene Reading)").
narrative_ontology:topic_domain(homoousios_christology__pro_nicene_reading, "historical_theology/ecclesiastical_politics/commitment_systems").

domain_priors:requires_active_enforcement(homoousios_christology__pro_nicene_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__pro_nicene_reading, 'f319db3f-cd78-4711-9e0d-8b8fa365f206').
narrative_ontology:cs_kernel_codification('f319db3f-cd78-4711-9e0d-8b8fa365f206', formalized).
narrative_ontology:cs_authority_grounding('f319db3f-cd78-4711-9e0d-8b8fa365f206', lineage).
narrative_ontology:cs_interpretation_layer_present('f319db3f-cd78-4711-9e0d-8b8fa365f206').
narrative_ontology:cs_reading_relation('f319db3f-cd78-4711-9e0d-8b8fa365f206', homoousios_christology__arian_reading, forecloses).
narrative_ontology:cs_reading_relation('f319db3f-cd78-4711-9e0d-8b8fa365f206', homoousios_christology__semi_arian_reading, forecloses).
narrative_ontology:cs_axiom('f319db3f-cd78-4711-9e0d-8b8fa365f206', foundational, christ_is_coeternal_with_father).
narrative_ontology:cs_axiom_status(christ_is_coeternal_with_father, holdable).
narrative_ontology:cs_axiom_grounding('f319db3f-cd78-4711-9e0d-8b8fa365f206', christ_is_coeternal_with_father, deontological).
narrative_ontology:cs_axiom('f319db3f-cd78-4711-9e0d-8b8fa365f206', foundational, divine_unity_requires_consubstantiality).
narrative_ontology:cs_axiom_status(divine_unity_requires_consubstantiality, holdable).
narrative_ontology:cs_axiom_grounding('f319db3f-cd78-4711-9e0d-8b8fa365f206', divine_unity_requires_consubstantiality, deontological).
narrative_ontology:cs_reference_frame('f319db3f-cd78-4711-9e0d-8b8fa365f206', nicene_orthodoxy_325ce).
narrative_ontology:cs_drift_state('f319db3f-cd78-4711-9e0d-8b8fa365f206', post_chalcedon_451ce, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f319db3f-cd78-4711-9e0d-8b8fa365f206', '').
narrative_ontology:cs_kernel_id(homoousios_christology__pro_nicene_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, nicene_bishops).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, imperial_authority).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, arian_clergy).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, semi_arian_clergy).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, dissenting_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary proponents and enforcers of the Homoousios doctrine, they gain theological coherence, institutional power, and imperial backing by upholding this creed. They administer anathemas and excommunications.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, nicene_bishops, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefits from a unified Christian doctrine that supports a stable, centralized imperial church, reducing theological disputes that could destabilize the empire. The Homoousios provides a clear, enforceable standard for orthodoxy.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, imperial_authority, beneficiary,
    institutional, generational, mobile, global).

% Adherents to the Arian view that Christ is created and subordinate. They face excommunication, deposition, and sometimes exile for refusing to assent to Homoousios. Their careers and spiritual authority are directly threatened.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, arian_clergy, payer,
    powerful, biographical, trapped, regional).

% Those who held that Christ was of 'similar substance' (Homoiousios), seeking a compromise. They were often pressured to conform to the Nicene creed, facing marginalization or eventual suppression if they resisted, though their position was less starkly opposed than the Arians.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, semi_arian_clergy, payer,
    moderate, biographical, constrained, regional).

% Lay Christians who followed Arian or Semi-Arian teachings, often due to local episcopal leadership or personal conviction. They faced social ostracism, denial of sacraments, and spiritual alienation if they did not conform to the Nicene orthodoxy, making exit from the imperial church a profound identity crisis.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, dissenting_laity, payer,
    powerless, immediate, identity_locked, local).

% Modern historians and theologians who analyze the historical, political, and theological dynamics of the Nicene controversy. They assess the structural forces at play without being subject to the constraint's enforcement.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, theological_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified theological understanding of Christ's divinity, resolving doctrinal disputes that threatened the unity and authority of the early Christian Church and the stability of the Roman Empire.
% TRANSFER_FUNCTION: Transfers theological authority and institutional legitimacy to the Nicene faction and the imperial church, while extracting conformity and suppressing alternative Christologies from dissenting clergy and laity.
% ABSENT_VOICES: Early Christian communities and theologians who held diverse Christological views prior to the Council of Nicaea, whose perspectives were systematically excluded or anathematized as the Nicene creed became enforced orthodoxy. Their voices would highlight the theological pluralism that existed before imperial consolidation.
% DISAPPEARANCE_RATIONALE: If the Homoousios doctrine and its enforcement vanished, the theological landscape of Christianity would be fundamentally altered. The historical trajectory of Trinitarian theology, the structure of ecclesiastical authority, and the relationship between church and state would have taken a vastly different path, leading to a complete rearrangement of Christian history and doctrine.
% FOUNDING_PROBLEM: Theological disputes over the nature of Christ's divinity, particularly Arianism, were causing widespread schism and instability within the Christian Church, threatening both its spiritual unity and its role as a pillar of the Roman Empire.
% FOUNDING_PROBLEM_CORROBORATION: While the Nicene bishops and their successors claimed the problem was live, modern historical scholarship (from outside the benefiting parties) largely agrees that the Arian controversy, as a live threat to imperial stability, was resolved by the 4th century. The doctrine's continued enforcement served to consolidate power and maintain theological uniformity rather than address an active schism.
narrative_ontology:disappearance_verdict(homoousios_christology__pro_nicene_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__pro_nicene_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__pro_nicene_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(homoousios_christology__pro_nicene_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__pro_nicene_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because adherence to Homoousios was enforced with severe penalties (anathema, deposition, exile), extracting conformity and loyalty. Suppression is very high (0.92) due to the active, often violent, suppression of alternative Christologies by both ecclesiastical and imperial authorities. Theater ratio is low (0.1) because the enforcement was genuinely aimed at theological and political control, not mere performance; the stakes were existential for both proponents and opponents. Accessibility collapse is high (0.75) as the imperial church systematically eliminated viable alternatives for theological expression and practice. Resistance is also high (0.8) reflecting the prolonged and intense theological and political struggles, including periods of Arian resurgence, before Nicene orthodoxy was definitively established.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Nicene bishops, the Homoousios was a necessary theological truth for salvation and church unity, a genuine coordination. From the perspective of Arian and Semi-Arian clergy, it was an imposed dogma, a snare designed to consolidate power and suppress legitimate theological inquiry. The engine's classification as Tangled Rope captures this hybrid nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Nicene bishops and imperial authority are clear beneficiaries, gaining institutional power and political stability. Arian and Semi-Arian clergy are direct targets, facing severe consequences for non-compliance. Dissenting laity are also targets, experiencing identity-locked exit options due to the profound social and spiritual costs of leaving the established church. The constraint subsidizes the power of the Nicene faction and the unity of the empire by extracting conformity from all others.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to resolve the Arian controversy and unify the church. While the immediate theological dispute eventually subsided, the enforcement mechanism persisted, transforming into a tool for maintaining hierarchical control and suppressing any theological deviation. The classification as Tangled Rope prevents mislabeling this as pure coordination (Rope) by highlighting the active extraction and suppression, or as pure extraction (Snare) by acknowledging the initial coordination function. The 'dead' status of the founding problem, coupled with 'world_rearranges' on disappearance, signals a constraint that outlived its original purpose but remained structurally vital for the beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_truth_vs_political_expediency,
    'To what extent was the adoption and enforcement of Homoousios driven by genuine theological conviction, versus political expediency for imperial unity?',
    'Detailed historical analysis of primary sources, including imperial decrees, conciliar acts, and theological treatises, weighing the arguments for theological necessity against evidence of political pressure and strategic alignment.',
    'If primarily theological, the coordination function is stronger, potentially shifting the classification closer to a Rope. If primarily political, the extractive function is amplified, reinforcing the Snare-like aspects of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_truth_vs_political_expediency, conceptual, 'Ambiguity in the primary driver of the Homoousios doctrine.').

omega_variable(
    identity_lock_strength_for_laity,
    'How deeply was the identity of dissenting laity fused with their local Arian/Semi-Arian traditions, making exit from the imperial church a true identity-locked condition?',
    'Sociological and anthropological studies of early Christian community formation and identity, examining narratives of conversion, apostasy, and communal belonging in regions with strong Arian presence.',
    'Stronger identity lock for laity increases their effective extraction and suppression, reinforcing the Snare-like qualities of the constraint from their seat. Weaker identity lock suggests more constrained but not identity-fused exit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_strength_for_laity, empirical, 'Degree of identity fusion for dissenting lay Christians.').

omega_variable(
    mandatrophy_timing_ambiguity,
    'When precisely did the ''founding problem'' of Arian schism transition from a live threat to a ''dead'' problem, and the constraint''s primary function shift from coordination to extraction?',
    'Historical consensus on the effective end of major Arian-Nicene conflicts and the beginning of sustained, unchallenged Nicene dominance, cross-referenced with the decline of active Arian theological production.',
    'An earlier ''dead'' date would strengthen the argument for the constraint''s later operation as primarily extractive. A later date would extend the period of genuine coordination, potentially dampening the overall extractiveness trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_timing_ambiguity, empirical, 'Uncertainty in the exact timing of mandatrophy for the Arian controversy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__pro_nicene_reading, 325, 451).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_christology__pro_nicene_reading, theater_ratio, 325, 0.05).
narrative_ontology:measurement(homo_tr_t350, homoousios_christology__pro_nicene_reading, theater_ratio, 350, 0.08).
narrative_ontology:measurement(homo_tr_t381, homoousios_christology__pro_nicene_reading, theater_ratio, 381, 0.1).
narrative_ontology:measurement(homo_tr_t410, homoousios_christology__pro_nicene_reading, theater_ratio, 410, 0.09).
narrative_ontology:measurement(homo_tr_t451, homoousios_christology__pro_nicene_reading, theater_ratio, 451, 0.1).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_christology__pro_nicene_reading, base_extractiveness, 325, 0.7).
narrative_ontology:measurement(homo_be_t350, homoousios_christology__pro_nicene_reading, base_extractiveness, 350, 0.78).
narrative_ontology:measurement(homo_be_t381, homoousios_christology__pro_nicene_reading, base_extractiveness, 381, 0.85).
narrative_ontology:measurement(homo_be_t410, homoousios_christology__pro_nicene_reading, base_extractiveness, 410, 0.83).
narrative_ontology:measurement(homo_be_t451, homoousios_christology__pro_nicene_reading, base_extractiveness, 451, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_christology__pro_nicene_reading, suppression_requirement, 325, 0.8).
narrative_ontology:measurement(homo_su_t350, homoousios_christology__pro_nicene_reading, suppression_requirement, 350, 0.88).
narrative_ontology:measurement(homo_su_t381, homoousios_christology__pro_nicene_reading, suppression_requirement, 381, 0.92).
narrative_ontology:measurement(homo_su_t410, homoousios_christology__pro_nicene_reading, suppression_requirement, 410, 0.9).
narrative_ontology:measurement(homo_su_t451, homoousios_christology__pro_nicene_reading, suppression_requirement, 451, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__pro_nicene_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, trinitarian_doctrine_development).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, imperial_church_authority).

% DUAL FORMULATION NOTE:
% This constraint is the 'pro_nicene_reading' of the 'homoousios_christology' kernel. It is structurally distinct from the 'arian_reading' and 'semi_arian_reading' due to differing ε values and stakeholder structures, but all three are linked as interpretations of the same core theological problem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
