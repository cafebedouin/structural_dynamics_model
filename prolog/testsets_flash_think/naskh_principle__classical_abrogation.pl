% ============================================================================
% CONSTRAINT STORY: naskh_principle__classical_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__classical_abrogation, []).

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
 *   constraint_id: naskh_principle__classical_abrogation
 *   human_readable: Classical Abrogation Principle (Naskh)
 *   domain: Islamic Jurisprudence/Quranic Hermeneutics
 *
 * SUMMARY:
 *   The classical abrogation (Naskh) principle in Islamic jurisprudence
 *   posits that later-revealed Quranic verses supersede earlier ones on the
 *   same legal or theological topic, based on chronological order. This
 *   principle provides a hierarchical method for resolving apparent textual
 *   contradictions, ensuring legal certainty and consistency within Islamic
 *   law. This constraint story instantiates the 'classical_abrogation'
 *   reading of the 'naskh_principle' kernel, acknowledging sibling readings
 *   like 'contextual_harmonization' and 'progressive_restriction'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__classical_abrogation, 0.6).
domain_priors:suppression_score(naskh_principle__classical_abrogation, 0.75).
domain_priors:theater_ratio(naskh_principle__classical_abrogation, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, extractiveness, 0.6).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__classical_abrogation, tangled_rope).
narrative_ontology:human_readable(naskh_principle__classical_abrogation, "Classical Abrogation Principle (Naskh)").
narrative_ontology:topic_domain(naskh_principle__classical_abrogation, "Islamic Jurisprudence/Quranic Hermeneutics").

domain_priors:requires_active_enforcement(naskh_principle__classical_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__classical_abrogation, 'b2582b30-c108-4fa7-8375-c487ae75e947').
narrative_ontology:cs_kernel_codification('b2582b30-c108-4fa7-8375-c487ae75e947', fixed_text).
narrative_ontology:cs_authority_grounding('b2582b30-c108-4fa7-8375-c487ae75e947', lineage).
narrative_ontology:cs_interpretation_layer_present('b2582b30-c108-4fa7-8375-c487ae75e947').
narrative_ontology:cs_reading_relation('b2582b30-c108-4fa7-8375-c487ae75e947', naskh_principle__contextual_harmonization, forecloses).
narrative_ontology:cs_reading_relation('b2582b30-c108-4fa7-8375-c487ae75e947', naskh_principle__progressive_restriction, forecloses).
narrative_ontology:cs_axiom('b2582b30-c108-4fa7-8375-c487ae75e947', foundational, later_revelation_supersedes_earlier).
narrative_ontology:cs_axiom_status(later_revelation_supersedes_earlier, holdable).
narrative_ontology:cs_axiom_grounding('b2582b30-c108-4fa7-8375-c487ae75e947', later_revelation_supersedes_earlier, conventional).
narrative_ontology:cs_axiom('b2582b30-c108-4fa7-8375-c487ae75e947', secondary, legal_certainty_priority).
narrative_ontology:cs_axiom_status(legal_certainty_priority, holdable).
narrative_ontology:cs_axiom_grounding('b2582b30-c108-4fa7-8375-c487ae75e947', legal_certainty_priority, instrumental).
narrative_ontology:cs_reference_frame('b2582b30-c108-4fa7-8375-c487ae75e947', early_islamic_legal_tradition).
narrative_ontology:cs_drift_state('b2582b30-c108-4fa7-8375-c487ae75e947', contemporary_islamic_thought, gap(stable, minor, false)).
narrative_ontology:cs_created_at('b2582b30-c108-4fa7-8375-c487ae75e947', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(naskh_principle__classical_abrogation, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, classical_jurists).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, legal_scholars).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, muftis).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, interpretive_flexibility_advocates).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, theological_coherence_advocates).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, lay_muslims_seeking_direct_understanding).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establish and maintain the interpretive hierarchy of Quranic verses, benefiting from the authority and legal certainty this principle provides. Their professional identity is deeply intertwined with this methodology.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, classical_jurists, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from a clear, established framework for deriving legal rulings from the Quran, which simplifies their work and provides a basis for consensus. However, they are constrained by its rules and hierarchy.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, legal_scholars, beneficiary,
    organized, biographical, constrained, global).

% Issue legal opinions (fatwas) with greater certainty and consistency by applying the abrogation principle, enhancing their authority and reducing ambiguity for their followers. Their authority is tied to the principle's acceptance.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, muftis, beneficiary,
    powerful, biographical, constrained, national).

% Bear the cost of reduced interpretive scope and the potential for theological coherence issues when earlier verses are deemed legally invalid. They seek alternative methods that preserve the validity of all verses.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, interpretive_flexibility_advocates, payer,
    organized, generational, constrained, global).

% Experience the principle as a challenge to the holistic coherence of the Quran, as it implies that some divine commands were temporary or superseded. They seek to reconcile all verses without invalidation.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, theological_coherence_advocates, payer,
    moderate, generational, constrained, global).

% Often rely on scholarly interpretations, losing direct access to a fully coherent Quranic text without mediation. They may struggle to reconcile verses that appear contradictory or to understand why some are 'abrogated'.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, lay_muslims_seeking_direct_understanding, payer,
    powerless, biographical, constrained, global).

% Propose alternative interpretive methods that seek to harmonize all Quranic verses by understanding them within their specific revelatory and situational contexts, rather than through chronological supersession. Their views are often marginalized by the dominant classical abrogation framework.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, contextual_harmonization_scholars, excluded,
    organized, generational, constrained, global).

% Argue that Quranic revelation progressively restricted permissions rather than abrogating earlier rulings, seeing a divine pedagogy rather than textual invalidation. Their interpretive framework is suppressed by the classical abrogation principle.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, progressive_restriction_scholars, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, hierarchical method for resolving apparent contradictions in Quranic legal and theological verses, ensuring legal certainty and consistency in Islamic law across diverse contexts and generations of jurists.
% TRANSFER_FUNCTION: Transfers interpretive authority from individual contextual understanding to a fixed chronological hierarchy, from earlier verses to later ones, and from interpretive flexibility to legal certainty, primarily benefiting established legal institutions and scholars.
% ABSENT_VOICES: Scholars advocating for contextual harmonization or progressive restriction are structurally excluded or marginalized; they would argue for alternative interpretive methods that preserve the validity of all verses but are kept out by the dominance of the classical abrogation framework.
% DISAPPEARANCE_RATIONALE: If the principle of abrogation and its enforcement vanished overnight, the entire edifice of classical Islamic jurisprudence would need to be re-evaluated. This would lead to widespread legal uncertainty, theological debate over seemingly contradictory verses, and a significant challenge to the authority of established legal schools.
% FOUNDING_PROBLEM: Apparent contradictions between different Quranic verses, particularly concerning legal rulings revealed at different stages of the early Muslim community's development, which posed challenges for consistent legal application.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of the principle (classical jurists) assert its necessity for legal consistency and resolving textual ambiguities. Critics (contextual harmonization scholars, progressive restriction scholars) acknowledge the historical problem of apparent contradictions but dispute the solution, arguing for alternative methods that do not invalidate verses. Independent historical analysis confirms the existence of the interpretive problem in early Islamic thought.
narrative_ontology:disappearance_verdict(naskh_principle__classical_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__classical_abrogation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__classical_abrogation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(naskh_principle__classical_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__classical_abrogation, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__classical_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__classical_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__classical_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.6) because while the principle provides a valuable coordination function for legal interpretation, it does so by imposing a fixed hierarchy that extracts interpretive flexibility and can create theological tensions. Suppression is high (0.75) as the principle actively suppresses alternative interpretive methodologies that would challenge its chronological supersession framework. Theater ratio is low (0.1) because the principle is a functional legal tool, not primarily performative. Accessibility collapse is moderate (0.6) as it collapses some interpretive alternatives but not all, and resistance is moderate (0.5) due to ongoing scholarly debates and alternative readings.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of classical jurists, the principle is a necessary and beneficial coordination mechanism for maintaining legal consistency and divine authority. From the perspective of those advocating for interpretive flexibility or theological coherence, the same principle operates as an extractive force that limits understanding and creates unnecessary textual invalidation. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical jurists, legal scholars, and muftis are beneficiaries and agenda-setters, as they gain authority and a clear framework for legal derivation. Advocates for interpretive flexibility, theological coherence, and lay Muslims seeking direct understanding are targets, as they bear the cost of reduced interpretive scope and mediated understanding. Scholars of alternative readings are excluded, as their methodologies are suppressed by the dominant framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naskh_principle_reading_ambiguity,
    'Is the ''classical_abrogation'' reading the only valid interpretation of apparent Quranic contradictions, or are ''contextual_harmonization'' or ''progressive_restriction'' equally valid or superior interpretive frameworks?',
    'Comparative theological and jurisprudential analysis, including historical reception and contemporary ethical implications of each reading, alongside empirical study of textual coherence under each framework.',
    'If a sibling reading were adopted, the constraint''s extractiveness and suppression would likely decrease, as interpretive flexibility would increase and the legal force of ''abrogated'' verses might be restored, leading to a reclassification away from Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naskh_principle_reading_ambiguity, conceptual, 'Ambiguity regarding the most appropriate hermeneutical principle for Quranic contradictions.').

omega_variable(
    chronological_revelation_certainty,
    'Is the chronological order of Quranic revelation always clear and universally agreed upon by scholars, or are there significant ambiguities that undermine the application of the abrogation principle?',
    'Exhaustive historical and textual analysis of all Quranic verses and their associated revelatory contexts, seeking consensus on chronological ordering among diverse scholarly traditions.',
    'If chronological order is found to be consistently ambiguous, the foundation of the classical abrogation principle would be weakened, potentially reducing its perceived legitimacy and thus its effective suppression and extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chronological_revelation_certainty, empirical, 'Uncertainty regarding the precise chronological order of Quranic verses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__classical_abrogation, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__classical_abrogation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nask_tr_t10, naskh_principle__classical_abrogation, theater_ratio, 10, 0.1).
narrative_ontology:measurement(nask_tr_t20, naskh_principle__classical_abrogation, theater_ratio, 20, 0.1).
narrative_ontology:measurement(nask_tr_t30, naskh_principle__classical_abrogation, theater_ratio, 30, 0.1).
narrative_ontology:measurement(nask_tr_t40, naskh_principle__classical_abrogation, theater_ratio, 40, 0.1).
narrative_ontology:measurement(nask_tr_t50, naskh_principle__classical_abrogation, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__classical_abrogation, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(nask_be_t10, naskh_principle__classical_abrogation, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(nask_be_t20, naskh_principle__classical_abrogation, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(nask_be_t30, naskh_principle__classical_abrogation, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(nask_be_t40, naskh_principle__classical_abrogation, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(nask_be_t50, naskh_principle__classical_abrogation, base_extractiveness, 50, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__classical_abrogation, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(nask_su_t10, naskh_principle__classical_abrogation, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(nask_su_t20, naskh_principle__classical_abrogation, suppression_requirement, 20, 0.73).
narrative_ontology:measurement(nask_su_t30, naskh_principle__classical_abrogation, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(nask_su_t40, naskh_principle__classical_abrogation, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(nask_su_t50, naskh_principle__classical_abrogation, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__classical_abrogation, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
