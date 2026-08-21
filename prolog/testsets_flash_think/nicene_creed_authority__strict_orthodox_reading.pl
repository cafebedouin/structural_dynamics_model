% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__strict_orthodox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__strict_orthodox_reading, []).

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
 *   constraint_id: nicene_creed_authority__strict_orthodox_reading
 *   human_readable: Nicene Creed Authority (Strict Orthodox Reading)
 *   domain: systematic_theology/ecclesiology/history_of_christian_doctrine
 *
 * SUMMARY:
 *   This constraint story instantiates the 'strict orthodox reading' of the
 *   Nicene Creed's authority. In this reading, the Creed functions as a
 *   binding metaphysical ontology for all believers, with any deviation
 *   constituting heresy that warrants ecclesiastical sanction. It is
 *   presented as a Tangled Rope, acknowledging its genuine coordination
 *   function (doctrinal unity) but highlighting the asymmetric extraction
 *   (control, suppression of heterodoxy) and active enforcement required for
 *   its persistence. The metrics reflect a high degree of extraction and
 *   suppression, which have generally intensified over centuries of
 *   ecclesiastical history.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, 0.75).
domain_priors:suppression_score(nicene_creed_authority__strict_orthodox_reading, 0.8).
domain_priors:theater_ratio(nicene_creed_authority__strict_orthodox_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__strict_orthodox_reading, tangled_rope).
narrative_ontology:human_readable(nicene_creed_authority__strict_orthodox_reading, "Nicene Creed Authority (Strict Orthodox Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__strict_orthodox_reading, "systematic_theology/ecclesiology/history_of_christian_doctrine").

domain_priors:requires_active_enforcement(nicene_creed_authority__strict_orthodox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__strict_orthodox_reading, '322aa4ad-21f3-445f-9ba3-bcf820de844c').
narrative_ontology:cs_kernel_codification('322aa4ad-21f3-445f-9ba3-bcf820de844c', fixed_text).
narrative_ontology:cs_authority_grounding('322aa4ad-21f3-445f-9ba3-bcf820de844c', lineage).
narrative_ontology:cs_interpretation_layer_present('322aa4ad-21f3-445f-9ba3-bcf820de844c').
narrative_ontology:cs_reading_relation('322aa4ad-21f3-445f-9ba3-bcf820de844c', nicene_creed_authority__symbolic_confessional_reading, coexists_with).
narrative_ontology:cs_reading_relation('322aa4ad-21f3-445f-9ba3-bcf820de844c', nicene_creed_authority__liturgical_habituation_reading, coexists_with).
narrative_ontology:cs_axiom('322aa4ad-21f3-445f-9ba3-bcf820de844c', foundational, creedal_metaphysical_truth).
narrative_ontology:cs_axiom_status(creedal_metaphysical_truth, holdable).
narrative_ontology:cs_axiom_grounding('322aa4ad-21f3-445f-9ba3-bcf820de844c', creedal_metaphysical_truth, theological).
narrative_ontology:cs_axiom('322aa4ad-21f3-445f-9ba3-bcf820de844c', foundational, ecclesiastical_interpretive_supremacy).
narrative_ontology:cs_axiom_status(ecclesiastical_interpretive_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('322aa4ad-21f3-445f-9ba3-bcf820de844c', ecclesiastical_interpretive_supremacy, conventional).
narrative_ontology:cs_reference_frame('322aa4ad-21f3-445f-9ba3-bcf820de844c', early_church_doctrinal_unity).
narrative_ontology:cs_drift_state('322aa4ad-21f3-445f-9ba3-bcf820de844c', contemporary_pluralistic_theology, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('322aa4ad-21f3-445f-9ba3-bcf820de844c', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, heterodox_communities).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, lay_interpreters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, orthodox_believers).
narrative_ontology:constraint_vindicates(nicene_creed_authority__strict_orthodox_reading, doctrinal_uniformity).
narrative_ontology:constraint_vindicates(nicene_creed_authority__strict_orthodox_reading, ecclesiastical_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets, interprets, and enforces creedal orthodoxy across the church. Benefits from the doctrinal control and unity, which reinforces its institutional authority and legitimacy. Deviation is met with ecclesiastical sanctions.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the direct costs of deviation, facing excommunication, marginalization, or persecution. Their theological interpretations are suppressed, and their ability to participate in the broader church life is severely restricted.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, heterodox_communities, payer,
    powerless, biographical, constrained, regional).

% Expected to conform to the official metaphysical ontology. While not always facing direct sanctions, their interpretive freedom is limited, and they risk being labeled heterodox if their personal understanding deviates too far from the prescribed doctrine.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, lay_interpreters, payer,
    moderate, biographical, constrained, local).

% Benefit from the perceived doctrinal stability, unity, and clear theological boundaries provided by the creed. They experience a sense of belonging and shared identity, but are also bound by the creed's strictures and the hierarchy's interpretations.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, orthodox_believers, beneficiary,
    moderate, biographical, constrained, global).

% Analyze the creed's historical development, theological implications, and contemporary relevance. While often operating within the tradition, their analytical perspective allows for critical distance from the enforcement mechanisms.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, theological_scholars, observer,
    analytical, generational, analytical, universal).

% Advocate for a less literal, more symbolic or historically contingent understanding of the creed. Their views are systematically excluded from the authoritative interpretive process and often dismissed as undermining doctrinal truth, despite their continued presence in theological discourse.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, symbolic_confessional_advocates, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy).
narrative_ontology:fixing_cost_class(nicene_creed_authority__strict_orthodox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, universally accepted metaphysical framework for Christian belief, ensuring doctrinal unity and preventing theological fragmentation across diverse communities and historical periods.
% TRANSFER_FUNCTION: Transfers interpretive authority and doctrinal control from individual believers and local communities to a centralized ecclesiastical hierarchy, enforced through sanctions for deviation from the prescribed metaphysical ontology.
% ABSENT_VOICES: Symbolic confessional advocates and liturgical habituation proponents are structurally excluded from the authoritative interpretive process; they would argue for alternative understandings of the creed's function and authority, emphasizing communal discernment or performative identity over strict metaphysical assent.
% DISAPPEARANCE_RATIONALE: If the strict orthodox authority of the Nicene Creed vanished overnight, the global landscape of Christian theology would fragment rapidly. Existing ecclesiastical power structures would face significant challenges, leading to diverse interpretations, new doctrinal formations, and a reorganization of Christian identity around alternative theological or communal principles.
% FOUNDING_PROBLEM: Theological disputes and heresies in the early Church, particularly regarding the nature of Christ and the Trinity, threatened the unity and coherence of Christian doctrine, leading to widespread fragmentation and conflict.
% FOUNDING_PROBLEM_CORROBORATION: Hierarchical clergy and traditionalists assert the problem of theological fragmentation is still live, citing ongoing challenges to traditional doctrines. Historical theologians and some lay movements argue the core problem of early heresies is substantially resolved, and the strict enforcement now serves institutional power more than doctrinal purity; independent historical scholarship and ecumenical dialogues often support this shifted-function reading.
narrative_ontology:disappearance_verdict(nicene_creed_authority__strict_orthodox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__strict_orthodox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__strict_orthodox_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(nicene_creed_authority__strict_orthodox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__strict_orthodox_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__strict_orthodox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_creed_authority__strict_orthodox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) stems from the transfer of interpretive authority from individuals to the hierarchy, and the costs borne by those who deviate. Suppression (0.80) is high due to the active policing of doctrine and the severe consequences for heresy, which effectively limits alternatives. The theater ratio (0.40) is moderate; while genuine theological work and pastoral care occur, a significant portion of institutional activity is dedicated to maintaining doctrinal boundaries and enforcing conformity, sometimes performatively. The historical measurements show a general increase in extractiveness and suppression as the institutional structures around the creed hardened and its enforcement became more systematic.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the hierarchical clergy, the strict orthodox reading is a necessary safeguard for divine truth and church unity, experienced as a Rope. From the perspective of heterodox communities and lay interpreters, the same structure is experienced as a Snare, actively suppressing alternative understandings and extracting conformity through coercion. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The hierarchical clergy are the primary beneficiaries, as the creed's strict enforcement solidifies their institutional power and interpretive monopoly. Heterodox communities and lay interpreters are the primary targets, bearing the costs of conformity or sanction. Orthodox believers receive coordination benefits (unity, clear doctrine) but also face constraints on their interpretive freedom, placing them in a more symmetric, though still constrained, position.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_assent_verifiability,
    'To what extent can genuine metaphysical assent to a complex ontology be reliably verified and enforced by an external authority, as opposed to merely outward conformity?',
    'Empirical studies on belief formation and internal conviction in highly regulated religious contexts, or theological arguments on the nature of faith and coercion.',
    'If genuine assent is largely unverifiable, the measured suppression primarily enforces outward conformity, making the constraint more extractive and theatrical than if it genuinely secured internal belief.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metaphysical_assent_verifiability, conceptual, 'Ambiguity regarding the verifiability of internal metaphysical assent.').

omega_variable(
    creedal_function_ambiguity,
    'Is the primary function of the Nicene Creed, in this reading, to establish a shared metaphysical truth, or to enforce a shared institutional identity and power structure?',
    'Analysis of historical enforcement patterns: if sanctions primarily target challenges to hierarchical authority rather than purely abstract theological points, it suggests a stronger institutional identity function.',
    'If the primary function is institutional identity, the coordination story is weaker, and the constraint leans more towards a Snare; if it''s genuinely shared metaphysical truth, the Tangled Rope classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creedal_function_ambiguity, empirical, 'Ambiguity between metaphysical truth and institutional identity as the creed''s primary function.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of heterodoxy structural (ecclesiastical power, legal sanctions) or internalized (fear of excommunication, social ostracization, self-censorship)?',
    'Post-exit suppression trajectory: if individuals continue to self-censor or fear deviation even after leaving the formal ecclesiastical structure, it indicates a significant internalized component.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as targets carry the suppression with them after formal exit, making the constraint more pervasive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for doctrinal deviation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__strict_orthodox_reading, 325, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 325, 0.1).
narrative_ontology:measurement(nice_tr_t451, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 451, 0.15).
narrative_ontology:measurement(nice_tr_t1054, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1054, 0.25).
narrative_ontology:measurement(nice_tr_t1517, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1517, 0.35).
narrative_ontology:measurement(nice_tr_t1960, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1960, 0.4).
narrative_ontology:measurement(nice_tr_t2024, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 325, 0.58).
narrative_ontology:measurement(nice_be_t451, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 451, 0.62).
narrative_ontology:measurement(nice_be_t1054, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1054, 0.68).
narrative_ontology:measurement(nice_be_t1517, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1517, 0.72).
narrative_ontology:measurement(nice_be_t1960, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1960, 0.74).
narrative_ontology:measurement(nice_be_t2024, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 325, 0.65).
narrative_ontology:measurement(nice_su_t451, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 451, 0.7).
narrative_ontology:measurement(nice_su_t1054, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1054, 0.75).
narrative_ontology:measurement(nice_su_t1517, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1517, 0.8).
narrative_ontology:measurement(nice_su_t1960, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1960, 0.78).
narrative_ontology:measurement(nice_su_t2024, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__strict_orthodox_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
