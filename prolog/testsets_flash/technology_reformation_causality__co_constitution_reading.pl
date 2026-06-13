% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__co_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__co_constitution_reading, []).

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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: technology_reformation_causality__co_constitution_reading
 *   human_readable: Co-Constitutive Causality of Printing Press and Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint describes the co-constitutive relationship between the
 *   printing press and the Protestant Reformation. It argues that technology
 *   (the printing press) enabled, but did not solely determine, the
 *   Reformation, and that social actors (reformers) actively shaped the
 *   content and impact of the press. This reading emphasizes a bidirectional
 *   causality, where both technology and society evolved in response to each
 *   other, rather than one unilaterally causing the other. The constraint
 *   itself is the dynamic interplay, not the press as a static object.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, 0.25).
domain_priors:suppression_score(technology_reformation_causality__co_constitution_reading, 0.15).
domain_priors:theater_ratio(technology_reformation_causality__co_constitution_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__co_constitution_reading, rope).
narrative_ontology:human_readable(technology_reformation_causality__co_constitution_reading, "Co-Constitutive Causality of Printing Press and Reformation").
narrative_ontology:topic_domain(technology_reformation_causality__co_constitution_reading, "history_of_technology/religious_history/media_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__co_constitution_reading, '92b2f11b-3a38-41e1-9413-fc60d677e992').
narrative_ontology:cs_kernel_codification('92b2f11b-3a38-41e1-9413-fc60d677e992', distributed).
narrative_ontology:cs_authority_grounding('92b2f11b-3a38-41e1-9413-fc60d677e992', diffuse_epistemic).
narrative_ontology:cs_reading_relation('92b2f11b-3a38-41e1-9413-fc60d677e992', technology_reformation_causality__technological_determinism_reading, forecloses).
narrative_ontology:cs_reading_relation('92b2f11b-3a38-41e1-9413-fc60d677e992', technology_reformation_causality__beneficiary_agency_reading, coexists_with).
narrative_ontology:cs_axiom('92b2f11b-3a38-41e1-9413-fc60d677e992', foundational, bidirectional_causality_between_tech_and_society).
narrative_ontology:cs_axiom_status(bidirectional_causality_between_tech_and_society, holdable).
narrative_ontology:cs_axiom_grounding('92b2f11b-3a38-41e1-9413-fc60d677e992', bidirectional_causality_between_tech_and_society, empirically_contingent).
narrative_ontology:cs_axiom('92b2f11b-3a38-41e1-9413-fc60d677e992', foundational, technology_enables_but_does_not_determine).
narrative_ontology:cs_axiom_status(technology_enables_but_does_not_determine, holdable).
narrative_ontology:cs_axiom_grounding('92b2f11b-3a38-41e1-9413-fc60d677e992', technology_enables_but_does_not_determine, empirically_contingent).
narrative_ontology:cs_reference_frame('92b2f11b-3a38-41e1-9413-fc60d677e992', complex_adaptive_systems_view).
narrative_ontology:cs_drift_state('92b2f11b-3a38-41e1-9413-fc60d677e992', contemporary_historical_scholarship, gap(stable, minor, true)).
narrative_ontology:cs_created_at('92b2f11b-3a38-41e1-9413-fc60d677e992', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__co_constitution_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, reformation_reformers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, printing_press_operators).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, vernacular_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, catholic_church_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Utilized the printing press to disseminate their theological arguments and vernacular Bibles, but also adapted their message and organizational strategies in response to the capabilities and limitations of print media. They were enabled by the press but also shaped its content and distribution.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, reformation_reformers, beneficiary,
    organized, generational, constrained, regional).

% Profited from the increased demand for printed materials, including religious texts. They were instrumental in the physical production and distribution, but their choices of what to print were influenced by market demand, censorship, and reformer patronage.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, printing_press_operators, beneficiary,
    moderate, biographical, mobile, local).

% Gained unprecedented access to religious texts and new ideas, fostering literacy and individual interpretation. Their demand for vernacular materials influenced what was printed, but their access was mediated by reformers and printers.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, vernacular_readers, beneficiary,
    powerless, biographical, mobile, local).

% Experienced a loss of control over information dissemination and religious interpretation due to the printing press. They attempted to suppress dissenting texts but ultimately had to adapt their own communication strategies to the new media environment.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, catholic_church_hierarchy, payer,
    institutional, civilizational, constrained, global).

% Study the complex interplay between technological innovation and social change, seeking to understand the nuanced, bidirectional causal relationships rather than monocausal explanations.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press coordinated the rapid, widespread dissemination of complex ideas and texts across diverse social groups, enabling a shared intellectual and religious discourse that was previously impossible.
% TRANSFER_FUNCTION: Transferred information, theological arguments, and religious authority from centralized ecclesiastical control to a more distributed network of reformers, printers, and vernacular readers.
% ABSENT_VOICES: Illiterate populations and those in regions without printing infrastructure were excluded from direct participation in the print-mediated discourse, their perspectives shaped by oral traditions or mediated interpretations.
% DISAPPEARANCE_RATIONALE: If the co-constitutive relationship between printing and the Reformation vanished, the historical trajectory of early modern Europe would be fundamentally altered. The Reformation would likely have remained a localized theological dispute, lacking the means for rapid, widespread dissemination and popular engagement, and the development of media and public discourse would have taken a different path.
% FOUNDING_PROBLEM: The problem was how to rapidly and widely disseminate complex theological arguments and vernacular texts to a broad audience, and how social actors could leverage and adapt to new communication technologies.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology and media studies corroborate that understanding the co-constitutive relationship between technology and society remains a live problem, as it informs contemporary debates about digital media, social change, and technological impact. This perspective is attested by academic scholarship outside the immediate beneficiaries of the Reformation.
narrative_ontology:disappearance_verdict(technology_reformation_causality__co_constitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__co_constitution_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__co_constitution_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(technology_reformation_causality__co_constitution_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__co_constitution_reading_tests).
:- end_tests(technology_reformation_causality__co_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it describes a genuine coordination function (dissemination of ideas) with relatively low extraction and suppression. The 'extraction' here is primarily the cost of adapting to and participating in the new media environment, rather than a deliberate rent-seeking mechanism. Suppression is low because while the Catholic Church attempted to suppress, the distributed nature of printing made it difficult to enforce comprehensively. Theater ratio is low as the function of the press was genuinely transformative, not performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the reformers and printers, the dynamic was a powerful enabling force (Rope). From the perspective of the Catholic Church, it was a disruptive force that eroded their authority (experiencing it as a form of extraction or even a Snare, though the constraint itself is not one). The analytical observer sees the co-constitutive dynamic as a Rope, acknowledging the differential impacts on various actors.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers, printers, and vernacular readers were beneficiaries, leveraging the press for their respective goals. The Catholic Church hierarchy was a 'payer' in the sense that it bore the costs of losing its information monopoly and had to adapt to a new media landscape. The constraint itself (the co-constitutive dynamic) did not extract from specific victims in the same way a Snare would, but rather shifted power and influence.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_direction_ambiguity,
    'What was the precise balance of causal influence between the printing press and the Reformation? Was it truly co-constitutive, or did one factor exert a stronger, albeit non-deterministic, influence?',
    'Further historical and sociological analysis, potentially using counterfactual modeling or comparative studies of similar technological introductions in different social contexts.',
    'If a stronger unidirectional influence were found (e.g., the press was more determinative than currently understood), the constraint might lean towards a more ''Mountain-like'' aspect of technological inevitability, or if social agency was overwhelmingly dominant, it might emphasize the ''Rope'' aspect of human coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_direction_ambiguity, empirical, 'Ambiguity in the precise weighting of bidirectional causality.').

omega_variable(
    extraction_vs_disruption_cost,
    'To what extent were the costs borne by the Catholic Church a form of ''extraction'' by the co-constitutive dynamic, versus simply the unavoidable costs of disruption and adaptation to a new technological paradigm?',
    'Economic analysis of the costs of institutional adaptation versus direct transfers of value, and comparative studies of other disruptive technologies.',
    'If the costs were primarily ''disruption costs'' rather than ''extraction'', the overall extractiveness of the constraint would be lower, reinforcing its Rope classification. If a clear mechanism of value transfer to the beneficiaries at the Church''s expense could be identified, it might push towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_disruption_cost, conceptual, 'Distinguishing between costs of disruption and active extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__co_constitution_reading, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1450, technology_reformation_causality__co_constitution_reading, theater_ratio, 1450, 0.05).
narrative_ontology:measurement(tech_tr_t1490, technology_reformation_causality__co_constitution_reading, theater_ratio, 1490, 0.08).
narrative_ontology:measurement(tech_tr_t1530, technology_reformation_causality__co_constitution_reading, theater_ratio, 1530, 0.1).
narrative_ontology:measurement(tech_tr_t1570, technology_reformation_causality__co_constitution_reading, theater_ratio, 1570, 0.1).
narrative_ontology:measurement(tech_tr_t1610, technology_reformation_causality__co_constitution_reading, theater_ratio, 1610, 0.1).
narrative_ontology:measurement(tech_tr_t1650, technology_reformation_causality__co_constitution_reading, theater_ratio, 1650, 0.1).

% Extraction over time
narrative_ontology:measurement(tech_be_t1450, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1450, 0.1).
narrative_ontology:measurement(tech_be_t1490, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1490, 0.15).
narrative_ontology:measurement(tech_be_t1530, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1530, 0.2).
narrative_ontology:measurement(tech_be_t1570, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1570, 0.25).
narrative_ontology:measurement(tech_be_t1610, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1610, 0.25).
narrative_ontology:measurement(tech_be_t1650, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1650, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1450, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1450, 0.05).
narrative_ontology:measurement(tech_su_t1490, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1490, 0.1).
narrative_ontology:measurement(tech_su_t1530, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1530, 0.15).
narrative_ontology:measurement(tech_su_t1570, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1570, 0.15).
narrative_ontology:measurement(tech_su_t1610, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1610, 0.15).
narrative_ontology:measurement(tech_su_t1650, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1650, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__co_constitution_reading, information_standard).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technology_reformation_causality__technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technology_reformation_causality__beneficiary_agency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'technology_reformation_causality' kernel, emphasizing co-constitution. It is linked to sibling readings that offer alternative causal explanations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
