% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__technological_determinism, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Printing Press as Inevitable Cause of Reformation (Technological Determinism Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'technological determinism'
 *   reading of the claim that the printing press caused the Reformation. In
 *   this reading, the printing press is treated as an autonomous,
 *   mountain-like force whose inherent properties (e.g., making censorship
 *   impossible, vernacular scripture inevitable) directly and inevitably led
 *   to the Reformation. Reformers are beneficiaries of this exogenous
 *   technological capacity, and the Church's resistance is futile against the
 *   technology's power. The low extractiveness and suppression reflect the
 *   view that the technology itself is a neutral, natural force, not an
 *   extractive human construct.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.05).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.02).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.05).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Printing Press as Inevitable Cause of Reformation (Technological Determinism Reading)").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, '8ab97683-d2b8-4188-b2ae-cefcc5e85215').
narrative_ontology:cs_kernel_codification('8ab97683-d2b8-4188-b2ae-cefcc5e85215', implicit).
narrative_ontology:cs_authority_grounding('8ab97683-d2b8-4188-b2ae-cefcc5e85215', diffuse_epistemic).
narrative_ontology:cs_reading_relation('8ab97683-d2b8-4188-b2ae-cefcc5e85215', press_reformation_causation__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('8ab97683-d2b8-4188-b2ae-cefcc5e85215', press_reformation_causation__mutual_shaping, forecloses).
narrative_ontology:cs_axiom('8ab97683-d2b8-4188-b2ae-cefcc5e85215', foundational, technology_as_autonomous_force).
narrative_ontology:cs_axiom_status(technology_as_autonomous_force, holdable).
narrative_ontology:cs_axiom_grounding('8ab97683-d2b8-4188-b2ae-cefcc5e85215', technology_as_autonomous_force, empirically_contingent).
narrative_ontology:cs_axiom('8ab97683-d2b8-4188-b2ae-cefcc5e85215', foundational, technological_effects_are_inevitable).
narrative_ontology:cs_axiom_status(technological_effects_are_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('8ab97683-d2b8-4188-b2ae-cefcc5e85215', technological_effects_are_inevitable, empirically_contingent).
narrative_ontology:cs_reference_frame('8ab97683-d2b8-4188-b2ae-cefcc5e85215', technological_imperative_framework).
narrative_ontology:cs_drift_state('8ab97683-d2b8-4188-b2ae-cefcc5e85215', contemporary_historiography, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('8ab97683-d2b8-4188-b2ae-cefcc5e85215', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, vernacular_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, printers_publishers).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, catholic_church).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The printing press itself, as an autonomous technological force, dictates the terms of information dissemination. Its inherent properties (speed, reproducibility, cost-effectiveness) are the primary drivers of historical change, making certain outcomes inevitable.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, printing_press_technology, agenda_setter,
    institutional, generational, analytical, global).

% Benefited from the press's inherent capacity to rapidly disseminate their ideas and vernacular translations of scripture, overcoming traditional gatekeepers. They are seen as downstream recipients of an exogenous technological capacity.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, protestant_reformers, beneficiary,
    powerful, biographical, arbitrage, regional).

% Suffered a loss of control over information flow and religious interpretation due to the press. Their attempts at censorship and maintaining Latin scripture were rendered futile by the technology's unstoppable spread and inherent properties.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_church, payer,
    institutional, civilizational, trapped, global).

% Gained unprecedented access to religious texts in their native languages, fostering individual interpretation and undermining the Church's monopoly on scripture. Their emergence is a direct, inevitable consequence of the press's capabilities.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, vernacular_readers, beneficiary,
    moderate, biographical, mobile, local).

% While benefiting economically from the demand for printed materials, their actions are largely seen as facilitating the inevitable spread of information, rather than actively shaping the Reformation's course. They are conduits for the technology's inherent power.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, printers_publishers, beneficiary,
    organized, biographical, arbitrage, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press inherently coordinates the rapid, widespread, and cost-effective dissemination of information, enabling a new scale of public discourse and knowledge sharing.
% TRANSFER_FUNCTION: Transfers the power of information control from centralized authorities (like the Church) to the technology itself and, by extension, to anyone with access to a press, making censorship ineffective.
% ABSENT_VOICES: The voices of those who believed in the inherent neutrality of technology, or the primacy of human agency in shaping its impact, are absent from this deterministic narrative. They would argue that the press was a tool whose effects depended on how it was used.
% DISAPPEARANCE_RATIONALE: If the claim of technological determinism vanished, the historical facts of the printing press and the Reformation would remain. What would change is the interpretation of their causal relationship, shifting from inevitable technological causation to more nuanced accounts of human agency and strategic deployment.
% FOUNDING_PROBLEM: The problem of understanding the primary drivers of historical change, particularly the role of technology in major societal transformations like the Reformation.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of technological determinism (e.g., early media theorists, some historians of technology) attest to the problem's live status. Critics (e.g., social constructivists of technology, many contemporary historians) argue the problem is framed too narrowly, ignoring human agency and social context, thus rendering the deterministic framing 'dead' as a complete explanation.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, world_unchanged).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causation__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causation__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causation__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics reflect the deterministic framing: extractiveness is low because the technology is seen as a neutral force, not actively extracting from anyone, but rather enabling. Suppression is low because the technology inherently breaks down prior suppressive mechanisms (like censorship). Accessibility collapse is high (0.95) because the technology fundamentally alters the landscape of information access, making alternatives to widespread printing nearly impossible. Resistance is low (0.08) because, in this view, resistance to the press's effects was largely ineffective. The claimed type is 'mountain' because the technology's impact is presented as an unchangeable, natural law of historical progression.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'printing_press_technology' (as an analytical construct), its effects are inevitable and natural. From the perspective of the 'catholic_church', the constraint is a destructive force. However, in this deterministic reading, the Church's perspective is framed as a reaction to an unstoppable force, rather than an active contestation of a human-made constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'printing_press_technology' itself is framed as the agenda-setter, dictating outcomes. 'Protestant_reformers' and 'vernacular_readers' are direct beneficiaries, as the technology enables their goals. The 'catholic_church' is the primary payer/victim, as its authority is undermined. 'Printers_publishers' are also beneficiaries, but primarily as conduits for the technology's inevitable effects. The directionality for the technology itself is near 0.0, as it is seen as a force that subsidizes certain outcomes without being a 'target' itself.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by asserting the technological determinism as a 'mountain' of historical causation. If the metrics were to show high extractiveness or active suppression by human agents, it would challenge the 'mountain' claim and suggest a 'snare' or 'tangled_rope' where human agency and power dynamics are more central. The low metrics here are consistent with the claim of an inevitable, natural-law-like process.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_agency_ambiguity,
    'Is the printing press an autonomous agent of historical change (as this reading claims), or a neutral tool whose impact is shaped by human agency and social context?',
    'Comparative historical analysis of printing''s adoption and impact in different cultural and political contexts, examining variations in outcomes despite similar technology.',
    'If the press is a neutral tool, the ''mountain'' classification for its causal role would be challenged, shifting towards a ''rope'' or ''tangled_rope'' where human choices and strategic deployment are central. This would also shift extractiveness and suppression to reflect human-driven dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_agency_ambiguity, conceptual, 'Ambiguity regarding the printing press''s inherent agency versus its role as a tool.').

omega_variable(
    causal_directionality_ambiguity,
    'Does the printing press unilaterally ''cause'' the Reformation, or is there a mutual shaping where the Reformation''s needs and actors also shaped the development and use of the press?',
    'Detailed historical studies tracing the co-evolution of printing technology, publishing practices, and religious movements, identifying feedback loops and reciprocal influences.',
    'If mutual shaping is dominant, the ''mountain'' classification would be inappropriate, as the constraint would be less about an inevitable force and more about a dynamic interaction, potentially leading to a ''rope'' or ''tangled_rope'' classification reflecting coordination and contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_directionality_ambiguity, empirical, 'Uncertainty about the unidirectional versus reciprocal nature of causation between the press and the Reformation.').

omega_variable(
    natural_law_vs_constructed_causation,
    'Is the causal link between the printing press and the Reformation a ''natural law'' of technological impact, or a constructed historical narrative that benefits certain interpretations of history?',
    'Analysis of historiographical debates and the political/ideological contexts in which deterministic narratives gained prominence, alongside counter-narratives emphasizing contingency and human choice.',
    'If the causation is a constructed narrative, the ''mountain'' claim would be reclassified, potentially as a ''tangled_rope'' or ''snare'' if the narrative serves to obscure human agency or power dynamics in historical processes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_causation, conceptual, 'Is the causal claim a natural law or a constructed narrative?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 1450, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__technological_determinism, theater_ratio, 1450, 0.01).
narrative_ontology:measurement(pres_tr_t1475, press_reformation_causation__technological_determinism, theater_ratio, 1475, 0.01).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causation__technological_determinism, theater_ratio, 1500, 0.01).
narrative_ontology:measurement(pres_tr_t1525, press_reformation_causation__technological_determinism, theater_ratio, 1525, 0.01).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causation__technological_determinism, theater_ratio, 1550, 0.01).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__technological_determinism, base_extractiveness, 1450, 0.01).
narrative_ontology:measurement(pres_be_t1475, press_reformation_causation__technological_determinism, base_extractiveness, 1475, 0.02).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causation__technological_determinism, base_extractiveness, 1500, 0.03).
narrative_ontology:measurement(pres_be_t1525, press_reformation_causation__technological_determinism, base_extractiveness, 1525, 0.04).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causation__technological_determinism, base_extractiveness, 1550, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causation__technological_determinism, suppression_requirement, 1450, 0.01).
narrative_ontology:measurement(pres_su_t1475, press_reformation_causation__technological_determinism, suppression_requirement, 1475, 0.01).
narrative_ontology:measurement(pres_su_t1500, press_reformation_causation__technological_determinism, suppression_requirement, 1500, 0.02).
narrative_ontology:measurement(pres_su_t1525, press_reformation_causation__technological_determinism, suppression_requirement, 1525, 0.02).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causation__technological_determinism, suppression_requirement, 1550, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
