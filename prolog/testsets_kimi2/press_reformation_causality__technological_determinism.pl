% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__technological_determinism, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: press_reformation_causality__technological_determinism
 *   human_readable: Technological Determinist Causality of the Reformation
 *   domain: history/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint instantiates the technological determinist reading of the
 *   press-Reformation kernel: the claim that the printing press operated as
 *   an autonomous, inevitable causal engine making vernacular scripture
 *   spread and Reformation success unstoppable. The reading treats the
 *   technology as a physical-historical mountain and human actors as
 *   downstream responders. This story authors the constraint as a mountain
 *   claim (the reading's own framing) while the metrics describe the active
 *   suppression of alternative causality and the extraction of historical
 *   agency â the engine measures the divergence between the reading's
 *   naturalization and its actual operation.
 *
 * KEY AGENTS:
 *   - determinist_historians: Primary beneficiary (institutional/mobile) â collect prestige from the inevitability narrative
 *   - protestant_reformers: Primary target (moderate/identity_locked) â agency extracted
 *   - vernacular_printers: Secondary target (moderate/constrained) â strategic choices erased
 *   - social_historians: Analytical observer (organized/analytical) â resistance and alternative documentation
 *   - tech_progress_ideologues: Secondary beneficiary (organized/arbitrage) â ideological rent extraction
 *   - catholic_institutional_voice: Excluded counter-narrative (institutional/trapped) â structurally absent from determinist frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, 0.72).
domain_priors:suppression_score(press_reformation_causality__technological_determinism, 0.78).
domain_priors:theater_ratio(press_reformation_causality__technological_determinism, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, extractiveness, 0.72).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causality__technological_determinism, "Technological Determinist Causality of the Reformation").
narrative_ontology:topic_domain(press_reformation_causality__technological_determinism, "history/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causality__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__technological_determinism, 'd7b4b4e5-29d5-46aa-b646-cf3168925af3').
narrative_ontology:cs_kernel_codification('d7b4b4e5-29d5-46aa-b646-cf3168925af3', implicit).
narrative_ontology:cs_authority_grounding('d7b4b4e5-29d5-46aa-b646-cf3168925af3', diffuse_epistemic).
narrative_ontology:cs_reading_relation('d7b4b4e5-29d5-46aa-b646-cf3168925af3', press_reformation_causality__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('d7b4b4e5-29d5-46aa-b646-cf3168925af3', press_reformation_causality__co_constitution, forecloses).
narrative_ontology:cs_axiom('d7b4b4e5-29d5-46aa-b646-cf3168925af3', foundational, autonomous_tech_determinism).
narrative_ontology:cs_axiom_status(autonomous_tech_determinism, holdable).
narrative_ontology:cs_axiom_grounding('d7b4b4e5-29d5-46aa-b646-cf3168925af3', autonomous_tech_determinism, empirically_contingent).
narrative_ontology:cs_axiom('d7b4b4e5-29d5-46aa-b646-cf3168925af3', foundational, agency_subordinate_to_affordance).
narrative_ontology:cs_axiom_status(agency_subordinate_to_affordance, holdable).
narrative_ontology:cs_axiom_grounding('d7b4b4e5-29d5-46aa-b646-cf3168925af3', agency_subordinate_to_affordance, conventional).
narrative_ontology:cs_reference_frame('d7b4b4e5-29d5-46aa-b646-cf3168925af3', technological_primacy_framework).
narrative_ontology:cs_drift_state('d7b4b4e5-29d5-46aa-b646-cf3168925af3', post_social_history_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d7b4b4e5-29d5-46aa-b646-cf3168925af3', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__technological_determinism, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, determinist_historians).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, tech_progress_ideologues).
narrative_ontology:constraint_victim(press_reformation_causality__technological_determinism, protestant_reformers).
narrative_ontology:constraint_victim(press_reformation_causality__technological_determinism, vernacular_printers).
narrative_ontology:constraint_vindicates(press_reformation_causality__technological_determinism, technological_autonomy_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Built academic careers, departments, and publishing lists on the thesis that the printing press autonomously caused the Reformation. Their institutional prestige, grant streams, and conference circuits depend on maintaining the inevitability narrative. They can exit to social or cultural history but face professional restructuring costs.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, determinist_historians, beneficiary,
    institutional, generational, mobile, global).

% Invoke the printing press as a historical proof-of-concept that technology inevitably drives progressive social change. They extract ideological legitimacy from the analogy to justify contemporary tech policy and investment narratives without maintaining the scholarly apparatus.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, tech_progress_ideologues, beneficiary,
    organized, generational, arbitrage, global).

% Within the determinist narrative their theological innovation, political risk-taking, and coalition-building are erased or treated as downstream effects of the press. Their historical agency is extracted to serve the causal priority of technology. Their identity is fused with the Reformation that the narrative reattributes.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, protestant_reformers, payer,
    moderate, biographical, identity_locked, continental).

% Their entrepreneurial decisions about what to print, where to distribute, how to price, and how to evade censorship are treated as automatic consequences of the press's existence rather than strategic action. They bear the cost of causal erasure.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, vernacular_printers, payer,
    moderate, biographical, constrained, continental).

% Study economic, political, and social contingency in the Reformation. They observe the determinist constraint from outside, documenting the agency that the narrative suppresses, and mount resistance through archival research showing non-print pathways and regional variation.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, social_historians, observer,
    organized, generational, analytical, global).

% Would argue for the robustness of oral and manuscript transmission and the importance of institutional theology, but is excluded from the determinist framing which treats Catholic resistance as merely reactive to the press rather than as an independent causal force.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, catholic_institutional_voice, excluded,
    institutional, generational, trapped, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates historiographical explanation by providing a single-material-cause framework for the rapid diffusion of Reformation ideas across sixteenth-century Europe.
% TRANSFER_FUNCTION: Moves causal credit and explanatory authority from human strategic actors (reformers, printers, patrons, readers) to the printing press as an autonomous technology; moves scholarly prestige and ideological legitimacy to determinist historians and tech-progress advocates.
% ABSENT_VOICES: Social historians emphasizing contingency, Catholic institutional historians emphasizing non-print theological transmission, and the reformers and printers themselves as strategists rather than effects. They are excluded by the determinist frame's insistence on technology as the sole prime mover.
% DISAPPEARANCE_RATIONALE: If the determinist causal constraint vanished overnight, the Reformation would need to be re-explained through human agency, institutional conflict, economic contingency, and regional variation; the current historiographical canon would reorganize around social history, and the prestige economy of technology historians would shift.
% FOUNDING_PROBLEM: How to explain the unprecedented speed, scale, and geographic reach of Reformation ideological diffusion in sixteenth-century Europe.
% FOUNDING_PROBLEM_CORROBORATION: Social historians and Catholic institutional historians attest the diffusion problem is real but contest the technological solution; they provide corroboration from outside the benefiting parties that the problem is live but that the determinist answer is not the only valid one.
narrative_ontology:disappearance_verdict(press_reformation_causality__technological_determinism, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__technological_determinism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causality__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__technological_determinism, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__technological_determinism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causality__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causality__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causality__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the narrative systematically transfers causal credit from human actors to the press. Suppression is high (0.78) because the determinist frame renders social, economic, and institutional explanations inaccessible or secondary. Theater ratio is moderate (0.45): the press's genuine affordance (mass reproduction) is real, but a growing share of the narrative performs inevitability rather than documenting contingency. Accessibility collapse is substantial (0.68) because once the determinist frame is adopted, alternative causal pathways seem unimaginable. Resistance (0.60) reflects sustained social-historical counter-research.
 *
 * PERSPECTIVAL GAP:
 *   The determinist historian seat and the reformer/printer seats compute differently: from the determinist position the press is an autonomous mountain and human actors are merely downstream; from the payer seats the same structure is active erasure of their strategic choices. The engine computes this divergence from the structural data â the mountain claim is the reading's self-description, not the computed classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Determinist historians and tech-progress ideologues sit at the beneficiary end (low d): they collect prestige, funding, and ideological legitimacy from the constraint's operation. Reformers and printers sit at the target end (high d): their agency is extracted by the narrative. Social historians are near symmetric (analytical, mobile). The Catholic institutional voice is excluded entirely, receiving no directionality because it is outside the conversation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the narrative as pure coordination (a helpful simplifying explanation) by requiring named victims and measuring suppression. The narrative does coordinate historical explanation, but the asymmetric extraction of agency from specific groups and the active suppression of social-historical alternatives demonstrate that it is not a rope. The mountain claim is the reading's own framing; the metrics and victim declarations reveal the extraction that the framing obscures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_history,
    'Is the printing press''s causal role in the Reformation a genuine natural-law-like historical inevitability, or a constructed historiographical narrative that benefits determinist scholars?',
    'Comparative historiography: if societies with comparable print technology but different religious politics show different outcomes, the inevitability claim is falsified.',
    'If constructed, the mountain claim is a false summit and the constraint reclassifies as tangled_rope or snare; if genuine natural law, the beneficiary structure must be explained as incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_history, empirical, 'Whether the determinist causality is natural or constructed').

omega_variable(
    agency_erasure_mechanism,
    'Is the erasure of human agency in this narrative structural (genuine lack of sources about printer strategy) or internalized (historiographical preference for materialist explanation)?',
    'Archival recovery of printer and reformer business records and correspondence; if strategic choice is documented but ignored, the erasure is internalized.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure because the field carries the suppression even when counter-evidence is available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_erasure_mechanism, conceptual, 'Structural vs internalized suppression of agency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__technological_determinism, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(press_ref_techdet_tr_t0, press_reformation_causality__technological_determinism, theater_ratio, 0, 0.1).
narrative_ontology:measurement(press_ref_techdet_tr_t24, press_reformation_causality__technological_determinism, theater_ratio, 24, 0.2).
narrative_ontology:measurement(press_ref_techdet_tr_t48, press_reformation_causality__technological_determinism, theater_ratio, 48, 0.35).
narrative_ontology:measurement(press_ref_techdet_tr_t72, press_reformation_causality__technological_determinism, theater_ratio, 72, 0.45).
narrative_ontology:measurement(press_ref_techdet_tr_t96, press_reformation_causality__technological_determinism, theater_ratio, 96, 0.5).
narrative_ontology:measurement(press_ref_techdet_tr_t120, press_reformation_causality__technological_determinism, theater_ratio, 120, 0.48).

% Extraction over time
narrative_ontology:measurement(press_ref_techdet_be_t0, press_reformation_causality__technological_determinism, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(press_ref_techdet_be_t24, press_reformation_causality__technological_determinism, base_extractiveness, 24, 0.42).
narrative_ontology:measurement(press_ref_techdet_be_t48, press_reformation_causality__technological_determinism, base_extractiveness, 48, 0.6).
narrative_ontology:measurement(press_ref_techdet_be_t72, press_reformation_causality__technological_determinism, base_extractiveness, 72, 0.72).
narrative_ontology:measurement(press_ref_techdet_be_t96, press_reformation_causality__technological_determinism, base_extractiveness, 96, 0.75).
narrative_ontology:measurement(press_ref_techdet_be_t120, press_reformation_causality__technological_determinism, base_extractiveness, 120, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(press_ref_techdet_su_t0, press_reformation_causality__technological_determinism, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(press_ref_techdet_su_t24, press_reformation_causality__technological_determinism, suppression_requirement, 24, 0.35).
narrative_ontology:measurement(press_ref_techdet_su_t48, press_reformation_causality__technological_determinism, suppression_requirement, 48, 0.55).
narrative_ontology:measurement(press_ref_techdet_su_t72, press_reformation_causality__technological_determinism, suppression_requirement, 72, 0.7).
narrative_ontology:measurement(press_ref_techdet_su_t96, press_reformation_causality__technological_determinism, suppression_requirement, 96, 0.72).
narrative_ontology:measurement(press_ref_techdet_su_t120, press_reformation_causality__technological_determinism, suppression_requirement, 120, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__technological_determinism, identity_coordination).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% This constraint is one member of the press_reformation_causality family, decomposed per the epsilon-invariance principle: the natural-language label 'press caused Reformation' conflates three structurally distinct claims. This story addresses the technological determinist reading (technology as autonomous mountain); siblings address strategic deployment (human agency weaponizing tech) and co-constitution (mutual feedback).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
