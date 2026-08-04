% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Technological Determinism of Print and Reformation
 *   domain: historical/technological/religious
 *
 * SUMMARY:
 *   This constraint instantiates the technological determinism reading of the
 *   contested kernel press_reformation_causation. The reading treats the
 *   printing press as an upstream mountain of historical causation that made
 *   ecclesiastical censorship impossible and vernacular scripture inevitable,
 *   thereby causing the Reformation. Sibling readings include
 *   strategic_deployment (human agency exploits neutral technology) and
 *   mutual_shaping (technology and reformers co-evolve). Within this reading,
 *   reformers are downstream beneficiaries of exogenous capacity, while the
 *   Catholic hierarchy and manuscript scribes bear the costs of technological
 *   obsolescence. The claim is mountain, but the presence of identifiable
 *   beneficiaries and the contested empirical status of inevitability
 *   position it as a false-summit candidate.
 *
 * KEY AGENTS:
 *   - protestant_reformers: Primary beneficiary (moderate/constrained) — receives exogenous communications capacity
 *   - urban_print_workshops: Secondary beneficiary (moderate/mobile) — profits from technologically framed demand
 *   - catholic_hierarchy: Primary payer (institutional/constrained) — loses information monopoly
 *   - manuscript_scribes: Secondary payer (powerless/trapped) — bears craft obsolescence
 *   - social_historians: Analytical observer (analytical/analytical) — contests the determinism narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.18).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.12).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.18).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Technological Determinism of Print and Reformation").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "historical/technological/religious").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, 'a77fabbf-6758-4025-9c8e-039b98e43981').
narrative_ontology:cs_kernel_codification('a77fabbf-6758-4025-9c8e-039b98e43981', distributed).
narrative_ontology:cs_authority_grounding('a77fabbf-6758-4025-9c8e-039b98e43981', expertise).
narrative_ontology:cs_interpretation_layer_present('a77fabbf-6758-4025-9c8e-039b98e43981').
narrative_ontology:cs_reading_relation('a77fabbf-6758-4025-9c8e-039b98e43981', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_reading_relation('a77fabbf-6758-4025-9c8e-039b98e43981', press_reformation_causation__mutual_shaping, coexists_with).
narrative_ontology:cs_axiom('a77fabbf-6758-4025-9c8e-039b98e43981', foundational, technology_as_autonomous_causal_force).
narrative_ontology:cs_axiom_status(technology_as_autonomous_causal_force, holdable).
narrative_ontology:cs_axiom_grounding('a77fabbf-6758-4025-9c8e-039b98e43981', technology_as_autonomous_causal_force, empirically_contingent).
narrative_ontology:cs_axiom('a77fabbf-6758-4025-9c8e-039b98e43981', foundational, censorship_impossibility_thesis).
narrative_ontology:cs_axiom_status(censorship_impossibility_thesis, holdable).
narrative_ontology:cs_axiom_grounding('a77fabbf-6758-4025-9c8e-039b98e43981', censorship_impossibility_thesis, empirically_contingent).
narrative_ontology:cs_reference_frame('a77fabbf-6758-4025-9c8e-039b98e43981', technological_upstream_causality).
narrative_ontology:cs_drift_state('a77fabbf-6758-4025-9c8e-039b98e43981', post_social_history_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a77fabbf-6758-4025-9c8e-039b98e43981', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, urban_print_workshops).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, vernacular_lay_readers).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, catholic_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, manuscript_scribes).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, media_determinism).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, technological_autonomy_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive an exogenous, irreversible communications infrastructure that disseminates their writings beyond ecclesiastical control; they do not create the press but benefit from its structural capacity to bypass traditional gatekeepers.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, protestant_reformers, beneficiary,
    moderate, biographical, constrained, continental).

% Operate the presses that produce vernacular texts; their economic survival depends on the volume of dissenting and popular literature that the narrative frames as technologically inevitable.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, urban_print_workshops, beneficiary,
    moderate, biographical, mobile, regional).

% Access scripture and polemic in their own languages for the first time; their reading practices are reorganized by the availability of cheap printed books that the constraint frames as inevitable once the press existed.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, vernacular_lay_readers, beneficiary,
    powerless, generational, constrained, continental).

% Loses the monopoly on textual interpretation and doctrinal dissemination; its censorship apparatus is rendered structurally obsolete by the press's capacity for mass reproduction, extracting centuries of information control.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_hierarchy, payer,
    institutional, generational, constrained, continental).

% Bear the obsolescence of hand-copying as printed books replace manuscripts; their specialized craft loses economic and institutional value, with no viable alternative skill path within the new media ecology.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, manuscript_scribes, payer,
    powerless, biographical, trapped, regional).

% Contest the technological determinism narrative through archival research emphasizing reformer agency, local political conditions, and the contingency of religious change; they observe the constraint from outside its own premises.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, social_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__technological_determinism, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the rapid, geographically distributed alignment of religious dissent by providing a single, reproducible textual infrastructure that replaces ecclesiastical mediation with direct print dissemination.
% TRANSFER_FUNCTION: Moves authority over textual interpretation and doctrinal access from the Catholic hierarchy and manuscript tradition to reformers, urban printers, and lay readers across politically fragmented Europe.
% ABSENT_VOICES: Peasant communities whose religiosity was oral and ritual rather than text-based; women mystics whose authority was experiential; and oral tradition carriers whose modes of transmission are invisible to a print-centric causal narrative.
% DISAPPEARANCE_RATIONALE: If the inevitability thesis disappeared, the Reformation would be re-read as contingent on human strategy, patronage, and local politics rather than technological compulsion; the entire early modern historiography would shift toward agency-centered narratives.
% FOUNDING_PROBLEM: How to explain the rapid, coordinated spread of Reformation ideas across politically fragmented Europe without centralized ecclesiastical support or existing mass media infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: Technological determinist historians and media theorists attest the problem; social historians and strategic-deployment scholars attest that the problem is better framed as one of human agency and institutional contingency. Corroboration from outside the benefiting parties: sociologists of technology and book historians who do not identify with the Reformation narrative but who have documented the press's material constraints.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.18, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.18) because the narrative frames the press as a neutral, natural diffusion mechanism rather than an active extraction device. Suppression is lower still (0.12) because the constraint's core claim is that censorship became impossible, not that a new suppression was imposed. Accessibility collapse is very high (0.88) because the reading asserts that once the press existed, alternatives to mass vernacular diffusion collapsed. Resistance is low (0.22) because Church resistance is narrated as futile and historiographical resistance from social historians is dismissed as missing the structural force of technology. Theater ratio is moderate (0.28) because the inevitability narrative contains a performative element that naturalizes what was historically contingent. Temporal measurements show extraction and theater rising as the narrative institutionalized in nineteenth- and twentieth-century historiography, then slightly declining under contemporary social-historical challenge.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats experience the constraint as a liberating mountain — an irreversible opening of communicative possibility. The payer seats experience the same historical process as a constructed narrative that naturalizes their dispossession and renders their resistance invisible. The engine computes this divergence from the structural data without the claim adjudicating which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (reformers, print workshops, lay readers) derive low directionality because the constraint subsidizes their textual capacity and historical standing. Payers (Catholic hierarchy, manuscript scribes) derive high directionality because the constraint extracts their control, legitimacy, and livelihood. Social historians as observers occupy the analytical exit with neutral directionality. The structural asymmetry is between those who gain exogenous media capacity and those whose institutional position depended on controlling scarce textual access.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — explaining rapid Reformation diffusion without centralized infrastructure — is contested rather than dead. Social historians argue the problem is solved by human agency and patronage networks, not by technological determinism. Because the problem status remains live in scholarly dispute, the constraint has not clearly outlived its mandate, though the theater ratio suggests some performative maintenance of the inevitability narrative beyond strict evidentiary need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    td_reading_kernel_location,
    'Does the technological determinism reading foreclose its sibling readings, or merely coexist with them as alternative historiographical framings?',
    'Formal historiographical survey tracking whether scholars hold hybrid or strictly exclusive commitments to technological causation versus strategic agency.',
    'If strict exclusivity holds, the reading relation should be forecloses rather than coexists_with, altering the engine''s coupling analysis between kernel readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(td_reading_kernel_location, conceptual, 'Structural relationship of TD reading to sibling readings in the kernel.').

omega_variable(
    technological_necessity_or_narrative,
    'Is the printing press''s causal role in the Reformation a structural feature of media history or a retrospective narrative construction that naturalizes technological change?',
    'Archival counterfactual analysis assessing whether Reformation diffusion patterns correlate with press density independently of reformer travel, patronage networks, and political protection.',
    'If the correlation is weak or contingent on human strategy, the mountain claim is a false summit and the constraint reclassifies to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_necessity_or_narrative, empirical, 'Natural law vs constructed narrative ambiguity for press causation.').

omega_variable(
    censorship_impossibility_empirical,
    'Did the press make censorship impossible, or did Catholic regimes maintain effective information control in some territories well into the seventeenth century?',
    'Systematic review of Index effectiveness, print license regimes, and smuggling interception rates across European polities 1500-1700.',
    'If censorship remained partially effective, the inevitability claim is overstated and the accessibility_collapse metric should be lower, weakening mountain certification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(censorship_impossibility_empirical, empirical, 'Empirical status of censorship impossibility thesis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t0, press_reformation_causation__technological_determinism, theater_ratio, 0, 0.1).
narrative_ontology:measurement(pres_tr_t10, press_reformation_causation__technological_determinism, theater_ratio, 10, 0.15).
narrative_ontology:measurement(pres_tr_t20, press_reformation_causation__technological_determinism, theater_ratio, 20, 0.2).
narrative_ontology:measurement(pres_tr_t30, press_reformation_causation__technological_determinism, theater_ratio, 30, 0.3).
narrative_ontology:measurement(pres_tr_t40, press_reformation_causation__technological_determinism, theater_ratio, 40, 0.35).
narrative_ontology:measurement(pres_tr_t50, press_reformation_causation__technological_determinism, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(pres_be_t0, press_reformation_causation__technological_determinism, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(pres_be_t10, press_reformation_causation__technological_determinism, base_extractiveness, 10, 0.1).
narrative_ontology:measurement(pres_be_t20, press_reformation_causation__technological_determinism, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(pres_be_t30, press_reformation_causation__technological_determinism, base_extractiveness, 30, 0.22).
narrative_ontology:measurement(pres_be_t40, press_reformation_causation__technological_determinism, base_extractiveness, 40, 0.28).
narrative_ontology:measurement(pres_be_t50, press_reformation_causation__technological_determinism, base_extractiveness, 50, 0.2).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(press_reformation_causation__technological_determinism, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the press_reformation_causation kernel. The TD reading claims mountain status for technology; siblings restore agency to human actors or describe co-evolution. Each reading carries a distinct epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
