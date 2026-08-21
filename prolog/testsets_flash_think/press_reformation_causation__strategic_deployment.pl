% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__strategic_deployment, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: press_reformation_causation__strategic_deployment
 *   human_readable: Strategic Deployment of the Printing Press in the Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint story analyzes the printing press during the Reformation
 *   from the 'strategic deployment' perspective. It posits that the
 *   technology itself was a neutral capacity, and its transformative impact
 *   stemmed from the deliberate, purposeful actions of agents—specifically
 *   reformers and printers—who exploited its potential. The press is viewed
 *   as a 'rope' for coordination, enabling the spread of ideas and the
 *   organization of movements, rather than an autonomous force driving
 *   historical change.
 *
 * KEY AGENTS:
 *   - Reformers: Primary beneficiaries (organized/mobile) — gained influence and coordinated movements.
 *   - Printers: Primary beneficiaries (moderate/mobile) — gained profit and expanded influence.
 *   - Catholic Church: Primary payer (institutional/constrained) — lost control over information and authority.
 *   - Literate Populace: Beneficiary (moderate/mobile) — gained access to information and fostered literacy.
 *   - Illiterate Populace: Excluded (powerless/trapped) — lacked direct access to benefits.
 *   - Analytical Historians: Observer (analytical/analytical) — study the historical dynamics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__strategic_deployment, 0.15).
domain_priors:suppression_score(press_reformation_causation__strategic_deployment, 0.1).
domain_priors:theater_ratio(press_reformation_causation__strategic_deployment, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, extractiveness, 0.15).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__strategic_deployment, rope).
narrative_ontology:human_readable(press_reformation_causation__strategic_deployment, "Strategic Deployment of the Printing Press in the Reformation").
narrative_ontology:topic_domain(press_reformation_causation__strategic_deployment, "history_of_technology/religious_history/media_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__strategic_deployment, '4eb87b75-52b3-4261-9456-31455dcd6194').
narrative_ontology:cs_kernel_codification('4eb87b75-52b3-4261-9456-31455dcd6194', implicit).
narrative_ontology:cs_authority_grounding('4eb87b75-52b3-4261-9456-31455dcd6194', practice).
narrative_ontology:cs_reading_relation('4eb87b75-52b3-4261-9456-31455dcd6194', press_reformation_causation__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('4eb87b75-52b3-4261-9456-31455dcd6194', press_reformation_causation__mutual_shaping, coexists_with).
narrative_ontology:cs_axiom('4eb87b75-52b3-4261-9456-31455dcd6194', foundational, human_agency_primary_driver).
narrative_ontology:cs_axiom_status(human_agency_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('4eb87b75-52b3-4261-9456-31455dcd6194', human_agency_primary_driver, conventional).
narrative_ontology:cs_axiom('4eb87b75-52b3-4261-9456-31455dcd6194', foundational, technology_as_neutral_capacity).
narrative_ontology:cs_axiom_status(technology_as_neutral_capacity, holdable).
narrative_ontology:cs_axiom_grounding('4eb87b75-52b3-4261-9456-31455dcd6194', technology_as_neutral_capacity, conventional).
narrative_ontology:cs_reference_frame('4eb87b75-52b3-4261-9456-31455dcd6194', human_intentionality_as_prime_mover).
narrative_ontology:cs_drift_state('4eb87b75-52b3-4261-9456-31455dcd6194', contemporary_historical_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4eb87b75-52b3-4261-9456-31455dcd6194', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__strategic_deployment, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, printers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, literate_populace).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, catholic_church).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively used the printing press to disseminate their theological and political ideas, bypassing traditional gatekeepers and coordinating their movements across Europe. They gained influence and followers through this strategic deployment.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, reformers, beneficiary,
    organized, generational, mobile, regional).

% Profited from the demand for printed materials, particularly those by reformers. They were key actors in the strategic deployment, often aligning with reformers for economic and ideological reasons, expanding their businesses and influence.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, printers, beneficiary,
    moderate, biographical, mobile, local).

% Suffered a loss of control over information dissemination and a challenge to its authority as reformers used the press to spread dissenting views. Its efforts to suppress printed materials were largely ineffective against the decentralized nature of printing.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, catholic_church, payer,
    institutional, civilizational, constrained, global).

% Gained unprecedented access to religious texts, pamphlets, and new ideas, fostering literacy and critical engagement. They were empowered by the availability of diverse perspectives.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, literate_populace, beneficiary,
    moderate, biographical, mobile, local).

% Remained largely excluded from direct engagement with printed materials, relying on oral dissemination or interpretations by others. While indirectly affected by the spread of ideas, they lacked direct access to the press's benefits.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, illiterate_populace, excluded,
    powerless, biographical, trapped, local).

% Study the historical impact of the printing press, analyzing the agency of reformers and printers in shaping its use and outcomes, and contrasting this with deterministic views.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, analytical_historians, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__strategic_deployment, reformers).
narrative_ontology:fixing_cost_class(press_reformation_causation__strategic_deployment, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled the rapid, widespread, and standardized dissemination of religious and political texts, coordinating the efforts of reformers and fostering a shared intellectual space for the Reformation movement.
% TRANSFER_FUNCTION: Transferred information, ideological influence, and cultural power from traditional ecclesiastical authorities to a broader literate public and to the reformers and printers who strategically leveraged the technology.
% ABSENT_VOICES: The illiterate populace, who could not directly access the printed word, and those whose views were not deemed profitable or politically expedient to print, were largely excluded from the direct conversation facilitated by the press.
% DISAPPEARANCE_RATIONALE: If the printing press had not been strategically deployed, the Reformation would have unfolded very differently, if at all. The rapid spread of ideas, the standardization of texts, and the coordination of reform movements would have been impossible, fundamentally altering the course of European history.
% FOUNDING_PROBLEM: The challenge of efficiently and widely disseminating complex theological arguments and vernacular scriptures to a broad audience, bypassing the slow and costly methods of manuscript copying and oral transmission.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Reformation and media studies widely corroborate that the printing press effectively solved the problem of mass information dissemination. While new problems of censorship and information overload emerged, the original problem of physical transmission was overcome. This is attested by numerous academic works outside the immediate beneficiaries.
narrative_ontology:disappearance_verdict(press_reformation_causation__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__strategic_deployment, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(press_reformation_causation__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__strategic_deployment, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__strategic_deployment_tests).
:- end_tests(press_reformation_causation__strategic_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) and suppression (0.10) reflect the view of the press as a neutral tool, primarily facilitating coordination rather than imposing costs or coercion. Any 'extraction' is seen as the natural profit of printers or the ideological gain of reformers, not systemic rent-seeking. Suppression is low because the technology itself was difficult to control once decentralized. Theater ratio is minimal (0.05) as the use of the press was highly functional and purposeful. Accessibility collapse is moderate (0.40) because while it opened access for the literate, it still excluded the illiterate. Resistance (0.30) was directed at the *content* disseminated, not the neutral technology itself, and was often ineffective.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of reformers and printers, the press was an empowering tool, a clear benefit. For the Catholic Church, it represented a challenge and a cost, as its traditional control over information eroded. The engine's per-seat classification will reflect these divergent experiences, with beneficiaries experiencing a 'rope' and the Catholic Church a 'snare' or 'tangled_rope' due to the costs it bore.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers and printers are clear beneficiaries (low d) as they directly leveraged the press for their goals and profited from it. The Catholic Church is a payer (high d) as it bore the costs of its authority being challenged and its control over information being undermined. The literate populace also benefits from increased access to information. The illiterate populace is excluded, not directly targeted or benefited by the *strategic deployment* of the press itself, but rather by the societal structures of literacy.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the press as a 'snare' by emphasizing the agency and coordination aspects. While the Catholic Church experienced it as extractive, the 'strategic deployment' reading highlights the genuine coordination function for reformers and printers. The founding problem of mass dissemination was solved, but the tool's persistence is due to its ongoing utility for new forms of communication and influence, not merely inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    press_neutrality_ambiguity,
    'Is the printing press truly a neutral capacity, or does its inherent structure (e.g., decentralization, reproducibility) inherently favor certain social or political outcomes, making ''neutrality'' a contested claim?',
    'Comparative historical analysis of other information technologies and their societal impacts, or theoretical analysis of technological affordances vs. human agency.',
    'If the press is found to have inherent biases or affordances that shape outcomes, the ''strategic deployment'' reading''s claim of neutrality would be weakened, potentially shifting the classification towards a ''tangled_rope'' or even ''snare'' for those whose interests were structurally disfavored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(press_neutrality_ambiguity, conceptual, 'Ambiguity regarding the inherent neutrality of the printing press technology.').

omega_variable(
    kernel_reading_divergence,
    'This constraint is one reading of the ''press_reformation_causation'' kernel. How would the classification and structural properties change if the ''technological_determinism'' or ''mutual_shaping'' readings were adopted?',
    'Comparative analysis of the structural properties (extractiveness, suppression, beneficiaries/victims) as authored in the sibling constraint stories for ''technological_determinism'' and ''mutual_shaping''.',
    'Adopting ''technological_determinism'' would likely increase the perceived extractiveness and suppression, as the technology itself would be seen as imposing outcomes, potentially shifting the classification towards a ''snare''. ''Mutual_shaping'' would likely present a more balanced view, potentially a ''tangled_rope'' reflecting co-evolutionary dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Impact of alternative readings of the ''press_reformation_causation'' kernel on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__strategic_deployment, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t0, press_reformation_causation__strategic_deployment, theater_ratio, 0, 0.02).
narrative_ontology:measurement(pres_tr_t50, press_reformation_causation__strategic_deployment, theater_ratio, 50, 0.03).
narrative_ontology:measurement(pres_tr_t100, press_reformation_causation__strategic_deployment, theater_ratio, 100, 0.05).
narrative_ontology:measurement(pres_tr_t150, press_reformation_causation__strategic_deployment, theater_ratio, 150, 0.04).
narrative_ontology:measurement(pres_tr_t200, press_reformation_causation__strategic_deployment, theater_ratio, 200, 0.05).

% Extraction over time
narrative_ontology:measurement(pres_be_t0, press_reformation_causation__strategic_deployment, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(pres_be_t50, press_reformation_causation__strategic_deployment, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(pres_be_t100, press_reformation_causation__strategic_deployment, base_extractiveness, 100, 0.15).
narrative_ontology:measurement(pres_be_t150, press_reformation_causation__strategic_deployment, base_extractiveness, 150, 0.16).
narrative_ontology:measurement(pres_be_t200, press_reformation_causation__strategic_deployment, base_extractiveness, 200, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t0, press_reformation_causation__strategic_deployment, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(pres_su_t50, press_reformation_causation__strategic_deployment, suppression_requirement, 50, 0.08).
narrative_ontology:measurement(pres_su_t100, press_reformation_causation__strategic_deployment, suppression_requirement, 100, 0.1).
narrative_ontology:measurement(pres_su_t150, press_reformation_causation__strategic_deployment, suppression_requirement, 150, 0.12).
narrative_ontology:measurement(pres_su_t200, press_reformation_causation__strategic_deployment, suppression_requirement, 200, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__strategic_deployment, information_standard).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'press_reformation_causation' kernel. This 'strategic_deployment' reading emphasizes human agency as the primary driver of the press's impact, viewing the technology as a neutral tool. It contrasts with 'technological_determinism' (press as autonomous cause) and 'mutual_shaping' (co-evolution of technology and agency).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
