% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__strategic_deployment, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: press_reformation_causality__strategic_deployment
 *   human_readable: Strategic Weaponization of the Printing Press in the Reformation
 *   domain: history/religious/media
 *
 * SUMMARY:
 *   This constraint instantiates the 'strategic_deployment' reading of the
 *   contested kernel press_reformation_causality. The natural-language claim
 *   that 'the printing press caused the Reformation' conflates three
 *   structurally distinct historiographical propositions: autonomous
 *   technological determination, strategic actor instrumentality, and
 *   co-constitutive feedback. This story isolates the strategic-deployment
 *   claim: that reformers and printers actively weaponized the press to erode
 *   Church authority. In this reading, the press functions as a coordination
 *   mechanism for reformers and printers while operating as an extractive
 *   snare against Church authority, requiring active strategic enforcement to
 *   maintain. The claim is tangled_rope; the metrics are authored
 *   independently to reflect the extractive asymmetry and active enforcement.
 *
 * KEY AGENTS:
 *   - reformers: Agenda-setters (organized/identity_locked) â religious leaders who commissioned and directed print campaigns to propagate dissent
 *   - printers: Beneficiaries (moderate/mobile) â workshop owners who profited from reformist print demand and aligned output with reformer networks
 *   - church_authority: Payer (institutional/constrained) â Catholic hierarchy whose interpretive monopoly and economic surplus were eroded by mass vernacular printing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, 0.72).
domain_priors:suppression_score(press_reformation_causality__strategic_deployment, 0.68).
domain_priors:theater_ratio(press_reformation_causality__strategic_deployment, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, extractiveness, 0.72).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__strategic_deployment, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__strategic_deployment, "Strategic Weaponization of the Printing Press in the Reformation").
narrative_ontology:topic_domain(press_reformation_causality__strategic_deployment, "history/religious/media").

domain_priors:requires_active_enforcement(press_reformation_causality__strategic_deployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__strategic_deployment, 'ff2b45fc-a26f-4fd4-8d0b-9b64dea5fe21').
narrative_ontology:cs_kernel_codification('ff2b45fc-a26f-4fd4-8d0b-9b64dea5fe21', distributed).
narrative_ontology:cs_authority_grounding('ff2b45fc-a26f-4fd4-8d0b-9b64dea5fe21', distributed).
narrative_ontology:cs_reading_relation('ff2b45fc-a26f-4fd4-8d0b-9b64dea5fe21', press_reformation_causality__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('ff2b45fc-a26f-4fd4-8d0b-9b64dea5fe21', press_reformation_causality__co_constitution, coexists_with).
narrative_ontology:cs_axiom('ff2b45fc-a26f-4fd4-8d0b-9b64dea5fe21', foundational, reformers_as_strategic_actors).
narrative_ontology:cs_axiom_status(reformers_as_strategic_actors, holdable).
narrative_ontology:cs_axiom_grounding('ff2b45fc-a26f-4fd4-8d0b-9b64dea5fe21', reformers_as_strategic_actors, empirically_contingent).
narrative_ontology:cs_axiom('ff2b45fc-a26f-4fd4-8d0b-9b64dea5fe21', foundational, press_as_neutral_instrument).
narrative_ontology:cs_axiom_status(press_as_neutral_instrument, holdable).
narrative_ontology:cs_axiom_grounding('ff2b45fc-a26f-4fd4-8d0b-9b64dea5fe21', press_as_neutral_instrument, empirically_contingent).
narrative_ontology:cs_reference_frame('ff2b45fc-a26f-4fd4-8d0b-9b64dea5fe21', agentic_technology_field).
narrative_ontology:cs_drift_state('ff2b45fc-a26f-4fd4-8d0b-9b64dea5fe21', contemporary_historiography, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('ff2b45fc-a26f-4fd4-8d0b-9b64dea5fe21', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__strategic_deployment, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, printers).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, church_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious leaders and activists who deliberately commissioned, authored, and distributed printed pamphlets, vernacular Bibles, and polemics to undermine Catholic authority and synchronize dissent across regions. Their personal and theological identities are fused with the movement; exit would mean recantation, exile, or abandonment of their reformist identity.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, reformers, agenda_setter,
    organized, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, reformers, beneficiary).

% Workshop owners and journeymen who produced reformist tracts at scale, often relocating to evade local censorship. They profit from the surge in demand for vernacular religious texts and controversial pamphlets, aligning output with reformer networks for economic gain. They can shift to secular or Catholic work if local pressure rises, but reputation and capital tie them to the reformist print economy.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, printers, beneficiary,
    moderate, biographical, mobile, regional).

% The Catholic Church hierarchy whose monopoly on scriptural interpretation and sacramental mediation is eroded by mass vernacular printing. Must expend resources on censorship, theological counter-polemics, and institutional reform to retain authority. Cannot exit its role as guardian of orthodoxy without dissolving its foundational claim.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, church_authority, payer,
    institutional, civilizational, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the production and distribution of reformist religious ideas across geographically dispersed populations by providing a standardized, reproducible medium that aligns reformer networks and creates a shared textual base for theological dissent.
% TRANSFER_FUNCTION: Moves interpretive authority and economic surplus from the Church's scribal-sacramental monopoly to reformer networks and the printing workshops that serve them; transfers attention and loyalty from Church-controlled channels to vernacular print.
% ABSENT_VOICES: The illiterate rural laity who could not access or afford printed texts and whose religious practice remained oral and local; they are absent from the printed record but constitute the majority of the population. Also local Catholic clergy whose pastoral authority was undermined but who lacked channels to contest the reformer narrative in print.
% DISAPPEARANCE_RATIONALE: If the strategic weaponization of printing vanished, reformers would lack the coordination medium to synchronize dissent across regions; the Church's interpretive monopoly would remain intact in much of Europe; printer networks would not have aggregated capital around religious controversy; and the pace and geographic spread of Reformation would be drastically reduced.
% FOUNDING_PROBLEM: The Church's monopoly on sacred text and interpretation prevented theological reform and restricted lay access to scripture; reformers needed a scalable, evasion-resistant channel to propagate dissent and coordinate geographically separated sympathizers.
% FOUNDING_PROBLEM_CORROBORATION: Catholic censorship advocates and Counter-Reformation authorities of the period attest to the threat from print. Modern book historians and bibliographers outside the reformer tradition corroborate the scale, economics, and geographic structure of the deployment.
narrative_ontology:disappearance_verdict(press_reformation_causality__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__strategic_deployment, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causality__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__strategic_deployment, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__strategic_deployment_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(press_reformation_causality__strategic_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the press deployment systematically transferred interpretive authority and economic surplus from the Church to reformer-printer networks. Suppression (0.68) reflects the constraint's active undermining of the Church's alternative (scribal/oral control) through volume and geographic diffusion, as well as the Church's escalating counter-censorship. Accessibility_collapse (0.60) captures the collapse of the Church's information monopoly once the press network was established; alternatives (controlled manuscript circulation) became non-viable at scale. Resistance (0.75) is high because the Church mounted sustained counter-propaganda, censorship, and institutional reform. Theater_ratio (0.28) is moderate: much printing was functionally aimed at doctrinal change, but a substantial fraction served performative factional signaling and reputational display within the reform movement.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seats (reformers, printers) experience the constraint as coordination and opportunity: the press solves the problem of synchronizing dissent and generating revenue. The payer seat (church_authority) experiences the same structure as extractive erosion of its foundational authority. The engine will compute per-seat directionality accordingly: low d for beneficiaries, high d for the institutional victim.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers and printers are declared beneficiaries; they derive religious-political and economic gains from the constraint. Church_authority is the declared victim, bearing the loss of monopoly and the costs of defensive expenditure. The automatic derivation assigns reformers/printers low d (beneficiary-side) and church_authority high d (target-side). No override is necessary because the structural relationship is transparent: the reformers and printers are the agents of deployment, while the Church is the object of extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring both coordination and extraction for tangled_rope. A pure snare reading would ignore the genuine coordination function the press served for reformers (solving the collective-action problem of synchronizing geographically dispersed dissent). A pure rope reading would ignore the asymmetric extraction from Church authority. The active-enforcement requirement (true) ensures the constraint is not treated as emergent or self-sustaining; it required continuous strategic investment by reformers and printers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    press_autonomy_vs_agency,
    'Does the printing press possess any autonomous causal force independent of the reformers'' strategic intent, or is its impact entirely reducible to agent deployment?',
    'Comparative analysis of print adoption in non-reform contexts (Catholic print, scientific print) to isolate technology-specific effects from actor-specific effects.',
    'If autonomous force is present, the extraction from Church authority is partly technology-driven; if fully reducible to agency, the constraint is pure instrumental snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(press_autonomy_vs_agency, conceptual, 'Whether printing technology has autonomous causal force or is purely instrumental').

omega_variable(
    printer_motivation_ambiguity,
    'Were printers driven primarily by economic opportunism or by theological sympathy, and does this distinction change the constraint''s classification?',
    'Archival analysis of printer contracts, dedications, and output mixes to separate profit motive from ideological commitment.',
    'If purely economic, the reformers-printer relationship is market coordination; if ideological, it is closer to identity-locked coalition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(printer_motivation_ambiguity, empirical, 'Ambiguity of printer motivation between profit and ideology').

omega_variable(
    kernel_reading_framing,
    'Is the strategic deployment reading a claim about historical causation or a projection of modern instrumental rationality onto early modern actors?',
    'Historiographical review of actor categories: whether ''strategy'' and ''weaponization'' are etic or emic descriptors in the primary source record.',
    'If emic, the constraint reflects self-conscious actor intent; if etic, it is an analytical imposition that may overstate coordination and understate emergent feedback.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the strategic deployment frame is actor-native or analytical-imposed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__strategic_deployment, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t0, press_reformation_causality__strategic_deployment, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pres_tr_t16, press_reformation_causality__strategic_deployment, theater_ratio, 16, 0.2).
narrative_ontology:measurement(pres_tr_t32, press_reformation_causality__strategic_deployment, theater_ratio, 32, 0.25).
narrative_ontology:measurement(pres_tr_t48, press_reformation_causality__strategic_deployment, theater_ratio, 48, 0.3).
narrative_ontology:measurement(pres_tr_t64, press_reformation_causality__strategic_deployment, theater_ratio, 64, 0.28).
narrative_ontology:measurement(pres_tr_t80, press_reformation_causality__strategic_deployment, theater_ratio, 80, 0.25).

% Extraction over time
narrative_ontology:measurement(pres_be_t0, press_reformation_causality__strategic_deployment, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(pres_be_t16, press_reformation_causality__strategic_deployment, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(pres_be_t32, press_reformation_causality__strategic_deployment, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(pres_be_t48, press_reformation_causality__strategic_deployment, base_extractiveness, 48, 0.72).
narrative_ontology:measurement(pres_be_t64, press_reformation_causality__strategic_deployment, base_extractiveness, 64, 0.74).
narrative_ontology:measurement(pres_be_t80, press_reformation_causality__strategic_deployment, base_extractiveness, 80, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t0, press_reformation_causality__strategic_deployment, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(pres_su_t16, press_reformation_causality__strategic_deployment, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(pres_su_t32, press_reformation_causality__strategic_deployment, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(pres_su_t48, press_reformation_causality__strategic_deployment, suppression_requirement, 48, 0.7).
narrative_ontology:measurement(pres_su_t64, press_reformation_causality__strategic_deployment, suppression_requirement, 64, 0.75).
narrative_ontology:measurement(pres_su_t80, press_reformation_causality__strategic_deployment, suppression_requirement, 80, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the press_reformation_causality kernel, decomposed per the Îµ-invariance principle because the natural-language label 'printing press caused the Reformation' conflates strategic actor deployment, autonomous technological force, and co-constitutive feedback. Each reading carries a distinct Îµ and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
