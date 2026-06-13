% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__co_constitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__co_constitution, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: press_reformation_causality__co_constitution
 *   human_readable: Print-Reformation Co-Constitution Dynamics
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint describes the co-constitutive relationship between the
 *   printing press and the Reformation. It posits that neither technology nor
 *   human agency was solely determinant, but rather that feedback loops
 *   between the evolving print economy and religious controversies shaped
 *   both. The printing press acted as a Scaffold, enabling new forms of
 *   communication, while the interactions between printers, reformers, and
 *   the Catholic Church created Tangled Rope dynamics of coordination and
 *   extraction. This reading emphasizes emergent properties and distributed
 *   agency over singular causal forces.
 *
 * KEY AGENTS:
 *   - printers_publishers: Primary beneficiaries (economic gains), also agenda_setters (shaping content)
 *   - reformation_leaders: Primary beneficiaries (dissemination of ideas), also agenda_setters (theological content)
 *   - catholic_church_hierarchy: Primary victims (loss of control, authority erosion), also agenda_setters (attempts at censorship)
 *   - vernacular_readers: Beneficiaries (access to new ideas), also payers (cost of books)
 *   - traditional_scribal_economy: Victims (obsolescence)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__co_constitution, 0.45).
domain_priors:suppression_score(press_reformation_causality__co_constitution, 0.6).
domain_priors:theater_ratio(press_reformation_causality__co_constitution, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, extractiveness, 0.45).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__co_constitution, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__co_constitution, "Print-Reformation Co-Constitution Dynamics").
narrative_ontology:topic_domain(press_reformation_causality__co_constitution, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causality__co_constitution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__co_constitution, '9d100034-aaf6-4624-8e56-c0624e00c304').
narrative_ontology:cs_kernel_codification('9d100034-aaf6-4624-8e56-c0624e00c304', distributed).
narrative_ontology:cs_authority_grounding('9d100034-aaf6-4624-8e56-c0624e00c304', practice).
narrative_ontology:cs_interpretation_layer_present('9d100034-aaf6-4624-8e56-c0624e00c304').
narrative_ontology:cs_reading_relation('9d100034-aaf6-4624-8e56-c0624e00c304', press_reformation_causality__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('9d100034-aaf6-4624-8e56-c0624e00c304', press_reformation_causality__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('9d100034-aaf6-4624-8e56-c0624e00c304', foundational, technology_human_agency_interdependent).
narrative_ontology:cs_axiom_status(technology_human_agency_interdependent, holdable).
narrative_ontology:cs_axiom_grounding('9d100034-aaf6-4624-8e56-c0624e00c304', technology_human_agency_interdependent, empirically_contingent).
narrative_ontology:cs_axiom('9d100034-aaf6-4624-8e56-c0624e00c304', foundational, emergent_properties_shape_history).
narrative_ontology:cs_axiom_status(emergent_properties_shape_history, holdable).
narrative_ontology:cs_axiom_grounding('9d100034-aaf6-4624-8e56-c0624e00c304', emergent_properties_shape_history, empirically_contingent).
narrative_ontology:cs_reference_frame('9d100034-aaf6-4624-8e56-c0624e00c304', dynamic_feedback_system).
narrative_ontology:cs_drift_state('9d100034-aaf6-4624-8e56-c0624e00c304', contemporary_historiography, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9d100034-aaf6-4624-8e56-c0624e00c304', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__co_constitution, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, printers_publishers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, reformation_leaders).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, vernacular_readers).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, catholic_church_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, traditional_scribal_economy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, vernacular_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained significant economic advantage and influence by printing and distributing Reformation texts. They actively sought out controversial content to maximize sales, thereby fueling the religious debates. Their choices shaped the content and reach of the Reformation.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, printers_publishers, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, printers_publishers, agenda_setter).

% Leveraged the printing press to rapidly disseminate their theological arguments, vernacular Bibles, and polemics, reaching a wider audience than ever before. Their theological innovations and writings provided the content that printers eagerly published.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, reformation_leaders, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, reformation_leaders, agenda_setter).

% Experienced a profound challenge to its authority and control over religious discourse. They attempted to suppress dissenting texts through censorship and excommunication, but often found their efforts outpaced by the speed and volume of print. They bore the costs of losing their monopoly on information.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, catholic_church_hierarchy, payer,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, catholic_church_hierarchy, agenda_setter).

% Gained unprecedented access to religious texts and theological debates in their own languages, fostering new forms of individual piety and critical engagement. They paid for books, but the benefits of literacy and direct access to scripture were substantial.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, vernacular_readers, beneficiary,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, vernacular_readers, payer).

% Comprised of scribes, illuminators, and monastic copyists whose livelihoods were rapidly undermined by the efficiency and lower cost of printed books. They had few alternatives and experienced significant economic displacement.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, traditional_scribal_economy, payer,
    powerless, immediate, trapped, local).

% Observed the religious controversies and the power of the press, often intervening to support or suppress certain factions based on political expediency. Their actions influenced the spread of the Reformation but were also reactive to the emergent dynamics.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, secular_rulers, observer,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, secular_rulers, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__co_constitution, printers_publishers).
narrative_ontology:fixing_cost_class(press_reformation_causality__co_constitution, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitated the rapid, widespread, and relatively standardized dissemination of religious and polemical texts across Europe, enabling a shared intellectual and theological discourse that transcended local boundaries.
% TRANSFER_FUNCTION: Transferred theological arguments, vernacular scripture, and polemical attacks from authors to a mass audience, simultaneously transferring economic value (book sales) to printers and publishers, and political/religious authority away from the Catholic Church.
% ABSENT_VOICES: Those who preferred traditional, oral, or manuscript-based forms of religious transmission were increasingly marginalized; their objections to the speed, volume, and content of print were drowned out by the new media's dominance. Also, those who might have offered a mediating theological position were often forced into starker choices by the polemical nature of print.
% DISAPPEARANCE_RATIONALE: If the co-constitutive dynamics of the printing press and the Reformation vanished, the religious, political, and social landscape of early modern Europe would be fundamentally different. The rapid spread of Protestant ideas, the rise of vernacular literacy, and the challenge to papal authority would not have occurred in the same way or with the same intensity, leading to a vastly different historical trajectory.
% FOUNDING_PROBLEM: The problem was the slow, expensive, and error-prone dissemination of texts, particularly religious and scholarly works, which limited access to knowledge and centralized control over information.
% FOUNDING_PROBLEM_CORROBORATION: The problem of slow, expensive text dissemination was largely 'solved' by the printing press itself. While new problems of information overload and censorship emerged, the original problem is dead. Historians of technology and media studies, from outside the direct beneficiaries of the print economy, corroborate this shift, noting the transformative impact of print on information access.
narrative_ontology:disappearance_verdict(press_reformation_causality__co_constitution, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__co_constitution, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__co_constitution, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(press_reformation_causality__co_constitution, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__co_constitution_tests).
:- end_tests(press_reformation_causality__co_constitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it involves genuine coordination (dissemination of ideas, creation of a public sphere) alongside asymmetric extraction. Printers and reformers benefited from the rapid spread of their ideas and products, while the Catholic Church and the traditional scribal economy bore significant costs in terms of lost authority and obsolescence. Active enforcement was required by the Church to suppress dissenting texts, and by printers to protect their intellectual property. The metrics reflect a dynamic where extractiveness and suppression increased as the co-constitutive feedback loops intensified.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of printers and reformers, the constraint was a powerful Rope or Scaffold, enabling their goals. From the perspective of the Catholic Church, it was a Snare, undermining its authority and extracting control. This reading, 'co_constitution', attempts to capture the systemic, emergent properties of these interactions, where no single agent fully controlled the outcome, but all were shaped by the evolving system.
 *
 * DIRECTIONALITY LOGIC:
 *   Printers and reformers were beneficiaries (d near 0.0) as the press amplified their reach and influence. Vernacular readers were also beneficiaries, gaining access to information, though they paid for books. The Catholic Church hierarchy and the traditional scribal economy were victims (d near 1.0), experiencing erosion of authority and economic displacement. The co-constitutive nature means that even beneficiaries were subject to the emergent dynamics, not fully in control.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'co_constitution' framing prevents mislabeling the print-Reformation dynamic as purely extractive (Snare) or purely coordinative (Rope). It acknowledges the genuine coordination function of the press in disseminating information, while also recognizing the asymmetric costs borne by those whose power structures were disrupted. It avoids the pitfall of viewing the press as a neutral tool or a purely deterministic force, instead highlighting the dynamic interplay of technology and human choices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    co_constitution_vs_determinism,
    'Is the relationship between the printing press and the Reformation one of co-constitution, or did technology autonomously determine the outcome?',
    'Detailed historical analysis of specific instances where human agency (e.g., editorial choices, theological debates) demonstrably altered the trajectory of print dissemination, or where technological limitations were overcome by strategic human action.',
    'If technological determinism were true, the constraint would be closer to a Mountain for the spread of ideas, with lower extractiveness from human agents. If co-constitution holds, the constraint remains a Tangled Rope, emphasizing the dynamic interplay.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(co_constitution_vs_determinism, conceptual, 'Distinguishing co-constitution from technological determinism in the Reformation.').

omega_variable(
    co_constitution_vs_strategic_deployment,
    'To what extent was the co-constitution a result of emergent feedback loops, versus deliberate, strategic deployment of the press by specific actors?',
    'Micro-historical studies tracing the evolution of print markets and religious controversies, identifying moments where emergent properties (e.g., unexpected demand for certain texts) drove further innovation, rather than top-down strategic planning.',
    'If strategic deployment were dominant, the constraint would lean more towards a Snare for the targeted populations, with higher suppression and more concentrated beneficiaries. Co-constitution implies a more distributed and less centrally controlled dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_constitution_vs_strategic_deployment, empirical, 'Distinguishing co-constitution from strategic deployment in the Reformation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__co_constitution, 1500, 1520).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t0, press_reformation_causality__co_constitution, theater_ratio, 0, 0.05).
narrative_ontology:measurement(pres_tr_t10, press_reformation_causality__co_constitution, theater_ratio, 10, 0.08).
narrative_ontology:measurement(pres_tr_t20, press_reformation_causality__co_constitution, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(pres_be_t0, press_reformation_causality__co_constitution, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(pres_be_t10, press_reformation_causality__co_constitution, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(pres_be_t20, press_reformation_causality__co_constitution, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t0, press_reformation_causality__co_constitution, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(pres_su_t10, press_reformation_causality__co_constitution, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(pres_su_t20, press_reformation_causality__co_constitution, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__co_constitution, information_standard).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__strategic_deployment).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'press_reformation_causality' kernel. This 'co_constitution' reading emphasizes emergent feedback loops, distinct from 'technological_determinism' (autonomous tech) and 'strategic_deployment' (intentional weaponization).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
