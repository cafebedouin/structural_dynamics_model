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
 *   constraint_id: press_reformation_causality__co_constitution
 *   human_readable: Co-Constitutive Causality of Print and Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint models the co-constitutive causality between the printing
 *   press and the Protestant Reformation. It argues that neither technology
 *   nor human agency was a sole determinant, but rather they engaged in
 *   dynamic feedback loops: print enabled new religious discourse, which in
 *   turn drove demand for print, leading to innovations in printing and
 *   distribution, and further intensifying religious controversy and social
 *   change. The constraint itself is the emergent causal structure of this
 *   historical process.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__co_constitution, 0.78).
domain_priors:suppression_score(press_reformation_causality__co_constitution, 0.65).
domain_priors:theater_ratio(press_reformation_causality__co_constitution, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, extractiveness, 0.78).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__co_constitution, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__co_constitution, "Co-Constitutive Causality of Print and Reformation").
narrative_ontology:topic_domain(press_reformation_causality__co_constitution, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causality__co_constitution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__co_constitution, '3de9bdc6-cced-47e2-bae1-5ea06c379ea7').
narrative_ontology:cs_kernel_codification('3de9bdc6-cced-47e2-bae1-5ea06c379ea7', distributed).
narrative_ontology:cs_authority_grounding('3de9bdc6-cced-47e2-bae1-5ea06c379ea7', practice).
narrative_ontology:cs_interpretation_layer_present('3de9bdc6-cced-47e2-bae1-5ea06c379ea7').
narrative_ontology:cs_reading_relation('3de9bdc6-cced-47e2-bae1-5ea06c379ea7', press_reformation_causality__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('3de9bdc6-cced-47e2-bae1-5ea06c379ea7', press_reformation_causality__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('3de9bdc6-cced-47e2-bae1-5ea06c379ea7', foundational, technology_human_agency_interdependent).
narrative_ontology:cs_axiom_status(technology_human_agency_interdependent, holdable).
narrative_ontology:cs_axiom_grounding('3de9bdc6-cced-47e2-bae1-5ea06c379ea7', technology_human_agency_interdependent, empirically_contingent).
narrative_ontology:cs_axiom('3de9bdc6-cced-47e2-bae1-5ea06c379ea7', foundational, causality_emergent_from_feedback).
narrative_ontology:cs_axiom_status(causality_emergent_from_feedback, holdable).
narrative_ontology:cs_axiom_grounding('3de9bdc6-cced-47e2-bae1-5ea06c379ea7', causality_emergent_from_feedback, empirically_contingent).
narrative_ontology:cs_reference_frame('3de9bdc6-cced-47e2-bae1-5ea06c379ea7', dynamic_interplay_framework).
narrative_ontology:cs_drift_state('3de9bdc6-cced-47e2-bae1-5ea06c379ea7', contemporary_media_studies_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3de9bdc6-cced-47e2-bae1-5ea06c379ea7', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(press_reformation_causality__co_constitution, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, printers_publishers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, reformation_leaders).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, literate_public).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, secular_rulers).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, catholic_church).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, traditional_scribes_clergy).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, persecuted_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, literate_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Economic actors who invested in printing technology and profited from the demand for religious texts and polemics. They actively sought out and published controversial material, shaping the discourse while also being shaped by market demand.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, printers_publishers, agenda_setter,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, printers_publishers, beneficiary).

% Religious figures who leveraged the printing press to disseminate their theological arguments, manifestos, and vernacular Bibles. Their ideas fueled the print economy, and print in turn amplified their reach and authority, but they also faced persecution and had to adapt their message for print.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, reformation_leaders, agenda_setter,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, reformation_leaders, beneficiary).

% The established religious authority that saw its monopoly on religious interpretation and its institutional power challenged by the spread of print. It attempted to suppress dissenting texts through censorship and persecution, but its efforts often backfired, further fueling the controversy and print demand.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, catholic_church, payer,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, catholic_church, agenda_setter).

% Political authorities who navigated the religious conflicts, sometimes supporting reformers to gain power from the Church, sometimes suppressing them to maintain order. They benefited from increased control over their territories but paid the cost of religious wars and social instability.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, secular_rulers, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, secular_rulers, beneficiary).

% Individuals who gained unprecedented access to religious texts, theological debates, and new ideas, empowering personal interpretation. They paid for books and bore the social costs of religious division and conflict, sometimes facing persecution for their beliefs.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, literate_public, beneficiary,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, literate_public, payer).

% Those whose livelihoods and social roles were undermined by the rise of print. Their skills in manuscript production became obsolete, and their exclusive role as interpreters of scripture was challenged, leading to economic and social displacement.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, traditional_scribes_clergy, payer,
    powerless, immediate, trapped, local).

% Individuals who, caught in the intensifying religious controversies amplified by print, faced persecution, torture, or execution from both Catholic and Protestant authorities for their beliefs. Their agency was severely constrained by the violent outcomes of the co-constitutive process.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, persecuted_dissenters, payer,
    powerless, immediate, trapped, local).

% The majority of the population who could not directly engage with printed texts. While indirectly affected by the Reformation's social and political consequences, they were largely excluded from the direct feedback loops between print and religious controversy.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, illiterate_masses, excluded,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitated the rapid, widespread dissemination of religious and political ideas, enabling the formation of new communities of belief and challenging existing hierarchies, while also coordinating the economic activity of the nascent print industry.
% TRANSFER_FUNCTION: Transferred authority over religious interpretation from the clergy to individual readers, and economic power from scribes to printers. It also transferred social capital and influence to those who could effectively use the new medium, and extracted significant social and political costs (conflict, persecution) from society.
% ABSENT_VOICES: Those who lacked literacy or access to print, or whose views were actively suppressed by dominant factions (both Catholic and Protestant). Their perspectives on the unfolding events were marginalized by the very medium that amplified others.
% DISAPPEARANCE_RATIONALE: If the co-constitutive feedback loops between print and religious controversy had not emerged, the Reformation as we know it would not have occurred. The spread of ideas would have been vastly slower, less widespread, and less impactful, fundamentally altering the course of European history and the development of modern media.
% FOUNDING_PROBLEM: The desire for religious reform, access to scripture in vernacular, and challenges to papal authority, coupled with the economic potential of printing and the social demand for information.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology, religion, and media studies widely corroborate the intertwined nature of these developments, recognizing the ongoing relevance of media's role in shaping social and religious change. Independent scholars and academic institutions outside of specific religious or technological advocacy groups attest to this co-constitutive dynamic.
narrative_ontology:disappearance_verdict(press_reformation_causality__co_constitution, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__co_constitution, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__co_constitution, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(press_reformation_causality__co_constitution, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__co_constitution, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__co_constitution_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causality__co_constitution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(press_reformation_causality__co_constitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The 'co-constitution' is classified as a Tangled Rope because it simultaneously coordinated the spread of ideas and the growth of the print economy (beneficiaries: printers, reformers, literate public) while also extracting immense social, political, and economic costs (victims: Catholic Church, traditional scribes, persecuted dissenters). The extractiveness is high due to the widespread conflict and disruption, and suppression is significant as both sides attempted to control the narrative through censorship and violence. Theater ratio is low as the effects were profoundly real. The metrics show an intensification of both extractiveness and suppression during the peak of the Reformation, followed by a slight moderation as new equilibria were established.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Catholic Church, the co-constitutive process was a snare, actively undermining its authority and leading to immense losses. From the perspective of reformers and printers, it was a powerful, albeit risky, rope or scaffold, enabling the spread of truth and economic opportunity. This reading, however, attempts to capture the emergent, systemic nature of the causality itself, which contained both coordination and extraction for different parties.
 *
 * DIRECTIONALITY LOGIC:
 *   Printers and Reformation leaders were primary beneficiaries, leveraging the new medium for economic gain and ideological dissemination, respectively. The Catholic Church and traditional scribes were clear targets, suffering loss of authority and economic displacement. Secular rulers had a mixed role, benefiting from increased power but also bearing the costs of instability. The literate public gained access to information but also faced the costs of conflict. The directionality reflects these complex, often conflicting, structural relationships within the co-constitutive process.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agency_vs_affordance_balance,
    'What was the precise balance of human agency (strategic choices by reformers/printers) versus technological affordance (inherent capabilities of the press) in driving the co-constitutive process?',
    'Detailed historical case studies comparing outcomes in regions with similar technological access but different agentic choices, or vice-versa.',
    'If agency is found to be overwhelmingly dominant, the constraint might lean more towards a ''strategic_deployment'' reading; if affordance is dominant, towards ''technological_determinism''. This reading assumes a dynamic, irreducible interplay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_vs_affordance_balance, empirical, 'The relative weight of human choice vs. technological capacity in shaping historical outcomes.').

omega_variable(
    co_constitution_vs_determinism_ambiguity,
    'Is the ''co_constitution'' reading sufficiently distinct from ''technological_determinism'' to warrant a separate classification, or does it merely describe a more nuanced form of technological influence?',
    'Conceptual analysis of the definitions of ''co-constitution'' versus ''determinism'' in media theory and history of technology, focusing on the irreducibility of mutual shaping.',
    'If the distinction is deemed insufficient, this constraint might be re-absorbed into a more complex ''technological_determinism'' reading, or the ''forecloses'' relation might be weakened. This reading asserts the irreducibility of the feedback loop.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(co_constitution_vs_determinism_ambiguity, conceptual, 'Conceptual boundary between co-constitutive and deterministic causal claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__co_constitution, 1500, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causality__co_constitution, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(pres_tr_t1520, press_reformation_causality__co_constitution, theater_ratio, 1520, 0.08).
narrative_ontology:measurement(pres_tr_t1540, press_reformation_causality__co_constitution, theater_ratio, 1540, 0.1).
narrative_ontology:measurement(pres_tr_t1560, press_reformation_causality__co_constitution, theater_ratio, 1560, 0.12).
narrative_ontology:measurement(pres_tr_t1580, press_reformation_causality__co_constitution, theater_ratio, 1580, 0.11).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causality__co_constitution, theater_ratio, 1600, 0.1).

% Extraction over time
narrative_ontology:measurement(pres_be_t1500, press_reformation_causality__co_constitution, base_extractiveness, 1500, 0.4).
narrative_ontology:measurement(pres_be_t1520, press_reformation_causality__co_constitution, base_extractiveness, 1520, 0.6).
narrative_ontology:measurement(pres_be_t1540, press_reformation_causality__co_constitution, base_extractiveness, 1540, 0.75).
narrative_ontology:measurement(pres_be_t1560, press_reformation_causality__co_constitution, base_extractiveness, 1560, 0.85).
narrative_ontology:measurement(pres_be_t1580, press_reformation_causality__co_constitution, base_extractiveness, 1580, 0.8).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causality__co_constitution, base_extractiveness, 1600, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1500, press_reformation_causality__co_constitution, suppression_requirement, 1500, 0.3).
narrative_ontology:measurement(pres_su_t1520, press_reformation_causality__co_constitution, suppression_requirement, 1520, 0.5).
narrative_ontology:measurement(pres_su_t1540, press_reformation_causality__co_constitution, suppression_requirement, 1540, 0.7).
narrative_ontology:measurement(pres_su_t1560, press_reformation_causality__co_constitution, suppression_requirement, 1560, 0.8).
narrative_ontology:measurement(pres_su_t1580, press_reformation_causality__co_constitution, suppression_requirement, 1580, 0.7).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causality__co_constitution, suppression_requirement, 1600, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__co_constitution, information_standard).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__strategic_deployment).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'press_reformation_causality' kernel, focusing on the dynamic, mutual shaping between print technology and the Reformation. It is linked to the 'technological_determinism' and 'strategic_deployment' readings as part of a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
