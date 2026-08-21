% ============================================================================
% CONSTRAINT STORY: reformation_composite__technological_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__technological_mediation_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reformation_composite__technological_mediation_reading
 *   human_readable: Printing Press as Catalyst for Reformation Mass Movement
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   This constraint is the 'technological mediation' reading of the
 *   'reformation_composite' kernel. It focuses on the printing press as the
 *   primary enabler and amplifier of the Reformation, transforming local
 *   theological dissent into a continental mass movement. This reading
 *   contrasts with 'theological_fragmentation_reading' (focus on doctrinal
 *   differences) and 'political_realignment_reading' (focus on state power).
 *   The constraint is classified as a Tangled Rope because while it enabled
 *   significant coordination (mass movement), it also involved asymmetric
 *   extraction (from the old information regime) and required active
 *   enforcement (censorship, licensing) to shape its mediating effects.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__technological_mediation_reading, 0.45).
domain_priors:suppression_score(reformation_composite__technological_mediation_reading, 0.55).
domain_priors:theater_ratio(reformation_composite__technological_mediation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__technological_mediation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__technological_mediation_reading, "Printing Press as Catalyst for Reformation Mass Movement").
narrative_ontology:topic_domain(reformation_composite__technological_mediation_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__technological_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__technological_mediation_reading, 'a0fc6d10-93ce-4106-8c71-8ff52bf4ddb2').
narrative_ontology:cs_kernel_codification('a0fc6d10-93ce-4106-8c71-8ff52bf4ddb2', formalized).
narrative_ontology:cs_authority_grounding('a0fc6d10-93ce-4106-8c71-8ff52bf4ddb2', expertise).
narrative_ontology:cs_interpretation_layer_present('a0fc6d10-93ce-4106-8c71-8ff52bf4ddb2').
narrative_ontology:cs_reading_relation('a0fc6d10-93ce-4106-8c71-8ff52bf4ddb2', reformation_composite__theological_fragmentation_reading, influences).
narrative_ontology:cs_reading_relation('a0fc6d10-93ce-4106-8c71-8ff52bf4ddb2', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_axiom('a0fc6d10-93ce-4106-8c71-8ff52bf4ddb2', foundational, information_dissemination_is_causal).
narrative_ontology:cs_axiom_status(information_dissemination_is_causal, holdable).
narrative_ontology:cs_axiom_grounding('a0fc6d10-93ce-4106-8c71-8ff52bf4ddb2', information_dissemination_is_causal, empirically_contingent).
narrative_ontology:cs_axiom('a0fc6d10-93ce-4106-8c71-8ff52bf4ddb2', secondary, technology_amplifies_social_movements).
narrative_ontology:cs_axiom_status(technology_amplifies_social_movements, holdable).
narrative_ontology:cs_axiom_grounding('a0fc6d10-93ce-4106-8c71-8ff52bf4ddb2', technology_amplifies_social_movements, empirically_contingent).
narrative_ontology:cs_reference_frame('a0fc6d10-93ce-4106-8c71-8ff52bf4ddb2', pre_print_information_scarcity).
narrative_ontology:cs_drift_state('a0fc6d10-93ce-4106-8c71-8ff52bf4ddb2', post_gutenberg_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('a0fc6d10-93ce-4106-8c71-8ff52bf4ddb2', '').
narrative_ontology:cs_kernel_id(reformation_composite__technological_mediation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, protestant_reformers).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, literate_public).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, secular_princes).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, catholic_church_hierarchy).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, illiterate_masses).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operated the printing presses, controlling the physical production and distribution of texts. They were subject to licensing and censorship by both secular and religious authorities, making their role one of both enablement and constraint.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, printing_press_operators, agenda_setter,
    moderate, biographical, constrained, local).

% Leveraged the printing press to rapidly disseminate their theological arguments, critiques of the Catholic Church, and vernacular Bibles, transforming local dissent into a widespread movement. They gained significant influence and followers through this mediation.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, protestant_reformers, beneficiary,
    powerful, biographical, mobile, continental).

% Lost its monopoly on information dissemination and theological interpretation. It expended significant resources on censorship, book burning, and counter-reformation propaganda to resist the print-mediated spread of Protestant ideas, bearing the costs of a fractured information environment.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, catholic_church_hierarchy, payer,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(reformation_composite__technological_mediation_reading, catholic_church_hierarchy, agenda_setter).

% Gained unprecedented access to diverse theological and political ideas, fostering intellectual engagement and contributing to the formation of new religious and social identities. They benefited from the expanded discourse and availability of texts.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, literate_public, beneficiary,
    moderate, biographical, mobile, regional).

% Were largely excluded from direct engagement with printed texts due to illiteracy. While indirectly influenced by sermons and oral dissemination of print-mediated ideas, they lacked direct access to the primary source material, making them vulnerable to narratives they could not directly verify and limiting their agency in the print-driven discourse.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, illiterate_masses, excluded,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(reformation_composite__technological_mediation_reading, illiterate_masses, payer).

% Gained political leverage by supporting or controlling printing presses within their territories, using religious differentiation to assert sovereignty against imperial and papal authority. They benefited from the ability to disseminate their own political and religious agendas.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, secular_princes, beneficiary,
    institutional, generational, mobile, national).

% Study the long-term impacts of the printing press on the Reformation, analyzing publication rates, literacy trends, and the content of printed materials to understand the technological mediation's role in historical change.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__technological_mediation_reading, diffuse).
narrative_ontology:fixing_cost_class(reformation_composite__technological_mediation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled the rapid, widespread dissemination of theological arguments and critiques, coordinating a decentralized intellectual and social movement across vast geographical areas, and allowing for the formation of new religious and political identities.
% TRANSFER_FUNCTION: Transferred theological ideas, political critiques, and religious authority from centralized ecclesiastical control to a broader, literate public, and from local dissenters to a continental audience. It also transferred power and influence to those who could effectively use the new medium.
% ABSENT_VOICES: Those without access to printing technology or literacy, or those whose views were actively suppressed by secular or religious authorities who *did* control presses. Their narratives were marginalized or silenced in the emerging print-mediated discourse.
% DISAPPEARANCE_RATIONALE: If the printing press and its mediating effects vanished, the Reformation would likely have remained a series of localized theological disputes, unable to coalesce into a continental mass movement. The scale, speed, and nature of the religious and political changes would have been fundamentally different, and the existing power structures would have retained their information monopolies for much longer.
% FOUNDING_PROBLEM: The slow, expensive, and controlled dissemination of information, particularly religious texts and theological arguments, which limited intellectual discourse, centralized authority, and prevented rapid, widespread social mobilization.
% FOUNDING_PROBLEM_CORROBORATION: Historians universally corroborate the transformative impact of the printing press on information dissemination and its role in enabling the Reformation's scale. Contemporary accounts also highlight the unprecedented speed of information spread, supporting the claim that the original problem of slow dissemination was fundamentally altered.
narrative_ontology:disappearance_verdict(reformation_composite__technological_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__technological_mediation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__technological_mediation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(reformation_composite__technological_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__technological_mediation_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__technological_mediation_reading_tests).
:- end_tests(reformation_composite__technological_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the significant shift in power and information control away from established authorities, creating new winners and losers. Suppression (0.55) is substantial due to active efforts by both religious and secular powers to control the content and distribution of printed materials through censorship, licensing, and punitive measures. The theater ratio remains low (0.1) as the printing press itself was a highly functional technology. Accessibility collapse (0.6) indicates that while new channels for information opened, the ability to participate in the emerging mass movement via print became dominant, making older, slower forms of communication less effective for large-scale mobilization. Resistance (0.4) was directed at the content and control of the mediation, rather than the technology itself.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Protestant reformers and the literate public, the printing press was a liberating force, enabling widespread coordination and the spread of truth. From the perspective of the Catholic Church, it was a destructive force that undermined authority and spread heresy, requiring active suppression. The engine's classification as Tangled Rope captures this dual nature of coordination and extraction/suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Protestant reformers, the literate public, and secular princes were beneficiaries, gaining influence, access to ideas, and political leverage, respectively. The Catholic Church hierarchy and the illiterate masses were victims: the former lost its information monopoly and expended resources on resistance, while the latter were excluded from direct participation in the print-mediated discourse and subject to narratives they couldn't directly verify. The active enforcement of censorship and licensing by various authorities shaped this mediation, making it a Tangled Rope.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_determinism_vs_enabler,
    'Is the printing press a deterministic cause of the Reformation''s scale, or merely an enabler whose effects were shaped by pre-existing theological and political conditions?',
    'Comparative historical analysis of other periods with similar technological shifts but different social contexts, or counterfactual history exploring the Reformation without the printing press.',
    'If deterministic, the technological mediation is a more fundamental constraint; if an enabler, its classification is more sensitive to the underlying social and political structures it amplified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_determinism_vs_enabler, conceptual, 'The extent to which technology determines or merely enables historical change.').

omega_variable(
    control_vs_liberation_of_information,
    'Did the printing press primarily liberate information from central control, or did it create new forms of control and exclusion based on access to presses and literacy?',
    'Empirical studies of literacy rates, book ownership, and censorship effectiveness across different regions and social strata during the Reformation period.',
    'If primarily liberating, the constraint leans more towards a Rope; if new forms of control dominated, it leans more towards a Snare or a more extractive Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_vs_liberation_of_information, empirical, 'The dual nature of the printing press as both a liberator and a new source of control over information.').

omega_variable(
    print_vs_oral_mediation_reach,
    'What was the actual reach of print-mediated ideas, and to what extent were they further disseminated through oral networks, particularly among the illiterate masses?',
    'Historical research into popular culture, sermons, and public readings, combined with analysis of the content of printed materials designed for oral transmission.',
    'If oral dissemination was dominant, the direct ''mediation'' of the printing press might be less impactful for the illiterate, shifting the victim profile or reducing the overall extractiveness of the print-centric view.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(print_vs_oral_mediation_reach, empirical, 'The interplay between print and oral culture in disseminating Reformation ideas.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__technological_mediation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t0, reformation_composite__technological_mediation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(refo_tr_t20, reformation_composite__technological_mediation_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(refo_tr_t40, reformation_composite__technological_mediation_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(refo_tr_t60, reformation_composite__technological_mediation_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(refo_tr_t80, reformation_composite__technological_mediation_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(refo_tr_t100, reformation_composite__technological_mediation_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(refo_be_t0, reformation_composite__technological_mediation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(refo_be_t20, reformation_composite__technological_mediation_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(refo_be_t40, reformation_composite__technological_mediation_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(refo_be_t60, reformation_composite__technological_mediation_reading, base_extractiveness, 60, 0.43).
narrative_ontology:measurement(refo_be_t80, reformation_composite__technological_mediation_reading, base_extractiveness, 80, 0.44).
narrative_ontology:measurement(refo_be_t100, reformation_composite__technological_mediation_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t0, reformation_composite__technological_mediation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(refo_su_t20, reformation_composite__technological_mediation_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(refo_su_t40, reformation_composite__technological_mediation_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(refo_su_t60, reformation_composite__technological_mediation_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(refo_su_t80, reformation_composite__technological_mediation_reading, suppression_requirement, 80, 0.53).
narrative_ontology:measurement(refo_su_t100, reformation_composite__technological_mediation_reading, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__technological_mediation_reading, information_standard).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__political_realignment_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
