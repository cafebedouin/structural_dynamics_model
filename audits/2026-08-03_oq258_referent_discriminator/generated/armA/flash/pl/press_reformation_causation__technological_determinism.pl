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
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Printing Press as Inevitable Cause of Reformation (Technological Determinism Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint represents the 'technological determinism' reading of the
 *   printing press's role in the Reformation. It posits the printing press as
 *   an autonomous, mountain-like force that inevitably caused the Reformation
 *   by making censorship impossible and vernacular scripture widespread. The
 *   technology itself is the primary agent, and its effects are seen as
 *   unavoidable. This reading emphasizes the inherent properties of the press
 *   (speed, reproducibility, cost-effectiveness) as the direct drivers of
 *   social and religious change, with human agency playing a secondary,
 *   reactive role. The claim is that the press was a Mountain, and the
 *   Reformation was its inevitable consequence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.05).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.95).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.05).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Printing Press as Inevitable Cause of Reformation (Technological Determinism Reading)").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, '056cca65-9d21-46da-ab81-913a6ceaf566').
narrative_ontology:cs_kernel_codification('056cca65-9d21-46da-ab81-913a6ceaf566', implicit).
narrative_ontology:cs_authority_grounding('056cca65-9d21-46da-ab81-913a6ceaf566', diffuse_epistemic).
narrative_ontology:cs_reading_relation('056cca65-9d21-46da-ab81-913a6ceaf566', press_reformation_causation__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('056cca65-9d21-46da-ab81-913a6ceaf566', press_reformation_causation__mutual_shaping, forecloses).
narrative_ontology:cs_axiom('056cca65-9d21-46da-ab81-913a6ceaf566', foundational, technology_as_autonomous_force).
narrative_ontology:cs_axiom_status(technology_as_autonomous_force, holdable).
narrative_ontology:cs_axiom_grounding('056cca65-9d21-46da-ab81-913a6ceaf566', technology_as_autonomous_force, empirically_contingent).
narrative_ontology:cs_axiom('056cca65-9d21-46da-ab81-913a6ceaf566', foundational, social_change_as_technologically_determined).
narrative_ontology:cs_axiom_status(social_change_as_technologically_determined, holdable).
narrative_ontology:cs_axiom_grounding('056cca65-9d21-46da-ab81-913a6ceaf566', social_change_as_technologically_determined, empirically_contingent).
narrative_ontology:cs_reference_frame('056cca65-9d21-46da-ab81-913a6ceaf566', technological_inevitability).
narrative_ontology:cs_drift_state('056cca65-9d21-46da-ab81-913a6ceaf566', contemporary_historiography, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('056cca65-9d21-46da-ab81-913a6ceaf566', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, vernacular_literacy_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, catholic_church_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The technology itself, acting as an autonomous force that dictates social outcomes. It inherently makes information dissemination rapid and cheap, thus rendering prior control mechanisms obsolete. It is the unyielding engine of change.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, printing_press_technology, agenda_setter,
    institutional, generational, analytical, universal).

% Benefited directly from the press's capacity to rapidly disseminate their ideas, critiques of the Church, and vernacular translations of scripture. They were downstream recipients of an exogenous technological capacity that amplified their message beyond any prior means.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, protestant_reformers, beneficiary,
    organized, biographical, arbitrage, continental).

% The primary institution whose authority and control over information were undermined by the printing press. Their attempts at censorship and control were rendered futile by the sheer volume and speed of printed material, leading to a loss of power and influence. They bore the cost of an inevitable technological shift.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_church_hierarchy, payer,
    institutional, civilizational, trapped, global).

% Benefited from the widespread availability of printed materials in local languages, which fueled a demand for literacy and direct engagement with religious texts, bypassing traditional clerical intermediaries. The press made their goals inherently achievable.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, vernacular_literacy_movements, beneficiary,
    moderate, generational, mobile, regional).

% Observe and analyze the historical forces at play, interpreting the printing press as a primary, deterministic cause of the Reformation, independent of human agency or strategic choices. Their analysis confirms the inevitability of the outcome.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, analytical_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press coordinated the rapid, widespread dissemination of information and ideas across Europe, enabling a shared intellectual and religious discourse that transcended prior geographical and linguistic barriers.
% TRANSFER_FUNCTION: Transferred the power of information control from centralized ecclesiastical authority to a decentralized network of printers, authors, and readers, making knowledge and religious texts accessible to a broader public.
% ABSENT_VOICES: Those who believed in the inherent neutrality of technology, or the primacy of human agency in historical change, are absent from this deterministic framing. They would argue that the press was a tool, not a cause, and its impact depended on how it was used.
% DISAPPEARANCE_RATIONALE: If the deterministic causal link vanished, the historical events of the Reformation would still have occurred, but their explanation would shift from technological inevitability to a more complex interplay of social, political, and religious factors. The 'world' of historical events remains, but its interpretation changes.
% FOUNDING_PROBLEM: The problem of slow, expensive, and centrally controlled information dissemination, which limited the spread of new ideas and maintained existing power structures.
% FOUNDING_PROBLEM_CORROBORATION: This reading itself asserts the problem was definitively 'solved' by the press, leading to the Reformation. Critics (from the 'strategic deployment' and 'mutual shaping' readings) would argue that the problem was not simply 'solved' but transformed, and that human agency was crucial in exploiting the press, not merely reacting to it. No corroboration from outside the deterministic framework exists for the 'dead' status as an inevitable outcome.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, world_unchanged).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
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
 *   The low extractiveness (0.05) and theater ratio (0.01) reflect the view that the press itself was not 'extracting' in a human sense, nor was its operation performative; it simply *was*. The high suppression (0.95) and accessibility collapse (0.98) indicate that the press's inherent properties made prior methods of information control (like censorship) almost entirely ineffective and collapsed alternatives for information dissemination. Resistance is low (0.02) because, from this deterministic perspective, resistance to the press's effects was largely futile. The claimed type is 'mountain' because this reading frames the technology as an unchangeable, natural-law-like force.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'printing_press_technology' (as an analytical construct representing the deterministic force), its operation is a natural unfolding of its inherent properties, leading to inevitable outcomes. From the 'catholic_church_hierarchy's' perspective, the press was an overwhelming, destructive force that extracted their power and control, leaving them trapped. The engine's classification will highlight this divergence between the 'mountain' claim and the 'snare-like' experience of the Church.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'printing_press_technology' itself is the agenda-setter, dictating the terms of information flow. 'Protestant_reformers' and 'vernacular_literacy_movements' are beneficiaries, as the press's inherent properties directly enabled their goals. The 'catholic_church_hierarchy' is the payer, bearing the cost of its authority being undermined by an unstoppable technological force. The directionality for the technology itself is near 0.0 (full beneficiary) as it 'benefits' from its own inherent capacity to shape the world, while the Church is near 1.0 (full target) as it is acted upon by this force.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by asserting the press's inherent, unchangeable nature as the primary driver. It avoids framing the Reformation as a 'snare' created by reformers, or a 'tangled rope' of strategic interaction, by emphasizing the technology's autonomous causal power. The question of mandatrophy is largely irrelevant here, as the 'mandate' is inherent to the technology's function, which is seen as perpetually 'live' in its causal effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agency_vs_determinism,
    'To what extent was the Reformation an inevitable outcome of printing press technology, versus a result of strategic choices and human agency in deploying the press?',
    'Comparative historical analysis of other regions/times where printing existed but did not lead to similar religious upheaval, or detailed studies of reformer''s explicit strategic use of the press.',
    'If agency is found to be significant, this ''technological determinism'' reading would be reclassified from a Mountain to a Rope or Tangled Rope, reflecting a more active, coordinated, or even extractive role for human actors. The extractiveness metric would rise as human choices are seen to drive outcomes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(agency_vs_determinism, conceptual, 'Ambiguity between technological inevitability and human choice in historical causation.').

omega_variable(
    causal_locus_ambiguity,
    'Is the ''printing_press_technology'' truly an ''agenda_setter'' (an autonomous causal force), or is it a neutral ''tool'' whose effects are entirely dependent on its users?',
    'Philosophical analysis of technological causality and historical evidence of how the press''s capabilities were perceived and utilized by contemporaries.',
    'If the press is re-framed as a neutral tool, its ''agenda_setter'' role would collapse, and the constraint would shift from a Mountain (inherent property) to a Rope or Snare (human-constructed and enforced), with extractiveness and suppression rising as human agency becomes the locus of control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_locus_ambiguity, conceptual, 'Whether technology is an autonomous agent or a neutral tool.').

omega_variable(
    false_summit_of_technology,
    'Is the claim that the printing press ''emerges_naturally'' as an inevitable cause a genuine natural law, or a constructed narrative that benefits those who wish to downplay human responsibility or agency in historical outcomes?',
    'Critical historiography examining the ideological functions of technological determinism as a narrative, and its implications for assigning responsibility for social change.',
    'If found to be a constructed narrative, the constraint would be reclassified from a Mountain to a Tangled Rope or Snare, with beneficiaries (e.g., those who benefit from a simplified, deterministic view of history) becoming more prominent, and extractiveness rising to reflect the ''cost'' of this narrative framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_of_technology, conceptual, 'Whether technological determinism is a natural law or a constructed narrative.').


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
narrative_ontology:measurement(pres_su_t1450, press_reformation_causation__technological_determinism, suppression_requirement, 1450, 0.05).
narrative_ontology:measurement(pres_su_t1475, press_reformation_causation__technological_determinism, suppression_requirement, 1475, 0.04).
narrative_ontology:measurement(pres_su_t1500, press_reformation_causation__technological_determinism, suppression_requirement, 1500, 0.03).
narrative_ontology:measurement(pres_su_t1525, press_reformation_causation__technological_determinism, suppression_requirement, 1525, 0.02).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causation__technological_determinism, suppression_requirement, 1550, 0.01).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__technological_determinism, information_standard).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'press_reformation_causation' kernel. This 'technological_determinism' reading emphasizes the press as an autonomous, mountain-like cause, distinct from readings that focus on strategic deployment or mutual shaping of technology and agency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
