% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanbali_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanbali_reading
 *   human_readable: Hanbali Usul al-Fiqh Method: Textual Restrictiveness
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   This constraint describes the Hanbali school's methodology within Islamic
 *   jurisprudence (Usul al-Fiqh), characterized by maximal reliance on
 *   textual sources (Quran and authenticated Hadith), minimal use of
 *   analogical reasoning (qiyas), preference for weak Hadith over qiyas, and
 *   the principle of sadd al-dhara'i (blocking the means to evil/innovation).
 *   It is one reading of the broader 'usul_al_fiqh_method' kernel. The
 *   Hanbali reading is CLAIMED as a Tangled Rope, reflecting its dual
 *   function of coordinating textual fidelity while extracting from
 *   alternative, more flexible legal development.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, 0.78).
domain_priors:suppression_score(usul_al_fiqh_method__hanbali_reading, 0.85).
domain_priors:theater_ratio(usul_al_fiqh_method__hanbali_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanbali_reading, "Hanbali Usul al-Fiqh Method: Textual Restrictiveness").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanbali_reading, "islamic_jurisprudence/legal_theory/comparative_law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanbali_reading, '90193acb-8a43-4606-aebf-25fabe224ef5').
narrative_ontology:cs_kernel_codification('90193acb-8a43-4606-aebf-25fabe224ef5', fixed_text).
narrative_ontology:cs_authority_grounding('90193acb-8a43-4606-aebf-25fabe224ef5', lineage).
narrative_ontology:cs_interpretation_layer_present('90193acb-8a43-4606-aebf-25fabe224ef5').
narrative_ontology:cs_reading_relation('90193acb-8a43-4606-aebf-25fabe224ef5', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('90193acb-8a43-4606-aebf-25fabe224ef5', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('90193acb-8a43-4606-aebf-25fabe224ef5', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('90193acb-8a43-4606-aebf-25fabe224ef5', foundational, textual_primacy_over_reason).
narrative_ontology:cs_axiom_status(textual_primacy_over_reason, holdable).
narrative_ontology:cs_axiom_grounding('90193acb-8a43-4606-aebf-25fabe224ef5', textual_primacy_over_reason, deontological).
narrative_ontology:cs_axiom('90193acb-8a43-4606-aebf-25fabe224ef5', foundational, sadd_al_dhara_i_as_legal_principle).
narrative_ontology:cs_axiom_status(sadd_al_dhara_i_as_legal_principle, holdable).
narrative_ontology:cs_axiom_grounding('90193acb-8a43-4606-aebf-25fabe224ef5', sadd_al_dhara_i_as_legal_principle, conventional).
narrative_ontology:cs_reference_frame('90193acb-8a43-4606-aebf-25fabe224ef5', early_salaf_practice).
narrative_ontology:cs_drift_state('90193acb-8a43-4606-aebf-25fabe224ef5', contemporary_islamic_revival, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('90193acb-8a43-4606-aebf-25fabe224ef5', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, hanbali_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, textualist_jurists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, conservative_religious_authorities).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, legal_reformers).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, local_customary_practices).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, lay_muslims).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, lay_muslims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary proponents and enforcers of this methodology, they define its parameters and apply it to derive legal rulings. Their professional identity and authority are deeply tied to its preservation and propagation.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, hanbali_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Jurists who align with the Hanbali emphasis on strict textual adherence find their interpretive approach validated and empowered by this methodology. They benefit from the clarity and perceived purity it offers.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, textualist_jurists, beneficiary,
    powerful, biographical, constrained, global).

% Religious and social authorities who seek to maintain traditional norms and prevent perceived innovations (bid'a) benefit from the Hanbali method's restrictive nature, using it to justify their positions and maintain social order.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, conservative_religious_authorities, beneficiary,
    institutional, generational, constrained, regional).

% Jurists from other schools (e.g., Hanafi) who advocate for more expansive use of qiyas (analogical reasoning) or ra'y (reasoned opinion) find their methodologies minimized or rejected by the Hanbali approach, bearing the cost of its dominance in certain contexts.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, rationalist_jurists, payer,
    moderate, biographical, constrained, global).

% Scholars and activists seeking to adapt Islamic law to modern social, economic, and political realities face significant hurdles from the Hanbali method's strictures, which limit the scope for reinterpretation or new legal development.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, legal_reformers, payer,
    powerless, generational, identity_locked, national).

% Indigenous or local customs ('urf) that might otherwise inform legal rulings are often overridden or deemed invalid if they lack direct textual support or contradict strict textual interpretations, leading to their suppression.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, local_customary_practices, excluded,
    powerless, generational, trapped, local).

% Many lay Muslims benefit from the perceived stability, clarity, and authenticity of a legal system rooted strictly in foundational texts. However, they may also bear the cost of its inflexibility in addressing contemporary issues or adapting to diverse cultural contexts.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, lay_muslims, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanbali_reading, lay_muslims, payer).

% Academics who study Islamic legal theory from an external, comparative perspective, analyzing its historical development, internal logic, and interaction with other legal systems. They do not directly participate in its enforcement or bear its costs.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, comparative_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a highly structured and text-centric methodology for deriving Islamic law, ensuring fidelity to the Quran and authenticated Hadith, and preventing perceived innovations (bid'a) by minimizing reliance on analogical reasoning (qiyas) and other non-textual sources.
% TRANSFER_FUNCTION: Transfers interpretive authority from individual juristic reasoning (ra'y, istihsan) and local custom ('urf) to a strict hierarchy of textual sources, concentrating legal power in those who master and apply this textualist methodology, often at the expense of contextual adaptation.
% ABSENT_VOICES: Jurists advocating for greater flexibility, contextual interpretation, or the incorporation of broader public interest (maslaha mursala) or local custom ('urf) are structurally marginalized or excluded from the primary interpretive discourse, as their methodologies are deemed secondary or invalid.
% DISAPPEARANCE_RATIONALE: If the Hanbali method's strictures and enforcement vanished, the legal landscape in regions where it is influential would undergo significant reorganization. Other schools' methodologies would gain prominence, leading to a more diverse and potentially more flexible approach to Islamic law, but also a period of interpretive flux and contestation over new legal derivations.
% FOUNDING_PROBLEM: To preserve the purity and authenticity of Islamic law from perceived innovations and deviations, ensuring strict fidelity to the Quran and Sunnah as understood by the earliest generations (Salaf), and to counter the perceived excesses of rationalist legal schools.
% FOUNDING_PROBLEM_CORROBORATION: Hanbali scholars and their adherents consistently attest that the founding problem of preserving textual purity and preventing bid'a remains live and urgent in contemporary contexts. However, jurists from other schools and external observers often view the problem as either overstated or as having been addressed by more flexible means, suggesting limited corroboration from outside the benefiting parties.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanbali_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanbali_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(usul_al_fiqh_method__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanbali_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the strict textualism and minimization of other sources effectively 'extracts' the possibility of legal development and contextual adaptation from jurists and communities seeking it. Suppression is very high (0.85) due to the active rejection and marginalization of alternative interpretive methodologies (e.g., expansive qiyas, istihsan, maslaha mursala) and the explicit blocking of innovations (sadd al-dhara'i). Theater ratio is moderate (0.25) as the methodology is genuinely applied, but some aspects of its defense against other schools or modern challenges might involve performative reaffirmation of strictures. Accessibility collapse is high (0.88) as, within this framework, alternative interpretive paths are largely foreclosed. Resistance is also high (0.70) from other schools and reformers who contest its restrictiveness.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of the Hanbali method perceive it as a pure Rope, a necessary coordination mechanism for preserving divine law. However, from the perspective of rationalist jurists or legal reformers, it operates as a Snare or Tangled Rope, extracting flexibility and suppressing alternative legal development under the guise of textual fidelity. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanbali scholars and conservative religious authorities are clear beneficiaries, as the method validates their interpretive authority and helps maintain traditional order. Textualist jurists also benefit from its clarity. Rationalist jurists, legal reformers, and local customary practices are victims, as their approaches are suppressed or overridden. Lay Muslims are both beneficiaries (perceived authenticity, stability) and payers (inflexibility in modern life).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_primacy_vs_interpretive_choice,
    'Is the Hanbali method''s maximal textual restrictiveness a direct, unavoidable consequence of divine intent, or is it a specific human interpretive choice among other valid possibilities?',
    'Comparative theological and jurisprudential analysis across diverse Islamic traditions, examining the historical and philosophical arguments for and against the absolute primacy of literal textual interpretation over other forms of reasoning.',
    'If primarily a human interpretive choice, the constraint''s ''emerges_naturally'' aspect (if claimed) would be undermined, and its extractiveness would be re-evaluated as a product of human agency rather than divine necessity, potentially shifting its classification towards a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_primacy_vs_interpretive_choice, conceptual, 'Ambiguity between divine mandate and human interpretation in legal methodology.').

omega_variable(
    sadd_al_dhara_i_scope,
    'Is the principle of sadd al-dhara''i (blocking the means to evil/innovation) applied as a necessary safeguard against genuine corruption, or is its scope expanded to stifle legitimate legal and social development?',
    'Empirical analysis of specific legal rulings derived from sadd al-dhara''i: assessing whether the ''evil'' being blocked is universally recognized or a contested interpretation, and whether the blocked ''means'' genuinely lead to harm or merely to change.',
    'If over-expanded, the suppression metric would be seen as higher than justified by genuine coordination, amplifying effective extraction and pushing the classification closer to a Snare. If strictly applied to clear harms, its coordination function would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sadd_al_dhara_i_scope, empirical, 'Scope and justification of blocking innovations.').

omega_variable(
    modern_challenges_impact,
    'To what extent has the Hanbali method adapted or resisted adaptation to modern challenges (e.g., human rights, global finance, bioethics), and what is the practical impact on its adherents?',
    'Sociological and legal studies examining the lived experience of Hanbali adherents in diverse contemporary societies, documenting instances of legal innovation, quiet adaptation, or persistent inflexibility in practice.',
    'If significant quiet adaptation occurs, the ''accessibility_collapse'' and ''suppression'' metrics might be lower in practice than in theory, suggesting a more flexible (though unacknowledged) operation. If resistance leads to significant hardship for adherents, the extractiveness is amplified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_challenges_impact, empirical, 'Adaptation vs. resistance to modern challenges.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanbali_reading, 850, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t850, usul_al_fiqh_method__hanbali_reading, theater_ratio, 850, 0.1).
narrative_ontology:measurement(usul_tr_t950, usul_al_fiqh_method__hanbali_reading, theater_ratio, 950, 0.15).
narrative_ontology:measurement(usul_tr_t1150, usul_al_fiqh_method__hanbali_reading, theater_ratio, 1150, 0.2).
narrative_ontology:measurement(usul_tr_t1450, usul_al_fiqh_method__hanbali_reading, theater_ratio, 1450, 0.22).
narrative_ontology:measurement(usul_tr_t1750, usul_al_fiqh_method__hanbali_reading, theater_ratio, 1750, 0.24).
narrative_ontology:measurement(usul_tr_t2020, usul_al_fiqh_method__hanbali_reading, theater_ratio, 2020, 0.25).

% Extraction over time
narrative_ontology:measurement(usul_be_t850, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 850, 0.6).
narrative_ontology:measurement(usul_be_t950, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 950, 0.65).
narrative_ontology:measurement(usul_be_t1150, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 1150, 0.7).
narrative_ontology:measurement(usul_be_t1450, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 1450, 0.73).
narrative_ontology:measurement(usul_be_t1750, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 1750, 0.75).
narrative_ontology:measurement(usul_be_t2020, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 2020, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t850, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 850, 0.7).
narrative_ontology:measurement(usul_su_t950, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 950, 0.75).
narrative_ontology:measurement(usul_su_t1150, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 1150, 0.8).
narrative_ontology:measurement(usul_su_t1450, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 1450, 0.82).
narrative_ontology:measurement(usul_su_t1750, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 1750, 0.84).
narrative_ontology:measurement(usul_su_t2020, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 2020, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanbali_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__shafii_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'usul_al_fiqh_method' kernel, which describes the foundational methodologies for deriving Islamic law. This Hanbali reading emphasizes textual restrictiveness, while other schools (Hanafi, Maliki, Shafii) offer more expansive or context-sensitive approaches. Each reading is modeled as a distinct constraint due to significant differences in their structural properties and ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
