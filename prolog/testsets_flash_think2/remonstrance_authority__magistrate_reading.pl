% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__magistrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__magistrate_reading, []).

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
 *   constraint_id: remonstrance_authority__magistrate_reading
 *   human_readable: Remonstrance Right (Magistrate Reading)
 *   domain: Constitutional History / Political Economy / Legal Authority
 *
 * SUMMARY:
 *   The remonstrance right, from the perspective of the Parlements
 *   (magistrate reading), was a fundamental constitutional mechanism in
 *   pre-revolutionary France. It allowed sovereign courts to refuse to
 *   register royal edicts they deemed contrary to fundamental laws, customs,
 *   or privileges, thereby delaying or blocking their implementation. This
 *   reading emphasizes its role in preserving ancient liberties against
 *   arbitrary royal innovation, particularly in fiscal matters.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, 0.65).
domain_priors:suppression_score(remonstrance_authority__magistrate_reading, 0.7).
domain_priors:theater_ratio(remonstrance_authority__magistrate_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__magistrate_reading, tangled_rope).
narrative_ontology:human_readable(remonstrance_authority__magistrate_reading, "Remonstrance Right (Magistrate Reading)").
narrative_ontology:topic_domain(remonstrance_authority__magistrate_reading, "Constitutional History / Political Economy / Legal Authority").

domain_priors:requires_active_enforcement(remonstrance_authority__magistrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__magistrate_reading, '30943578-2d7e-42c5-8d15-fe54bea7d726').
narrative_ontology:cs_kernel_codification('30943578-2d7e-42c5-8d15-fe54bea7d726', formalized).
narrative_ontology:cs_authority_grounding('30943578-2d7e-42c5-8d15-fe54bea7d726', lineage).
narrative_ontology:cs_interpretation_layer_present('30943578-2d7e-42c5-8d15-fe54bea7d726').
narrative_ontology:cs_reading_relation('30943578-2d7e-42c5-8d15-fe54bea7d726', remonstrance_authority__crown_reading, coexists_with).
narrative_ontology:cs_axiom('30943578-2d7e-42c5-8d15-fe54bea7d726', foundational, ancient_liberties_immutable).
narrative_ontology:cs_axiom_status(ancient_liberties_immutable, holdable).
narrative_ontology:cs_axiom_grounding('30943578-2d7e-42c5-8d15-fe54bea7d726', ancient_liberties_immutable, deontological).
narrative_ontology:cs_axiom('30943578-2d7e-42c5-8d15-fe54bea7d726', foundational, royal_power_limited_by_law).
narrative_ontology:cs_axiom_status(royal_power_limited_by_law, holdable).
narrative_ontology:cs_axiom_grounding('30943578-2d7e-42c5-8d15-fe54bea7d726', royal_power_limited_by_law, conventional).
narrative_ontology:cs_reference_frame('30943578-2d7e-42c5-8d15-fe54bea7d726', constitutional_monarchy_by_custom).
narrative_ontology:cs_drift_state('30943578-2d7e-42c5-8d15-fe54bea7d726', late_ancien_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('30943578-2d7e-42c5-8d15-fe54bea7d726', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__magistrate_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, parlements_magistrates).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, crown_fiscal_policy).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, french_populace).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, french_populace).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, royal_ministers).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, french_crown).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise the right of remonstrance, interpreting fundamental laws and customs to resist royal edicts. They directly benefit from the preservation of their tax exemptions and privileges, but face royal pressure and potential exile if they resist too strongly.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, parlements_magistrates, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, parlements_magistrates, beneficiary).

% Issues edicts to raise revenue and assert absolute authority. The Crown's fiscal policies are thwarted by successful remonstrances, forcing it to seek alternative, often less efficient, means of funding. It actively seeks to suppress or bypass the remonstrance right.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, french_crown, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, french_crown, agenda_setter).

% Indirectly bear the costs of royal fiscal policies when remonstrances fail, as new taxes are imposed. They are indirect beneficiaries when remonstrances succeed in blocking burdensome edicts, but have no direct voice in the process.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, french_populace, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, french_populace, beneficiary).

% Advise the Crown on policy and strategy, including how to overcome parliamentary resistance. They benefit from the successful implementation of royal policy and the maintenance of royal authority.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, royal_ministers, agenda_setter,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, royal_ministers, beneficiary).

% Analyze the historical and legal significance of the remonstrance right, its impact on constitutional development, and the power dynamics between the Crown and the Parlements.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, historians_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a formal legal mechanism for the Parlements to review, register, and potentially challenge royal edicts, thereby coordinating royal legislative authority with the established legal and customary framework of the kingdom.
% TRANSFER_FUNCTION: Secures the tax exemptions and privileges of the magistracy by preventing the Crown from imposing certain fiscal edicts, effectively transferring (or preventing the transfer of) revenue from the Crown's treasury to the privileged classes, and ultimately to the broader populace if new taxes are blocked.
% ABSENT_VOICES: The broader French populace, who bore the ultimate burden of taxation and had no direct representation in the Parlements, would advocate for more equitable fiscal policies and a more direct voice in governance.
% DISAPPEARANCE_RATIONALE: If the remonstrance right had vanished overnight, the French Crown would have faced significantly fewer legal checks on its fiscal and legislative authority, likely leading to more arbitrary and rapid policy implementation, potentially accelerating the path to revolution or consolidating absolute monarchy more effectively.
% FOUNDING_PROBLEM: To ensure that royal edicts conformed to the fundamental laws, customs, and privileges of the kingdom, preventing arbitrary rule and safeguarding the established legal order against royal innovation.
% FOUNDING_PROBLEM_CORROBORATION: Legal treatises by jurists of the era, historical records of parliamentary debates, and the long-standing tradition of customary law attest to the perceived necessity of this check on royal power. The Crown, however, consistently disputed its legitimacy and necessity.
narrative_ontology:disappearance_verdict(remonstrance_authority__magistrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__magistrate_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__magistrate_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(remonstrance_authority__magistrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__magistrate_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__magistrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__magistrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is set at 0.65 (and rising) because, while presented as a defense of liberties, the successful exercise of the right directly secured the tax exemptions and privileges of the magistracy and other privileged classes, effectively extracting from the Crown's fiscal capacity. `Suppression` is high (0.7) due to the Crown's consistent efforts to overcome remonstrances through 'lits de justice' and exile. `Theater_ratio` is moderate (0.4) as the process involved significant ritual and public performance, but also had genuine legal and political stakes. `Accessibility_collapse` is high (0.8) as the remonstrance was one of the few formal, institutionalized checks on royal power. `Resistance` is high (0.75) reflecting the Crown's active opposition to the Parlements' assertions.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown's perspective (the sibling reading), the remonstrance right was an illegitimate minoritarian veto protecting particularist privileges, an obstacle to necessary reforms and national unity. This magistrate reading, however, frames it as a vital constitutional safeguard. The engine's per-seat classification will highlight this divergence, showing the right as a defense of liberty from the magistrate's seat, but as an extractive impediment from the Crown's.
 *
 * DIRECTIONALITY LOGIC:
 *   The `parlements_magistrates` are beneficiaries as the right preserves their privileges and tax exemptions, and agenda-setters as they actively wield it. The `french_crown` and its `fiscal_policy` are the primary targets/victims, as their revenue-raising efforts are directly thwarted. The `french_populace` are indirect victims when the right fails to block burdensome taxes, but also indirect beneficiaries when it succeeds. Royal ministers benefit from the Crown's success.
 *
 * MANDATROPHY ANALYSIS:
 *   From the magistrate's perspective, the founding problem of preventing arbitrary royal innovation remained live throughout the period. However, the right's operation increasingly became intertwined with the defense of specific aristocratic privileges, leading to accusations that its function had drifted from universal liberty to particularist extraction. The rising extractiveness and theater ratio in the measurements reflect this drift, even as the claimed type remains 'tangled_rope' due to its dual coordination and extractive functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_vs_privilege,
    'To what extent did the remonstrance right genuinely uphold fundamental laws for the common good, versus primarily serving to protect the specific privileges and tax exemptions of the magistracy and other elites?',
    'Comparative historical analysis of the content of remonstrances over time, examining whose interests were consistently defended, and the outcomes for different social classes.',
    'If primarily a defense of privilege, the constraint''s effective extractiveness is higher and its coordination function more tenuous, pushing it closer to a Snare. If genuinely upholding broader legal principles, its Rope-like qualities are stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_vs_privilege, conceptual, 'Ambiguity between universal constitutional defense and particularist aristocratic privilege.').

omega_variable(
    effectiveness_vs_delay,
    'How often did remonstrances lead to genuine changes in royal policy, versus merely delaying or ritualizing the implementation of edicts that were eventually enforced?',
    'Quantitative historical analysis of royal edicts and remonstrances, tracking policy outcomes and the frequency of ''lits de justice'' (royal overrides).',
    'If remonstrances were mostly performative delays, the constraint''s theater_ratio is higher and its actual suppressive power lower, potentially pushing it towards a Piton. If they frequently forced policy changes, its Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_vs_delay, empirical, 'The actual impact of remonstrances on royal policy versus their symbolic role.').

omega_variable(
    crown_suppression_impact,
    'Did the Crown''s suppression of remonstrances (e.g., through exile of magistrates) effectively break resistance, or did it merely intensify opposition and delegitimize royal authority?',
    'Sociological and political analysis of public opinion and elite cohesion following acts of royal suppression, including the long-term political consequences.',
    'If suppression consistently backfired, the constraint''s effective suppression is lower than measured, and its resistance higher, indicating a more volatile and contested Tangled Rope. If suppression was effective, the constraint''s persistence is more directly tied to coercion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(crown_suppression_impact, empirical, 'The true impact of royal suppression on the remonstrance right.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__magistrate_reading, 1750, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t0, remonstrance_authority__magistrate_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(remo_tr_t7, remonstrance_authority__magistrate_reading, theater_ratio, 7, 0.35).
narrative_ontology:measurement(remo_tr_t14, remonstrance_authority__magistrate_reading, theater_ratio, 14, 0.4).
narrative_ontology:measurement(remo_tr_t21, remonstrance_authority__magistrate_reading, theater_ratio, 21, 0.45).
narrative_ontology:measurement(remo_tr_t28, remonstrance_authority__magistrate_reading, theater_ratio, 28, 0.5).
narrative_ontology:measurement(remo_tr_t39, remonstrance_authority__magistrate_reading, theater_ratio, 39, 0.55).

% Extraction over time
narrative_ontology:measurement(remo_be_t0, remonstrance_authority__magistrate_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(remo_be_t7, remonstrance_authority__magistrate_reading, base_extractiveness, 7, 0.65).
narrative_ontology:measurement(remo_be_t14, remonstrance_authority__magistrate_reading, base_extractiveness, 14, 0.7).
narrative_ontology:measurement(remo_be_t21, remonstrance_authority__magistrate_reading, base_extractiveness, 21, 0.75).
narrative_ontology:measurement(remo_be_t28, remonstrance_authority__magistrate_reading, base_extractiveness, 28, 0.8).
narrative_ontology:measurement(remo_be_t39, remonstrance_authority__magistrate_reading, base_extractiveness, 39, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t0, remonstrance_authority__magistrate_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(remo_su_t7, remonstrance_authority__magistrate_reading, suppression_requirement, 7, 0.65).
narrative_ontology:measurement(remo_su_t14, remonstrance_authority__magistrate_reading, suppression_requirement, 14, 0.7).
narrative_ontology:measurement(remo_su_t21, remonstrance_authority__magistrate_reading, suppression_requirement, 21, 0.75).
narrative_ontology:measurement(remo_su_t28, remonstrance_authority__magistrate_reading, suppression_requirement, 28, 0.8).
narrative_ontology:measurement(remo_su_t39, remonstrance_authority__magistrate_reading, suppression_requirement, 39, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__magistrate_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'remonstrance_authority' kernel. It represents the 'magistrate_reading', which emphasizes the right as a constitutional check. The sibling 'crown_reading' (remonstrance_authority__crown_reading) presents a contrasting view of the right as an illegitimate veto.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
