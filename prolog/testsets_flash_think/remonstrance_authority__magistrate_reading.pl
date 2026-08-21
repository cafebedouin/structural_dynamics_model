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
 *   constraint_id: remonstrance_authority__magistrate_reading
 *   human_readable: Remonstrance Right (Magistrate Reading)
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   This constraint story analyzes the Remonstrance Right from the
 *   perspective of the Parlements (the 'magistrate reading') in
 *   pre-revolutionary France. From this viewpoint, the right was a
 *   fundamental constitutional mechanism intended to preserve ancient
 *   liberties and prevent arbitrary royal innovation, particularly in fiscal
 *   matters. However, its operation often resulted in the protection of
 *   aristocratic fiscal privileges, leading to high effective extraction from
 *   the Crown and the general populace. This constraint is one reading of the
 *   broader 'remonstrance_authority' kernel, which was deeply contested
 *   between the Crown and the Parlements.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, 0.65).
domain_priors:suppression_score(remonstrance_authority__magistrate_reading, 0.75).
domain_priors:theater_ratio(remonstrance_authority__magistrate_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__magistrate_reading, rope).
narrative_ontology:human_readable(remonstrance_authority__magistrate_reading, "Remonstrance Right (Magistrate Reading)").
narrative_ontology:topic_domain(remonstrance_authority__magistrate_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__magistrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__magistrate_reading, '4fda3f6b-d954-4cde-8abe-c9c34ebeacb1').
narrative_ontology:cs_kernel_codification('4fda3f6b-d954-4cde-8abe-c9c34ebeacb1', formalized).
narrative_ontology:cs_authority_grounding('4fda3f6b-d954-4cde-8abe-c9c34ebeacb1', lineage).
narrative_ontology:cs_interpretation_layer_present('4fda3f6b-d954-4cde-8abe-c9c34ebeacb1').
narrative_ontology:cs_reading_relation('4fda3f6b-d954-4cde-8abe-c9c34ebeacb1', remonstrance_authority__crown_reading, coexists_with).
narrative_ontology:cs_axiom('4fda3f6b-d954-4cde-8abe-c9c34ebeacb1', foundational, ancient_liberties_immutable).
narrative_ontology:cs_axiom_status(ancient_liberties_immutable, holdable).
narrative_ontology:cs_axiom_grounding('4fda3f6b-d954-4cde-8abe-c9c34ebeacb1', ancient_liberties_immutable, deontological).
narrative_ontology:cs_axiom('4fda3f6b-d954-4cde-8abe-c9c34ebeacb1', foundational, parlementary_registration_essential).
narrative_ontology:cs_axiom_status(parlementary_registration_essential, holdable).
narrative_ontology:cs_axiom_grounding('4fda3f6b-d954-4cde-8abe-c9c34ebeacb1', parlementary_registration_essential, conventional).
narrative_ontology:cs_reference_frame('4fda3f6b-d954-4cde-8abe-c9c34ebeacb1', traditional_constitutional_balance).
narrative_ontology:cs_drift_state('4fda3f6b-d954-4cde-8abe-c9c34ebeacb1', pre_french_revolution_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('4fda3f6b-d954-4cde-8abe-c9c34ebeacb1', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__magistrate_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, parlements_magistrates).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, french_crown).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, general_populace).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, royal_ministers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The members of the sovereign courts (Parlements) who asserted the right of remonstrance. They saw themselves as guardians of fundamental laws and ancient liberties, using the right to block royal edicts, particularly those impacting their fiscal privileges. Their identity and authority were deeply tied to this mechanism.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, parlements_magistrates, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, parlements_magistrates, beneficiary).

% The monarch and royal government, whose legislative and fiscal initiatives were frequently obstructed by the Parlements' remonstrances. They viewed the right as an illegitimate obstruction to necessary reforms and royal authority, bearing the political and financial costs of blocked legislation.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, french_crown, payer,
    institutional, biographical, constrained, national).

% The broader population, whose tax burdens and economic conditions were indirectly affected by the Parlements' defense of aristocratic fiscal privileges. They bore the costs of a system that often protected the wealthy from taxation, leading to resentment and instability.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, general_populace, payer,
    powerless, immediate, trapped, national).

% The King's advisors and administrators, responsible for drafting and implementing royal policy. They faced direct opposition from the Parlements and bore the political consequences of failed reforms, often resorting to 'lits de justice' to force registration.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, royal_ministers, agenda_setter,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, royal_ministers, payer).

% Analyze the historical role and impact of the remonstrance right, often debating whether it genuinely served as a constitutional check or primarily protected particularist interests. They provide an external, analytical perspective on the constraint's operation.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, historians_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__magistrate_reading, parlements_magistrates).
narrative_ontology:fixing_cost_class(remonstrance_authority__magistrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for the Parlements to review, comment on, and provisionally block royal edicts, ensuring they conform to existing laws, customs, and fundamental principles, thereby maintaining a perceived constitutional balance.
% TRANSFER_FUNCTION: Transfers effective legislative veto power from the Crown to the Parlements, and shifts fiscal burdens away from the privileged magistracy onto other segments of society by blocking tax reforms.
% ABSENT_VOICES: The unrepresented commoners and the Third Estate, whose interests were often not directly served by the Parlements' defense of 'ancient liberties' (which frequently meant aristocratic privileges). They would have advocated for more equitable taxation and broader representation.
% DISAPPEARANCE_RATIONALE: If the remonstrance right vanished overnight, the balance of power between the French Crown and its judiciary would fundamentally shift. The Crown would gain unchecked legislative authority, likely leading to more rapid and potentially arbitrary reforms, while the Parlements would lose their primary means of influence, fundamentally altering the constitutional landscape of pre-revolutionary France.
% FOUNDING_PROBLEM: To prevent arbitrary royal decrees and ensure the preservation of fundamental laws, customs, and privileges, particularly those related to taxation and property rights, against perceived royal innovation or absolutism.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars acknowledge the historical role of remonstrances in limiting royal power and asserting a form of constitutionalism. However, they contest whether it genuinely served the public good or primarily protected the vested interests and fiscal exemptions of the magistracy, citing contemporary critiques from royal ministers and later revolutionary figures.
narrative_ontology:disappearance_verdict(remonstrance_authority__magistrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__magistrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__magistrate_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The claimed type is 'rope' because the Parlements genuinely believed the remonstrance right served a vital coordination function in maintaining constitutional balance and protecting fundamental laws. However, the metrics reflect the reality of its operation: high extractiveness (0.65 rising to 0.75) due to the fiscal exemptions it secured for the magistracy at the expense of the Crown and other taxpayers; high suppression (0.75 rising to 0.85) as the Crown increasingly tried to override or abolish the right, and the Parlements actively resisted; and a rising theater ratio (0.4 rising to 0.45) as confrontations became more public and performative, often serving political posturing as much as genuine legal review. The increasing values over the interval reflect the escalating conflict between the Crown and Parlements leading up to the French Revolution.
 *
 * PERSPECTIVAL GAP:
 *   The magistrate's perspective (claimed 'rope') emphasizes the coordination function of constitutional balance and liberty protection. The Crown's perspective (the 'crown_reading' sibling constraint) would likely view the same mechanism as a 'snare' or 'tangled_rope' – an illegitimate obstruction to necessary governance, driven by particularist interests. The engine's computation of a more extractive type from the authored metrics, despite the 'rope' claim, captures this fundamental perspectival divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Parlements' magistrates are the primary beneficiaries, as the right protected their privileges and enhanced their institutional power. The French Crown and the general populace are the victims, bearing the costs of blocked reforms and inequitable taxation. Royal ministers, while part of the Crown's agenda-setting, also bear the political costs of the constraint's operation. The 'identity_locked' exit option for the magistrates reflects how their professional and social standing was intrinsically tied to their role as guardians of this right.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liberty_vs_privilege_ambiguity,
    'To what extent did the remonstrance right genuinely preserve ''ancient liberties'' for the broader populace, versus primarily protecting the specific fiscal and social privileges of the magistracy?',
    'Detailed historical analysis of the specific edicts blocked and their impact on different social classes, coupled with contemporary public opinion and economic data.',
    'If primarily protecting privilege, the constraint''s effective extractiveness is higher and its coordination function weaker, pushing it closer to a Snare. If genuinely protecting broader liberties, its coordination function is stronger, supporting the Rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liberty_vs_privilege_ambiguity, empirical, 'Ambiguity regarding the true beneficiaries of the remonstrance right''s operation.').

omega_variable(
    constitutional_vs_obstructionist_framing,
    'Is the remonstrance right a legitimate constitutional check on power, or an obstructionist tool used by a privileged minority to resist necessary state reforms?',
    'Comparative constitutional analysis of similar historical mechanisms and their long-term societal outcomes, alongside a re-evaluation of the Crown''s proposed reforms and their potential benefits.',
    'If a legitimate check, the ''rope'' claim is strengthened. If primarily obstructionist, the constraint''s suppression of royal initiatives is seen as illegitimate, increasing its effective extractiveness from the Crown''s perspective and pushing it towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_vs_obstructionist_framing, conceptual, 'Conceptual framing of the remonstrance right as either constitutional or obstructionist.').

omega_variable(
    crown_reading_divergence,
    'How would the ''crown_reading'' of the remonstrance authority kernel structurally differ from this ''magistrate_reading''?',
    'Analysis of the ''remonstrance_authority__crown_reading'' constraint story, specifically its base properties, stakeholders, and axioms.',
    'The ''crown_reading'' would likely show lower extractiveness from the Crown''s perspective (as it views the remonstrance as a cost, not a benefit), higher suppression (as it views the remonstrance as an illegitimate imposition), and a claimed type closer to Snare or Tangled Rope, reflecting its view of the mechanism as an illegitimate veto.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(crown_reading_divergence, conceptual, 'Structural differences between the magistrate and crown readings of the remonstrance authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__magistrate_reading, 1715, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t1715, remonstrance_authority__magistrate_reading, theater_ratio, 1715, 0.2).
narrative_ontology:measurement(remo_tr_t1730, remonstrance_authority__magistrate_reading, theater_ratio, 1730, 0.25).
narrative_ontology:measurement(remo_tr_t1745, remonstrance_authority__magistrate_reading, theater_ratio, 1745, 0.3).
narrative_ontology:measurement(remo_tr_t1760, remonstrance_authority__magistrate_reading, theater_ratio, 1760, 0.35).
narrative_ontology:measurement(remo_tr_t1775, remonstrance_authority__magistrate_reading, theater_ratio, 1775, 0.4).
narrative_ontology:measurement(remo_tr_t1789, remonstrance_authority__magistrate_reading, theater_ratio, 1789, 0.45).

% Extraction over time
narrative_ontology:measurement(remo_be_t1715, remonstrance_authority__magistrate_reading, base_extractiveness, 1715, 0.55).
narrative_ontology:measurement(remo_be_t1730, remonstrance_authority__magistrate_reading, base_extractiveness, 1730, 0.6).
narrative_ontology:measurement(remo_be_t1745, remonstrance_authority__magistrate_reading, base_extractiveness, 1745, 0.65).
narrative_ontology:measurement(remo_be_t1760, remonstrance_authority__magistrate_reading, base_extractiveness, 1760, 0.7).
narrative_ontology:measurement(remo_be_t1775, remonstrance_authority__magistrate_reading, base_extractiveness, 1775, 0.72).
narrative_ontology:measurement(remo_be_t1789, remonstrance_authority__magistrate_reading, base_extractiveness, 1789, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t1715, remonstrance_authority__magistrate_reading, suppression_requirement, 1715, 0.6).
narrative_ontology:measurement(remo_su_t1730, remonstrance_authority__magistrate_reading, suppression_requirement, 1730, 0.65).
narrative_ontology:measurement(remo_su_t1745, remonstrance_authority__magistrate_reading, suppression_requirement, 1745, 0.7).
narrative_ontology:measurement(remo_su_t1760, remonstrance_authority__magistrate_reading, suppression_requirement, 1760, 0.75).
narrative_ontology:measurement(remo_su_t1775, remonstrance_authority__magistrate_reading, suppression_requirement, 1775, 0.8).
narrative_ontology:measurement(remo_su_t1789, remonstrance_authority__magistrate_reading, suppression_requirement, 1789, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__magistrate_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, royal_fiscal_policy).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, absolute_monarchy_doctrine).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, remonstrance_authority__crown_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'magistrate_reading' of the 'remonstrance_authority' kernel, which also includes the 'crown_reading'. These two readings represent fundamentally opposed interpretations of the same constitutional mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
