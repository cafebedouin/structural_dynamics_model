% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__oligopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__oligopoly_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: article_27_veto_power__oligopoly_reading
 *   human_readable: UNSC Article 27 Veto Power (Oligopoly Reading)
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This constraint story presents the 'oligopoly reading' of the UN Security
 *   Council's Article 27 veto power. In this reading, the veto is not a
 *   coordination mechanism but a structural entrenchment of geopolitical
 *   oligopoly, allowing the P5 permanent members to extract ongoing authority
 *   rents and block institutional evolution. The Charter's immutability,
 *   particularly regarding the veto, is leveraged to maintain this power
 *   asymmetry. The claimed type is 'snare' because its primary function is
 *   extraction and suppression of alternatives for the non-P5 majority,
 *   despite its original framing as a 'rope' for great power coordination.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, 0.85).
domain_priors:suppression_score(article_27_veto_power__oligopoly_reading, 0.9).
domain_priors:theater_ratio(article_27_veto_power__oligopoly_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__oligopoly_reading, snare).
narrative_ontology:human_readable(article_27_veto_power__oligopoly_reading, "UNSC Article 27 Veto Power (Oligopoly Reading)").
narrative_ontology:topic_domain(article_27_veto_power__oligopoly_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:requires_active_enforcement(article_27_veto_power__oligopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__oligopoly_reading, '921f5918-2062-4f11-8ad7-3407676460ce').
narrative_ontology:cs_kernel_codification('921f5918-2062-4f11-8ad7-3407676460ce', fixed_text).
narrative_ontology:cs_authority_grounding('921f5918-2062-4f11-8ad7-3407676460ce', extraction).
narrative_ontology:cs_interpretation_layer_present('921f5918-2062-4f11-8ad7-3407676460ce').
narrative_ontology:cs_reading_relation('921f5918-2062-4f11-8ad7-3407676460ce', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('921f5918-2062-4f11-8ad7-3407676460ce', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('921f5918-2062-4f11-8ad7-3407676460ce', foundational, un_charter_as_oligopoly_instrument).
narrative_ontology:cs_axiom_status(un_charter_as_oligopoly_instrument, holdable).
narrative_ontology:cs_axiom_grounding('921f5918-2062-4f11-8ad7-3407676460ce', un_charter_as_oligopoly_instrument, conventional).
narrative_ontology:cs_axiom('921f5918-2062-4f11-8ad7-3407676460ce', foundational, geopolitical_power_as_rent_extraction).
narrative_ontology:cs_axiom_status(geopolitical_power_as_rent_extraction, holdable).
narrative_ontology:cs_axiom_grounding('921f5918-2062-4f11-8ad7-3407676460ce', geopolitical_power_as_rent_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('921f5918-2062-4f11-8ad7-3407676460ce', post_wwii_great_power_consensus).
narrative_ontology:cs_drift_state('921f5918-2062-4f11-8ad7-3407676460ce', contemporary_multipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('921f5918-2062-4f11-8ad7-3407676460ce', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__oligopoly_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, p5_permanent_members).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, non_p5_un_member_states).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, global_south_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five permanent members of the UN Security Council (China, France, Russia, United Kingdom, United States) who possess the veto power. They use it to block resolutions that threaten their national interests or those of their allies, effectively entrenching their geopolitical status and preventing reforms that would dilute their power. They benefit from the status quo by maintaining disproportionate influence over global security decisions.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, p5_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).

% The vast majority of UN member states who do not possess the veto. They bear the cost of an unresponsive Security Council, often seeing critical resolutions blocked despite overwhelming support, particularly on issues of human rights, conflict intervention, or institutional reform. Their exit options are limited to symbolic protests or forming alternative, less legitimate, international bodies.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, non_p5_un_member_states, payer,
    organized, biographical, constrained, global).

% A subset of non-P5 states, often disproportionately affected by conflicts or crises that the Security Council fails to address due to vetoes. They are structurally trapped, as their security and development are often contingent on effective international action, which the veto power frequently obstructs. They have no effective means to reform the system from within.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, global_south_nations, payer,
    moderate, generational, trapped, global).

% The administrative body of the UN, tasked with implementing Security Council resolutions. It observes the paralysis caused by vetoes and often advocates for reform, but has no power to alter the Charter or compel P5 members. Its effectiveness is constrained by the veto power.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, un_secretariat, observer,
    institutional, biographical, constrained, global).

% Non-governmental organizations and advocacy groups that push for UN reform and more effective international action. They are excluded from direct decision-making power within the Security Council but exert moral and political pressure, documenting the human cost of veto-induced inaction. Their 'exit' is to shift advocacy to other international fora.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, international_civil_society, excluded,
    moderate, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The veto power, in this reading, does not primarily serve a coordination function but rather entrenches a specific power distribution. Any 'coordination' is among the P5 to maintain their collective oligopoly, not for the broader international community.
% TRANSFER_FUNCTION: Transfers effective decision-making authority and geopolitical influence from the collective UN membership to the P5 permanent members. It also transfers the costs of inaction and institutional paralysis to the non-P5 states, particularly those affected by conflicts.
% ABSENT_VOICES: The voices of the global majority, particularly those from the Global South, are effectively absent from the decision-making process when their proposals for reform or action are vetoed. They are present in the General Assembly but lack the power to compel the Security Council.
% DISAPPEARANCE_RATIONALE: If the veto power disappeared overnight, the Security Council would immediately become more representative and responsive. Resolutions on long-standing conflicts and institutional reforms would likely pass, fundamentally altering global governance and redistributing power away from the P5. The international system would undergo a significant rearrangement.
% FOUNDING_PROBLEM: The veto power was established to prevent the UN from becoming ineffective due to great power disagreements, ensuring that no major power would be compelled into action against its vital interests, thereby preventing a repeat of the League of Nations' failure.
% FOUNDING_PROBLEM_CORROBORATION: While the P5 members still claim the founding problem (preventing great power conflict) is live, the vast majority of non-P5 states, international legal scholars, and civil society organizations attest that the problem has evolved. The veto now primarily serves to protect narrow national interests and block necessary reforms, rather than preventing global war, making the original problem 'dead' in its current application. This is corroborated by numerous UN General Assembly resolutions and academic analyses.
narrative_ontology:disappearance_verdict(article_27_veto_power__oligopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__oligopoly_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__oligopoly_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_27_veto_power__oligopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__oligopoly_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__oligopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_27_veto_power__oligopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the veto allows the P5 to disproportionately shape global security outcomes and protect their interests, often at the expense of the broader international community. Suppression is very high (0.90) because there is no effective legal or political mechanism to bypass or remove the veto without P5 consent, trapping the non-P5 majority. Theater ratio is low (0.20) because the veto's function is quite direct and effective in blocking action, with little performative overhead beyond the rhetoric of 'responsibility'. The increasing extractiveness and suppression over time reflect the growing divergence between the original intent and the current geopolitical reality, where the P5's power has become more entrenched relative to the rest of the world.
 *
 * PERSPECTIVAL GAP:
 *   The P5 members perceive the veto as a necessary safeguard for global stability (a 'rope' or 'mountain' of geopolitical reality), preventing reckless interventions and ensuring great power buy-in. The non-P5 majority, however, experiences it as a 'snare' that traps them in an outdated power structure, blocking essential action and reform. This story explicitly adopts the latter, more critical, perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   The P5 permanent members are clear beneficiaries and agenda-setters (d near 0.0), as the veto directly grants them disproportionate power and protects their interests. Non-P5 UN member states, especially Global South nations, are the primary victims and payers (d near 1.0), bearing the costs of an often-paralyzed Security Council and lacking any effective recourse. The UN Secretariat and international civil society act as observers or excluded parties, documenting the effects but unable to alter the fundamental structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits clear mandatrophy: the founding problem (preventing great power war by ensuring consent) is largely 'dead' in its current application, having been superseded by the veto's use for narrow national interests and blocking reform. The classification as a 'snare' prevents mislabeling this as coordination, highlighting the coercive and extractive nature of its current operation. The persistence is due to the P5's self-interest in maintaining their oligopoly, not a genuine, live coordination need for the broader international system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_status_ambiguity,
    'Is the founding problem (preventing great power war by ensuring consent) truly ''dead'', or does the veto still serve a vital, if imperfect, coordination function in preventing direct conflict among nuclear powers?',
    'Analysis of counterfactual scenarios where the veto was absent: would major power conflicts have escalated without it? This requires historical and geopolitical modeling.',
    'If the founding problem is still live, the constraint might lean more towards a ''tangled_rope'' (coordination with extraction) or even a ''rope'' (pure coordination), reducing its extractiveness. If ''dead'', the ''snare'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_status_ambiguity, conceptual, 'Ambiguity regarding the current relevance of the veto''s original coordination mandate.').

omega_variable(
    reform_path_viability,
    'Are there genuinely viable, albeit difficult, paths to reform the veto power (e.g., through General Assembly pressure, P5 self-restraint, or alternative international bodies) that would reduce the ''suppression'' metric, or is the system truly ''trapped''?',
    'Empirical observation of reform movements'' success or failure over a longer time horizon, and analysis of the political will within P5 states to cede power.',
    'If viable reform paths exist and gain traction, the ''suppression'' metric would decrease, potentially shifting the classification towards a ''tangled_rope'' by demonstrating some agency for the non-P5. If no paths emerge, the ''snare'' classification is solidified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_path_viability, empirical, 'Uncertainty about the true extent of suppression and the possibility of institutional reform.').

omega_variable(
    oligopoly_vs_great_power_responsibility,
    'Is the P5''s exercise of the veto primarily driven by self-interested oligopoly maintenance, or by a genuine, albeit self-defined, sense of ''great power responsibility'' for global stability?',
    'Detailed case studies of vetoed resolutions, analyzing the stated justifications against the actual geopolitical outcomes and the P5''s national interests. This is a conceptual distinction with empirical implications.',
    'If ''responsibility'' is the dominant driver, the ''extractiveness'' might be re-evaluated as a necessary cost of maintaining global order, potentially softening the ''snare'' classification. If ''oligopoly'' dominates, the ''snare'' classification is strongly affirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oligopoly_vs_great_power_responsibility, conceptual, 'Distinguishing between self-serving power maintenance and a perceived duty to global stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__oligopoly_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_27_veto_power__oligopoly_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(arti_tr_t1965, article_27_veto_power__oligopoly_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(arti_tr_t1985, article_27_veto_power__oligopoly_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(arti_tr_t2005, article_27_veto_power__oligopoly_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(arti_tr_t2024, article_27_veto_power__oligopoly_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_27_veto_power__oligopoly_reading, base_extractiveness, 1945, 0.6).
narrative_ontology:measurement(arti_be_t1965, article_27_veto_power__oligopoly_reading, base_extractiveness, 1965, 0.7).
narrative_ontology:measurement(arti_be_t1985, article_27_veto_power__oligopoly_reading, base_extractiveness, 1985, 0.75).
narrative_ontology:measurement(arti_be_t2005, article_27_veto_power__oligopoly_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement(arti_be_t2024, article_27_veto_power__oligopoly_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_27_veto_power__oligopoly_reading, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(arti_su_t1965, article_27_veto_power__oligopoly_reading, suppression_requirement, 1965, 0.75).
narrative_ontology:measurement(arti_su_t1985, article_27_veto_power__oligopoly_reading, suppression_requirement, 1985, 0.8).
narrative_ontology:measurement(arti_su_t2005, article_27_veto_power__oligopoly_reading, suppression_requirement, 2005, 0.85).
narrative_ontology:measurement(arti_su_t2024, article_27_veto_power__oligopoly_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
