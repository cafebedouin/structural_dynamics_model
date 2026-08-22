% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__absolutist_reading, []).

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
 *   constraint_id: speech_harm_boundary__absolutist_reading
 *   human_readable: Absolutist Speech Protection (High Harm Threshold)
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This constraint represents an absolutist reading of speech protection,
 *   where the threshold for overriding speech rights due to harm is extremely
 *   high. It prioritizes speaker autonomy and a broad 'marketplace of ideas'
 *   over the protection of individuals or groups from the harms of speech.
 *   The constraint is claimed as a 'rope' by its proponents (a coordination
 *   mechanism for open discourse), but its high extractiveness from targets
 *   of harmful speech and active enforcement of a narrow unprotected category
 *   leads to a 'snare' classification from the perspective of those bearing
 *   the costs. The metrics reflect the actual operation, not the claimed
 *   ideal.
 *
 * KEY AGENTS:
 *   - speakers_of_controversial_speech: Primary beneficiary (mobile exit) — benefits from broad protection.
 *   - targets_of_harmful_speech: Primary payer (trapped exit) — bears the costs of unprotected harm.
 *   - free_speech_advocates: Secondary beneficiary (analytical exit) — defends the broad interpretation.
 *   - vulnerable_groups: Secondary payer (identity_locked exit) — disproportionately affected by harm.
 *   - courts_and_judiciary: Agenda setter (constrained exit) — interprets and enforces the high harm threshold.
 *   - legislators: Agenda setter (constrained exit) — limited in ability to regulate harmful speech.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, 0.85).
domain_priors:suppression_score(speech_harm_boundary__absolutist_reading, 0.1).
domain_priors:theater_ratio(speech_harm_boundary__absolutist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__absolutist_reading, snare).
narrative_ontology:human_readable(speech_harm_boundary__absolutist_reading, "Absolutist Speech Protection (High Harm Threshold)").
narrative_ontology:topic_domain(speech_harm_boundary__absolutist_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__absolutist_reading, 'd07cd727-19e8-4a52-8c28-919a04a4c841').
narrative_ontology:cs_kernel_codification('d07cd727-19e8-4a52-8c28-919a04a4c841', fixed_text).
narrative_ontology:cs_authority_grounding('d07cd727-19e8-4a52-8c28-919a04a4c841', lineage).
narrative_ontology:cs_interpretation_layer_present('d07cd727-19e8-4a52-8c28-919a04a4c841').
narrative_ontology:cs_reading_relation('d07cd727-19e8-4a52-8c28-919a04a4c841', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_reading_relation('d07cd727-19e8-4a52-8c28-919a04a4c841', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('d07cd727-19e8-4a52-8c28-919a04a4c841', foundational, marketplace_of_ideas_maximization).
narrative_ontology:cs_axiom_status(marketplace_of_ideas_maximization, holdable).
narrative_ontology:cs_axiom_grounding('d07cd727-19e8-4a52-8c28-919a04a4c841', marketplace_of_ideas_maximization, deontological).
narrative_ontology:cs_axiom('d07cd727-19e8-4a52-8c28-919a04a4c841', foundational, state_neutrality_on_content).
narrative_ontology:cs_axiom_status(state_neutrality_on_content, holdable).
narrative_ontology:cs_axiom_grounding('d07cd727-19e8-4a52-8c28-919a04a4c841', state_neutrality_on_content, conventional).
narrative_ontology:cs_reference_frame('d07cd727-19e8-4a52-8c28-919a04a4c841', unfettered_expression_paradigm).
narrative_ontology:cs_drift_state('d07cd727-19e8-4a52-8c28-919a04a4c841', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d07cd727-19e8-4a52-8c28-919a04a4c841', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__absolutist_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, speakers_of_controversial_speech).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, free_speech_advocates).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, targets_of_harmful_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, vulnerable_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals benefit from a broad scope of protected speech, allowing them to express views that may be offensive or harmful to others without legal repercussions, unless it falls into a very narrow, unprotected category. Their autonomy is maximized.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, speakers_of_controversial_speech, beneficiary,
    moderate, immediate, mobile, global).

% These individuals bear the direct costs of speech that causes emotional distress, reputational damage, or incites hostility, without adequate legal recourse due to the high threshold for intervention. They are often left to absorb the harm.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, targets_of_harmful_speech, payer,
    powerless, immediate, trapped, local).

% These groups actively defend the broad interpretation of speech protection, viewing any restriction as a slippery slope to censorship. They benefit from the legal precedent that prioritizes speaker autonomy over potential harm.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, free_speech_advocates, beneficiary,
    organized, generational, analytical, national).

% These groups are disproportionately affected by hate speech, incitement to discrimination, and other forms of harmful expression, experiencing systemic marginalization and psychological distress with limited legal protection. Their identity makes exit from the target position impossible.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, vulnerable_groups, payer,
    powerless, generational, identity_locked, national).

% These institutions interpret and enforce the boundaries of protected speech. Under this reading, they are constrained to apply a very high harm threshold, often leading to outcomes that prioritize speaker rights over victim protection, even when significant harm is evident.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, courts_and_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% These bodies are responsible for crafting laws that balance speech rights with public safety and order. Under this absolutist reading, their ability to regulate harmful speech is severely limited by judicial precedent, making it difficult to address emerging forms of harm.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, legislators, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, albeit high, threshold for what constitutes unprotected speech, providing predictability for speakers and minimizing state intervention in expression.
% TRANSFER_FUNCTION: Transfers the burden of harm from speakers of controversial or offensive speech to the targets and vulnerable groups, who must absorb the negative impacts without legal redress.
% ABSENT_VOICES: Victims of speech-related harm, particularly those from marginalized communities, are often unheard in the legal discourse that shapes this absolutist interpretation. Their experiences of harm are systematically de-prioritized or dismissed as the 'cost of free speech'.
% DISAPPEARANCE_RATIONALE: If this absolutist reading vanished, the legal landscape around speech would immediately shift. Courts would likely adopt more nuanced balancing tests, leading to increased regulation of harmful speech, more legal recourse for victims, and a re-evaluation of the scope of speaker autonomy. The balance of power between speakers and targets would fundamentally alter.
% FOUNDING_PROBLEM: The constraint was established to prevent government censorship and protect a robust marketplace of ideas, ensuring that unpopular or dissenting views could be expressed without fear of suppression.
% FOUNDING_PROBLEM_CORROBORATION: Free speech advocates and some legal scholars attest that the threat of government overreach remains live, necessitating strong protections. However, targets of harmful speech and human rights organizations argue that the problem has shifted, and the current framework now enables harm rather than preventing it, making the 'live' status contested.
narrative_ontology:disappearance_verdict(speech_harm_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_harm_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__absolutist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the significant and often unmitigated harm borne by targets of speech that falls below the extremely high intervention threshold. Suppression is low (0.1) because the constraint's purpose is to minimize suppression of speech, not to enforce it. Theater ratio is low (0.05) as the system genuinely operates to protect speech, even if the consequences are extractive for others. Accessibility collapse is low (0.2) because alternatives to harmful speech (e.g., counter-speech, private action) are theoretically available, but often ineffective against systemic harm. Resistance is high (0.7) due to ongoing advocacy and legal challenges from groups seeking greater protection from speech-related harm.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of speakers and free speech advocates, this constraint is a 'rope' that coordinates open discourse and protects fundamental liberties. From the perspective of targets and vulnerable groups, it operates as a 'snare' that extracts their safety and dignity, leaving them exposed to harm under the guise of freedom. The courts, as agenda-setters, are caught between these competing interpretations, often forced by precedent to uphold the absolutist reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers of controversial speech and free speech advocates are clear beneficiaries, as the constraint maximizes their autonomy and influence (low directionality). Targets of harmful speech and vulnerable groups are clear victims, bearing the costs of the constraint's operation (high directionality, especially for identity_locked vulnerable groups). Courts and legislators, while powerful, are constrained by the absolutist interpretation, making their directionality more symmetric, as they must uphold a system that benefits some while harming others.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling broad speech protection as pure extraction by acknowledging its genuine coordination function for speakers. However, it risks mislabeling the extraction from targets as a necessary cost of coordination, rather than an asymmetric burden. The high extractiveness and resistance metrics, coupled with the 'contested' status of the founding problem, suggest a potential for mandatrophy where the original coordination function (preventing censorship) has been overshadowed by the enablement of harm.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolutism_vs_harm_threshold,
    'Is the high harm threshold a necessary component of robust free speech, or an arbitrary line that enables harm?',
    'Comparative legal analysis of jurisdictions with different harm thresholds and their impact on both speech vitality and victim protection. Empirical studies on the actual chilling effect of lower thresholds vs. the actual harm prevented.',
    'If the threshold is found to be arbitrary, it would weaken the justification for the absolutist reading, potentially shifting classification towards a ''tangled_rope'' or ''snare'' for targets. If necessary, it would reinforce the ''rope'' aspect for speakers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutism_vs_harm_threshold, conceptual, 'Whether the high harm threshold is structurally justified or merely a policy choice.').

omega_variable(
    identity_locked_victim_agency,
    'To what extent does the ''identity_locked'' status of vulnerable groups genuinely prevent exit, versus being a rhetorical framing to highlight their disproportionate burden?',
    'Sociological studies on the lived experience of vulnerable groups, examining their actual capacity to ''exit'' or mitigate the effects of harmful speech, and the psychological costs of such ''exit'' attempts. Legal analysis of available protective measures and their efficacy.',
    'If ''identity_locked'' is found to be primarily rhetorical, it would reduce the effective extraction from these groups (lower d), potentially shifting their seat classification. If it reflects a genuine structural inability to exit, it reinforces the ''snare'' classification for them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_victim_agency, empirical, 'The true nature of ''identity_locked'' exit for vulnerable groups.').

omega_variable(
    founding_problem_obsolescence,
    'Has the original problem of government censorship been sufficiently mitigated such that the absolutist reading now primarily serves to protect harmful speech rather than dissenting speech?',
    'Historical analysis of censorship trends, comparative studies of speech regulation in different eras, and analysis of contemporary threats to free expression. Examination of the types of speech currently protected by the high threshold.',
    'If the original problem is largely obsolete, it would strengthen the argument for mandatrophy, reclassifying the constraint towards a ''piton'' or ''snare'' for targets, as its primary function would have shifted from coordination to extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding problem of censorship is still the primary driver of the absolutist reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__absolutist_reading, 1969, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1969, speech_harm_boundary__absolutist_reading, theater_ratio, 1969, 0.05).
narrative_ontology:measurement(spee_tr_t1980, speech_harm_boundary__absolutist_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(spee_tr_t1990, speech_harm_boundary__absolutist_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(spee_tr_t2000, speech_harm_boundary__absolutist_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(spee_tr_t2010, speech_harm_boundary__absolutist_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(spee_tr_t2024, speech_harm_boundary__absolutist_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(spee_be_t1969, speech_harm_boundary__absolutist_reading, base_extractiveness, 1969, 0.7).
narrative_ontology:measurement(spee_be_t1980, speech_harm_boundary__absolutist_reading, base_extractiveness, 1980, 0.75).
narrative_ontology:measurement(spee_be_t1990, speech_harm_boundary__absolutist_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(spee_be_t2000, speech_harm_boundary__absolutist_reading, base_extractiveness, 2000, 0.82).
narrative_ontology:measurement(spee_be_t2010, speech_harm_boundary__absolutist_reading, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement(spee_be_t2024, speech_harm_boundary__absolutist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1969, speech_harm_boundary__absolutist_reading, suppression_requirement, 1969, 0.1).
narrative_ontology:measurement(spee_su_t1980, speech_harm_boundary__absolutist_reading, suppression_requirement, 1980, 0.1).
narrative_ontology:measurement(spee_su_t1990, speech_harm_boundary__absolutist_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(spee_su_t2000, speech_harm_boundary__absolutist_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(spee_su_t2010, speech_harm_boundary__absolutist_reading, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(spee_su_t2024, speech_harm_boundary__absolutist_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__absolutist_reading, information_standard).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__harm_balancing_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'speech_harm_boundary' kernel. Its absolutist interpretation directly influences the operational space and legitimacy of alternative readings that seek to balance speech with harm or dignity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
