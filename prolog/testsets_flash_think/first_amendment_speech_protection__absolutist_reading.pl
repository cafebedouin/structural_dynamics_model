% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__absolutist_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: first_amendment_speech_protection__absolutist_reading
 *   human_readable: First Amendment Absolutist Speech Protection
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint story instantiates the 'absolutist reading' of the First
 *   Amendment's speech protection, which interprets 'no law' to mean
 *   categorical protection for speech, except for a few narrow, historically
 *   recognized exclusions. This reading maximizes the protected speech set,
 *   often externalizing the costs of harmful speech onto targeted minorities
 *   and vulnerable communities. The claimed type is 'rope' from the
 *   perspective of its proponents, who view it as coordinating free
 *   expression, but the authored metrics reflect its highly extractive and
 *   resistant nature for those bearing its costs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, 0.85).
domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, 0.15).
domain_priors:theater_ratio(first_amendment_speech_protection__absolutist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__absolutist_reading, rope).
narrative_ontology:human_readable(first_amendment_speech_protection__absolutist_reading, "First Amendment Absolutist Speech Protection").
narrative_ontology:topic_domain(first_amendment_speech_protection__absolutist_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__absolutist_reading, '750b106f-c722-44b4-870d-974a3cb33dfc').
narrative_ontology:cs_kernel_codification('750b106f-c722-44b4-870d-974a3cb33dfc', fixed_text).
narrative_ontology:cs_authority_grounding('750b106f-c722-44b4-870d-974a3cb33dfc', lineage).
narrative_ontology:cs_interpretation_layer_present('750b106f-c722-44b4-870d-974a3cb33dfc').
narrative_ontology:cs_reading_relation('750b106f-c722-44b4-870d-974a3cb33dfc', first_amendment_speech_protection__harm_limited_reading, forecloses).
narrative_ontology:cs_reading_relation('750b106f-c722-44b4-870d-974a3cb33dfc', first_amendment_speech_protection__categorical_balancing_reading, forecloses).
narrative_ontology:cs_axiom('750b106f-c722-44b4-870d-974a3cb33dfc', foundational, speech_is_categorically_protected).
narrative_ontology:cs_axiom_status(speech_is_categorically_protected, holdable).
narrative_ontology:cs_axiom_grounding('750b106f-c722-44b4-870d-974a3cb33dfc', speech_is_categorically_protected, deontological).
narrative_ontology:cs_axiom('750b106f-c722-44b4-870d-974a3cb33dfc', foundational, no_law_means_no_law_textualism).
narrative_ontology:cs_axiom_status(no_law_means_no_law_textualism, holdable).
narrative_ontology:cs_axiom_grounding('750b106f-c722-44b4-870d-974a3cb33dfc', no_law_means_no_law_textualism, conventional).
narrative_ontology:cs_reference_frame('750b106f-c722-44b4-870d-974a3cb33dfc', original_intent_categorical_protection).
narrative_ontology:cs_drift_state('750b106f-c722-44b4-870d-974a3cb33dfc', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('750b106f-c722-44b4-870d-974a3cb33dfc', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, speakers).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, majority_groups).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, ideological_movements).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, targeted_minorities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, vulnerable_communities).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, free_speech_absolutism_doctrine).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, marketplace_of_ideas_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for the categorical protection of speech, interpreting 'no law' literally and resisting any limitations based on content or potential harm, except for narrow historical exclusions. They shape legal discourse and judicial appointments.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, absolutist_proponents, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from a broad zone of protected expression, allowing them to articulate views without fear of government censorship, even if those views are offensive or controversial. Their ability to speak is maximized.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, speakers, beneficiary,
    powerful, biographical, mobile, national).

% Benefit from the ability to express dominant cultural or political views without legal impediment, reinforcing their social position and influence. The costs of such speech are rarely borne by them.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, majority_groups, beneficiary,
    organized, generational, mobile, national).

% Leverage expansive speech protections to disseminate their messages, recruit members, and influence public opinion, often without accountability for the social harms their speech may cause.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, ideological_movements, beneficiary,
    organized, generational, mobile, national).

% Bear the brunt of harmful speech (e.g., hate speech, incitement to discrimination) that is protected under this reading. They experience systemic oppression, psychological distress, and physical threats, with limited legal recourse.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, targeted_minorities, payer,
    powerless, generational, trapped, national).

% Are disproportionately affected by speech that incites violence, harassment, or discrimination, finding their ability to participate equally in public life curtailed by an environment made hostile by protected harmful expression.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, vulnerable_communities, payer,
    powerless, generational, trapped, national).

% Seek to balance speech rights with the need to protect individuals and communities from demonstrable harm. Their arguments for speech regulation are often dismissed or marginalized by the absolutist framework, which prioritizes speaker liberty above all else.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, harm_reduction_advocates, excluded,
    organized, biographical, constrained, national).

% Interpret and enforce the First Amendment, often grappling with the tension between protecting speech and preventing harm. Under the absolutist reading, their role is to strike down most speech regulations, even those aimed at mitigating social harms.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Analyze and critique the various interpretations of the First Amendment, documenting the impacts of the absolutist reading on different social groups and proposing alternative frameworks.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, legal_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, high bar for government intervention in speech, coordinating a broad zone of expressive liberty and minimizing state censorship across diverse viewpoints.
% TRANSFER_FUNCTION: Transfers the burden of social and psychological harm caused by speech (e.g., hate speech, incitement) from the state/speaker to targeted individuals and communities, who bear the costs of systemic oppression and hostility.
% ABSENT_VOICES: Targeted minorities and vulnerable communities, whose experiences of harm are often dismissed or externalized as an unavoidable cost of liberty. Harm reduction advocates and those seeking to regulate speech based on its impact are structurally marginalized.
% DISAPPEARANCE_RATIONALE: If this absolutist reading vanished overnight, the legal landscape for speech would fundamentally shift. Courts would likely adopt more balancing tests, allowing for greater regulation of harmful speech, and potentially rebalancing the rights of speakers against the safety and dignity of communities. The public discourse would reorganize around different norms of acceptable expression.
% FOUNDING_PROBLEM: To prevent government censorship and ensure a robust marketplace of ideas, protecting individual expression from state overreach and ensuring a free exchange of political and social thought.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (e.g., some civil liberties organizations, certain legal scholars) attest the problem of state censorship is still live and requires robust protection. Critics (e.g., civil rights organizations, other legal scholars, social justice advocates) argue the founding problem is substantially solved regarding direct state censorship, and the arrangement now primarily serves to protect harmful speech, exacerbating inter-group conflict. Legislative hearing testimony and independent social science research from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(first_amendment_speech_protection__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__absolutist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.85) because the absolutist reading systematically transfers the burden of harm from speakers to targeted groups, who suffer social, psychological, and sometimes physical costs without legal recourse. Suppression is low (0.15) because the constraint's primary function is to prevent suppression of speech itself. Resistance is high (0.75) due to ongoing advocacy from civil rights groups and scholars challenging this interpretation. Theater ratio is low (0.1) as the constraint is actively and effectively enforced through judicial review.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of speakers and absolutist proponents, this reading functions as a 'rope,' coordinating a broad zone of liberty. From the perspective of targeted minorities and harm reduction advocates, the same structure operates as a 'snare,' extracting their safety and dignity as a cost of others' speech. The engine will compute this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers, majority groups, and ideological movements are clear beneficiaries (low directionality) as their expressive freedom is maximized. Targeted minorities and vulnerable communities are clear victims/targets (high directionality) as they bear the externalized costs of harmful speech. Harm reduction advocates are excluded, their arguments systematically marginalized by this framework. Courts and absolutist proponents act as agenda-setters, enforcing and perpetuating this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contestability,
    'Is the ''absolutist reading'' the only textually or historically defensible interpretation of the First Amendment''s ''no law'' clause, or is it a constructed interpretation that serves specific interests?',
    'Comprehensive historical and textual analysis of the First Amendment''s drafting and early interpretations, alongside contemporary legal and philosophical arguments for alternative readings.',
    'If other readings are found equally or more defensible, the absolutist reading''s claim to naturalness or inevitability is undermined, potentially shifting its classification from a claimed ''rope'' to a ''tangled_rope'' or ''snare'' for those it harms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contestability, conceptual, 'Ambiguity regarding the textual and historical grounding of the absolutist reading.').

omega_variable(
    harm_externalization_justification,
    'Is the externalization of harm onto targeted minorities a necessary and justifiable cost of maximizing free speech, or an unjust transfer of burden that undermines the liberty of the harmed?',
    'Sociological and psychological studies on the impact of hate speech and discriminatory expression, combined with ethical and political philosophy arguments regarding the scope and limits of liberty.',
    'If the externalized harm is deemed unjustifiable, the constraint''s extractiveness is confirmed as illegitimate, strengthening its classification towards ''snare'' for victims. If deemed a necessary cost, the ''rope'' framing gains some (contested) legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_externalization_justification, preference, 'Ethical justification for the harm externalized by expansive speech protections.').

omega_variable(
    founding_problem_shift_acknowledgment,
    'Has the primary problem the First Amendment was designed to solve shifted from state censorship to inter-group harm and private power, and is this shift acknowledged by the absolutist reading''s proponents?',
    'Analysis of historical and contemporary threats to free expression, distinguishing state action from private/social harms. Examination of judicial opinions and advocacy positions for explicit acknowledgment or denial of this shift.',
    'If the problem has shifted and is unacknowledged, the constraint''s persistence may be driven by inertia or rent-seeking (mandatrophy), pushing its classification towards ''piton'' or ''snare'' for those it harms, despite its claimed ''rope'' function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_shift_acknowledgment, empirical, 'Whether the First Amendment''s original purpose remains relevant to contemporary challenges.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__absolutist_reading, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t2000, first_amendment_speech_protection__absolutist_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(firs_tr_t2005, first_amendment_speech_protection__absolutist_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(firs_tr_t2010, first_amendment_speech_protection__absolutist_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(firs_tr_t2015, first_amendment_speech_protection__absolutist_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(firs_tr_t2020, first_amendment_speech_protection__absolutist_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(firs_tr_t2025, first_amendment_speech_protection__absolutist_reading, theater_ratio, 2025, 0.1).
narrative_ontology:measurement(firs_tr_t2030, first_amendment_speech_protection__absolutist_reading, theater_ratio, 2030, 0.1).

% Extraction over time
narrative_ontology:measurement(firs_be_t2000, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(firs_be_t2005, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 2005, 0.78).
narrative_ontology:measurement(firs_be_t2010, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(firs_be_t2015, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 2015, 0.82).
narrative_ontology:measurement(firs_be_t2020, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 2020, 0.84).
narrative_ontology:measurement(firs_be_t2025, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 2025, 0.85).
narrative_ontology:measurement(firs_be_t2030, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 2030, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t2000, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(firs_su_t2005, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 2005, 0.11).
narrative_ontology:measurement(firs_su_t2010, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 2010, 0.12).
narrative_ontology:measurement(firs_su_t2015, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 2015, 0.13).
narrative_ontology:measurement(firs_su_t2020, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 2020, 0.14).
narrative_ontology:measurement(firs_su_t2025, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 2025, 0.15).
narrative_ontology:measurement(firs_su_t2030, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 2030, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, hate_speech_regulation).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, online_content_moderation).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__harm_limited_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'first_amendment_speech_protection' kernel. Its ε value and stakeholder structure differ significantly from the 'harm_limited_reading' and 'categorical_balancing_reading' due to its maximalist interpretation of speech protection and externalization of harm.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
