% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: first_amendment_speech_protection__absolutist_reading
 *   human_readable: First Amendment Absolutist Reading — 'No Law Means No Law'
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the absolutist reading of the First Amendment
 *   speech kernel: the text's 'no law' phrasing is read as a near-categorical
 *   bar on government regulation of speech content, with protection yielding
 *   only to a narrow, historically fixed set of exceptions (true threats,
 *   incitement to imminent lawless action, obscenity, fraud). Under this
 *   reading, the doctrine's refusal to weigh case-by-case harm is the
 *   feature, not a bug — it denies government the discretionary censorship
 *   tool that was historically used against dissidents and minorities. But
 *   the same categorical refusal externalizes the accumulated harm of
 *   organized hate speech, harassment campaigns, and captive-audience
 *   degradation onto the targets of that speech, who have no doctrinal path
 *   to relief absent a narrow enumerated exception. This is ONE of three
 *   readings of the shared kernel; the harm_limited_reading and
 *   categorical_balancing_reading are separate constraint stories with their
 *   own ε, beneficiary/victim sets, and classifications — they are not
 *   represented here, only linked via network and
 *   cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - political_speakers: beneficiary (organized/arbitrage) — maximal expressive latitude
 *   - organized_hate_movements: beneficiary/agenda_setter (organized/arbitrage) — strategic doctrinal boundary-setting
 *   - racial_and_religious_minorities: payer (powerless/trapped) — absorbs externalized harm
 *   - constitutional_courts: agenda_setter (institutional/analytical) — administers the categorical line
 *   - content_neutral_regulators: excluded (institutional/constrained) — foreclosed from harm-based tailoring
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, 0.58).
domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, 0.42).
domain_priors:theater_ratio(first_amendment_speech_protection__absolutist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__absolutist_reading, "First Amendment Absolutist Reading — 'No Law Means No Law'").
narrative_ontology:topic_domain(first_amendment_speech_protection__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__absolutist_reading, '6e3e4779-aab4-494b-a588-19ac1904aba4').
narrative_ontology:cs_kernel_codification('6e3e4779-aab4-494b-a588-19ac1904aba4', fixed_text).
narrative_ontology:cs_authority_grounding('6e3e4779-aab4-494b-a588-19ac1904aba4', lineage).
narrative_ontology:cs_interpretation_layer_present('6e3e4779-aab4-494b-a588-19ac1904aba4').
narrative_ontology:cs_reading_relation('6e3e4779-aab4-494b-a588-19ac1904aba4', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e3e4779-aab4-494b-a588-19ac1904aba4', first_amendment_speech_protection__categorical_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('6e3e4779-aab4-494b-a588-19ac1904aba4', foundational, textual_categoricalism_over_harm_weighing).
narrative_ontology:cs_axiom_status(textual_categoricalism_over_harm_weighing, holdable).
narrative_ontology:cs_axiom_grounding('6e3e4779-aab4-494b-a588-19ac1904aba4', textual_categoricalism_over_harm_weighing, conventional).
narrative_ontology:cs_axiom('6e3e4779-aab4-494b-a588-19ac1904aba4', foundational, externalized_harm_is_liberty_cost_not_regulable_interest).
narrative_ontology:cs_axiom_status(externalized_harm_is_liberty_cost_not_regulable_interest, holdable).
narrative_ontology:cs_axiom_grounding('6e3e4779-aab4-494b-a588-19ac1904aba4', externalized_harm_is_liberty_cost_not_regulable_interest, instrumental).
narrative_ontology:cs_reference_frame('6e3e4779-aab4-494b-a588-19ac1904aba4', founding_era_textual_command_against_discretionary_censorship).
narrative_ontology:cs_drift_state('6e3e4779-aab4-494b-a588-19ac1904aba4', contemporary_networked_harassment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6e3e4779-aab4-494b-a588-19ac1904aba4', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, political_speakers).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, media_and_press_organizations).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, organized_hate_movements).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, majority_group_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, racial_and_religious_minorities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, targets_of_organized_harassment_campaigns).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, vulnerable_speech_targets_in_captive_settings).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, textualist_categorical_supremacy_doctrine).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__absolutist_reading, marketplace_of_ideas_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in political, ideological, and dissenting speech with near-total protection from government sanction regardless of content or offensiveness, so long as no narrow historical exception (true threats, incitement to imminent lawless action, obscenity, fraud) applies. Can say almost anything about anyone without legal consequence, and rely on courts to strike down content-based restrictions as a matter of near-mechanical rule.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, political_speakers, beneficiary,
    organized, generational, arbitrage, national).

% Publish, broadcast, and distribute speech — including speech that provokes, insults, or harms reputational and social interests of targeted groups — with categorical legal cover. The absolutist reading gives them maximal protection against defamation-adjacent regulation and content-based licensing regimes.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, media_and_press_organizations, beneficiary,
    institutional, generational, arbitrage, national).

% Deploy the categorical rule strategically: organize rallies, distribute propaganda, and coordinate harassment campaigns just short of the narrow historical exceptions (true threats, incitement), knowing the doctrine treats the harm caused to targets as externalized cost rather than a regulable interest. They actively litigate to keep the exceptions narrow, functioning as agenda-setters who shape where the doctrinal line sits.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, organized_hate_movements, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__absolutist_reading, organized_hate_movements, agenda_setter).

% Absorb the accumulated social, psychological, and physical-safety costs of speech the doctrine refuses to regulate — cross burnings short of true threats, organized slurs, dehumanizing propaganda, coordinated intimidation campaigns. Cannot relocate away from a national legal rule; their only recourse is political organizing to eventually shift doctrine, a generational project with no guarantee of success.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, racial_and_religious_minorities, payer,
    powerless, biographical, trapped, national).

% Individuals or small groups subjected to coordinated online or in-person harassment that stays technically short of 'true threat' or 'incitement' thresholds. Bear acute, immediate costs — fear, reputational destruction, withdrawal from public life — while the speakers face no legal exposure because the categorical rule treats the harm as constitutionally irrelevant unless it crosses a narrow bright line.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, targets_of_organized_harassment_campaigns, payer,
    powerless, immediate, constrained, regional).

% People in workplaces, schools, or institutional settings who cannot leave and are exposed to protected speech that degrades their standing (e.g., ideologically motivated harassment that a captive-audience or hostile-environment framework would otherwise regulate). Under the absolutist reading their captivity is not itself a sufficient predicate for regulation absent an enumerated exception.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, vulnerable_speech_targets_in_captive_settings, payer,
    powerless, immediate, trapped, local).

% Legislatures and agencies that might otherwise craft narrowly tailored, harm-responsive speech regulation (anti-harassment statutes, hate-speech civil remedies) are foreclosed from doing so by the categorical rule's refusal to weigh harm case-by-case. Their proposed balancing frameworks are not part of the operative doctrine under this reading — they exist only as briefs and dissents.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, content_neutral_regulators, excluded,
    institutional, generational, constrained, national).

% Administer the categorical rule, deciding what falls inside the narrow historical exceptions (obscenity, true threats, incitement, fraud, fighting words in narrowing form) and what does not. They enforce the rule's near-absolute character by striking down content-based and harm-based regulations, treating case-by-case balancing as itself the danger to be avoided.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, constitutional_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__absolutist_reading, diffuse).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a predictable, content-neutral rule that prevents the government of the day from selectively silencing dissidents, minorities, or unpopular movements by declaring their speech 'harmful' — a genuine coordination good against majoritarian censorship, historically vindicated by cases where the government targeted civil rights speakers, war protesters, and religious minorities under harm-based pretexts.
% TRANSFER_FUNCTION: Moves the cost of speech-caused harm from the state (which would otherwise absorb enforcement and adjudication costs of harm-based regulation) and from speech targets (who bear the externalized harm) to targeted minorities and vulnerable individuals, while speakers and press organizations retain the full benefit of maximal expressive latitude.
% ABSENT_VOICES: Targets of hate speech and organized harassment campaigns are structurally absent from the doctrinal conversation that sets the boundary of the historical exceptions — their harm testimony enters as amicus briefs and dissenting opinions, not as operative doctrine, because the categorical rule is designed precisely to exclude case-by-case harm weighing.
% DISAPPEARANCE_RATIONALE: If the absolutist reading were abandoned overnight in favor of harm-based or balancing regimes, political speech, press practices, and hate-movement organizing would face immediate new civil and criminal exposure; legislatures would begin drafting harm-responsive statutes; litigation over what counts as 'demonstrable harm' would become the central battleground of speech law, replacing the current bright-line exception litigation.
% FOUNDING_PROBLEM: Colonial and early-republic governments used seditious libel, blasphemy, and licensing laws to silence political dissidents and religious minorities under the banner of preventing 'harm' or 'disorder' — the categorical rule was built to deny government the discretionary harm-based tool that had been used as a censorship weapon.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and civil liberties organizations outside any speaker-beneficiary group attest the founding problem (discretionary government censorship of dissidents) remains partially live in authoritarian and semi-authoritarian contexts abroad and in some domestic contexts (protest policing). Civil rights scholars and organizations representing targeted minorities — themselves outside the beneficiary set — attest that the founding problem has been substantially supplanted by a different live problem (organized, harm-causing speech by well-resourced private and quasi-organized actors) that the categorical rule was never designed to address and now shields.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(first_amendment_speech_protection__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__absolutist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) is substantial but not extreme: the rule genuinely coordinates against majoritarian censorship (a real, historically vindicated function), but the accumulated cost of unregulated organized speech-harm has risen over the twentieth-to-twenty-first century as harassment campaigns became more organized and technologically amplified — hence the rising base_extractiveness series. Suppression (0.42) reflects that the doctrine's coercive force falls mainly on regulators seeking to act, not on speakers, and has settled at a stable moderate level as the doctrine matured; the suppression_requirement series therefore declines slightly and then flattens, reflecting doctrinal consolidation rather than an enforcement ratchet. Accessibility_collapse (0.35) is moderate-low: harm-based and balancing alternatives remain live in legal scholarship, dissenting opinions, and other jurisdictions, so alternatives have not fully collapsed even though the operative U.S. doctrine has settled on the categorical line. Resistance (0.72) is high because civil rights organizations, harassment-target advocacy groups, and reform-minded scholars actively contest the doctrine in courts, legislatures, and public discourse.
 *
 * DIRECTIONALITY LOGIC:
 *   Political speakers, press organizations, and organized hate movements are beneficiaries: the rule subsidizes their expressive activity by denying government any harm-based lever against them, and their exit options (arbitrage — they can speak across venues, jurisdictions, platforms) are maximal. Racial/religious minorities, harassment targets, and captive-setting targets are victims: the same categorical rule that protects speakers denies them a doctrinal path to relief, and their exit options range from constrained to trapped because withdrawal from public life or relocation does not escape a national legal rule. Constitutional courts sit as agenda_setter with analytical exit — they administer the boundary but do not personally bear its costs or collect its benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (discretionary government censorship of dissidents via harm-based pretexts) is contested as live-vs-dead: it remains partially live against state actors but the rule's operative burden has shifted toward shielding well-resourced private speech-harm campaigns that the doctrine was never built to police. Classifying this as tangled_rope (rather than snare) preserves the genuine coordination function — the rule really does prevent a documented historical censorship tool — while still naming the asymmetric extraction the same structure now enables. Reclassifying to a pure snare would erase the doctrine's real anti-censorship function; reclassifying to a pure rope would erase the documented, unaddressed cost borne by targeted minorities. The tangled_rope reading holds both facts without dissolving either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_absolutism_vs_constructed_doctrine,
    'Is the ''no law means no law'' reading a genuine discovery of the constitutional text''s plain meaning, or a twentieth-century doctrinal construction that retrofits categorical absolutism onto a text the framers understood as compatible with substantial regulation (libel, blasphemy, fighting words were regulated at the founding)?',
    'Historical linguistic and legal-practice analysis of founding-era speech regulation (seditious libel prosecutions continued after ratification; blasphemy and obscenity laws were widely unchallenged) compared against the doctrine''s actual twentieth-century emergence in cases like Brandenburg and its progeny.',
    'If the absolutist reading is a constructed doctrine rather than a textual discovery, its claim to categorical/mountain-like inevitability is undermined, and the beneficiary structure identified here (speakers, press, organized movements) becomes evidence of a false-summit dynamic rather than neutral textualism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_absolutism_vs_constructed_doctrine, conceptual, 'Whether absolutist textualism is discovered or constructed doctrine.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three kernel readings (absolutist, harm_limited, categorical_balancing) diverge — is it in the interpretation of ''no law,'' in the scope of the historical exceptions, or in the weight assigned to accumulated third-party harm?',
    'Doctrinal comparison across the three sibling constraint stories: absolutist_reading treats the exception list as closed and harm as constitutionally irrelevant outside it; harm_limited_reading would treat demonstrable harm as an independent, expandable predicate for regulation; categorical_balancing_reading would treat speech value and harm as commensurable and to be weighed case-by-case. The disagreement is located in whether harm is a fixed enumerated category or an open, evidence-responsive one.',
    'If the disagreement is really about harm-category openness rather than textual meaning, then the absolutist reading''s claim to being the ''plain meaning'' reading is weaker than its self-presentation suggests, since all three readings agree on the text and diverge only on how harm enters the analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locates the structural disagreement among the three kernel readings in harm-category treatment, not textual meaning.').

omega_variable(
    exception_list_stability,
    'Is the narrow historical exception list (true threats, incitement, obscenity, fraud) itself stable, or does it quietly expand and contract in ways that functionally import harm-balancing under a categorical label?',
    'Track how courts have redefined ''true threat'' and ''incitement'' over time (e.g., narrowing incitement standards post-Brandenburg vs. debates over online true-threat doctrine post-Counterman) to see whether the categorical frame is doing real interpretive work or is itself drifting toward disguised balancing.',
    'If the exception list is not stable, the absolutist reading''s claim to categorical predictability (its main coordination benefit) is weaker than claimed, and part of the measured extractiveness may reflect disguised, unacknowledged balancing rather than genuine rule-following.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exception_list_stability, empirical, 'Whether the categorical exception list is stable or functions as covert balancing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__absolutist_reading, 1919, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t1919, first_amendment_speech_protection__absolutist_reading, theater_ratio, 1919, 0.12).
narrative_ontology:measurement(firs_tr_t1940, first_amendment_speech_protection__absolutist_reading, theater_ratio, 1940, 0.15).
narrative_ontology:measurement(firs_tr_t1969, first_amendment_speech_protection__absolutist_reading, theater_ratio, 1969, 0.18).
narrative_ontology:measurement(firs_tr_t1992, first_amendment_speech_protection__absolutist_reading, theater_ratio, 1992, 0.22).
narrative_ontology:measurement(firs_tr_t2010, first_amendment_speech_protection__absolutist_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(firs_tr_t2024, first_amendment_speech_protection__absolutist_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(firs_be_t1919, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 1919, 0.38).
narrative_ontology:measurement(firs_be_t1940, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 1940, 0.42).
narrative_ontology:measurement(firs_be_t1969, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 1969, 0.46).
narrative_ontology:measurement(firs_be_t1992, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 1992, 0.5).
narrative_ontology:measurement(firs_be_t2010, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 2010, 0.54).
narrative_ontology:measurement(firs_be_t2024, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t1919, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 1919, 0.55).
narrative_ontology:measurement(firs_su_t1940, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 1940, 0.5).
narrative_ontology:measurement(firs_su_t1969, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 1969, 0.44).
narrative_ontology:measurement(firs_su_t1992, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 1992, 0.42).
narrative_ontology:measurement(firs_su_t2010, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 2010, 0.42).
narrative_ontology:measurement(firs_su_t2024, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__absolutist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__absolutist_reading, 0.12).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, harm_limited_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the first_amendment_speech_protection kernel. harm_limited_reading and categorical_balancing_reading are separate constraint files with independently authored ε, beneficiary/victim structures, and classifications — they are NOT alternative measurements of this constraint, but structurally distinct constraints sharing a textual kernel. This absolutist reading maximizes the protected-speech set and externalizes minority harm as a cost of liberty; the harm_limited_reading would internalize demonstrable harm as a regulable predicate (likely raising protections for harm-targets and correspondingly lowering speaker latitude); the categorical_balancing_reading would treat speech value and harm as commensurable for case-by-case weighing (an intermediate structure). All three should be linked bidirectionally via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
