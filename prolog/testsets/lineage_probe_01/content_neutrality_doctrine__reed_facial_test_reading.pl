% ============================================================================
% CONSTRAINT STORY: content_neutrality_doctrine__reed_facial_test_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_content_neutrality_reed_facial, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: content_neutrality_doctrine__reed_facial_test_reading
 *   human_readable: Content Neutrality Doctrine: Reed's Facial Test Reading
 *   domain: constitutional_law/first_amendment
 *
 * SUMMARY:
 *   The Reed v. Town of Gilbert doctrine instantiates one specific reading of
 *   the content-neutrality kernel — a principle governing how laws that
 *   categorize or regulate speech must be evaluated under the First
 *   Amendment. Reed's reading sharpens the triggering condition for strict
 *   scrutiny by adopting a facial test: if a law draws categorical lines
 *   based on topic, message, or subject matter on its face, it is
 *   content-based and presumptively fails, regardless of the legislature's
 *   benign purpose or the regulation's secondary effects. This reading
 *   forecloses two doctrinal escape routes that had existed under prior law:
 *   (1) the benign-purpose defense (a content-based law could survive if
 *   enacted for a reason unrelated to suppressing speech) and (2) the
 *   secondary-effects doctrine (a law targeting a category defined by content
 *   could survive if the regulation was justified by effects, not by
 *   hostility to the message). This constraint story instantiates ONLY the
 *   facial test reading. The sibling readings — compelled-speech reading
 *   (centering the speaker's right not to express) and secondary-effects
 *   reading (permitting content-defined categories if justified by effects) —
 *   are structurally distinct constraints with different extractiveness
 *   profiles and different victim sets. Each reading constitutes a different
 *   constraint; they are not alternative views of the same constraint.
 *
 * KEY AGENTS:
 *   - Speakers disfavored by content-based rules (beneficiary of this reading): those whose message or topic falls within a law's content-defined category. Reed's test presumptively protects them by barring the escape routes that had previously allowed such rules to survive.
 *   - Regulatory drafters (primary victim): legislators and administrative agencies that had used content-defined categories to achieve legitimate regulatory goals (e.g., zoning rules distinguishing commercial signs from ideological signs; regulations distinguishing obscenity from core speech). Reed's test suppresses the strategies available to them.
 *   - Secondary effects regulators (secondary victim): agencies attempting to regulate harms (crime, blight, traffic, public health) via rules that incidentally track content categories. The secondary-effects doctrine permitted these; Reed's test presumes them content-based and requires strict scrutiny.
 *   - The Court system (institutional beneficiary): the doctrine provides clear, bright-line rules (facial vs. as-applied, content-based vs. content-neutral) that reduce interpretive discretion and enable consistent case-sorting.
 *   - The public safety coalition (organized victim): cities, public health authorities, law enforcement seeking to regulate conduct via content-correlated categories. Reed's test constrains their regulatory toolkit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(content_neutrality_doctrine__reed_facial_test_reading, 0.35).
domain_priors:suppression_score(content_neutrality_doctrine__reed_facial_test_reading, 0.62).
domain_priors:theater_ratio(content_neutrality_doctrine__reed_facial_test_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(content_neutrality_doctrine__reed_facial_test_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(content_neutrality_doctrine__reed_facial_test_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(content_neutrality_doctrine__reed_facial_test_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(content_neutrality_doctrine__reed_facial_test_reading, tangled_rope).
narrative_ontology:human_readable(content_neutrality_doctrine__reed_facial_test_reading, "Content Neutrality Doctrine: Reed's Facial Test Reading").
narrative_ontology:topic_domain(content_neutrality_doctrine__reed_facial_test_reading, "constitutional_law/first_amendment").

domain_priors:requires_active_enforcement(content_neutrality_doctrine__reed_facial_test_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(content_neutrality_doctrine__reed_facial_test_reading, '9cba37eb-18a2-42dd-9d5c-77290caff605').
narrative_ontology:cs_kernel_codification('9cba37eb-18a2-42dd-9d5c-77290caff605', fixed_text).
narrative_ontology:cs_authority_grounding('9cba37eb-18a2-42dd-9d5c-77290caff605', lineage).
narrative_ontology:cs_interpretation_layer_present('9cba37eb-18a2-42dd-9d5c-77290caff605').
narrative_ontology:cs_reading_relation('9cba37eb-18a2-42dd-9d5c-77290caff605', content_neutrality_doctrine__compelled_speech_reading, coexists_with).
narrative_ontology:cs_reading_relation('9cba37eb-18a2-42dd-9d5c-77290caff605', content_neutrality_doctrine__secondary_effects_reading, forecloses).
narrative_ontology:cs_axiom('9cba37eb-18a2-42dd-9d5c-77290caff605', foundational, facial_test_triggers_strict_scrutiny).
narrative_ontology:cs_axiom_status(facial_test_triggers_strict_scrutiny, holdable).
narrative_ontology:cs_axiom_grounding('9cba37eb-18a2-42dd-9d5c-77290caff605', facial_test_triggers_strict_scrutiny, deontological).
narrative_ontology:cs_axiom('9cba37eb-18a2-42dd-9d5c-77290caff605', foundational, benign_purpose_escape_route_barred).
narrative_ontology:cs_axiom_status(benign_purpose_escape_route_barred, holdable).
narrative_ontology:cs_axiom_grounding('9cba37eb-18a2-42dd-9d5c-77290caff605', benign_purpose_escape_route_barred, deontological).
narrative_ontology:cs_reference_frame('9cba37eb-18a2-42dd-9d5c-77290caff605', categorical_content_sorting_presumptively_suspect).
narrative_ontology:cs_drift_state('9cba37eb-18a2-42dd-9d5c-77290caff605', post_reed_implementation, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('9cba37eb-18a2-42dd-9d5c-77290caff605', '').
narrative_ontology:cs_kernel_id(content_neutrality_doctrine__reed_facial_test_reading, content_neutrality_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(content_neutrality_doctrine__reed_facial_test_reading, speakers_disfavored_by_content_rules).
narrative_ontology:constraint_victim(content_neutrality_doctrine__reed_facial_test_reading, regulatory_drafting_flexibility).
narrative_ontology:constraint_victim(content_neutrality_doctrine__reed_facial_test_reading, secondary_effects_regulation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISFAVORED SPEAKER (SNARE) — A speaker whose message falls within a law's content-defined category (topic, viewpoint, message) faces strict scrutiny with no escape route. Reed's test provides no exception for benign purposes or secondary effects — the facial test closes all such loopholes. The speaker is trapped by the categorical rule regardless of the state's actual motive. Maximum extraction: the law presumptively fails even if well-intentioned.
constraint_indexing:constraint_classification(content_neutrality_doctrine__reed_facial_test_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGULATORY DRAFTER (TANGLED ROPE) — The drafter benefits from regulatory flexibility (coordination function: sorting problems by topic and regulating the problematic category is often functionally sensible). But Reed's test suppresses the escape route of claiming benign purpose or secondary effects — the drafter must now justify content-based rules through strict scrutiny or avoid them entirely. Mixed extraction: genuine coordination function (topic-sorting can solve real problems) but suppressed alternatives.
constraint_indexing:constraint_classification(content_neutrality_doctrine__reed_facial_test_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COURT SYSTEM (ROPE) — Reed's test creates clear line-drawing rules (facial vs. as-applied, content-based vs. content-neutral) that reduce interpretive discretion and litigation ambiguity. The court benefits from the test's bright-line structure. Net coordination: the doctrine provides a functional mechanism for sorting cases and limiting judicial discretion. The suppression of escape routes (benign purpose, secondary effects) is the coordination function itself — it makes the rule predictable.
constraint_indexing:constraint_classification(content_neutrality_doctrine__reed_facial_test_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC SAFETY COALITION (TANGLED ROPE) — Organized agents seeking to regulate harm (crime, blight, public health) face suppressed regulatory pathways. Reed's test closes the secondary effects doctrine — zoning adult businesses becomes presumptively content-based and subject to strict scrutiny, even though the regulatory goal (crime/blight reduction) is content-neutral. The coalition experiences genuine extraction: legitimate regulatory functions are barred or severely constrained. But some coordination benefit remains: the test's clarity enables public entities to understand regulatory boundaries in advance.
constraint_indexing:constraint_classification(content_neutrality_doctrine__reed_facial_test_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the principle that law may not suppress speech based on its message is a near-immutable commitment of liberal democracy. Reed's test appears to enforce an irreducible logical limit: either a law draws lines by content (and faces strict scrutiny) or it does not (and enjoys rational basis review). The test seems to enforce a binary logical gate that cannot be bypassed. However, the structural data contradicts this — beneficiaries and victims are identifiable, regulatory pathways are suppressed, and escape routes were deliberately foreclosed. The engine will classify this as a false summit.
constraint_indexing:constraint_classification(content_neutrality_doctrine__reed_facial_test_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: SECONDARY EFFECTS DOCTRINE (PITON) — Prior jurisprudence (City of Renton, Barnes v. Glen Theatre, Inc.) had permitted content-defined regulations if justified by secondary effects rather than message suppression. Reed's test degrades this doctrine to theater: courts still invoke secondary effects rhetoric, but the facial test now presumes the law is content-based and requires strict scrutiny regardless. The old doctrine persists through inertia in judicial language but has lost functional force. Theater ratio reflects the gap between the doctrine's nominal existence and its actual legal power.
constraint_indexing:constraint_classification(content_neutrality_doctrine__reed_facial_test_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(content_neutrality_doctrine__reed_facial_test_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(content_neutrality_doctrine__reed_facial_test_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(content_neutrality_doctrine__reed_facial_test_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(content_neutrality_doctrine__reed_facial_test_reading, TR),
    TR >= 0.70.

:- end_tests(content_neutrality_doctrine__reed_facial_test_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The Reed reading imposes significant constraints on regulatory flexibility — the suppression of benign-purpose and secondary-effects defenses is a real narrowing of prior doctrine. However, extractiveness is not as high as a pure snare (0.46+) because the constraint preserves the possibility of narrow tailoring under strict scrutiny; content-based rules can survive if they meet the compelling interest + narrow tailoring test. The extraction is the foreclosure of escape routes, not an absolute bar. Suppression (0.62): Moderate-high. The facial test forecloses two major doctrinal strategies (benign-purpose, secondary effects) that had previously permitted content-based rules. Regulatory drafters face significant barriers to deploying content-defined categories. However, suppression is not total — strict scrutiny remains available, and some content-based rules (regulating true threats, fraud, defamation) have survived. Theater ratio (0.58): Moderate-high. The facial test involves some performative elements: courts still recite secondary-effects language and benign-purpose considerations even after Reed has presumptively barred them. The doctrine's prior formulations persist in judicial rhetoric even though they have lost doctrinal force. The piton perspective captures this degradation — the old doctrine is maintained through inertia in language but has been functionally displaced by the facial test.
 *
 * PERSPECTIVAL GAP:
 *   The Reed facial test reading produces a perspectival gap between beneficiaries (speakers disfavored by content rules) and victims (regulatory drafters). Disfavored speakers experience the constraint as a snare — they are protected, but the rule is categorical and admits no mercy. Drafters experience it as tangled rope — they have a legitimate coordination function (sorting regulatory problems by category) but face suppressed escape routes. The doctrinal beneficiary (the court) experiences it as rope — the bright-line test provides clear structure. The secondary-effects regulator experiences it as tangled rope — genuine regulatory interests (crime, blight) are suppressed. The analytical observer risks seeing this as a mountain — an immutable principle of liberal democracy — but the structural data reveals it as a constructed institutional arrangement with identifiable beneficiaries and victims.
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation of directionality values follows the structural positions: disfavored speakers are beneficiaries of this reading with trapped exit (no escape from the facial test once their speech is content-categorized) → low d → negative f(d) → negative effective extraction. Regulatory drafters are victims with constrained exit (they can avoid content-based rules entirely or attempt strict scrutiny, but neither option is costless) → moderate-high d. The court system is a beneficiary with arbitrage exit (the bright-line test is optional for the court to apply but provides clear advantages) → low d. Secondary-effects regulators are organized victims with constrained exit → moderate d. The facial test deliberately flattens prior doctrine's complexity by removing d modulation — the benign-purpose escape route allowed prior doctrine to decrease d for well-intentioned drafters; Reed removes this possibility, increasing all drafters' d equally.
 *
 * MANDATROPHY ANALYSIS:
 *   The Reed reading avoids mandatrophy by showing that the content-neutrality kernel permits multiple structurally distinct readings, each with its own extractiveness. No single reading is 'the' content-neutrality doctrine — the doctrine is the presheaf of readings indexed by observational position. The facial test reading is the reading that maximally protects disfavored speakers (highest beneficiary protection) and maximally suppresses regulatory drafters (highest victim constraint). The secondary-effects reading achieves the opposite weighting. The compelled-speech reading centers a different structural axis (the speaker's right not to compel, not the regulator's constraints). The three readings together resolve the mandatrophy: they show that 'content neutrality' is a contested kernel with inherent under-determination, not a single constraint mislabeled three ways.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    facial_versus_as_applied_line,
    'Can a facially content-based law ever satisfy strict scrutiny? Is the facial test a per se rule or a rebuttable presumption?',
    'Examination of post-Reed cases permitting facially content-based laws under strict scrutiny (e.g., regulations of true threats, fraud, defamation). Clarification of whether strict scrutiny is actually strict in practice or whether ''compelling interest + narrow tailoring'' permits content-based rules to survive.',
    'If facially content-based laws can survive strict scrutiny: the test''s extractiveness drops (suppression of escape routes is less absolute). If strict scrutiny is per se fatal: extractiveness confirmed (suppression is total for disfavored speakers).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(facial_versus_as_applied_line, empirical, 'Whether strict scrutiny permits facially content-based laws to survive').

omega_variable(
    content_based_definition_boundary,
    'What counts as ''drawing lines by topic or message on its face''? Does implicit content-basedness (a rule that targets content without naming it) escape the facial test?',
    'Analysis of laws that regulate conduct closely correlated with content (e.g., nude dancing, burning flags) but do not explicitly name the content. Determination of whether the facial test is triggered by explicit category-naming or by purpose-revealed content sorting.',
    'If only explicit category-naming triggers the test: many content-targeting laws escape facial review. If implicit content-sorting triggers it: the test captures a broader regulatory space and suppresses more escape routes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(content_based_definition_boundary, conceptual, 'Boundary between explicit and implicit content-basedness').

omega_variable(
    secondary_effects_foreclosure_scope,
    'Has Reed entirely foreclosed the secondary effects doctrine, or does it remain available for laws targeting non-speech conduct that incidentally affects expression?',
    'Post-Reed case law tracking: regulations of conduct (zoning, time/place/manner) that achieve public safety or land-use goals without targeting content directly. Whether courts permit these to evade strict scrutiny via secondary effects reasoning.',
    'If secondary effects remains viable: regulatory drafters retain a suppressed but available escape route (conduct-targeting that avoids facial content-basedness). If entirely foreclosed: suppression is comprehensive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_effects_foreclosure_scope, empirical, 'Scope of secondary effects doctrine post-Reed').

omega_variable(
    reading_contest_identity,
    'Which reading of the content-neutrality kernel is this? How does the facial test reading differ structurally from compelled-speech and secondary-effects readings?',
    'Jurisprudential analysis of competing doctrinal framings: Reed''s facial test (content on the face of the law triggers strict scrutiny regardless of purpose) vs. compelled-speech framing (neutrality is about the speaker''s right not to express, symmetrical to the right to speak freely) vs. secondary-effects softening (content-defined categories are permissible if justified by effects, not message suppression).',
    'This omega is conceptual/doctrinal. The reading_relations and axioms (in cs_structure) are the primary structural differentiation. This omega documents the kernel contest itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_identity, conceptual, 'Content neutrality kernel readings and their structural differentiation').

omega_variable(
    benign_purpose_escape_route_foreclosure,
    'Was the suppression of benign-purpose defenses an intentional narrowing of content-neutrality doctrine, or an incidental logical consequence of the facial test?',
    'Examination of Reed opinion and its predecessors. Determination of whether the Court deliberately closed the benign-purpose escape route or whether it followed from the facial test logic.',
    'If intentional narrowing: the suppression is a doctrinal choice with consequences for regulatory flexibility (high extractiveness from drafters'' perspective). If logical consequence: the test is enforcing an inherent doctrinal structure (lower extractiveness, closer to coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benign_purpose_escape_route_foreclosure, empirical, 'Whether benign-purpose suppression was intentional or logical').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(content_neutrality_doctrine__reed_facial_test_reading, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(content_neutrality_reed_tr_t0, content_neutrality_doctrine__reed_facial_test_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(content_neutrality_reed_tr_t2, content_neutrality_doctrine__reed_facial_test_reading, theater_ratio, 2, 0.52).
narrative_ontology:measurement(content_neutrality_reed_tr_t5, content_neutrality_doctrine__reed_facial_test_reading, theater_ratio, 5, 0.58).

% Extraction over time
narrative_ontology:measurement(content_neutrality_reed_be_t0, content_neutrality_doctrine__reed_facial_test_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(content_neutrality_reed_be_t2, content_neutrality_doctrine__reed_facial_test_reading, base_extractiveness, 2, 0.28).
narrative_ontology:measurement(content_neutrality_reed_be_t5, content_neutrality_doctrine__reed_facial_test_reading, base_extractiveness, 5, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(content_neutrality_reed_su_t0, content_neutrality_doctrine__reed_facial_test_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(content_neutrality_reed_su_t2, content_neutrality_doctrine__reed_facial_test_reading, suppression_requirement, 2, 0.54).
narrative_ontology:measurement(content_neutrality_reed_su_t5, content_neutrality_doctrine__reed_facial_test_reading, suppression_requirement, 5, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(content_neutrality_doctrine__reed_facial_test_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(content_neutrality_doctrine__reed_facial_test_reading, content_neutrality_doctrine__compelled_speech_reading).
narrative_ontology:affects_constraint(content_neutrality_doctrine__reed_facial_test_reading, content_neutrality_doctrine__secondary_effects_reading).

% DUAL FORMULATION NOTE:
% The content-neutrality doctrine kernel decomposes into three structurally distinct readings: Reed's facial test, compelled speech, and secondary effects. Each reading is a separate constraint with its own ε, beneficiary/victim structure, and classification profile. They are linked via the kernel_id (content_neutrality_doctrine) and constitute a constraint family. All three are instantiations of the same legal principle but applied to different structural situations. The facial test reading (this constraint) forecloses the regulatory escape routes that the secondary-effects reading preserves, and both differ from the compelled-speech reading's focus on the speaker's affirmative right not to compel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
