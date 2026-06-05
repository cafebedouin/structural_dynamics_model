% ============================================================================
% CONSTRAINT STORY: content_neutrality_doctrine__secondary_effects_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_content_neutrality_secondary_effects, []).

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
 *   constraint_id: content_neutrality_doctrine__secondary_effects_reading
 *   human_readable: Content Neutrality Doctrine: Secondary Effects Reading
 *   domain: constitutional_law/first_amendment
 *
 * SUMMARY:
 *   The secondary effects reading of content neutrality doctrine permits the
 *   state to regulate speech-carrying conduct by reference to its harmful
 *   secondary effects rather than its message. Adult businesses may be zoned
 *   to address documented crime and blight — the zoning is nominally
 *   content-neutral because it targets effects, not the sexual content
 *   displayed. Yet the category 'adult business' is necessarily defined by
 *   the content it displays: sexually explicit materials or conduct. The
 *   doctrine thus treats a content-defined category as content-neutral where
 *   secondary effects justify the regulation. This is the 'acknowledged
 *   fiction' — courts know the category is content-defined but apply
 *   content-neutral scrutiny (rational basis) rather than strict scrutiny
 *   because secondary effects provide an alternative rationale. The
 *   constraint exhibits tangled rope structure: genuine coordination function
 *   (municipalities can address documented nuisances) combined with
 *   asymmetric extraction (disfavored speech is suppressed under the fiction
 *   of neutrality). The theater ratio (0.68) reflects the doctrine's reliance
 *   on the fiction that rationality-basis scrutiny applies to a
 *   content-defined category. The extractiveness (0.52) is moderate because
 *   the coordination function is real (secondary effects zoning does address
 *   some documented harms) but the suppression mechanism (the fiction itself)
 *   is substantial.
 *
 * KEY AGENTS:
 *   - Adult Business Owners: Primary victims (powerless/trapped) — zoned under nominally neutral category that is actually content-defined; no exit from jurisdiction preserves the restrictions
 *   - Municipal Governments: Primary beneficiaries (institutional/arbitrage) — gain regulatory authority to address documented crime/blight without facing strict scrutiny; the secondary effects fiction enables coordination
 *   - Courts/Appellate System: Organized enforcers (organized/constrained) — apply the fiction while acknowledging its incoherence; constrained by precedent and by the need to provide coherent doctrine
 *   - Constitutional Scholars: Moderate observers (moderate/constrained) — benefit from doctrine stability (career mastery) while bearing cost of epistemic incoherence; constrained by field pressure to affirm settled doctrine
 *   - Legislative Reformers: Powerful agents (powerful/mobile) — see secondary effects doctrine as temporary expedient; Reed sharpened facial test, creating pressure for explicit statutory alternatives
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent doctrine as immutable law of how regulation balances expression and order
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(content_neutrality_doctrine__secondary_effects_reading, 0.52).
domain_priors:suppression_score(content_neutrality_doctrine__secondary_effects_reading, 0.48).
domain_priors:theater_ratio(content_neutrality_doctrine__secondary_effects_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(content_neutrality_doctrine__secondary_effects_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(content_neutrality_doctrine__secondary_effects_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(content_neutrality_doctrine__secondary_effects_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(content_neutrality_doctrine__secondary_effects_reading, tangled_rope).
narrative_ontology:human_readable(content_neutrality_doctrine__secondary_effects_reading, "Content Neutrality Doctrine: Secondary Effects Reading").
narrative_ontology:topic_domain(content_neutrality_doctrine__secondary_effects_reading, "constitutional_law/first_amendment").

domain_priors:requires_active_enforcement(content_neutrality_doctrine__secondary_effects_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(content_neutrality_doctrine__secondary_effects_reading, '12f69868-7e1a-4110-91ad-7912bc9119e7').
narrative_ontology:cs_kernel_codification('12f69868-7e1a-4110-91ad-7912bc9119e7', formalized).
narrative_ontology:cs_authority_grounding('12f69868-7e1a-4110-91ad-7912bc9119e7', lineage).
narrative_ontology:cs_interpretation_layer_present('12f69868-7e1a-4110-91ad-7912bc9119e7').
narrative_ontology:cs_reading_relation('12f69868-7e1a-4110-91ad-7912bc9119e7', content_neutrality_doctrine__compelled_speech_reading, coexists_with).
narrative_ontology:cs_reading_relation('12f69868-7e1a-4110-91ad-7912bc9119e7', content_neutrality_doctrine__reed_facial_test_reading, influences).
narrative_ontology:cs_axiom('12f69868-7e1a-4110-91ad-7912bc9119e7', foundational, secondary_effects_category_justifies_content_neutral_scrutiny).
narrative_ontology:cs_axiom_status(secondary_effects_category_justifies_content_neutral_scrutiny, holdable).
narrative_ontology:cs_axiom_grounding('12f69868-7e1a-4110-91ad-7912bc9119e7', secondary_effects_category_justifies_content_neutral_scrutiny, conventional).
narrative_ontology:cs_axiom('12f69868-7e1a-4110-91ad-7912bc9119e7', secondary, content_definition_does_not_defeat_neutrality_where_effects_justify).
narrative_ontology:cs_axiom_status(content_definition_does_not_defeat_neutrality_where_effects_justify, holdable).
narrative_ontology:cs_axiom_grounding('12f69868-7e1a-4110-91ad-7912bc9119e7', content_definition_does_not_defeat_neutrality_where_effects_justify, instrumental).
narrative_ontology:cs_reference_frame('12f69868-7e1a-4110-91ad-7912bc9119e7', content_neutral_secondary_effects_framework).
narrative_ontology:cs_drift_state('12f69868-7e1a-4110-91ad-7912bc9119e7', post_reed_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('12f69868-7e1a-4110-91ad-7912bc9119e7', '').
narrative_ontology:cs_kernel_id(content_neutrality_doctrine__secondary_effects_reading, content_neutrality_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(content_neutrality_doctrine__secondary_effects_reading, municipal_regulation_authority).
narrative_ontology:constraint_victim(content_neutrality_doctrine__secondary_effects_reading, disfavored_speech_category).
narrative_ontology:constraint_victim(content_neutrality_doctrine__secondary_effects_reading, doctrine_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ADULT BUSINESS OWNER (SNARE) — Trapped by zoning categories nominally content-neutral (addressing crime/blight) that are applied exclusively to content the municipality disfavors. No exit: relocating within jurisdiction lands in another zone with the same restrictions; federal commerce exemptions do not apply to zoning; challenging requires expensive litigation against entrenched municipal authority. Experiences maximal extraction masked by performative neutrality doctrine.
constraint_indexing:constraint_classification(content_neutrality_doctrine__secondary_effects_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSTITUTIONAL SCHOLAR (TANGLED ROPE) — Constrained by the doctrine's internal contradictions (category is content-defined but treated as content-neutral) and by the costs of publishing counterargument in a doctrine-affirming field. Benefits from the doctrine's continued stability (career advancement through mastery of settled law) while bearing the cost of epistemic incoherence. Mixed coordination and extraction: the doctrine coordinates judicial review doctrine but extracts theoretical coherence.
constraint_indexing:constraint_classification(content_neutrality_doctrine__secondary_effects_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MUNICIPAL GOVERNMENT (ROPE) — Primary beneficiary. The secondary effects doctrine enables coordination: legitimate zoning to address documented harms (crime, blight) can proceed without strict scrutiny. The doctrine's fiction (treating content-defined categories as content-neutral where secondary effects justify them) permits regulation that would be impermissible under strict content scrutiny. Experiences the constraint as a solution to the coordination problem of regulating disruptive uses while preserving First Amendment cover.
constraint_indexing:constraint_classification(content_neutrality_doctrine__secondary_effects_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: APPELLATE COURTS (TANGLED ROPE) — Organized to apply the doctrine but constrained by its internal contradictions. Courts acknowledge the fiction ('secondary effects doctrine permits content-defined categories to be treated as content-neutral when effects justify') while enforcing the framework. Benefits from the doctrine's providing a stable categorical structure for judicial review; bears the cost of cognitive dissonance and loses legitimacy when the fiction becomes widely visible. Active enforcement required to maintain the inconsistency.
constraint_indexing:constraint_classification(content_neutrality_doctrine__secondary_effects_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGISLATIVE REFORM MOVEMENT (SCAFFOLD) — Powerful actors (civil liberties advocates, academic reformers, some state legislatures) see the secondary effects doctrine as a temporary expedient that can be replaced by more coherent doctrine. Explicit sunset mechanism: Reed v. Town of Gilbert (2015) sharpened the facial-test trigger, creating pressure for doctrine revision. Exit path visible: states and federal legislation can establish explicit secondary-effects tests with clear content-neutral criteria (decibel limits, setback distances) that don't require the fiction of treating content-defined categories as neutral. Extraction is low because these agents have agency and see the constraint as dissolvable.
constraint_indexing:constraint_classification(content_neutrality_doctrine__secondary_effects_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some category-based regulation appears necessary to address localized harms (crime clusters near adult venues, blight in commercial districts). The observer risks naturalizing the secondary effects doctrine as an immutable feature of how republics manage conflicting interests (free expression vs. public order). However, the structural data contradicts this: the constraint is a doctrine of judicial review (contingent institutional), not a law of nature. The engine's false summit detector will identify this as naturalization.
constraint_indexing:constraint_classification(content_neutrality_doctrine__secondary_effects_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(content_neutrality_doctrine__secondary_effects_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(content_neutrality_doctrine__secondary_effects_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(content_neutrality_doctrine__secondary_effects_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(content_neutrality_doctrine__secondary_effects_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(content_neutrality_doctrine__secondary_effects_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint suppresses disfavored speech (adult businesses) under the guise of neutrality, but the suppression is not total — empirical secondary effects do exist in some cases, and municipalities have legitimate interest in nuisance abatement. The extractiveness reflects that the doctrine permits suppression where genuine secondary effects justify it, but also permits suppression where the secondary effects claim is pretextual. The measurement trajectory (0.42 → 0.52 over the interval) shows increasing extractiveness as the secondary effects doctrine has been applied more expansively post-City of Renton v. Playtime Theatres (1986). Suppression (0.48): Moderate. The secondary effects doctrine relaxes suppression requirements compared to strict content scrutiny (which would be near-total suppression) but maintains suppression of the disfavored category by permitting rational-basis review where secondary effects can be shown. The suppression is not achieved through prohibition but through zoning dispersal, which is a softer mechanism. Theater ratio (0.68): High. The doctrine's reliance on the fiction that content-defined categories can be treated as content-neutral constitutes theatrical performance — courts acknowledge the fiction while enforcing it. The theater reflects the cognitive dissonance between the category definition (content-based) and the review standard applied (content-neutral scrutiny).
 *
 * PERSPECTIVAL GAP:
 *   The primary gap is between the beneficiary's experience (municipal coordination enabled by the fiction) and the victim's experience (suppression masked by nominal neutrality). The secondary gap is between the scholar's and the court's perspectives: both acknowledge the fiction, but the scholar can theorize it as incoherent while the court must enforce it as doctrine. The analytical observer risks collapse to a mountain perspective (regulation of disruptive uses is inherent to governance) that naturalizes the contingent doctrinal choice. The Reed sharpening created a third gap: between pre-Reed doctrine (fiction relatively stable) and post-Reed doctrine (facial-test scrutiny threatens to expose the fiction, creating pressure for legislative reform).
 *
 * DIRECTIONALITY LOGIC:
 *   The municipal government is the beneficiary (derives d from arbitrage exit + coordination function). Adult business owners are victims (derive d from trapped exit + disfavored category). Courts are organized enforcers (derive d from constrained exit + institutional power). The fiction enables the municipal government to experience low effective extraction (they gain coordination benefit without strict scrutiny) while the adult business owner experiences high extraction (suppression through zoning that nominally addresses secondary effects). The scholar experiences moderate extraction (benefits from doctrine stability, bears cost of incoherence). The legislative reformer experiences low extraction (sees exit path through statutory alternatives).
 *
 * MANDATROPHY ANALYSIS:
 *   The secondary effects reading resolves the mandatrophy by showing that tangled rope classification is correct: the doctrine contains both genuine coordination function (municipalities address documented nuisances) and asymmetric extraction (disfavored speech is suppressed under the fiction of neutrality). The fiction IS the extraction mechanism — treating a content-defined category as content-neutral permits rational-basis review instead of strict scrutiny, enabling suppression that would be impermissible if the category's content-definitional character were acknowledged. The snare perspective (adult business owner) sees only extraction because the owner experiences the zoning as suppression regardless of whether secondary effects exist. The rope perspective (municipal government) sees only coordination because the government benefits from the doctrine permitting nuisance abatement without strict scrutiny. The tangled rope perspective (courts, scholars) captures both: the doctrine coordinates legitimate regulation while extracting through the fiction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_secondary_effects_verification,
    'Do adult businesses actually generate documented crime and blight at rates sufficient to justify zoning restrictions, or does the secondary effects doctrine permit suppression of disfavored speech under the guise of effect-based regulation?',
    'Controlled empirical comparison: crime and blight rates in zones with adult businesses vs. control zones with other commercial uses; statistical analysis of whether secondary effects zoning removes documented nuisances or simply excludes disfavored speech',
    'If effects are genuine and zoning addresses them: constraint is legitimate tangled rope with real coordination function. If effects are pretextual or overstated: constraint is snare masquerading as rope; suppression is actual and high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_secondary_effects_verification, empirical, 'Whether documented secondary effects justify zoning restrictions or serve as pretext').

omega_variable(
    category_content_definitional_loop,
    'Is ''adult business'' a content-neutral category defining an activity type, or is it fundamentally content-defined (identifying businesses based on the sexual content they display), creating a logical incoherence in the neutrality claim?',
    'Doctrinal analysis: examine whether ''adult business'' can be defined without reference to the content (sexually explicit materials/conduct), or whether the category is necessarily defined by content type. Compare to genuinely content-neutral categories (setback distance, hours of operation) that do not require identifying specific content.',
    'If content-defined: the secondary effects doctrine is an acknowledged fiction permitting suppression by relabeling. Extractiveness remains high; suppression is masked but not reduced. If somehow content-neutral: the doctrine is coherent and the snare perspective is misclassifying.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(category_content_definitional_loop, conceptual, 'Whether ''adult business'' is necessarily content-defined or can be defined content-neutrally').

omega_variable(
    reed_facial_test_impact,
    'How does Reed v. Town of Gilbert (2015) sharpening the facial-test trigger affect the secondary effects doctrine''s viability? Does the doctrine survive as an exception, or does it require reformulation to withstand facial-test scrutiny?',
    'Post-Reed case law analysis: survey decisions applying secondary effects doctrine after Reed; identify whether courts treat the doctrine as surviving Reed or as requiring explicit reformulation with more rigorous secondary-effects showing',
    'If doctrine survives intact: the fiction remains stable; extractiveness maintained. If reformulated: the scaffold perspective is correct, and legislated alternatives with explicit content-neutral criteria emerge; extractiveness declines over time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reed_facial_test_impact, empirical, 'Whether Reed reshapes or stabilizes the secondary effects doctrine').

omega_variable(
    reading_vs_compelled_speech_foreclosure,
    'Does the secondary effects reading logically foreclose the compelled speech reading of content neutrality, or do the two readings coexist as different aspects of the same doctrine?',
    'Doctrinal analysis: examine whether accepting secondary effects as a basis for content-neutral suppression logically commits one to rejecting compelled speech doctrine (the state cannot require speech any more than forbid it). Or do courts hold both simultaneously?',
    'If foreclosure: the two readings are incompatible commitments. If coexistence: the doctrine contains internal contradiction that permits multiple simultaneous readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_compelled_speech_foreclosure, conceptual, 'Whether secondary effects and compelled speech readings are logically compatible').

omega_variable(
    fiction_explicit_acknowledgment,
    'How do courts and scholars acknowledge the secondary effects fiction? Is the contradiction explicit (courts saying ''this is content-defined but we treat it as neutral'') or implicit (courts eliding the category definition)?',
    'Doctrinal literature analysis: search Supreme Court and appellate opinions for explicit acknowledgment vs. implicit evasion of the category-definition problem',
    'If explicit: the doctrine is transparent fiction with measured cognitive dissonance. If implicit: the constraint operates through obscurity rather than coherent framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fiction_explicit_acknowledgment, empirical, 'Degree of explicit acknowledgment of the secondary effects doctrine''s logical incoherence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(content_neutrality_doctrine__secondary_effects_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cntse_tr_t0, content_neutrality_doctrine__secondary_effects_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(cntse_tr_t20, content_neutrality_doctrine__secondary_effects_reading, theater_ratio, 20, 0.62).
narrative_ontology:measurement(cntse_tr_t40, content_neutrality_doctrine__secondary_effects_reading, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(cntse_be_t0, content_neutrality_doctrine__secondary_effects_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cntse_be_t20, content_neutrality_doctrine__secondary_effects_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(cntse_be_t40, content_neutrality_doctrine__secondary_effects_reading, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(content_neutrality_doctrine__secondary_effects_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(content_neutrality_doctrine__secondary_effects_reading, content_neutrality_doctrine__compelled_speech_reading).
narrative_ontology:affects_constraint(content_neutrality_doctrine__secondary_effects_reading, content_neutrality_doctrine__reed_facial_test_reading).
narrative_ontology:affects_constraint(content_neutrality_doctrine__secondary_effects_reading, zoning_secondary_effects__crime_blight_justification).

% DUAL FORMULATION NOTE:
% The secondary effects reading is one aspect of the broader content_neutrality_doctrine kernel. Sibling readings (compelled speech, Reed facial test) are separate constraints with different ε values and different mechanisms. The secondary effects reading emphasizes the fiction permitting content-defined categories to be regulated under content-neutral scrutiny; the Reed reading emphasizes the facial-test trigger that threatens to expose the fiction; the compelled speech reading treats neutrality's logical other half. Each reading has its own extractiveness profile: secondary effects reading ε=0.52 (moderate extraction through fiction); Reed reading ε≈0.38 (higher scrutiny reduces extraction); compelled speech reading ε≈0.35 (symmetrical constraint reduces extraction). Network links track doctrinal interdependence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
