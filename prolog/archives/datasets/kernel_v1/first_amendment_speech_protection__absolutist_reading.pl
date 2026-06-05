% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: first_amendment_speech_protection__absolutist_reading
 *   human_readable: First Amendment Absolutist Reading: 'No Law' as Categorical Protection
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The absolutist reading of the First Amendment declares that 'no law'
 *   means no law — the categorical protection of speech admits only narrow
 *   historical exclusions (incitement, true threats, defamation in limited
 *   contexts). This reading is one of three structurally distinct readings of
 *   the contested kernel 'first_amendment_speech_protection.' The absolutist
 *   reading maximizes the protected speech set by declaring that
 *   content-based regulations, viewpoint restrictions, hate speech laws, and
 *   harassment remedies are categorically forbidden. This constraint
 *   distributes benefits and costs asymmetrically: speakers with
 *   institutional backing and majority support experience pure coordination
 *   gain (Rope perspective), while historically targeted minorities and
 *   harassment victims experience pure extraction (Snare perspective). The
 *   reading's tension derives from its implicit normative claim: categorical
 *   protection is worth the externalization of suppression costs onto
 *   minorities. This is not a claim about the meaning of text — all three
 *   sibling readings claim to interpret the same text accurately — but a
 *   claim about institutional design: which constitutional arrangement best
 *   serves justice? The absolutist reading chooses speaker liberty over
 *   minority protection, naturalizing this choice as the only coherent
 *   interpretation of 'no law.'
 *
 * KEY AGENTS:
 *   - Speakers with Institutional/Majority Backing (institutional/arbitrage) — beneficiary; experiences constraint as coordination gain; institutional media, corporations, organized political movements
 *   - Targeted Minorities and Harassment Victims (powerless/trapped) — primary victims; bears full suppression cost; individuals, marginalized communities, historically silenced groups
 *   - Historically Silenced Communities (moderate/constrained) — secondary victims; experiences entrenched power asymmetry; structural subordination reinforced by asymmetric speech rights
 *   - Counter-Speech Movements and Civil Rights Advocates (organized/constrained) — organized opposition; benefits from speaker protection but oppressed by enforcement of categorical prohibition on protective laws
 *   - Federal Judiciary (institutional/arbitrage) — operator of the constraint through doctrine; maintains piton performance (declares absolutism, implements exceptions) to arbitrage between text and political sustainability
 *   - Analytical Observer (analytical/analytical) — risks naturalizing contingent reading choice as inevitable constitutional truth
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, 0.52).
domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, 0.68).
domain_priors:theater_ratio(first_amendment_speech_protection__absolutist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__absolutist_reading, snare).
narrative_ontology:human_readable(first_amendment_speech_protection__absolutist_reading, "First Amendment Absolutist Reading: 'No Law' as Categorical Protection").
narrative_ontology:topic_domain(first_amendment_speech_protection__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__absolutist_reading, 'aabc1512-40e0-4209-aa0e-45fcdc94bf75').
narrative_ontology:cs_kernel_codification('aabc1512-40e0-4209-aa0e-45fcdc94bf75', fixed_text).
narrative_ontology:cs_authority_grounding('aabc1512-40e0-4209-aa0e-45fcdc94bf75', lineage).
narrative_ontology:cs_interpretation_layer_present('aabc1512-40e0-4209-aa0e-45fcdc94bf75').
narrative_ontology:cs_reading_relation('aabc1512-40e0-4209-aa0e-45fcdc94bf75', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_reading_relation('aabc1512-40e0-4209-aa0e-45fcdc94bf75', first_amendment_speech_protection__categorical_balancing_reading, influences).
narrative_ontology:cs_axiom('aabc1512-40e0-4209-aa0e-45fcdc94bf75', foundational, no_law_means_categorical).
narrative_ontology:cs_axiom_status(no_law_means_categorical, holdable).
narrative_ontology:cs_axiom_grounding('aabc1512-40e0-4209-aa0e-45fcdc94bf75', no_law_means_categorical, deontological).
narrative_ontology:cs_axiom('aabc1512-40e0-4209-aa0e-45fcdc94bf75', foundational, speaker_liberty_priority_over_harm_reduction).
narrative_ontology:cs_axiom_status(speaker_liberty_priority_over_harm_reduction, holdable).
narrative_ontology:cs_axiom_grounding('aabc1512-40e0-4209-aa0e-45fcdc94bf75', speaker_liberty_priority_over_harm_reduction, deontological).
narrative_ontology:cs_reference_frame('aabc1512-40e0-4209-aa0e-45fcdc94bf75', categorical_speech_protection_framework).
narrative_ontology:cs_drift_state('aabc1512-40e0-4209-aa0e-45fcdc94bf75', contemporary_hate_speech_and_harassment_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('aabc1512-40e0-4209-aa0e-45fcdc94bf75', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, speakers_with_majority_backing).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, commercial_speech_interests).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, organized_political_movements).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, targeted_minorities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, harassment_targets).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, historically_silenced_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED MINORITY / HARASSMENT TARGET (SNARE) — Bears full extraction cost. The absolutist reading maximizes speaker protection at the direct expense of targeted minorities' safety, dignity, and capacity to participate in public discourse. No exit option: the victim cannot leave the nation to escape speech-based harassment and incitement. The constraint operates through suppression of protective alternatives (hate speech law, doxxing remedies, harassment injunctions) — declaring them categorically forbidden under 'no law' framing.
constraint_indexing:constraint_classification(first_amendment_speech_protection__absolutist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HISTORICALLY SILENCED COMMUNITIES (SNARE) — The absolutist reading entrenches historical power asymmetries: dominant groups have megaphones, networks, and institutional platforms; marginalized groups have microphones and face coordinated silencing campaigns. Extraction occurs through the externalization of suppression costs. Communities must spend resources on counter-speech, self-protection, and cultural/institutional repair. Exit is constrained by social ties, economic dependency, and the universality of the First Amendment across all contexts.
constraint_indexing:constraint_classification(first_amendment_speech_protection__absolutist_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SPEAKERS WITH INSTITUTIONAL/MAJORITY BACKING (ROPE) — Benefits directly from categorical protection. Experiences the constraint as coordination: the rule 'no law' on speech eliminates costly uncertainty and legal negotiation over speech boundaries. Institutional speakers (media, corporations, political movements) can exercise arbitrage — they have alternative platforms, legal resources, and social power to amplify or shift speech. Net beneficiary experiencing pure coordination gain.
constraint_indexing:constraint_classification(first_amendment_speech_protection__absolutist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COUNTER-SPEECH MOVEMENTS AND CIVIL RIGHTS ADVOCATES (TANGLED ROPE) — Organized agents pushing back against the absolutist reading. Experience mixed benefit and extraction: they benefit from being able to organize, protest, and advocate without prior restraint (coordinating function), but face suppression of their attempts to create protective structures for targeted minorities. The constraint enforces that their protective speech (doxxing remedies, hate speech codes) is categorically forbidden — active enforcement of the 'no law' principle against their proposals.
constraint_indexing:constraint_classification(first_amendment_speech_protection__absolutist_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL JUDICIARY'S FIRST AMENDMENT JURISPRUDENCE (PITON) — The absolutist reading is largely performative at the operational level. Courts have carved out narrow historical exceptions (incitement, true threats, harassment in specific contexts) and refuse to apply absolutism uniformly. The judiciary sees the 'no law' framing as a canonical but degraded principle — invoked to block protective measures while being quietly overridden in practice. Theater ratio high because the absolutist principle is declared and defended but not actually enforced categorically. Courts arbitrage between text and outcomes, maintaining the theater of absolutism while allowing exceptions.
constraint_indexing:constraint_classification(first_amendment_speech_protection__absolutist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the absolutist reading appears as an immutable principle: free speech is a categorical good, the Framers intended categorical protection ('no law' means no law), and any exception to categorical protection is a slippery slope toward tyranny. This perspective naturalizes the reading as the only coherent interpretation of the constitutional text and First Amendment purpose. However, the structural data (beneficiaries, victims, active enforcement, suppression mechanisms) reveals this as a false summit — the 'naturalness' of the absolutist reading conceals that it is a specific reading chosen from contested alternatives.
constraint_indexing:constraint_classification(first_amendment_speech_protection__absolutist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__absolutist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(first_amendment_speech_protection__absolutist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(first_amendment_speech_protection__absolutist_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(first_amendment_speech_protection__absolutist_reading, TR),
    TR >= 0.70.

:- end_tests(first_amendment_speech_protection__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The absolutist reading extracts by externalizing suppression costs onto minorities. Speakers benefit from categorical protection; minorities bear costs of organized harassment, incitement, and systemic oppression enabled by the reading. The extraction is not maximal (0.66+) because beneficiaries do not receive direct rents and some speech is still legally prohibited (narrow exceptions). Suppression (0.68): High. The constraint operates by suppressing alternative protective mechanisms: hate speech law (forbidden), harassment remedies (categorically blocked), platform liability for user speech (suppressed via Section 230 interpretation grounded in First Amendment absolutism), incitement thresholds (set extremely high). The suppression is structural and active — courts enforce the categorical protection by striking down protective measures. Theater ratio (0.35): Low-moderate. The absolutist reading is less performative than the piton perspective's doctrine-in-practice (which invokes absolutism while carving exceptions). The reading maintains theoretical clarity at the cost of real harm externalization — it does what it says, making the extraction visible rather than hidden.
 *
 * PERSPECTIVAL GAP:
 *   The absolutist reading produces maximum perspectival divergence. The beneficiary (institutional speakers) sees pure coordination — categorical clarity enables free expression without constant legal negotiation. The victim (targeted minority) sees pure extraction — the constraint transfers speaker liberty gains directly into suppression costs. The organized opposition (counter-speech movements) sees tangled rope — they benefit from counter-speech protection but are oppressed by prohibition on protective law. The judiciary sees piton — the reading is doctrinally central but operationally undermined by narrow exceptions applied in context. The analytical observer risks seeing mountain — naturalizing the reading as the only coherent interpretation of constitutional text — but the structural data reveals false summit: the reading is one choice from contested alternatives, chosen for ideological/institutional reasons rather than textual necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from structural position: Speakers with institutional backing are beneficiaries with arbitrage exit (d ≈ 0.10, highly negative chi) — they have alternative platforms, resources, legal power. Targeted minorities are victims with no exit (d ≈ 0.95, highly positive chi) — they cannot leave the nation or escape speech effects. Organized counter-speech movements are constrained but organized (d ≈ 0.55, moderate-high chi) — they have agency and coordination but face active suppression of their protective proposals. The piton judiciary sees the constraint as degraded but operational (d derived from arbitrage + institutional power, but offset by internal recognition of exception necessity). The analytical observer is at risk of identity lock — their professional commitment to textual interpretation may prevent them from seeing the political choice embedded in the reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The absolutist reading resolves mandatrophy by exposing the choice: First Amendment doctrine can be organized around speaker liberty (maximizing protection, externalizing harm onto minorities) or around harm prevention (minimizing speaker protection, externalizing liberty costs onto speakers). The constraint does not prove that 'no law' is the only coherent reading — it demonstrates that reading selection is a normative choice disguised as textual interpretation. The mandatrophy is present and unresolved: neither 'categorical protection is constitutional principle' nor 'harm reduction is constitutional principle' flows inevitably from the text. The absolutist reading's mandatrophy resolution consists of asserting categorical priority (speaker liberty > minority protection) while denying it is an assertion (claiming it is the only coherent reading). The engine's false summit detector will flag this as naturalization of a contingent choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_selection_mechanism,
    'What institutional or ideological factors determine adoption of the absolutist reading over the harm_limited or categorical_balancing readings?',
    'Historical analysis of when each reading became dominant in different circuits; identification of ideological coalitions supporting each reading; examination of funding and institutional backing for absolutist legal organizations (CATO, libertarian think tanks) vs harm-limited advocates (civil rights organizations)',
    'If reading selection is driven by ideology/funding rather than textual analysis: the absolutist reading is a constructed constraint (snare), not a natural law. If reading selection is driven by superior textual argument: mountain classification may be defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_mechanism, empirical, 'Institutional mechanisms driving selection of absolutist reading').

omega_variable(
    harm_measurement_impossibility,
    'Is harm caused by speech (harassment, incitement, systemic oppression reinforcement) measurable as a constraint on First Amendment scope, or is harm measurement inherently indeterminate?',
    'Comparative empirical study: correlate hate speech legislation with measurable reduction in hate crimes, community safety metrics, targeted group participation rates; contrast with jurisdictions using absolutist approach. Identify whether harm thresholds can be operationalized or remain perpetually contested.',
    'If harm measurable and responsive to law: harm_limited reading gains structural support. If harm perpetually contested: absolutist reading''s denial of harm-based limits gains support but reveals itself as a value choice rather than empirical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_measurement_impossibility, empirical, 'Whether speech-caused harm is measurable for First Amendment scope decisions').

omega_variable(
    power_asymmetry_normativity,
    'Does the absolutist reading''s externalization of suppression costs onto historically silenced communities constitute a normative feature or a regrettable side effect?',
    'Textual analysis of absolutist arguments: do they acknowledge or dismiss the asymmetry between resourced and unresourced speakers? Historical comparison: did the Framers intend categorical protection to apply to all speakers equally, or did they understand the First Amendment as protection for a specific (dominant) coalition? Normative philosophy: should a reading that externalizes costs onto minorities be preferred despite structural benefits?',
    'If acknowledged normative feature: the reading is explicitly choosing speaker liberty over minority protection (snare classification sustained). If dismissed as side effect: the reading is engaged in naturalization (false summit). If Framers understood asymmetry: reading selection was already political. If Framers did not: modern application is extrapolation beyond textual intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_asymmetry_normativity, conceptual, 'Whether power asymmetry is normative feature or denied side effect of absolutism').

omega_variable(
    sibling_reading_foreclosure_vs_coexistence,
    'Does the absolutist reading logically foreclose the harm_limited reading, or do they represent distinct normative commitments that can coexist in different frameworks?',
    'Logical analysis: test whether ''no law'' (absolutist) is logically contradictory with ''law protecting against demonstrable harm'' (harm_limited) within a single interpretive framework. Test whether both readings can be held by different parties without internal inconsistency. Examine case law: do courts actually treat readings as mutually exclusive or as alternatives in play?',
    'If logically contradictory: forecloses relation. If logically independent: coexists_with relation. If contingently contradictory (depend on auxiliary premises): influences relation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_vs_coexistence, conceptual, 'Logical relationship between absolutist and harm_limited readings').

omega_variable(
    historical_narrow_exclusions_status,
    'Are the ''narrow historical exclusions'' (incitement, true threats, etc.) genuinely narrow and historically grounded, or do they represent implicit rejections of categorical absolutism?',
    'Genealogical analysis of exception doctrine: trace how incitement, true threats, and harassment doctrines evolved; identify whether courts added exceptions because absolutism proved unworkable or because exceptions existed at the Founding. If exceptions accumulated as doctrine matured: suggest absolutism was never actually operative. If exceptions trace to the Framers: suggest they did not intend categorical protection.',
    'If exceptions are workarounds: absolutist reading is aspirational but not structural (piton classification supported). If exceptions are founding-era: absolutist reading is a modern gloss (snare for introducing extraction through selective reading). If exceptions are principle-based limits: some other reading (categorical_balancing) may be more accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_narrow_exclusions_status, empirical, 'Status of narrow exclusions in absolutist reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__absolutist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fa_abs_theater_t0, first_amendment_speech_protection__absolutist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(fa_abs_theater_t50, first_amendment_speech_protection__absolutist_reading, theater_ratio, 50, 0.32).
narrative_ontology:measurement(fa_abs_theater_t100, first_amendment_speech_protection__absolutist_reading, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(fa_abs_extractiveness_t0, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fa_abs_extractiveness_t50, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(fa_abs_extractiveness_t100, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 100, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(fa_abs_suppression_t0, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fa_abs_suppression_t50, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 50, 0.64).
narrative_ontology:measurement(fa_abs_suppression_t100, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__categorical_balancing_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__harm_limited_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, hate_speech_legal_regulation).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, platform_liability_speech_governance).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, harassment_law_boundaries).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel first_amendment_speech_protection. The three readings (absolutist, categorical_balancing, harm_limited) are structurally distinct constraints with different ε values, beneficiary/victim sets, and classification profiles. All three interpret the same constitutional text; the contest is over institutional design. Decomposition is required by ε-invariance: the readings differ in what they count as extractive vs coordinative, which boundaries are natural vs constructed, and who bears costs. Network edges link the three readings bidirectionally; they also affect downstream constraints like hate speech regulation and platform liability (which downstream constraints operationalize the choice between readings).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_speech_protection__absolutist_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
