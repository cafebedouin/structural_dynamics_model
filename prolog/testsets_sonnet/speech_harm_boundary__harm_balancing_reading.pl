% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__harm_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__harm_balancing_reading, []).

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
 *   constraint_id: speech_harm_boundary__harm_balancing_reading
 *   human_readable: Speech-Harm Boundary — Proportionality Balancing Reading
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This constraint models one specific reading of the speech-harm boundary
 *   kernel: the proportionality-balancing reading, under which speech
 *   protection is presumptive but yields case-by-case to demonstrated harm
 *   through multi-factor judicial weighing. This is structurally distinct
 *   from the absolutist_reading (near-total protection, extreme harm
 *   threshold) and the dignity_reading (personhood-denying speech
 *   categorically excluded ex ante). Under this reading, unprotected
 *   categories are broader than the absolutist reading's but narrower and
 *   more evidence-dependent than the dignity reading's categorical exclusions
 *   — moderate epsilon reflects genuine, but unpredictable and unevenly
 *   distributed, restriction costs borne by speakers whose material is
 *   contested but not categorically excluded.
 *
 * KEY AGENTS:
 *   - courts_administering_balancing_tests: institutional agenda-setter administering the shifting weight formula
 *   - targeted_minority_groups and harassment_complainants: beneficiaries of a demonstrated-harm remedy route
 *   - controversial_speakers, advocacy_organizations_near_the_line, satirists_and_provocateurs: payers bearing unpredictability and self-censorship costs
 *   - platform_intermediaries: powerful agenda-setter/payer with cross-jurisdictional exit unavailable to individual speakers
 *   - absolutist_free_speech_advocates: excluded voice arguing the harm exception swallows the presumption
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, 0.42).
domain_priors:suppression_score(speech_harm_boundary__harm_balancing_reading, 0.48).
domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__harm_balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__harm_balancing_reading, "Speech-Harm Boundary — Proportionality Balancing Reading").
narrative_ontology:topic_domain(speech_harm_boundary__harm_balancing_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__harm_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__harm_balancing_reading, '9136df45-d520-4cfa-86ac-4ad0cc912bf4').
narrative_ontology:cs_kernel_codification('9136df45-d520-4cfa-86ac-4ad0cc912bf4', distributed).
narrative_ontology:cs_authority_grounding('9136df45-d520-4cfa-86ac-4ad0cc912bf4', practice).
narrative_ontology:cs_interpretation_layer_present('9136df45-d520-4cfa-86ac-4ad0cc912bf4').
narrative_ontology:cs_reading_relation('9136df45-d520-4cfa-86ac-4ad0cc912bf4', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9136df45-d520-4cfa-86ac-4ad0cc912bf4', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('9136df45-d520-4cfa-86ac-4ad0cc912bf4', foundational, harm_must_be_demonstrated_not_presumed).
narrative_ontology:cs_axiom_status(harm_must_be_demonstrated_not_presumed, holdable).
narrative_ontology:cs_axiom_grounding('9136df45-d520-4cfa-86ac-4ad0cc912bf4', harm_must_be_demonstrated_not_presumed, empirically_contingent).
narrative_ontology:cs_axiom('9136df45-d520-4cfa-86ac-4ad0cc912bf4', foundational, protection_and_restriction_are_commensurable_via_weighing).
narrative_ontology:cs_axiom_status(protection_and_restriction_are_commensurable_via_weighing, holdable).
narrative_ontology:cs_axiom_grounding('9136df45-d520-4cfa-86ac-4ad0cc912bf4', protection_and_restriction_are_commensurable_via_weighing, instrumental).
narrative_ontology:cs_reference_frame('9136df45-d520-4cfa-86ac-4ad0cc912bf4', case_by_case_proportionality_adjudication).
narrative_ontology:cs_drift_state('9136df45-d520-4cfa-86ac-4ad0cc912bf4', contemporary_platform_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9136df45-d520-4cfa-86ac-4ad0cc912bf4', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, targeted_minority_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, harassment_complainants).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, courts_administering_balancing_tests).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, controversial_speakers).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, advocacy_organizations_near_the_line).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, satirists_and_provocateurs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, legislatures_and_regulators).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, platform_intermediaries).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, proportionality_as_constitutional_method).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, harm_principle_as_speech_limit).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate case-by-case whether demonstrated harm outweighs the presumptive value of the speech at issue, applying multi-factor proportionality tests (severity, targeting, context, alternative channels). They set and revise the doctrinal weights and control which harms count as cognizable, effectively administering the boundary they claim only to interpret.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, courts_administering_balancing_tests, agenda_setter,
    institutional, generational, analytical, national).

% Gain standing to seek restriction or remedy when speech demonstrably causes group-directed harm (harassment, group libel, incitement-adjacent hate speech). Cannot exit the broader speech environment; their protection depends entirely on courts finding harm sufficiently demonstrated, which is contested and inconsistent.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, targeted_minority_groups, beneficiary,
    moderate, biographical, constrained, national).

% Individuals subjected to targeted harassing speech who can invoke the harm threshold for redress. They bear the evidentiary burden of demonstrating harm under a standard that shifts case to case, and have no exit from the platforms or communities where the harassment occurs.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, harassment_complainants, beneficiary,
    powerless, immediate, trapped, local).

% Speak on contested political, religious, or social topics and bear the restriction, liability, or platform sanction when a court or intermediary finds their speech crosses the demonstrated-harm line. The line's proportionality weighting is unpredictable in advance, so they self-censor to avoid the risk of an adverse balancing outcome — a cost imposed even absent an actual finding.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, controversial_speakers, payer,
    moderate, biographical, constrained, national).

% Organizations engaged in sharp-edged political advocacy, protest rhetoric, or group criticism that risks classification as group libel or harassment. They must fund legal defense and doctrinal monitoring to know where the shifting proportionality line currently sits, an ongoing compliance cost with no clean exit short of moderating their advocacy.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, advocacy_organizations_near_the_line, payer,
    organized, generational, constrained, national).

% Rely on provocation, exaggeration, and offense as core expressive technique. Under a harm-balancing standard their work is judged partly by subjective and cumulative audience impact, which is harder to defend in advance than under a bright-line rule; individually powerless against the institutional weighting process.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, satirists_and_provocateurs, payer,
    powerless, immediate, constrained, national).

% Enact hate-speech, harassment, and group-libel statutes that operationalize the harm threshold, then benefit from the resulting discretionary enforcement power over contested political and social speech.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, legislatures_and_regulators, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__harm_balancing_reading, legislatures_and_regulators, beneficiary).

% Implement content-moderation policies modeled on the harm-balancing standard, absorbing liability risk for under-moderation while facing user backlash for over-moderation. They can relocate compliance operations across jurisdictions, giving them more exit than any individual speaker.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, platform_intermediaries, agenda_setter,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__harm_balancing_reading, platform_intermediaries, payer).

% Argue that any demonstrated-harm exception swallows the presumption and that only content-neutral, viewpoint-blind rules should govern. Their objection is heard in dissenting opinions and academic literature but does not control the doctrine's operative test.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, absolutist_free_speech_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__harm_balancing_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_harm_boundary__harm_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable doctrinal mechanism for resolving the genuine tension between expressive liberty and concrete harms (targeted harassment, group defamation, incitement-adjacent speech) without either an absolute speech shield or a categorical harm veto — allowing courts to calibrate protection to context rather than applying a single rule to all speech.
% TRANSFER_FUNCTION: Shifts the cost of unpredictability from harmed parties (who gain a route to redress) onto speakers operating near the contested boundary, who absorb self-censorship costs, litigation risk, and platform sanction risk in exchange for the possibility of protection when harm cannot be demonstrated.
% ABSENT_VOICES: Absolutist free-speech advocates and, from the opposite direction, dignity-reading proponents who believe the harm test still under-protects targeted groups by requiring proof of harm rather than treating personhood-denying speech as categorically unprotected — both are present in the surrounding debate but neither controls the operative doctrinal test.
% DISAPPEARANCE_RATIONALE: If the proportionality-balancing standard vanished overnight, courts would have to fall back to either a near-absolute protection rule or a categorical harm-based exclusion rule; ongoing harassment, hate-speech, and group-libel litigation would resolve under a wholly different logic, and the current population of contested cases (workplace harassment speech, group-targeted online abuse, protest-adjacent advocacy) would be decided by a different mechanism entirely.
% FOUNDING_PROBLEM: Neither an absolute speech shield nor a harm-based categorical exclusion adequately handled the actual variety of speech disputes courts faced — some clearly protected, some clearly harmful, many genuinely contested on the facts — so proportionality balancing was adopted to give courts a case-sensitive tool.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars outside any advocacy tradition (proportionality-doctrine literature spanning multiple jurisdictions) attest that the case-by-case variety of speech-harm disputes remains real and unresolved by either bright-line alternative; dissenting judges from both the absolutist and dignity traditions corroborate that hard cases persist, even while disputing which fixed rule should replace balancing.
narrative_ontology:disappearance_verdict(speech_harm_boundary__harm_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__harm_balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__harm_balancing_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_harm_boundary__harm_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__harm_balancing_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__harm_balancing_reading_tests).
:- end_tests(speech_harm_boundary__harm_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the harm-balancing standard does impose real, if inconsistent, restriction costs on a defined population of near-the-line speakers, but the presumption of protection is genuine and most speech clears it without incident. Suppression (0.48) reflects the chilling effect of doctrinal unpredictability — speakers self-censor in advance of any actual finding because the proportionality weighting cannot be known ex ante. Theater ratio is comparatively low (0.28): the balancing apparatus does real adjudicative work rather than merely performing scrutiny, though its share of purely symbolic gatekeeping activity has risen modestly as caseloads and doctrinal elaboration have grown over the interval.
 *
 * DIRECTIONALITY LOGIC:
 *   Targeted minority groups and harassment complainants sit toward the beneficiary end: the reading gives them a route to redress unavailable under the absolutist reading. Controversial speakers, advocacy organizations, and satirists sit toward the target end: they bear the cost of an unpredictable proportionality test even when ultimately vindicated, because litigation risk and self-censorship are incurred before any finding. Courts and legislatures occupy the agenda-setter position — they administer and can revise the weighting without directly paying its costs. Platform intermediaries hold unusually strong exit (jurisdictional arbitrage) relative to individual speakers, which the derivation captures without needing an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine variety in speech-harm disputes that neither bright-line alternative handles well — remains live by cross-jurisdictional scholarly and judicial corroboration, distinguishing this from a zombie doctrine maintained by inertia. The tangled_rope classification (rather than snare) is warranted because the coordination function is real and independently corroborated, not merely claimed by the beneficiaries of restriction; but the requirement of active enforcement and a genuine payer class (speakers absorbing unpredictability costs) means it is not a pure rope either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_speech_harm_boundary,
    'Among the three live readings of the speech-harm boundary kernel (absolutist, dignity, harm-balancing), which reading a given jurisdiction or era actually operationalizes is itself contested — the same statutory or constitutional text can be read as instantiating any of the three.',
    'Track which doctrinal test (categorical exclusion vs. multi-factor balancing vs. near-absolute protection with narrow carve-outs) actually controls case outcomes in a given jurisdiction over time; a jurisdiction can drift between readings without formal amendment.',
    'If a jurisdiction claiming the harm-balancing reading in fact applies dignity-reading categorical exclusions in practice, the beneficiary/victim structure and epsilon authored here would not describe that jurisdiction''s actual operation — a separate story would be needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_speech_harm_boundary, conceptual, 'Which kernel reading a given jurisdiction actually instantiates in practice may diverge from its declared doctrine.').

omega_variable(
    harm_balancing_sibling_delta_location,
    'Where exactly does the harm-balancing reading''s proportionality test diverge from the absolutist reading''s near-absolute protection, and from the dignity reading''s categorical exclusion — is the disagreement located in the harm threshold, the evidentiary burden, or the scope of protected categories?',
    'Compare case outcomes across jurisdictions applying each reading to matched fact patterns (e.g., group-targeted online harassment) to isolate whether the outcome divergence traces to threshold height, evidentiary standard, or categorical scope.',
    'Locating the disagreement precisely determines which structural element (threshold, evidence, or category) would need to shift for one reading to functionally converge with another, which bears on whether the three readings are genuinely distinct constraints or points on a single continuum.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_balancing_sibling_delta_location, conceptual, 'Structural location of the disagreement between sibling readings of the speech-harm kernel.').

omega_variable(
    proportionality_predictability_erosion,
    'Is the rising suppression_requirement trajectory (self-censorship from doctrinal unpredictability) an inherent feature of any case-by-case balancing test, or a symptom of this particular doctrine''s weighting factors becoming more numerous and less determinate over time?',
    'Compare chilling-effect measures (self-censorship surveys, pre-publication legal review rates) across balancing regimes with stable versus expanding factor lists.',
    'If inherent to balancing as such, no doctrinal reform within this reading reduces the cost; if a symptom of factor proliferation, simplifying the test could lower suppression without abandoning the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_predictability_erosion, empirical, 'Whether rising chilling effects are intrinsic to balancing tests or a fixable symptom of doctrinal complexity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__harm_balancing_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__harm_balancing_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(spee_tr_t8, speech_harm_boundary__harm_balancing_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(spee_tr_t16, speech_harm_boundary__harm_balancing_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(spee_tr_t24, speech_harm_boundary__harm_balancing_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(spee_tr_t32, speech_harm_boundary__harm_balancing_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__harm_balancing_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(spee_be_t8, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(spee_be_t16, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 16, 0.37).
narrative_ontology:measurement(spee_be_t24, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(spee_be_t32, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(spee_su_t8, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(spee_su_t16, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(spee_su_t24, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 24, 0.46).
narrative_ontology:measurement(spee_su_t32, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 32, 0.47).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__harm_balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% Three sibling constraints decompose the natural-language 'speech-harm boundary' concept per the ε-invariance principle: absolutist_reading (near-total protection), dignity_reading (categorical exclusion of personhood-denying speech), and this harm_balancing_reading (case-by-case proportionality). Each carries its own epsilon, beneficiary/victim structure, and claimed type. They are linked via affects_constraints because doctrinal shifts in one reading (e.g., a jurisdiction moving from balancing toward categorical exclusion) directly affect which reading a given legal system is understood to instantiate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
