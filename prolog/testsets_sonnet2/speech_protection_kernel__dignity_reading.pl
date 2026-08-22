% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__dignity_reading, []).

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
 *   constraint_id: speech_protection_kernel__dignity_reading
 *   human_readable: Speech Protection Conditioned on Non-Subordination (Dignity Reading)
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This story instantiates the dignity reading of the speech protection
 *   kernel: the claim that speech protection is conditional on the speech not
 *   functioning as structural subordination of a target group. Under this
 *   reading, hate speech and group libel fall outside protection not because
 *   they cause individual, provable harm (the harm_threshold reading) and not
 *   because they fail truth-discovery (the marketplace reading), but because
 *   they operate as a mechanism that entrenches a group's unequal social
 *   status. This is a distinct constraint from its four sibling readings —
 *   each has its own beneficiary/victim structure and its own epsilon, and
 *   none of the siblings are described here. The dignity reading genuinely
 *   coordinates protection for historically subordinated groups against
 *   organized degradation campaigns, but the boundary between 'subordinating'
 *   and 'merely offensive' speech is administratively drawn by agenda-setting
 *   bodies (equality agencies, campus tribunals, some courts) whose
 *   application has, per the temporal record, drifted toward broader
 *   viewpoint suppression over the interval.
 *
 * KEY AGENTS:
 *   - historically_targeted_minority_groups: Primary beneficiary (powerless/trapped) — protected from subordinating speech but cannot exit the targeted identity
 *   - equality_rights_advocates: Agenda-setter (organized/mobile) — presses courts to expand recognition of group harm
 *   - anti_discrimination_agencies: Agenda-setter (institutional/analytical) — administers the line between offense and subordination
 *   - speakers_of_disfavored_viewpoints: Primary target (moderate/constrained) — bears liability risk from ex post characterization
 *   - political_dissidents_using_inflammatory_rhetoric: Secondary target (powerless/trapped) — asymmetric application against counter-speech
 *   - constitutional_courts: Analytical observer (institutional/analytical) — sets the durable doctrinal boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, 0.58).
domain_priors:suppression_score(speech_protection_kernel__dignity_reading, 0.52).
domain_priors:theater_ratio(speech_protection_kernel__dignity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__dignity_reading, "Speech Protection Conditioned on Non-Subordination (Dignity Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__dignity_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__dignity_reading, '32f756c3-0d35-4a97-9554-c63f2ada3385').
narrative_ontology:cs_kernel_codification('32f756c3-0d35-4a97-9554-c63f2ada3385', distributed).
narrative_ontology:cs_authority_grounding('32f756c3-0d35-4a97-9554-c63f2ada3385', distributed).
narrative_ontology:cs_reading_relation('32f756c3-0d35-4a97-9554-c63f2ada3385', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('32f756c3-0d35-4a97-9554-c63f2ada3385', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('32f756c3-0d35-4a97-9554-c63f2ada3385', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('32f756c3-0d35-4a97-9554-c63f2ada3385', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('32f756c3-0d35-4a97-9554-c63f2ada3385', foundational, group_dignity_as_condition_of_protection).
narrative_ontology:cs_axiom_status(group_dignity_as_condition_of_protection, holdable).
narrative_ontology:cs_axiom_grounding('32f756c3-0d35-4a97-9554-c63f2ada3385', group_dignity_as_condition_of_protection, deontological).
narrative_ontology:cs_axiom('32f756c3-0d35-4a97-9554-c63f2ada3385', foundational, structural_group_harm_distinct_from_individual_harm).
narrative_ontology:cs_axiom_status(structural_group_harm_distinct_from_individual_harm, holdable).
narrative_ontology:cs_axiom_grounding('32f756c3-0d35-4a97-9554-c63f2ada3385', structural_group_harm_distinct_from_individual_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('32f756c3-0d35-4a97-9554-c63f2ada3385', post_war_anti_subordination_jurisprudence).
narrative_ontology:cs_drift_state('32f756c3-0d35-4a97-9554-c63f2ada3385', contemporary_platform_and_campus_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('32f756c3-0d35-4a97-9554-c63f2ada3385', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__dignity_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, historically_targeted_minority_groups).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, equality_rights_advocates).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, anti_discrimination_agencies).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, speakers_of_disfavored_viewpoints).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, political_dissidents_using_inflammatory_rhetoric).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, satirists_and_provocateurs).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, equal_dignity_as_constitutional_baseline).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, group_libel_as_cognizable_harm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of groups with a documented history of persecution who are shielded, under this reading, from speech that functions to reassert their subordinate status (organized hate campaigns, group libel, dehumanizing propaganda). They cannot exit the identity the speech targets and depend on the arrangement to keep public discourse from normalizing their subordination.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, historically_targeted_minority_groups, beneficiary,
    powerless, generational, trapped, national).

% Civil rights organizations, equality-law scholars, and litigators who press courts and legislatures to recognize structural group harm as a distinct legal category and to draw the line where speech becomes subordination. They set the doctrinal agenda for where the boundary sits and benefit institutionally (funding, standing, precedent) from the boundary's recognition.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, equality_rights_advocates, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__dignity_reading, equality_rights_advocates, beneficiary).

% Administrative and judicial bodies that operationalize the dignity standard — hate speech tribunals, human rights commissions, campus speech codes — determining case by case whether contested speech crosses from protected expression into subordinating conduct. They administer the enforcement machinery and can expand or narrow its reach.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, anti_discrimination_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Individuals and organizations expressing views on race, religion, gender, or nationality that a tribunal may characterize as subordinating rather than merely offensive. They bear liability, deplatforming, or prosecution risk turning on an ex post characterization they could not reliably predict when speaking; exit means self-censorship rather than relocation.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, speakers_of_disfavored_viewpoints, payer,
    moderate, biographical, constrained, national).

% Activists and dissidents who use harsh, group-directed rhetoric against powerful institutions or majority groups (e.g. denouncing an ethnic majority's historical conduct) and find the dignity standard applied asymmetrically against them, since the doctrine was built to protect historically subordinated groups and can be read to exclude counter-speech from dominant groups as a target category.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, political_dissidents_using_inflammatory_rhetoric, payer,
    powerless, biographical, trapped, national).

% Comedians, cartoonists, and provocateurs whose work trades in stereotype and exaggeration for satirical effect. Under the dignity standard, their work is vulnerable to being read as functioning group subordination regardless of satirical intent, and they must self-edit against an unpredictable line.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, satirists_and_provocateurs, payer,
    moderate, biographical, constrained, national).

% Apex courts that must decide, case by case, whether the dignity standard is being applied as a genuine anti-subordination test or as viewpoint suppression dressed in equality language. Their rulings set the durable boundary between this reading and its siblings.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__dignity_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__dignity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared basis for distinguishing speech that merely offends from speech that functions to entrench a group's subordinate social status, allowing courts and legislatures to protect vulnerable groups from speech-enabled degradation without abandoning speech protection altogether.
% TRANSFER_FUNCTION: Moves the burden of proving that speech is 'merely offensive' rather than 'structurally subordinating' from targeted groups onto the speaker; shifts litigation, reputational, and self-censorship costs from historically targeted groups onto speakers whose rhetoric is characterized as subordinating.
% ABSENT_VOICES: Speakers whose rhetoric is aimed at powerful or majority groups on behalf of a minority cause are often not treated as needing the doctrine's protection even though their speech is punished under it; groups without an established persecution narrative (newer, smaller, or politically unpopular minorities) rarely get the benefit of the doctrine's protective reading despite arguably qualifying.
% DISAPPEARANCE_RATIONALE: Equality advocates and targeted groups argue that without the dignity reading, group libel and organized dehumanization campaigns would flourish unchecked and public discourse would re-normalize subordination; free-speech absolutists and many dissidents argue the underlying harms are already addressed by harassment, incitement, and defamation law, and that removing the dignity-conditional overlay would simply return speech law to firmer, more predictable ground without functionally exposing anyone to new harm.
% FOUNDING_PROBLEM: Formally neutral speech protection historically permitted organized campaigns of group vilification (racist, antisemitic, and other group-targeted propaganda) that functioned to entrench social subordination even where no individual defamation or incitement claim could be proven; the doctrine was built to close that gap.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars studying post-WWII European hate speech regimes and post-apartheid South African jurisprudence attest the founding problem was real and partially addressed. Civil liberties organizations outside the equality-advocacy coalition (including some minority-rights free-speech groups) attest that the doctrine, as currently administered, has drifted from remedying organized subordination campaigns toward suppressing disfavored viewpoints more broadly, and that this drift is not corroborated by the doctrine's own beneficiaries.
narrative_ontology:disappearance_verdict(speech_protection_kernel__dignity_reading, contested).
narrative_ontology:founding_problem_status(speech_protection_kernel__dignity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__dignity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__dignity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58 at interval end) reflects the doctrine's demonstrated drift: what began as narrow protection against organized group-vilification campaigns has, per the measurement series, expanded to cover a widening range of disfavored viewpoint expression. Suppression (0.52) is substantial but not extreme — the doctrine still requires an administrative or judicial finding of subordinating function, unlike a blanket speech ban, so real (if narrowing) space for contested speech persists. Resistance (0.68) is high because free-speech constituencies, dissidents, and satirists actively litigate and organize against expansive applications. Accessibility collapse (0.40) is moderate: alternative venues and workarounds (private platforms, encrypted communication, foreign-hosted speech) still exist for most disfavored speakers, so the doctrine has not achieved anything like a mountain's near-total foreclosure.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically targeted minority groups and the agencies/advocates who administer the standard sit near the beneficiary end: the arrangement was built to protect them and they set or benefit from where the line is drawn. Speakers of disfavored viewpoints, dissidents using harsh rhetoric against dominant groups, and satirists sit near the target end: they bear the liability and self-censorship costs from an ex post, administratively-drawn characterization they cannot reliably predict. The asymmetry documented for dissidents (payer, powerless, trapped) reflects that the doctrine's protective logic was built around a specific persecution narrative and does not extend symmetrically to counter-speech directed upward at dominant groups, even when it uses comparably harsh rhetoric.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — organized group-vilification campaigns evading individual defamation and incitement law — was real and is corroborated by comparative constitutional scholarship outside the advocacy coalition that built the doctrine. Classifying this as tangled_rope rather than snare preserves that genuine coordination function (protecting powerless, trapped minority groups from organized subordination) while registering, via the extractiveness trend and the founding_problem_status of 'contested,' that the doctrine's administered boundary has drifted toward broader viewpoint control than the founding problem justifies. A pure snare classification would erase the real coordination achieved for historically subordinated groups; a pure rope classification would erase the documented asymmetric cost now borne by dissidents and satirists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subordination_vs_offense_line_stability,
    'Is there a stable, non-viewpoint-dependent way to distinguish speech that ''functions as structural subordination'' from speech that is merely deeply offensive to a group, or does the line inevitably import the adjudicator''s own political priors?',
    'Longitudinal analysis of case outcomes under the dignity standard across jurisdictions, checking whether the subordination/offense line correlates with adjudicator ideology or target-group political power rather than with objective features of the speech act.',
    'If the line is not stable and correlates with adjudicator priors, the doctrine functions closer to viewpoint-based extraction than to genuine anti-subordination coordination, supporting reclassification toward snare over time; if stable, the tangled_rope classification with a genuine coordination core is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_vs_offense_line_stability, empirical, 'Whether the subordination/offense boundary is administrable without importing viewpoint bias.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the dignity reading the correct lens for evaluating speech that targets historically dominant groups on behalf of a subordinated cause, or does its persecution-narrative-dependent design structurally exclude such speech from ever being read as protected regardless of its content?',
    'Compare doctrinal treatment of comparably harsh rhetoric directed downward (at minorities) versus upward (at majorities/institutions) across a matched sample of adjudicated cases.',
    'If treatment is systematically asymmetric in a way not explained by differential real-world subordination effects, the dignity reading''s own axioms are being applied inconsistently with its stated foundational claim, which would support the political_dissidents_using_inflammatory_rhetoric victim declaration as structural rather than incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the dignity reading''s persecution-narrative design produces asymmetric application.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does robust institutional adoption of the dignity reading (via agency rulemaking, campus codes, and platform policy) create structural pressure that narrows the practical scope for the absolutist and marketplace readings to operate, even where no single legal framework formally forecloses them?',
    'Track whether jurisdictions or institutions that adopt strong dignity-standard doctrine show measurable contraction in absolutist-reading and marketplace-reading legal argument success rates in the same courts over time.',
    'If contraction is measurable, the reading_relations should register ''influences'' (already declared) rather than mere coexistence in practice, even though no logical foreclosure exists — this affects how downstream contamination-propagation analysis should weight this constraint''s edges to its siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, empirical, 'Whether adoption of the dignity reading structurally narrows sibling readings'' practical scope without foreclosing them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__dignity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__dignity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(spee_tr_t8, speech_protection_kernel__dignity_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(spee_tr_t16, speech_protection_kernel__dignity_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(spee_tr_t24, speech_protection_kernel__dignity_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(spee_tr_t32, speech_protection_kernel__dignity_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__dignity_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__dignity_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(spee_be_t8, speech_protection_kernel__dignity_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(spee_be_t16, speech_protection_kernel__dignity_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(spee_be_t24, speech_protection_kernel__dignity_reading, base_extractiveness, 24, 0.51).
narrative_ontology:measurement(spee_be_t32, speech_protection_kernel__dignity_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__dignity_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__dignity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(spee_su_t8, speech_protection_kernel__dignity_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(spee_su_t16, speech_protection_kernel__dignity_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(spee_su_t24, speech_protection_kernel__dignity_reading, suppression_requirement, 24, 0.47).
narrative_ontology:measurement(spee_su_t32, speech_protection_kernel__dignity_reading, suppression_requirement, 32, 0.5).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__dignity_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__dignity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__dignity_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This story is one of five members of the speech_protection_kernel constraint family, each instantiating a distinct reading of the same contested kernel (conditional speech protection). The dignity_reading carries a moderate, rising epsilon (0.32->0.58) reflecting genuine but drifting anti-subordination coordination; the sibling readings (absolutist, harm_threshold, marketplace, democratic_participation) are authored as separate files with their own epsilon values and stakeholder structures reflecting their distinct core premises. Network edges here are declared bidirectionally-relevant: institutional adoption of the dignity reading creates downstream pressure on the scope within which the absolutist and marketplace readings can operate (see the sibling_reading_foreclosure_scope omega), while the harm_threshold reading's narrower individual-harm test is the most direct doctrinal competitor for occupying the same case law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
