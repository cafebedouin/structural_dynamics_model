% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__balancing_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: speech_protection_boundary__balancing_reading
 *   human_readable: Speech Protection Boundary — Case-by-Case Balancing Reading
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint models the balancing reading of the
 *   speech_protection_boundary kernel: courts determine whether speech is
 *   protected by weighing First Amendment interests case-by-case against
 *   countervailing constitutional values (equality, dignity, public safety)
 *   and demonstrated harms. Unlike the absolutist reading (near-categorical
 *   protection, narrow Brandenburg-style exception) and the harm_limited
 *   reading (protection conditional on absence of dignitary/equality harm),
 *   the balancing reading distributes the gatekeeper function across the
 *   judiciary rather than fixing it in a bright-line rule. This produces
 *   genuine responsiveness to context — coded speech, algorithmically
 *   amplified harassment, and emergent harms can be addressed without waiting
 *   for legislative or doctrinal overhaul — but the same flexibility becomes
 *   a resource-dependent litigation surface: parties who can afford repeated
 *   appeals shape the boundary over time in ways low-resource speakers cannot
 *   contest.
 *
 * KEY AGENTS:
 *   - judiciary_as_institution: Primary agenda-setter (institutional/analytical) — administers the weighing test, gains interpretive authority and institutional flexibility
 *   - litigants_with_resources_to_appeal: Beneficiary (powerful/arbitrage) — can relitigate boundary questions repeatedly, effectively co-authoring doctrine over time
 *   - targeted_groups_seeking_dignitary_relief: Beneficiary (organized/constrained) — gain a doctrinal avenue to weigh equality and dignity harms against speech claims that the absolutist reading would foreclose
 *   - low_resource_speakers: Primary payer (powerless/trapped) — bear the cost of unpredictability; cannot afford to litigate close cases to a favorable outcome
 *   - marginal_political_movements: Payer (powerless/constrained) — face case-by-case scrutiny of their speech that better-resourced or more mainstream speakers do not encounter as acutely
 *   - speakers_in_lower_courts_facing_unpredictable_outcomes: Payer (moderate/constrained) — experience circuit-level variance in how the balancing test is applied, without the resources to appeal to resolve it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, 0.42).
domain_priors:suppression_score(speech_protection_boundary__balancing_reading, 0.48).
domain_priors:theater_ratio(speech_protection_boundary__balancing_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__balancing_reading, "Speech Protection Boundary — Case-by-Case Balancing Reading").
narrative_ontology:topic_domain(speech_protection_boundary__balancing_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__balancing_reading, '2316fdb5-58fc-4a05-a745-d3c1743eef18').
narrative_ontology:cs_kernel_codification('2316fdb5-58fc-4a05-a745-d3c1743eef18', distributed).
narrative_ontology:cs_authority_grounding('2316fdb5-58fc-4a05-a745-d3c1743eef18', practice).
narrative_ontology:cs_interpretation_layer_present('2316fdb5-58fc-4a05-a745-d3c1743eef18').
narrative_ontology:cs_reading_relation('2316fdb5-58fc-4a05-a745-d3c1743eef18', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2316fdb5-58fc-4a05-a745-d3c1743eef18', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('2316fdb5-58fc-4a05-a745-d3c1743eef18', foundational, no_constitutional_value_is_categorically_supreme).
narrative_ontology:cs_axiom_status(no_constitutional_value_is_categorically_supreme, holdable).
narrative_ontology:cs_axiom_grounding('2316fdb5-58fc-4a05-a745-d3c1743eef18', no_constitutional_value_is_categorically_supreme, conventional).
narrative_ontology:cs_axiom('2316fdb5-58fc-4a05-a745-d3c1743eef18', secondary, context_sensitive_adjudication_produces_more_accurate_outcomes_than_bright_line_rules).
narrative_ontology:cs_axiom_status(context_sensitive_adjudication_produces_more_accurate_outcomes_than_bright_line_rules, holdable).
narrative_ontology:cs_axiom_grounding('2316fdb5-58fc-4a05-a745-d3c1743eef18', context_sensitive_adjudication_produces_more_accurate_outcomes_than_bright_line_rules, instrumental).
narrative_ontology:cs_reference_frame('2316fdb5-58fc-4a05-a745-d3c1743eef18', post_brandenburg_multifactor_scrutiny_framework).
narrative_ontology:cs_drift_state('2316fdb5-58fc-4a05-a745-d3c1743eef18', contemporary_platform_speech_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2316fdb5-58fc-4a05-a745-d3c1743eef18', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__balancing_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, judiciary_as_institution).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, litigants_with_resources_to_appeal).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, targeted_groups_seeking_dignitary_relief).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, low_resource_speakers).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, marginal_political_movements).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, speakers_in_lower_courts_facing_unpredictable_outcomes).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, no_speech_right_is_absolute).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, constitutional_values_must_be_reconciled_contextually).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the case-by-case weighing test, deciding which constitutional values prevail in each speech dispute. Retains and expands interpretive authority precisely because the boundary is not fixed by a bright-line categorical rule; each new case is an occasion to further specify (or leave open) the doctrine's content.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, judiciary_as_institution, agenda_setter,
    institutional, civilizational, analytical, national).

% Can afford to bring repeated cases to shape how the balancing test is applied in contexts favorable to their interests, effectively co-authoring doctrine over time through sustained litigation investment that ordinary speakers cannot match.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, litigants_with_resources_to_appeal, beneficiary,
    powerful, generational, arbitrage, national).

% Gain a doctrinal avenue to argue that speech causing dignitary or equality harm should yield to countervailing constitutional interests, an avenue the absolutist reading would largely foreclose. Depend on courts weighing their asserted harms seriously case by case, which is itself uncertain.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, targeted_groups_seeking_dignitary_relief, beneficiary,
    organized, biographical, constrained, national).

% Face the same case-by-case standard as well-funded parties but cannot afford to litigate a close case through appeal to a favorable outcome. Must guess in advance whether their speech will be protected, often choosing silence rather than risk an adverse, unpredictable ruling.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, low_resource_speakers, payer,
    powerless, immediate, trapped, local).

% Their speech is more likely to be characterized as presenting demonstrated harms justifying restriction, since they lack the institutional standing or media access to frame their own speech favorably before a court applies the balancing test.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, marginal_political_movements, payer,
    powerless, biographical, constrained, national).

% Experience significant variance in how the balancing test is applied across circuits and judges for facially similar speech claims, without the resources to appeal a disfavorable outcome to a court that might resolve the variance.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, speakers_in_lower_courts_facing_unpredictable_outcomes, payer,
    moderate, immediate, constrained, regional).

% Argue from outside most individual cases that the balancing approach itself is the problem — that any standard short of near-categorical protection invites erosion by whichever value currently commands judicial sympathy. Their structural critique of the balancing reading as a category is rarely the direct subject of any single case.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, free_speech_advocacy_organizations, excluded,
    organized, generational, constrained, national).

% Study outcome patterns across the balancing doctrine's application, documenting where it produces principled context-sensitivity versus unexplained judge-dependent variance, informing but not controlling how the doctrine develops.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__balancing_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_boundary__balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows courts to resolve genuinely hard speech cases — where categorical rules give clearly wrong answers in some contexts — by weighing the actual competing constitutional interests and demonstrated harms present in each case, rather than forcing every dispute through a single fixed rule.
% TRANSFER_FUNCTION: Moves predictability and the practical power to shape doctrine from speakers who cannot afford sustained litigation to well-resourced repeat litigants and to the judiciary's own interpretive discretion; correspondingly moves some protective benefit toward groups asserting dignitary or equality harms who would fare worse under a more absolutist standard.
% ABSENT_VOICES: Free speech advocacy organizations objecting to the balancing approach as a category (rather than to any single outcome) rarely get to litigate that structural objection directly, since courts resolve individual cases rather than adjudicate the wisdom of the standard itself. Low-resource and marginal speakers whose cases never reach appellate review are effectively unheard on how the standard is refined over time.
% DISAPPEARANCE_RATIONALE: If case-by-case balancing disappeared and were replaced by a categorical rule (either pole), a large body of pending and future speech disputes would resolve differently and predictably rather than depending on judge, circuit, and litigant resources; well-resourced repeat litigants would lose their primary lever for incrementally reshaping the boundary, and low-resource speakers would gain predictability at the cost of losing case-specific consideration of context.
% FOUNDING_PROBLEM: Neither an absolute free-speech rule nor a rule conditioning protection on absence of harm could handle the full range of speech disputes courts actually faced — some speech causes serious, particularized harm that a purely categorical protective rule would ignore, while some harm-based exceptions would be so broad they would swallow protection for legitimate dissent. Balancing was adopted to let courts weigh the actual competing interests in each case.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional law scholars outside the litigating parties broadly corroborate that genuinely hard cases exist where categorical rules produce poor results, supporting the founding problem's continued vitality. However, the same scholarly literature also documents substantial unexplained outcome variance correlated with circuit and judicial composition rather than case facts, which free speech advocacy organizations and some scholars read as evidence that the current institutional form of balancing has drifted from principled context-sensitivity toward judge-dependent discretion — a status not attested by the judiciary itself, which continues to describe its own weighing as principled.
narrative_ontology:disappearance_verdict(speech_protection_boundary__balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_boundary__balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__balancing_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__balancing_reading_tests).
:- end_tests(speech_protection_boundary__balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects a genuine coordination function — courts adapting doctrine to context that categorical rules cannot anticipate — combined with a real transfer: unpredictability cost is not borne evenly but concentrated on speakers who cannot afford to litigate the boundary repeatedly. Suppression (0.48) is moderate: the balancing framework does not categorically forbid disfavored speech the way a harm-limited reading might, but it does create a chilling effect from unpredictability itself — speakers self-censor rather than risk an adverse case-by-case determination. Theater ratio (0.28) is present but not dominant: balancing opinions genuinely engage with competing interests, but a growing share of judicial language performs 'careful weighing' to legitimate outcomes that track other factors (circuit composition, litigant resources). Accessibility collapse (0.35) is moderate-low — the categorical alternatives (absolutist or harm-limited rules) remain live and contested, unlike a settled natural-law-style boundary. Resistance (0.58) is substantial: both free-speech absolutists and harm-focused reformers actively contest the balancing approach from opposite directions, which is itself evidence this is a contested reading rather than a stable consensus position.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, balancing is principled contextual adjudication — the considered alternative to both rigid absolutism and categorical harm-exclusion, tracking constitutional values as they actually compete in hard cases. From a low-resource speaker's seat, the same framework is an unpredictable gauntlet: the rule that will be applied to their speech is not knowable in advance and depends on who is willing and able to litigate it to a favorable forum. The engine's per-seat computation should register this divergence: institutional/analytical seats see coordination; powerless/trapped seats see cost without a corresponding voice in how the standard evolves.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary as an institution is the primary agenda-setter: it administers the weighing test, retains interpretive discretion, and its authority expands precisely because the boundary is not fixed by a bright-line rule. Well-resourced repeat litigants function as structural beneficiaries — they can afford to bring case after case that incrementally reshapes the boundary in their favor, converting judicial discretion into a lever they alone can pull effectively. Targeted groups seeking redress for dignitary or equality harms also benefit from the case-by-case approach, since it offers an avenue foreclosed by the absolutist reading's near-categorical protection. Low-resource speakers, marginal political movements, and litigants without appellate capacity are structural targets: they face the same discretionary standard without the resources to shape or predict its application, and they bear the chilling effect of not knowing in advance which side of the line their speech falls on.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that neither absolute protection nor blanket harm-exclusion adequately handles every speech controversy — remains genuinely live; new categories of contested speech (algorithmically amplified harassment, coded incitement, synthetic media) continue to emerge that no categorical rule anticipated. This argues against treating the constraint as a pure mandatrophy case (a mandate persisting after its problem died). However, the specific INSTITUTIONAL SHAPE the balancing reading takes — case-by-case judicial weighing without codified standards — has arguably outlived any claim to being the uniquely necessary response to that founding problem, since both sibling readings claim to address the same underlying tension through different, more predictable mechanisms. The mandatrophy question here is not 'is the problem dead' but 'has this particular institutional means become self-perpetuating independent of whether it is the best available means' — a question the omega on balancing-versus-categorical legitimacy is designed to probe.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_versus_categorical_legitimacy,
    'Is case-by-case balancing a genuine improvement in fit between speech doctrine and social reality, or is it a mechanism that quietly transfers gatekeeping power from fixed rules (which constrain judges) to individual judges (who can rationalize outcomes post hoc)?',
    'Longitudinal analysis of outcome variance across circuits and judges for factually similar speech claims; if variance is high and correlates with judicial ideology rather than case facts, the balancing framework functions as discretion-laundering rather than principled weighing.',
    'High unexplained variance would support classifying the constraint as substantially extractive (a snare wearing coordination language); low variance would support the tangled_rope or even rope reading of genuine contextual calibration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_versus_categorical_legitimacy, empirical, 'Whether balancing produces principled variance or judge-dependent unpredictability.').

omega_variable(
    kernel_reading_selection,
    'This constraint instantiates the balancing_reading of the speech_protection_boundary kernel, structurally distinct from the absolutist_reading (near-categorical protection, harm exception limited to Brandenburg) and the harm_limited_reading (protection conditional on absence of dignitary/equality harm). Is the balancing reading''s claim to occupy a principled ''middle'' actually a stable third position, or is it a name for wherever judicial discretion currently happens to land between the two poles?',
    'Compare case outcomes attributed to ''balancing'' against outcomes that would be predicted by either pole reading; a balancing reading with no independent predictive content beyond ex post rationalization of either pole''s results indicates the middle position is not structurally distinct but is a discretion sink.',
    'If balancing has no independent predictive content, its coordination claim (principled contextual adjudication) collapses and the constraint reads closer to snare (extraction of predictability from speakers, redistributed to whichever value the deciding judge favors); if it does have independent content, the tangled_rope classification holds — real coordination function (context-sensitivity) coexisting with real extraction (unpredictability cost borne disproportionately by low-resource speakers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Whether the balancing reading is a genuine third position or an unstable label for ad hoc discretion between the absolutist and harm-limited poles.').

omega_variable(
    gatekeeper_distribution_effect,
    'Does distributing the gatekeeper function across the judiciary (rather than fixing it in a categorical rule) produce net-beneficial responsiveness to emerging harms (e.g., coded incitement, algorithmically amplified speech) or does it produce a structural advantage for whichever party can afford to litigate the boundary repeatedly?',
    'Track relitigation rates and win rates by party resources across circuits over the interval; sustained resource-correlated success in redefining the boundary would indicate the distributed gatekeeper function has been captured by well-resourced repeat litigants.',
    'Would refine whether the primary beneficiary is the judiciary''s institutional flexibility or a subset of well-funded litigants using that flexibility as a wedge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gatekeeper_distribution_effect, empirical, 'Whether distributed judicial discretion advantages repeat, well-resourced litigants over ordinary speakers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__balancing_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__balancing_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(spee_tr_t10, speech_protection_boundary__balancing_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(spee_tr_t20, speech_protection_boundary__balancing_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(spee_tr_t30, speech_protection_boundary__balancing_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(spee_tr_t40, speech_protection_boundary__balancing_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(spee_tr_t50, speech_protection_boundary__balancing_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__balancing_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(spee_be_t10, speech_protection_boundary__balancing_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(spee_be_t20, speech_protection_boundary__balancing_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(spee_be_t30, speech_protection_boundary__balancing_reading, base_extractiveness, 30, 0.39).
narrative_ontology:measurement(spee_be_t40, speech_protection_boundary__balancing_reading, base_extractiveness, 40, 0.41).
narrative_ontology:measurement(spee_be_t50, speech_protection_boundary__balancing_reading, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__balancing_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(spee_su_t10, speech_protection_boundary__balancing_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(spee_su_t20, speech_protection_boundary__balancing_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(spee_su_t30, speech_protection_boundary__balancing_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement(spee_su_t40, speech_protection_boundary__balancing_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(spee_su_t50, speech_protection_boundary__balancing_reading, suppression_requirement, 50, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__balancing_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__harm_limited_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the speech_protection_boundary kernel. The absolutist_reading and harm_limited_reading are separate constraint files with their own ε values, beneficiary/victim structures, and computed types — per the ε-invariance principle, they are not alternative measurements of this constraint but structurally distinct constraints sharing a contested kernel. The balancing_reading's moderate ε (0.42) sits between the absolutist_reading's expected low ε (protection is the default, exceptions narrow) and the harm_limited_reading's expected higher ε (protection is conditional, unprotected findings routine) — this ordering is a prediction to be checked against the sibling files' authored values, not a constraint imposed here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_boundary__balancing_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
