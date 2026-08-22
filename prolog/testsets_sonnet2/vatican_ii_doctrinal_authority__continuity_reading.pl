% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__continuity_reading
 *   human_readable: Vatican II as Hermeneutic of Continuity (Organic Development Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This story instantiates the continuity reading of the Vatican II kernel:
 *   the claim that the Council's documents, and the liturgical and pastoral
 *   changes that followed, represent organic development of previously
 *   implicit teaching rather than doctrinal rupture. Under this reading,
 *   apparent novelties (religious liberty, ecumenism, collegiality,
 *   liturgical reform) are explications, not innovations, and post-conciliar
 *   excesses are implementation failures attributable to poor catechesis and
 *   undisciplined local application, not to defects in the conciliar texts or
 *   intent. This is authored as one reading among several live readings of
 *   the same kernel; the rupture_progressive_reading and
 *   rupture_traditionalist_reading constraints are separate stories with
 *   their own ε values and structural data, not alternative measurements of
 *   this one.
 *
 * KEY AGENTS:
 *   - post_conciliar_papacy: sets the authoritative interpretation (institutional/analytical) — administers legitimacy
 *   - curial_magisterium: produces and defends the continuity framework (institutional/analytical)
 *   - continuity_school_theologians: supply scholarly labor for the reading (organized/mobile)
 *   - traditionalist_clergy_and_laity: bear cost of having their rupture-perception classified as error (moderate/constrained)
 *   - progressive_reform_clergy_and_laity: bear cost of having their spirit-of-the-Council claims foreclosed (moderate/constrained)
 *   - confused_parish_catechesis_recipients: absorb the gap between lived discontinuity and official narrative (powerless/trapped)
 *   - ecclesiastical_historians: analytical observers of the drafting and reception record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__continuity_reading, 0.28).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__continuity_reading, 0.42).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__continuity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__continuity_reading, rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__continuity_reading, "Vatican II as Hermeneutic of Continuity (Organic Development Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__continuity_reading, 'f60dfce0-e089-4d3d-9bbe-880f3cf1ab47').
narrative_ontology:cs_kernel_codification('f60dfce0-e089-4d3d-9bbe-880f3cf1ab47', fixed_text).
narrative_ontology:cs_authority_grounding('f60dfce0-e089-4d3d-9bbe-880f3cf1ab47', lineage).
narrative_ontology:cs_interpretation_layer_present('f60dfce0-e089-4d3d-9bbe-880f3cf1ab47').
narrative_ontology:cs_reading_relation('f60dfce0-e089-4d3d-9bbe-880f3cf1ab47', vatican_ii_doctrinal_authority__rupture_progressive_reading, forecloses).
narrative_ontology:cs_reading_relation('f60dfce0-e089-4d3d-9bbe-880f3cf1ab47', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, forecloses).
narrative_ontology:cs_axiom('f60dfce0-e089-4d3d-9bbe-880f3cf1ab47', foundational, magisterial_teaching_cannot_be_reversed).
narrative_ontology:cs_axiom_status(magisterial_teaching_cannot_be_reversed, holdable).
narrative_ontology:cs_axiom_grounding('f60dfce0-e089-4d3d-9bbe-880f3cf1ab47', magisterial_teaching_cannot_be_reversed, deontological).
narrative_ontology:cs_axiom('f60dfce0-e089-4d3d-9bbe-880f3cf1ab47', foundational, apparent_novelty_is_always_explicable_as_prior_implicit_teaching).
narrative_ontology:cs_axiom_status(apparent_novelty_is_always_explicable_as_prior_implicit_teaching, holdable).
narrative_ontology:cs_axiom_grounding('f60dfce0-e089-4d3d-9bbe-880f3cf1ab47', apparent_novelty_is_always_explicable_as_prior_implicit_teaching, conventional).
narrative_ontology:cs_axiom('f60dfce0-e089-4d3d-9bbe-880f3cf1ab47', secondary, post_conciliar_excess_is_implementation_error_not_conciliar_intent).
narrative_ontology:cs_axiom_status(post_conciliar_excess_is_implementation_error_not_conciliar_intent, holdable).
narrative_ontology:cs_axiom_grounding('f60dfce0-e089-4d3d-9bbe-880f3cf1ab47', post_conciliar_excess_is_implementation_error_not_conciliar_intent, conventional).
narrative_ontology:cs_reference_frame('f60dfce0-e089-4d3d-9bbe-880f3cf1ab47', pre_conciliar_ordinary_and_extraordinary_magisterium).
narrative_ontology:cs_drift_state('f60dfce0-e089-4d3d-9bbe-880f3cf1ab47', post_synodal_contemporary_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f60dfce0-e089-4d3d-9bbe-880f3cf1ab47', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, curial_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, conciliar_period_bishops_conference).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, post_conciliar_papacy).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, continuity_school_theologians).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_clergy_and_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, progressive_reform_clergy_and_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, confused_parish_catechesis_recipients).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, doctrinal_development_thesis).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, magisterial_infallibility_continuity).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, hermeneutic_of_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the authoritative interpretation of the Council's documents, adjudicates disputes about whether a given post-conciliar practice or teaching is continuous with prior tradition, and disciplines clergy who read the Council as rupture in either direction. Collects the legitimacy benefit of being able to characterize any contested change as 'already implicit' in prior teaching.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, post_conciliar_papacy, agenda_setter,
    institutional, civilizational, analytical, global).

% Administers the ongoing interpretive apparatus (congregations, commissions, catechisms) that produces the continuity reading as settled teaching. Benefits from a framework in which no admission of doctrinal rupture is ever structurally possible, insulating prior magisterial pronouncements from being characterized as having been superseded or wrong.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, curial_magisterium, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__continuity_reading, curial_magisterium, beneficiary).

% Academic and pastoral theologians whose scholarly and ecclesiastical careers are built on demonstrating continuity between conciliar and pre-conciliar teaching. Gain institutional standing, publication platforms, and proximity to Roman authority from supplying the interpretive labor the continuity reading requires.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, continuity_school_theologians, beneficiary,
    organized, generational, mobile, global).

% Experience specific conciliar and post-conciliar changes (liturgical reform, religious liberty teaching, ecumenism) as substantive breaks with prior doctrine and discipline. Under the continuity reading their perception of rupture is itself classified as error or lack of proper hermeneutical formation; those who act on it (old-rite communities, SSPX-aligned clergy) face canonical restriction, and the reading forecloses their central grievance from ever registering as a live doctrinal claim rather than a pastoral misunderstanding.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_clergy_and_laity, payer,
    moderate, generational, constrained, global).

% Read the Council as authorizing a trajectory of reform beyond the letter of the texts (women's ordination, doctrinal development on sexuality, collegial governance). Under the continuity reading their appeals to a 'spirit of the Council' are treated as unauthorized extrapolation; reforms they pursue on that basis are disciplined as departures from what the Council actually taught, foreclosing their claim to conciliar warrant.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, progressive_reform_clergy_and_laity, payer,
    moderate, generational, constrained, global).

% Ordinary Catholics who received inconsistent catechesis during and after the implementation period, absorbing genuinely discontinuous liturgical and pastoral practice while being told at the doctrinal level that nothing has changed. Bear the cost of reconciling lived experience of upheaval with an official narrative of seamless continuity, largely without the theological training to adjudicate the dispute themselves.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, confused_parish_catechesis_recipients, payer,
    powerless, biographical, trapped, local).

% Hold that the Council authorized an open-ended reform trajectory ('spirit of the Council') that the continuity reading does not entertain as a live possibility; their account of the documents' ambiguity as generative rather than merely prudential is treated by the continuity reading as a category error, not engaged on its own terms.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, rupture_progressive_theologians, excluded,
    organized, generational, mobile, global).

% Hold that the conciliar documents themselves (not merely their implementation) contain ambiguities enabling heterodox teaching, and that this constitutes genuine rupture. The continuity reading treats this as inadmissible without engaging the specific textual claims (e.g. on religious liberty, ecumenism) at the level of doctrine, routing all such objections into 'implementation error.'
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, rupture_traditionalist_theologians, excluded,
    organized, generational, constrained, global).

% Study the textual, procedural, and reception history of the Council independent of any single ecclesiastical faction's stake in the outcome. Can compare conciliar drafting history, minority reports, and subsequent magisterial statements against each of the competing readings.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, ecclesiastical_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative interpretive frame that allows the institution to absorb genuine change (in liturgy, ecumenical posture, religious-liberty teaching, collegiality) without conceding that the deposit of faith or prior infallible teaching was ever mistaken — preserving the coordination good of a stable, non-self-contradicting magisterium across centuries.
% TRANSFER_FUNCTION: Moves interpretive authority and legitimacy toward the papacy and curial apparatus that administers the continuity reading, and moves the cost of reconciling lived discontinuity onto those who experienced the changes as rupture (in either direction) — their testimony about experienced change is reclassified as misunderstanding rather than evidence.
% ABSENT_VOICES: Both the progressive and traditionalist rupture readings are structurally excluded from being live doctrinal claims under this reading — they are permitted as pastoral or disciplinary problems (misapplication, insufficient formation) but not as competing accounts of what actually happened at the level of doctrine. Lay recipients of contradictory catechesis have no forum to register that the two things (continuity claim, discontinuous lived experience) don't reconcile.
% DISAPPEARANCE_RATIONALE: If the continuity reading were abandoned as the magisterium's official self-description, the Church's claim to doctrinal inerrancy across time would need an alternative account of how substantive change occurred — either an admission of correctable error (rupture_traditionalist) or an embrace of open development beyond textual limits (rupture_progressive). Curial and papal authority structures depend heavily on the continuity frame; traditionalist and progressive factions would read its disappearance very differently, hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: The Council needed to update the Church's pastoral posture toward the modern world (liturgy, other Christians, other religions, religious freedom, the laity's role) without appearing to contradict prior ex cathedra and ordinary magisterial teaching, which would undermine the doctrine of infallibility itself.
% FOUNDING_PROBLEM_CORROBORATION: The papacy and curial theologians (the reading's own beneficiaries) attest the founding problem is solved by continuity. Independent ecclesiastical historians outside the magisterium's employ (e.g. scholars of the Bologna school and rival Roman-school historians) document that conciliar drafting history shows genuine unresolved tension between continuity and rupture framings among the bishops themselves at the time — corroboration from outside the beneficiary set is mixed, not confirmatory.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__continuity_reading, contested).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__continuity_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).
:- end_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   ε is authored low (0.28) because, under this reading's own lights, doctrinal extraction is minimal — no prior binding teaching is admitted to have been reversed, so there is little rent extracted at the level of doctrine per se. However suppression is moderate-high (0.42) because maintaining the reading requires actively disciplining both traditionalist claims of rupture and progressive claims of a 'spirit' beyond the text — the coordination good (a non-self-contradicting magisterium) is real but its maintenance is not cost-free. Theater ratio is meaningfully elevated (0.38, rising over the interval) because a substantial share of magisterial and academic effort goes into producing continuity narratives for changes that, at the level of liturgical and pastoral practice, were genuinely large and fast (the expected structural delta: low ε on doctrine, higher on practice) — the theater is the labor of narrating discontinuous practice as continuous doctrine. accessibility_collapse (0.55) and resistance (0.5) sit at rope/tangled_rope territory rather than mountain territory: this is an authored institutional interpretation, not a natural law, and it meets real, organized resistance from both flanks, which a genuine mountain would not.
 *
 * DIRECTIONALITY LOGIC:
 *   The papacy and curial magisterium are near the full-beneficiary end: they administer the interpretive apparatus and collect the legitimacy benefit of an unbroken teaching authority claim. Continuity-school theologians are secondary beneficiaries via career and institutional proximity. Traditionalist and progressive clergy/laity are targets: their competing accounts of the Council are not merely disagreed with but structurally excluded from consideration as live doctrinal possibilities, and those who act on their reading face canonical consequences. Ordinary catechesis recipients are the most powerless payers — they bear the cost of reconciling contradictory signals with no institutional standing to contest the frame.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two opposite mislabelings: it does not let the continuity reading be dismissed as pure extraction (there is a genuine coordination function — an institution claiming perpetual doctrinal inerrancy needs SOME account of how it changes without erring, and this reading supplies one that is internally coherent and has scholarly defenders), nor does it let the reading pass as costless coordination (maintaining it requires active, ongoing suppression of two well-organized alternative readings, which is real coercive overhead, not merely persuasion).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    development_vs_reversal_distinguishability,
    'Is there a principled, reading-independent criterion for distinguishing ''organic development of implicit teaching'' from ''doctrinal reversal dressed as development,'' or does the distinction only exist relative to which reading one already holds?',
    'Compare specific test cases (religious liberty in Dignitatis Humanae versus the Syllabus of Errors; ecumenism versus prior anathemas) against a reading-external criterion such as Newman''s own tests for development, applied by scholars unaffiliated with either magisterial or traditionalist institutions.',
    'If no reading-independent criterion is available, the continuity reading''s central claim is unfalsifiable by construction, which would push classification toward a higher-suppression, more clearly asymmetric structure regardless of the low authored ε on doctrine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(development_vs_reversal_distinguishability, conceptual, 'Whether development-vs-reversal is a discoverable fact or a reading-relative framing choice.').

omega_variable(
    kernel_committer_structure,
    'Which of the four declared readings of the Vatican II kernel (composite_overdetermination, continuity, rupture_progressive, rupture_traditionalist) best accounts for the documentary and reception record, and is the disagreement itself resolvable or a permanent feature of the kernel?',
    'This is the committer-frame question routed here per Rule 2 rather than folded into this story''s ε or metrics. Resolution would require independent historical-critical work on conciliar drafting minutes, minority reports (e.g. the Coetus Internationalis Patrum records), and post-conciliar magisterial statements, assessed by historians outside all four constituencies'' institutional interests.',
    'If the composite_overdetermination_reading is judged most accurate, it would suggest the four-reading kernel structure itself is a downstream artifact of collapsing several independent structural changes into one contested event — which would not change this story''s own ε but would change how much explanatory weight any single reading (including this one) should be given in institutional practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'The committer structure of the Vatican II kernel: naming the reading, the sibling readings, and where the disagreement is located.').

omega_variable(
    implementation_error_attribution_scope,
    'How much of the post-conciliar liturgical and catechetical disruption is properly attributable to ''implementation error'' as this reading claims, versus being a foreseeable or intended consequence of the conciliar texts and the reform commissions that produced concrete liturgical books?',
    'Archival study of the Consilium''s working papers and correspondence, compared against the final conciliar texts, to assess whether the scope of post-conciliar liturgical change was anticipated by conciliar fathers or exceeded what a plain reading of Sacrosanctum Concilium authorized.',
    'If the disruption significantly exceeded what the texts authorized, the ''implementation error, not conciliar intent'' move that keeps this reading''s ε low on doctrine would look increasingly strained, and the theater_ratio finding would need to be revised upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_error_attribution_scope, empirical, 'Whether post-conciliar disruption was implementation error or a foreseeable structural consequence, bearing on this reading''s central attribution move.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__continuity_reading, 1962, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1962, 0.2).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1975, 0.3).
narrative_ontology:measurement(vati_tr_t1988, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1988, 0.34).
narrative_ontology:measurement(vati_tr_t2000, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(vati_tr_t2013, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2013, 0.37).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1962, 0.18).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1975, 0.22).
narrative_ontology:measurement(vati_be_t1988, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1988, 0.24).
narrative_ontology:measurement(vati_be_t2000, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(vati_be_t2013, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2013, 0.27).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1962, 0.3).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1975, 0.4).
narrative_ontology:measurement(vati_su_t1988, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1988, 0.48).
narrative_ontology:measurement(vati_su_t2000, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(vati_su_t2013, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2013, 0.41).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__continuity_reading, 0.1).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the vatican_ii_doctrinal_authority kernel. Each reading authors its own ε, beneficiary/victim structure, and claimed type from the same underlying historical record. The continuity_reading authors low ε on doctrine and elevated theater_ratio on practice; the rupture_traditionalist_reading is expected to author high ε broadly (genuine rupture, real victims of heterodox implementation); the rupture_progressive_reading is expected to author low suppression and framing of the continuity/traditionalist readings as themselves the extractive constraint; the composite_overdetermination_reading treats the whole kernel as an artifact of bundling distinct independent reforms. Network edges here record kernel co-membership, not causal dependency in the BGS sense.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
