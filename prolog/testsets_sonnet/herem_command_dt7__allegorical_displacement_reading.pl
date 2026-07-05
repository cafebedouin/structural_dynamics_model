% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__allegorical_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__allegorical_displacement_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: herem_command_dt7__allegorical_displacement_reading
 *   human_readable: Allegorical-Displacement Reading of the Deuteronomy 7 Herem Command
 *   domain: religious/hermeneutical
 *
 * SUMMARY:
 *   This story generates the allegorical-displacement reading of the
 *   Deuteronomy 7 herem command as its own structurally distinct constraint,
 *   separate from the durable-separation reading and the
 *   contextual-supersession reading, per the ε-invariance principle: the
 *   three readings disagree not merely on evaluation but on what the text's
 *   'nations' refer to, and that disagreement changes the victim set and the
 *   extraction profile entirely. Under this reading, 'nations' are
 *   typological placeholders for internalized vice and temptation; the herem
 *   command is relocated wholesale into a metaphor for spiritual
 *   self-discipline, and no ethnic group is a structural party to the
 *   constraint at all. This produces near-zero extractiveness on interethnic
 *   relations, because there is, under this reading, no interethnic relation
 *   being governed.
 *
 * KEY AGENTS:
 *   - practicing_believers_seeking_moral_formation: Primary beneficiary (moderate/mobile) — uses the reading as devotional resource
 *   - allegorical_interpretive_tradition: Agenda-setter (institutional/arbitrage) — establishes and transmits the typological convention
 *   - literalist_and_historical_critical_readers: Excluded voice (moderate/mobile) — holds the ethnic-referent reading unaddressed by this frame
 *   - biblical_scholars_of_ancient_near_eastern_context: Analytical observer — assesses exegetical defensibility against genre convention
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__allegorical_displacement_reading, 0.08).
domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, 0.35).
domain_priors:theater_ratio(herem_command_dt7__allegorical_displacement_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__allegorical_displacement_reading, rope).
narrative_ontology:human_readable(herem_command_dt7__allegorical_displacement_reading, "Allegorical-Displacement Reading of the Deuteronomy 7 Herem Command").
narrative_ontology:topic_domain(herem_command_dt7__allegorical_displacement_reading, "religious/hermeneutical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__allegorical_displacement_reading, 'bcfd54df-138a-4b5d-aa1e-e76a1ad88686').
narrative_ontology:cs_kernel_codification('bcfd54df-138a-4b5d-aa1e-e76a1ad88686', fixed_text).
narrative_ontology:cs_authority_grounding('bcfd54df-138a-4b5d-aa1e-e76a1ad88686', practice).
narrative_ontology:cs_interpretation_layer_present('bcfd54df-138a-4b5d-aa1e-e76a1ad88686').
narrative_ontology:cs_reading_relation('bcfd54df-138a-4b5d-aa1e-e76a1ad88686', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('bcfd54df-138a-4b5d-aa1e-e76a1ad88686', herem_command_dt7__contextual_supersession_reading, coexists_with).
narrative_ontology:cs_axiom('bcfd54df-138a-4b5d-aa1e-e76a1ad88686', foundational, nations_denote_internal_vice_not_ethnic_referent).
narrative_ontology:cs_axiom_status(nations_denote_internal_vice_not_ethnic_referent, holdable).
narrative_ontology:cs_axiom_grounding('bcfd54df-138a-4b5d-aa1e-e76a1ad88686', nations_denote_internal_vice_not_ethnic_referent, conventional).
narrative_ontology:cs_axiom('bcfd54df-138a-4b5d-aa1e-e76a1ad88686', secondary, conquest_language_is_metaphor_for_moral_self_discipline).
narrative_ontology:cs_axiom_status(conquest_language_is_metaphor_for_moral_self_discipline, holdable).
narrative_ontology:cs_axiom_grounding('bcfd54df-138a-4b5d-aa1e-e76a1ad88686', conquest_language_is_metaphor_for_moral_self_discipline, instrumental).
narrative_ontology:cs_reference_frame('bcfd54df-138a-4b5d-aa1e-e76a1ad88686', patristic_typological_exegesis).
narrative_ontology:cs_drift_state('bcfd54df-138a-4b5d-aa1e-e76a1ad88686', contemporary_devotional_reception, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('bcfd54df-138a-4b5d-aa1e-e76a1ad88686', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, practicing_believers_seeking_moral_formation).
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, allegorical_interpretive_tradition).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, typological_reading_of_conquest_narratives).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, primacy_of_internal_spiritual_warfare_over_literal_ethnic_violence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reads the herem text as a call to root out sin, temptation, and vice within their own soul rather than as a historical mandate for violence against ethnic groups. Uses the 'nations' as a vocabulary for internal moral struggle (pride, lust, idolatry) and gains a devotional framework without any claim on real-world outsiders. Free to adopt or set aside this reading without institutional cost.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, practicing_believers_seeking_moral_formation, beneficiary,
    moderate, biographical, mobile, global).

% The patristic-through-contemporary lineage (Origen, later spiritualizing commentators, some strands of devotional and homiletical literature) that establishes and transmits the typological reading. Sets the interpretive convention that 'nations' denotes vice-categories, not peoples, and thereby determines which readings of the text circulate as live options within its own communities. Has broad latitude to refine or abandon the allegory without losing institutional footing, since the reading's plausibility rests on exegetical argument rather than enforcement.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, allegorical_interpretive_tradition, agenda_setter,
    institutional, generational, arbitrage, global).

% Historians and readers who hold that the text's 'nations' denote actual Canaanite peoples and that the command described real (or purportedly real) conquest violence. Their reading is not addressed or refuted within this constraint's own operation — the allegorical frame simply relocates the discussion elsewhere and does not engage the ethnic-referent question on its own terms.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, literalist_and_historical_critical_readers, excluded,
    moderate, biographical, mobile, global).

% Assess whether the typological reading is exegetically defensible against the text's own ancient Near Eastern conquest-ideology genre conventions, comparing it to sibling readings that retain a historical or ethnic referent. Their analysis can shift which reading a given community regards as the responsible one, without themselves being bound by any of the readings.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, biblical_scholars_of_ancient_near_eastern_context, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__allegorical_displacement_reading, diffuse).
narrative_ontology:fixing_cost_class(herem_command_dt7__allegorical_displacement_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a devotional and ethical vocabulary that lets a religious community use a violent conquest narrative as a resource for internal moral formation (temptation, sin, vice) without endorsing or requiring literal violence against any living ethnic group.
% TRANSFER_FUNCTION: Moves interpretive authority from a literal-historical or ethnic-referent reading toward a spiritualized one; the practical effect is a transfer of the text's rhetorical force away from claims about real peoples and onto the reader's own moral discipline. No material or physical transfer between persons occurs under this reading.
% ABSENT_VOICES: Historical-critical readers and communities descended from or identifying with the peoples named in the text (e.g., contemporary readers concerned with how conquest narratives have been used against indigenous or ethnic groups elsewhere) are not consulted by the allegorical frame — it simply does not engage their referent-question, rather than rebutting it.
% DISAPPEARANCE_RATIONALE: For communities that hold the allegorical reading, its disappearance would remove a devotional resource and force re-engagement with the text's literal violence, which some communities would experience as a real loss of usable scripture; for readers who never adopted it, nothing changes because the underlying text and its historical-critical questions persist independently of this interpretive overlay.
% FOUNDING_PROBLEM: How can a community that regards Deuteronomy 7 as scripture read and use a command to annihilate named peoples without that command functioning as a live mandate for ethnic violence in the present?
% FOUNDING_PROBLEM_CORROBORATION: Historical-critical scholars outside the allegorical tradition corroborate that the founding problem (how to defuse the text's apparent ethnic-violence mandate) is real and unresolved by textual criticism alone; they do not corroborate that typological displacement is the correct solution, only that some solution to the problem is needed by communities retaining the text as authoritative.
narrative_ontology:disappearance_verdict(herem_command_dt7__allegorical_displacement_reading, contested).
narrative_ontology:founding_problem_status(herem_command_dt7__allegorical_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__allegorical_displacement_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(herem_command_dt7__allegorical_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__allegorical_displacement_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__allegorical_displacement_reading_tests).
:- end_tests(herem_command_dt7__allegorical_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.05-0.08 across the interval) because, on this reading's own terms, the constraint never touches a real ethnic party — the 'victims' of the underlying literal command are reinterpreted as abstract vices with no capacity to bear cost. Suppression (0.35) is moderate rather than low: even though no one is materially extracted from, the reading does exercise real interpretive pressure — it forecloses straightforward literal engagement with the text for communities operating inside the tradition, which is a real, if soft, constraint on reading practice. Theater ratio is modest and slowly rising (0.15 to 0.22), reflecting that some of the interpretive apparatus (typological commentary, homiletical convention) increasingly performs continuity with tradition rather than doing fresh exegetical work, but the core function — giving readers usable devotional content — remains substantially real throughout.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (practicing believers), the constraint looks like a rope: a genuine coordination device that lets scripture remain usable without moral cost. From the excluded seat (literalist/historical-critical readers), the same textual tradition looks incomplete or evasive — not extractive exactly, but structurally silent on the referent-question they consider primary. The engine should compute low extraction from both seats here, since neither seat bears a material cost under this reading; the divergence is about relevance and adequacy, not extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (practicing believers, the interpretive tradition itself) sit near the full-beneficiary end because the reading was constructed to serve their devotional and institutional continuity needs. There is no victim group under this reading — the abstract vices ('sin,' 'temptation') that occupy the structural position of the biblical 'nations' are not agents and cannot bear cost, which is precisely the structural delta this reading produces relative to its siblings. The excluded readers are not victims of this constraint; they are simply unaddressed by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (defusing the ethnic-violence mandate of a scripturally authoritative conquest text) is authored as live and unresolved by textual criticism alone, and the allegorical reading persists as one live attempted solution among several rather than as a vestigial arrangement outliving its function — so no mandatrophy claim is warranted here; the reading's mandate (making the text usable without ethnic-violence implication) still corresponds to an active need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    typological_referent_displacement_validity,
    'Is the typological reading (''nations'' = internal vices) a legitimate recovery of authorial or canonical intent, or a retrospective theological solution imposed on a text whose plain historical sense concerns real peoples?',
    'Comparative philological and genre analysis against other ancient Near Eastern conquest and treaty-curse texts, cross-checked against the earliest attested reception history (pre-patristic Jewish interpretive tradition) to see whether typological displacement predates or postdates the apparent need to defuse the text''s literal ethnic referent.',
    'If the typological reading is shown to be a late theological accommodation rather than a recovered original sense, this constraint''s claim to be describing the text''s actual referent (rather than substituting a more comfortable one) weakens substantially, though the constraint''s devotional function for its practicing community would be unaffected.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(typological_referent_displacement_validity, conceptual, 'Whether the allegorical referent-shift is exegetically recovered or theologically imposed.').

omega_variable(
    kernel_reading_selection_pressure,
    'Given that all three readings of the herem_command_dt7 kernel (allegorical_displacement, durable_separation, contextual_supersession) are held by different living communities, what determines which reading a given interpretive community adopts, and does that selection correlate with the community''s exposure to or distance from the text''s potential ethnic-violence application?',
    'Sociological and historical study of which communities gravitate toward which reading, cross-referenced with those communities'' proximity to conflicts where conquest narratives have been invoked as present-tense justification (vs. communities using the text purely devotionally with no such proximate application).',
    'If reading-selection correlates strongly with a community''s need to avoid confronting a live ethnic-violence application, that would suggest the allegorical reading functions partly as a pressure-relief mechanism rather than a purely exegetical conclusion — which would not change this story''s authored ε (it remains near zero on its own terms) but would bear on how much interpretive weight the reading can bear when adjudicating the kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, empirical, 'Whether reading-adoption tracks exegetical argument or community exposure to real-world application pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__allegorical_displacement_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(here_tr_t20, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(here_tr_t40, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(here_tr_t60, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement(here_tr_t80, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 80, 0.21).
narrative_ontology:measurement(here_tr_t100, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(here_be_t20, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 20, 0.06).
narrative_ontology:measurement(here_be_t40, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 40, 0.07).
narrative_ontology:measurement(here_be_t60, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 60, 0.07).
narrative_ontology:measurement(here_be_t80, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 80, 0.08).
narrative_ontology:measurement(here_be_t100, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 100, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(herem_command_dt7__allegorical_displacement_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__allegorical_displacement_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(herem_command_dt7__allegorical_displacement_reading, 0.08).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__contextual_supersession_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the herem_command_dt7 kernel. allegorical_displacement_reading forecloses durable_separation_reading (a real, present-tense ethnic outsider cannot simultaneously be a mere typological placeholder for an internal vice within the same interpretive framework — the two readings make incompatible claims about what the text's 'nations' denote). It coexists_with contextual_supersession_reading because a reader can hold that the text is BOTH read typologically for devotional purposes AND was historically a bounded ancient directive later superseded — these are not mutually exclusive within a single tradition (many communities hold both simultaneously: typological reading for present use, historical-critical acknowledgment for the past). ε differs sharply across the three: near-zero here (no real ethnic party), substantially higher on durable_separation_reading (real designated outsiders, active boundary maintenance), and moderate on contextual_supersession_reading (historical ethnic referent acknowledged but bracketed as no longer operative).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
