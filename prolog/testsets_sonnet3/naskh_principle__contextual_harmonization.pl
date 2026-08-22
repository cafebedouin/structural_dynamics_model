% ============================================================================
% CONSTRAINT STORY: naskh_principle__contextual_harmonization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__contextual_harmonization, []).

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
 *   constraint_id: naskh_principle__contextual_harmonization
 *   human_readable: Contextual Harmonization Reading of Naskh (No Abrogation, Only Contextual Specification)
 *   domain: religious_legal/hermeneutic
 *
 * SUMMARY:
 *   This story instantiates the contextual-harmonization reading of the naskh
 *   (abrogation) kernel: the claim that apparent contradictions among Quranic
 *   verses are resolved not by declaring later verses to cancel earlier ones,
 *   but by showing each verse addresses a distinct revelatory and situational
 *   context, all of which remain simultaneously valid. This reading
 *   coordinates a genuine theological need (preserving divine textual
 *   coherence) while creating an extraction dynamic: interpretive authority
 *   concentrates in jurists skilled at contextual reconstruction, at the cost
 *   of legal predictability for those who need closed, administrable rulings.
 *   The reading is authored on its own terms; the classical_abrogation
 *   reading (fixed chronological cancellation) and progressive_restriction
 *   reading (permission-to-restriction pedagogical arc) are separate
 *   constraints, sibling files in the same kernel family, not alternative
 *   measurements of this one.
 *
 * KEY AGENTS:
 *   - contextualist_jurists: agenda_setter/beneficiary (organized/mobile) — administer and profit from the contextual-reconstruction methodology
 *   - reformist_theologians: beneficiary (moderate/mobile) — use the reading to limit application of harsh verses without declaring them abrogated
 *   - lay_believers_seeking_coherence: beneficiary (powerless/constrained) — gain theological comfort but depend on jurist mediation
 *   - legal_certainty_seekers and litigants_needing_fixed_rulings: payer (powerless/trapped) — bear the cost of reopened, unpredictable rulings
 *   - classical_madhhab_authorities: payer/excluded (institutional/constrained) — lose the authority of their closed abrogation catalogues
 *   - comparative_religion_scholars: observer (analytical) — see the full contest across the three kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__contextual_harmonization, 0.42).
domain_priors:suppression_score(naskh_principle__contextual_harmonization, 0.38).
domain_priors:theater_ratio(naskh_principle__contextual_harmonization, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, extractiveness, 0.42).
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(naskh_principle__contextual_harmonization, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__contextual_harmonization, tangled_rope).
narrative_ontology:human_readable(naskh_principle__contextual_harmonization, "Contextual Harmonization Reading of Naskh (No Abrogation, Only Contextual Specification)").
narrative_ontology:topic_domain(naskh_principle__contextual_harmonization, "religious_legal/hermeneutic").

domain_priors:requires_active_enforcement(naskh_principle__contextual_harmonization).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__contextual_harmonization, '67b67192-af3b-45d3-948a-c2a6c274fed5').
narrative_ontology:cs_kernel_codification('67b67192-af3b-45d3-948a-c2a6c274fed5', fixed_text).
narrative_ontology:cs_authority_grounding('67b67192-af3b-45d3-948a-c2a6c274fed5', lineage).
narrative_ontology:cs_interpretation_layer_present('67b67192-af3b-45d3-948a-c2a6c274fed5').
narrative_ontology:cs_reading_relation('67b67192-af3b-45d3-948a-c2a6c274fed5', naskh_principle__classical_abrogation, forecloses).
narrative_ontology:cs_reading_relation('67b67192-af3b-45d3-948a-c2a6c274fed5', naskh_principle__progressive_restriction, coexists_with).
narrative_ontology:cs_axiom('67b67192-af3b-45d3-948a-c2a6c274fed5', foundational, no_verse_is_ever_invalidated).
narrative_ontology:cs_axiom_status(no_verse_is_ever_invalidated, holdable).
narrative_ontology:cs_axiom_grounding('67b67192-af3b-45d3-948a-c2a6c274fed5', no_verse_is_ever_invalidated, deontological).
narrative_ontology:cs_axiom('67b67192-af3b-45d3-948a-c2a6c274fed5', secondary, contradiction_resolved_by_context_not_chronology).
narrative_ontology:cs_axiom_status(contradiction_resolved_by_context_not_chronology, holdable).
narrative_ontology:cs_axiom_grounding('67b67192-af3b-45d3-948a-c2a6c274fed5', contradiction_resolved_by_context_not_chronology, conventional).
narrative_ontology:cs_reference_frame('67b67192-af3b-45d3-948a-c2a6c274fed5', classical_naskh_catalogue_consensus).
narrative_ontology:cs_drift_state('67b67192-af3b-45d3-948a-c2a6c274fed5', contemporary_reformist_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('67b67192-af3b-45d3-948a-c2a6c274fed5', '').
narrative_ontology:cs_kernel_id(naskh_principle__contextual_harmonization, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__contextual_harmonization, contextualist_jurists).
narrative_ontology:constraint_beneficiary(naskh_principle__contextual_harmonization, reformist_theologians).
narrative_ontology:constraint_beneficiary(naskh_principle__contextual_harmonization, lay_believers_seeking_coherence).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, legal_certainty_seekers).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, classical_madhhab_authorities).
narrative_ontology:constraint_victim(naskh_principle__contextual_harmonization, litigants_needing_fixed_rulings).
narrative_ontology:constraint_vindicates(naskh_principle__contextual_harmonization, quranic_internal_consistency_doctrine).
narrative_ontology:constraint_vindicates(naskh_principle__contextual_harmonization, divine_speech_non_contradiction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and apply the asbab al-nuzul (occasions of revelation) methodology to argue that no verse is truly cancelled — every verse governs its own circumstantial domain. This methodology is their professional and intellectual capital; they administer which contexts apply to which cases and gain interpretive authority precisely because the harmonization method requires their specialized contextual reconstruction.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, contextualist_jurists, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__contextual_harmonization, contextualist_jurists, beneficiary).

% Use contextual harmonization to argue that harsher or more restrictive verses apply only to specific historical circumstances (war, particular tribal disputes) rather than universally, enabling more permissive contemporary applications on issues like apostasy, slavery, and gender relations without declaring any verse false or cancelled.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, reformist_theologians, beneficiary,
    moderate, civilizational, mobile, global).

% Receive a theologically satisfying account in which no part of their scripture is ever wrong or discarded — everything remains eternally valid in its proper context. This resolves the psychological discomfort of apparent divine self-contradiction but requires trusting jurist-mediated contextual determinations they cannot independently verify.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, lay_believers_seeking_coherence, beneficiary,
    powerless, biographical, constrained, local).

% Litigants, judges applying sharia-based family or criminal law, and ordinary Muslims needing a definite ruling (e.g., on inheritance shares, testimony rules, or apostasy penalties) find that under this reading no verse is ever simply superseded — every apparently resolved question can be reopened by recontextualization. They bear the cost of interpretive instability precisely where they need finality.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, legal_certainty_seekers, payer,
    powerless, immediate, trapped, local).

% Established schools of jurisprudence built centuries of case law on chronological abrogation (naskh) rulings — a fixed, closed catalogue of which verses cancel which. This reading destabilizes that closed catalogue, forcing re-litigation of settled doctrine and reducing the authority of traditional abrogation-lists (like those compiled by al-Suyuti or Ibn Salama) that these institutions' authority depends on.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, classical_madhhab_authorities, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(naskh_principle__contextual_harmonization, classical_madhhab_authorities, excluded).

% Individuals before religious courts (divorce, inheritance, criminal hudud cases) need a determinate answer to 'what does the verse require now.' Contextual harmonization can be invoked by either party's advocate to argue the applicable context differs from precedent, extending litigation and creating unpredictable outcomes for people with immediate, high-stakes personal circumstances.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, litigants_needing_fixed_rulings, payer,
    powerless, immediate, trapped, local).

% Study how different Islamic legal traditions resolve apparent scriptural contradiction, comparing contextual harmonization to chronological abrogation and progressive-restriction readings as competing hermeneutic strategies with different institutional consequences.
narrative_ontology:constraint_stakeholder(naskh_principle__contextual_harmonization, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a doctrine that preserves the theological premise that the Quran, as divine speech, cannot truly contradict itself — every apparent contradiction is resolved by showing the two verses address different, non-overlapping circumstances rather than one verse cancelling another.
% TRANSFER_FUNCTION: Moves interpretive authority and case-resolution power from closed, chronologically-fixed abrogation catalogues (controlled by classical madhhab institutions) toward contextualist jurists who can reconstruct or dispute the applicable circumstantial context for any verse in any new case.
% ABSENT_VOICES: Litigants and judges who need administrable, predictable rulings rarely have standing in the hermeneutic debate itself — the contest over naskh methodology occurs among jurists and theologians, not among the people whose divorce, inheritance, or criminal cases are decided under whichever reading prevails.
% DISAPPEARANCE_RATIONALE: If contextual harmonization vanished as an accepted methodology, classical abrogation catalogues would regain uncontested authority, legal rulings on previously 'reopened' questions would revert to fixed chronological-supersession answers, and reformist arguments relying on contextual specification (rather than outright abrogation) to limit harsh verses would lose their doctrinal foothold — significant swaths of contemporary reformist jurisprudence depend on this reading remaining available.
% FOUNDING_PROBLEM: Early exegetes faced an apparent theological problem: the Quran contains verses on the same topics (fighting, inheritance, wine, qibla direction) that seem to give different or conflicting rulings. Something had to explain the coexistence of these verses without impugning divine consistency.
% FOUNDING_PROBLEM_CORROBORATION: Sunni and Shia legal theorists across centuries (not only contextualist jurists) attest that the underlying exegetical problem — reconciling apparently conflicting verses — remains a live methodological question; classical abrogation theorists like al-Suyuti and modern critics of naskh doctrine (including scholars skeptical of BOTH resolution methods, e.g. some Quranist and academic Islamic-studies scholars outside any jurist guild) independently corroborate that the underlying contradiction problem is real and unresolved by consensus, even as they disagree on which resolution mechanism is correct.
narrative_ontology:disappearance_verdict(naskh_principle__contextual_harmonization, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__contextual_harmonization, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__contextual_harmonization, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(naskh_principle__contextual_harmonization, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__contextual_harmonization, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__contextual_harmonization_tests).
:- end_tests(naskh_principle__contextual_harmonization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the reading does genuine coordination work (resolving a real theological tension) but that coordination consistently redistributes case-resolution power toward contextualist jurists and away from settled catalogues and the litigants who relied on them. Suppression (0.38) is lower than extraction because the reading does not primarily operate through coercion — it operates through interpretive reopening enforced by scholarly consensus-formation and judicial deference to jurist authority, which is real but less coercive than, say, criminal enforcement. Accessibility collapse (0.35) is moderate-low: alternative resolution methods (classical abrogation, progressive restriction) remain visible and contested, so alternatives have not collapsed. Resistance (0.55) is meaningfully high: classical authorities and certainty-seekers actively contest this reading's expansion, which is exactly why it functions as a tangled rope rather than a settled rope.
 *
 * PERSPECTIVAL GAP:
 *   From the contextualist jurist seat, this reading looks like a rope: pure theological coordination solving a real interpretive problem with minimal coercive overhead. From the litigant or classical-authority seat, the same structure computes as extractive: their settled expectations are perpetually reopenable, and authority has been redirected toward a class of interpreters they cannot bypass. The engine's per-seat computation should register this divergence rather than resolve it — that divergence is what marks the reading as tangled rather than a clean rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Contextualist jurists and reformist theologians sit near the beneficiary end: they gain interpretive latitude and professional authority from the reading's adoption. Lay believers seeking theological coherence benefit psychologically but are not the primary structural beneficiaries — they are closer to symmetric, receiving comfort but bearing some downstream unpredictability. Legal certainty seekers and litigants are structural targets: trapped exit (they cannot simply choose a different legal system for an active case), and the reading's operation directly increases the instability they must absorb. Classical madhhab authorities are targets of a different kind — institutional actors whose authority base (the closed abrogation catalogue) is eroded by this reading's success, giving them constrained exit despite high nominal power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling apparently conflicting verses without impugning divine consistency — remains live by outside corroboration (independent Islamic-studies scholarship and cross-tradition jurists confirm the underlying exegetical tension is real, not manufactured). This blocks a mandatrophy verdict: the coordination function this reading performs has not gone dead while the extractive apparatus persists on inertia. However the story documents a genuine tension the mandatrophy frame is built to catch: distinguishing 'this reading solves a live theological problem' from 'this reading is used to indefinitely defer legal closure that jurists benefit from deferring.' The tangled_rope classification holds both: real coordination (textual coherence) plus asymmetric extraction (interpretive power flowing to a jurist class at litigants' expense) under active enforcement (ongoing scholarly and judicial contestation over which verses' rulings can be reopened).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harmonization_vs_indefinite_deferral,
    'Is contextual harmonization a stable interpretive method that resolves specific contradictions once per verse-pair, or does it function as an indefinitely reopenable procedure that jurists invoke whenever a settled ruling becomes inconvenient?',
    'Track whether specific verse-pairs harmonized under this method, once resolved by a recognized jurist consensus, remain closed in subsequent case law, or whether the same pairs get recontextualized repeatedly across different rulings and eras.',
    'If harmonizations remain closed once made, the reading functions closer to a genuine rope with a real, bounded coordination product. If the same textual pairs are repeatedly recontextualized to fit new circumstances, the reading functions closer to a snare wearing coordination language, since ''contextual specification'' becomes an unlimited discretionary override available to whoever controls the jurist seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harmonization_vs_indefinite_deferral, empirical, 'Whether contextual harmonization is a bounded resolution method or an indefinitely renewable discretionary override.').

omega_variable(
    kernel_reading_selection_criteria,
    'What determines which of the three naskh-kernel readings (classical_abrogation, contextual_harmonization, progressive_restriction) a given jurist, school, or era adopts — internal exegetical merit, or the institutional/political interests the reading happens to serve?',
    'Comparative historical study of when and why particular schools or reformers shifted between naskh readings, cross-referenced against the institutional interests each shift served (e.g., whether adoption of contextual harmonization correlates with reformist political projects rather than purely textual argument).',
    'If reading-selection tracks institutional interest more than textual argument, all three kernel readings — including this one — should be read partly as instruments of the interests they serve, which does not change this story''s own epsilon but strengthens the case that the kernel itself, not any single reading, is the locus of contestation worth tracking across the family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_criteria, conceptual, 'Whether reading-selection within the naskh kernel is driven by exegetical merit or institutional interest.').

omega_variable(
    jurist_authority_concentration,
    'Does the contextual-harmonization method require an unusually specialized interpretive elite (asbab al-nuzul reconstruction, historical-critical training) that narrows genuine decision-making to a smaller circle than classical abrogation''s fixed catalogue did?',
    'Compare the credentialing and training requirements actually invoked by contextualist jurists against those required to apply a closed abrogation list; measure whether the practicing interpretive circle for contextual harmonization is smaller or larger than for classical abrogation in a given era.',
    'If the interpretive circle narrows, the concentration of authority in contextualist jurists documented in this story''s directionality analysis is understated; if it does not narrow, the extraction reading should be weighted down relative to the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurist_authority_concentration, empirical, 'Whether contextual harmonization narrows or widens the practical interpretive elite relative to classical abrogation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__contextual_harmonization, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__contextual_harmonization, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nask_tr_t20, naskh_principle__contextual_harmonization, theater_ratio, 20, 0.22).
narrative_ontology:measurement(nask_tr_t40, naskh_principle__contextual_harmonization, theater_ratio, 40, 0.24).
narrative_ontology:measurement(nask_tr_t60, naskh_principle__contextual_harmonization, theater_ratio, 60, 0.25).
narrative_ontology:measurement(nask_tr_t80, naskh_principle__contextual_harmonization, theater_ratio, 80, 0.27).
narrative_ontology:measurement(nask_tr_t100, naskh_principle__contextual_harmonization, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__contextual_harmonization, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(nask_be_t20, naskh_principle__contextual_harmonization, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(nask_be_t40, naskh_principle__contextual_harmonization, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(nask_be_t60, naskh_principle__contextual_harmonization, base_extractiveness, 60, 0.39).
narrative_ontology:measurement(nask_be_t80, naskh_principle__contextual_harmonization, base_extractiveness, 80, 0.41).
narrative_ontology:measurement(nask_be_t100, naskh_principle__contextual_harmonization, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__contextual_harmonization, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(nask_su_t20, naskh_principle__contextual_harmonization, suppression_requirement, 20, 0.32).
narrative_ontology:measurement(nask_su_t40, naskh_principle__contextual_harmonization, suppression_requirement, 40, 0.34).
narrative_ontology:measurement(nask_su_t60, naskh_principle__contextual_harmonization, suppression_requirement, 60, 0.36).
narrative_ontology:measurement(nask_su_t80, naskh_principle__contextual_harmonization, suppression_requirement, 80, 0.37).
narrative_ontology:measurement(nask_su_t100, naskh_principle__contextual_harmonization, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__contextual_harmonization, identity_coordination).
narrative_ontology:boltzmann_floor_override(naskh_principle__contextual_harmonization, 0.1).
narrative_ontology:affects_constraint(naskh_principle__contextual_harmonization, naskh_principle__classical_abrogation).
narrative_ontology:affects_constraint(naskh_principle__contextual_harmonization, naskh_principle__progressive_restriction).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the naskh_principle kernel (see cs_structure.reading_relations for the typed edges). classical_abrogation authors a fixed, closed chronological-supersession catalogue with high legal predictability but low adaptability. progressive_restriction authors a pedagogical-arc reading that treats restriction-direction as divine teaching without declaring any verse invalid. contextual_harmonization (this file) trades predictability for adaptive coherence, concentrating interpretive power in contextualist jurists. Each reading has its own epsilon, beneficiaries, victims, and classification; none should be averaged with the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
