% ============================================================================
% CONSTRAINT STORY: utopian_fiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_utopian_fiction_reading, []).

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
 *   constraint_id: utopian_fiction_reading
 *   human_readable: Fiat Debate as Utopian Social Criticism Practice
 *   domain: debate_theory/political_philosophy
 *
 * SUMMARY:
 *   Competitive policy/fiat debate asks students to argue for or against
 *   fiated government action they have no power to enact. Multiple accounts
 *   exist for why this is worthwhile despite lacking formal-agenda power.
 *   This story instantiates ONE such account: the utopian-fiction reading,
 *   which locates the practice's value entirely in the pedagogical and
 *   cultural function of debaters occupying the role of a public-agenda
 *   social critic — imagining, defending, and attacking visions of policy as
 *   a species of utopian and critical literature performed live, explicitly
 *   without any claim that the activity is, or needs to be, politically
 *   efficacious in the world formal institutions inhabit. This is the most
 *   self-limiting of the six readings in the fiat_efficacy_kernel family: it
 *   wins its ground by conceding the most.
 *
 * KEY AGENTS:
 *   - competitive_debaters: primary practitioners, low formal power, high pedagogical benefit
 *   - debate_coaches: agenda-setters for what the activity means and is judged on
 *   - debate_league_administrators: institutional beneficiaries of a stable justificatory account
 *   - policymakers_and_institutions: structurally excluded by the reading's own central claim
 *   - debate_theorists: analytical observers comparing this reading against its siblings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(utopian_fiction_reading, 0.22).
domain_priors:suppression_score(utopian_fiction_reading, 0.18).
domain_priors:theater_ratio(utopian_fiction_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(utopian_fiction_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(utopian_fiction_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(utopian_fiction_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(utopian_fiction_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(utopian_fiction_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(utopian_fiction_reading, rope).
narrative_ontology:human_readable(utopian_fiction_reading, "Fiat Debate as Utopian Social Criticism Practice").
narrative_ontology:topic_domain(utopian_fiction_reading, "debate_theory/political_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(utopian_fiction_reading, '812ceb62-19c8-42aa-872c-db2417d1e093').
narrative_ontology:cs_kernel_codification('812ceb62-19c8-42aa-872c-db2417d1e093', distributed).
narrative_ontology:cs_authority_grounding('812ceb62-19c8-42aa-872c-db2417d1e093', practice).
narrative_ontology:cs_interpretation_layer_present('812ceb62-19c8-42aa-872c-db2417d1e093').
narrative_ontology:cs_reading_relation('812ceb62-19c8-42aa-872c-db2417d1e093', fiat_efficacy_kernel__empirical_precedent_reading, coexists_with).
narrative_ontology:cs_reading_relation('812ceb62-19c8-42aa-872c-db2417d1e093', fiat_efficacy_kernel__scholarship_reading, coexists_with).
narrative_ontology:cs_reading_relation('812ceb62-19c8-42aa-872c-db2417d1e093', fiat_efficacy_kernel__truth_procedure_reading, influences).
narrative_ontology:cs_reading_relation('812ceb62-19c8-42aa-872c-db2417d1e093', fiat_efficacy_kernel__predictive_synthesis_reading, coexists_with).
narrative_ontology:cs_reading_relation('812ceb62-19c8-42aa-872c-db2417d1e093', fiat_efficacy_kernel__empathy_simulation_reading, coexists_with).
narrative_ontology:cs_axiom('812ceb62-19c8-42aa-872c-db2417d1e093', foundational, value_located_in_practice_not_formal_power).
narrative_ontology:cs_axiom_status(value_located_in_practice_not_formal_power, holdable).
narrative_ontology:cs_axiom_grounding('812ceb62-19c8-42aa-872c-db2417d1e093', value_located_in_practice_not_formal_power, conventional).
narrative_ontology:cs_axiom('812ceb62-19c8-42aa-872c-db2417d1e093', foundational, efficacy_question_bracketed_not_answered).
narrative_ontology:cs_axiom_status(efficacy_question_bracketed_not_answered, holdable).
narrative_ontology:cs_axiom_grounding('812ceb62-19c8-42aa-872c-db2417d1e093', efficacy_question_bracketed_not_answered, instrumental).
narrative_ontology:cs_reference_frame('812ceb62-19c8-42aa-872c-db2417d1e093', debate_as_pedagogical_criticism_practice).
narrative_ontology:cs_drift_state('812ceb62-19c8-42aa-872c-db2417d1e093', contemporary_competitive_circuit_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('812ceb62-19c8-42aa-872c-db2417d1e093', '').
narrative_ontology:cs_kernel_id(utopian_fiction_reading, fiat_efficacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(utopian_fiction_reading, competitive_debaters).
narrative_ontology:constraint_beneficiary(utopian_fiction_reading, debate_coaches).
narrative_ontology:constraint_beneficiary(utopian_fiction_reading, debate_league_administrators).
narrative_ontology:constraint_vindicates(utopian_fiction_reading, pedagogical_value_of_simulated_agenda_setting).
narrative_ontology:constraint_vindicates(utopian_fiction_reading, criticism_practice_independent_of_formal_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Argue fiated government or policy actions in rounds with no formal power to enact anything discussed. Under this reading, they gain practice inhabiting the stance of a social critic addressing the public agenda — imagining, contesting, and defending visions of how institutions could act — without any claim that their fiat resolutions change real institutions. They can leave the activity at graduation or before with no lasting institutional consequence.
narrative_ontology:constraint_stakeholder(utopian_fiction_reading, competitive_debaters, beneficiary,
    powerless, biographical, mobile, national).

% Design curricula and judging norms that frame fiat as an educational device for practicing critical and utopian argument, explicitly instructing students not to treat in-round advocacy as a claim to real-world efficacy. They administer the norm that gives the activity this particular justification, and they benefit professionally from a defensible account of what the activity is for.
narrative_ontology:constraint_stakeholder(utopian_fiction_reading, debate_coaches, agenda_setter,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(utopian_fiction_reading, debate_coaches, beneficiary).

% Run the leagues and tournaments that depend on a stable, defensible rationale for what fiat debate accomplishes. This reading supplies that rationale (pedagogy and cultural training in criticism, not real political action), which lets the institution continue without having to answer harder questions about efficacy or influence on actual policy.
narrative_ontology:constraint_stakeholder(utopian_fiction_reading, debate_league_administrators, beneficiary,
    organized, generational, constrained, national).

% The actual holders of formal agenda power that debate resolutions purport to fiat. Under this reading they are irrelevant by design — the reading explicitly denies debate needs or has any purchase on their decisions, so their absence from the conversation is not an oversight but the reading's central structural claim.
narrative_ontology:constraint_stakeholder(utopian_fiction_reading, policymakers_and_institutions, excluded,
    institutional, generational, analytical, national).

% Scholars and practitioners who argue fiat debate either overclaims real efficacy or underdelivers on any efficacy at all, and who would push the community toward one of the sibling readings (empirical precedent, scholarship, truth procedure). This reading answers them by narrowing its own claim rather than engaging their empirical challenge, which leaves their objection formally addressed but not substantively resolved.
narrative_ontology:constraint_stakeholder(utopian_fiction_reading, debate_critics_of_fiat, excluded,
    moderate, biographical, constrained, national).

% Analyze what fiat debate is actually doing structurally, comparing this self-limiting reading against the five sibling readings that claim more (precedent, scholarship, truth-testing, prediction, empathy). They observe that this reading survives critique precisely by refusing to claim what the critique targets.
narrative_ontology:constraint_stakeholder(utopian_fiction_reading, debate_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(utopian_fiction_reading, diffuse).
narrative_ontology:fixing_cost_class(utopian_fiction_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Trains participants to occupy the stance of a social critic engaging the public agenda — constructing, defending, and attacking utopian and critical visions of policy — as a repeatable, judged, comparative practice, independent of any claim that the practice changes real institutions.
% TRANSFER_FUNCTION: Moves argumentative skill, rhetorical confidence, and a specific self-concept (myself as someone who publicly critiques policy) to debaters; moves professional legitimacy and a defensible institutional rationale to coaches and league administrators; moves nothing to the formal-agenda holders the resolutions nominally address, because the reading explicitly declines to claim any transfer there.
% ABSENT_VOICES: Policymakers and institutions are absent because the reading structurally excludes them as irrelevant to its claim of value. Critics who think fiat debate should be judged by real-world efficacy are present in the broader discourse but are answered by the reading's retreat to pedagogy rather than by engaging their empirical claim directly.
% DISAPPEARANCE_RATIONALE: If fiat debate vanished overnight, competitive debaters and coaches would lose a specific formation activity and a professional field would contract, so arrangements clearly depend on it at that level. But because this reading denies the activity has any real-world policy effect, its disappearance would leave the actual public agenda and formal institutions completely unchanged — the parties dispute which scale of 'world' is the relevant one, which is exactly the boundary this reading draws around itself.
% FOUNDING_PROBLEM: Students needed a low-stakes, repeatable practice environment for developing the skills and dispositions of public policy criticism — argument construction, opposition research, articulating alternative futures — without the cost, risk, or gatekeeping of actual political participation.
% FOUNDING_PROBLEM_CORROBORATION: Communication and education scholars outside competitive debate (rhetoric and civic-education researchers) corroborate that simulated deliberative practice measurably develops argumentative and critical skills transferable to civic life, independent of debate community self-report. No corroboration exists, however, for any claim beyond skill formation — that absence is itself part of what this reading concedes by design.
narrative_ontology:disappearance_verdict(utopian_fiction_reading, contested).
narrative_ontology:founding_problem_status(utopian_fiction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(utopian_fiction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(utopian_fiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(utopian_fiction_reading, 0.22, 'claude-sonnet-5', 'fiat_efficacy_kernel_2026_20260803_102258', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(utopian_fiction_reading_tests).
:- end_tests(utopian_fiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) and rises only slightly over the interval because the activity draws mainly on participants' voluntary time and tuition/program resources rather than extracting rents from an unwilling party; there are no victims because no one is coerced into treating debate's fiat as real-world binding under this reading. Theater ratio is moderate and rising (0.28 to 0.40) because, over time, competitive debate communities have layered increasing performative and gamesmanship elements (technical jargon, speed, judge-signaling conventions) onto the core critical-practice function, without that drift threatening the underlying pedagogical claim this reading makes. Suppression and accessibility_collapse are both moderate-low: alternatives to fiat debate (other forensics formats, civic education models) remain visible and are not actively suppressed, and resistance is moderate because critics of the activity's efficacy claims remain active and unresolved by this reading, not silenced by it.
 *
 * PERSPECTIVAL GAP:
 *   From inside the activity (debaters, coaches), the practice reads as genuine, low-cost coordination around skill-building. From the vantage of a critic pressing for real efficacy (debate_critics_of_fiat), the same structure reads as a retreat that avoids the harder empirical question rather than answering it. The engine should register this as a coordination-dominant profile precisely because the reading structurally declines to make the extraction-adjacent claims (real policy influence, real epistemic authority) that the sibling readings advance.
 *
 * DIRECTIONALITY LOGIC:
 *   Debaters and coaches sit near the beneficiary end: they receive skill formation, professional legitimacy, and a coherent story for participation, and bear only the ordinary costs of time and effort common to any educational activity. League administrators similarly benefit from having a stable rationale to operate under. Policymakers are excluded rather than positioned as targets — the reading's entire structure is built to ensure they are neither burdened nor implicated by debate's fiat claims, which is why no victims are declared.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for low-stakes practice in public policy criticism) remains live and is corroborated by external civic-education scholarship, so this is not a case of mandatrophy — the mandate has not outlived its function. What could look like mandatrophy from a sibling reading's perspective (debate 'failing' to be politically efficacious) is not a failure under this reading at all, because efficacy was never the promise. The classification must not treat the absence of real-world policy impact as evidence of decayed function; that absence is the reading's premise, not its drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiat_efficacy_kernel_reading_choice,
    'Is the utopian_fiction_reading the correct or dominant reading of why fiat debate is valuable, or do practitioners and coaches actually operate under one of the five sibling readings (empirical_precedent, scholarship, truth_procedure, predictive_synthesis, empathy_simulation) while using this reading''s language as public-facing cover?',
    'Ethnographic or survey research into what debate coaches and league administrators actually tell students the activity is for, compared against what they claim in public accreditation and funding contexts; divergence between internal and external framing would indicate a different reading is operative.',
    'If a sibling reading (e.g., truth_procedure_reading, which claims epistemic authority) is actually operative, the low extraction and low suppression scores authored here would not transfer, since sibling readings carry different beneficiary/victim structures and different claims to authority that could implicate policymakers as targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiat_efficacy_kernel_reading_choice, conceptual, 'Whether the self-limiting utopian-fiction account is the community''s actual operative justification or a defensive framing layered over a stronger claim.').

omega_variable(
    theater_drift_versus_core_function,
    'Does the rising theater_ratio (competitive gamesmanship, technical jargon, speed-reading conventions) represent genuine drift away from the pedagogical criticism function this reading claims, or is it orthogonal — a competitive-format artifact that coexists with intact pedagogical value?',
    'Compare skill-transfer outcomes (civic engagement, argument quality in non-debate contexts) between high-theater competitive formats and lower-theater educational formats within the same debate tradition.',
    'If theater drift correlates with reduced skill transfer, the reading''s central pedagogical claim weakens over time even without any claim of real-world policy efficacy, which would push the classification toward piton (declining function, persisting performative form) rather than rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theater_drift_versus_core_function, empirical, 'Whether rising performative competitive conventions erode the pedagogical function this reading rests its entire claim on.').

omega_variable(
    self_limitation_as_strength_or_weakness,
    'Is the reading''s explicit refusal to claim real-world efficacy a principled epistemic virtue (honesty about what a simulation can deliver) or a strategic retreat that forecloses legitimate critique by making the claim unfalsifiable at the level critics actually care about?',
    'No empirical resolution mechanism exists for this framing question; it depends on whether one values activities by their stated aims or by their broader social effects regardless of stated aims.',
    'Under the first framing, this reading is the most defensible of the six and should classify cleanly as rope; under the second, its self-limitation is itself a mild extraction of legitimacy (borrowing the prestige of ''political debate'' while disclaiming political stakes), pushing toward a mild tangled_rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(self_limitation_as_strength_or_weakness, preference, 'Whether the reading''s narrowness is honest self-limitation or a strategic hedge against critique.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(utopian_fiction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(utop_tr_t0, utopian_fiction_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(utop_tr_t8, utopian_fiction_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(utop_tr_t16, utopian_fiction_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(utop_tr_t24, utopian_fiction_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(utop_tr_t32, utopian_fiction_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(utop_tr_t40, utopian_fiction_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(utop_be_t0, utopian_fiction_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(utop_be_t8, utopian_fiction_reading, base_extractiveness, 8, 0.17).
narrative_ontology:measurement(utop_be_t16, utopian_fiction_reading, base_extractiveness, 16, 0.19).
narrative_ontology:measurement(utop_be_t24, utopian_fiction_reading, base_extractiveness, 24, 0.2).
narrative_ontology:measurement(utop_be_t32, utopian_fiction_reading, base_extractiveness, 32, 0.21).
narrative_ontology:measurement(utop_be_t40, utopian_fiction_reading, base_extractiveness, 40, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(utopian_fiction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(utopian_fiction_reading, identity_coordination).
narrative_ontology:affects_constraint(utopian_fiction_reading, empirical_precedent_reading).
narrative_ontology:affects_constraint(utopian_fiction_reading, scholarship_reading).
narrative_ontology:affects_constraint(utopian_fiction_reading, truth_procedure_reading).
narrative_ontology:affects_constraint(utopian_fiction_reading, predictive_synthesis_reading).
narrative_ontology:affects_constraint(utopian_fiction_reading, empathy_simulation_reading).

% DUAL FORMULATION NOTE:
% This story is one of six sibling readings of the fiat_efficacy_kernel, each claiming a distinct structural account of why fiat/policy debate has value despite the absence of formal-agenda power. utopian_fiction_reading is the most self-limiting: it explicitly denies the real-world-efficacy claims that the empirical_precedent, scholarship, truth_procedure, and predictive_synthesis readings make, and it locates value in practice-of-criticism rather than in the perspective-taking capacity the empathy_simulation_reading emphasizes. Each sibling should carry its own ε and its own stakeholder set; do not average or hedge across them in any single story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
