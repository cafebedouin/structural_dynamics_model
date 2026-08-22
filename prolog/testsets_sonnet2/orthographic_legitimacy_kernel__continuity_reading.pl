% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__continuity_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: orthographic_legitimacy_kernel__continuity_reading
 *   human_readable: Orthographic Legitimacy via Textual-Continuity Reading (Turkish Script Reform)
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the continuity_reading of the
 *   orthographic_legitimacy_kernel: the claim that legitimate orthography is
 *   whatever preserves a population's access to its own historical,
 *   religious, and literary corpus. Under this reading, the 1928 Turkish
 *   alphabet reform is evaluated not for its administrative or ideological
 *   aims but for its effect on textual continuity — and the effect this
 *   reading foregrounds is severance: post-reform generations cannot read
 *   pre-reform material without specialist mediation. The reading treats
 *   script incompatibility itself as a near-mountain fact (once two writing
 *   systems diverge and one is abandoned in public education, unmediated
 *   bidirectional access does not spontaneously return), while still logging
 *   the modest concentrated benefit accruing to those who retained the older
 *   literacy. This is a low-ε reading by design: the story is about
 *   accumulating LOSS of access, not about an ongoing extraction mechanism,
 *   and the sibling readings (instrumentalist, modernist) are separate
 *   constraints with their own ε, not alternative measurements of this one.
 *
 * KEY AGENTS:
 *   - arabic_script_literate_clergy: retained beneficiary of continuity
 *   - ottoman_archive_custodians: retained beneficiary, professional
 *   - post_reform_generations: primary bearer of severed access
 *   - religious_students_denied_source_texts: bearer of mediated-access cost
 *   - the_turkish_state: agenda_setter whose founding act fixed the incompatibility
 *   - linguistic_historians: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__continuity_reading, 0.22).
domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, 0.68).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__continuity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__continuity_reading, mountain).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__continuity_reading, "Orthographic Legitimacy via Textual-Continuity Reading (Turkish Script Reform)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__continuity_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:emerges_naturally(orthographic_legitimacy_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__continuity_reading, '206a2476-5206-4ec2-96f8-491d4f68bdd5').
narrative_ontology:cs_kernel_codification('206a2476-5206-4ec2-96f8-491d4f68bdd5', distributed).
narrative_ontology:cs_authority_grounding('206a2476-5206-4ec2-96f8-491d4f68bdd5', lineage).
narrative_ontology:cs_interpretation_layer_present('206a2476-5206-4ec2-96f8-491d4f68bdd5').
narrative_ontology:cs_reading_relation('206a2476-5206-4ec2-96f8-491d4f68bdd5', orthographic_legitimacy_kernel__modernist_reading, forecloses).
narrative_ontology:cs_reading_relation('206a2476-5206-4ec2-96f8-491d4f68bdd5', orthographic_legitimacy_kernel__instrumentalist_reading, coexists_with).
narrative_ontology:cs_axiom('206a2476-5206-4ec2-96f8-491d4f68bdd5', foundational, textual_continuity_is_constitutive_of_legitimacy).
narrative_ontology:cs_axiom_status(textual_continuity_is_constitutive_of_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('206a2476-5206-4ec2-96f8-491d4f68bdd5', textual_continuity_is_constitutive_of_legitimacy, conventional).
narrative_ontology:cs_axiom('206a2476-5206-4ec2-96f8-491d4f68bdd5', secondary, rupture_from_prior_corpus_is_a_cost_not_a_virtue).
narrative_ontology:cs_axiom_status(rupture_from_prior_corpus_is_a_cost_not_a_virtue, holdable).
narrative_ontology:cs_axiom_grounding('206a2476-5206-4ec2-96f8-491d4f68bdd5', rupture_from_prior_corpus_is_a_cost_not_a_virtue, deontological).
narrative_ontology:cs_reference_frame('206a2476-5206-4ec2-96f8-491d4f68bdd5', ottoman_textual_continuity).
narrative_ontology:cs_drift_state('206a2476-5206-4ec2-96f8-491d4f68bdd5', post_1928_reform_era, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('206a2476-5206-4ec2-96f8-491d4f68bdd5', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__continuity_reading, arabic_script_literate_clergy).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__continuity_reading, ottoman_archive_custodians).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, religious_students_denied_source_texts).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__continuity_reading, textual_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain the ability to read Quranic exegesis, Ottoman legal opinions, and devotional literature directly in the script those texts were composed in. Their authority as interpreters partly rests on possessing a literacy the wider population no longer holds; they did not design the incompatibility, but they are the ones left standing on the far side of it.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, arabic_script_literate_clergy, beneficiary,
    organized, generational, identity_locked, national).

% Maintain and interpret centuries of administrative, legal, and literary documents written in Ottoman Turkish (Arabic script). Their professional standing depends on a specialized skill that becomes rarer and more valuable precisely because the reform cut off casual public access to the same material.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, ottoman_archive_custodians, beneficiary,
    moderate, generational, constrained, national).

% Educated entirely in the Latin alphabet, they cannot read their grandparents' letters, pre-1928 newspapers, tombstones, or religious texts without specialist mediation or translation. The severance is not something they chose or can individually undo; it was set before they were born and the cost is paid in lost direct access, generation after generation.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations, payer,
    powerless, civilizational, trapped, national).

% Seeking religious education, they must now learn a second script system or rely entirely on transliterated/translated intermediaries to access primary theological sources that earlier generations read directly. Some pursue specialized Ottoman-script literacy at real personal cost; most simply accept mediated access as the ceiling of what is available to them.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, religious_students_denied_source_texts, payer,
    powerless, biographical, constrained, national).

% Enacted and enforces the 1928 alphabet reform as settled national policy; from this reading's vantage the state is not scored as extracting from the continuity break, but it is the party whose 1928 decision is the physical fact this reading treats as now irreversible — the incompatibility persists because the state's founding act, not any party's ongoing choice, fixed the two scripts apart.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, the_turkish_state, agenda_setter,
    institutional, civilizational, analytical, national).

% Study the reform's effects on literacy, textual access, and cultural transmission from outside any interest in defending or reversing it. They document the continuity break as an empirical fact of orthographic history rather than adjudicating whether the reform was justified.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, linguistic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None, properly speaking — this reading does not claim the script change coordinates a shared problem; it treats the resulting script incompatibility as a fixed structural fact about which written system can access which prior texts, analogous to a natural discontinuity rather than a solved coordination problem.
% TRANSFER_FUNCTION: Direct, unmediated textual access moves away from the general population and concentrates in a narrowing specialist class (religious scholars, Ottoman archivists) who retain or acquire Arabic-script literacy; nothing material is transferred TO anyone by the incompatibility itself — the transfer is a loss of a previously distributed capability, redistributed toward those who kept the old skill.
% ABSENT_VOICES: The generation that made the 1928 decision cannot be questioned about whether they anticipated multi-generational severance from religious and literary sources; today's ordinary citizens, who bear the access cost, were never consulted on a decision made before their birth and have no forum in which the orthographic choice itself is revisited.
% DISAPPEARANCE_RATIONALE: The script incompatibility does not disappear by any party's decision the way an enforced fee or a policy would — from this reading's lights it is now a physical/historical fact (a discontinuity in the written record) rather than an active arrangement anyone maintains day to day. Removing 'legitimacy claims about it' would not restore direct access to pre-1928 texts; the severance itself is not a maintained constraint but a settled fact this reading treats as mountain-like.
% FOUNDING_PROBLEM: The reform was undertaken (by the reading's own lights, as the state's founding act) with stated aims of modernization and literacy — but the continuity reading's specific concern is that whatever those aims were, the reform severed the population's unmediated access to a millennium of Ottoman, Islamic, and literary textual tradition.
% FOUNDING_PROBLEM_CORROBORATION: Independent linguistic historians and archivists outside any religious or nationalist interest corroborate that direct public access to pre-1928 Ottoman-script material collapsed after the reform and has not recovered; UNESCO-adjacent heritage-preservation literature on endangered script literacy makes the same observation from outside Turkish domestic politics.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__continuity_reading, world_unchanged).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__continuity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__continuity_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__continuity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(orthographic_legitimacy_kernel__continuity_reading),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(orthographic_legitimacy_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22 at T=96) and rising only slowly because this reading does not see an active extraction mechanism — it sees an accumulating access gap that widens gently as generational distance from pre-reform literacy grows and as fewer people retain any personal bridge to it. Suppression starts high (0.85) at reform enactment, reflecting the state's initial enforcement of exclusive Latin-script education, and gradually eases (to 0.68) as the incompatibility becomes self-sustaining through generational turnover rather than requiring continued active enforcement — the suppression shifts from policy enforcement to structural default. Accessibility collapse is authored very high (0.88): once a generation is raised entirely in the new script, the alternative of casual bilingual literacy does not really exist for them without deliberate specialist study. Resistance is moderate (0.35): active resistance from clergy, some historians, and cultural conservatives exists but has not reversed the reform in nearly a century.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (clergy, archivists), the constraint looks like a mountain: something now beyond anyone's power to casually reverse, defining who can and cannot read what. From the victim seats (post-reform generations), the same structure looks like an inherited deprivation — not something actively enforced against them today, but a closed door nonetheless. The engine should compute these divergently from the same low-ε, high-accessibility-collapse structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Arabic-script-literate clergy and Ottoman archive custodians are coded as beneficiaries because the severance concentrates value in their retained skill — not because they engineered the severance, but because it structurally advantages them relative to the general population. Post-reform generations and religious students are victims: they bear the cost of an access gap they did not create and largely cannot close without significant individual investment. The state sits as agenda_setter but is deliberately NOT scored as an extraction beneficiary under this reading — the continuity reading treats the state's founding act as the fixing event, not as an ongoing rent-collection mechanism, which is why ε stays low despite clear victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (this reading's construal: what continuity was severed, and why) is marked dead as an active state project — nobody is actively pursuing further severance from pre-1928 texts, the reform is a century-old settled fact — while the disappearance_verdict is world_unchanged, because the severance is not a live arrangement anyone is maintaining that could be undone by removing enforcement. This combination is exactly what should NOT trigger a mandatrophy/capture flag: there is no zombie institution collecting rents from a dead problem here, only an accumulated historical fact with diffuse, non-extractive costs. Classifying this as mountain-with-declared-beneficiaries (FSM candidate) rather than snare or tangled_rope prevents mislabeling a genealogical/historical loss as an active extraction racket.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_vs_constructed_severance,
    'Is the post-1928 script incompatibility a genuine natural-law-like fact of orthographic divergence, or a constructed and actively defended state choice that continues to benefit specific literate classes and could in principle be reversed (e.g., via bilingual education policy)?',
    'Comparative study of societies that reversed or partially reversed analogous script reforms (e.g., limited reintroduction of traditional scripts in education) to determine whether the incompatibility is technically reversible at acceptable cost, versus genuinely locked in by compounding generational effects.',
    'If reversible at moderate cost and merely defended by inertia or clergy/archivist professional interest, this reading''s mountain claim weakens toward tangled_rope or piton; if genuinely locked in by compounding illiteracy-in-the-old-script across generations, the mountain claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_constructed_severance, conceptual, 'Whether the script severance is natural-law-like or a defended, reversible policy artifact.').

omega_variable(
    committer_framing_location,
    'This constraint is one reading (continuity_reading) of the orthographic_legitimacy_kernel; sibling readings (instrumentalist_reading, modernist_reading) locate legitimacy in literacy/administrative efficiency and in Western-modernity alignment respectively. Where exactly does the disagreement between these readings live?',
    'Compare the three readings'' beneficiary/victim structures directly: instrumentalist_reading likely treats the same severed-access population as an acceptable cost of literacy gains (low victim salience); modernist_reading likely treats the severance as the POINT of the reform (victims reframed as beneficiaries of modernization) rather than as a cost. The disagreement is located in whether textual severance is (a) an unfortunate but real loss (this reading), (b) an acceptable tradeoff for a different good (instrumentalist), or (c) a desired rupture (modernist).',
    'If a sibling reading recodes the same population as beneficiaries rather than victims, that reading''s ε and type will diverge sharply from this one even though both describe the same 1928 event — confirming the readings are structurally distinct constraints, not measurement variants of one constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_framing_location, conceptual, 'Locating exactly where the three kernel readings diverge in beneficiary/victim assignment.').

omega_variable(
    suppression_mechanism_shift,
    'Is the currently-measured suppression (0.68 at T=96) still partly state-enforced (e.g., religious education policy, script use restrictions) or now entirely a passive/structural effect of generational illiteracy in the old script?',
    'Review of current Turkish education and religious-instruction policy regarding Ottoman-script literacy access, compared against enforcement intensity at reform enactment.',
    'If suppression is now purely structural (no active enforcement), the constraint''s mountain-like character strengthens; if meaningful active suppression persists (e.g., restrictions on religious-script instruction), the tangled_rope reading gains support and the mountain claim weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_shift, empirical, 'Whether present-day access suppression is active enforcement or passive structural inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__continuity_reading, 0, 96).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(orth_tr_t0, observed).
narrative_ontology:measurement(orth_tr_t12, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 12, 0.07).
narrative_ontology:measurement_basis(orth_tr_t12, observed).
narrative_ontology:measurement(orth_tr_t24, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 24, 0.09).
narrative_ontology:measurement_basis(orth_tr_t24, observed).
narrative_ontology:measurement(orth_tr_t48, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 48, 0.11).
narrative_ontology:measurement_basis(orth_tr_t48, observed).
narrative_ontology:measurement(orth_tr_t72, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 72, 0.13).
narrative_ontology:measurement_basis(orth_tr_t72, observed).
narrative_ontology:measurement(orth_tr_t96, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 96, 0.15).
narrative_ontology:measurement_basis(orth_tr_t96, observed).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(orth_be_t0, observed).
narrative_ontology:measurement(orth_be_t12, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 12, 0.08).
narrative_ontology:measurement_basis(orth_be_t12, observed).
narrative_ontology:measurement(orth_be_t24, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 24, 0.12).
narrative_ontology:measurement_basis(orth_be_t24, observed).
narrative_ontology:measurement(orth_be_t48, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 48, 0.16).
narrative_ontology:measurement_basis(orth_be_t48, observed).
narrative_ontology:measurement(orth_be_t72, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 72, 0.19).
narrative_ontology:measurement_basis(orth_be_t72, observed).
narrative_ontology:measurement(orth_be_t96, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 96, 0.22).
narrative_ontology:measurement_basis(orth_be_t96, observed).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement_basis(orth_su_t0, observed).
narrative_ontology:measurement(orth_su_t12, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 12, 0.78).
narrative_ontology:measurement_basis(orth_su_t12, observed).
narrative_ontology:measurement(orth_su_t24, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement_basis(orth_su_t24, observed).
narrative_ontology:measurement(orth_su_t48, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 48, 0.71).
narrative_ontology:measurement_basis(orth_su_t48, observed).
narrative_ontology:measurement(orth_su_t72, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 72, 0.69).
narrative_ontology:measurement_basis(orth_su_t72, observed).
narrative_ontology:measurement(orth_su_t96, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 96, 0.68).
narrative_ontology:measurement_basis(orth_su_t96, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_legitimacy_kernel__continuity_reading, 0.08).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__instrumentalist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__modernist_reading).

% DUAL FORMULATION NOTE:
% This story is the continuity_reading member of a three-story kernel family (orthographic_legitimacy_kernel). All three readings share the 1928 Turkish alphabet reform as referent but author independent ε values: continuity_reading is low-ε and mountain-leaning (loss-of-access framing, no active extraction mechanism); instrumentalist_reading is expected mid-ε (efficiency gains vs. costs to non-adopters); modernist_reading is expected high-ε and tangled_rope/snare-leaning (rupture-as-virtue framing recasts the same victim population as intended beneficiaries of modernization, which is itself the extractive move this reading's own kernel-mates would flag). Do not average these three ε values or treat them as one constraint measured three ways — per the ε-invariance principle they are three distinct constraints linked here by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
