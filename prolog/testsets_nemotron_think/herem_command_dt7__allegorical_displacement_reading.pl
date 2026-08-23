% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__allegorical_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: herem_command_dt7__allegorical_displacement_reading
 *   human_readable: Herem Command as Allegorical Spiritual Warfare (Displacement Reading)
 *   domain: biblical_hermeneutics/religious_ethics/commitment_system
 *
 * SUMMARY:
 *   This constraint story models the allegorical displacement reading of the
 *   herem (ban/devoted-to-destruction) command in Deuteronomy and Joshua. The
 *   reading asserts that the 'nations' marked for destruction (Hittites,
 *   Amorites, Canaanites, etc.) are typological placeholders for spiritual
 *   enemies — sin, temptation, vice — and that the conquest narrative depicts
 *   internal moral warfare, not ethnic cleansing. The constraint operates as
 *   a hermeneutical rule: when you encounter herem, read it allegorically.
 *   From this reading's own lights, the constraint has near-zero
 *   extractiveness on interethnic relations (the victim set collapses to
 *   abstract vices) and functions as a spiritual mountain — a natural law of
 *   the soul's warfare. The metrics reflect this self-assessment: low
 *   extraction, low suppression, low theater. But the stakeholder surface
 *   reveals an asymmetry: literalist believers experience the reading as a
 *   constraint that extracts interpretive labor and marginalizes their
 *   conviction, while liberal theologians and spiritual practitioners
 *   benefit. The engine will compute per-seat classifications from this
 *   structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__allegorical_displacement_reading, 0.12).
domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, 0.15).
domain_priors:theater_ratio(herem_command_dt7__allegorical_displacement_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__allegorical_displacement_reading, mountain).
narrative_ontology:human_readable(herem_command_dt7__allegorical_displacement_reading, "Herem Command as Allegorical Spiritual Warfare (Displacement Reading)").
narrative_ontology:topic_domain(herem_command_dt7__allegorical_displacement_reading, "biblical_hermeneutics/religious_ethics/commitment_system").

domain_priors:emerges_naturally(herem_command_dt7__allegorical_displacement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__allegorical_displacement_reading, 'd7005494-ee1e-48a4-8cc4-f6a77418858a').
narrative_ontology:cs_kernel_codification('d7005494-ee1e-48a4-8cc4-f6a77418858a', fixed_text).
narrative_ontology:cs_authority_grounding('d7005494-ee1e-48a4-8cc4-f6a77418858a', lineage).
narrative_ontology:cs_interpretation_layer_present('d7005494-ee1e-48a4-8cc4-f6a77418858a').
narrative_ontology:cs_reading_relation('d7005494-ee1e-48a4-8cc4-f6a77418858a', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('d7005494-ee1e-48a4-8cc4-f6a77418858a', herem_command_dt7__contextual_supersession_reading, coexists_with).
narrative_ontology:cs_axiom('d7005494-ee1e-48a4-8cc4-f6a77418858a', foundational, herem_is_purely_allegorical).
narrative_ontology:cs_axiom_status(herem_is_purely_allegorical, holdable).
narrative_ontology:cs_axiom_grounding('d7005494-ee1e-48a4-8cc4-f6a77418858a', herem_is_purely_allegorical, theological).
narrative_ontology:cs_axiom('d7005494-ee1e-48a4-8cc4-f6a77418858a', foundational, conquest_is_internal_spiritual_warfare).
narrative_ontology:cs_axiom_status(conquest_is_internal_spiritual_warfare, holdable).
narrative_ontology:cs_axiom_grounding('d7005494-ee1e-48a4-8cc4-f6a77418858a', conquest_is_internal_spiritual_warfare, theological).
narrative_ontology:cs_reference_frame('d7005494-ee1e-48a4-8cc4-f6a77418858a', allegorical_spiritual_hermeneutic).
narrative_ontology:cs_drift_state('d7005494-ee1e-48a4-8cc4-f6a77418858a', modern_historical_critical_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d7005494-ee1e-48a4-8cc4-f6a77418858a', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, spiritual_practitioners).
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, liberal_theologians).
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, interfaith_dialogue_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(herem_command_dt7__allegorical_displacement_reading, literalist_believers).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, allegorical_hermeneutic).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, spiritual_warfare_metaphor).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, typological_reading_of_conquest_narratives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use the allegorical reading as a framework for daily moral formation — the 'nations' become named vices (pride, lust, greed) to be conquered through prayer, asceticism, and virtue cultivation. They gain a coherent spiritual practice without ethical cost to outsiders. Exit means adopting a different spiritual framework; no structural barrier prevents this.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, spiritual_practitioners, beneficiary,
    moderate, biographical, mobile, global).

% Produce and authorize the allegorical reading in seminaries, commentaries, and ecumenical bodies. They benefit professionally and institutionally from a hermeneutic that resolves the genocide problem while preserving scriptural authority. Their exit options are high — they can shift to historical-critical or other frameworks without losing institutional standing.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, liberal_theologians, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__allegorical_displacement_reading, liberal_theologians, beneficiary).

% Deploy the allegorical reading in multi-faith settings to neutralize texts that would otherwise appear as warrant for religious violence. They benefit from a shared hermeneutical resource that renders herem non-threatening to neighbors. Exit is trivial — they can use other interpretive strategies or withdraw from dialogue.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, interfaith_dialogue_participants, beneficiary,
    organized, biographical, mobile, global).

% Experience the allegorical reading as a constraint that marginalizes their conviction that the text means what it says. In denominations or institutions where the allegorical reading is normative, maintaining a literal hermeneutic requires interpretive labor, social friction, and sometimes institutional exit. Their identity is fused to the literal text; leaving the reading feels like leaving the faith.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, literalist_believers, payer,
    organized, generational, identity_locked, global).

% Analyze the allegorical reading as a historical phenomenon — a reception-history layer that reflects modern ethical sensibilities more than ancient authorial intent. They neither collect nor pay; they map the constraint's operation across traditions and periods.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, historical_critics, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a hermeneutical resolution to the ethical crisis posed by divinely commanded genocide texts: allows communities to retain scripture as authoritative while relocating the violence to an internal spiritual domain where it harms no ethnic other.
% TRANSFER_FUNCTION: Moves interpretive authority from the plain sense of the text (literal conquest of named peoples) to a spiritualized sense (metaphorical conquest of vices). The transfer is from literalist communities — who lose the text's surface meaning — to allegorical communities who gain ethical coherence.
% ABSENT_VOICES: Ancient Near Eastern readers for whom herem was a lived reality of warfare and theology; modern Palestinian theologians who read the conquest narratives as ongoing dispossession warrant; Jewish interpreters who read herem as historically bounded Torah law, not allegory. These voices are structurally excluded from the liberal Christian theological guilds where this reading dominates.
% DISAPPEARANCE_RATIONALE: If the allegorical reading vanished overnight, liberal Protestant and Catholic communities would lose their primary hermeneutical resource for neutralizing the genocide texts. Literalist readings would surge in those spaces, interfaith dialogue would lose a shared non-violent hermeneutic, and the ethical crisis of herem would reopen acutely. The world of modern scriptural reception would rearrange.
% FOUNDING_PROBLEM: The crisis of divine violence in scripture: how to confess a text that commands the extermination of entire peoples as the word of a good God, without either rejecting the text or endorsing genocide.
% FOUNDING_PROBLEM_CORROBORATION: The crisis is attested by the persistent production of allegorical commentaries across 1700+ years (Origen to von Balthasar), by the explicit statements of Vatican II and post-conciliar documents on scripture and violence, and by the lived testimony of believers who report the allegorical reading as the only thing keeping them in the faith. No corroboration needed from outside the beneficiary set — the crisis is empirically observable in reception history.
narrative_ontology:disappearance_verdict(herem_command_dt7__allegorical_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__allegorical_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__allegorical_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(herem_command_dt7__allegorical_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__allegorical_displacement_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__allegorical_displacement_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, ExtMetricName, E),
    domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(herem_command_dt7__allegorical_displacement_reading),
    narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(herem_command_dt7__allegorical_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.12 because the reading itself claims zero interethnic extraction and the primary cost is interpretive (literalists must work against the dominant allegorical current). Suppression at 0.15 reflects that the reading does not coerce — literalist communities persist — but institutional dominance in mainline seminaries and ecumenical bodies creates soft pressure. Theater is low (0.08) because the spiritual practice is genuine, not performative. Accessibility collapse at 0.45: literal reading remains fully accessible and vibrant in fundamentalist and confessional traditions, but its plausibility structure has eroded in academic and mainline contexts. Resistance at 0.38: literalist and historical-critical pushback is real but has not displaced the allegorical reading in its institutional strongholds.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats, this constraint is a mountain — a natural spiritual law that liberates. From the payer seat (literalist_believers), it operates as a snare — an enforced interpretive regime that extracts their hermeneutical birthright. The engine computes this divergence; the authored claim (mountain) reflects the reading's self-understanding, not the payer's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Spiritual practitioners and interfaith participants are beneficiaries (d near 0.0) — they receive a coherent, non-violent hermeneutic with no cost to others. Liberal theologians are agenda_setters who also benefit (d ~0.1) — they author and institutionalize the reading. Literalist believers are payers (d ~0.85) — they bear the interpretive and social cost of the reading's dominance while being identity-locked to their contrary conviction. Historical critics are analytical observers (d=0.5). The engine will derive directionality from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The allegorical reading solves a genuine founding problem (divine violence in scripture) that remains live. It has not atrophied into a piton — its coordination function (ethical coherence for faith communities) is active and its beneficiaries are real. But the founding problem itself is contested: historical critics argue the crisis is manufactured by reading ancient texts through modern ethics; Palestinian theologians argue the allegorical reading erases the political reality of conquest. The mandatrophy question is whether the reading still serves its founding purpose or has become a cover for avoiding historical responsibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'This constraint is one reading (allegorical_displacement_reading) of kernel herem_command_dt7. How does the structural classification change when the kernel is instantiated by the other two readings (durable_separation_reading, contextual_supersession_reading)?',
    'Generate separate constraint stories for each sibling reading with their own ε, stakeholders, and metrics. Compare the computed seat classifications across the three stories to map the kernel''s structural deformation under different readings.',
    'If durable_separation_reading computes as snare/tangled_rope with ethnic victims, and contextual_supersession_reading computes as scaffold with sunset, the kernel itself is not a single constraint but a structural generator of divergent constraints. The allegorical reading''s mountain classification would be reading-relative, not kernel-intrinsic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Commitment-system framing: one kernel, three readings, three constraint stories with divergent structural classifications.').

omega_variable(
    covert_extraction_from_literalists,
    'Does the allegorical reading covertly extract interpretive authority and communal belonging from literalist believers, despite claiming zero extractiveness?',
    'Measure the institutional and social costs borne by literalist believers in contexts where the allegorical reading is normative (denominational ordination standards, seminary curricula, ecumenical statements). If costs are substantial and systematic, the reading''s effective extraction on the payer seat is higher than its self-assessed ε.',
    'If covert extraction is confirmed, the constraint''s mountain claim fails for the literalist seat — the engine would compute a snare or tangled_rope classification for that seat, revealing a false summit structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covert_extraction_from_literalists, empirical, 'Whether the reading''s self-declared zero extractiveness holds across all seats or masks asymmetric extraction on the literalist payer seat.').

omega_variable(
    spiritual_warfare_as_extraction_mechanism,
    'Does the ''internal conquest'' metaphor function as an extraction mechanism on spiritual practitioners themselves — requiring continuous performative self-surveillance that benefits religious authorities?',
    'Ethnographic study of spiritual formation practices in communities where this reading is normative: measure time/labor devoted to ''conquering sin,'' correlation with clerical authority structures, and whether practitioners experience the metaphor as liberating or burdening.',
    'If the metaphor extracts spiritual labor for institutional benefit, the constraint''s theater ratio and extractiveness are understated even for the beneficiary seats. The mountain claim would collapse to piton or tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(spiritual_warfare_as_extraction_mechanism, empirical, 'Whether the allegorical reading''s internal domain harbors its own extraction dynamic (spiritual labor for institutional authority).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__allegorical_displacement_reading, 200, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(herem_command_dt7__allegorical_displacement_reading_tr_t200, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 200, 0.03).
narrative_ontology:measurement(herem_command_dt7__allegorical_displacement_reading_tr_t500, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 500, 0.04).
narrative_ontology:measurement(herem_command_dt7__allegorical_displacement_reading_tr_t1000, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1000, 0.05).
narrative_ontology:measurement(herem_command_dt7__allegorical_displacement_reading_tr_t1500, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1500, 0.06).
narrative_ontology:measurement(herem_command_dt7__allegorical_displacement_reading_tr_t1800, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1800, 0.07).
narrative_ontology:measurement(herem_command_dt7__allegorical_displacement_reading_tr_t1950, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(herem_command_dt7__allegorical_displacement_reading_tr_t2025, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 2025, 0.08).

% Extraction over time
narrative_ontology:measurement(herem_command_dt7__allegorical_displacement_reading_be_t200, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 200, 0.05).
narrative_ontology:measurement(herem_command_dt7__allegorical_displacement_reading_be_t500, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 500, 0.07).
narrative_ontology:measurement(herem_command_dt7__allegorical_displacement_reading_be_t1000, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1000, 0.08).
narrative_ontology:measurement(herem_command_dt7__allegorical_displacement_reading_be_t1500, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1500, 0.1).
narrative_ontology:measurement(herem_command_dt7__allegorical_displacement_reading_be_t1800, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1800, 0.11).
narrative_ontology:measurement(herem_command_dt7__allegorical_displacement_reading_be_t1950, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1950, 0.12).
narrative_ontology:measurement(herem_command_dt7__allegorical_displacement_reading_be_t2025, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 2025, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(herem_command_dt7__allegorical_displacement_reading_su_t200, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 200, 0.05).
narrative_ontology:measurement(herem_command_dt7__allegorical_displacement_reading_su_t500, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 500, 0.08).
narrative_ontology:measurement(herem_command_dt7__allegorical_displacement_reading_su_t1000, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 1000, 0.1).
narrative_ontology:measurement(herem_command_dt7__allegorical_displacement_reading_su_t1500, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 1500, 0.12).
narrative_ontology:measurement(herem_command_dt7__allegorical_displacement_reading_su_t1800, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 1800, 0.14).
narrative_ontology:measurement(herem_command_dt7__allegorical_displacement_reading_su_t1950, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 1950, 0.15).
narrative_ontology:measurement(herem_command_dt7__allegorical_displacement_reading_su_t2025, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 2025, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__allegorical_displacement_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(herem_command_dt7__allegorical_displacement_reading, 0.08).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__contextual_supersession_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories constituting the herem_command_dt7 constraint family. Each reading instantiates a different constraint with distinct ε, victim sets, and classifications. The allegorical reading relocates the constraint entirely to the internal spiritual domain (zero interethnic extraction, abstract vices as victims). The durable separation reading maintains the literal ethnic boundary (high interethnic extraction, concrete victim groups). The contextual supersession reading treats the constraint as historically bounded with a sunset (scaffold). Together they demonstrate the ε-invariance principle: the kernel label 'herem' covers structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(herem_command_dt7__allegorical_displacement_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
