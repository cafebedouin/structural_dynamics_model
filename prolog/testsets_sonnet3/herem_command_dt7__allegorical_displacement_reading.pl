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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: herem_command_dt7__allegorical_displacement_reading
 *   human_readable: Allegorical-Displacement Reading of the Deuteronomy 7 Herem Command
 *   domain: religious_ethics/hermeneutics
 *
 * SUMMARY:
 *   This story instantiates the allegorical-displacement reading of the
 *   Deuteronomy 7 herem command, one of three contested readings of a single
 *   kernel (the herem command itself). Under this reading, the 'seven
 *   nations' named for destruction are typological figures for internal
 *   spiritual enemies — sin, temptation, disordered appetite — and the
 *   command to 'utterly destroy' them is a metaphor for rigorous internal
 *   moral discipline, not a directive concerning any real ethnic population.
 *   This reading, traceable to patristic allegorists and persisting in
 *   various devotional and homiletical traditions, structurally relocates the
 *   entire extractive weight of the command away from interethnic relations
 *   and onto the individual believer's inner life. Per the ε-invariance
 *   principle, this is authored as a wholly separate constraint from the
 *   durable_separation_reading (which reads the nations as real, and the
 *   separation mandate as timeless and binding) and the
 *   contextual_supersession_reading (which reads the command as a real
 *   historical directive later morally superseded). Each reading has its own
 *   ε, its own beneficiary/victim structure, and its own classification; they
 *   are linked only via the shared kernel_id in omega variables and
 *   cs_structure, never merged into one constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__allegorical_displacement_reading, 0.08).
domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, 0.35).
domain_priors:theater_ratio(herem_command_dt7__allegorical_displacement_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__allegorical_displacement_reading, rope).
narrative_ontology:human_readable(herem_command_dt7__allegorical_displacement_reading, "Allegorical-Displacement Reading of the Deuteronomy 7 Herem Command").
narrative_ontology:topic_domain(herem_command_dt7__allegorical_displacement_reading, "religious_ethics/hermeneutics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__allegorical_displacement_reading, '0428a2bd-9100-44b0-9ffa-1c56fd6cff09').
narrative_ontology:cs_kernel_codification('0428a2bd-9100-44b0-9ffa-1c56fd6cff09', fixed_text).
narrative_ontology:cs_authority_grounding('0428a2bd-9100-44b0-9ffa-1c56fd6cff09', lineage).
narrative_ontology:cs_interpretation_layer_present('0428a2bd-9100-44b0-9ffa-1c56fd6cff09').
narrative_ontology:cs_reading_relation('0428a2bd-9100-44b0-9ffa-1c56fd6cff09', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('0428a2bd-9100-44b0-9ffa-1c56fd6cff09', herem_command_dt7__contextual_supersession_reading, coexists_with).
narrative_ontology:cs_axiom('0428a2bd-9100-44b0-9ffa-1c56fd6cff09', foundational, conquest_narrative_is_non_literal_typology).
narrative_ontology:cs_axiom_status(conquest_narrative_is_non_literal_typology, holdable).
narrative_ontology:cs_axiom_grounding('0428a2bd-9100-44b0-9ffa-1c56fd6cff09', conquest_narrative_is_non_literal_typology, conventional).
narrative_ontology:cs_axiom('0428a2bd-9100-44b0-9ffa-1c56fd6cff09', foundational, nations_signify_internal_spiritual_enemies_not_ethnic_groups).
narrative_ontology:cs_axiom_status(nations_signify_internal_spiritual_enemies_not_ethnic_groups, holdable).
narrative_ontology:cs_axiom_grounding('0428a2bd-9100-44b0-9ffa-1c56fd6cff09', nations_signify_internal_spiritual_enemies_not_ethnic_groups, conventional).
narrative_ontology:cs_reference_frame('0428a2bd-9100-44b0-9ffa-1c56fd6cff09', patristic_typological_hermeneutic).
narrative_ontology:cs_drift_state('0428a2bd-9100-44b0-9ffa-1c56fd6cff09', contemporary_devotional_use, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('0428a2bd-9100-44b0-9ffa-1c56fd6cff09', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, practicing_adherents_pursuing_moral_formation).
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, allegorical_interpretive_tradition).
narrative_ontology:constraint_victim(herem_command_dt7__allegorical_displacement_reading, abstracted_vices_and_temptations).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, typological_reading_of_conquest_narratives).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, non_literal_hermeneutic_of_violence_texts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals within traditions that adopt this reading use the herem narrative as a spiritual formation resource: 'conquering the nations' becomes a template for disciplining internal vices like pride, greed, or lust. They receive a moral-training framework without any call to real-world violence or ethnic exclusion, and can freely adopt, modify, or set aside the allegorical frame without institutional cost.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, practicing_adherents_pursuing_moral_formation, beneficiary,
    moderate, biographical, mobile, national).

% Teachers, exegetes, and denominational bodies (in the lineage of Origen, Gregory of Nyssa, and later spiritualizing commentators) who articulate and transmit the typological reading. They administer which texts get allegorized and how, and benefit by resolving an otherwise morally troubling text into a usable pastoral tool, without needing enforcement machinery since adoption is voluntary and interpretive.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, allegorical_interpretive_tradition, agenda_setter,
    institutional, generational, mobile, global).

% The 'Canaanite nations' under this reading are not real ethnic populations but personifications of sin, temptation, and disordered desire, which the believer is enjoined to 'utterly destroy' within themselves. Listed for structural completeness only: this is a non-agent entity, so it is excluded from directionality and beneficiary/victim derivation despite occupying the payer role in the metaphor.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, abstracted_vices_and_temptations, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_non_agent(herem_command_dt7__allegorical_displacement_reading, abstracted_vices_and_temptations).

% Communities and scholars who read the herem texts as describing real historical peoples and real historical violence (whether endorsing, condemning, or historicizing it) are largely absent from the allegorical reading's internal discourse. They would object that the allegorical move erases the text's historical victims and its genuine ethical difficulty by relocating it entirely into metaphor, but this reading's interpretive community does not typically engage their objection on its own terms.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, literalist_and_ethnic_reading_communities, excluded,
    organized, generational, constrained, national).

% Historians and textual critics who examine the archaeological and comparative evidence for whether Canaanite peoples existed as described, whether herem was practiced, invented, or exaggerated, and how ancient Near Eastern conquest rhetoric functioned. They can adjudicate the text's original referent without being invested in either the allegorical or literalist reading's pastoral stakes.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, biblical_scholars_of_ancient_near_eastern_conquest, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared interpretive resource that lets communities retain a canonical, authoritative text containing otherwise morally troubling commands (total destruction of named peoples) by relocating its referent to an internal, non-violent domain of spiritual struggle, enabling continued liturgical and devotional use without endorsing ethnic violence.
% TRANSFER_FUNCTION: Moves interpretive authority and moral weight from the text's plain historical referent (named ethnic groups slated for destruction) to abstract internal referents (sin, temptation, disordered desire), effectively transferring the passage's ethical cost away from any real population and onto the individual believer's self-discipline.
% ABSENT_VOICES: Historical-critical scholars documenting the plausible historical violence or its absence, and descendant or comparably-positioned communities who might object to having their textual erasure treated as a hermeneutical convenience, are largely outside the interpretive tradition's internal conversation; the allegorical reading proceeds without needing to adjudicate their claims.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, adherent communities that rely on it would lose a way to use the herem texts devotionally without confronting their violent plain sense; some would shift to contextual-supersession or purely historical readings, others might drift toward discomfort with the text or renewed literalism. Whether 'the world rearranges' depends on which alternative reading fills the vacuum, hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: The plain historical sense of Deuteronomy 7's herem command (total destruction of named Canaanite nations) is morally troubling to later readers who wish to retain the text's canonical authority; the allegorical move was built to preserve devotional/pastoral use of the passage while dissolving its violent ethnic referent.
% FOUNDING_PROBLEM_CORROBORATION: Patristic-era interpreters (Origen, Gregory of Nyssa) attest the founding motive directly in their own commentaries, and this corroboration comes from within the allegorizing tradition itself; outside corroboration comes from historical-critical scholars who independently confirm that the plain-sense reading was and remains morally troubling to many readers, which is the problem the allegorical move responds to — though those scholars do not necessarily endorse the allegorical solution as historically warranted.
narrative_ontology:disappearance_verdict(herem_command_dt7__allegorical_displacement_reading, contested).
narrative_ontology:founding_problem_status(herem_command_dt7__allegorical_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__allegorical_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored near-zero (0.08) because, under this reading's own lights, no real population bears any cost — the 'victims' are abstracted vices, a non-agent entity excluded from directionality computation. Suppression is moderate (0.35) because the reading does still exercise interpretive pressure: communities that adopt it are implicitly discouraged from taking the plain historical sense seriously, and the allegorical move can suppress engagement with the text's actual moral difficulty. Theater ratio is low and rises only slightly over the long interval (0.10 to 0.15 across roughly 1800 years of reception history), reflecting that the interpretive tradition is not primarily performative — it does real devotional and pedagogical work — though some drift toward formulaic allegorization is visible in later homiletical use. Accessibility collapse is moderate (0.3): once a community fully adopts the allegorical frame, the plain historical reading becomes harder to recover as a live option within that community's discourse, but it is not foreclosed system-wide since rival readings persist elsewhere.
 *
 * DIRECTIONALITY LOGIC:
 *   The interpretive tradition and its practicing adherents are the structural beneficiaries: they retain a usable canonical text and gain a moral-formation resource, at essentially no ethnic or interpersonal cost. Because the reading's own referent-shift removes any real human victim, the derived directionality for actual persons in this reading is uniformly low — there is no real target population to extract from. The 'payer' entry (abstracted vices) is marked as a non-agent specifically so it does not feed the beneficiary/victim engine machinery as if it were a real party; its role is structural bookkeeping only, preserving the schema's tangled-rope-adjacent shape without falsely implying a genuine victim exists.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a canonical text containing morally troubling ethnic-violence commands) remains live in the sense that the plain text still exists and still requires some interpretive strategy; the allegorical reading does not let the mandate go obsolete so much as permanently redirect it. This forecloses a certain kind of mandatrophy — the reading does not persist as dead ritual after its function disappears, because its function (moral formation via the conquest narrative) continues to be actively used. The risk this classification guards against is treating the allegorical move as a benign, victimless coordination device (a rope) while ignoring the omega-documented worry that the erasure of the text's plain historical referent is itself a form of quiet extraction from historical memory and from communities who would read the text differently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    referent_displacement_legitimacy,
    'Does the allegorical reading recover the text''s deeper intended sense, or does it evacuate the text''s plain historical claim to avoid confronting a genuinely troubling command?',
    'Comparative analysis of the text''s original ancient Near Eastern genre conventions (whether conquest narratives of this type were understood literally or hyperbolically by their first audiences) against the patristic allegorists'' stated hermeneutical warrants; also, examination of whether the allegorical move arose primarily from theological necessity (post-supersessionist discomfort) or from independent exegetical method.',
    'If the allegorical move is judged an ad hoc evasion of the text''s historical claim, this reading functions partly as a face-saving reinterpretation that a durable_separation or contextual_supersession reading would call intellectually dishonest; if judged a legitimate typological hermeneutic consistent with the text''s broader canonical placement, the near-zero extractiveness holds cleanly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(referent_displacement_legitimacy, conceptual, 'Whether allegorization is genuine typology or motivated erasure of a historical claim.').

omega_variable(
    kernel_reading_selection_mechanism,
    'What determines which of the three readings (allegorical_displacement, contextual_supersession, durable_separation) a given interpretive community adopts, and is that selection itself tracking anything other than the community''s antecedent theological commitments?',
    'Historical-sociological study of which communities adopt which reading and why, cross-referenced against those communities'' independent doctrinal commitments (e.g., allegorical readings cluster in traditions already committed to spiritualizing hermeneutics generally, not uniquely for this text).',
    'If reading-selection is fully explained by prior doctrinal commitment rather than by textual or historical evidence, all three kernel readings are better modeled as downstream expressions of separate theological systems rather than as competing empirical claims about one text — which would strengthen the case for treating them as genuinely disjoint constraints rather than resolvable rivals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, conceptual, 'Whether reading selection across the kernel tracks evidence or antecedent doctrine.').

omega_variable(
    erased_victim_memory_cost,
    'Does the allegorical reading''s relocation of the text''s referent impose a real, if diffuse, cost on historical memory and on communities analogous to the displaced ''nations,'' even though no present-day ethnic group is the text''s literal referent?',
    'Track whether communities using this reading engage at all with archaeological/historical scholarship on Canaanite peoples and the ethics of conquest narratives, versus whether the allegorical move functions to foreclose that engagement entirely.',
    'If the allegorical reading functions to foreclose historical-ethical engagement with the conquest narrative''s plain sense, the extractiveness authored here (0.08) may understate a diffuse harm to historical memory and to the moral seriousness with which the text''s violence is treated, even though no living victim group is being extracted from.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(erased_victim_memory_cost, empirical, 'Whether allegorization forecloses moral reckoning with the text''s historical violence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__allegorical_displacement_reading, 0, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(here_tr_t300, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 300, 0.11).
narrative_ontology:measurement(here_tr_t600, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 600, 0.12).
narrative_ontology:measurement(here_tr_t900, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 900, 0.13).
narrative_ontology:measurement(here_tr_t1200, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1200, 0.14).
narrative_ontology:measurement(here_tr_t1500, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1500, 0.15).
narrative_ontology:measurement(here_tr_t1800, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1800, 0.15).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(here_be_t300, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 300, 0.06).
narrative_ontology:measurement(here_be_t600, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 600, 0.06).
narrative_ontology:measurement(here_be_t900, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 900, 0.07).
narrative_ontology:measurement(here_be_t1200, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1200, 0.07).
narrative_ontology:measurement(here_be_t1500, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1500, 0.08).
narrative_ontology:measurement(here_be_t1800, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1800, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(herem_command_dt7__allegorical_displacement_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__contextual_supersession_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the herem_command_dt7 kernel. herem_command_dt7__durable_separation_reading treats the nations as real and the separation mandate as timeless; herem_command_dt7__contextual_supersession_reading treats the nations as real and the command as historically bounded and superseded; this story (allegorical_displacement_reading) treats the nations as non-literal typological figures, collapsing the victim set to abstract vices and driving extractiveness on interethnic relations to near-zero. All three share the same source text and kernel_id but diverge completely on referent, victim structure, and classification, and must not be merged into a single story or averaged across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
