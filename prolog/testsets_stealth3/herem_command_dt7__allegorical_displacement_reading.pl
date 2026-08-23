% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__allegorical_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Herem Command (Deut 7) — Allegorical Displacement Reading: Interior Moral Warfare Template
 *   domain: religious/hermeneutical/commitment-system
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the herem kernel (Deuteronomy 7):
 *   the allegorical displacement reading, in which the command's nations are
 *   typological placeholders for spiritual enemies and its conquest is
 *   interior moral warfare. The standing arrangement under contest, and
 *   therefore the epsilon referent, is the allegorical discipline itself as
 *   this reading assesses it: communities teach the typological key,
 *   practitioners wage the commanded warfare against personified vices, and
 *   the teaching office transmits the mapping. Assessed by the reading's own
 *   lights, no ethnic or religious group is targeted, the victim set
 *   collapses to abstract vices, and the violence is realized as metaphorical
 *   self-discipline; the reading's endorsed alternative arrangements are NOT
 *   the referent. Per the epsilon-invariance principle, the sibling readings
 *   of the same kernel are separate constraint files with their own epsilon,
 *   beneficiaries, and victims; committer structure is routed to the omega
 *   variables below, not folded into this classification. KEY AGENTS (by
 *   structural relationship): - practicing_believers: Primary
 *   participant-beneficiary (moderate/constrained) — adopts the discipline,
 *   bears its ascetic demands, receives its formative benefit -
 *   typological_catechists: Agenda-setting interpreter
 *   (institutional/constrained) — formulates and transmits the key, collects
 *   interpretive authority - typified_vices: Nominal target, NON-AGENT
 *   (agent=false) — the abstract enemy set onto which the command's force is
 *   displaced; excluded from derivation - nations_named_in_herem_texts:
 *   Excluded party (powerless/trapped) — the historical populations the text
 *   names; displaced as referents, absent from the conversation -
 *   reception_historians: Analytical observer (institutional/analytical) —
 *   documents the reading's emergence and effects from outside the devotional
 *   seat
 *
 * KEY AGENTS:
 *   - practicing_believers: Primary participant-beneficiary (moderate/constrained) — bears the discipline's labor, receives its formative benefit
 *   - typological_catechists: Agenda-setting interpreter (institutional/constrained) — administers the typological key, collects interpretive authority and status
 *   - typified_vices: Non-agent nominal target (agent=false) — abstract vices personified as the seven nations; the collapsed victim set, excluded from derivation
 *   - nations_named_in_herem_texts: Excluded party (powerless/trapped) — the text's historical referents, displaced and seatless in the interpretive tradition
 *   - reception_historians: Analytical observer (institutional/analytical) — studies the reading's genealogy and effects without a devotional stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__allegorical_displacement_reading, 0.12).
domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, 0.22).
domain_priors:theater_ratio(herem_command_dt7__allegorical_displacement_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__allegorical_displacement_reading, rope).
narrative_ontology:human_readable(herem_command_dt7__allegorical_displacement_reading, "Herem Command (Deut 7) — Allegorical Displacement Reading: Interior Moral Warfare Template").
narrative_ontology:topic_domain(herem_command_dt7__allegorical_displacement_reading, "religious/hermeneutical/commitment-system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__allegorical_displacement_reading, 'dfbe8d81-599b-422a-98d5-91cd7eebfa0b').
narrative_ontology:cs_kernel_codification('dfbe8d81-599b-422a-98d5-91cd7eebfa0b', fixed_text).
narrative_ontology:cs_authority_grounding('dfbe8d81-599b-422a-98d5-91cd7eebfa0b', lineage).
narrative_ontology:cs_interpretation_layer_present('dfbe8d81-599b-422a-98d5-91cd7eebfa0b').
narrative_ontology:cs_reading_relation('dfbe8d81-599b-422a-98d5-91cd7eebfa0b', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('dfbe8d81-599b-422a-98d5-91cd7eebfa0b', herem_command_dt7__contextual_supersession_reading, influences).
narrative_ontology:cs_axiom('dfbe8d81-599b-422a-98d5-91cd7eebfa0b', foundational, herem_nations_are_typological_not_ethnic).
narrative_ontology:cs_axiom_status(herem_nations_are_typological_not_ethnic, holdable).
narrative_ontology:cs_axiom_grounding('dfbe8d81-599b-422a-98d5-91cd7eebfa0b', herem_nations_are_typological_not_ethnic, theological).
narrative_ontology:cs_axiom('dfbe8d81-599b-422a-98d5-91cd7eebfa0b', foundational, conquest_fulfilled_only_as_interior_warfare).
narrative_ontology:cs_axiom_status(conquest_fulfilled_only_as_interior_warfare, holdable).
narrative_ontology:cs_axiom_grounding('dfbe8d81-599b-422a-98d5-91cd7eebfa0b', conquest_fulfilled_only_as_interior_warfare, instrumental).
narrative_ontology:cs_reference_frame('dfbe8d81-599b-422a-98d5-91cd7eebfa0b', typological_interior_warfare_template).
narrative_ontology:cs_drift_state('dfbe8d81-599b-422a-98d5-91cd7eebfa0b', contemporary_hermeneutical_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dfbe8d81-599b-422a-98d5-91cd7eebfa0b', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, practicing_believers).
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, typological_catechists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(herem_command_dt7__allegorical_displacement_reading, practicing_believers).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, typological_hermeneutic_principle).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, interior_warfare_sanctification_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopt the typological discipline their communities teach: they rehearse the mapping of the seven named nations onto pride, covetousness, deception, and idolatrous attachment, and wage the commanded warfare against these in self-examination, fasting, confession, and renunciation. What flows to them is moral formation and a settled conscience about a difficult canonical text; what flows from them is the continuous labor of the discipline itself. Leaving the frame is possible, since other devotional approaches to the same canon exist, but it means surrendering a practice their formation, community life, and liturgical year are built around.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, practicing_believers, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__allegorical_displacement_reading, practicing_believers, payer).

% Preachers, catechists, and spiritual directors who formulate, transmit, and police the typological key: they decide how each element of the command (the named nations, the devoted things, the demolished altars) maps onto interior reality, train successors in the mapping, and correct deviation in pulpit and classroom. The reading supplies the teaching office with a usable text and concentrates interpretive authority in their hands; abandoning the key would unsettle curricula, lectionaries, and their own standing, so they maintain it across generations.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, typological_catechists, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__allegorical_displacement_reading, typological_catechists, beneficiary).

% Within this reading, the entities the command targets are not peoples but dispositions: pride, covetousness, fear, and idolatrous attachment, personified as the seven nations. They are marked for destruction without mercy and granted no treaty, but they are abstractions: they bear no real cost, collect nothing, and exist only as the governed content of the practitioner's interior life. Listed for completeness of the reading's own frame; excluded from all structural derivation.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, typified_vices, payer,
    powerless, generational, trapped, universal).
narrative_ontology:stakeholder_non_agent(herem_command_dt7__allegorical_displacement_reading, typified_vices).

% The historical populations the text names as objects of annihilation. Under this reading they are displaced as referents, since the command is held never to have been about them, but they are equally absent from the conversation that displaces them: no descendant or memory-community sits in the interpretive tradition, and the determination that the text's violence is metaphor is made entirely without them. They cannot exit the canon that names them.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, nations_named_in_herem_texts, excluded,
    powerless, generational, trapped, regional).

% Scholars of reception history and hermeneutics who study when and how the allegorical displacement emerged, what usability problem it solved, and what it preserved or deferred. They take no devotional seat, collect no benefit from the discipline, and document its operation from outside the tradition.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, reception_historians, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__allegorical_displacement_reading, typological_catechists).
narrative_ontology:fixing_cost_class(herem_command_dt7__allegorical_displacement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives dispersed canon-holding communities a single shared interpretive key that renders a settlement-era annihilation command teachable: the text's nations, cities, and spoils are mapped to interior dispositions, so congregations can retain the passage in lectionary and catechesis without practicing or endorsing its literal content. It solves the collective problem of maintaining scriptural continuity across generations without literal application.
% TRANSFER_FUNCTION: Moves interpretive labor and ascetic effort from practitioners into their own moral formation (largely self-directed, returning to the self); moves interpretive authority, pulpit credibility, and teaching status to the catechists who master and transmit the typological key; moves attention away from the text's historical referents toward its reusable pattern.
% ABSENT_VOICES: Lay practitioners receive the typology but do not participate in formulating it. Historians and archaeologists of the ancient Levant, whose evidence about the commands' original setting sits outside the devotional conversation, are not consulted. The descendant and memory-communities of the peoples the text names have no seat in the tradition that redirects their ancestors' story into metaphor.
% DISAPPEARANCE_RATIONALE: Lectionaries, catechetical curricula, homiletic handbooks, and spiritual-direction practice are built on the typological template. Overnight removal would force canon-holding communities to shelve the passage, restore literal application, or invent a replacement hermeneutic; the devotional economy around the text would reorganize within a generation.
% FOUNDING_PROBLEM: A canon-defining text commands the annihilation of named nations; communities that retain the canon as authoritative scripture needed a way to keep the text in circulation without practicing, endorsing, or being scandalized by its literal content.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: academic reception history documents the usability motive operating independently across Alexandrian Jewish allegory, patristic typology, and rabbinic midrash, and the problem visibly recurs whenever literalist readings resurface. Scholars with no devotional stake in the allegorical solution attest both the problem and its persistence.
narrative_ontology:disappearance_verdict(herem_command_dt7__allegorical_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__allegorical_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__allegorical_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(herem_command_dt7__allegorical_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__allegorical_displacement_reading, 0.12, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is low (0.12) because no human class bears a net transfer: the discipline's demands fall on the practitioner and return to the practitioner as formation, and the only accruing flow is the modest status-and-authority stream to the teaching office. Suppression (0.22) is a raw structural property, unscaled by power or scope: the discipline's severity ('no mercy, no treaty') is directed at the self, and no external coercion machinery belongs to this constraint as such. Theater ratio (0.28) reflects a real formative function carrying a rhetorical overlay: the militant vocabulary in preaching often exceeds operative practice, especially in the modern period. Accessibility collapse is low (0.25) because mastering the typological key closes no alternative: critical scholarship, other devotional methods, and non-allegorical engagement with the text remain open. Resistance (0.20) is modest: the discipline meets occasional objection that interiorization evades the text's difficulty, but practitioners adopt it voluntarily and it defends itself by usefulness rather than force. Claimed type is rope, authored independently of the metrics: the arrangement solves a genuine collective problem (keeping a canonical text usable and teachable) with minimal coercive overhead, participants are net beneficiaries by the reading's own account, and no alternative is suppressed by the constraint itself. The measurement series run on one shared time grid (calendar years CE: 240, 600, 1000, 1500, 1900, 2025) with every tracked metric authored at every point. The series show a low, gently humped extraction curve peaking in the scholastic-medieval consolidation (when interpretive status-rents were largest), a theater ratio climbing through the medieval period as homiletic rhetoric inflated and partially receding under modern pastoral and historical consciousness, and a mild enforcement-intensity arc tracing the institutional consolidation and later relaxation of the allegorical requirement.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the practitioner seat the arrangement is formation: a demanding but self-owned discipline that returns what it takes. From the catechist seat it is stewardship plus a concentrated authority stream: the key is the teaching office's principal asset for handling the hardest text in the lectionary. From the excluded historical-memory seat the same structure appears as displacement: the text's recorded violence is redirected into metaphor by a conversation in which the named peoples have never had a voice. The engine computes this per-seat divergence from the structural data; the authored rope claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (practicing_believers, typological_catechists) derive low directionality, damping effective extraction toward subsidy for the seats the arrangement serves. No victims are declared among real agents, so no seat derives an amplified target directionality; this absence IS the reading's core structural claim, not an omission. The typified vices are authored as a non-agent payer (agent=false) to make the collapsed victim set visible while ensuring they feed no directionality or chi computation, exactly as the non-agent registry requires. The nations named in the text enter only as an excluded stakeholder, which is commentary-grade and drives no classification override.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading-indexed epsilon prevents misclassification in both directions. Scored against the text's literal history, the herem arrangement would register as violently extractive; scored against the allegorical discipline as this reading holds it, extraction is near-zero on interethnic relations. Neither number describes the other's constraint. The mandatrophy question resolves cleanly here: the founding problem (canon usability) is live, corroborated from outside the beneficiary set, so the arrangement persists because its problem persists rather than by inertia, and the theater ratio (0.28) sits well below the degraded-performance range. The mismatch consumer finds status=live paired with verdict=world_rearranges: no zombie flag. The open risk is not obsolescence but latency, carried in the omega variables: whether the displaced template stays permanently abstract or remains available for re-targeting under stress.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_provenance,
    'This constraint is one reading of kernel herem_command_dt7 (the allegorical_displacement_reading); which reading a community adopts determines the entire structural profile — victim set, extraction, enforcement — so how is adoption distributed and stabilized?',
    'Survey catechetical materials, preaching corpora, and denominational teaching statements to classify communities by operative reading; adoption is observable in how each tradition handles the nation lists and the destruction vocabulary.',
    'Switching readings swaps the constraint wholesale: this file''s near-zero interethnic extraction and abstract victim set hold only under the allegorical reading; the durable-separation and contextual-supersession files carry materially different profiles for the same kernel text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_provenance, conceptual, 'Committer-frame provenance: one kernel, three readings, three constraints; this story authors only the allegorical displacement instance.').

omega_variable(
    allegory_stability_vs_latent_template,
    'Is the displacement stable — the victim set permanently abstract — or does the allegorical frame preserve the destruction template in dormant form, available for re-literalization against real groups under stress?',
    'Reception-history analysis of episodes in which communities formed by the allegorical reading re-applied herem categories to real populations (crusade preaching, colonial applications, sectarian conflicts): did the typological frame inhibit transfer, or supply ready-made categories for it?',
    'If the template is latent, effective extraction is higher than authored and the constraint drifts toward tangled_rope or snare behavior under mobilization; if stable, the near-zero profile holds across conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allegory_stability_vs_latent_template, empirical, 'Whether interiorization neutralizes the text''s violent potential or banks it.').

omega_variable(
    displacement_completeness,
    'Is the displacement total — every element of the command (nations, devoted things, treaties, altars) mapped to interior reality — or partial, with some elements retaining literal behavioral residue?',
    'Systematic survey of how each Deuteronomy 7 element is handled across the allegorical exegetical tradition: which components are consistently spiritualized and which are ever given practical observance.',
    'Partial displacement leaves small real-world behavioral constraints (for example, relational separation practices) with nonzero extraction and a thinner victim set than pure abstraction; total displacement supports the authored near-zero profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_completeness, conceptual, 'Whether the reading''s relocation of the command to the interior life is complete or leaves literal residue.').

omega_variable(
    ascetic_severity_scrupulosity_ambiguity,
    'Is the discipline''s demanded severity — merciless interior warfare, no treaty with vice — formative for practitioners, or does it corrode into scrupulosity, shame, and compulsive self-monitoring?',
    'Pastoral and psychological study of practitioners formed under the discipline: rates of scrupulosity, spiritual distress, and abandonment compared against practitioners of gentler formative frameworks within the same traditions.',
    'If corrosive, the discipline''s true cost to its participants exceeds the beneficiary framing, the practitioners'' secondary payer position weighs heavier, and the net-benefit premise underlying the rope claim weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ascetic_severity_scrupulosity_ambiguity, empirical, 'Whether the internalized severity of the discipline is benign formation or a hidden cost borne by its own beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__allegorical_displacement_reading, 240, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(herem_alleg_disp_tr_t240, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 240, 0.15).
narrative_ontology:measurement(herem_alleg_disp_tr_t600, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 600, 0.2).
narrative_ontology:measurement(herem_alleg_disp_tr_t1000, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1000, 0.26).
narrative_ontology:measurement(herem_alleg_disp_tr_t1500, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1500, 0.31).
narrative_ontology:measurement(herem_alleg_disp_tr_t1900, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1900, 0.33).
narrative_ontology:measurement(herem_alleg_disp_tr_t2025, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(herem_alleg_disp_be_t240, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 240, 0.08).
narrative_ontology:measurement(herem_alleg_disp_be_t600, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 600, 0.1).
narrative_ontology:measurement(herem_alleg_disp_be_t1000, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1000, 0.12).
narrative_ontology:measurement(herem_alleg_disp_be_t1500, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1500, 0.14).
narrative_ontology:measurement(herem_alleg_disp_be_t1900, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1900, 0.11).
narrative_ontology:measurement(herem_alleg_disp_be_t2025, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 2025, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(herem_alleg_disp_su_t240, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 240, 0.16).
narrative_ontology:measurement(herem_alleg_disp_su_t600, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 600, 0.19).
narrative_ontology:measurement(herem_alleg_disp_su_t1000, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 1000, 0.24).
narrative_ontology:measurement(herem_alleg_disp_su_t1500, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 1500, 0.28).
narrative_ontology:measurement(herem_alleg_disp_su_t1900, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 1900, 0.23).
narrative_ontology:measurement(herem_alleg_disp_su_t2025, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 2025, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__allegorical_displacement_reading, information_standard).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, contextual_supersession_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the herem command' conflates three structurally distinct constraints sharing one kernel text (Deuteronomy 7). This file instantiates the allegorical-displacement reading alone: epsilon is authored for the interior-warfare discipline as this reading assesses it (near-zero interethnic extraction, victim set collapsed to abstract vices). The durable-separation reading (timeless ethnic-bounded-membership mandate; high extraction from designated outsiders) and the contextual-supersession reading (historically bounded directive; near-zero present extraction via expiry) are separate stories with their own epsilon, beneficiaries, and victims. Family members are linked via affects_constraints. Epsilon differs across the family because the referent of 'the nations' differs across readings, not because one constraint is measured under different observables; each member is separately epsilon-invariant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
