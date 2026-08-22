% ============================================================================
% CONSTRAINT STORY: anthropological_record__creationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__creationist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: anthropological_record__creationist_reading
 *   human_readable: Divine-Creation Reading of the Anthropological Record
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   Within religious communities that adopt it, this reading disciplines how
 *   the archaeological, paleontological, and genetic record may be
 *   interpreted: the record is held to reveal divine creative acts compatible
 *   with a scriptural timeline or with designed complexity, and accounts
 *   excluding divine causation are ruled out before the evidence is weighed.
 *   The arrangement under contest — the epsilon referent — is this
 *   interpretive discipline as it actually operates in subscribing
 *   communities, not the naturalist alternative its holders reject; the
 *   metric values are authored analytically over that fixed referent, while
 *   the holder seat's own assessment (fidelity to revelation, negligible
 *   extraction) is recorded as the perspectival divergence the engine
 *   computes per seat. Claim and metrics are independent authored facts: the
 *   claimed type states my structural judgment; the metrics state the
 *   arrangement's observed operation. This file is one of three readings of
 *   the anthropological_record kernel; the siblings are separate constraints
 *   linked in network.affects_constraints. KEY AGENTS (by structural
 *   relationship): - religious_authority_structures: Agenda-setting
 *   beneficiary (institutional/identity_locked) — defines faithful reading,
 *   collects deference, enrollment, and giving -
 *   creationist_apologetics_ministries: Secondary beneficiary with
 *   enforcement duties (organized/identity_locked) — funded by the defense
 *   remaining necessary - believing_community_members: Dual-positioned
 *   rank-and-file (moderate/constrained) — receive meaning and belonging,
 *   absorb collision costs - questioning_youth_in_creationist_schools:
 *   Primary payer (powerless/trapped) — bear the sharpest costs of the
 *   reading's demands - credentialed_scientists_within_tradition: Displaced
 *   adjudicators (powerful/constrained) — qualified to assess the record,
 *   stripped of standing inside the tradition - parochial_science_educators:
 *   Compromised intermediaries (moderate/constrained) — transmit the reading
 *   under employment pressure - secular_scientific_establishment: Excluded
 *   rival authority (institutional/mobile) — denied adjudicative standing in
 *   subscribing communities - philosophy_of_religion_scholars: Analytical
 *   observer (analytical/analytical) — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__creationist_reading, 0.65).
domain_priors:suppression_score(anthropological_record__creationist_reading, 0.55).
domain_priors:theater_ratio(anthropological_record__creationist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__creationist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__creationist_reading, "Divine-Creation Reading of the Anthropological Record").
narrative_ontology:topic_domain(anthropological_record__creationist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__creationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__creationist_reading, 'b810f6c0-82b9-45ed-8e19-b3ca92bcd3c8').
narrative_ontology:cs_kernel_codification('b810f6c0-82b9-45ed-8e19-b3ca92bcd3c8', fixed_text).
narrative_ontology:cs_authority_grounding('b810f6c0-82b9-45ed-8e19-b3ca92bcd3c8', lineage).
narrative_ontology:cs_interpretation_layer_present('b810f6c0-82b9-45ed-8e19-b3ca92bcd3c8').
narrative_ontology:cs_reading_relation('b810f6c0-82b9-45ed-8e19-b3ca92bcd3c8', anthropological_record__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('b810f6c0-82b9-45ed-8e19-b3ca92bcd3c8', anthropological_record__indigenous_epistemology_reading, coexists_with).
narrative_ontology:cs_axiom('b810f6c0-82b9-45ed-8e19-b3ca92bcd3c8', foundational, special_creation_events_in_record).
narrative_ontology:cs_axiom_status(special_creation_events_in_record, holdable).
narrative_ontology:cs_axiom_grounding('b810f6c0-82b9-45ed-8e19-b3ca92bcd3c8', special_creation_events_in_record, theological).
narrative_ontology:cs_axiom('b810f6c0-82b9-45ed-8e19-b3ca92bcd3c8', secondary, empirical_adjudication_subordinate_to_revelation).
narrative_ontology:cs_axiom_status(empirical_adjudication_subordinate_to_revelation, holdable).
narrative_ontology:cs_axiom_grounding('b810f6c0-82b9-45ed-8e19-b3ca92bcd3c8', empirical_adjudication_subordinate_to_revelation, theological).
narrative_ontology:cs_reference_frame('b810f6c0-82b9-45ed-8e19-b3ca92bcd3c8', record_as_divine_testimony).
narrative_ontology:cs_drift_state('b810f6c0-82b9-45ed-8e19-b3ca92bcd3c8', post_kitzmiller_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b810f6c0-82b9-45ed-8e19-b3ca92bcd3c8', '').
narrative_ontology:cs_kernel_id(anthropological_record__creationist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, religious_authority_structures).
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, creationist_apologetics_ministries).
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, believing_community_members).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, questioning_youth_in_creationist_schools).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, credentialed_scientists_within_tradition).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, parochial_science_educators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, believing_community_members).
narrative_ontology:constraint_vindicates(anthropological_record__creationist_reading, biblical_infallibility_doctrine).
narrative_ontology:constraint_vindicates(anthropological_record__creationist_reading, designed_complexity_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Denominational boards, seminaries, and parochial school systems that define what counts as faithful reading of the record. They write curricula, credential clergy, publish position statements, and correct deviation. The reading's continuance keeps their interpretive office indispensable: deference, enrollment, and giving flow toward the offices that guard it. Stepping back from the reading would mean surrendering the claim that grounds their authority, so the option functions as self-dissolution.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, religious_authority_structures, agenda_setter,
    institutional, generational, identity_locked, global).

% Media organizations, museums, and lecture circuits devoted to defending the reading. Donations, ticket sales, book royalties, and staff careers all depend on the defense remaining necessary. Their research output aims at sustaining the reading's plausibility rather than testing it. Leaving the work would forfeit donor bases and identities built over decades.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, creationist_apologetics_ministries, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(anthropological_record__creationist_reading, creationist_apologetics_ministries, agenda_setter).

% Ordinary members who receive a unified account of where humans come from, why they matter, and how to live; the account binds families, congregations, and generations together. They also absorb friction when schooling, media, or a child's questions collide with the account, and they bear the labor of keeping the two worlds separate. Leaving would mean renegotiating every close relationship at once.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, believing_community_members, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(anthropological_record__creationist_reading, believing_community_members, payer).

% Students in homes and schools where the reading is taught as settled fact. When they notice the tension between what they are taught and what they read elsewhere, their options are silence, concealed doubt, or a rupture that can cost family, friendships, and housing. Most lack independent income or standing to negotiate the terms of their own learning.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, questioning_youth_in_creationist_schools, payer,
    powerless, biographical, trapped, local).

% Geologists, biologists, and archaeologists who belong to the tradition and find the reading untenable. Their professional training qualifies them above all others to assess the record, yet within their own communities their conclusions carry no adjudicative weight; speaking plainly marks them for correction or exclusion. They can work anywhere professionally, but exercising that mobility on this question means leaving the community that formed them.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, credentialed_scientists_within_tradition, excluded,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(anthropological_record__creationist_reading, credentialed_scientists_within_tradition, payer).

% Teachers in creationist schools tasked with presenting the reading as science. Many hold degrees from mainstream programs and privately harbor reservations; their employment, and often their standing in their congregation, depends on teaching past those reservations. Changing what they teach is not theirs to decide.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, parochial_science_educators, payer,
    moderate, biographical, constrained, regional).

% Universities, museums, journals, and professional societies whose methods and findings the reading rules out of bounds in subscribing communities. They date, sequence, and excavate, but their verdicts are treated as one more opinion to be rebutted rather than as adjudication. Participation in the communities' internal deliberation would require accepting premises their methods cannot adopt.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, secular_scientific_establishment, excluded,
    institutional, generational, mobile, global).

% Historians and philosophers of science and religion who study how the reading arose, what it protects, and how it changes. They take testimony from every seat, archive the court fights and curriculum wars, and owe allegiance to none of the parties.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, philosophy_of_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__creationist_reading, religious_authority_structures).
narrative_ontology:fixing_cost_class(anthropological_record__creationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a single authoritative account of human origins, nature, and destiny that binds cosmology to morality; coordinates congregational teaching, schooling, and family formation around one narrative; and settles existential questions about death, purpose, and human dignity with one answer, sparing members the cost of adjudicating rival expert testimonies themselves.
% TRANSFER_FUNCTION: Moves epistemic authority from credentialed science and individual judgment to the tradition's interpretive offices; moves deference, attendance, enrollment, and giving toward those offices and their allied ministries; and moves the reputational and relational costs of doubt onto questioners.
% ABSENT_VOICES: Credentialed scientists inside the tradition would object that adjudicative standing follows competence, not office; they sit in the pews but are absent from the interpretive councils, their objections routed through apologetic rebuttal rather than engagement. Questioning youth have no forum at all. The secular scientific establishment is outside the walls entirely. In many traditions, laypeople and women have limited standing in doctrinal bodies. The unanimity visible inside subscribing communities is therefore partly an artifact of who was admitted to the room.
% DISAPPEARANCE_RATIONALE: Creationist schools would restructure immediately, the apologetics economy would collapse, and large numbers of members would face an identity crisis their congregations are built to prevent; some communities would schism over what replaces the reading. The underlying faith would persist in altered form, but the specific arrangements — curricula, ministries, credentialing, donor flows — demonstrably depend on the reading and would rearrange around its absence.
% FOUNDING_PROBLEM: Once geology, evolutionary biology, and comparative philology made materialist readings of the record credible, communities whose authority rests on scriptural infallibility faced a choice: revise the authority structure, or insulate the reading of the record from scientific adjudication. The reading was consolidated — visibly from the 1910-1925 fundamentalist controversies onward — to solve that problem: protect revelatory authority and communal coherence against a science that could not be answered on its own terms.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: historians of American and global religion document the post-Darwin consolidation and its institutional motives; the court record (Scopes 1925, McLean 1982, Edwards v. Aguillard 1987, Kitzmiller v. Dover 2005) attests the continuing contest over the reading's public enforcement; published defector testimony attests the lived costs the arrangement manages. Theistic-evolutionist traditions that faced the same founding problem and resolved it by revising the authority structure corroborate both that the problem was real and that this particular resolution was a choice rather than a necessity.
narrative_ontology:disappearance_verdict(anthropological_record__creationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__creationist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__creationist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(anthropological_record__creationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__creationist_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__creationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__creationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__creationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.65 because the arrangement's costs concentrate sharply: questioning youth face rupture-level losses, credentialed scientists inside the tradition lose the standing their training earned, and educators teach against their own assessments, while the majority of members experience net meaning and belonging. Suppression (0.55) is real but mostly institutional and social rather than state-coercive in most jurisdictions: curricula are controlled, dissent is corrected, doubt is pastorally managed; part of it is internalized (see the suppression-mechanism omega). Theater (0.38) reflects a research apparatus — baraminology, alternative dating critiques — whose output is consumed for reassurance inside the community rather than tested against it, alongside genuinely functional catechesis and community formation. Accessibility collapse (0.62): inside the frame, naturalist readings collapse almost completely — they are not merely mistaken but corrosive of the gospel's internal logic — yet abundant alternatives persist outside subscribing communities, so collapse is partial at the population level. Resistance (0.55) is sustained: theistic-evolution movements inside the traditions, organized secular criticism, and recurring litigation. The temporal series runs on one shared grid (nine points spanning 1910-2025) and shows a genuine cycle rather than monotonic drift: enforcement surges (the anti-evolution statutes around 1925; the equal-time legislation of 1981-87), judicial defeats, retrenchment into institutions, rebranding (scientific creationism, then intelligent design), and renewed institutional hardening. I track suppression_requirement because enforcement capacity visibly changed — legal machinery was built, struck down, and rebuilt in new forms — rather than holding static; the troughs are real relaxations of coercive capacity even where institutional enforcement persisted. Base properties report the end-state (T=115) values.
 *
 * PERSPECTIVAL GAP:
 *   Four seats compute differently from identical structural data. From the authority seat the arrangement is stewardship: guarding a revelation that orders life and death against a reading that dissolves it. From the questioning-youth seat it is captivity: every door out is a door through something loved. From the credentialed-scientist seat it is dispossession: adjudicative authority transferred from competence to office. From the apologetics-ministry seat it is livelihood and calling fused into one identity. The engine computes these divergences from power, exit, and directional position; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Authority structures and apologetics ministries sit near the beneficiary end: the reading's continuance is what makes their offices necessary and their revenues durable. Rank-and-file believers sit near symmetric — genuine coordination goods received, diffuse collision costs paid. Questioning youth sit near the full-target end: they pay the highest prices with the least exit. Parochial educators derive high as payers with constrained exit. The secular scientific establishment is excluded rather than coordinated; its delegitimation is the enforcement object itself, and its mobility limits what the arrangement can take from it. One override is declared: credentialed scientists within the tradition would derive toward the middle on professional mobility alone, so the powerful atom is raised to 0.78 — their extraction is precisely the denial of standing inside their own community, and professional exit does not refund it; it purchases relief only by leaving the community that formed them. The analytical seat pays and receives nothing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting revelatory authority against materialist readings of the record while keeping the community coherent — is still live inside subscribing communities, so nothing here is resolved by obsolescence; the R5 mismatch consumer should find status=live paired with world_rearranges and raise no zombie flag. The tangled-rope classification earns its keep by refusing both simplifications: a pure-extraction label would erase the real coordination goods (identity, moral formation, intergenerational continuity) that sustain millions of unharmed participants, while a pure-coordination label would erase the youth, scientists, and educators who pay concentrated, enforced costs. The arrangement holds because enforcement is active — curricula, credentialing, correction — and because the parties who could relax it are the parties whose legitimacy depends on not relaxing it, which is why fixing_cost is prohibitive and the gains land on a named seat rather than diffusely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the anthropological_record kernel (the creationist_reading). What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative classification across the three reading files: the naturalist_reading transfers adjudicative authority to credentialed science and removes the divine-causation requirement; the indigenous_epistemology_reading transfers the knowledge channel to sustained oral tradition and relational continuity. The disagreement is located in the adjudicative channel for the record''s meaning, not in the data itself.',
    'Adoption of the naturalist reading within a community empties this reading''s victim set (no materialist timeline to suppress) and thins its beneficiary set to institutions retained by other goods; adoption of the indigenous reading leaves this reading''s structure intact but adds a rival knowledge channel it must either accommodate or police.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this file instantiates one of three rival readings of the anthropological-record kernel; sibling readings are separate epsilon-invariant constraints.').

omega_variable(
    disclosure_vs_construction,
    'Is the reading a disclosure of what the record actually shows (as its holders assert) or a constructed interpretive discipline whose persistence serves institutional continuity?',
    'Convergent-evidence test: independent lines (radiometric dating, genomics, stratigraphy) agreeing on deep time count against disclosure of a recent created order; the reading''s response pattern to such convergence — harmonizing reinterpretation versus defensive apparatus-building — diagnoses which function dominates.',
    'If constructed, the arrangement''s coordination claims weaken and its enforcement profile reads as authority maintenance; if disclosed, part of the measured suppression is protection of a truth its holders believe lives depend on, and the extraction assessment must discount accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclosure_vs_construction, conceptual, 'Whether the reading tracks the record''s content or institutional persistence needs.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (curriculum control, credentialing, congregational correction, in some jurisdictions statute) or internalized (doubt experienced as sin, thought-policing that persists without enforcers)?',
    'Post-exit suppression trajectory of former members: if doubt-policing and evidential avoidance persist after leaving the enforcing community, the internalized share is substantial.',
    'If internalized, effective suppression exceeds the structural measure — members carry the policing with them after exit, and exit alone does not release it; the omega splits the scalar into structural and internalized components for downstream analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in the subscribing communities.').

omega_variable(
    rank_and_file_net_position,
    'Do ordinary believing members experience net benefit (meaning, belonging, moral formation) exceeding their epistemic and collision costs, or are they primarily bearing the arrangement''s costs?',
    'Longitudinal wellbeing and doubt-distress studies inside subscribing communities compared against matched religious communities that do not enforce the reading.',
    'If net-benefiting, the victim declarations overstate for this seat and the arrangement sits closer to pure coordination; if net-harmed, the extraction is broader than the dissenter minority and the beneficiary declaration for this seat should be discounted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rank_and_file_net_position, empirical, 'Net position of the rank-and-file seat between coordination goods and borne costs.').

omega_variable(
    enforcement_cycle_driver,
    'Is the oscillating enforcement intensity (legal surges, judicial defeats, retrenchment, rebranding) an extraction-sustaining mechanism — intermittent reinforcement that re-commits the base after each defeat — or an exogenous echo of broader culture-war cycles?',
    'Compare donation and enrollment trajectories following enforcement defeats against trajectories following external provocations; if defeats reliably energize the base more than provocations do, the cycle is endogenous to the arrangement.',
    'If endogenous, the oscillation is part of the persistence machinery and the suppression_requirement troughs overstate genuine relaxation; if exogenous, the series should be read as environmental noise around a stable enforcement baseline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cycle_driver, empirical, 'Whether the enforcement cycle is an internal reinforcement mechanism or external noise.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel ''what the record shows about origins'' (three answers to one question) or ''who may adjudicate the record''s meaning'' (three rivals for one office)?',
    'Test whether the readings dispute answers or dispute standing: if each reading''s core claim concerns an epistemic channel rather than a content proposition, the office-framing is the correct one.',
    'Under the office-framing, this reading''s defining feature is the transfer of adjudicative monopoly away from credentialed science, and classification should weight the displaced-scientist seat more heavily; under the content-framing, the timeline-suppression feature dominates and the scientist seat is secondary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Two coherent framings of the kernel yield different weighting of the reading''s structural features.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__creationist_reading, 0, 115).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__creationist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(anth_tr_t0, observed).
narrative_ontology:measurement(anth_tr_t15, anthropological_record__creationist_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement_basis(anth_tr_t15, observed).
narrative_ontology:measurement(anth_tr_t30, anthropological_record__creationist_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement_basis(anth_tr_t30, observed).
narrative_ontology:measurement(anth_tr_t45, anthropological_record__creationist_reading, theater_ratio, 45, 0.28).
narrative_ontology:measurement_basis(anth_tr_t45, observed).
narrative_ontology:measurement(anth_tr_t60, anthropological_record__creationist_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement_basis(anth_tr_t60, observed).
narrative_ontology:measurement(anth_tr_t75, anthropological_record__creationist_reading, theater_ratio, 75, 0.42).
narrative_ontology:measurement_basis(anth_tr_t75, observed).
narrative_ontology:measurement(anth_tr_t90, anthropological_record__creationist_reading, theater_ratio, 90, 0.45).
narrative_ontology:measurement_basis(anth_tr_t90, observed).
narrative_ontology:measurement(anth_tr_t105, anthropological_record__creationist_reading, theater_ratio, 105, 0.4).
narrative_ontology:measurement_basis(anth_tr_t105, observed).
narrative_ontology:measurement(anth_tr_t115, anthropological_record__creationist_reading, theater_ratio, 115, 0.38).
narrative_ontology:measurement_basis(anth_tr_t115, observed).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__creationist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(anth_be_t0, observed).
narrative_ontology:measurement(anth_be_t15, anthropological_record__creationist_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement_basis(anth_be_t15, observed).
narrative_ontology:measurement(anth_be_t30, anthropological_record__creationist_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(anth_be_t30, observed).
narrative_ontology:measurement(anth_be_t45, anthropological_record__creationist_reading, base_extractiveness, 45, 0.4).
narrative_ontology:measurement_basis(anth_be_t45, observed).
narrative_ontology:measurement(anth_be_t60, anthropological_record__creationist_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement_basis(anth_be_t60, observed).
narrative_ontology:measurement(anth_be_t75, anthropological_record__creationist_reading, base_extractiveness, 75, 0.58).
narrative_ontology:measurement_basis(anth_be_t75, observed).
narrative_ontology:measurement(anth_be_t90, anthropological_record__creationist_reading, base_extractiveness, 90, 0.6).
narrative_ontology:measurement_basis(anth_be_t90, observed).
narrative_ontology:measurement(anth_be_t105, anthropological_record__creationist_reading, base_extractiveness, 105, 0.63).
narrative_ontology:measurement_basis(anth_be_t105, observed).
narrative_ontology:measurement(anth_be_t115, anthropological_record__creationist_reading, base_extractiveness, 115, 0.65).
narrative_ontology:measurement_basis(anth_be_t115, observed).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__creationist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(anth_su_t0, observed).
narrative_ontology:measurement(anth_su_t15, anthropological_record__creationist_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(anth_su_t15, observed).
narrative_ontology:measurement(anth_su_t30, anthropological_record__creationist_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement_basis(anth_su_t30, observed).
narrative_ontology:measurement(anth_su_t45, anthropological_record__creationist_reading, suppression_requirement, 45, 0.45).
narrative_ontology:measurement_basis(anth_su_t45, observed).
narrative_ontology:measurement(anth_su_t60, anthropological_record__creationist_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement_basis(anth_su_t60, observed).
narrative_ontology:measurement(anth_su_t75, anthropological_record__creationist_reading, suppression_requirement, 75, 0.66).
narrative_ontology:measurement_basis(anth_su_t75, observed).
narrative_ontology:measurement(anth_su_t90, anthropological_record__creationist_reading, suppression_requirement, 90, 0.6).
narrative_ontology:measurement_basis(anth_su_t90, observed).
narrative_ontology:measurement(anth_su_t105, anthropological_record__creationist_reading, suppression_requirement, 105, 0.57).
narrative_ontology:measurement_basis(anth_su_t105, observed).
narrative_ontology:measurement(anth_su_t115, anthropological_record__creationist_reading, suppression_requirement, 115, 0.55).
narrative_ontology:measurement_basis(anth_su_t115, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__creationist_reading, identity_coordination).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the anthropological_record kernel (epsilon-invariance principle): the colloquial label 'what the record reveals' covers three structurally distinct arrangements differing in adjudicative channel and in beneficiary/victim structure. This file authors the creationist reading (epsilon approximately 0.65: an enforced interpretive discipline with concentrated dissent costs and a named-seat gain flow). The naturalist reading authors the standing scientific arrangement (negligible internal extraction; its epsilon reflects external pressures upon it). The indigenous reading authors the oral-tradition arrangement (its epsilon reflects colonial-era and ongoing suppression of that channel). The upstream/downstream edges run in both directions — each reading cites the others' failures to legitimize itself — so all three files link mutually via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(anthropological_record__creationist_reading, powerful, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
