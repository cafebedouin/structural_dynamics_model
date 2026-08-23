% ============================================================================
% CONSTRAINT STORY: genesis_creation_narrative__literal_young_earth
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_narrative__literal_young_earth, []).

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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: genesis_creation_narrative__literal_young_earth
 *   human_readable: Literal Young-Earth Reading of Genesis 1-2 as Enforced Hermeneutic
 *   domain: religious/hermeneutical/epistemic
 *
 * SUMMARY:
 *   Within conservative Protestant institutions, Genesis 1-2 is read as an
 *   inerrant historical-scientific chronicle: six consecutive
 *   twenty-four-hour days, a recent creation dated through genealogies, a
 *   global flood. The reading is not merely held; it is administered. Faculty
 *   covenants, ordination standards, curricula, publishing pipelines, and
 *   speaker-vetting machinery make assent a condition of employment,
 *   credentialing, and belonging, while a media-and-museum economy monetizes
 *   its defense. The reading presents itself as fixed divine givens — not a
 *   human arrangement at all — which is precisely why its institutional
 *   beneficiaries, enforcement dependencies, and suppressed alternatives
 *   matter to classify correctly. FAMILY NOTE (epsilon-invariance): the
 *   colloquial label 'the Genesis creation account' decomposes into three
 *   structurally distinct stories — this one (inerrant chronicle; substantial
 *   suppression, real victims), theistic_evolutionary (framework compatible
 *   with science; enforcement pressure but different victim set), and
 *   allegorical_ancient_near_east (mythopoetic literature; near-zero
 *   epistemic suppression). Each carries its own epsilon over the same text;
 *   they are linked via network.affects_constraints, not averaged.
 *
 * KEY AGENTS:
 *   - young_earth_apologetics_ministries: Agenda-setting collector (organized/arbitrage) — produces and polices the account; media assets portable
 *   - denominational_doctrine_gatekeepers: Credentialing beneficiary with enforcement duties (institutional/constrained)
 *   - pastoral_leadership_class: Identity-locked beneficiary-payer (organized/identity_locked)
 *   - non_literalist_believers: Conforming payer under censure (moderate/identity_locked)
 *   - christian_academy_faculty: Career-exposed payer (moderate/constrained)
 *   - creation_curricula_students: Trapped youngest payers (powerless/trapped)
 *   - evolutionary_biology_researchers: Excluded counterparties (institutional/mobile)
 *   - science_religion_historians: Analytical observers (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__literal_young_earth, 0.68).
domain_priors:suppression_score(genesis_creation_narrative__literal_young_earth, 0.78).
domain_priors:theater_ratio(genesis_creation_narrative__literal_young_earth, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__literal_young_earth, extractiveness, 0.68).
narrative_ontology:constraint_metric(genesis_creation_narrative__literal_young_earth, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(genesis_creation_narrative__literal_young_earth, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__literal_young_earth, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(genesis_creation_narrative__literal_young_earth, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__literal_young_earth, tangled_rope).
narrative_ontology:human_readable(genesis_creation_narrative__literal_young_earth, "Literal Young-Earth Reading of Genesis 1-2 as Enforced Hermeneutic").
narrative_ontology:topic_domain(genesis_creation_narrative__literal_young_earth, "religious/hermeneutical/epistemic").

domain_priors:requires_active_enforcement(genesis_creation_narrative__literal_young_earth).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__literal_young_earth, 'a3a66b99-ad92-479f-bf64-09555444e8b9').
narrative_ontology:cs_kernel_codification('a3a66b99-ad92-479f-bf64-09555444e8b9', fixed_text).
narrative_ontology:cs_authority_grounding('a3a66b99-ad92-479f-bf64-09555444e8b9', lineage).
narrative_ontology:cs_interpretation_layer_present('a3a66b99-ad92-479f-bf64-09555444e8b9').
narrative_ontology:cs_reading_relation('a3a66b99-ad92-479f-bf64-09555444e8b9', genesis_creation_narrative__theistic_evolutionary, forecloses).
narrative_ontology:cs_reading_relation('a3a66b99-ad92-479f-bf64-09555444e8b9', genesis_creation_narrative__allegorical_ancient_near_east, forecloses).
narrative_ontology:cs_axiom('a3a66b99-ad92-479f-bf64-09555444e8b9', foundational, recent_creation_six_day_sequence_factual).
narrative_ontology:cs_axiom_status(recent_creation_six_day_sequence_factual, holdable).
narrative_ontology:cs_axiom_grounding('a3a66b99-ad92-479f-bf64-09555444e8b9', recent_creation_six_day_sequence_factual, empirically_contingent).
narrative_ontology:cs_axiom('a3a66b99-ad92-479f-bf64-09555444e8b9', secondary, inerrancy_extends_to_genesis_chronicle).
narrative_ontology:cs_axiom_status(inerrancy_extends_to_genesis_chronicle, holdable).
narrative_ontology:cs_axiom_grounding('a3a66b99-ad92-479f-bf64-09555444e8b9', inerrancy_extends_to_genesis_chronicle, theological).
narrative_ontology:cs_reference_frame('a3a66b99-ad92-479f-bf64-09555444e8b9', plain_sense_inerrant_chronicle).
narrative_ontology:cs_drift_state('a3a66b99-ad92-479f-bf64-09555444e8b9', post_genomic_deep_time_consensus, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a3a66b99-ad92-479f-bf64-09555444e8b9', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__literal_young_earth, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__literal_young_earth, young_earth_apologetics_ministries).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__literal_young_earth, denominational_doctrine_gatekeepers).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__literal_young_earth, pastoral_leadership_class).
narrative_ontology:constraint_victim(genesis_creation_narrative__literal_young_earth, non_literalist_believers).
narrative_ontology:constraint_victim(genesis_creation_narrative__literal_young_earth, christian_academy_faculty).
narrative_ontology:constraint_victim(genesis_creation_narrative__literal_young_earth, creation_curricula_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(genesis_creation_narrative__literal_young_earth, pastoral_leadership_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate museums, publishing houses, video studios, school curricula, and speaker networks that produce the young-earth account and decide which books, speakers, and materials circulate in partner churches and schools, running dedicated rebuttal programs against challenges. Revenue arrives as donations, admissions, product sales, and licensing; staff careers and organizational continuity depend on the account staying mandatory in client institutions. Assets are media-portable: the organizations can shift audiences and markets far faster than congregations can.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, young_earth_apologetics_ministries, agenda_setter,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__literal_young_earth, young_earth_apologetics_ministries, beneficiary).

% Seminary trustees, faculty committees, and denominational courts adopt faculty covenants and ordination standards requiring affirmation of six-day recent creation; they examine candidates, review publications, and adjudicate disputes over the reading. Enrollment, donor confidence, and denominational peace ride on visible firmness, and loosening the standard invites splits and donor flight, so the standard is administered rather than revisited.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, denominational_doctrine_gatekeepers, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__literal_young_earth, denominational_doctrine_gatekeepers, agenda_setter).

% Teach and preach the plain-sense chronicle weekly; congregational trust, network standing, and livelihood follow from unwavering fidelity. Many harbor private questions about the science but cannot voice them without endangering their calling, since doubt from the pulpit reads as betrayal. Leaving the reading means leaving vocation, congregation, and social world simultaneously.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, pastoral_leadership_class, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__literal_young_earth, pastoral_leadership_class, payer).

% Members whose own contact with the evidence — geology, genomes, cosmology — leads them to accept an ancient earth or common descent while remaining inside the tradition. They face teaching bans, platform withdrawal, and quiet censure; staying costs ongoing silence, leaving costs congregation, family approval, and a lifelong-formed identity. Most manage the tension privately.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, non_literalist_believers, payer,
    moderate, biographical, identity_locked, national).

% Professors and researchers at confessional colleges and seminaries sign covenants requiring the reading; hiring, tenure, and publication channels run through it. Work touching origins must be framed within flood geology or set aside, and public deviation has ended careers. Moving to secular institutions forfeits network standing, seniority, and often research momentum.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, christian_academy_faculty, payer,
    moderate, biographical, constrained, national).

% Children and teenagers in homeschool cooperatives and Christian schools receive science texts presenting a roughly six-thousand-year earth and a global flood as settled fact and evolution as deliberate fraud. They choose none of it; the account shapes their preparation before they meet counterevidence, typically at university, where the collision lands on trust as much as content.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, creation_curricula_students, payer,
    powerless, immediate, trapped, local).

% Working scientists barred by charter from teaching or speaking inside these institutions except as debate foils; their findings enter the community mainly through hostile summary. Outside, they publish and teach freely, so the bar runs one way, and maintaining it is what the internal standard exists to do.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, evolutionary_biology_researchers, excluded,
    institutional, generational, mobile, global).

% Scholars of religion and science who archive and analyze the movement from denominational records, correspondence, and survey data. Their accounts document the reading's emergence as an organized program in the early twentieth century and its shifting functions since; neither party commissions or controls their findings.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, science_religion_historians, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_narrative__literal_young_earth, young_earth_apologetics_ministries).
narrative_ontology:fixing_cost_class(genesis_creation_narrative__literal_young_earth, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a transnational conservative-Protestant identity around a single authoritative account of origins: a shared curriculum, a crisp boundary between insiders and outsiders, and a doctrinal chain in which the reliability of Genesis underwrites the authority of the whole canon ('if Genesis falls, the Bible falls'). Stated without evaluation.
% TRANSFER_FUNCTION: Moves money, attention, and institutional trust from rank-and-file believers, donors, and tuition-paying families toward apologetics ministries, publishing houses, and credentialing institutions; moves careers, standing, and speech permissions from dissenting scholars toward conforming ones; moves curricular content control from trained scientists to confessional authorities.
% ABSENT_VOICES: Non-literalist believers in the pews, evolutionary scientists (barred by charter), and the students subject to the curricula have no seat where the reading is adjudicated; confessional standards are drafted, examined, and enforced exclusively by the parties who benefit from them.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would dissolve the creation-apologetics economy (museums, curricula, media networks), force seminaries and denominations to rewrite covenants or schism over the void, release silenced faculty and pastors, and strand a generation of curricula mid-stream — while Genesis itself would remain in the canon, read theologically as many older traditions read it. The rearrangement is concentrated and costly for named parties, not diffuse.
% FOUNDING_PROBLEM: Defend the authority of Scripture and the plausibility of the faith against nineteenth-century historical geology and Darwinian evolution, which adherents experienced as a combined assault (with higher criticism) on Christianity's foundations; later, supply a unified identity marker for the fundamentalist coalition after its public defeats.
% FOUNDING_PROBLEM_CORROBORATION: Internal parties (apologetics ministries, confessional seminaries) attest the problem is live, citing ongoing threats to faith from evolutionary teaching. External corroboration cuts the other way: historians of science (Ronald Numbers, 'The Creationists') document the modern organized origin and shifting functions of the movement; the U.S. National Academy of Sciences and the Kitzmiller v. Dover ruling record the scientific consensus against the reading's empirical claims; old-earth evangelical theologians attest from inside the tradition that the founding crisis is scientifically resolved and persistence is sociological. No party outside the beneficiary set attests the founding problem remains live in its original form.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__literal_young_earth, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_narrative__literal_young_earth, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__literal_young_earth, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(genesis_creation_narrative__literal_young_earth, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__literal_young_earth, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_narrative__literal_young_earth_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(genesis_creation_narrative__literal_young_earth, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(genesis_creation_narrative__literal_young_earth_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects concrete, recurring costs borne by named groups: dismissed faculty, silenced pastors, miseducated students, censured members — against real but smaller offsetting goods (identity, community, doctrinal coherence) that partially subsidize participants. Suppression (0.78) is high because persistence depends on actively closing exits and alternatives — covenant termination, ordination refusal, platform withdrawal — not on voluntary preference; suppression is authored as a raw structural property and is not scaled by power or scope anywhere downstream. Theater (0.42) captures the movement's growing spectacle sector — museum attractions, debate circuits, conference pageantry — activity that performs fidelity rather than extending the account's explanatory reach; the underlying teaching and community functions are real, so theater stays below half. Accessibility collapse (0.62) is strong-but-porous: inside the community, non-literal readings are foreclosed as apostasy, while outside them alternatives flourish and leak back in through the internet. Resistance (0.66) is sustained and organized: old-earth evangelicals, professional scientific societies, court losses (Edwards v. Aguillard; Kitzmiller v. Dover), and recurring internal dissent. Boltzmann note: coordination_type is identity_coordination, and the gaming risk flagged for that type is live here — the reading wraps extraction in identity language ('this is who we are'), so the Power x Scope coupling test deserves scrutiny; the identity offset accommodates genuine boundary maintenance, not the covenant-enforced silencing of credentialed dissent. Temporally the series share one grid (t=0 is 1900 CE, 25-year steps to t=125 = 2025 CE); dynamics are ratchet-shaped with enforcement waves (Scopes 1925, the post-Genesis-Flood institutionalization 1961-1980, the platform era) superimposed on secular rise — waves, not a repeating cycle, so no intermittent-reinforcement claim is made. Endpoint values match base_properties by construction of the shared grid.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently. From the ministry and gatekeeper seats the arrangement is a treasured inheritance they administer — coordination goods, boundary clarity, institutional continuity. From the faculty, student, and dissenting-believer seats the same machinery operates as coercive control over belief, speech, and career. From the excluded scientists' seat it is simple epistemic closure with a one-way door. From the historian's seat it is a twentieth-century organizational project presenting itself as timeless. The engine computes these per-seat divergences from power, exit, and role data; the story-level claim does not and should not reconcile them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: the ministries (collect revenue and reach; arbitrage exit keeps them near the subsidy end), the gatekeepers (collect enrollment, donor trust, adjudicatory prestige), and the pastoral class (collect authority and standing — but with a payer secondary role, since their own speech is bound). Victims: dissenting believers (locked by identity), faculty (locked by career channel), students (trapped by age and dependence). Derived directionalities from role-plus-exit suffice here, so no directionality_overrides entries are authored: the available power atoms collide across seats with opposed relationships (organized spans both ministries and pastors; institutional spans both gatekeepers and excluded researchers), so any per-power-atom override would contaminate one seat to adjust the other. The excluded researchers carry no beneficiary/victim declaration by design — their exclusion is recorded on the stakeholder surface and in the absent-voices answer, not converted into a pseudo-victimship.
 *
 * MANDATROPHY ANALYSIS:
 *   Two mislabels guard against. First, pure-extraction mislabeling: the reading does coordinate something genuine — a durable transnational identity, an intergenerationally transmissible curriculum, a doctrinal chain tying origins to canon authority — so a snare verdict would erase the real goods participants receive and mispredict why exit is costly. Second, fixed-truth mislabeling: the reading presents itself as unconstructed givens, yet it emerged as an organized program within living historiographic memory, employs dedicated enforcement machinery, and maintains itself against converging independent evidence lines — hallmarks of a constructed, actively held arrangement, not a natural limit. Mandatrophy status: the founding problem (geology-and-Darwin as existential threat) is contested rather than dead — live for adherents, resolved by external scientific and historical consensus — and the arrangement has visibly outgrown its original function into identity maintenance and spectacle, tracked by the rising theater_ratio. The mismatch consumer should read founding_problem_status=contested alongside the world_rearranges verdict and the named-seat gain_flow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_index,
    'This constraint is one reading (literal_young_earth) of the genesis_creation_narrative kernel; what would change if a sibling reading were instantiated instead?',
    'Compile the sibling stories separately (genesis_creation_narrative__theistic_evolutionary, genesis_creation_narrative__allegorical_ancient_near_east) and compare victim sets, suppression profiles, and epsilon across the family.',
    'The allegorical sibling carries near-zero epistemic suppression and a near-empty victim set; the theistic_evolutionary sibling dissolves the science conflict and relocates enforcement to softer credentialing pressure. Merging the readings into one story would average away the suppression profile unique to this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_index, conceptual, 'Committer-frame routing: one kernel, three readings, three structurally distinct constraints.').

omega_variable(
    epsilon_seat_indexing,
    'Story-level extractiveness is authored from the arrangement''s operation on dissenters and the taught; from the reading''s own seat the same arrangement scores near zero (fidelity to revelation framed as blessing, not burden). Which seat''s indexing governs?',
    'Per-seat engine computation from the structural data: payer seats with locked exits versus beneficiary seats with arbitrage yield divergent effective extraction without a single reconciled scalar.',
    'Payer-seat classifications come out snare-flavored; beneficiary-seat classifications come out rope-flavored; forcing one number erases the divergence that is the finding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_seat_indexing, conceptual, 'Reading-indexed epsilon over a fixed referent: the enforced literalist hermeneutic itself.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression mostly structural (covenants, credentialing gates, platform withdrawal) or internalized (believers self-censor before any gate acts)?',
    'Post-exit trajectory interviews with leavers: if self-censorship and dread persist after the gates are removed, a large fraction is internalized.',
    'If internalized, effective suppression exceeds the structural measure — targets carry the constraint with them after formally free exit exists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in a confessional community.').

omega_variable(
    dominion_license_causality,
    'Does the dominion-as-exploitation license attributed to this reading causally flow from the literalist hermeneutic itself, or from correlated political conservatism?',
    'Matched comparison of environmental attitudes across creationist versus old-earth evangelical populations sharing political demographics.',
    'Determines the weight of the network edge to environmental-dominion constraints; a weak causal path demotes the edge to correlation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dominion_license_causality, empirical, 'Whether the reading''s behavioral delta on dominion is hermeneutically caused.').

omega_variable(
    textual_genre_naturalness,
    'Is the plain-sense-historical genre a property of the text itself or a constructed hermeneutical tradition layered onto Ancient Near Eastern mythopoetic literature?',
    'Reception of ANE comparative philology and literary-structure analysis among specialists with no stake in the confessional dispute.',
    'If the genre is constructed, the reading''s presentation of itself as fixed textual givens is a false-summit pattern with identifiable gatekeeper beneficiaries; if the text self-identifies as reportage, part of the binding force is referential rather than institutional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_genre_naturalness, conceptual, 'Naturalness ambiguity: text-immanent genre versus constructed interpretive tradition.').

omega_variable(
    axiom_revision_threshold,
    'What evidence, if any, would this reading accept as overriding its recent-creation premise — accelerated decay rates, appearance-of-age, or nothing?',
    'Track documented defection and disciplinary cases triggered specifically by age-of-earth conclusions, versus cases absorbed by auxiliary hypotheses.',
    'Decides whether the empirically contingent foundational axiom behaves as live-testable or as shielded by the theological wrapper in practice, which governs the foreclosure computation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_revision_threshold, empirical, 'Practical revisability of the reading''s core empirical axiom.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__literal_young_earth, 0, 125).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, genesis_creation_narrative__literal_young_earth, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gene_tr_t25, genesis_creation_narrative__literal_young_earth, theater_ratio, 25, 0.2).
narrative_ontology:measurement(gene_tr_t50, genesis_creation_narrative__literal_young_earth, theater_ratio, 50, 0.24).
narrative_ontology:measurement(gene_tr_t75, genesis_creation_narrative__literal_young_earth, theater_ratio, 75, 0.31).
narrative_ontology:measurement(gene_tr_t100, genesis_creation_narrative__literal_young_earth, theater_ratio, 100, 0.39).
narrative_ontology:measurement(gene_tr_t125, genesis_creation_narrative__literal_young_earth, theater_ratio, 125, 0.42).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, genesis_creation_narrative__literal_young_earth, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gene_be_t25, genesis_creation_narrative__literal_young_earth, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(gene_be_t50, genesis_creation_narrative__literal_young_earth, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(gene_be_t75, genesis_creation_narrative__literal_young_earth, base_extractiveness, 75, 0.6).
narrative_ontology:measurement(gene_be_t100, genesis_creation_narrative__literal_young_earth, base_extractiveness, 100, 0.66).
narrative_ontology:measurement(gene_be_t125, genesis_creation_narrative__literal_young_earth, base_extractiveness, 125, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, genesis_creation_narrative__literal_young_earth, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(gene_su_t25, genesis_creation_narrative__literal_young_earth, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(gene_su_t50, genesis_creation_narrative__literal_young_earth, suppression_requirement, 50, 0.52).
narrative_ontology:measurement(gene_su_t75, genesis_creation_narrative__literal_young_earth, suppression_requirement, 75, 0.68).
narrative_ontology:measurement(gene_su_t100, genesis_creation_narrative__literal_young_earth, suppression_requirement, 100, 0.74).
narrative_ontology:measurement(gene_su_t125, genesis_creation_narrative__literal_young_earth, suppression_requirement, 125, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__literal_young_earth, identity_coordination).
narrative_ontology:affects_constraint(genesis_creation_narrative__literal_young_earth, genesis_creation_narrative__theistic_evolutionary).
narrative_ontology:affects_constraint(genesis_creation_narrative__literal_young_earth, genesis_creation_narrative__allegorical_ancient_near_east).
narrative_ontology:affects_constraint(genesis_creation_narrative__literal_young_earth, environmental_dominion_ethic).

% DUAL FORMULATION NOTE:
% Constraint family: 'the Genesis creation account' is a colloquial label covering three structurally distinct constraints (epsilon-invariance decomposition). Upstream: the allegorical_ancient_near_east reading (highest scholarly confidence, negligible extraction) supplies the comparative-literature evidence that pressures the other two; theistic_evolutionary mediates between it and the literalist camp. Downstream: this literalist reading influences both siblings' operating environments — its enforcement machinery raises the legitimacy cost of holding them inside conservative institutions — and, via the dominion-as-exploitation reading of Genesis 1:28 (expected structural delta), feeds the environmental_dominion_ethic constraint. Edge weights to the dominion constraint await the dominion_license_causality omega's resolution. Each family member links the others via affects_constraints per the family rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
