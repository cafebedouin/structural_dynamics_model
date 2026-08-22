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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Genesis 1-2 Literal Young-Earth Reading: Institutionalized Inerrant-Chronicle Requirement
 *   domain: religious/hermeneutical/science-religion
 *
 * SUMMARY:
 *   Within conservative Protestant institutions, Genesis 1-2 is required
 *   reading-as-chronicle: members, clergy, educators, and scientists must
 *   affirm that the text records six literal 24-hour days culminating in a
 *   recent creation, and non-literal readings are treated as error. This
 *   story instantiates the literal_young_earth reading of the Genesis
 *   creation-narrative kernel as a single ε-invariant constraint. The ε
 *   referent is the standing arrangement under contest — the
 *   institutionalized requirement of literalist assent itself — and its value
 *   describes that arrangement as it actually operates, not any arrangement
 *   the reading would prefer. The claim/metric gap is deliberate: the reading
 *   presents itself as pure fidelity to Scripture (a coordination claim),
 *   while the authored metrics describe an actively enforced arrangement with
 *   substantial asymmetric costs; the engine measures that divergence rather
 *   than reconciling it.
 *
 * KEY AGENTS:
 *   - conservative_denominational_leadership: agenda setter (institutional/identity_locked) — sets the doctrinal boundary, administers discipline, speaks for the community
 *   - creationist_ministry_organizations: primary beneficiary (organized/mobile) — collects donations, curriculum sales, admissions, and media revenue tied to the required reading
 *   - sincerely_convinced_believers: beneficiary carrying payer costs (organized/constrained) — receives identity, certainty, and community; carries friction with the scientific culture
 *   - privately_doubting_believers: primary payer (moderate/identity_locked) — public assent masking private doubt; exit is family, community, and livelihood rupture
 *   - creationist_institution_scientists: payer (moderate/identity_locked) — credentialed scientists bound by signed statements; conclusions must fit the framework
 *   - creation_science_students: payer (powerless/constrained) — taught the reading as science without opt-out or standing
 *   - non_literalist_theologians: excluded voice (moderate/mobile) — argues alternative readings from outside authorized channels
 *   - evolutionary_scientists: excluded voice (organized/mobile) — produces the categorically-rejected evidence; no seat inside
 *   - hermeneutics_scholars: analytical observer (analytical/analytical) — sees text, institutions, and enforcement whole
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__literal_young_earth, 0.62).
domain_priors:suppression_score(genesis_creation_narrative__literal_young_earth, 0.8).
domain_priors:theater_ratio(genesis_creation_narrative__literal_young_earth, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__literal_young_earth, extractiveness, 0.62).
narrative_ontology:constraint_metric(genesis_creation_narrative__literal_young_earth, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(genesis_creation_narrative__literal_young_earth, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__literal_young_earth, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(genesis_creation_narrative__literal_young_earth, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__literal_young_earth, tangled_rope).
narrative_ontology:human_readable(genesis_creation_narrative__literal_young_earth, "Genesis 1-2 Literal Young-Earth Reading: Institutionalized Inerrant-Chronicle Requirement").
narrative_ontology:topic_domain(genesis_creation_narrative__literal_young_earth, "religious/hermeneutical/science-religion").

domain_priors:requires_active_enforcement(genesis_creation_narrative__literal_young_earth).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__literal_young_earth, 'b9dd326e-671d-4ae0-bd72-1dc3f28b19da').
narrative_ontology:cs_kernel_codification('b9dd326e-671d-4ae0-bd72-1dc3f28b19da', fixed_text).
narrative_ontology:cs_authority_grounding('b9dd326e-671d-4ae0-bd72-1dc3f28b19da', lineage).
narrative_ontology:cs_interpretation_layer_present('b9dd326e-671d-4ae0-bd72-1dc3f28b19da').
narrative_ontology:cs_reading_relation('b9dd326e-671d-4ae0-bd72-1dc3f28b19da', genesis_creation_narrative__theistic_evolutionary, forecloses).
narrative_ontology:cs_reading_relation('b9dd326e-671d-4ae0-bd72-1dc3f28b19da', genesis_creation_narrative__allegorical_ancient_near_east, forecloses).
narrative_ontology:cs_axiom('b9dd326e-671d-4ae0-bd72-1dc3f28b19da', foundational, genesis_inerrant_historical_chronicle).
narrative_ontology:cs_axiom_status(genesis_inerrant_historical_chronicle, holdable).
narrative_ontology:cs_axiom_grounding('b9dd326e-671d-4ae0-bd72-1dc3f28b19da', genesis_inerrant_historical_chronicle, empirically_contingent).
narrative_ontology:cs_axiom('b9dd326e-671d-4ae0-bd72-1dc3f28b19da', foundational, plain_sense_hermeneutic_sufficiency).
narrative_ontology:cs_axiom_status(plain_sense_hermeneutic_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('b9dd326e-671d-4ae0-bd72-1dc3f28b19da', plain_sense_hermeneutic_sufficiency, conventional).
narrative_ontology:cs_reference_frame('b9dd326e-671d-4ae0-bd72-1dc3f28b19da', plain_sense_inerrant_chronicle).
narrative_ontology:cs_drift_state('b9dd326e-671d-4ae0-bd72-1dc3f28b19da', contemporary_scientific_scholarly_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b9dd326e-671d-4ae0-bd72-1dc3f28b19da', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__literal_young_earth, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__literal_young_earth, creationist_ministry_organizations).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__literal_young_earth, conservative_denominational_leadership).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__literal_young_earth, sincerely_convinced_believers).
narrative_ontology:constraint_victim(genesis_creation_narrative__literal_young_earth, privately_doubting_believers).
narrative_ontology:constraint_victim(genesis_creation_narrative__literal_young_earth, creationist_institution_scientists).
narrative_ontology:constraint_victim(genesis_creation_narrative__literal_young_earth, creation_science_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(genesis_creation_narrative__literal_young_earth, sincerely_convinced_believers).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__literal_young_earth, biblical_inerrancy_doctrine).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__literal_young_earth, recent_creation_chronology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the doctrinal statements that ordination, membership, teaching, and missionary service require, including affirmations that Genesis 1-2 records six literal days and a recent creation. Administers discipline for clergy and members who publicly teach otherwise, approves curricula, credentials creationist schools, and speaks for the community in public disputes over origins. Their office, standing, and the cohesion they preside over are bound to the boundary they maintain; stepping back from it would cost them their position within their own constituency.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, conservative_denominational_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Publish curricula, run museums and media outlets, stage debates, and organize conferences presenting Genesis 1-2 as a literal historical record. Funded by donations, curriculum sales, admissions, and media revenue that depend on the literal reading remaining the community's required position. They operate across congregations and countries and can shift messaging, but their institutional purpose is bound to the reading they promote.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, creationist_ministry_organizations, beneficiary,
    organized, biographical, mobile, global).

% Hold the literal reading as personally true and experience it as the plain meaning of Scripture and a source of certainty, identity, and community. They support the institutions that teach it, and they also carry costs the arrangement generates: social friction with the wider scientific culture and the requirement to treat mainstream geology, biology, and cosmology as mistaken. Leaving the framework would mean leaving their religious world.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, sincerely_convinced_believers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__literal_young_earth, sincerely_convinced_believers, payer).

% Attend, tithe, and affirm the literal reading in public while privately doubting it, often after exposure to mainstream science. Open objection would trigger discipline, damaged family relationships, and in some cases loss of church-affiliated employment, so doubt stays private. Exit means losing family, community, and a salvation-framework that structures their whole life; many describe the doubt itself as shameful, which keeps it unspoken even where no immediate external penalty would follow.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, privately_doubting_believers, payer,
    moderate, biographical, identity_locked, national).

% Hold advanced degrees in geology, biology, or related fields and teach or research at colleges and organizations that require signed statements affirming a recent creation and a global flood. Their published work must be framed within those commitments regardless of what the data suggest to them, and several have lost positions when their conclusions drifted. Exit means abandoning a career built inside a small professional world and, typically, their religious community at the same time.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, creationist_institution_scientists, payer,
    moderate, biographical, identity_locked, national).

% Are taught the literal reading as science in schools, homeschool curricula, and church programs chosen by their parents and communities. They cannot opt out and lack standing to object, and they commonly encounter mainstream science only later, at personal epistemic and social cost. Some carry the framework into higher education and experience the collision there.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, creation_science_students, payer,
    powerless, immediate, constrained, local).

% Work in mainline seminaries and universities and argue, within the tradition's own scholarly norms, that Genesis 1-2 is ancient literature making theological rather than chronological claims. Conservative institutions do not platform them, review their work, or admit their arguments into teaching materials; they address the constituency from outside its authorized channels.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, non_literalist_theologians, excluded,
    moderate, biographical, mobile, global).

% Produce the evidence — radiometric dating, genomics, the fossil record — that the literal reading's empirical claims stand against. They are categorically declared mistaken by the arrangement and have no seat in its internal deliberations; their only channel is the public scientific culture the community has walled itself against.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, evolutionary_scientists, excluded,
    organized, generational, mobile, global).

% Study Genesis 1-2's genre, its Ancient Near Eastern literary context, and the history of its interpretation, including the long pre-modern tradition of non-literal day readings. They hold an analytical seat: they collect no revenue and bear no discipline, and they can see the whole structure — text, institutions, and the machinery that binds them.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, hermeneutics_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_narrative__literal_young_earth, creationist_ministry_organizations).
narrative_ontology:fixing_cost_class(genesis_creation_narrative__literal_young_earth, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single authorized interpretive framework that binds the community's scriptural authority to specific empirical commitments: it coordinates membership boundaries, educational curricula, and doctrinal identity around a shared origin account, and it carries the hermeneutic-consistency argument (that a figurative Genesis unravels the authority basis for the community's other doctrines).
% TRANSFER_FUNCTION: Moves assent, loyalty, tuition, and donations from congregation members, students, and their families toward denominational institutions and creationist ministries; moves epistemic and social cost onto doubting members, scientifically trained members, and students.
% ABSENT_VOICES: Non-literalist theologians and evolutionary scientists are structurally excluded from conservative institutional teaching, publication, and review; doubting members are physically present but kept voiceless because open objection triggers discipline. The unanimity of the reading inside these institutions partly reflects who was never admitted to the conversation and what saying otherwise costs those who were.
% DISAPPEARANCE_RATIONALE: If the requirement and its enforcement vanished overnight, conservative Protestant education would reorganize: faith statements would be rewritten, creationist curricula and ministries would lose their warrant and revenue base, ordination and membership boundaries would be renegotiated, and the community would face either schism or rapid doctrinal reconstruction around a different reading of the text.
% FOUNDING_PROBLEM: Preserving the authority of Scripture and the identity boundary of conservative Protestant communities against the perceived corrosive force of evolutionary science and liberal theology — a defense consolidated as an institutional boundary marker during the fundamentalist-modernist controversies and re-armed by the 1961 flood-geology revival.
% FOUNDING_PROBLEM_CORROBORATION: Historians of American fundamentalism, writing from outside the benefiting parties, corroborate the genealogy: the modern institutionalized form dates to the fundamentalist-modernist controversy and the 1961 revival of flood geology, and they attest that the founding problem was the community's perceived defense of scriptural authority. Deconversion narratives and doubting members corroborate from inside that the perceived threat remains live. Evolutionary scientists and biblical scholars corroborate that the empirical content is false and the genre reading mistaken — they attest the problem's framing, not its validity. No neutral party attests the problem as correctly framed; what is corroborated is its social reality, not its epistemic warrant.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__literal_young_earth, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_narrative__literal_young_earth, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__literal_young_earth, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(genesis_creation_narrative__literal_young_earth, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__literal_young_earth, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction (0.62 at interval end) is substantial but bounded: the arrangement delivers real goods — certainty, identity, community, an integrated worldview — to sincerely convinced members while imposing heavy assent costs on doubters, institution-bound scientists, and students. Suppression (0.80) is high and mostly structural: faith statements, membership discipline, employment conditions, curriculum control, and platform exclusion keep non-literalist readings out of authorized channels; an estimated minority share (roughly 40%) is internalized — doubt experienced as spiritual failure — with the split carried as an omega. Theater (0.35) is moderate: the doctrine is functionally load-bearing for the institutions, but a growing share of assent is performative — public affirmation masking private doubt — as the series shows. Accessibility collapse (0.45) and resistance (0.55) reflect that alternatives persist and are reachable (adjacent denominations, evolutionary-creation organizations, deconversion) rather than collapsed as a natural law would collapse them. All three measurement series share one time grid (1961-2025, seven points) so no metric is sampled against another metric's end-state; the trajectories show enforcement machinery maturing and hardening (suppression), movement professionalization raising assent costs (extraction), and private doubt growing under public assent requirements (theater). Suppression is authored as a raw structural property; only extractiveness is scaled, by the engine, through directionality and spatial scope.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the denominational leadership seat the arrangement is faithful guardianship: the boundary protects Scripture's authority and the community's identity, and the reading is the plain sense of the text — a coordination experience. From the sincerely-convinced seat it is nearly pure coordination: the goods are real and the costs are accepted as truth's price. From the privately-doubting and institution-scientist seats the same structure operates as enforced silence with identity-locked exit — an experience much closer to pure extraction. The identity-lock mechanism is relational-institutional fusion: religious identity, family, livelihood, and the salvation-framework are constituted through the community, so exit is self-rupture rather than relocation; if the community tolerated open doubt, those seats' exit options would shift toward constrained and the arrangement's effective costs would fall. The engine derives these divergences from the structural data; the story's claimed type does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Denominational leadership and creationist ministries sit near the beneficiary end: leadership sets and administers the boundary (with its own exit locked by identity), and the ministries collect the concentrated financial gains with mobile operation across markets. Sincerely convinced believers sit near-symmetric with a beneficiary tilt: genuine goods received, real costs carried. Privately doubting believers, institution-bound scientists, and students sit near the target end — they bear the assent costs, and identity lock or dependency removes arbitrage-grade exit, placing them near full-target. The excluded seats (non-literalist theologians, evolutionary scientists) are objects of the boundary-keeping without standing inside the transfer flows. No directionality overrides are used: the beneficiary/victim declarations plus exit options already produce these positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — defending scriptural authority and community identity against evolutionary science and liberal theology — remains live inside the constituency (though its validity is disputed from outside, hence the contested status), so this is not a resolved-mandate case: the arrangement is maintained by an active perceived threat, not by inertia alone. The tangled_rope classification prevents two mislabels. Reading the arrangement as pure coordination would erase the documented asymmetric costs borne by doubters, scientists, and students. Reading it as pure extraction would erase the genuine coordination it performs — a shared origin account, a hermeneutic-consistency argument, and a membership boundary the community experiences as real goods. Keeping both faces on the record lets the per-seat computation show who experiences which face, and the theater series (0.15 to 0.35) tracks the growing performative share without letting theatricality stand in for the structural cost asymmetry, which is what separates this case from an inertial one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the genesis_creation_narrative kernel — what changes structurally if a sibling reading is institutionalized instead?',
    'Not resolvable by data alone: the readings disagree over what the text asserts (chronicle versus framework versus genre). Resolution is a shift in which reading a community institutionalizes, observable as changes in faith-statement language, curricula, and discipline practice.',
    'Under theistic_evolutionary the victim set shifts toward members caught between scientific and religious communities and the enforcement structure thins; under allegorical_ancient_near_east the historical-scientific claim set disappears, enforcement collapses toward academic convention, and the measured costs fall toward coordination-cost levels.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: which reading of the Genesis kernel this constraint instantiates and what sibling readings would change.').

omega_variable(
    coordination_extraction_separability,
    'Does the coordination the arrangement performs — scriptural-authority maintenance, membership boundary, hermeneutic consistency — require the literal chronicle specifically, or is it separable from it?',
    'Compare communities that relocated the authority-maintenance function onto non-literalist readings (evolutionary-creation organizations, allegorical traditions): if authority and cohesion hold without the chronicle requirement, the functions are separable.',
    'If separable, the chronicle requirement is a boundary marker riding on a real coordination function and the measured costs are the enforcement costs of a constructed boundary; if inseparable, part of the cost is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the literal chronicle requirement is structurally necessary to the arrangement''s coordination function.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (faith statements, discipline, employment conditions, curriculum control) or internalized (doubt experienced as sin, fear of apostasy, shame that persists without external penalty)?',
    'Post-exit trajectory: track whether the felt compulsion to assent and the shame about doubt persist after members leave the institutions; deconversion narratives and exit interviews are the data.',
    'If substantially internalized, the effective suppression is higher than the structural machinery alone shows and persists beyond the arrangement''s reach; the structural share would be lower than the roughly 60/40 split authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized split of the arrangement''s suppressive force.').

omega_variable(
    plain_sense_naturality,
    'Is the literal 24-hour-day reading the natural plain sense of Genesis 1-2, as the reading claims, or a historically constructed hermeneutic — given the documented pre-modern tradition of non-literal day interpretations (Origen, Augustine) and the modern reading''s consolidation as a controversy boundary marker?',
    'History of interpretation: establish whether non-literal readings were mainstream before the modern controversy and what social conditions accompanied the literal reading''s consolidation as required doctrine.',
    'If the plain-sense claim fails, the arrangement''s naturality defense collapses and its persistence rests wholly on enforcement and identity — strengthening the extraction-side reading of the metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plain_sense_naturality, conceptual, 'Whether the literalist reading is a natural default or a constructed hermeneutic.').

omega_variable(
    institutional_assent_gap,
    'What fraction of public assent to the literal reading inside conservative institutions is private doubt, and how does that gap vary by institution, generation, and exposure to mainstream science?',
    'Anonymous belief surveys within creationist institutions and congregations compared against public affirmations; longitudinal tracking of students entering mainstream higher education.',
    'A wide assent gap raises the true theater ratio above the authored 0.35 and indicates the arrangement maintains public unanimity at growing private cost — accumulation in a form the public record undercounts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_assent_gap, empirical, 'The gap between public assent and private belief inside the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__literal_young_earth, 1961, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gcn_literal_young_earth_tr_t1961, genesis_creation_narrative__literal_young_earth, theater_ratio, 1961, 0.15).
narrative_ontology:measurement_basis(gcn_literal_young_earth_tr_t1961, observed).
narrative_ontology:measurement(gcn_literal_young_earth_tr_t1970, genesis_creation_narrative__literal_young_earth, theater_ratio, 1970, 0.18).
narrative_ontology:measurement_basis(gcn_literal_young_earth_tr_t1970, observed).
narrative_ontology:measurement(gcn_literal_young_earth_tr_t1980, genesis_creation_narrative__literal_young_earth, theater_ratio, 1980, 0.22).
narrative_ontology:measurement_basis(gcn_literal_young_earth_tr_t1980, observed).
narrative_ontology:measurement(gcn_literal_young_earth_tr_t1990, genesis_creation_narrative__literal_young_earth, theater_ratio, 1990, 0.26).
narrative_ontology:measurement_basis(gcn_literal_young_earth_tr_t1990, observed).
narrative_ontology:measurement(gcn_literal_young_earth_tr_t2000, genesis_creation_narrative__literal_young_earth, theater_ratio, 2000, 0.3).
narrative_ontology:measurement_basis(gcn_literal_young_earth_tr_t2000, observed).
narrative_ontology:measurement(gcn_literal_young_earth_tr_t2010, genesis_creation_narrative__literal_young_earth, theater_ratio, 2010, 0.33).
narrative_ontology:measurement_basis(gcn_literal_young_earth_tr_t2010, observed).
narrative_ontology:measurement(gcn_literal_young_earth_tr_t2025, genesis_creation_narrative__literal_young_earth, theater_ratio, 2025, 0.35).
narrative_ontology:measurement_basis(gcn_literal_young_earth_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(gcn_literal_young_earth_be_t1961, genesis_creation_narrative__literal_young_earth, base_extractiveness, 1961, 0.45).
narrative_ontology:measurement_basis(gcn_literal_young_earth_be_t1961, observed).
narrative_ontology:measurement(gcn_literal_young_earth_be_t1970, genesis_creation_narrative__literal_young_earth, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement_basis(gcn_literal_young_earth_be_t1970, observed).
narrative_ontology:measurement(gcn_literal_young_earth_be_t1980, genesis_creation_narrative__literal_young_earth, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement_basis(gcn_literal_young_earth_be_t1980, observed).
narrative_ontology:measurement(gcn_literal_young_earth_be_t1990, genesis_creation_narrative__literal_young_earth, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement_basis(gcn_literal_young_earth_be_t1990, observed).
narrative_ontology:measurement(gcn_literal_young_earth_be_t2000, genesis_creation_narrative__literal_young_earth, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement_basis(gcn_literal_young_earth_be_t2000, observed).
narrative_ontology:measurement(gcn_literal_young_earth_be_t2010, genesis_creation_narrative__literal_young_earth, base_extractiveness, 2010, 0.61).
narrative_ontology:measurement_basis(gcn_literal_young_earth_be_t2010, observed).
narrative_ontology:measurement(gcn_literal_young_earth_be_t2025, genesis_creation_narrative__literal_young_earth, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement_basis(gcn_literal_young_earth_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(gcn_literal_young_earth_su_t1961, genesis_creation_narrative__literal_young_earth, suppression_requirement, 1961, 0.5).
narrative_ontology:measurement_basis(gcn_literal_young_earth_su_t1961, observed).
narrative_ontology:measurement(gcn_literal_young_earth_su_t1970, genesis_creation_narrative__literal_young_earth, suppression_requirement, 1970, 0.58).
narrative_ontology:measurement_basis(gcn_literal_young_earth_su_t1970, observed).
narrative_ontology:measurement(gcn_literal_young_earth_su_t1980, genesis_creation_narrative__literal_young_earth, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement_basis(gcn_literal_young_earth_su_t1980, observed).
narrative_ontology:measurement(gcn_literal_young_earth_su_t1990, genesis_creation_narrative__literal_young_earth, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement_basis(gcn_literal_young_earth_su_t1990, observed).
narrative_ontology:measurement(gcn_literal_young_earth_su_t2000, genesis_creation_narrative__literal_young_earth, suppression_requirement, 2000, 0.74).
narrative_ontology:measurement_basis(gcn_literal_young_earth_su_t2000, observed).
narrative_ontology:measurement(gcn_literal_young_earth_su_t2010, genesis_creation_narrative__literal_young_earth, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement_basis(gcn_literal_young_earth_su_t2010, observed).
narrative_ontology:measurement(gcn_literal_young_earth_su_t2025, genesis_creation_narrative__literal_young_earth, suppression_requirement, 2025, 0.8).
narrative_ontology:measurement_basis(gcn_literal_young_earth_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__literal_young_earth, identity_coordination).
narrative_ontology:affects_constraint(genesis_creation_narrative__literal_young_earth, genesis_creation_narrative__theistic_evolutionary).
narrative_ontology:affects_constraint(genesis_creation_narrative__literal_young_earth, genesis_creation_narrative__allegorical_ancient_near_east).

% DUAL FORMULATION NOTE:
% The colloquial label 'what Genesis 1-2 says about creation' covers three structurally distinct arrangements and is decomposed per the ε-invariance principle into three readings of the genesis_creation_narrative kernel: this file (literal_young_earth — enforced inerrant-chronicle assent; ε reflects enforced assent with high suppression and concentrated institutional gains), genesis_creation_narrative__theistic_evolutionary (a compatibility framework whose costs fall mainly on members straddling the scientific and religious communities), and genesis_creation_narrative__allegorical_ancient_near_east (a genre claim carried largely by academic convention, minimal enforcement, low ε). Each story has its own ε, beneficiaries, and victims; the ε values differ because the arrangements differ, not because one constraint is measured with different observables. This reading is the upstream member — the one with institutional enforcement machinery — and structurally influences the siblings' operating environment; the edges are recorded here and in each sibling's file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
