% ============================================================================
% CONSTRAINT STORY: genesis_creation_cosmology__young_earth_literal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_cosmology__young_earth_literal, []).

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
 *   constraint_id: genesis_creation_cosmology__young_earth_literal
 *   human_readable: Young-Earth Literalist Reading of Genesis Creation
 *   domain: religion/philosophy_of_science
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the genesis_creation_cosmology
 *   kernel: young_earth_literal, the claim that Genesis narrates six literal
 *   24-hour days roughly 6000-10000 years ago, held with enforcement weight
 *   inside the denominations, schools, and media institutions that govern by
 *   it. The constraint under description is the enforced interpretive
 *   arrangement, not the text of Genesis and not the scientific account it
 *   rejects. ASSUMPTIONS: (1) the interval anchors t=0 at 1859 (publication
 *   of On the Origin of Species), when the arrangement shifted from cultural
 *   default to defended position; (2) the institutional form analyzed is the
 *   Anglophone literalist complex (denominations, parochial and homeschool
 *   systems, creation-media organizations) with global missionary extension;
 *   (3) epsilon is authored over this enforced arrangement as it actually
 *   operates on the parties it binds, per the fixed kernel-reading referent
 *   rule - the readings themselves are not averaged and no sibling reading
 *   appears in this file's structural data. CONSTRAINT FAMILY: the colloquial
 *   label 'what Genesis says about creation' decomposes into three
 *   structurally distinct constraints - this reading (high epsilon:
 *   enforcement-bearing, victim-generating), theistic_evolution (moderate-low
 *   epsilon: non-literal reading without cosmological enforcement), and
 *   literary_framework (near-zero epsilon: ANE schema read as non-assertive
 *   scaffolding, no victims). They share the kernel text and diverge on
 *   epsilon, victim sets, and enforcement load; the links are recorded in
 *   network.affects_constraints. The claimed type (tangled_rope) and the
 *   authored metrics are independent facts: I believe the structure genuinely
 *   coordinates inerrantist community life AND asymmetrically extracts
 *   epistemic costs from members, students, and dissenters; the metrics
 *   describe that operation without being tuned to any predicted engine
 *   output.
 *
 * KEY AGENTS:
 *   - young_earth_apologetics_ministries: agenda-setter and principal collector ([institutional]/[identity_locked]) - runs enforcement, accrues revenue and authority, institutionally fused with the message
 *   - literalist_denominational_bodies: agenda-setter ([institutional]/[constrained]) - holds ordination standards, retains formal amendment power at schism cost
 *   - creation_curriculum_publishers: beneficiary ([organized]/[mobile]) - collects curriculum sales without running enforcement
 *   - rank_file_literalist_believers: dual-positioned beneficiary-payer ([moderate]/[identity_locked]) - receives identity and certainty, pays tithes and lifelong evidential-avoidance costs
 *   - literalist_students: primary payer ([powerless]/[trapped]) - absorb the constraint's science content before any capacity to evaluate it
 *   - doubting_congregants: primary payer ([moderate]/[identity_locked]) - bear maximal felt extraction with minimal formal power
 *   - science_teachers_in_literalist_schools: payer ([moderate]/[constrained]) - trade professional integrity for employment and community standing
 *   - scientific_consensus_community: nominal payer ([institutional]/[arbitrage]) - declared victim whose arbitrage-grade exit dampens effective extraction toward zero
 *   - theistic_evolution_advocates: excluded ([organized]/[mobile]) - barred from the platforms their admission would dissolve
 *   - philosophy_of_science_observers: analytical observer - maps the authority ordering without collecting or paying
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_cosmology__young_earth_literal, 0.67).
domain_priors:suppression_score(genesis_creation_cosmology__young_earth_literal, 0.56).
domain_priors:theater_ratio(genesis_creation_cosmology__young_earth_literal, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_cosmology__young_earth_literal, extractiveness, 0.67).
narrative_ontology:constraint_metric(genesis_creation_cosmology__young_earth_literal, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(genesis_creation_cosmology__young_earth_literal, theater_ratio, 0.54).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_cosmology__young_earth_literal, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(genesis_creation_cosmology__young_earth_literal, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_cosmology__young_earth_literal, tangled_rope).
narrative_ontology:human_readable(genesis_creation_cosmology__young_earth_literal, "Young-Earth Literalist Reading of Genesis Creation").
narrative_ontology:topic_domain(genesis_creation_cosmology__young_earth_literal, "religion/philosophy_of_science").

domain_priors:requires_active_enforcement(genesis_creation_cosmology__young_earth_literal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_cosmology__young_earth_literal, 'b814a46d-4023-441d-bf75-1378bb7bdbcb').
narrative_ontology:cs_kernel_codification('b814a46d-4023-441d-bf75-1378bb7bdbcb', fixed_text).
narrative_ontology:cs_authority_grounding('b814a46d-4023-441d-bf75-1378bb7bdbcb', lineage).
narrative_ontology:cs_interpretation_layer_present('b814a46d-4023-441d-bf75-1378bb7bdbcb').
narrative_ontology:cs_reading_relation('b814a46d-4023-441d-bf75-1378bb7bdbcb', genesis_creation_cosmology__theistic_evolution, forecloses).
narrative_ontology:cs_reading_relation('b814a46d-4023-441d-bf75-1378bb7bdbcb', genesis_creation_cosmology__literary_framework, forecloses).
narrative_ontology:cs_axiom('b814a46d-4023-441d-bf75-1378bb7bdbcb', foundational, genesis_narrates_literal_recent_creation).
narrative_ontology:cs_axiom_status(genesis_narrates_literal_recent_creation, holdable).
narrative_ontology:cs_axiom_grounding('b814a46d-4023-441d-bf75-1378bb7bdbcb', genesis_narrates_literal_recent_creation, empirically_contingent).
narrative_ontology:cs_axiom('b814a46d-4023-441d-bf75-1378bb7bdbcb', foundational, plain_sense_exegesis_authoritative_over_empirical_consensus).
narrative_ontology:cs_axiom_status(plain_sense_exegesis_authoritative_over_empirical_consensus, holdable).
narrative_ontology:cs_axiom_grounding('b814a46d-4023-441d-bf75-1378bb7bdbcb', plain_sense_exegesis_authoritative_over_empirical_consensus, deontological).
narrative_ontology:cs_reference_frame('b814a46d-4023-441d-bf75-1378bb7bdbcb', six_day_fiat_creation_recent_chronology).
narrative_ontology:cs_drift_state('b814a46d-4023-441d-bf75-1378bb7bdbcb', contemporary_deep_time_consensus_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b814a46d-4023-441d-bf75-1378bb7bdbcb', '').
narrative_ontology:cs_kernel_id(genesis_creation_cosmology__young_earth_literal, genesis_creation_cosmology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__young_earth_literal, young_earth_apologetics_ministries).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__young_earth_literal, literalist_denominational_bodies).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__young_earth_literal, creation_curriculum_publishers).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__young_earth_literal, rank_file_literalist_believers).
narrative_ontology:constraint_victim(genesis_creation_cosmology__young_earth_literal, rank_file_literalist_believers).
narrative_ontology:constraint_victim(genesis_creation_cosmology__young_earth_literal, literalist_students).
narrative_ontology:constraint_victim(genesis_creation_cosmology__young_earth_literal, doubting_congregants).
narrative_ontology:constraint_victim(genesis_creation_cosmology__young_earth_literal, science_teachers_in_literalist_schools).
narrative_ontology:constraint_victim(genesis_creation_cosmology__young_earth_literal, scientific_consensus_community).
narrative_ontology:constraint_vindicates(genesis_creation_cosmology__young_earth_literal, biblical_inerrancy_doctrine).
narrative_ontology:constraint_vindicates(genesis_creation_cosmology__young_earth_literal, plain_sense_hermeneutic_supremacy).
narrative_ontology:constraint_vindicates(genesis_creation_cosmology__young_earth_literal, flood_geology_model).
narrative_ontology:constraint_vindicates(genesis_creation_cosmology__young_earth_literal, fixity_of_created_kinds).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate museums, media networks, publishing houses, and speaker bureaus devoted to defending a recent six-day creation. Set message discipline through publication vetting and speaker credentialing, and enforce it by defunding or deplatforming affiliates who concede deep time. Revenue, staffing, and institutional purpose all flow from the kernel staying fixed; pivoting to a non-literal message would dissolve the donor base and the organization's reason for existing.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, young_earth_apologetics_ministries, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(genesis_creation_cosmology__young_earth_literal, young_earth_apologetics_ministries, beneficiary).

% Hold ordination standards and confessional clauses requiring a literal Adam, a historical fall, and a recent creation. Enforce through credential review and church discipline. They collect compliance and doctrinal cohesion, and they retain formal power to amend the standard, but amendment carries predictable schism cost, so revision is priced far above its apparent benefit.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, literalist_denominational_bodies, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(genesis_creation_cosmology__young_earth_literal, literalist_denominational_bodies, beneficiary).

% Sell textbooks and video courses presenting young-earth geology, biology, and astronomy as settled science to schools and homeschool networks. They collect sales from the constraint without administering it, and could in principle repurpose their production capacity for other educational content.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, creation_curriculum_publishers, beneficiary,
    organized, biographical, mobile, national).

% Receive a complete, coherent origin narrative, membership identity, and assurance that the whole of scripture stands or falls together. They pay tithes that fund the apparatus, perform ongoing evidential avoidance (steering around geology documentaries, dating methods, genomic arguments), and manage dissonance privately. Leaving would mean reconstructing community, family relationships, and self-concept simultaneously.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, rank_file_literalist_believers, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(genesis_creation_cosmology__young_earth_literal, rank_file_literalist_believers, payer).

% Are taught young-earth chronology and anti-evolution material as science by parents, schools, and churches, before possessing any independent basis to evaluate it. Exit is deferred to adulthood and priced in family rupture; many discover the scale of the discrepancy only upon reaching university science coursework.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, literalist_students, payer,
    powerless, immediate, trapped, national).

% Privately conclude that the evidence contradicts the taught chronology but continue attending, giving, and teaching their children the line. Voicing doubt risks losing spouse, parents, friendship networks, and, in their received framework, eternal security itself, so silence is the rational strategy and the constraint travels with them wherever they go.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, doubting_congregants, payer,
    moderate, biographical, identity_locked, local).

% Must teach from board-approved materials that assert a recent creation, concealing their own training where it disagrees. Compliance is a condition of employment and standing in the community; leaving means forfeiting position, references, and often a whole social world built around the school.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, science_teachers_in_literalist_schools, payer,
    moderate, biographical, constrained, national).

% Finds its core findings declared false inside literalist institutions and its members periodically caricatured in creationist media. It pays in misrepresented work, blocked dialogue with large publics, and episodic fights over school standards, but it holds near-total exit insulation: the enterprise runs on redundancy and indifference, and almost nothing it does depends on literalist assent.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, scientific_consensus_community, payer,
    institutional, civilizational, arbitrage, global).

% Scholars and organizations arguing that evolutionary cosmology and orthodox Christian faith are compatible. They are barred from literalist pulpits, platforms, and publisher lists precisely because their position, if admitted, dissolves the enforcement object; their arguments circulate only outside the walls or self-censored within them.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, theistic_evolution_advocates, excluded,
    organized, generational, mobile, global).

% Historians of science and religion and philosophers of science who map how the arrangement orders authority between text and evidence, who bears its costs, and how its enforcement machinery evolved. They collect no rents and pay none.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, philosophy_of_science_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_cosmology__young_earth_literal, young_earth_apologetics_ministries).
narrative_ontology:fixing_cost_class(genesis_creation_cosmology__young_earth_literal, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves an identity-and-coherence problem for communities committed to scriptural inerrancy: it supplies a single shared origin narrative, contains the slippery-slope worry that conceding Genesis 1 concedes the resurrection, draws a crisp membership boundary, and lets members extend trust to teachers and leaders without independent verification.
% TRANSFER_FUNCTION: Moves epistemic deference and curriculum control from members and students toward denominational authorities and creation-media institutions; moves tithe and media revenue toward the apologetics apparatus; moves certainty, identity, and community belonging back to members.
% ABSENT_VOICES: Working geologists, astronomers, and geneticists are absent from every body that sets the curriculum inside literalist institutions; doubting congregants are present but self-silenced by sanction expectations; theistic-evolution scholars are formally excluded from pulpits and platforms. All three speak from outside the walls, or not at all.
% DISAPPEARANCE_RATIONALE: If the enforced requirement vanished overnight, literalist denominations would fracture or formally revise their hermeneutic, the creation-media economy would collapse within a decade, homeschool and parochial science curricula would converge on mainstream material, and millions of members would face an unplanned reconstruction of belief, community, and identity. The text of Genesis would remain; the arrangement built around one reading of it would not survive intact.
% FOUNDING_PROBLEM: Protect the authority and coherence of scripture against the emerging deep-time and evolutionary sciences: first as a diffuse post-Darwinian anxiety in the late nineteenth century, then as organized opposition (The Fundamentals, the anti-evolution crusades, and after 1961 the flood-geology reconstruction movement).
% FOUNDING_PROBLEM_CORROBORATION: Historians of American religion (notably the monograph literature on the creationist movement) corroborate from outside the benefiting parties that the founding problem was a real post-Darwinian legitimacy crisis rather than a retrospective invention; national scientific academies independently attest that the evidential conflict remains open by continuing to issue statements on it. What no outside source corroborates is the young-earth resolution of the problem: every attestation of the problem's persistence comes paired with rejection of the arrangement's answer, and the arrangement's own beneficiaries are the only parties who attest that its answer succeeds.
narrative_ontology:disappearance_verdict(genesis_creation_cosmology__young_earth_literal, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_cosmology__young_earth_literal, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_cosmology__young_earth_literal, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(genesis_creation_cosmology__young_earth_literal, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_cosmology__young_earth_literal, 0.67, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_cosmology__young_earth_literal_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(genesis_creation_cosmology__young_earth_literal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(genesis_creation_cosmology__young_earth_literal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.67 at interval end: the arrangement delivers genuine goods (coherence, identity, trusted authority) while concentrating epistemic costs - distorted science education, dissonance management, sanction exposure - on members least equipped to refuse them. Suppression 0.56 is a raw structural property (unscaled): enforcement is real but bounded - curriculum approval, credential review, social sanction - and stops at the boundary of institutions that cannot reach the scientific enterprise at large. Theater ratio 0.54 crosses the Goodhart line: a majority of contemporary apologetic output (apparent-age arguments, accelerated-decay models, museum dioramas answering objections decades stale) functions to reassure insiders rather than engage outside evidence, a signature of the flood-geology reconstruction's accumulating epicycles. Accessibility_collapse 0.42: alternatives do not fully collapse - the scientific literature, rival readings, and exit itself all remain reachable - though inside the framework, granting textual authority collapses rival readings completely, hence the forecloses relations below. Resistance 0.58: sustained organized counter-pressure from theistic-evolution scholarship, mainline denominations, and science communication, plus pervasive quiet internal doubt. The measurement series run on one shared eight-point grid so every metric is authored at every examined time point. The series smooth a real historical wave (anti-evolution surge circa 1920s, retreat through the 1930s-50s, flood-geology revival after 1961, platform maturation after 1990, mild softening as cultural enforcement power wanes post-2010); the grid resolves that wave coarsely and the suppression_requirement series is authored because the story specifically traces enforcement-capacity build-up and partial attrition, not merely extraction shift.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute wildly different constraints from identical structural data. From the agenda-setter seats the arrangement is a treasured inheritance they administer: coordination they built, defend, and fund. From the doubting-congregant seat - same denomination, same doctrines, modest formal power - the identical structure operates as totalizing extraction with identity-locked exit. The scientific_consensus seat is the sharpest divergence: declared a victim by the story's structural data, yet holding arbitrage-grade exit, it should compute near-zero felt extraction - the constraint extracts almost nothing it can collect from that seat. Rank-and-file believers split internally: net beneficiaries on the identity ledger, net payers on the epistemic ledger, which is precisely the dual-positioned profile the engine should register rather than average away.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (ministries, denominations, publishers, believers) drive those seats toward the subsidy end; victim declarations (students, doubters, teachers, consensus, and believers' secondary position) drive them toward the target end. Trapped and identity_locked exits amplify targets toward full-target d (students, doubters); arbitrage exit damps the consensus seat hard regardless of its victim declaration. No directionality_overrides are authored, deliberately: overrides key on the power atom, and this story's atoms are heterogeneously occupied - the moderate atom contains both net-subsidized believers and maximally extracted doubters; the institutional atom contains both the apparatus that profits and the consensus that barely pays - so any atom-level override would smear corrections across structurally opposite seats. The derivation chain from declarations plus exit options already produces the correct differentiation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (defending scriptural authority against deep-time science) is attested live by both sides for opposite reasons, so the R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges: coherent, no zombie flag, no capture signal. The watch item is theater_ratio crossing 0.5 in the final third of the interval: the arrangement increasingly performs defense (museums, debate circuits, rebuttal media) rather than maintaining the coordination it originally supplied, which is mandatrophy pressure building inside a still-functioning constraint rather than mandatrophy achieved. Classification prevents mislabeling in both directions: reading the arrangement as pure snare erases the real identity and coherence goods members voluntarily consume; reading it as pure rope erases who pays for those goods and who cannot refuse. Tangled rope names both halves; the per-seat computation distributes them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This story instantiates only the young_earth_literal reading of the genesis_creation_cosmology kernel; what changes structurally if a sibling reading governs the same communities?',
    'Author the sibling stories (theistic_evolution, literary_framework) over the same population and compare victim sets, suppression loads, and epsilon; convergence against this file validates the decomposition.',
    'Under literary_framework governance the victim set empties of students and doubters (no cosmological assertion is enforced), suppression collapses toward voluntary-identity levels, and epsilon approaches zero; under theistic_evolution governance scientific_consensus leaves the victim set entirely and enforcement machinery atrophies to confessional preference.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: which reading governs determines the constraint''s entire victim and enforcement architecture.').

omega_variable(
    scientific_consensus_victim_materiality,
    'Is scientific_consensus_community a materially burdened victim of this constraint, or structurally insulated to the point that its victim status is nominal?',
    'Weigh realized costs (misrepresentation incidence in creationist media, episodic standards-policy losses, forgone dialogue with literalist publics) against the consensus enterprise''s total throughput and redundancy; if the ratio is negligible, demote the seat from victims.',
    'Demotion shrinks the victim set to in-community payers and tightens the reading toward intra-community extraction; retention supports characterizing the constraint as generating genuine cross-community conflict rather than purely internal cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scientific_consensus_victim_materiality, empirical, 'Whether the declared victim with arbitrage exit actually pays anything the constraint can collect.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression binding doubting members structural (credential review, curriculum control, sanction expectation) or internalized (identity fusion, fear of apostasy consequences, trained evidential avoidance)?',
    'Post-exit trajectory study of leavers: if self-censorship, evidential avoidance, and authority deference persist long after enforcement reach ends, the internalized share is substantial.',
    'If largely internalized, effective suppression exceeds the structural measure, persists beyond institutional boundaries, and raises identity_locked ratings across all member seats; the constraint would partly travel with its targets after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in the member seats.').

omega_variable(
    conviction_vs_rent_persistence,
    'Does the arrangement persist primarily through sincere participant conviction or through the apparatus''s rent dependence on kernel stability?',
    'Track institutional behavior across revenue shocks decoupled from the kernel (donor-base shifts, platform changes, leadership turnover): rent-driven institutions defend the revenue line under shock; conviction-driven ones absorb doctrinal revision at financial cost.',
    'Rent dominance pushes the agenda-setter seats toward captured-extraction and sharpens the snare flavor; conviction dominance supports genuine tangled_rope with voluntary identity coordination carrying real coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conviction_vs_rent_persistence, empirical, 'Persistence driver: belief versus institutional rent dependence.').

omega_variable(
    epsilon_referent_boundary,
    'Do the authored costs all belong to THIS constraint, or does part of the measured burden (culture-war expenditure, public polarization) belong to a separable adjacent constraint over public science-education standards?',
    'Decomposition test: author the standards-conflict as its own story and check whether its epsilon is invariant across observables; costs that track the political fight rather than the interpretive arrangement migrate to the sibling story.',
    'Clean separation leaves this file''s epsilon measuring only the enforced interpretive arrangement; failure to separate inflates epsilon with costs the arrangement merely touches, overstating extraction attributable to textual authority itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epsilon_referent_boundary, conceptual, 'Boundary of the epsilon referent against an adjacent culture-war constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_cosmology__young_earth_literal, 0, 165).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(genesis_yec_tr_t0, genesis_creation_cosmology__young_earth_literal, theater_ratio, 0, 0.15).
narrative_ontology:measurement(genesis_yec_tr_t25, genesis_creation_cosmology__young_earth_literal, theater_ratio, 25, 0.22).
narrative_ontology:measurement(genesis_yec_tr_t50, genesis_creation_cosmology__young_earth_literal, theater_ratio, 50, 0.28).
narrative_ontology:measurement(genesis_yec_tr_t75, genesis_creation_cosmology__young_earth_literal, theater_ratio, 75, 0.26).
narrative_ontology:measurement(genesis_yec_tr_t100, genesis_creation_cosmology__young_earth_literal, theater_ratio, 100, 0.33).
narrative_ontology:measurement(genesis_yec_tr_t125, genesis_creation_cosmology__young_earth_literal, theater_ratio, 125, 0.44).
narrative_ontology:measurement(genesis_yec_tr_t150, genesis_creation_cosmology__young_earth_literal, theater_ratio, 150, 0.52).
narrative_ontology:measurement(genesis_yec_tr_t165, genesis_creation_cosmology__young_earth_literal, theater_ratio, 165, 0.54).

% Extraction over time
narrative_ontology:measurement(genesis_yec_be_t0, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(genesis_yec_be_t25, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(genesis_yec_be_t50, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(genesis_yec_be_t75, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 75, 0.48).
narrative_ontology:measurement(genesis_yec_be_t100, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 100, 0.52).
narrative_ontology:measurement(genesis_yec_be_t125, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 125, 0.62).
narrative_ontology:measurement(genesis_yec_be_t150, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 150, 0.68).
narrative_ontology:measurement(genesis_yec_be_t165, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 165, 0.67).

% Suppression requirement over time
narrative_ontology:measurement(genesis_yec_su_t0, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(genesis_yec_su_t25, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 25, 0.38).
narrative_ontology:measurement(genesis_yec_su_t50, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(genesis_yec_su_t75, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 75, 0.48).
narrative_ontology:measurement(genesis_yec_su_t100, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 100, 0.55).
narrative_ontology:measurement(genesis_yec_su_t125, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 125, 0.62).
narrative_ontology:measurement(genesis_yec_su_t150, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 150, 0.58).
narrative_ontology:measurement(genesis_yec_su_t165, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 165, 0.56).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=165
narrative_ontology:measurement(genesis_yec_grid_01, genesis_creation_cosmology__young_earth_literal, accessibility_collapse(class), 0, 0.2).
narrative_ontology:measurement(genesis_yec_grid_02, genesis_creation_cosmology__young_earth_literal, accessibility_collapse(class), 165, 0.45).
narrative_ontology:measurement(genesis_yec_grid_03, genesis_creation_cosmology__young_earth_literal, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(genesis_yec_grid_04, genesis_creation_cosmology__young_earth_literal, accessibility_collapse(individual), 165, 0.6).
narrative_ontology:measurement(genesis_yec_grid_05, genesis_creation_cosmology__young_earth_literal, accessibility_collapse(organizational), 0, 0.3).
narrative_ontology:measurement(genesis_yec_grid_06, genesis_creation_cosmology__young_earth_literal, accessibility_collapse(organizational), 165, 0.7).
narrative_ontology:measurement(genesis_yec_grid_07, genesis_creation_cosmology__young_earth_literal, accessibility_collapse(structural), 0, 0.1).
narrative_ontology:measurement(genesis_yec_grid_08, genesis_creation_cosmology__young_earth_literal, accessibility_collapse(structural), 165, 0.15).
narrative_ontology:measurement(genesis_yec_grid_09, genesis_creation_cosmology__young_earth_literal, resistance(class), 0, 0.15).
narrative_ontology:measurement(genesis_yec_grid_10, genesis_creation_cosmology__young_earth_literal, resistance(class), 165, 0.55).
narrative_ontology:measurement(genesis_yec_grid_11, genesis_creation_cosmology__young_earth_literal, resistance(individual), 0, 0.1).
narrative_ontology:measurement(genesis_yec_grid_12, genesis_creation_cosmology__young_earth_literal, resistance(individual), 165, 0.45).
narrative_ontology:measurement(genesis_yec_grid_13, genesis_creation_cosmology__young_earth_literal, resistance(organizational), 0, 0.2).
narrative_ontology:measurement(genesis_yec_grid_14, genesis_creation_cosmology__young_earth_literal, resistance(organizational), 165, 0.5).
narrative_ontology:measurement(genesis_yec_grid_15, genesis_creation_cosmology__young_earth_literal, resistance(structural), 0, 0.15).
narrative_ontology:measurement(genesis_yec_grid_16, genesis_creation_cosmology__young_earth_literal, resistance(structural), 165, 0.6).
narrative_ontology:measurement(genesis_yec_grid_17, genesis_creation_cosmology__young_earth_literal, stakes_inflation(class), 0, 0.2).
narrative_ontology:measurement(genesis_yec_grid_18, genesis_creation_cosmology__young_earth_literal, stakes_inflation(class), 165, 0.4).
narrative_ontology:measurement(genesis_yec_grid_19, genesis_creation_cosmology__young_earth_literal, stakes_inflation(individual), 0, 0.35).
narrative_ontology:measurement(genesis_yec_grid_20, genesis_creation_cosmology__young_earth_literal, stakes_inflation(individual), 165, 0.6).
narrative_ontology:measurement(genesis_yec_grid_21, genesis_creation_cosmology__young_earth_literal, stakes_inflation(organizational), 0, 0.3).
narrative_ontology:measurement(genesis_yec_grid_22, genesis_creation_cosmology__young_earth_literal, stakes_inflation(organizational), 165, 0.65).
narrative_ontology:measurement(genesis_yec_grid_23, genesis_creation_cosmology__young_earth_literal, stakes_inflation(structural), 0, 0.05).
narrative_ontology:measurement(genesis_yec_grid_24, genesis_creation_cosmology__young_earth_literal, stakes_inflation(structural), 165, 0.1).
narrative_ontology:measurement(genesis_yec_grid_25, genesis_creation_cosmology__young_earth_literal, suppression(class), 0, 0.25).
narrative_ontology:measurement(genesis_yec_grid_26, genesis_creation_cosmology__young_earth_literal, suppression(class), 165, 0.45).
narrative_ontology:measurement(genesis_yec_grid_27, genesis_creation_cosmology__young_earth_literal, suppression(individual), 0, 0.3).
narrative_ontology:measurement(genesis_yec_grid_28, genesis_creation_cosmology__young_earth_literal, suppression(individual), 165, 0.55).
narrative_ontology:measurement(genesis_yec_grid_29, genesis_creation_cosmology__young_earth_literal, suppression(organizational), 0, 0.35).
narrative_ontology:measurement(genesis_yec_grid_30, genesis_creation_cosmology__young_earth_literal, suppression(organizational), 165, 0.72).
narrative_ontology:measurement(genesis_yec_grid_31, genesis_creation_cosmology__young_earth_literal, suppression(structural), 0, 0.05).
narrative_ontology:measurement(genesis_yec_grid_32, genesis_creation_cosmology__young_earth_literal, suppression(structural), 165, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_cosmology__young_earth_literal, identity_coordination).
narrative_ontology:affects_constraint(genesis_creation_cosmology__young_earth_literal, genesis_creation_cosmology__theistic_evolution).
narrative_ontology:affects_constraint(genesis_creation_cosmology__young_earth_literal, genesis_creation_cosmology__literary_framework).

% DUAL FORMULATION NOTE:
% The colloquial label 'what Genesis says about creation' decomposes, per the epsilon-invariance principle, into three constraint stories sharing one kernel text: young_earth_literal (this file - enforced empirical claim bound to textual authority; high epsilon, victims include students, doubters, and scientific consensus), theistic_evolution (non-literal reading compatible with evolutionary cosmology; moderate-low epsilon, no cosmological enforcement), and literary_framework (ANE schema as non-assertive scaffold; near-zero epsilon, no victim set). The readings are linked as a family because the upstream claim - that the text carries authoritative content at all - is cited by each downstream reading as its warrant, while the enforcement structure unique to this reading is what generates its elevated epsilon. No single story can hold all three: they assert incompatible propositional contents of the same text, which is recorded structurally in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
