% ============================================================================
% CONSTRAINT STORY: anthropological_record__creationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Creationist Reading of the Anthropological Record (Enforced Origin Account)
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   Within religious communities that adopt the creationist reading, the
 *   fossil, archaeological, and genetic record is taught to reveal divine
 *   creative episode(s) compatible with a scriptural timeline or with
 *   designed complexity. The arrangement does real coordinating work — a
 *   shared origin account binds education, worship, charity, and identity
 *   across generations — while simultaneously suppressing the materialist
 *   timeline inside its institutions and stripping credentialed science of
 *   adjudicative authority over origins. Enforcement runs through curriculum
 *   control, parochial schooling, doctrinal discipline, and social sanction
 *   rather than state power, especially after the legal defeats of the 1980s
 *   pushed the apparatus into parallel institutions. Receipts concentrate:
 *   tithes, tuition, and deference flow to denominational structures, with a
 *   growing commercial share to creation-science enterprises. Costs
 *   concentrate on those with the least exit: students screened from
 *   alternatives, doubters whose questioning strains their entire social
 *   world, and trained scientists trading professional credibility against
 *   belonging. The epsilon referent is this standing arrangement — the
 *   enforced reading-regime as it operates — not any endorsed alternative.
 *   KEY AGENTS (by structural relationship): - denominational_authorities:
 *   agenda-setter and primary receipt seat (institutional/identity_locked) —
 *   administers doctrine, collects tithes and tuition -
 *   creation_science_enterprises: commercial beneficiary (organized/mobile) —
 *   monetizes the teaching via museums, curricula, media -
 *   rank_and_file_believers: dual-positioned beneficiary-payer
 *   (moderate/identity_locked) — receives meaning and mutual aid, pays
 *   epistemic and exit costs - doubting_youth_and_students: primary payer
 *   (powerless/constrained) — bears the screening of alternatives and the
 *   cost of later discovery - dissenting_scientists_in_faith_communities:
 *   payer (moderate/constrained) — professional credibility traded against
 *   communal belonging - naturalist_scientific_community: excluded party
 *   (institutional/mobile) — barred from adjudication inside the governed
 *   communities - courts_and_school_boards: observer
 *   (institutional/analytical) — sets the public-classroom boundary
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__creationist_reading, 0.63).
domain_priors:suppression_score(anthropological_record__creationist_reading, 0.68).
domain_priors:theater_ratio(anthropological_record__creationist_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__creationist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__creationist_reading, "Creationist Reading of the Anthropological Record (Enforced Origin Account)").
narrative_ontology:topic_domain(anthropological_record__creationist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__creationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__creationist_reading, '05171279-6792-4701-95a2-74550fcb59fc').
narrative_ontology:cs_kernel_codification('05171279-6792-4701-95a2-74550fcb59fc', fixed_text).
narrative_ontology:cs_authority_grounding('05171279-6792-4701-95a2-74550fcb59fc', lineage).
narrative_ontology:cs_interpretation_layer_present('05171279-6792-4701-95a2-74550fcb59fc').
narrative_ontology:cs_reading_relation('05171279-6792-4701-95a2-74550fcb59fc', anthropological_record__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('05171279-6792-4701-95a2-74550fcb59fc', anthropological_record__indigenous_epistemology_reading, coexists_with).
narrative_ontology:cs_axiom('05171279-6792-4701-95a2-74550fcb59fc', foundational, divine_causation_required_for_origins).
narrative_ontology:cs_axiom_status(divine_causation_required_for_origins, holdable).
narrative_ontology:cs_axiom_grounding('05171279-6792-4701-95a2-74550fcb59fc', divine_causation_required_for_origins, theological).
narrative_ontology:cs_axiom('05171279-6792-4701-95a2-74550fcb59fc', foundational, designed_complexity_discernible_in_record).
narrative_ontology:cs_axiom_status(designed_complexity_discernible_in_record, holdable).
narrative_ontology:cs_axiom_grounding('05171279-6792-4701-95a2-74550fcb59fc', designed_complexity_discernible_in_record, empirically_contingent).
narrative_ontology:cs_reference_frame('05171279-6792-4701-95a2-74550fcb59fc', scripture_as_origin_adjudicator).
narrative_ontology:cs_drift_state('05171279-6792-4701-95a2-74550fcb59fc', contemporary_post_genomic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('05171279-6792-4701-95a2-74550fcb59fc', '').
narrative_ontology:cs_kernel_id(anthropological_record__creationist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, denominational_authorities).
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, creation_science_enterprises).
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, rank_and_file_believers).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, doubting_youth_and_students).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, dissenting_scientists_in_faith_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, rank_and_file_believers).
narrative_ontology:constraint_vindicates(anthropological_record__creationist_reading, scriptural_inerrancy_doctrine).
narrative_ontology:constraint_vindicates(anthropological_record__creationist_reading, designed_complexity_hypothesis).
narrative_ontology:constraint_vindicates(anthropological_record__creationist_reading, flood_geology_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set doctrine and run the institutions — seminaries, day schools, publishing houses, ordination pipelines — through which members encounter accounts of human origins. Teach that scripture fixes the timeline and that divine acts explain the appearance of design in the fossil and genetic record. Income arrives as tithes, tuition, and donations; authority arrives as the recognized power to say what the record means. Stepping off this teaching risks splitting congregations and collapsing the donor and enrollment base that funds everything else, and the institution's identity has become fused with the teaching itself.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, denominational_authorities, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(anthropological_record__creationist_reading, denominational_authorities, beneficiary).

% Operate museums, curriculum lines, camps, and media outlets that package the divine-creation account for families and churches. Ticket sales, textbook contracts, and program fees fund staff and exhibits. Their audience is bounded by the communities that already accept the teaching, so their commercial fortunes rise and fall with the teaching's health rather than with outside scientific opinion; they could pivot content if the market shifted, though their brand is now deeply committed.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, creation_science_enterprises, beneficiary,
    organized, biographical, mobile, national).

% Attend, tithe, enroll children, and volunteer; receive in return a coherent origin story, moral formation, and a dense web of mutual aid. The same web raises the price of doubt: questioning the teaching strains marriages, friendships, and standing in the congregation. Most never test the account against outside materials; those who do typically do so quietly, and leaving means losing the entire social world, not just a belief.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, rank_and_file_believers, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(anthropological_record__creationist_reading, rank_and_file_believers, payer).

% Grow up inside day schools, homeschool co-ops, and youth programs where the divine-creation account is the only one presented, and where materials suggesting an older timeline are screened out. As teenagers they encounter contrary evidence online or at university with no map for reconciling it. Minors cannot choose their schools; young adults who voice doubt risk losing college funding, family support, and their community.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, doubting_youth_and_students, payer,
    powerless, biographical, constrained, national).

% Trained in geology, biology, or anthropology, employed by or belonging to the same congregations. They hold the mainstream timeline to be well supported but face pressure to stay silent, sign statements they disagree with, or leave. Speaking up costs jobs at parochial schools and ministries; staying silent costs integrity and career visibility; leaving costs the entire social world they were raised in.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, dissenting_scientists_in_faith_communities, payer,
    moderate, biographical, constrained, national).

% Produces the mainstream timeline through universities, museums, and journals. Inside the communities governed by this teaching its findings carry no adjudicative weight: textbooks are filtered, speakers disinvited, and its methods portrayed as hostile to faith. It functions freely outside those boundaries and contests the teaching mainly where the teaching seeks access to publicly funded science classrooms.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, naturalist_scientific_community, excluded,
    institutional, generational, mobile, global).

% Decide where the teaching may appear in publicly funded science classrooms. Successive rulings have confined it to private institutions, which pushed the teaching's infrastructure further into parallel schooling. They hear testimony from all sides and can alter the teaching's public reach, though not its internal authority within the communities that hold it.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, courts_and_school_boards, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__creationist_reading, denominational_authorities).
narrative_ontology:fixing_cost_class(anthropological_record__creationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single origin account that binds cosmology, morality, and group identity together: it coordinates education (day schools, homeschool curricula), congregational life, and boundary-maintenance around one interpretive authority, solving the problem of how a dispersed community transmits a unified worldview across generations.
% TRANSFER_FUNCTION: Moves money (tithes, tuition, ticket and curriculum revenue) and epistemic deference from members, parents, and students toward denominational structures and creation-science enterprises; moves adjudicative authority over the record away from credentialed science and toward clerical and apologetic institutions.
% ABSENT_VOICES: Naturalist scientists have no seat in the communities' internal adjudication — their findings enter only pre-filtered or as foils. Doubting members speak, if at all, anonymously or after exit. Children encounter the account before possessing any standing to object; their future objections are the voices most systematically absent from the rooms where curricula are chosen.
% DISAPPEARANCE_RATIONALE: Overnight removal would strand a large parallel infrastructure — thousands of day schools, homeschool networks, museums, curriculum houses, and ministries funded by the teaching. Congregations would split between accommodation and retention; millions of students would encounter the mainstream timeline for the first time inside their own institutions; the enterprises' market would vanish. Nothing physical rearranges, but the social and financial architecture built on the reading would reorganize within a generation.
% FOUNDING_PROBLEM: Reconciling the apparent age and development of the natural record with a plain reading of scripture, and protecting the community's scriptural authority against epistemologies that relocate adjudication to credentialed science — a tension sharpened into crisis by Darwin's publication and the fundamentalist-modernist controversy.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by historians of American religion documenting the continuous anti-evolution movement from the 1920s onward, by century-long survey series showing persistent popular rejection of the mainstream timeline, and by the published testimony of former adherents and dissenting scientists. The persistence of accommodationist movements (theistic-evolution denominations) independently attests that the underlying tension remains live even among parties that rejected this reading's solution.
narrative_ontology:disappearance_verdict(anthropological_record__creationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__creationist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__creationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(anthropological_record__creationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__creationist_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is 0.63 because costs concentrate on those least able to bear them (minors, doubters, dissenting professionals) while receipts concentrate in institutions whose revenue scales with adherence. Suppression is 0.68 because persistence depends on active machinery — curriculum screening, parochial and homeschool infrastructure, doctrinal discipline, social sanction — not on voluntary assent alone. Theater is 0.32: the formation and community function is real, but a growing share of activity is performative apologetics (staged debates, exhibit halls presenting assertion as evidence) that serves identity reinforcement more than inquiry. Accessibility_collapse is 0.58: alternatives survive outside the governed communities and increasingly leak in through online exposure, but are foreclosed within the institutions themselves. Resistance is 0.52: deconversion waves, internal old-earth factions, secular litigation, and quiet dual-bookkeeping meet the teaching continuously. The measurement series run on ONE shared grid (1920, 1950, 1970, 1987, 2007, 2020) with every tracked metric authored at every point. The suppression series traces an enforcement ratchet with a mid-century trough: blunt legal bans (1920s) collapsed after public ridicule, enforcement retreated to the private sphere (1950s), then rebuilt as hardened parallel infrastructure following the Genesis-Flood revival (1961+) and the post-Edwards institutional consolidation — enforcement capacity changed qualitatively (state bans to private systems), which is why suppression_requirement is tracked rather than left static. Theater rises with the professionalized apologetics industry. Boltzmann type is identity_coordination: the dominant function is boundary and membership maintenance; the FNL gaming alert applies — identity framing ('this is who we are') is precisely the cover story under which asymmetric extraction hides, so Power x Scope coupling concentrating costs on powerless agents deserves scrutiny here.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement reads as covenantal fidelity: authorities experience themselves as protecting a community's scripture and children from an epistemology they regard as hostile. From the payer seats the identical structure operates as epistemic enclosure with real, unevenly distributed costs. Rank-and-file believers straddle the gap — genuine recipients of meaning and mutual aid who nonetheless carry exit costs that grow with tenure. The engine computes per-seat classifications from the power/exit data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Denominational authorities and creation-science enterprises derive directionality near the beneficiary pole: they collect revenue and deference and control the rules. Rank-and-file believers sit nearer symmetric — genuine subsidy in meaning and mutual aid offset by epistemic and exit costs, with identity_lock amplifying whatever net position obtains. Doubting students and dissenting scientists derive near the target pole, amplified by constrained exits and, for lifers, identity locks. The naturalist scientific community is excluded rather than coordinated: it loses adjudicative jurisdiction rather than paying transfers, so its structural position differs from the paying victims even though it is the reading's principal rhetorical target. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms produce the correct relationships, and the schema's override surface keys on power atoms, which would collide across same-power seats here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling the record's apparent age and development with a plain scriptural reading, and defending scriptural adjudicative authority — remains live for the adopting communities, so no mandatrophy resolution is declared. The tangled_rope classification guards both failure modes: a pure-snare reading would erase the real coordination (cohesion, moral formation, charity, intergenerational transmission) that sustains millions of adherents in good faith; a pure-rope reading would erase the identifiable payers — minors with no school choice, doubters facing total social loss, scientists forced to choose between integrity and belonging. If the founding problem ever dies (through reconciliation or through the empirical arms' refutation forcing retreat to purely theological framing), expect piton drift: theatrical maintenance of a reading whose investigative function has atrophied, detectable via the rising theater_ratio series already visible in the measurements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates one reading (creationist_reading) of the kernel anthropological_record; how would the classification of the SAME evidentiary referent shift under the sibling readings?',
    'Compare the sibling files (naturalist_reading, indigenous_epistemology_reading): each authors its own epsilon, beneficiaries, and victims over the shared referent; divergent computed types across the family locate the disagreement structurally.',
    'If the naturalist sibling computes this regime as a snare-grade epistemic enclosure while this file computes the naturalist adjudication regime as the extractive displacement, the corpus records symmetric indexicality rather than a neutral fact about the record itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification of a shared kernel; cross-reading comparison is the resolution path.').

omega_variable(
    designed_complexity_empirical_status,
    'Are the reading''s empirically-warranted arms (irreducible complexity, genetic-entropy arguments, flood-geology correlations) tenable against the accumulating genomic and stratigraphic record?',
    'Track the peer-reviewed prediction track record of design-based research programs against mainstream results; watch for institutional retreat to purely theological framing.',
    'Systematic refutation would deepen the axiom_overriding drift toward severe magnitude and push the regime toward enforcement-only persistence; durable confirmation would strengthen the rope-side coordination reading and lower measured extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(designed_complexity_empirical_status, empirical, 'Empirical tenability of the designed-complexity arm of the reading.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression inside the governed communities primarily structural (curriculum control, school discipline, social sanction) or internalized (identity fusion that makes doubt feel like betrayal)?',
    'Post-exit trajectory studies of deconverts: if doubt remains costly after leaving the community''s institutions, a large internalized component is present.',
    'An internalized component raises effective suppression above the structural measure and predicts slower decay than enrollment data alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in an identity-fused population.').

omega_variable(
    believer_net_position_ambiguity,
    'Do rank-and-file believers sit net-beneficiary (meaning, cohesion, mutual aid outweighing epistemic and opportunity costs) or net-payer once costs are fully counted?',
    'Longitudinal comparisons of matched cohorts inside and outside the teaching on wellbeing, community durability, and science-literacy outcomes, weighted by explicit value judgments about what counts as benefit.',
    'Net-benefit strengthens the genuine-coordination half of the tangled-rope structure; net-pay shifts the regime toward snare drift with ordinary adherents as principal victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(believer_net_position_ambiguity, preference, 'Valuation-dependent net position of ordinary adherents.').

omega_variable(
    generational_persistence_trajectory,
    'Is the regime decaying under youth attrition and secularization, or consolidating through parallel institutions (day schools, homeschool networks, museums)?',
    'Cohort belief surveys, parochial enrollment series, and ministry finance disclosures across the coming decades.',
    'Decay would read the arrangement as transitional residue of a defensive era; consolidation predicts continued suppression ratchet and extraction accumulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_persistence_trajectory, empirical, 'Lifecycle direction of the creationist institutional regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__creationist_reading, 1920, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t1920, anthropological_record__creationist_reading, theater_ratio, 1920, 0.15).
narrative_ontology:measurement_basis(anth_tr_t1920, observed).
narrative_ontology:measurement(anth_tr_t1950, anthropological_record__creationist_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement_basis(anth_tr_t1950, observed).
narrative_ontology:measurement(anth_tr_t1970, anthropological_record__creationist_reading, theater_ratio, 1970, 0.22).
narrative_ontology:measurement_basis(anth_tr_t1970, observed).
narrative_ontology:measurement(anth_tr_t1987, anthropological_record__creationist_reading, theater_ratio, 1987, 0.26).
narrative_ontology:measurement_basis(anth_tr_t1987, observed).
narrative_ontology:measurement(anth_tr_t2007, anthropological_record__creationist_reading, theater_ratio, 2007, 0.3).
narrative_ontology:measurement_basis(anth_tr_t2007, observed).
narrative_ontology:measurement(anth_tr_t2020, anthropological_record__creationist_reading, theater_ratio, 2020, 0.32).
narrative_ontology:measurement_basis(anth_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(anth_be_t1920, anthropological_record__creationist_reading, base_extractiveness, 1920, 0.42).
narrative_ontology:measurement_basis(anth_be_t1920, observed).
narrative_ontology:measurement(anth_be_t1950, anthropological_record__creationist_reading, base_extractiveness, 1950, 0.38).
narrative_ontology:measurement_basis(anth_be_t1950, observed).
narrative_ontology:measurement(anth_be_t1970, anthropological_record__creationist_reading, base_extractiveness, 1970, 0.48).
narrative_ontology:measurement_basis(anth_be_t1970, observed).
narrative_ontology:measurement(anth_be_t1987, anthropological_record__creationist_reading, base_extractiveness, 1987, 0.54).
narrative_ontology:measurement_basis(anth_be_t1987, observed).
narrative_ontology:measurement(anth_be_t2007, anthropological_record__creationist_reading, base_extractiveness, 2007, 0.6).
narrative_ontology:measurement_basis(anth_be_t2007, observed).
narrative_ontology:measurement(anth_be_t2020, anthropological_record__creationist_reading, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement_basis(anth_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t1920, anthropological_record__creationist_reading, suppression_requirement, 1920, 0.55).
narrative_ontology:measurement_basis(anth_su_t1920, observed).
narrative_ontology:measurement(anth_su_t1950, anthropological_record__creationist_reading, suppression_requirement, 1950, 0.45).
narrative_ontology:measurement_basis(anth_su_t1950, observed).
narrative_ontology:measurement(anth_su_t1970, anthropological_record__creationist_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement_basis(anth_su_t1970, observed).
narrative_ontology:measurement(anth_su_t1987, anthropological_record__creationist_reading, suppression_requirement, 1987, 0.62).
narrative_ontology:measurement_basis(anth_su_t1987, observed).
narrative_ontology:measurement(anth_su_t2007, anthropological_record__creationist_reading, suppression_requirement, 2007, 0.66).
narrative_ontology:measurement_basis(anth_su_t2007, observed).
narrative_ontology:measurement(anth_su_t2020, anthropological_record__creationist_reading, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement_basis(anth_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__creationist_reading, identity_coordination).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% Constraint family: anthropological_record decomposes by READING, not by observable — all three readings share one evidentiary referent and differ in causal attribution and adjudicative authority, so each carries its own epsilon, beneficiaries, victims, and claimed type. This file links both siblings. Upstream/downstream pressure between readings runs through education policy, court rulings, and mission history rather than through shared metrics; the naturalist reading's institutional dominance supplies the background pressure against which this reading's enforcement infrastructure was built.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
