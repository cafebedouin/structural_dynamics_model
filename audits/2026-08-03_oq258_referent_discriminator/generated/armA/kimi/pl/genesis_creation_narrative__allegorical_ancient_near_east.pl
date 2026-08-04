% ============================================================================
% CONSTRAINT STORY: genesis_creation_narrative__allegorical_ancient_near_east
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_narrative__allegorical_ancient_near_east, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: genesis_creation_narrative__allegorical_ancient_near_east
 *   human_readable: Genesis 1-2 as Ancient Near Eastern Mythopoetic Literature
 *   domain: religious_studies/biblical_hermeneutics
 *
 * SUMMARY:
 *   This constraint is one reading of the contested
 *   genesis_creation_narrative kernel: the allegorical_ancient_near_east
 *   reading. It treats Genesis 1-2 as Ancient Near Eastern mythopoetic
 *   literature with no adjudicative authority over cosmology, biology, or
 *   natural history. The reading is institutionalized in mainline Protestant
 *   seminaries, university religious studies departments, and liberal
 *   Catholic theology. It coordinates science-religion relations by complete
 *   decoupling, but asymmetrically extracts textual authority from
 *   traditional religious communities and strips normative force from
 *   dominion and creation-order metaphors. Sibling readings include
 *   literal_young_earth (inerrant chronicle) and theistic_evolutionary
 *   (harmonizing framework). The claim/metric independence is maintained: the
 *   reading is claimed as tangled_rope because it provides genuine
 *   coordination while structurally extracting from traditionalist seats; the
 *   metrics describe moderate extractiveness, moderate suppression, and
 *   significant theater from scholarly performance.
 *
 * KEY AGENTS:
 *   - ane_scholarly_guild: Agenda-setter (institutional/analytical/global) — establishes ANE comparative methodology and peer-review norms
 *   - mainline_religious_institutions: Primary beneficiary (organized/constrained/national) — gains intellectual respectability and reduced science conflict
 *   - scientific_community: Secondary beneficiary (institutional/analytical/global) — receives reduced political opposition to natural science
 *   - religious_traditionalists: Primary payer (organized/constrained/national) — loses textual authority and normative grounding
 *   - traditional_lay_believers: Deep payer (powerless/identity_locked/local) — experiences loss of scriptural certainty
 *   - conservative_theological_academy: Excluded voice (institutional/constrained/national) — holds literal readings but is absent from mainstream venues
 *   - secular_religious_studies_scholars: Analytical observer (institutional/analytical/global) — studies the constraint without participating in its costs or benefits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__allegorical_ancient_near_east, 0.48).
domain_priors:suppression_score(genesis_creation_narrative__allegorical_ancient_near_east, 0.55).
domain_priors:theater_ratio(genesis_creation_narrative__allegorical_ancient_near_east, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, extractiveness, 0.48).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__allegorical_ancient_near_east, tangled_rope).
narrative_ontology:human_readable(genesis_creation_narrative__allegorical_ancient_near_east, "Genesis 1-2 as Ancient Near Eastern Mythopoetic Literature").
narrative_ontology:topic_domain(genesis_creation_narrative__allegorical_ancient_near_east, "religious_studies/biblical_hermeneutics").

domain_priors:requires_active_enforcement(genesis_creation_narrative__allegorical_ancient_near_east).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__allegorical_ancient_near_east, '025de4d3-f503-44b0-9483-ad159d0201d5').
narrative_ontology:cs_kernel_codification('025de4d3-f503-44b0-9483-ad159d0201d5', fixed_text).
narrative_ontology:cs_authority_grounding('025de4d3-f503-44b0-9483-ad159d0201d5', expertise).
narrative_ontology:cs_interpretation_layer_present('025de4d3-f503-44b0-9483-ad159d0201d5').
narrative_ontology:cs_reading_relation('025de4d3-f503-44b0-9483-ad159d0201d5', genesis_creation_narrative__literal_young_earth, forecloses).
narrative_ontology:cs_reading_relation('025de4d3-f503-44b0-9483-ad159d0201d5', genesis_creation_narrative__theistic_evolutionary, influences).
narrative_ontology:cs_axiom('025de4d3-f503-44b0-9483-ad159d0201d5', foundational, genesis_ane_mythopoetic_not_chronicle).
narrative_ontology:cs_axiom_status(genesis_ane_mythopoetic_not_chronicle, holdable).
narrative_ontology:cs_axiom_grounding('025de4d3-f503-44b0-9483-ad159d0201d5', genesis_ane_mythopoetic_not_chronicle, empirically_contingent).
narrative_ontology:cs_axiom('025de4d3-f503-44b0-9483-ad159d0201d5', foundational, scripture_no_cosmological_authority).
narrative_ontology:cs_axiom_status(scripture_no_cosmological_authority, holdable).
narrative_ontology:cs_axiom_grounding('025de4d3-f503-44b0-9483-ad159d0201d5', scripture_no_cosmological_authority, deontological).
narrative_ontology:cs_reference_frame('025de4d3-f503-44b0-9483-ad159d0201d5', ane_comparative_literary_framework).
narrative_ontology:cs_drift_state('025de4d3-f503-44b0-9483-ad159d0201d5', contemporary_mainline_practice, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('025de4d3-f503-44b0-9483-ad159d0201d5', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, ane_scholarly_guild).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, mainline_religious_institutions).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, scientific_community).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, religious_traditionalists).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, traditional_lay_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and enforces the historical-critical interpretive framework through peer review, seminary curricula, and academic hiring. Advances ANE comparative methodology as normative for reading Genesis. Benefits from research funding, institutional prestige, and the cultural capital of 'sophisticated' biblical literacy.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, ane_scholarly_guild, agenda_setter,
    institutional, generational, analytical, global).

% Adopt the ANE reading to retain educated membership and avoid public conflict with modern science. Use Genesis liturgically and devotionally while disclaiming its cosmological and biological authority. Gain intellectual respectability and reduced cognitive dissonance for congregants, but lose the ability to appeal to creation order for normative ethics.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, mainline_religious_institutions, beneficiary,
    organized, generational, constrained, national).

% Encounters negligible theological opposition to cosmology, geology, or biology from communities holding this reading. Treats Genesis as culturally significant myth rather than a competing empirical account. Benefits from reduced political pressure against teaching evolution and conducting stem-cell or climate research.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, scientific_community, beneficiary,
    institutional, civilizational, analytical, global).

% Lose textual authority for cosmological, historical, and certain ethical claims when denominations adopt the ANE reading. Their interpretive tradition is marginalized in mainline seminaries and denominational statements. Must either accept diminished normative force, leave for confessional institutions, or mount costly institutional opposition.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, religious_traditionalists, payer,
    organized, generational, constrained, national).

% Hold personal faith frameworks in which Genesis provides historical and cosmological certainty. Taught in mainline settings that these passages are 'just myth' or culturally conditioned poetry. Experience cognitive dissonance, shame for 'unsophisticated' belief, or rupture with congregations that no longer affirm the text's literal truthfulness.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, traditional_lay_believers, payer,
    powerless, biographical, identity_locked, local).

% Maintains confessional, literal, or harmonizing readings of Genesis. Structurally excluded from tenure-track positions in research universities, mainstream society memberships, and flagship journal editorial boards. Would object to the ANE framing on historical and theological grounds but is absent from the venues where this reading is normative.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, conservative_theological_academy, excluded,
    institutional, generational, constrained, national).

% Study the rise and institutional function of the ANE reading as a sociological and historical phenomenon. Do not benefit from the constraint's coordination nor pay its extraction costs. Analyze how the reading operates as a boundary-maintenance mechanism between scientific and religious institutional spheres.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, secular_religious_studies_scholars, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_narrative__allegorical_ancient_near_east, ane_scholarly_guild).
narrative_ontology:fixing_cost_class(genesis_creation_narrative__allegorical_ancient_near_east, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents direct conflict between biblical faith and modern natural science by decoupling Genesis 1-2 from cosmological and biological claims, allowing religious and scientific communities to operate in non-overlapping magisteria without direct contradiction.
% TRANSFER_FUNCTION: Moves interpretive authority over Genesis 1-2 from traditional religious communities and the text itself to the ANE scholarly guild and the natural sciences; moves normative ethical grounding away from creation-order appeals derived from dominion or stewardship metaphors.
% ABSENT_VOICES: Conservative evangelical and confessional Jewish scholars who hold to Mosaic authorship and historical referentiality are excluded from mainstream academic biblical studies. Also absent are theologians who derive normative environmental or gender ethics from a literal creation order, and lay believers whose spiritual formation depends on the text's historical truthfulness.
% DISAPPEARANCE_RATIONALE: If the allegorical ANE reading vanished overnight, mainline seminaries would need to re-engage the science-religion conflict directly, denominational statements on creation would revert to literal or harmonizing frameworks, and the scholarly guild would lose a central pillar of the historical-critical method. The institutional boundary between theology and natural science would destabilize.
% FOUNDING_PROBLEM: The Enlightenment and the rise of historical geology and evolutionary biology created a crisis of authority in which biblical chronology appeared to contradict empirical natural history, threatening the intellectual credibility of religious institutions.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of science (e.g., James Moore, Ronald Numbers) and sociologists of religion attest that the conflict was historically significant for 19th- and 20th-century religious institutions. Conservative apologists contest that the conflict was manufactured by overreading the text and by Enlightenment anti-supernaturalism; they attest the problem was illusory from the outset. The corroboration is therefore split along party lines, with external non-beneficiaries on both sides.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__allegorical_ancient_near_east, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_narrative__allegorical_ancient_near_east, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__allegorical_ancient_near_east, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(genesis_creation_narrative__allegorical_ancient_near_east, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__allegorical_ancient_near_east, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(genesis_creation_narrative__allegorical_ancient_near_east, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate: the reading genuinely reduces conflict but strips cosmological and certain ethical authority from the text, imposing real costs on traditionalist seats. Suppression (0.55) reflects that literal readings are marginalized in mainline academic and ecclesial venues despite thriving in popular evangelicalism. Theater ratio (0.40) captures the elaborate scholarly apparatus (ANE parallels, form criticism, redaction history) that performs sophistication while often re-describing widely available literary observations. Accessibility collapse (0.70) is high because, once the ANE framework is accepted, literal readings appear intellectually naive in educated contexts. Resistance (0.60) is substantial due to ongoing evangelical institutional counter-builds and political creationism. The measurement series show rising extraction and enforcement through the 20th century, stabilizing as the reading achieved establishment status and then facing slight pressure from conservative resurgence.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (scholarly guild, mainline churches, scientific community) experience the constraint as liberating coordination that preserves religious meaning without scientific conflict. The payer seats (traditionalists, lay believers) experience the same structure as an authority transfer that dissolves textual certainty and normative grounding. The engine computes this divergence from the structural data: identical constraint, opposite directionalities, producing different seat-level classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries cluster near the low-d end: the scholarly guild sets the rules and collects prestige; mainline institutions collect respectability; the scientific community collects reduced opposition. Victims cluster near the high-d end: traditionalists lose authority and institutional voice; lay believers with identity-locked faith lose cognitive coherence. The agenda-setter is not a pure beneficiary—it pays some cost in scholarly labor—but its directionality is strongly subsidized by the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this reading as pure rope (harmless coordination) by requiring victim declarations that capture the authority transfer and normative loss imposed on traditionalist seats. It prevents mislabeling as snare by preserving the genuine coordination function: the science-religion conflict is a real collective-action problem, and this reading solves it for millions of believers. The temporal measurements show that extraction rose alongside institutionalization, distinguishing the current tangled state from any hypothetical pure-coordination origin.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_normative_authority_scope,
    'Does the ANE reading strip only historical-scientific authority from Genesis 1-2, or does it inevitably erode all normative authority including ethics and theology?',
    'Ethnographic and homiletical analysis of communities holding this reading to determine whether they still derive normative claims (environmental stewardship, sabbath observance, gender complementarity) from the creation narratives.',
    'If all normative authority is stripped, the constraint extracts more heavily and the victim set expands to include ethicists and liturgical communities; if only scientific authority is removed, extraction is more narrowly bounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_normative_authority_scope, conceptual, 'Ambiguity about the scope of textual authority loss under the ANE reading').

omega_variable(
    kernel_reading_boundary_location,
    'This constraint is one reading of the genesis_creation_narrative kernel. Where exactly is the structural disagreement located among the three sibling readings, and does this reading foreclose or merely influence the theistic_evolutionary sibling?',
    'Comparative structural analysis of the three readings'' epsilon, beneficiary, and victim profiles to identify which domains of life (cosmology, biology, ethics, liturgy) each reading treats as textually governed.',
    'Determines whether the reading relation to theistic_evolutionary should be forecloses or influences; affects the classification boundary between tangled_rope and rope for the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Structural location of disagreement among kernel readings').

omega_variable(
    enforcement_mechanism_nature,
    'Is the reading''s persistence maintained primarily by evidential argumentation (ANE comparative data, literary analysis) or by institutional gatekeeping (seminary hiring, curriculum control, peer-review exclusion)?',
    'Quantitative analysis of mainline seminary faculty appointments, journal acceptance rates for non-ANE readings, and curriculum requirements over the last fifty years.',
    'If gatekeeping dominates, directionality for traditionalist seats rises and the constraint trends snare-ward; if evidence dominates, it trends rope-ward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_nature, empirical, 'Empirical basis versus institutional enforcement of the ANE reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__allegorical_ancient_near_east, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gene_tr_t30, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 30, 0.25).
narrative_ontology:measurement(gene_tr_t60, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 60, 0.32).
narrative_ontology:measurement(gene_tr_t90, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 90, 0.38).
narrative_ontology:measurement(gene_tr_t120, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 120, 0.4).
narrative_ontology:measurement(gene_tr_t150, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 150, 0.4).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gene_be_t30, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(gene_be_t60, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 60, 0.48).
narrative_ontology:measurement(gene_be_t90, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 90, 0.5).
narrative_ontology:measurement(gene_be_t120, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 120, 0.49).
narrative_ontology:measurement(gene_be_t150, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 150, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(gene_su_t30, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(gene_su_t60, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(gene_su_t90, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 90, 0.58).
narrative_ontology:measurement(gene_su_t120, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 120, 0.56).
narrative_ontology:measurement(gene_su_t150, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 150, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__allegorical_ancient_near_east, identity_coordination).
narrative_ontology:boltzmann_floor_override(genesis_creation_narrative__allegorical_ancient_near_east, 0.08).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__literal_young_earth).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__theistic_evolutionary).

% DUAL FORMULATION NOTE:
% The natural-language label 'Genesis creation narrative' conflates three structurally distinct readings: allegorical ANE mythopoetry (this file), literal young-earth chronicle, and theistic-evolutionary harmony. Each reading has a different epsilon, different beneficiary/victim structures, and different epistemic scope. They form a constraint family because they compete for the same interpretive-institutional space and the dominance of one structurally affects the viability of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
