% ============================================================================
% CONSTRAINT STORY: anthropological_record__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__naturalist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: anthropological_record__naturalist_reading
 *   human_readable: Naturalist Reading of the Anthropological Record (Evolution/Migration via Scientific Method)
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This constraint models the naturalist reading of the anthropological
 *   record kernel: the claim that the human fossil, archaeological, and
 *   genetic record reveals materialist origins (evolution via natural
 *   selection, population migrations tracked through genetics and
 *   archaeology) knowable through scientific method. This reading is
 *   instantiated here as a single, ε-stable constraint distinct from the
 *   creationist reading and the indigenous epistemology reading — those are
 *   separate constraints in this family, not alternative measurements of this
 *   one. The naturalist reading's genuine coordination function (a shared,
 *   falsifiable standard for evaluating deep-time claims) coexists with a
 *   credentialing and publication gatekeeping apparatus that extracts career
 *   and legitimacy costs from non-credentialed researchers,
 *   religiously-affiliated scientists, and indigenous knowledge-holders whose
 *   interpretive frameworks are excluded a priori rather than evaluated on
 *   evidentiary merit.
 *
 * KEY AGENTS:
 *   - credentialed_paleoanthropologists: Primary agenda-setter/beneficiary (institutional/arbitrage) — controls interpretive legitimacy
 *   - peer_reviewed_journals: Enforcement mechanism (institutional/arbitrage) — gatekeeps what counts as valid science
 *   - non_credentialed_amateur_researchers: Primary target (powerless/trapped) — bears exclusion cost
 *   - indigenous_knowledge_keepers_excluded_from_interpretation: Excluded voice (powerless/trapped) — treated as data source, not interpretive peer
 *   - religiously_affiliated_researchers_seeking_mainstream_legitimacy: Secondary target (moderate/constrained) — pays career tax for framework deviation
 *   - philosophers_of_science: Analytical observer — documents demarcation dynamics from outside credentialing structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__naturalist_reading, 0.58).
domain_priors:suppression_score(anthropological_record__naturalist_reading, 0.62).
domain_priors:theater_ratio(anthropological_record__naturalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__naturalist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__naturalist_reading, "Naturalist Reading of the Anthropological Record (Evolution/Migration via Scientific Method)").
narrative_ontology:topic_domain(anthropological_record__naturalist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__naturalist_reading, 'ed29b7c6-4d4e-42ff-a43b-45c8d2e6c35d').
narrative_ontology:cs_kernel_codification('ed29b7c6-4d4e-42ff-a43b-45c8d2e6c35d', distributed).
narrative_ontology:cs_authority_grounding('ed29b7c6-4d4e-42ff-a43b-45c8d2e6c35d', expertise).
narrative_ontology:cs_interpretation_layer_present('ed29b7c6-4d4e-42ff-a43b-45c8d2e6c35d').
narrative_ontology:cs_reading_relation('ed29b7c6-4d4e-42ff-a43b-45c8d2e6c35d', anthropological_record__creationist_reading, forecloses).
narrative_ontology:cs_reading_relation('ed29b7c6-4d4e-42ff-a43b-45c8d2e6c35d', anthropological_record__indigenous_epistemology_reading, influences).
narrative_ontology:cs_axiom('ed29b7c6-4d4e-42ff-a43b-45c8d2e6c35d', foundational, methodological_naturalism_is_exclusive_epistemic_gate).
narrative_ontology:cs_axiom_status(methodological_naturalism_is_exclusive_epistemic_gate, holdable).
narrative_ontology:cs_axiom_grounding('ed29b7c6-4d4e-42ff-a43b-45c8d2e6c35d', methodological_naturalism_is_exclusive_epistemic_gate, conventional).
narrative_ontology:cs_axiom('ed29b7c6-4d4e-42ff-a43b-45c8d2e6c35d', foundational, supernatural_causation_is_scientifically_inadmissible).
narrative_ontology:cs_axiom_status(supernatural_causation_is_scientifically_inadmissible, holdable).
narrative_ontology:cs_axiom_grounding('ed29b7c6-4d4e-42ff-a43b-45c8d2e6c35d', supernatural_causation_is_scientifically_inadmissible, instrumental).
narrative_ontology:cs_reference_frame('ed29b7c6-4d4e-42ff-a43b-45c8d2e6c35d', post_darwinian_synthesis_consensus).
narrative_ontology:cs_drift_state('ed29b7c6-4d4e-42ff-a43b-45c8d2e6c35d', contemporary_credentialing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ed29b7c6-4d4e-42ff-a43b-45c8d2e6c35d', '').
narrative_ontology:cs_kernel_id(anthropological_record__naturalist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, credentialed_paleoanthropologists).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, research_universities).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, peer_reviewed_journals).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, non_credentialed_amateur_researchers).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, indigenous_knowledge_keepers_excluded_from_interpretation).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, religiously_affiliated_researchers_seeking_mainstream_legitimacy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, the_lay_public).
narrative_ontology:constraint_vindicates(anthropological_record__naturalist_reading, methodological_naturalism).
narrative_ontology:constraint_vindicates(anthropological_record__naturalist_reading, common_descent_hypothesis).
narrative_ontology:constraint_vindicates(anthropological_record__naturalist_reading, out_of_africa_migration_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control excavation permits, journal peer review, museum curation, and university hiring committees. Determine which interpretations of fossil and genetic evidence count as legitimate science. Their careers, funding, and institutional standing depend on the naturalist framework remaining the sole gatekept lens for interpreting the record.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, credentialed_paleoanthropologists, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, credentialed_paleoanthropologists, beneficiary).

% Receive grant funding, prestige, and tuition revenue tied to producing credentialed interpreters of the record. Benefit from the credentialing monopoly without directly enforcing it themselves; enforcement is delegated to professional societies and journals.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, research_universities, beneficiary,
    institutional, generational, arbitrage, global).

% Gatekeep publication of findings, requiring methodological naturalism as a condition of consideration. Reject submissions invoking supernatural causation or non-standard evidentiary methods regardless of their empirical content, and thereby control what counts as the settled record.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, peer_reviewed_journals, agenda_setter,
    institutional, generational, arbitrage, global).

% May possess relevant field observations, local knowledge, or independent analysis but lack institutional affiliation. Their findings are dismissed or ignored by gatekept venues regardless of merit; the only path to legitimacy is years of credentialing they often cannot afford or access.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, non_credentialed_amateur_researchers, payer,
    powerless, biographical, trapped, national).

% Hold oral traditions describing origins and migrations that sometimes align with, sometimes diverge from, the naturalist timeline. Their accounts are treated as anthropological data ABOUT them rather than candidate interpretive frameworks in their own right, even when their traditions predate and sometimes corroborate specific findings later 'discovered' by credentialed researchers.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, indigenous_knowledge_keepers_excluded_from_interpretation, excluded,
    powerless, civilizational, trapped, regional).

% Trained scientists whose personal or institutional religious commitments conflict with strict methodological naturalism. Must either bracket those commitments entirely in professional output or risk exclusion from tenure, publication, and funding — a career-long tax on any deviation from the naturalist framing regardless of the quality of their empirical work.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, religiously_affiliated_researchers_seeking_mainstream_legitimacy, payer,
    moderate, biographical, constrained, national).

% Receives textbook and museum accounts of human origins as settled scientific consensus. Benefits from a coherent, evidence-grounded origin narrative but has no meaningful voice in adjudicating disputes over interpretation and rarely encounters the gatekeeping machinery that produces the consensus they consume.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, the_lay_public, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, the_lay_public, excluded).

% Study the demarcation criteria, methodological commitments, and sociology of the paleoanthropological field from outside its credentialing structure. Can document where naturalist gatekeeping tracks genuine evidentiary rigor versus where it functions as boundary-policing against rival epistemic communities.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, philosophers_of_science, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Methodological naturalism solves a genuine coordination problem: it provides a shared, testable, intersubjectively falsifiable standard for adjudicating claims about fossil dating, genetic lineage, and migration patterns, allowing disparate researchers across institutions and continents to build cumulative, checkable knowledge rather than incommensurable competing narratives.
% TRANSFER_FUNCTION: Moves interpretive authority over the human origins record from lay observers, oral-tradition keepers, and non-credentialed researchers to credentialed institutional actors; moves funding, publication access, and legitimacy toward those who accept and enforce the naturalist framework, and away from those who do not.
% ABSENT_VOICES: Indigenous knowledge keepers whose oral traditions encode migration and origin claims are treated as ethnographic subjects rather than epistemic peers; their accounts are mined for corroborating data points but rarely engaged as an alternative interpretive method with standing to adjudicate contested findings. Religiously-motivated researchers with relevant training are structurally discouraged from raising interpretive alternatives in professional venues.
% DISAPPEARANCE_RATIONALE: If methodological naturalism's gatekeeping apparatus vanished overnight, the underlying fossil and genetic evidence would not change, but the credentialing pipeline, journal review standards, and institutional hiring practices that currently enforce a single interpretive lens would need to be rebuilt or replaced — mainstream scientists argue this would cause epistemic chaos and legitimize pseudoscience; critics argue it would simply open space for competing evidentiary traditions to be heard on their merits. The parties dispute which outcome would follow.
% FOUNDING_PROBLEM: Nineteenth and twentieth century natural history needed a way to distinguish empirically testable claims about deep human history from unfalsifiable theological or mythological claims, so that fossil and genetic evidence could be evaluated on consistent, checkable grounds across a growing international scientific community.
% FOUNDING_PROBLEM_CORROBORATION: Historians and philosophers of science outside the paleoanthropological credentialing structure (e.g., science studies scholars examining the historical demarcation debates) corroborate that the original coordination problem — establishing checkable standards against unfalsifiable claims — was real and substantially solved decades ago. The same outside observers note the credentialing apparatus has since expanded well beyond that founding function into broader boundary-policing against indigenous epistemologies and religiously-affiliated researchers, a claim credentialed insiders themselves largely do not corroborate and often actively dispute.
narrative_ontology:disappearance_verdict(anthropological_record__naturalist_reading, contested).
narrative_ontology:founding_problem_status(anthropological_record__naturalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__naturalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(anthropological_record__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__naturalist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__naturalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__naturalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (moderate-high, rising over the interval) because the naturalist framework's coordination function is real — methodological naturalism genuinely enables cumulative, checkable knowledge production about fossil dating and genetic lineage — but the credentialing apparatus built atop it has expanded to extract legitimacy and career costs from interpreters who accept the same evidentiary base yet are excluded on grounds unrelated to evidence quality (lack of credentials, religious affiliation, non-Western epistemic tradition). Suppression (0.62) is authored higher than extraction because the mechanism operates primarily through exclusion from legitimate venues rather than direct extraction of resources — a rejected submission or an unrenewed excavation permit is a suppression event more than an extraction event. Theater ratio is modest (0.28) and rising, reflecting that most gatekeeping activity still serves genuine quality control, with a growing but still minority share serving boundary-policing against rival epistemic communities rather than evidentiary rigor.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed paleoanthropologists, research universities, and peer-reviewed journals sit near the full-beneficiary end: they set the interpretive rules, collect the institutional rewards (funding, prestige, hiring authority) and face no meaningful check from those excluded. Non-credentialed researchers and religiously-affiliated scientists sit near the full-target end: they bear the cost of exclusion or the tax of self-censorship without commensurate voice in setting the rules. Indigenous knowledge keepers are structurally excluded rather than merely disadvantaged — their traditions are mined as ethnographic data rather than engaged as a competing interpretive method, which is a distinct and arguably deeper structural position than 'losing' a fair evaluation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinguishing falsifiable claims about deep human history from unfalsifiable theological or mythological claims — was substantially solved decades ago; radiometric dating, genetic sequencing, and stratigraphic method are now routine and largely uncontested as TOOLS. What remains contested is whether the credentialing and publication gatekeeping apparatus built to defend those tools against genuinely unfalsifiable claims has since calcified into boundary-policing against claims that ARE potentially falsifiable but originate outside the credentialed community (indigenous oral chronologies making testable claims about migration timing; religiously-motivated researchers proposing testable hypotheses framed in theological language). Classifying this as tangled_rope rather than snare or mountain prevents two mislabeling errors: treating the entire naturalist apparatus as pure extraction (which would erase its genuine, functioning coordination role in producing checkable knowledge) and treating it as a pure mountain of natural law (which would erase the credentialing structure's identifiable, institutionally-benefiting administrators and its actively enforced exclusion of alternative interpreters).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalism_as_method_vs_naturalism_as_gatekeeping,
    'Is methodological naturalism, as actually practiced in paleoanthropology, functioning as a necessary epistemic discipline (excluding genuinely untestable claims) or has it drifted into a boundary-policing apparatus that excludes potentially testable claims merely because they originate outside credentialed institutions or use non-standard framing?',
    'Systematic review of rejected submissions and denied excavation permits to determine what fraction were rejected on evidentiary grounds versus on grounds of non-credentialed origin, unconventional framing, or explicit religious/indigenous provenance, holding evidentiary quality constant.',
    'If rejections track evidentiary quality, this reading is closer to a rope (genuine coordination with minimal excess extraction). If rejections systematically track provenance independent of evidentiary quality, this reading is closer to a snare wearing naturalist justification as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalism_as_method_vs_naturalism_as_gatekeeping, empirical, 'Whether naturalist gatekeeping tracks evidentiary rigor or credential/provenance boundary-policing.').

omega_variable(
    corroboration_vs_appropriation_of_indigenous_knowledge,
    'When credentialed researchers use indigenous oral tradition as corroborating data for a naturalist finding, does this represent genuine epistemic partnership or extraction of indigenous intellectual labor without according indigenous knowledge-keepers interpretive standing?',
    'Track co-authorship, attribution, and decision-making authority in published findings that draw on indigenous oral tradition, compared to findings drawing on other non-indigenous field data sources.',
    'If indigenous knowledge is systematically used as data without corresponding interpretive authority or co-authorship, this strengthens the case that the excluded_voices victim declaration is structurally accurate rather than incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corroboration_vs_appropriation_of_indigenous_knowledge, empirical, 'Whether indigenous oral tradition is treated as data source or interpretive partner.').

omega_variable(
    kernel_framing_alternative,
    'Is the more defensible framing of this constraint the credentialing institution itself (as authored here), or the deeper layer of the demarcation criterion (falsifiability/testability) that the credentialing institution claims to merely apply? A framing centered on the demarcation criterion itself might reveal it as a genuinely near-mountain epistemic principle, while the credentialing institution built atop it is where the extraction concentrates.',
    'Author a decomposed sibling story isolating the demarcation criterion (falsifiability as an epistemic principle) from the institutional credentialing apparatus that administers access to publishing and funding under that criterion; compare their independently computed classifications.',
    'If the demarcation criterion alone computes as mountain-like (low extraction, high accessibility collapse, low resistance) while the credentialing apparatus computes as tangled_rope or snare, this confirms the two are properly separate constraints per the ε-invariance principle and this story is correctly scoped to the institutional layer rather than the underlying epistemic principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether the credentialing institution and the underlying falsifiability criterion require separate constraint stories.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__naturalist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__naturalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(anth_tr_t10, anthropological_record__naturalist_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(anth_tr_t20, anthropological_record__naturalist_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(anth_tr_t30, anthropological_record__naturalist_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(anth_tr_t40, anthropological_record__naturalist_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement(anth_tr_t50, anthropological_record__naturalist_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(anth_tr_t60, anthropological_record__naturalist_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__naturalist_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(anth_be_t10, anthropological_record__naturalist_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(anth_be_t20, anthropological_record__naturalist_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(anth_be_t30, anthropological_record__naturalist_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(anth_be_t40, anthropological_record__naturalist_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement(anth_be_t50, anthropological_record__naturalist_reading, base_extractiveness, 50, 0.56).
narrative_ontology:measurement(anth_be_t60, anthropological_record__naturalist_reading, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__naturalist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(anth_su_t10, anthropological_record__naturalist_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(anth_su_t20, anthropological_record__naturalist_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(anth_su_t30, anthropological_record__naturalist_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(anth_su_t40, anthropological_record__naturalist_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(anth_su_t50, anthropological_record__naturalist_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(anth_su_t60, anthropological_record__naturalist_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__naturalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(anthropological_record__naturalist_reading, 0.08).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__creationist_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposed from the natural-language label 'the anthropological record.' Each sibling reads the same underlying fossil/genetic/archaeological/oral-tradition record through a different epistemic framework, producing structurally distinct claims about what the record reveals and by what method it is knowable. The naturalist_reading (this file) authors ε=0.58 reflecting credentialing-gatekeeping extraction atop a genuine coordination function; the creationist_reading and indigenous_epistemology_reading siblings author their own independent ε values reflecting their own distinct extraction mechanisms and beneficiary/victim structures. These are not three measurements of one constraint — per the ε-invariance principle, differing extraction profiles under differing observables signal three constraints, not one contested metric.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
