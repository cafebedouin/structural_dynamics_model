% ============================================================================
% CONSTRAINT STORY: notability_guidelines__inclusionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__inclusionist_reading, []).

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
 *   constraint_id: notability_guidelines__inclusionist_reading
 *   human_readable: Wikipedia Notability Guideline (WP:N) as Gatekeeping Apparatus Excluding Marginalized Knowledge
 *   domain: digital_commons_governance/knowledge_infrastructure
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested WP:N kernel: the
 *   inclusionist reading, which holds that the notability guideline's
 *   'reliable, independent, secondary sources' test operates as a structural
 *   gatekeeping apparatus that systematically converts pre-existing
 *   media-coverage bias into epistemic exclusion. This is presented as a
 *   clean, self-contained constraint with its own stable extraction profile —
 *   it does NOT describe or average over the sibling deletionist reading
 *   (WP:N as necessary quality filter) or the deliberative reading (WP:N as
 *   perpetual negotiating process). Those are separate constraints, generated
 *   separately, and linked here only via the cs_structure reading relations
 *   and omega variables, per the ε-invariance principle: the same guideline
 *   text supports multiple structurally distinct constraints depending on
 *   which premise a reader brings to it, and folding them together would
 *   corrupt a single stable ε.
 *
 * KEY AGENTS:
 *   - institutional_knowledge_producers: primary beneficiary (institutional/arbitrage) — pre-existing coverage converts costlessly into notability currency
 *   - veteran_wikipedia_editors: agenda_setter (organized/arbitrage) — interpret and enforce the guideline at AfD
 *   - indigenous_knowledge_keepers: primary target (powerless/trapped) — oral knowledge cannot retroactively generate print citation trails
 *   - global_south_local_historians, women_in_stem_biography_subjects, diaspora_community_archivists: secondary targets bearing the same sourcing bias at different power levels
 *   - oral_tradition_cultures: excluded — entire knowledge systems structurally unrepresented in the deliberative process
 *   - wikimedia_foundation: analytical observer — commissions the equity research documenting the gap without altering the standard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, 0.71).
domain_priors:suppression_score(notability_guidelines__inclusionist_reading, 0.68).
domain_priors:theater_ratio(notability_guidelines__inclusionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__inclusionist_reading, snare).
narrative_ontology:human_readable(notability_guidelines__inclusionist_reading, "Wikipedia Notability Guideline (WP:N) as Gatekeeping Apparatus Excluding Marginalized Knowledge").
narrative_ontology:topic_domain(notability_guidelines__inclusionist_reading, "digital_commons_governance/knowledge_infrastructure").

domain_priors:requires_active_enforcement(notability_guidelines__inclusionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__inclusionist_reading, '5d3d34c0-cf85-4b0f-9ec0-ccabc769e4d4').
narrative_ontology:cs_kernel_codification('5d3d34c0-cf85-4b0f-9ec0-ccabc769e4d4', formalized).
narrative_ontology:cs_authority_grounding('5d3d34c0-cf85-4b0f-9ec0-ccabc769e4d4', practice).
narrative_ontology:cs_interpretation_layer_present('5d3d34c0-cf85-4b0f-9ec0-ccabc769e4d4').
narrative_ontology:cs_reading_relation('5d3d34c0-cf85-4b0f-9ec0-ccabc769e4d4', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5d3d34c0-cf85-4b0f-9ec0-ccabc769e4d4', notability_guidelines__deliberative_reading, influences).
narrative_ontology:cs_axiom('5d3d34c0-cf85-4b0f-9ec0-ccabc769e4d4', foundational, source_availability_encodes_structural_bias_not_merit).
narrative_ontology:cs_axiom_status(source_availability_encodes_structural_bias_not_merit, holdable).
narrative_ontology:cs_axiom_grounding('5d3d34c0-cf85-4b0f-9ec0-ccabc769e4d4', source_availability_encodes_structural_bias_not_merit, empirically_contingent).
narrative_ontology:cs_axiom('5d3d34c0-cf85-4b0f-9ec0-ccabc769e4d4', secondary, neutral_rule_application_can_produce_disparate_epistemic_outcomes).
narrative_ontology:cs_axiom_status(neutral_rule_application_can_produce_disparate_epistemic_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('5d3d34c0-cf85-4b0f-9ec0-ccabc769e4d4', neutral_rule_application_can_produce_disparate_epistemic_outcomes, conventional).
narrative_ontology:cs_reference_frame('5d3d34c0-cf85-4b0f-9ec0-ccabc769e4d4', verifiability_as_neutral_arbiter).
narrative_ontology:cs_drift_state('5d3d34c0-cf85-4b0f-9ec0-ccabc769e4d4', post_equity_research_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5d3d34c0-cf85-4b0f-9ec0-ccabc769e4d4', '').
narrative_ontology:cs_kernel_id(notability_guidelines__inclusionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, established_media_organizations).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, veteran_wikipedia_editors).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, indigenous_knowledge_keepers).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, global_south_local_historians).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, women_in_stem_biography_subjects).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, diaspora_community_archivists).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, oral_tradition_cultures).
narrative_ontology:constraint_vindicates(notability_guidelines__inclusionist_reading, verifiability_over_truth_doctrine).
narrative_ontology:constraint_vindicates(notability_guidelines__inclusionist_reading, encyclopedic_neutrality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major newspapers, university presses, and Western broadcast media already generate the 'reliable, independent, secondary sources' the guideline demands. Their existing output is pre-converted into notability currency at no additional cost, letting their subjects, framings, and coverage priorities become the encyclopedia's default map of what is worth knowing.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, institutional_knowledge_producers, beneficiary,
    institutional, generational, arbitrage, global).

% Long-tenured editors, many embedded in Anglophone/Western academic and journalistic networks, interpret and enforce WP:N at Articles for Deletion. They know which sourcing patterns survive scrutiny and which get challenged, giving them structural control over whose knowledge counts as 'established' without needing to change the written rule at all.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, veteran_wikipedia_editors, agenda_setter,
    organized, biographical, arbitrage, global).

% Hold knowledge transmitted through oral tradition, community record, and land-based practice that is rarely written up by outlets the guideline recognizes as 'reliable secondary sources.' Articles documenting their histories, figures, or practices are repeatedly nominated for deletion as 'unverifiable' even when the knowledge is verifiable within its own community's epistemic practice. They cannot manufacture the citation trail the rule demands after the fact.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, indigenous_knowledge_keepers, payer,
    powerless, generational, trapped, local).

% Document regional figures, events, and institutions covered extensively in local-language press that Western-dominant editorial pools treat as less 'reliable' or simply cannot evaluate. Their sourcing is real but structurally invisible to the enforcement apparatus, so their contributions face disproportionate deletion regardless of underlying factual accuracy.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, global_south_local_historians, payer,
    powerless, biographical, constrained, regional).

% Accomplished researchers and professionals whose achievements are systematically under-covered by media that historically prioritized male counterparts. The notability bar, calibrated to the coverage patterns of a biased media history, reproduces that bias by treating thinner press coverage as evidence of lesser notability rather than evidence of unequal coverage.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, women_in_stem_biography_subjects, payer,
    moderate, biographical, constrained, national).

% Volunteer editors documenting migrant and diaspora community history, institutions, and figures using community newsletters, ethnic press, and oral history projects. These sources are routinely dismissed as insufficiently independent or reliable by deletion discussions dominated by editors unfamiliar with the source ecosystem, and the archivists rarely participate in AfD debates themselves due to language and process barriers.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, diaspora_community_archivists, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__inclusionist_reading, diaspora_community_archivists, excluded).

% Entire knowledge systems built on transmission through elders, ceremony, and community consensus rather than durable print. They have no seat at AfD, no representative editor pool proportional to their populations, and no mechanism to contest a rule written around print-literate, archive-heavy societies.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, oral_tradition_cultures, excluded,
    powerless, civilizational, trapped, global).

% Funds and hosts the infrastructure but formally defers content policy to the editing community. Commissions equity and diversity studies documenting the gap yet has not altered the sourcing standard itself, treating the guideline as community self-governance outside its remit.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikimedia_foundation, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:fixing_cost_class(notability_guidelines__inclusionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: WP:N genuinely solves a real problem: without some verifiability floor, the encyclopedia would fill with unverifiable claims, promotional content, and hoaxes that no volunteer force could fact-check at scale.
% TRANSFER_FUNCTION: The guideline transfers epistemic legitimacy and visibility from communities whose knowledge circulates through oral, local-language, or community-internal channels to institutions and individuals whose activity is already captured by Western/Anglophone print and broadcast media — without any transfer of money, the currency moved is durable recognition and discoverability.
% ABSENT_VOICES: Indigenous knowledge keepers, Global South historians, and diaspora archivists rarely participate in AfD discussions in numbers proportional to their stake — language barriers, unfamiliarity with wiki-bureaucratic process, and the sheer time cost of contesting deletion mean the loudest voices in the room are structurally the guideline's beneficiaries, not its victims.
% DISAPPEARANCE_RATIONALE: Editors who benefit from the current sourcing regime argue the encyclopedia would collapse into unmanageable noise without it (world_rearranges toward chaos); excluded communities and inclusionist editors argue that removing or substantially loosening the guideline would simply let already-real, already-documented knowledge into the commons that the current filter arbitrarily excludes (world_rearranges toward equity). Both camps agree the world would rearrange — they dispute the direction and value of the rearrangement, which is why this constraint exists as a live kernel dispute rather than a settled fact.
% FOUNDING_PROBLEM: Wikipedia in its early growth years faced flooding by vanity pages, promotional content, hoaxes, and unverifiable claims that threatened both its credibility and its volunteer editors' capacity to police content; WP:N was built to give deletion discussions an objective-seeming, source-based test.
% FOUNDING_PROBLEM_CORROBORATION: Wikimedia Foundation's own diversity and equity research (e.g. gender gap and Global South participation studies) corroborates, from outside the beneficiary set of veteran editors, that the guideline's practical effect diverges sharply from its stated verifiability rationale — documenting systematic underrepresentation that tracks source-availability bias rather than actual notability. Academic media studies scholars have independently reached the same conclusion. Veteran editors themselves largely maintain the founding problem (content quality control) remains live and sufficient justification on its own.
narrative_ontology:disappearance_verdict(notability_guidelines__inclusionist_reading, contested).
narrative_ontology:founding_problem_status(notability_guidelines__inclusionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__inclusionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(notability_guidelines__inclusionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__inclusionist_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__inclusionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(notability_guidelines__inclusionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.42 to 0.71) as the encyclopedia's growth converts more of the world's institutional attention into notability precedent, entrenching source patterns that reflect historical media bias rather than correcting for it — each AfD precedent citing a prior AfD precedent hardens the exclusion. Theater ratio also rises (0.20 to 0.42): a growing share of deletion-discussion activity performs neutral, rule-following adjudication ('just enforcing WP:N, nothing personal') while the substantive effect is disproportionate removal of marginalized-community content, which is the theatrical layer masking a directional outcome. Suppression rises moderately (0.45 to 0.68) as deletion enforcement has professionalized into semi-formal patrol structures (new page patrol, AfD regulars) that make contesting removal procedurally costly for editors unfamiliar with wiki-bureaucratic norms.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (veteran editors), the guideline is neutral rule enforcement applied consistently regardless of subject. From the payer seats (indigenous knowledge keepers, diaspora archivists), the identical rule, applied identically, produces systematically disparate outcomes because the underlying evidentiary landscape it draws on was never neutral. The engine computes these as different per-seat classifications from the same structural facts — the divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional knowledge producers and veteran editors sit near the beneficiary end of directionality: their coverage or interpretive labor is what the guideline rewards, and their exit options (arbitrage — they can always find or manufacture qualifying sources, or simply outlast a deletion debate) insulate them from ever bearing the guideline's cost. The victim groups sit near the full-target end: indigenous knowledge keepers and oral tradition cultures are trapped — they cannot generate the retroactive citation trail the rule demands, no matter how verifiable their knowledge is within its own tradition. Diaspora archivists and Global South historians are constrained rather than fully trapped, since some qualifying local-language sourcing exists but is discounted by an editor pool unequipped to evaluate it. Women in STEM sit at moderate power with constrained exit: they have more resources to contest deletion than powerless groups but are still bound by the underlying media-coverage asymmetry the guideline launders into 'objective' notability.
 *
 * MANDATROPHY ANALYSIS:
 *   The inclusionist reading does not claim WP:N has no coordination function — it explicitly names the genuine problem (unverifiable, promotional, hoax content at scale) the guideline was built to solve. What it disputes is whether that founding problem, as currently operationalized through a fixed sourcing standard calibrated to institutional/Western media output, remains proportionate to the harm it now causes. This prevents mislabeling the whole apparatus as pure extraction: the coordination function is real and named (vindicated_propositions), but the classification tracks that the guideline's actual operation, per this reading's structural data, produces asymmetric extraction against marginalized knowledge — hence snare rather than rope, with the enforcement requirement and named victim set the schema demands for that classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    guideline_text_vs_enforcement_pattern_locus,
    'Is the exclusion this reading identifies located in the text of WP:N itself, or in how a demographically narrow editor pool interprets and enforces that text at AfD?',
    'Compare deletion outcomes for comparably-sourced articles about Western vs. non-Western/marginalized-community subjects, controlling for objective source quality and quantity, across different composition of AfD participant pools.',
    'If the exclusion tracks editor composition rather than the guideline text, the fix is editor-pool diversification (a deliberative-reading remedy); if it tracks the text''s structural preference for institutional/print sourcing, the fix requires rewriting the guideline itself (an inclusionist-reading remedy). This determines which sibling reading''s proposed intervention would actually address the harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guideline_text_vs_enforcement_pattern_locus, empirical, 'Whether exclusion is located in guideline text or enforcement demographics.').

omega_variable(
    natural_versus_constructed_notability_standard,
    'Is a source-verifiability floor an unavoidable structural necessity for any large-scale collaborative encyclopedia (making some version of WP:N inevitable regardless of who writes it), or is the specific ''reliable secondary source'' calibration a contingent, historically-biased construction that could be replaced by an equally rigorous but differently-calibrated standard?',
    'Study alternative verifiability frameworks used by other large-scale knowledge commons (e.g., participatory oral-history archives with community-attestation verification models) to see whether comparable reliability can be achieved without print/broadcast-centric sourcing.',
    'If some notability floor is unavoidable, this reading''s snare classification applies to the SPECIFIC calibration, not to gatekeeping as such — narrowing the intervention target. If the specific calibration is fully contingent, the case for the deletionist reading''s necessity claim weakens substantially.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_versus_constructed_notability_standard, conceptual, 'Whether some notability standard is structurally necessary versus this one being a contingent, biased construction.').

omega_variable(
    reading_selection_evidence_basis,
    'What specific signals guided selecting the inclusionist framing (structural gatekeeping/exclusion) over the deletionist framing (necessary quality filter) or the deliberative framing (evolving negotiation) as the operative lens for this story?',
    'The Wikimedia Foundation''s own published equity research (gender gap studies, Global South editor participation studies) and independent academic media-studies literature on systemic bias in AfD outcomes were treated as the corroborating signal for this reading''s beneficiary/victim structure, per the founding_problem_corroboration requirement that genealogy claims not rest solely on beneficiary self-report.',
    'Had the deletionist framing been selected instead, the same guideline text would classify closer to rope or mountain (a necessary, low-extraction quality control mechanism), with no named victims and no snare gate triggered — demonstrating that the classification is a function of which reading is instantiated, not an intrinsic property of the guideline text alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_evidence_basis, conceptual, 'Conceptual under-determination between the three kernel readings and the evidentiary basis for selecting this one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__inclusionist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__inclusionist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nota_tr_t4, notability_guidelines__inclusionist_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(nota_tr_t8, notability_guidelines__inclusionist_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(nota_tr_t12, notability_guidelines__inclusionist_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(nota_tr_t16, notability_guidelines__inclusionist_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__inclusionist_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__inclusionist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(nota_be_t4, notability_guidelines__inclusionist_reading, base_extractiveness, 4, 0.49).
narrative_ontology:measurement(nota_be_t8, notability_guidelines__inclusionist_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(nota_be_t12, notability_guidelines__inclusionist_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(nota_be_t16, notability_guidelines__inclusionist_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__inclusionist_reading, base_extractiveness, 20, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__inclusionist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(nota_su_t4, notability_guidelines__inclusionist_reading, suppression_requirement, 4, 0.51).
narrative_ontology:measurement(nota_su_t8, notability_guidelines__inclusionist_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(nota_su_t12, notability_guidelines__inclusionist_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(nota_su_t16, notability_guidelines__inclusionist_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(nota_su_t20, notability_guidelines__inclusionist_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__inclusionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(notability_guidelines__inclusionist_reading, 0.08).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, notability_guidelines__deletionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, notability_guidelines__deliberative_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the notability_guidelines kernel (same WP:N text, three structurally distinct constraints). deletionist_reading treats the identical guideline as a low-extraction quality filter (rope/mountain-adjacent); deliberative_reading treats it as an ongoing negotiation process without a fixed extraction profile; this inclusionist_reading treats it as an extractive snare with a named beneficiary (institutional knowledge producers) and named victims (marginalized-knowledge communities). Per the ε-invariance principle, these are NOT the same constraint viewed from three angles — each has its own stable ε and its own classification, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
