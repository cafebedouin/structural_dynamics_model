% ============================================================================
% CONSTRAINT STORY: knowledge_legitimacy_biomedicine_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_knowledge_legitimacy_biomedicine_flat_control, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: knowledge_legitimacy_biomedicine_flat_control
 *   human_readable: Biomedical Knowledge Legitimacy Apparatus
 *   domain: epistemology/institutional/scientific
 *
 * SUMMARY:
 *   The biomedical knowledge legitimacy apparatus determines which claims
 *   about health, disease, and treatment are accepted as valid. It
 *   coordinates through peer review, institutional credentialing,
 *   methodological standards, and publication hierarchies. The apparatus
 *   genuinely filters dangerous misinformation, but it also concentrates
 *   epistemic authority in elite institutions and suppresses knowledge claims
 *   from independent researchers, practitioners, patient advocates, and
 *   Global South institutions. The claim/metric independence is preserved:
 *   the constraint is claimed as tangled_rope (genuine coordination with
 *   asymmetric extraction) while the metrics describe substantial and rising
 *   extraction, high suppression, and moderate theatricality. The engine
 *   measures the divergence between structural claim and operational reality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(knowledge_legitimacy_biomedicine_flat_control, 0.68).
domain_priors:suppression_score(knowledge_legitimacy_biomedicine_flat_control, 0.72).
domain_priors:theater_ratio(knowledge_legitimacy_biomedicine_flat_control, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(knowledge_legitimacy_biomedicine_flat_control, extractiveness, 0.68).
narrative_ontology:constraint_metric(knowledge_legitimacy_biomedicine_flat_control, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(knowledge_legitimacy_biomedicine_flat_control, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(knowledge_legitimacy_biomedicine_flat_control, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(knowledge_legitimacy_biomedicine_flat_control, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(knowledge_legitimacy_biomedicine_flat_control, tangled_rope).
narrative_ontology:human_readable(knowledge_legitimacy_biomedicine_flat_control, "Biomedical Knowledge Legitimacy Apparatus").
narrative_ontology:topic_domain(knowledge_legitimacy_biomedicine_flat_control, "epistemology/institutional/scientific").

domain_priors:requires_active_enforcement(knowledge_legitimacy_biomedicine_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(knowledge_legitimacy_biomedicine_flat_control, knowledge_legitimacy_biomedicine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(knowledge_legitimacy_biomedicine_flat_control, elite_research_institutions).
narrative_ontology:constraint_beneficiary(knowledge_legitimacy_biomedicine_flat_control, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(knowledge_legitimacy_biomedicine_flat_control, high_impact_journals).
narrative_ontology:constraint_beneficiary(knowledge_legitimacy_biomedicine_flat_control, credentialed_specialists).
narrative_ontology:constraint_victim(knowledge_legitimacy_biomedicine_flat_control, independent_researchers).
narrative_ontology:constraint_victim(knowledge_legitimacy_biomedicine_flat_control, practitioners_without_institutional_affiliation).
narrative_ontology:constraint_victim(knowledge_legitimacy_biomedicine_flat_control, patient_advocacy_researchers).
narrative_ontology:constraint_victim(knowledge_legitimacy_biomedicine_flat_control, global_south_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(knowledge_legitimacy_biomedicine_flat_control, patients_and_public).
narrative_ontology:constraint_victim(knowledge_legitimacy_biomedicine_flat_control, patients_and_public).
narrative_ontology:constraint_vindicates(knowledge_legitimacy_biomedicine_flat_control, peer_review_as_quality_guarantee).
narrative_ontology:constraint_vindicates(knowledge_legitimacy_biomedicine_flat_control, institutional_affiliation_as_competence_signal).
narrative_ontology:constraint_vindicates(knowledge_legitimacy_biomedicine_flat_control, publication_hierarchy_as_epistemic_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the infrastructure that determines what counts as legitimate biomedical knowledge: they host the laboratories, employ the credentialed researchers, sit on editorial boards, and administer peer review. Their institutional prestige is both the input to and the output of the legitimacy apparatus — a claim gains authority from institutional origin, and the institution gains authority from producing validated claims. They can route research through multiple validation channels and have the resources to meet every procedural requirement.
narrative_ontology:constraint_stakeholder(knowledge_legitimacy_biomedicine_flat_control, elite_research_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Fund substantial biomedical research and benefit from a legitimacy apparatus that privileges large-scale randomized controlled trials, institutional review boards, and publication in high-impact journals — all of which require capital and infrastructure they possess. The apparatus validates the knowledge claims that support their products while creating barriers to evidence that challenges profitability. They can commission research, fund academic positions, and influence which questions get investigated.
narrative_ontology:constraint_stakeholder(knowledge_legitimacy_biomedicine_flat_control, pharmaceutical_industry, beneficiary,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(knowledge_legitimacy_biomedicine_flat_control, pharmaceutical_industry, agenda_setter).

% Operate as gatekeepers determining which knowledge claims achieve wide circulation and citation. Their impact factors depend on selectivity, which creates incentive to publish findings from elite institutions and reject work from unknown sources regardless of methodological quality. They collect subscription revenue, article processing charges, and prestige from their position as arbiters. Their editorial boards overlap heavily with elite research institutions.
narrative_ontology:constraint_stakeholder(knowledge_legitimacy_biomedicine_flat_control, high_impact_journals, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(knowledge_legitimacy_biomedicine_flat_control, high_impact_journals, beneficiary).

% Hold medical degrees, research doctorates, and institutional appointments that grant them authority to make knowledge claims. The legitimacy apparatus protects their professional jurisdiction by requiring credentials and institutional affiliation as prerequisites for being heard. They benefit from reduced competition and enhanced authority, but are also constrained by the apparatus — their claims must conform to institutional norms and methodological orthodoxy to maintain legitimacy.
narrative_ontology:constraint_stakeholder(knowledge_legitimacy_biomedicine_flat_control, credentialed_specialists, beneficiary,
    powerful, biographical, constrained, national).

% Produce knowledge claims without institutional affiliation or credentialing. The legitimacy apparatus systematically discounts their work regardless of methodological rigor: peer reviewers reject submissions from unknown institutions, funding agencies require institutional overhead, and clinical practice guidelines ignore evidence not published in high-impact journals. They bear the cost of exclusion — their findings are treated as illegitimate by default, and they lack the resources to meet the procedural requirements that would grant legitimacy.
narrative_ontology:constraint_stakeholder(knowledge_legitimacy_biomedicine_flat_control, independent_researchers, payer,
    moderate, biographical, trapped, local).

% Practice medicine or conduct clinical observation outside academic medical centers. They accumulate practical knowledge from patient care but lack the institutional position to have that knowledge recognized as legitimate. The apparatus requires them to defer to published research from elite institutions even when their clinical experience suggests different conclusions. Their professional identity depends on biomedical legitimacy structures, but those structures systematically devalue their epistemic contributions.
narrative_ontology:constraint_stakeholder(knowledge_legitimacy_biomedicine_flat_control, practitioners_without_institutional_affiliation, payer,
    moderate, biographical, identity_locked, local).

% Investigate conditions affecting their communities, often with methodological sophistication but without institutional backing. The legitimacy apparatus treats patient-generated evidence as anecdotal regardless of sample size or rigor. They can organize to demand attention but cannot grant themselves legitimacy — that remains controlled by institutional gatekeepers. Their knowledge claims are systematically subordinated to institutional research even when institutional researchers have no lived experience of the conditions being studied.
narrative_ontology:constraint_stakeholder(knowledge_legitimacy_biomedicine_flat_control, patient_advocacy_researchers, payer,
    organized, biographical, constrained, national).

% Conduct biomedical research in institutions outside North America and Western Europe. The legitimacy apparatus privileges English-language publication, Global North institutional affiliations, and research questions relevant to wealthy populations. Their work is systematically undercited, their journals are excluded from major indexes, and their institutional affiliations carry less weight in peer review. They bear the cost of geographic and linguistic barriers that are presented as quality standards but function as extraction mechanisms.
narrative_ontology:constraint_stakeholder(knowledge_legitimacy_biomedicine_flat_control, global_south_researchers, payer,
    organized, generational, trapped, continental).

% Benefit from a system that filters out fraudulent medical claims and validates effective treatments through systematic evidence. They also bear costs when the legitimacy apparatus suppresses inconvenient findings, delays recognition of harms, or privileges profitable interventions over effective but unpatentable ones. They have no exit — they must trust the knowledge claims the apparatus validates because they lack the expertise and access to evaluate evidence themselves.
narrative_ontology:constraint_stakeholder(knowledge_legitimacy_biomedicine_flat_control, patients_and_public, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(knowledge_legitimacy_biomedicine_flat_control, patients_and_public, payer).

% Determine which evidence counts for drug approval, clinical guidelines, and public health policy. They rely on the legitimacy apparatus to identify valid knowledge claims but also shape what the apparatus validates by setting evidentiary standards. Their institutional position gives them analytical distance, but they are also captured by the same legitimacy structures they regulate — their staff are drawn from elite institutions and their standards privilege the kinds of evidence those institutions produce.
narrative_ontology:constraint_stakeholder(knowledge_legitimacy_biomedicine_flat_control, regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Study how knowledge claims gain legitimacy and how institutional structures shape what counts as evidence. They can see the full apparatus — the coordination function that filters noise from signal, and the extraction function that concentrates epistemic authority in institutions that benefit from that concentration. They document how methodological requirements that appear neutral systematically advantage well-resourced actors.
narrative_ontology:constraint_stakeholder(knowledge_legitimacy_biomedicine_flat_control, epistemology_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective problem of distinguishing reliable biomedical knowledge from noise, fraud, and error in a domain where false claims can kill. Peer review, replication requirements, methodological standards, and institutional credentialing create shared filters that prevent dangerous misinformation from being treated as medical fact.
% TRANSFER_FUNCTION: Moves epistemic authority and professional jurisdiction from independent researchers, practitioners, patient advocates, and Global South institutions to elite research institutions, pharmaceutical industry, high-impact journals, and credentialed specialists. The transfer is enforced through peer review rejection, funding denial, citation exclusion, and professional marginalization.
% ABSENT_VOICES: Independent researchers without institutional affiliation, practitioners whose clinical knowledge is dismissed as anecdotal, patient communities whose lived experience is treated as epistemically inferior, and Global South researchers whose work is systematically undercited. They would argue that methodological requirements are calibrated to exclude their contributions rather than to ensure quality, but they are structurally prevented from participating in the standard-setting process.
% DISAPPEARANCE_RATIONALE: If the legitimacy apparatus vanished overnight, biomedical knowledge production would reorganize around different validation mechanisms. Practitioners would rely more heavily on clinical experience, patient communities would generate and share evidence through alternative channels, independent researchers would publish without institutional gatekeeping, and the pharmaceutical industry would lose its ability to suppress inconvenient findings through journal control. The coordination function would need to be rebuilt, but the current distribution of epistemic authority would collapse.
% FOUNDING_PROBLEM: Early modern medicine lacked systematic methods to distinguish effective treatments from quackery, leading to widespread harm from unvalidated interventions. The apparatus was built to create shared standards for evidence evaluation and to concentrate expertise in institutions that could maintain those standards.
% FOUNDING_PROBLEM_CORROBORATION: Elite institutions and regulatory agencies attest the founding problem remains live and the apparatus is necessary to prevent dangerous misinformation. Independent researchers, patient advocates, and science studies scholars attest the founding problem has been substantially solved but the apparatus persists as a mechanism for concentrating professional jurisdiction and suppressing challenges to institutional authority. Meta-research on publication bias, replication failures, and industry influence from outside the benefiting institutions supports the shifted-function reading.
narrative_ontology:disappearance_verdict(knowledge_legitimacy_biomedicine_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(knowledge_legitimacy_biomedicine_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(knowledge_legitimacy_biomedicine_flat_control, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-18',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(knowledge_legitimacy_biomedicine_flat_control, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(knowledge_legitimacy_biomedicine_flat_control_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(knowledge_legitimacy_biomedicine_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(knowledge_legitimacy_biomedicine_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.68 at interval end) because methodological requirements and institutional gatekeeping systematically advantage well-resourced actors while excluding contributions from independent and non-Western researchers. Suppression is higher (0.72) because the apparatus actively enforces exclusion through peer review rejection, funding denial, and professional marginalization — alternatives to institutional validation are suppressed, not merely discouraged. Theater ratio is moderate (0.41): peer review and replication requirements perform real quality control functions, but a growing share of enforcement activity defends institutional jurisdiction rather than epistemic quality. Accessibility collapse is moderate (0.48) because alternative validation mechanisms exist but are systematically delegitimized. Resistance is substantial (0.58) because excluded groups actively contest the apparatus and document its biases. The measurement series shows extraction, theater, and suppression all rising over the interval as the apparatus has matured and hardened.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (elite institutions, journals, regulatory agencies) should compute the constraint as coordination they maintain to ensure quality. The payer seats (independent researchers, patient advocates, Global South researchers) should compute it as enforced extraction that systematically excludes their contributions regardless of methodological rigor. The engine derives this divergence from the structural data — the beneficiary/victim declarations, exit options, and power differentials. The claim does not adjudicate between these perspectives; it states what the author believes is structurally true of the substrate as a single constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Elite research institutions, pharmaceutical industry, high-impact journals, and credentialed specialists are structural beneficiaries — they collect epistemic authority, professional jurisdiction, and economic rents from controlling legitimacy. Independent researchers, non-affiliated practitioners, patient advocates, and Global South researchers are targets — they bear the cost of exclusion and must defer to institutional knowledge claims even when their own evidence suggests different conclusions. Patients and public are near symmetric: genuine benefit from filtering misinformation, diffuse cost from suppressed inconvenient findings. Regulatory agencies have analytical distance but are partly captured by the structures they regulate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinguishing reliable biomedical knowledge from dangerous misinformation — was genuine and the apparatus initially solved it. The mandatrophy question is whether that function has been substantially solved while the apparatus persists as extraction. Evidence: systematic publication bias favoring industry-funded research, replication crisis in high-impact journals, geographic and linguistic barriers that function as quality proxies, and documented cases of institutional suppression of inconvenient findings. The apparatus still performs coordination, but extraction has accumulated as institutional actors have learned to game the legitimacy mechanisms. This is the tangled_rope signature: both functions are present, neither has fully displaced the other, and the constraint requires active enforcement to maintain both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    methodological_standards_vs_institutional_barriers,
    'Are the methodological requirements that exclude non-institutional researchers calibrated to ensure epistemic quality, or to protect institutional jurisdiction?',
    'Systematic comparison of methodological rigor between accepted institutional research and rejected independent research. If rejected work shows equal or superior rigor, the standards function as barriers rather than quality filters.',
    'If standards are barriers, the apparatus is primarily extractive and the coordination function could be maintained with lower exclusion. If standards track quality, the extraction is the necessary cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodological_standards_vs_institutional_barriers, empirical, 'Whether methodological gatekeeping tracks epistemic quality or institutional advantage.').

omega_variable(
    peer_review_as_quality_vs_jurisdiction,
    'Does peer review primarily filter for methodological quality, or does it primarily enforce conformity to institutional norms and protect established paradigms?',
    'Analysis of peer review outcomes controlling for methodological rigor, institutional affiliation, and paradigm challenge. If affiliation and paradigm conformity predict acceptance independent of rigor, peer review functions as jurisdiction protection.',
    'If peer review is jurisdiction protection, the legitimacy apparatus is substantially extractive. If it tracks quality independent of institutional origin, the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peer_review_as_quality_vs_jurisdiction, empirical, 'Whether peer review filters quality or enforces institutional control.').

omega_variable(
    replication_crisis_as_coordination_failure,
    'Does the replication crisis in high-impact biomedical journals indicate that the legitimacy apparatus is failing at its coordination function, or that it prioritizes institutional prestige over epistemic reliability?',
    'Comparison of replication rates between high-impact institutional publications and lower-prestige independent publications. If replication rates are equal or higher outside elite journals, the apparatus privileges prestige over reliability.',
    'If the apparatus privileges prestige, extraction has substantially displaced coordination. If replication failures are evenly distributed, the coordination function is intact but imperfect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(replication_crisis_as_coordination_failure, empirical, 'Whether replication crisis indicates coordination failure or extraction priority.').

omega_variable(
    patient_knowledge_epistemic_status,
    'Is patient-generated evidence about treatment outcomes epistemically inferior to institutional research, or is it systematically devalued because it threatens institutional authority?',
    'Comparison of patient-reported outcome data with institutional clinical trial data for the same interventions. If patient data shows equal or superior predictive validity, its exclusion is jurisdictional rather than epistemic.',
    'If patient knowledge is systematically devalued for jurisdictional reasons, the apparatus extracts from patient communities by denying them epistemic standing. If it is genuinely inferior, the exclusion is coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(patient_knowledge_epistemic_status, conceptual, 'Whether patient knowledge exclusion is epistemic or jurisdictional.').

omega_variable(
    global_south_research_quality,
    'Are Global South research institutions systematically excluded because their work is lower quality, or because the legitimacy apparatus privileges Global North institutional affiliations independent of quality?',
    'Blinded review of research from Global South and Global North institutions controlling for methodological rigor. If Global South work is rejected at higher rates when institutional affiliation is visible, the apparatus extracts geographically.',
    'If exclusion is geographic rather than quality-based, the apparatus concentrates epistemic authority in wealthy regions through mechanisms that appear neutral. If quality tracks geography, the coordination function justifies the distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_south_research_quality, empirical, 'Whether Global South exclusion tracks quality or institutional geography.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(knowledge_legitimacy_biomedicine_flat_control, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(know_tr_t0, knowledge_legitimacy_biomedicine_flat_control, theater_ratio, 0, 0.18).
narrative_ontology:measurement(know_tr_t8, knowledge_legitimacy_biomedicine_flat_control, theater_ratio, 8, 0.23).
narrative_ontology:measurement(know_tr_t16, knowledge_legitimacy_biomedicine_flat_control, theater_ratio, 16, 0.29).
narrative_ontology:measurement(know_tr_t24, knowledge_legitimacy_biomedicine_flat_control, theater_ratio, 24, 0.34).
narrative_ontology:measurement(know_tr_t32, knowledge_legitimacy_biomedicine_flat_control, theater_ratio, 32, 0.38).
narrative_ontology:measurement(know_tr_t40, knowledge_legitimacy_biomedicine_flat_control, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(know_be_t0, knowledge_legitimacy_biomedicine_flat_control, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(know_be_t8, knowledge_legitimacy_biomedicine_flat_control, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(know_be_t16, knowledge_legitimacy_biomedicine_flat_control, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(know_be_t24, knowledge_legitimacy_biomedicine_flat_control, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(know_be_t32, knowledge_legitimacy_biomedicine_flat_control, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(know_be_t40, knowledge_legitimacy_biomedicine_flat_control, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(know_su_t0, knowledge_legitimacy_biomedicine_flat_control, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(know_su_t8, knowledge_legitimacy_biomedicine_flat_control, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(know_su_t16, knowledge_legitimacy_biomedicine_flat_control, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(know_su_t24, knowledge_legitimacy_biomedicine_flat_control, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(know_su_t32, knowledge_legitimacy_biomedicine_flat_control, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(know_su_t40, knowledge_legitimacy_biomedicine_flat_control, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(knowledge_legitimacy_biomedicine_flat_control, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
