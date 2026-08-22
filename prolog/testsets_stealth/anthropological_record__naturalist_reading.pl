% ============================================================================
% CONSTRAINT STORY: anthropological_record__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: anthropological_record__naturalist_reading
 *   human_readable: Naturalist Reading of the Anthropological Record (Credential-Gated Scientific Interpretation)
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This story instantiates the naturalist reading of the
 *   anthropological_record kernel as an institutional constraint: the
 *   standing arrangement under contest is the credential-gated regime through
 *   which the record (fossils, strata, genomes, artifacts) may be interpreted
 *   — scientific method with naturalist commitments, administered by
 *   associations, funders, and universities, and closed to supernatural
 *   causation and to non-credentialed interpreters. The claim and the metrics
 *   are independent authored facts: the claimed type states what I believe is
 *   structurally true (a genuine coordination function bound to asymmetric
 *   extraction), while the metrics describe the regime's actual operation,
 *   including its historical buildup. This file is one member of a
 *   three-story constraint family: the kernel decomposes into
 *   creationist_reading and indigenous_epistemology_reading, each with its
 *   own epsilon, beneficiaries, and victims; neither sibling's values are
 *   averaged into this file, and the family links are declared in
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - disciplinary_associations: agenda-setter (institutional/arbitrage) — owns journals and norms, collects dues and prestige
 *   - funding_bodies: agenda-setter (institutional/arbitrage) — propagates admissibility criteria through grant review
 *   - credentialed_palaeoanthropologists: primary beneficiary with enforcement duties (organized/identity_locked) — collects interpretive authority, supplies review labor
 *   - research_universities: primary beneficiary (institutional/arbitrage) — captures tuition, overhead, and collections custody
 *   - graduate_students_and_postdocs: primary target (powerless/trapped) — pays the credential toll with sunk costs
 *   - non_credentialed_interpreters: secondary target (moderate/constrained) — excluded from publication regardless of content
 *   - indigenous_knowledge_holders: secondary target and excluded voice (organized/identity_locked) — traditions read as data, remains held institutionally
 *   - scriptural_literalist_communities: secondary target (organized/identity_locked) — ruled inadmissible a priori, builds parallel institutions
 *   - philosophy_of_science_observers: analytical observer — sees the full structure including the gatekeeping asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__naturalist_reading, 0.68).
domain_priors:suppression_score(anthropological_record__naturalist_reading, 0.58).
domain_priors:theater_ratio(anthropological_record__naturalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__naturalist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__naturalist_reading, "Naturalist Reading of the Anthropological Record (Credential-Gated Scientific Interpretation)").
narrative_ontology:topic_domain(anthropological_record__naturalist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__naturalist_reading, '8f790a40-5a12-4a3b-98ca-c9568de11804').
narrative_ontology:cs_kernel_codification('8f790a40-5a12-4a3b-98ca-c9568de11804', distributed).
narrative_ontology:cs_authority_grounding('8f790a40-5a12-4a3b-98ca-c9568de11804', expertise).
narrative_ontology:cs_interpretation_layer_present('8f790a40-5a12-4a3b-98ca-c9568de11804').
narrative_ontology:cs_reading_relation('8f790a40-5a12-4a3b-98ca-c9568de11804', anthropological_record__creationist_reading, forecloses).
narrative_ontology:cs_reading_relation('8f790a40-5a12-4a3b-98ca-c9568de11804', anthropological_record__indigenous_epistemology_reading, influences).
narrative_ontology:cs_axiom('8f790a40-5a12-4a3b-98ca-c9568de11804', foundational, methodological_naturalism_exclusivity).
narrative_ontology:cs_axiom_status(methodological_naturalism_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('8f790a40-5a12-4a3b-98ca-c9568de11804', methodological_naturalism_exclusivity, instrumental).
narrative_ontology:cs_axiom('8f790a40-5a12-4a3b-98ca-c9568de11804', foundational, physical_evidence_over_testimony).
narrative_ontology:cs_axiom_status(physical_evidence_over_testimony, holdable).
narrative_ontology:cs_axiom_grounding('8f790a40-5a12-4a3b-98ca-c9568de11804', physical_evidence_over_testimony, empirically_contingent).
narrative_ontology:cs_reference_frame('8f790a40-5a12-4a3b-98ca-c9568de11804', empirically_self_correcting_canon).
narrative_ontology:cs_drift_state('8f790a40-5a12-4a3b-98ca-c9568de11804', contemporary_open_science_era, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('8f790a40-5a12-4a3b-98ca-c9568de11804', '').
narrative_ontology:cs_kernel_id(anthropological_record__naturalist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, credentialed_palaeoanthropologists).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, research_universities).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, disciplinary_associations).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, graduate_students_and_postdocs).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, non_credentialed_interpreters).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, indigenous_knowledge_holders).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, scriptural_literalist_communities).
narrative_ontology:constraint_vindicates(anthropological_record__naturalist_reading, methodological_naturalism).
narrative_ontology:constraint_vindicates(anthropological_record__naturalist_reading, common_descent).
narrative_ontology:constraint_vindicates(anthropological_record__naturalist_reading, deep_time_chronology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Professional bodies for anthropology, archaeology, and palaeoanthropology write the ethics codes, own the flagship journals, run the annual meetings, and set the credential norms that determine who may present interpretations of the record. They collect dues, submission fees, and prestige from the membership the norms create.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, disciplinary_associations, agenda_setter,
    institutional, generational, arbitrage, global).

% Grant agencies apply admissibility criteria in panel review that require natural-causation framing and method-standard designs; projects framed otherwise do not receive support. Their criteria propagate the frame into every funded excavation, lab, and publication, and they bear none of the training costs the frame imposes.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, funding_bodies, agenda_setter,
    institutional, generational, arbitrage, national).

% Hold faculty posts, edit journals, referee submissions, and speak with authority on human origins by virtue of doctoral credentials. Decades of training fuse their professional identity with the naturalist frame, so leaving it would mean leaving the profession. They also supply the unpaid review labor and absorb the publish-or-perish pressure the system generates.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, credentialed_palaeoanthropologists, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, credentialed_palaeoanthropologists, agenda_setter).

% Collect tuition from the degree programs that issue the credential, overhead on the grants the frame unlocks, and custody of the fossil, skeletal, and material collections the discipline studies. Prestige rankings reward them for administering the pipeline.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, research_universities, beneficiary,
    institutional, generational, arbitrage, global).

% Pay tuition and surrender years of low-paid training to obtain the credential that admits them to interpretation. By mid-program the sunk costs exceed any realistic alternative, so objection to the frame's terms is priced out; attrition quietly removes those who resist.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, graduate_students_and_postdocs, payer,
    powerless, immediate, trapped, national).

% Avocational archaeologists, independent researchers, and popular writers who produce site reports, syntheses, and hypotheses. Journals reject their work on provenance grounds regardless of content quality; their realistic outlets are self-publishing and amateur societies with negligible reach. Exit means abandoning the subject they care about.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, non_credentialed_interpreters, payer,
    moderate, biographical, constrained, national).

% Custodians of oral traditions and of the ancestral remains held in institutional collections. Their origin accounts enter the literature only as raw data or folklore, never as interpretation; consultation regimes and repatriation law give partial standing but no seat on editorial boards or grant panels. Their commitment to place and ancestry is constitutive, so exiting the dispute is not a live option.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, indigenous_knowledge_holders, payer,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, indigenous_knowledge_holders, excluded).

% Communities whose origin accounts are ruled inadmissible before examination because they invoke non-material causation. They respond by building parallel schools, museums, and presses rather than revising the commitment, and their children meet the naturalist frame as mandatory curriculum. Their identity is fused with the rejected account.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, scriptural_literalist_communities, payer,
    organized, civilizational, identity_locked, global).

% Scholars of demarcation, epistemic authority, and the sociology of science who analyze how the discipline decides what counts as knowing the record. They take no side in origin disputes and can see the whole structure, including the difference between the evidence and the institutions that license its interpretation.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, philosophy_of_science_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__naturalist_reading, research_universities).
narrative_ontology:fixing_cost_class(anthropological_record__naturalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Shared methodological standards — falsifiability, replicability, stratigraphic and genetic dating conventions, natural-causation framing — let fragmentary evidence (fossils, strata, genomes, artifacts) be combined into cumulative, testable accounts of human origins that different labs, generations, and countries can check against each other.
% TRANSFER_FUNCTION: Moves interpretive authority over human origins, together with the material flows attached to it (tuition, grant overhead, publication credit, custody of remains and collections), from uncredentialed interpreters and source communities toward credentialed specialists and their institutions.
% ABSENT_VOICES: Indigenous knowledge holders appear in the conversation only as objects of study or petitioners for repatriation, never as interpreters with standing; non-credentialed researchers appear only as rejected submissions; scriptural communities appear only as subjects of refutation. All three would object that admissibility is decided a priori, and none sits on editorial boards, hiring committees, or grant panels.
% DISAPPEARANCE_RATIONALE: If the credential-gated naturalist regime vanished overnight, human-origins discourse would fragment into competing epistemic communities with no shared adjudication: curricula, museum governance, funding criteria, repatriation negotiations, and the careers built on the credential pipeline would all reorganize. The physical evidence would remain, but every arrangement built on licensed interpretation of it would rearrange.
% FOUNDING_PROBLEM: In the nineteenth century, claims about human origins were dominated by scriptural chronologies and untestable speculation; the arrangement was built to establish a shared, disciplined method capable of adjudicating origin claims against physical evidence — stratigraphy first, then fossils, then genetics.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: predictive successes checked by independent laboratories (ancestral hominin forms found at predicted strata and dates; archaic-admixture signals in ancient DNA confirmed by competing sequencing groups), and judicial acceptance of the method's adjudicatory role in testimony that included philosophers of science outside the discipline. Indigenous and literalist communities dispute the method's exclusivity, not the existence of the original adjudication problem; no party attests that the problem is dead.
narrative_ontology:disappearance_verdict(anthropological_record__naturalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__naturalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__naturalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(anthropological_record__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__naturalist_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderately high (0.68 at interval end) because the credential requirement prices interpretation behind tuition and years of training, publication rights are reserved to the credentialed, and source communities' knowledge and ancestral remains entered institutional custody largely uncompensated — yet the regime renders real services (dating infrastructure, replication, cumulative synthesis), which keeps it short of pure-extraction territory. Suppression (0.58) is institutional rather than coercive: nothing stops anyone from believing or saying otherwise, but journals, hiring, and funding close against non-naturalist framing and uncredentialed authorship. Theater is low-moderate (0.28): peer review and methods sections are mostly functional, with a performative fringe of rigor-signaling. Accessibility collapse is 0.52 — inside the institutions, alternatives collapse almost completely once the admissibility rule is understood; culturally, parallel institutions persist, so collapse is far from total. Resistance is 0.60: a century of literalist counter-institution-building, indigenous sovereignty campaigns over remains and data, avocational-researcher grievances, and open-science pressure. The temporal series run on one shared grid (1859, 1900, 1925, 1953, 1980, 2005, 2026) with every tracked metric authored at every point. The suppression_requirement series is authored deliberately because this story specifically traces enforcement-capacity change: enforcement machinery built up through professionalization (1900), the statute-and-trial era (1925), the modern synthesis (1953), and grant-and-tenure consolidation (1980), peaked around 2005, and has partially softened by 2026 as preprints, open access, citizen science, and co-governance agreements lower the walls. Base properties reflect the interval-end state.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (associations, funders) should compute a coordination-first picture: from where they sit, the arrangement is quality control they administer and the extraction is invisible overhead collected elsewhere. The trapped payer seat (graduate students) and the identity-locked targets (indigenous holders, literalist communities) should compute a heavily extracted picture: the same rules that look like standards from the editor's desk look like a closed door from the submission queue. The credentialed beneficiary seat sits between — collecting authority while paying review labor and publish-or-perish pressure — which is why its directionality carries an override. The engine computes these divergences from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: universities and associations sit near the subsidy end (low d), amplified by arbitrage-grade exit — they administer the game and could reframe it. Credentialed palaeoanthropologists receive an explicit override (d = 0.18 rather than a near-zero derived value) because they are dual-positioned: they collect interpretive authority but also bear the enforcement costs (referee labor, career precarity), so they are less than full beneficiaries. Targets cluster near the full-target end: graduate students are powerless and trapped (highest d); indigenous knowledge holders and literalist communities are identity-locked, which pins them near full-target even where they are organized; non-credentialed interpreters are constrained but mobile enough to soften d slightly. Scope is continental-to-global for the institutional seats and regional for source communities, so verification difficulty amplifies extraction most where the regime operates at largest scale.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — adjudicating origin claims against physical evidence — is live: new evidence (ancient DNA, new hominin finds) keeps arriving and keeps requiring exactly the adjudication the arrangement was built for, so no mandatrophy is declared and no sunset applies. The classification matters in both directions. Reading the regime as pure coordination (rope) would erase the credential rents, the uncompensated custody of remains, and the a priori closure against source-community epistemologies; reading it as pure extraction (snare) would erase the demonstrated coordination value — predictive success, replication, cumulative synthesis — that no parallel institution has matched. The tangled-rope classification holds both halves: the same standards that make cumulative knowledge possible are the ones that gate who may interpret, and the gate is where the asymmetry lives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading (naturalist_reading) of the anthropological_record kernel; what would change structurally if a sibling reading (creationist_reading or indigenous_epistemology_reading) became the operative institutional frame?',
    'Comparative analysis across the three reading files in the family: identify which seats invert (under the sibling frames, credentialed academics become the excluded or targeted party and the current targets become the agenda-setters), and locate the disagreement in the admissibility criterion each reading fixes.',
    'If a sibling frame were adopted, the beneficiary/victim sets invert and this file''s classification ceases to describe the operative arrangement; the family, not any single file, carries the full structure of the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    credential_filter_vs_rent,
    'Is the credential requirement a genuine competence filter whose screening value matches what it costs to pass, or a rent-extracting barrier whose cost exceeds its filtering contribution?',
    'Outcome comparison of credentialed versus avocational contributions to the record: validated site discoveries, correct identifications, and reproducible syntheses produced outside the credential pipeline, weighed against error rates, controlling for access to equipment and collections.',
    'If filtering value approximates barrier cost, the gatekeeping component moves toward pure coordination; if rent dominates, the regime shifts toward the snare boundary and rate-limiting remedies (open review tiers, avocational accreditation) become warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_filter_vs_rent, empirical, 'Whether the credential gate filters competence or collects rent beyond it.').

omega_variable(
    suppression_scope_ambiguity,
    'Is the measured suppression confined to institutional exclusion (publication, hiring, funding) or does it extend to legal and curricular coercion over belief and teaching?',
    'Compare enforcement inside versus outside institutional boundaries across the interval: the statute-and-trial era shows legally coerced curricula running in both directions at different times and places, while the contemporary regime enforces mainly through editorial and funding gates.',
    'If suppression is institutional-only, effective extraction concentrates on the seats inside or seeking entry; if it extends to curricular or legal coercion, the trapped and identity-locked seats carry amplified effective extraction and the regime approaches the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_scope_ambiguity, empirical, 'Scope of the regime''s suppressive force: institutional gates versus broader coercion.').

omega_variable(
    core_apparatus_decomposability,
    'Can the empirical core of this reading (evolutionary origins knowable by method) be separated from its institutional apparatus (the credentialing and publication monopoly), such that they are two constraints rather than one?',
    'Examine open-science and preprint-native corners of the discipline where the core persists under weaker gatekeeping: if predictive and cumulative performance holds while the credential monopoly loosens, the core and the apparatus are separable and warrant separate stories with distinct epsilon values.',
    'If separable, the near-uncontested core approaches mountain-like status while the apparatus carries the extraction alone; if inseparable, part of the measured extraction is the price of the coordination itself and stays attributed to this single constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(core_apparatus_decomposability, conceptual, 'Whether the reading''s empirical core and its credentialing apparatus are one constraint or two.').

omega_variable(
    oral_tradition_admissibility,
    'Can sustained oral tradition function as admissible evidence within the naturalist frame — as corroborated paleoenvironmental and settlement records — or is it categorically excluded by the frame''s admissibility rule?',
    'Audit documented cases where oral traditions have been tested against geological, archaeological, or genomic findings (sea-level recall, event-dated landscapes, kinship-structured sample provenance) and track whether corroboration converts tradition into citable evidence or leaves it as anecdote.',
    'If admissible in practice, the regime''s closure against indigenous epistemology is partial and measured extraction from that seat is lower than declared; if categorical, the exclusion is structural and the indigenous seat''s effective extraction is understated at the declared values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oral_tradition_admissibility, empirical, 'Whether the frame''s admissibility rule excludes oral tradition absolutely or admits corroborated tradition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__naturalist_reading, 1859, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t1859, anthropological_record__naturalist_reading, theater_ratio, 1859, 0.1).
narrative_ontology:measurement_basis(anth_tr_t1859, observed).
narrative_ontology:measurement(anth_tr_t1900, anthropological_record__naturalist_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement_basis(anth_tr_t1900, observed).
narrative_ontology:measurement(anth_tr_t1925, anthropological_record__naturalist_reading, theater_ratio, 1925, 0.22).
narrative_ontology:measurement_basis(anth_tr_t1925, observed).
narrative_ontology:measurement(anth_tr_t1953, anthropological_record__naturalist_reading, theater_ratio, 1953, 0.25).
narrative_ontology:measurement_basis(anth_tr_t1953, observed).
narrative_ontology:measurement(anth_tr_t1980, anthropological_record__naturalist_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement_basis(anth_tr_t1980, observed).
narrative_ontology:measurement(anth_tr_t2005, anthropological_record__naturalist_reading, theater_ratio, 2005, 0.33).
narrative_ontology:measurement_basis(anth_tr_t2005, observed).
narrative_ontology:measurement(anth_tr_t2026, anthropological_record__naturalist_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(anth_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(anth_be_t1859, anthropological_record__naturalist_reading, base_extractiveness, 1859, 0.3).
narrative_ontology:measurement_basis(anth_be_t1859, observed).
narrative_ontology:measurement(anth_be_t1900, anthropological_record__naturalist_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement_basis(anth_be_t1900, observed).
narrative_ontology:measurement(anth_be_t1925, anthropological_record__naturalist_reading, base_extractiveness, 1925, 0.48).
narrative_ontology:measurement_basis(anth_be_t1925, observed).
narrative_ontology:measurement(anth_be_t1953, anthropological_record__naturalist_reading, base_extractiveness, 1953, 0.55).
narrative_ontology:measurement_basis(anth_be_t1953, observed).
narrative_ontology:measurement(anth_be_t1980, anthropological_record__naturalist_reading, base_extractiveness, 1980, 0.62).
narrative_ontology:measurement_basis(anth_be_t1980, observed).
narrative_ontology:measurement(anth_be_t2005, anthropological_record__naturalist_reading, base_extractiveness, 2005, 0.66).
narrative_ontology:measurement_basis(anth_be_t2005, observed).
narrative_ontology:measurement(anth_be_t2026, anthropological_record__naturalist_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(anth_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t1859, anthropological_record__naturalist_reading, suppression_requirement, 1859, 0.2).
narrative_ontology:measurement_basis(anth_su_t1859, observed).
narrative_ontology:measurement(anth_su_t1900, anthropological_record__naturalist_reading, suppression_requirement, 1900, 0.35).
narrative_ontology:measurement_basis(anth_su_t1900, observed).
narrative_ontology:measurement(anth_su_t1925, anthropological_record__naturalist_reading, suppression_requirement, 1925, 0.55).
narrative_ontology:measurement_basis(anth_su_t1925, observed).
narrative_ontology:measurement(anth_su_t1953, anthropological_record__naturalist_reading, suppression_requirement, 1953, 0.6).
narrative_ontology:measurement_basis(anth_su_t1953, observed).
narrative_ontology:measurement(anth_su_t1980, anthropological_record__naturalist_reading, suppression_requirement, 1980, 0.66).
narrative_ontology:measurement_basis(anth_su_t1980, observed).
narrative_ontology:measurement(anth_su_t2005, anthropological_record__naturalist_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement_basis(anth_su_t2005, observed).
narrative_ontology:measurement(anth_su_t2026, anthropological_record__naturalist_reading, suppression_requirement, 2026, 0.58).
narrative_ontology:measurement_basis(anth_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__naturalist_reading, information_standard).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__creationist_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% Constraint family from kernel decomposition (epsilon-invariance): the colloquial label 'what the anthropological record reveals' conflates three structurally distinct claims, written as three stories. This file (naturalist_reading) carries the credential-gated scientific arrangement with moderate-high extraction; creationist_reading carries the scriptural/design arrangement with its own beneficiary and victim structure; indigenous_epistemology_reading carries the oral-tradition arrangement likewise. Each story has a single stable epsilon over its own standing arrangement; the upstream naturalist file influences the indigenous sibling's operating environment (funding criteria, museum custody, publication standing) without foreclosing it, and forecloses the creationist sibling within any single methodological framework. Neither sibling's epsilon appears in this file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(anthropological_record__naturalist_reading, organized, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
