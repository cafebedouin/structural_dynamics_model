% ============================================================================
% CONSTRAINT STORY: anthropological_record__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: anthropological_record__naturalist_reading
 *   human_readable: Naturalist Reading of the Anthropological Record
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   The naturalist reading of the anthropological record asserts that human
 *   origins (evolution, migration) are knowable exclusively through the
 *   scientific method, and that this method yields a materialist account. As
 *   a constraint, it operates through the institutional authority of
 *   credentialed science: funding, publication, academic positions, and
 *   curricular control are allocated to those who accept and enforce the
 *   naturalist framework. Supernatural causation (creationist reading) and
 *   relational-oral epistemologies (indigenous reading) are excluded not
 *   merely as false but as outside the domain of legitimate knowledge. The
 *   constraint presents itself as a mountain — the record *is* materialist
 *   and the scientific method *is* the unique key — but its persistence
 *   depends on active enforcement (credentialing, peer review, funding gates)
 *   and it extracts substantial rents (epistemic authority, material
 *   resources) for the credentialed establishment while suppressing
 *   non-credentialed interpreters. The claimed type (tangled_rope) reflects
 *   the authoring seat's judgment that the constraint has a genuine
 *   coordination function (standardizing evidence evaluation, pooling
 *   cognitive labor) but also an asymmetric extraction function (the
 *   establishment collects the gains of that coordination).
 *
 * KEY AGENTS:
 *   - scientific_establishment: agenda_setter (institutional/analytical) — sets the epistemic rules, controls funding and publication
 *   - credentialed_scientists: beneficiary (organized/biographical) — collect epistemic authority, funding, career advancement
 *   - non_credentialed_interpreters: payer (powerless/biographical) — excluded from legitimacy, funding, publication; bear costs of marginalization
 *   - indigenous_communities: payer (powerless/generational) — their epistemologies suppressed, their relationship to ancestors and place delegitimized
 *   - creationist_proponents: excluded (moderate/biographical) — structurally barred from the naturalist framework; would object but are not in the conversation
 *   - analytical_observer: observer (analytical/civilizational) — sees the full structure without collecting or paying
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__naturalist_reading, 0.72).
domain_priors:suppression_score(anthropological_record__naturalist_reading, 0.68).
domain_priors:theater_ratio(anthropological_record__naturalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__naturalist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__naturalist_reading, "Naturalist Reading of the Anthropological Record").
narrative_ontology:topic_domain(anthropological_record__naturalist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__naturalist_reading, '554fe304-4b3a-4a61-9b4d-8c5610b2771c').
narrative_ontology:cs_kernel_codification('554fe304-4b3a-4a61-9b4d-8c5610b2771c', distributed).
narrative_ontology:cs_authority_grounding('554fe304-4b3a-4a61-9b4d-8c5610b2771c', expertise).
narrative_ontology:cs_interpretation_layer_present('554fe304-4b3a-4a61-9b4d-8c5610b2771c').
narrative_ontology:cs_reading_relation('554fe304-4b3a-4a61-9b4d-8c5610b2771c', anthropological_record__creationist_reading, forecloses).
narrative_ontology:cs_reading_relation('554fe304-4b3a-4a61-9b4d-8c5610b2771c', anthropological_record__indigenous_epistemology_reading, forecloses).
narrative_ontology:cs_axiom('554fe304-4b3a-4a61-9b4d-8c5610b2771c', foundational, scientific_method_sole_epistemic_authority).
narrative_ontology:cs_axiom_status(scientific_method_sole_epistemic_authority, holdable).
narrative_ontology:cs_axiom_grounding('554fe304-4b3a-4a61-9b4d-8c5610b2771c', scientific_method_sole_epistemic_authority, instrumental).
narrative_ontology:cs_axiom('554fe304-4b3a-4a61-9b4d-8c5610b2771c', secondary, credentialing_gatekeeping_necessary_for_quality).
narrative_ontology:cs_axiom_status(credentialing_gatekeeping_necessary_for_quality, holdable).
narrative_ontology:cs_axiom_grounding('554fe304-4b3a-4a61-9b4d-8c5610b2771c', credentialing_gatekeeping_necessary_for_quality, conventional).
narrative_ontology:cs_reference_frame('554fe304-4b3a-4a61-9b4d-8c5610b2771c', scientific_materialist_framework).
narrative_ontology:cs_drift_state('554fe304-4b3a-4a61-9b4d-8c5610b2771c', contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('554fe304-4b3a-4a61-9b4d-8c5610b2771c', '').
narrative_ontology:cs_kernel_id(anthropological_record__naturalist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, credentialed_scientists).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, scientific_institutions).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, funding_agencies).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, non_credentialed_interpreters).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, indigenous_communities).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, creationist_proponents).
narrative_ontology:constraint_vindicates(anthropological_record__naturalist_reading, scientific_method_epistemic_supremacy).
narrative_ontology:constraint_vindicates(anthropological_record__naturalist_reading, materialist_ontology_of_human_origins).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the epistemic rules for what counts as valid knowledge about human origins through control of funding agencies, journal editorships, university departments, and professional societies. Justifies the naturalist framework as the only reliable method. Collects the epistemic rents (authority, prestige, material resources) directly.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, scientific_establishment, agenda_setter,
    institutional, generational, analytical, global).

% Gain epistemic authority, grant funding, publication venues, and career advancement by operating within the naturalist framework. They also bear compliance costs (grant writing, peer review, methodological conformity). Their exit is constrained: leaving the framework means losing professional standing and resources.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, credentialed_scientists, beneficiary,
    organized, biographical, constrained, global).

% Independent researchers, citizen scientists, and scholars without institutional affiliation who engage with the anthropological record. They are excluded from funding, major journals, and academic positions. Their interpretations are dismissed as pseudoscience regardless of evidentiary merit. Exit is trapped: they cannot access the resources needed to participate on equal terms.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, non_credentialed_interpreters, payer,
    powerless, biographical, trapped, local).

% Hold relational-oral epistemologies of human origins tied to specific places and ancestors. Their knowledge is systematically excluded from the naturalist record, their data (genetic, archaeological) often extracted without consent, and their authority over their own histories denied. Exit is identity-locked: their epistemology is constitutive of their communal identity; abandoning it would be cultural erasure.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, indigenous_communities, payer,
    powerless, generational, identity_locked, regional).

% Advocate for divine creation or intelligent design as the explanation of human origins. They are structurally barred from the naturalist framework's institutions (journals, funding, curricula). They would object to the naturalist monopoly but are kept out by the same boundary-work that defines the constraint. Their exit is trapped within the naturalist system, though they maintain parallel institutions.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, creationist_proponents, excluded,
    moderate, biographical, trapped, national).

% Philosophers of science, historians of anthropology, and meta-scientific analysts who study the constraint from outside. They neither collect its rents nor pay its costs. Their exit is analytical: they can adopt any framing for analytical purposes.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__naturalist_reading, scientific_establishment).
narrative_ontology:fixing_cost_class(anthropological_record__naturalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes the evaluation of fragmentary and ambiguous evidence (fossils, genetics, archaeology) across a global research community, enabling cumulative knowledge-building and pooled cognitive labor that no single researcher could achieve.
% TRANSFER_FUNCTION: Moves epistemic authority, research funding, publication access, curricular control, and career advancement from non-credentialed interpreters and alternative epistemologies to the credentialed scientific establishment, as the price of participating in the coordinated enterprise.
% ABSENT_VOICES: Indigenous elders and knowledge-keepers whose relational-oral epistemologies are excluded from the naturalist framework; creationist theorists whose divine-causation models are ruled out a priori; independent scholars without institutional credentials. They are structurally absent from the rooms where funding, publication, and curricular decisions are made.
% DISAPPEARANCE_RATIONALE: If the naturalist reading's enforcement vanished overnight, funding would diversify to indigenous-led research and creationist institutes, journals would adopt pluralistic review standards, curricula would include multiple epistemologies, and the scientific establishment would lose its epistemic monopoly — the anthropology of human origins would reorganize into a genuinely pluralistic field.
% FOUNDING_PROBLEM: Making reliable knowledge from fragmentary, ambiguous, and non-replicable evidence (fossils, genetic traces, archaeological sites) about events that occurred deep in the past and cannot be experimentally repeated.
% FOUNDING_PROBLEM_CORROBORATION: The scientific establishment attests the problem is live, citing constant new fossil finds and genomic revisions. Indigenous scholars and philosophers of science corroborate that the evidentiary problem remains, but argue the *exclusivity* of the naturalist solution is the mandatrophy — the problem does not require a single epistemic monopoly. No party outside the beneficiary set attests that the exclusivity is still necessary.
narrative_ontology:disappearance_verdict(anthropological_record__naturalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__naturalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__naturalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(anthropological_record__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__naturalist_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.72) is high because the credentialing gate captures the vast majority of epistemic and material rewards in anthropology/evolutionary biology. Suppression (0.68) is substantial because alternatives are not merely disfavored but structurally excluded from funding, journals, and curricula. Theater ratio (0.41) is moderate: the coordination function (shared evidentiary standards, replication) is real, but a growing share of enforcement activity defends the boundary against indigenous and creationist challengers rather than improving the science. Accessibility collapse (0.63) is significant: once the naturalist framework is internalized, it becomes difficult to even formulate the questions that indigenous or creationist epistemologies ask. Resistance (0.55) is notable: creationist legal challenges, indigenous data sovereignty movements, and post-colonial critiques constitute active resistance. The measurement series run on a shared time grid (0–150 years, roughly Darwin to present) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter (scientific establishment) experiences the constraint as a rope: it coordinates a global research enterprise, solves the collective-action problem of evidence evaluation, and its enforcement is the price of that coordination. The payer seats (non-credentialed interpreters, indigenous communities) experience it as a snare: the coordination story is cover for an epistemic monopoly that extracts their labor (data, specimens, cultural knowledge) while denying them authority. The beneficiary seat (credentialed scientists) sits near symmetric: they benefit from the coordination but also pay the costs of compliance (grant writing, peer review). The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The scientific establishment is the structural beneficiary (collects epistemic rents, controls the rules — d near 0.1). Credentialed scientists are beneficiaries with moderate power (d ~0.2). Non-credentialed interpreters and indigenous communities are targets with trapped/identity_locked exit (d ~0.9). Creationist proponents are excluded (trapped exit, d ~0.95) but their exclusion is the enforcement object itself. The analytical observer has d=0.5 by definition. The derivation chain: beneficiary/victim declarations + power level + exit options yield these directionalities; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (making sense of fragmentary fossil/genetic evidence) is live — new data constantly arrive — but the *exclusivity* of the naturalist reading has outlived its coordination function. The constraint now persists because the establishment extracts rents from its monopoly, not because pluralistic epistemologies would break the coordination. This is mandatrophy: the mandate (exclusive epistemic authority) has outlived its function (reliable knowledge production). The classification (tangled_rope) captures this by requiring both coordination and extraction; a pure snare would deny the coordination function, a pure rope would deny the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalist_reading_as_mountain_or_construct,
    'Is the naturalist reading''s claim to exclusive epistemic authority a genuine reflection of the record''s structure (mountain) or a constructed constraint that benefits credentialed scientists (tangled rope/snare)?',
    'Historical analysis of whether the exclusion of supernatural and indigenous epistemologies was driven by empirical failure of those frameworks or by institutional boundary-work; counterfactual assessment of whether the record itself forces a single materialist interpretation.',
    'If mountain, the high extractiveness and suppression are necessary coordination costs of truth-tracking; if constructed, they are rents extracted by the scientific establishment via credentialing gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalist_reading_as_mountain_or_construct, conceptual, 'Whether the naturalist reading''s epistemic monopoly is a natural law or a socially maintained arrangement.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (funding gatekeeping, publication barriers, legal exclusion from curricula) or internalized (non-credentialed interpreters accepting scientific authority as legitimate)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism (credentialing) is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in epistemic exclusion.').

omega_variable(
    cs_framing_underdetermination,
    'Does the naturalist reading''s commitment system ground its authority in the formalized scientific literature (formalized) or in the ongoing practice of the scientific community (practice)?',
    'Trace whether drift absorption occurs through formal texts (peer-reviewed canon) or through live communal practice (conference norms, informal replication standards).',
    'If formalized, the interpretation layer is the peer-review system; if practice, the interpretation layer is the community of practitioners. Different drift dynamics and foreclosure patterns follow.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative framings of the naturalist reading''s commitment-system structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__naturalist_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anthropological_record__naturalist_reading_tr_t0, anthropological_record__naturalist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(anthropological_record__naturalist_reading_tr_t0, observed).
narrative_ontology:measurement(anthropological_record__naturalist_reading_tr_t30, anthropological_record__naturalist_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(anthropological_record__naturalist_reading_tr_t30, observed).
narrative_ontology:measurement(anthropological_record__naturalist_reading_tr_t60, anthropological_record__naturalist_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement_basis(anthropological_record__naturalist_reading_tr_t60, observed).
narrative_ontology:measurement(anthropological_record__naturalist_reading_tr_t90, anthropological_record__naturalist_reading, theater_ratio, 90, 0.37).
narrative_ontology:measurement_basis(anthropological_record__naturalist_reading_tr_t90, observed).
narrative_ontology:measurement(anthropological_record__naturalist_reading_tr_t120, anthropological_record__naturalist_reading, theater_ratio, 120, 0.4).
narrative_ontology:measurement_basis(anthropological_record__naturalist_reading_tr_t120, observed).
narrative_ontology:measurement(anthropological_record__naturalist_reading_tr_t150, anthropological_record__naturalist_reading, theater_ratio, 150, 0.41).
narrative_ontology:measurement_basis(anthropological_record__naturalist_reading_tr_t150, observed).

% Extraction over time
narrative_ontology:measurement(anthropological_record__naturalist_reading_be_t0, anthropological_record__naturalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(anthropological_record__naturalist_reading_be_t0, observed).
narrative_ontology:measurement(anthropological_record__naturalist_reading_be_t30, anthropological_record__naturalist_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement_basis(anthropological_record__naturalist_reading_be_t30, observed).
narrative_ontology:measurement(anthropological_record__naturalist_reading_be_t60, anthropological_record__naturalist_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(anthropological_record__naturalist_reading_be_t60, observed).
narrative_ontology:measurement(anthropological_record__naturalist_reading_be_t90, anthropological_record__naturalist_reading, base_extractiveness, 90, 0.64).
narrative_ontology:measurement_basis(anthropological_record__naturalist_reading_be_t90, observed).
narrative_ontology:measurement(anthropological_record__naturalist_reading_be_t120, anthropological_record__naturalist_reading, base_extractiveness, 120, 0.68).
narrative_ontology:measurement_basis(anthropological_record__naturalist_reading_be_t120, observed).
narrative_ontology:measurement(anthropological_record__naturalist_reading_be_t150, anthropological_record__naturalist_reading, base_extractiveness, 150, 0.72).
narrative_ontology:measurement_basis(anthropological_record__naturalist_reading_be_t150, observed).

% Suppression requirement over time
narrative_ontology:measurement(anthropological_record__naturalist_reading_su_t0, anthropological_record__naturalist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(anthropological_record__naturalist_reading_su_t0, observed).
narrative_ontology:measurement(anthropological_record__naturalist_reading_su_t30, anthropological_record__naturalist_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement_basis(anthropological_record__naturalist_reading_su_t30, observed).
narrative_ontology:measurement(anthropological_record__naturalist_reading_su_t60, anthropological_record__naturalist_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement_basis(anthropological_record__naturalist_reading_su_t60, observed).
narrative_ontology:measurement(anthropological_record__naturalist_reading_su_t90, anthropological_record__naturalist_reading, suppression_requirement, 90, 0.6).
narrative_ontology:measurement_basis(anthropological_record__naturalist_reading_su_t90, observed).
narrative_ontology:measurement(anthropological_record__naturalist_reading_su_t120, anthropological_record__naturalist_reading, suppression_requirement, 120, 0.64).
narrative_ontology:measurement_basis(anthropological_record__naturalist_reading_su_t120, observed).
narrative_ontology:measurement(anthropological_record__naturalist_reading_su_t150, anthropological_record__naturalist_reading, suppression_requirement, 150, 0.68).
narrative_ontology:measurement_basis(anthropological_record__naturalist_reading_su_t150, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__naturalist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(anthropological_record__naturalist_reading, 0.15).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__creationist_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% The anthropological_record kernel decomposes into three readings with distinct ε values and beneficiary/victim structures. The naturalist reading (this story) has high ε via credentialing; the creationist reading has high ε via doctrinal enforcement; the indigenous reading has low ε but high suppression via colonial erasure. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
