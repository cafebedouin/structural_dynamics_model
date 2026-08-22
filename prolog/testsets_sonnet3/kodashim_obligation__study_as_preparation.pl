% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_preparation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_preparation, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: kodashim_obligation__study_as_preparation
 *   human_readable: Kodashim Study as Preparatory Discipline for Messianic Temple Restoration
 *   domain: religious/legal/textual
 *
 * SUMMARY:
 *   This story instantiates the 'study_as_preparation' reading of the
 *   Kodashim obligation kernel: sacrificial law remains formally binding
 *   despite the Temple's absence, and the point of studying its technical
 *   minutiae is instrumental — to preserve procedural knowledge intact for a
 *   future messianic restoration when performance could resume. Under this
 *   reading the current generation bears a real but modest cost (deferred
 *   cosmic repair, diverted study time) while the beneficiary is a
 *   not-yet-existing future community. This is deliberately narrow: it does
 *   NOT claim (as study_as_performance does) that the study itself completes
 *   the sacrificial function spiritually, and it does NOT claim (as
 *   study_as_archive does) that the material is merely historical with no
 *   live legal claim on anyone. Those are different constraints, authored
 *   separately, sharing this kernel.
 *
 * KEY AGENTS:
 *   - rabbinic_scholarly_class: institutional agenda-setter and incidental beneficiary — administers and maintains the preparatory framing, gains professional standing from mastery of dormant law
 *   - future_messianic_generation: the structural beneficiary — a projected, not-yet-existing agent whose eventual need justifies present cost
 *   - current_generation_practitioners: the payer — bears opportunity cost and psychological weight of binding-but-unperformable obligation
 *   - reform_and_reconstructionist_communities: excluded alternative reading, structurally outside curricular authority
 *   - textual_scholars: analytical observer of the doctrine's function and history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_preparation, 0.22).
domain_priors:suppression_score(kodashim_obligation__study_as_preparation, 0.28).
domain_priors:theater_ratio(kodashim_obligation__study_as_preparation, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, extractiveness, 0.22).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_preparation, scaffold).
narrative_ontology:human_readable(kodashim_obligation__study_as_preparation, "Kodashim Study as Preparatory Discipline for Messianic Temple Restoration").
narrative_ontology:topic_domain(kodashim_obligation__study_as_preparation, "religious/legal/textual").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_preparation).
narrative_ontology:has_sunset_clause(kodashim_obligation__study_as_preparation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_preparation, '64e30c6d-59cd-48a7-804e-f4edc29077b3').
narrative_ontology:cs_kernel_codification('64e30c6d-59cd-48a7-804e-f4edc29077b3', fixed_text).
narrative_ontology:cs_authority_grounding('64e30c6d-59cd-48a7-804e-f4edc29077b3', lineage).
narrative_ontology:cs_interpretation_layer_present('64e30c6d-59cd-48a7-804e-f4edc29077b3').
narrative_ontology:cs_reading_relation('64e30c6d-59cd-48a7-804e-f4edc29077b3', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('64e30c6d-59cd-48a7-804e-f4edc29077b3', kodashim_obligation__study_as_archive, influences).
narrative_ontology:cs_axiom('64e30c6d-59cd-48a7-804e-f4edc29077b3', foundational, law_remains_binding_pending_restoration).
narrative_ontology:cs_axiom_status(law_remains_binding_pending_restoration, holdable).
narrative_ontology:cs_axiom_grounding('64e30c6d-59cd-48a7-804e-f4edc29077b3', law_remains_binding_pending_restoration, conventional).
narrative_ontology:cs_axiom('64e30c6d-59cd-48a7-804e-f4edc29077b3', foundational, study_is_instrumental_not_efficacious).
narrative_ontology:cs_axiom_status(study_is_instrumental_not_efficacious, holdable).
narrative_ontology:cs_axiom_grounding('64e30c6d-59cd-48a7-804e-f4edc29077b3', study_is_instrumental_not_efficacious, deontological).
narrative_ontology:cs_reference_frame('64e30c6d-59cd-48a7-804e-f4edc29077b3', second_temple_sacrificial_praxis).
narrative_ontology:cs_drift_state('64e30c6d-59cd-48a7-804e-f4edc29077b3', contemporary_diaspora, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('64e30c6d-59cd-48a7-804e-f4edc29077b3', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_preparation, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, future_messianic_generation).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, rabbinic_scholarly_class).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, current_generation_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the curriculum that keeps Kodashim (the sacrificial-law tractates) as binding subject matter within the study cycle. Sets which technical details are preserved, taught, and debated. Derives professional and communal standing from mastery of this material even though no sacrifice can currently be offered; the discipline's persistence is partly the source of its own authority.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, rabbinic_scholarly_class, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_preparation, rabbinic_scholarly_class, beneficiary).

% A projected future community that would, upon Temple restoration, need the technical knowledge of sacrificial procedure to resume performance without discontinuity. This beneficiary does not yet exist and cannot presently confirm receipt of the benefit; the entire justificatory structure rests on its eventual arrival.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, future_messianic_generation, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_preparation, future_messianic_generation).

% Devote substantial study time to detailed sacrificial procedures (blood-sprinkling order, disqualifying blemishes, priestly rotations) that cannot be performed in their lifetime. They bear the opportunity cost — time not spent on presently-actionable law — and carry the psychological weight of being bound by a law they cannot fulfill. Exit from the obligation to study is constrained by communal and denominational expectation, not physical force.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, current_generation_practitioners, payer,
    moderate, biographical, constrained, global).

% Have largely opted out of treating Kodashim as binding-but-dormant law, reading it instead as historical or ethical text. Their reading is structurally excluded from Orthodox curricular authority even though it represents a live alternative resolution of the same kernel; they are not consulted in setting the study-as-preparation framework.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, reform_and_reconstructionist_communities, excluded,
    organized, generational, mobile, global).

% Academic and comparative-religion scholars who analyze the preparation claim without being bound by it, tracing how the doctrine of dormant-but-binding law functions across exile, and whether the preparatory framing is itself a later theological innovation rather than an original feature of the law.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, textual_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_preparation, rabbinic_scholarly_class).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_preparation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves highly technical, procedurally precise knowledge (exact sacrificial order, priestly qualifications, disqualifying defects, altar geometry) across centuries of non-performance, so that if Temple worship resumes, practitioners are not reconstructing procedure from scratch.
% TRANSFER_FUNCTION: Moves study time, cognitive effort, and communal prestige from the current generation of students toward the maintenance of a body of law whose payoff (correct performance) is deferred to an unspecified future generation; scholarly authority within the present accrues to those who master the dormant material.
% ABSENT_VOICES: Communities that read Kodashim as archive rather than binding-but-dormant law are excluded from the curricular decision that keeps it in the binding-and-unperformable category; a hypothetical restored priesthood, were performance to resume, might also object to specific preserved rulings as impractical or superseded, but has no present voice.
% DISAPPEARANCE_RATIONALE: If the obligation to study Kodashim as preparatory law vanished, the rabbinic curriculum would need to recharacterize the tractates (likely sliding toward the archive reading), scholarly prestige currently tied to mastery of dormant sacrificial law would need a new basis, and communities committed to imminent restoration would lose a central marker of readiness; other observers hold the world would barely change since the law was never performable anyway, making the study function purely notional. The two camps genuinely dispute which is true.
% FOUNDING_PROBLEM: Following the Temple's destruction, the sacrificial system that had structured Israelite religious life became physically impossible to perform, yet the tradition needed to explain why the law remained authoritative and how continuity with a restored future could be maintained.
% FOUNDING_PROBLEM_CORROBORATION: The rabbinic scholarly class attests the founding problem (continuity pending restoration) remains live and structurally necessary. Reform and Reconstructionist thinkers, writing from outside the beneficiary class, attest that the founding problem — literal restoration of animal sacrifice — is not merely deferred but has been effectively abandoned as a live communal goal, making the 'preparation' framing a legacy justification rather than an active project; some academic historians of religion corroborate this outside view, noting minimal institutional investment in actual restoration logistics (e.g., red heifer breeding, priestly genealogy verification) relative to investment in textual study itself.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_preparation, contested).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_preparation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_preparation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_preparation, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_preparation, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_preparation_tests).
:- end_tests(kodashim_obligation__study_as_preparation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22 at interval end) because the cost extracted from current practitioners — study time and unresolved obligation — is real but modest relative to constraints with concentrated material extraction; there is no rent collector skimming value, only a diffuse deferral. Suppression is moderate-low (0.28): the obligation is maintained through communal and curricular expectation, not coercive enforcement, though deviation (treating Kodashim as archive) carries real social cost within Orthodox contexts, which is why suppression is non-trivial. Theater ratio is low (0.15) because the study function is genuinely substantive (detailed legal reasoning, real intellectual labor) rather than performative — this distinguishes it sharply from a piton, where the function has hollowed into pure performance. Accessibility collapse is moderate (0.6): once inside the framework, alternatives (archive reading, performance reading) are known but socially costly to adopt, so collapse is real but not total. Resistance is low (0.2) since the arrangement is broadly accepted within its practicing community and actively defended by scholarly authority.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic scholarly class sits near the beneficiary end: it sets the curriculum and derives status from the material's continued centrality, though its exit is identity-locked rather than freely arbitraged — leaving the framework would cost it its structural role. The future messianic generation is coded as a non-agent beneficiary (agent: false) because it cannot presently act, confirm, or refuse the benefit; it exists only as the story's justificatory horizon. Current generation practitioners sit toward the target end: they pay the cost (time, deferred fulfillment) without being able to collect the benefit within their own lifetime, and their exit from the obligation is constrained by communal expectation rather than free choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification with sunset clause is deliberate: this reading holds that Temple restoration is the intended terminus of the arrangement — study is transitional preparation, not a permanent steady-state good in itself. This prevents mislabeling the constraint as either a Snare (which would require identifiable extraction without coordination function) or a pure Rope (which would understate the real, unresolved cost borne by a generation that may never see the restoration the framework promises). Because the sunset condition (Temple restoration) has not arrived in ~1900 years of the interval modeled, the mandatrophy question is live: has the 'transitional' framing quietly become a permanent institutional identity for the scholarly class, even as it professes to await sunset? The founding_problem mismatch (status: contested, corroboration split along beneficiary lines) is exactly the signal this framework is built to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preparation_vs_performance_efficacy,
    'Does the preparation reading''s claim — that study preserves knowledge FOR a future restoration, as opposed to the performance reading''s claim that study itself IS the cosmic function — reflect a genuine doctrinal distinction, or are they the same practice narrated two ways by different theological schools?',
    'Comparative analysis of how classical sources (e.g., the liturgical framing ''let our lips substitute for bullocks,'' Hosea 14:3, as invoked in different halakhic and kabbalistic traditions) assign efficacy to study, cross-referenced with whether communities holding this reading maintain distinct restoration-logistics infrastructure (priestly genealogy records, red heifer programs) that a pure performance-reading community would have no reason to maintain.',
    'If preparation and performance readings are doctrinally indistinguishable in practice, this story and its sibling ''study_as_performance'' may need to be merged or their divergence documented as purely rhetorical rather than structural — which would violate the epsilon-invariance principle if their extraction profiles were forced to match. If they are genuinely distinct (as this story assumes), the two stories correctly model different constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preparation_vs_performance_efficacy, conceptual, 'Whether preparation and performance readings are structurally distinct or notational variants of one practice.').

omega_variable(
    restoration_infrastructure_investment,
    'If the preparation reading is genuinely held (not merely legacy rhetoric), why has institutional investment in actual restoration logistics (red heifer breeding programs, priestly genealogical verification, Temple architectural planning) remained marginal relative to investment in textual study alone?',
    'Survey of institutional budgets and organizational effort across Orthodox institutions: proportion devoted to Kodashim textual study versus proportion devoted to concrete restoration-readiness projects.',
    'A large gap would support the founding_problem_status=''dead-in-practice'' reading favored by outside corroborators (Reform/Reconstructionist critics, some academic historians), suggesting the scaffold''s sunset condition is not being actively pursued and the arrangement functions closer to permanent identity-maintenance (pointing toward eventual piton drift) than genuine transitional preparation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_infrastructure_investment, empirical, 'Whether preparatory intent is matched by proportional restoration-readiness investment.').

omega_variable(
    beneficiary_non_existence_problem,
    'Can a non-existent future generation coherently function as the directionality beneficiary of a present-day cost, or does treating it as agent:false understate how the CURRENT scholarly class captures real present benefit (status, authority, institutional continuity) using the future generation as justificatory cover?',
    'Track whether scholarly authority and institutional position within the rabbinic class would survive the removal of the future-restoration justification — i.e., counterfactually, if messianic restoration were declared theologically impossible, would Kodashim study and its associated prestige structure persist unchanged?',
    'If scholarly status would persist largely unchanged, the true present beneficiary is the rabbinic_scholarly_class itself and the future-generation framing functions partly as legitimating narrative — this would push the classification toward tangled_rope rather than scaffold, since a stable present beneficiary alongside a real coordination function is the tangled_rope signature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_non_existence_problem, conceptual, 'Whether the non-existent future beneficiary masks a present concentrated beneficiary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_preparation, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_preparation, theater_ratio, 0, 0.05).
narrative_ontology:measurement(koda_tr_t300, kodashim_obligation__study_as_preparation, theater_ratio, 300, 0.07).
narrative_ontology:measurement(koda_tr_t700, kodashim_obligation__study_as_preparation, theater_ratio, 700, 0.09).
narrative_ontology:measurement(koda_tr_t1100, kodashim_obligation__study_as_preparation, theater_ratio, 1100, 0.11).
narrative_ontology:measurement(koda_tr_t1500, kodashim_obligation__study_as_preparation, theater_ratio, 1500, 0.13).
narrative_ontology:measurement(koda_tr_t1900, kodashim_obligation__study_as_preparation, theater_ratio, 1900, 0.15).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_preparation, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(koda_be_t300, kodashim_obligation__study_as_preparation, base_extractiveness, 300, 0.14).
narrative_ontology:measurement(koda_be_t700, kodashim_obligation__study_as_preparation, base_extractiveness, 700, 0.17).
narrative_ontology:measurement(koda_be_t1100, kodashim_obligation__study_as_preparation, base_extractiveness, 1100, 0.19).
narrative_ontology:measurement(koda_be_t1500, kodashim_obligation__study_as_preparation, base_extractiveness, 1500, 0.2).
narrative_ontology:measurement(koda_be_t1900, kodashim_obligation__study_as_preparation, base_extractiveness, 1900, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_obligation__study_as_preparation, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(koda_su_t300, kodashim_obligation__study_as_preparation, suppression_requirement, 300, 0.22).
narrative_ontology:measurement(koda_su_t700, kodashim_obligation__study_as_preparation, suppression_requirement, 700, 0.24).
narrative_ontology:measurement(koda_su_t1100, kodashim_obligation__study_as_preparation, suppression_requirement, 1100, 0.25).
narrative_ontology:measurement(koda_su_t1500, kodashim_obligation__study_as_preparation, suppression_requirement, 1500, 0.27).
narrative_ontology:measurement(koda_su_t1900, kodashim_obligation__study_as_preparation, suppression_requirement, 1900, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_preparation, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_obligation__study_as_preparation, 0.1).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_archive).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the kodashim_obligation kernel (study_as_preparation, study_as_performance, study_as_archive), each sharing the same textual kernel (the Kodashim tractates) but diverging on binding status, efficacy claim, and beneficiary identity. study_as_preparation carries low extractiveness (0.22) and a scaffold classification (sunset = messianic restoration); study_as_performance is expected to carry a different extraction/enforcement profile (efficacy claimed as presently realized, likely tangled_rope or rope depending on enforcement); study_as_archive is expected to carry the lowest extraction and weakest enforcement requirement (no live legal claim). Each is authored independently per the epsilon-invariance principle; do not average their epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
