% ============================================================================
% CONSTRAINT STORY: anthropological_record__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Institutionalized Naturalist Epistemic Monopoly on Human Origins
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   The anthropological recordâfossil, genetic, and archaeologicalâis
 *   interpreted by the naturalist reading as revealing purely materialist
 *   human origins (evolutionary descent, migratory patterns) knowable only
 *   through credentialed application of the scientific method. This reading
 *   functions as an institutional constraint: it excludes supernatural
 *   causation, delegitimizes non-credentialed interpreters, and suppresses
 *   indigenous epistemologies that advance relational or oral accounts of
 *   continuity. The constraint is one reading of the contested
 *   anthropological_record kernel; sibling readings include creationist
 *   divine-creation accounts and indigenous epistemologies of relational
 *   continuity. This JSON instantiates only the naturalist reading.
 *
 * KEY AGENTS:
 *   - scientific_establishment (institutional/agenda_setter): controls peer review, funding, and tenure criteria
 *   - credentialed_researchers (organized/beneficiary): receive epistemic authority and resources within the gatekept system
 *   - secular_education_systems (institutional/beneficiary): derive curricular authority from teaching the authorized account
 *   - indigenous_knowledge_holders (powerless/payer): bear epistemic exclusion and delegitimization of oral tradition
 *   - non_credentialed_interpreters (moderate/payer): barred from journals and labeled pseudoscience regardless of evidence quality
 *   - creationist_communities (organized/excluded): structurally barred from scientific discourse and public education standards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__naturalist_reading, 0.78).
domain_priors:suppression_score(anthropological_record__naturalist_reading, 0.85).
domain_priors:theater_ratio(anthropological_record__naturalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__naturalist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__naturalist_reading, "Institutionalized Naturalist Epistemic Monopoly on Human Origins").
narrative_ontology:topic_domain(anthropological_record__naturalist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__naturalist_reading, '0c8fcf88-18eb-4580-85a8-c0661c74ebcd').
narrative_ontology:cs_kernel_codification('0c8fcf88-18eb-4580-85a8-c0661c74ebcd', formalized).
narrative_ontology:cs_authority_grounding('0c8fcf88-18eb-4580-85a8-c0661c74ebcd', expertise).
narrative_ontology:cs_interpretation_layer_present('0c8fcf88-18eb-4580-85a8-c0661c74ebcd').
narrative_ontology:cs_reading_relation('0c8fcf88-18eb-4580-85a8-c0661c74ebcd', anthropological_record__creationist_reading, forecloses).
narrative_ontology:cs_reading_relation('0c8fcf88-18eb-4580-85a8-c0661c74ebcd', anthropological_record__indigenous_epistemology_reading, influences).
narrative_ontology:cs_axiom('0c8fcf88-18eb-4580-85a8-c0661c74ebcd', foundational, material_causation_sufficiency).
narrative_ontology:cs_axiom_status(material_causation_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('0c8fcf88-18eb-4580-85a8-c0661c74ebcd', material_causation_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('0c8fcf88-18eb-4580-85a8-c0661c74ebcd', foundational, credentialing_epistemic_authority).
narrative_ontology:cs_axiom_status(credentialing_epistemic_authority, holdable).
narrative_ontology:cs_axiom_grounding('0c8fcf88-18eb-4580-85a8-c0661c74ebcd', credentialing_epistemic_authority, conventional).
narrative_ontology:cs_reference_frame('0c8fcf88-18eb-4580-85a8-c0661c74ebcd', empirical_materialism_framework).
narrative_ontology:cs_drift_state('0c8fcf88-18eb-4580-85a8-c0661c74ebcd', post_colonial_epistemological_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0c8fcf88-18eb-4580-85a8-c0661c74ebcd', '').
narrative_ontology:cs_kernel_id(anthropological_record__naturalist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, credentialed_researchers).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, secular_education_systems).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, indigenous_knowledge_holders).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, non_credentialed_interpreters).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, creationist_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls peer review boards, funding criteria, and tenure standards for anthropology and related sciences. Enforces materialist methodology and credentialing as prerequisites for legitimate knowledge claims about human origins. Derives institutional authority from the perceived success of empirical science.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, scientific_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% Receive salaries, grants, tenure, and epistemic authority by publishing within the credentialed framework. Their professional identity and career trajectories are fused with the peer-review and credentialing system.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, credentialed_researchers, beneficiary,
    organized, biographical, identity_locked, global).

% Receive public funding, curricular authority, and cultural legitimacy by teaching the naturalist account as settled science. Their curricular scope is bounded by standards that exclude non-naturalist origin accounts.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, secular_education_systems, beneficiary,
    institutional, generational, constrained, national).

% Bear oral traditions and relational knowledge of ancestral continuity and place. Their epistemologies are dismissed as non-falsifiable or anecdotal within peer review; they are excluded from funding and academic discourse on human origins.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, indigenous_knowledge_holders, payer,
    powerless, generational, trapped, local).

% Include independent scholars, local historians, and religious scholars who analyze archaeological or genetic evidence without institutional credentials. Their submissions are barred from journals and their interpretations labeled pseudoscience regardless of internal evidence quality.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, non_credentialed_interpreters, payer,
    moderate, biographical, constrained, national).

% Advance supernatural causal accounts of human origins. They are structurally excluded from peer review, scientific discourse, and public education standards; their absence is enforced by definitional boundaries of science rather than by empirical refutation alone.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, creationist_communities, excluded,
    organized, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates empirical inquiry into human origins by establishing shared standards for evidence, reproducibility, and falsifiability across dispersed research sites.
% TRANSFER_FUNCTION: Moves epistemic authority and resource access from non-credentialed interpreters, indigenous knowledge systems, and supernatural accounts to credentialed research institutions and secular education systems.
% ABSENT_VOICES: Indigenous knowledge holders and non-credentialed interpreters are absent from peer review and funding panels; creationist scholars are absent from curriculum design committees. Their exclusion is structural, not incidental.
% DISAPPEARANCE_RATIONALE: If the naturalist reading's institutional monopoly vanished, peer review would admit non-materialist and non-credentialed contributions, funding would flow to indigenous research paradigms, public school curricula would reopen to multiple origin accounts, and the current distribution of epistemic authority would reorganize.
% FOUNDING_PROBLEM: How to distinguish reliable, reproducible empirical knowledge about human origins from theological speculation, folk narrative, and politically motivated myth-making in the 19thâ20th century context of colonial anthropology and church-state conflicts.
% FOUNDING_PROBLEM_CORROBORATION: Post-colonial anthropologists and indigenous scholars attest the founding problem has shifted; historians of science attest the original problem was genuine but its solution has been co-opted. No party outside the benefiting credentialed institutions attests the current gatekeeping is still necessary for empirical rigor.
narrative_ontology:disappearance_verdict(anthropological_record__naturalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__naturalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__naturalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(anthropological_record__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__naturalist_reading, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.78) because the credentialing apparatus extracts epistemic authority and resource control from excluded parties. Suppression is higher (0.85) because the constraint depends on actively excluding supernatural causation and non-credentialed voice; without this enforcement, alternative readings would gain parity. Theater is moderate (0.45): peer review and methodological standards perform genuine coordination, but an increasing share of activity is performative maintenance of 'objectivity' that masks gatekeeping. Resistance is moderate (0.55) from indigenous advocacy, creationist legal challenges, and internal post-colonial critique. Accessibility collapse is high (0.75): once inside the credentialing system, practitioners rarely encounter viable methodological alternatives as legitimate.
 *
 * PERSPECTIVAL GAP:
 *   The credentialed beneficiary seat experiences the constraint as necessary coordination against pseudoscience; the indigenous and non-credentialed payer seats experience it as extractive epistemic violence. The engine computes this divergence from structural dataâdirectionality near 0.0 for beneficiaries and near 1.0 for trapped or constrained payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (credentialed researchers, secular education systems) sit near the full-beneficiary end: the constraint subsidizes their authority and resource access. Victims (indigenous holders, non-credentialed interpreters, creationist communities) sit near the full-target end: the constraint extracts epistemic standing and material opportunity from them. The scientific establishment, despite setting the agenda, is identity-locked to the method; its exit is constrained by professional fusion, placing its directionality between pure beneficiary and symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâdistinguishing empirical knowledge from speculation in a context of colonial myth-makingâwas genuine. The mandatrophy risk is mislabeling the current gatekeeping as still serving that founding problem, when substantial critique (post-colonial, indigenous science studies) attests the problem has shifted. The Tangled Rope classification captures both the residual coordination function (empirical standards) and the layered extraction (credentialing monopoly), preventing either pure-snare dismissal or pure-rope apologism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the naturalist reading''s empirical axiom foreclose indigenous epistemology and creationist causation, or merely coexist as one competing framework?',
    'Comparative analysis of institutional admission criteria: if indigenous oral tradition or supernatural causation can be admitted as evidence within naturalist peer review without logical contradiction, the relation is coexists_with; if the materialist premise inherently excludes them, it is forecloses.',
    'If forecloses, the naturalist reading is structurally brittle and its high suppression is internally necessary; if coexists_with, the suppression is an extrinsic enforcement choice and the constraint is more extractive than logically required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural relationship between naturalist reading and sibling readings').

omega_variable(
    credentialing_extraction_or_competence,
    'Does the credentialing gatekeeping function primarily ensure methodological competence, or does it extract epistemic rents by excluding valid alternative methodologies?',
    'Audit of peer review outcomes: measure the rate at which methodologically sound but epistemologically non-naturalist submissions are rejected on grounds other than empirical failure.',
    'If credentialing tracks competence, the coordination function is genuine and extraction is lower; if it tracks epistemic conformity, the extraction is higher and the coordination story is cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credentialing_extraction_or_competence, empirical, 'Whether credentialing ensures competence or enforces conformity').

omega_variable(
    suppression_scope_ambiguity,
    'Is the suppression of non-naturalist origin accounts a structural feature of empirical method itself, or an extrinsic institutional enforcement layered atop the method?',
    'Historical comparison across scientific communities with varying institutional boundaries: if non-naturalist accounts are excluded even in minimally institutionalized empirical communities, the suppression is method-structural; if exclusion correlates with institutional gatekeeping rather than methodological rigor, it is extrinsic.',
    'If structural, the constraint''s classification shifts toward Mountain-like immunity for the method itself; if extrinsic, the extraction is institutional and the Tangled Rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_scope_ambiguity, conceptual, 'Ambiguity between methodological and institutional suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__naturalist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anthro_nat_tr_t0, anthropological_record__naturalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(anthro_nat_tr_t6, anthropological_record__naturalist_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(anthro_nat_tr_t12, anthropological_record__naturalist_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(anthro_nat_tr_t18, anthropological_record__naturalist_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement(anthro_nat_tr_t24, anthropological_record__naturalist_reading, theater_ratio, 24, 0.43).
narrative_ontology:measurement(anthro_nat_tr_t30, anthropological_record__naturalist_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(anthro_nat_be_t0, anthropological_record__naturalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(anthro_nat_be_t6, anthropological_record__naturalist_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(anthro_nat_be_t12, anthropological_record__naturalist_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(anthro_nat_be_t18, anthropological_record__naturalist_reading, base_extractiveness, 18, 0.72).
narrative_ontology:measurement(anthro_nat_be_t24, anthropological_record__naturalist_reading, base_extractiveness, 24, 0.76).
narrative_ontology:measurement(anthro_nat_be_t30, anthropological_record__naturalist_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(anthro_nat_su_t0, anthropological_record__naturalist_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(anthro_nat_su_t6, anthropological_record__naturalist_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(anthro_nat_su_t12, anthropological_record__naturalist_reading, suppression_requirement, 12, 0.75).
narrative_ontology:measurement(anthro_nat_su_t18, anthropological_record__naturalist_reading, suppression_requirement, 18, 0.8).
narrative_ontology:measurement(anthro_nat_su_t24, anthropological_record__naturalist_reading, suppression_requirement, 24, 0.83).
narrative_ontology:measurement(anthro_nat_su_t30, anthropological_record__naturalist_reading, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__naturalist_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
