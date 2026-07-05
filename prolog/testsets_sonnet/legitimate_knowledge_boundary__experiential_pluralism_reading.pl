% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__experiential_pluralism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__experiential_pluralism_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: legitimate_knowledge_boundary__experiential_pluralism_reading
 *   human_readable: Experiential Pluralism Reading of the Legitimate Knowledge Boundary
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This story instantiates the experiential-pluralism reading of the
 *   contested 'legitimate knowledge boundary' kernel: the claim that lived
 *   experience and community validation constitute legitimate epistemic
 *   authority in their own right, with formal methodology treated as one
 *   validation tool among several rather than the gatekeeper. This reading
 *   emerged largely from patient advocacy movements (AIDS activism's
 *   insistence on patient-reported treatment knowledge), disability studies,
 *   indigenous knowledge frameworks, and standpoint epistemology in feminist
 *   theory. It functions as a genuine coordination mechanism where
 *   credentialed institutions had previously excluded relevant knowers
 *   entirely — but it also creates room for community consensus to substitute
 *   for validation when the community's own internal power structure
 *   suppresses dissenting experiential claims, or when unfalsifiable claims
 *   accrete legitimacy through repetition and solidarity rather than
 *   correction.
 *
 * KEY AGENTS:
 *   - affected_communities: primary beneficiary (organized/mobile) — gain standing to have their experience count as evidence
 *   - patient_advocacy_groups: beneficiary and agenda_setter (organized/constrained) — organize and legitimate experiential claims, but also police what counts as 'authentic' community knowledge
 *   - credentialed_researchers_displaced_by_populist_veto: payer (powerful/constrained) — lose exclusive gatekeeping authority, sometimes justifiably, sometimes not
 *   - vulnerable_patients_exposed_to_unvalidated_claims: payer (powerless/trapped) — bear the cost when unvalidated experiential claims circulating with high community legitimacy turn out to be false or harmful
 *   - indigenous_knowledge_holders: beneficiary (organized/constrained) — gain recognition for traditional knowledge systems previously dismissed
 *   - science_and_technology_studies_scholars: observer (analytical/analytical) — theorize and sometimes advocate for the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.28).
domain_priors:suppression_score(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.22).
domain_priors:theater_ratio(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__experiential_pluralism_reading, rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__experiential_pluralism_reading, "Experiential Pluralism Reading of the Legitimate Knowledge Boundary").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__experiential_pluralism_reading, "epistemology/science_and_technology_studies/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__experiential_pluralism_reading, 'bbdd01dd-2539-4af1-9137-c04b580022b8').
narrative_ontology:cs_kernel_codification('bbdd01dd-2539-4af1-9137-c04b580022b8', distributed).
narrative_ontology:cs_authority_grounding('bbdd01dd-2539-4af1-9137-c04b580022b8', practice).
narrative_ontology:cs_interpretation_layer_present('bbdd01dd-2539-4af1-9137-c04b580022b8').
narrative_ontology:cs_reading_relation('bbdd01dd-2539-4af1-9137-c04b580022b8', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('bbdd01dd-2539-4af1-9137-c04b580022b8', legitimate_knowledge_boundary__hybrid_coproduction_reading, influences).
narrative_ontology:cs_axiom('bbdd01dd-2539-4af1-9137-c04b580022b8', foundational, lived_experience_is_self_validating_evidence).
narrative_ontology:cs_axiom_status(lived_experience_is_self_validating_evidence, holdable).
narrative_ontology:cs_axiom_grounding('bbdd01dd-2539-4af1-9137-c04b580022b8', lived_experience_is_self_validating_evidence, conventional).
narrative_ontology:cs_axiom('bbdd01dd-2539-4af1-9137-c04b580022b8', foundational, methodological_standards_are_one_tool_not_the_gate).
narrative_ontology:cs_axiom_status(methodological_standards_are_one_tool_not_the_gate, holdable).
narrative_ontology:cs_axiom_grounding('bbdd01dd-2539-4af1-9137-c04b580022b8', methodological_standards_are_one_tool_not_the_gate, instrumental).
narrative_ontology:cs_reference_frame('bbdd01dd-2539-4af1-9137-c04b580022b8', credentialed_gatekeeping_baseline).
narrative_ontology:cs_drift_state('bbdd01dd-2539-4af1-9137-c04b580022b8', post_patient_activism_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('bbdd01dd-2539-4af1-9137-c04b580022b8', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, affected_communities).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, patient_advocacy_groups).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, indigenous_knowledge_holders).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, grassroots_movement_organizers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_researchers_displaced_by_populist_veto).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, vulnerable_patients_exposed_to_unvalidated_claims).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, situated_knowledge_thesis).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, community_epistemic_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities with direct lived experience of a condition or issue (e.g. chronic illness, environmental harm, disability) gain formal standing to have their testimony treated as evidence rather than anecdote. They participate in advisory boards, contribute to research agendas, and increasingly shape what counts as a legitimate research question. Their exit option is real: they can and do walk away from institutions that refuse to recognize their standing and build parallel validation structures.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, affected_communities, beneficiary,
    organized, generational, mobile, national).

% Organize community testimony into structured input for research funders, regulators, and journals. They set internal standards for what counts as authentic or representative community knowledge, and administer the practical machinery (review panels, community advisory boards) through which experiential claims gain institutional weight. Their exit is constrained by dependence on continued institutional recognition to remain relevant advocates.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, patient_advocacy_groups, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, patient_advocacy_groups, beneficiary).

% Traditional ecological and medical knowledge systems gain formal recognition alongside or in place of purely methodological validation, particularly in environmental management and public health contexts. Recognition is a genuine gain after long historical exclusion, but remains geographically and institutionally constrained — recognition in one jurisdiction or agency does not transfer automatically to others.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, indigenous_knowledge_holders, beneficiary,
    organized, generational, constrained, regional).

% Researchers whose methodologically grounded findings are overridden, delayed, or defunded when they conflict with strongly held community consensus (e.g. contested treatment efficacy claims). They retain professional standing and can publish or seek funding elsewhere, but face real friction and reputational risk when contesting community-validated claims labeled as authoritative lived experience.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_researchers_displaced_by_populist_veto, payer,
    powerful, biographical, constrained, national).

% Individuals who encounter community-endorsed but empirically unverified claims (about treatments, causes, or risks) circulating with high perceived legitimacy because they carry community consensus rather than methodological validation. They typically lack the resources or standing to independently verify claims and bear the cost — delayed proper treatment, financial harm, or physical risk — when the community consensus turns out to be wrong.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, vulnerable_patients_exposed_to_unvalidated_claims, payer,
    powerless, immediate, trapped, local).

% Theorize the standpoint-epistemology and situated-knowledge frameworks that underpin this reading, study its adoption across institutions, and sometimes actively advocate for its expansion. They observe and interpret the contest between readings rather than bearing its costs or collecting its benefits directly.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, science_and_technology_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__experiential_pluralism_reading, diffuse).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__experiential_pluralism_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the real problem of institutions with no formal mechanism to register the testimony of people with direct lived experience of a condition, harm, or context — pooling distributed experiential knowledge that credentialed methodology alone was structurally blind to (e.g. patient-reported side effects methodology hadn't measured, environmental harms local communities detected before instruments did).
% TRANSFER_FUNCTION: Moves epistemic authority and downstream resources (research funding priorities, regulatory attention, media credibility, policy standing) from credentialed institutions that previously held near-exclusive gatekeeping power toward organized communities and advocacy groups who can now claim standing on the basis of experience and consensus rather than methodological validation alone.
% ABSENT_VOICES: Dissenting members within the 'validated' community whose experience contradicts the advocacy group's consensus position are structurally quiet — their dissent is filtered out before reaching institutional attention because the advocacy group, not the raw community, is the one setting the agenda and speaking for it. Also largely absent: the class of vulnerable patients who lack any advocacy organization at all and so get neither credentialed nor experiential standing.
% DISAPPEARANCE_RATIONALE: If experiential-pluralism standing vanished overnight, affected communities and indigenous knowledge holders would lose formal input channels into research agendas, funding panels, and regulatory review that took decades of activism to establish; credentialed institutions would revert to near-exclusive gatekeeping, and the historical exclusion problem this reading was built to address would resurface immediately for populations who currently rely on it as their primary channel of institutional voice.
% FOUNDING_PROBLEM: Credentialed institutions systematically dismissed or discounted the testimony of people with direct lived experience — patients whose reported symptoms were dismissed absent lab confirmation, indigenous communities whose ecological knowledge was ignored until replicated by Western science, disabled people whose accounts of their own needs were overridden by clinical judgment.
% FOUNDING_PROBLEM_CORROBORATION: Historians of medicine and STS scholars outside the advocacy movements themselves (e.g. accounts of the AIDS activist intervention into clinical trial design) corroborate that the founding exclusion was real and substantially addressed in some domains. Independent health-policy analysts and some credentialed researchers who are not beneficiaries of the reading corroborate that in other domains the exclusion persists largely unaddressed, while in a subset of domains (some alternative-medicine and anti-vaccine-adjacent communities) advocacy-organized 'lived experience' consensus has itself become a vector for unvalidated and sometimes harmful claims — a finding advocacy organizations themselves generally dispute.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__experiential_pluralism_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__experiential_pluralism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__experiential_pluralism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).
:- end_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28) and rising modestly, reflecting that the reading's core function — including previously excluded knowers — is genuinely coordinative rather than extractive, but that as the reading gains institutional traction (funding panels, IRBs incorporating 'lived experience' review, journal policies) some rent-seeking around claimed authenticity begins to accrue (self-appointed community spokespeople, advocacy-org gatekeeping). Suppression is low (0.22): the reading explicitly lowers barriers rather than raising them, and its main coercive mechanism is reputational (accusing dissenters of epistemic injustice) rather than structural exclusion. Accessibility collapse is low (0.2) by design — that is the reading's entire point, and it is descriptively true that alternatives (methodological gatekeeping) remain visible and contested, not erased. Resistance is moderate (0.45) because credentialed institutions and some patient populations actively contest the reading's expansion into clinical and policy domains where false experiential claims can cause real harm.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary communities' seat, this reading looks like Rope: it solves the real coordination problem of experience going unheard and undervalued by institutions that had no mechanism to register it. From the seat of the vulnerable patient exposed to an unvalidated but community-endorsed claim (e.g. a stigmatized alternative treatment gaining legitimacy through advocacy-group consensus rather than trial data), the same structure can look extractive — the community's endorsement substitutes for verification, and the cost of that substitution falls on people who trusted the community's authority. The engine should register this divergence, not smooth it into a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (affected communities, advocacy groups, indigenous knowledge holders) get low d because the reading was built to elevate exactly their standing — it subsidizes their epistemic authority relative to the prior credentialed-only regime. Displaced credentialed researchers get moderate-high d because they lose exclusive gatekeeping power, though their institutional position gives them exit options (they retain publication venues, funding networks, and professional standing elsewhere) — hence 'constrained' rather than 'trapped'. Vulnerable patients exposed to unvalidated claims get the highest d: they are trapped (limited access to alternative validation, often already marginalized by the very system this reading seeks to correct) and bear costs they did not choose and often cannot detect until harmed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — credentialed institutions systematically dismissing the testimony of those with direct experience (patients, indigenous communities, disabled people) — remains substantially live in many domains, which is why this reading should not be treated as a pure legacy holdover. But mandatrophy risk exists at the margins: once a domain's exclusion problem is substantially resolved (e.g. a field that has genuinely integrated patient-reported outcomes into trial design), continued blanket deference to community consensus over methodological triangulation risks becoming a captured mandate that primarily serves advocacy-organization legitimacy rather than the excluded knowers it was built to protect. This tension is exactly why the story is authored as Rope rather than Tangled Rope: the coordination function is real and the extraction is currently secondary, but the omega on distributed-validation capture risk flags where that could flip.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_multiplicity,
    'Is the legitimate-knowledge boundary genuinely three structurally distinct constraints (credentialed_expertise, experiential_pluralism, hybrid_coproduction), or one contested kernel whose readings are policy postures over the same underlying epistemic fact?',
    'Track whether institutions that adopt this reading produce durable, falsifiable outcome differences (e.g. patient-led research agendas that generate reproducible clinical findings) versus outcomes indistinguishable from unvalidated community consensus absorbed uncritically.',
    'If the reading is a genuine alternative validation architecture with its own error-correction, it is closer to Rope; if community validation systematically fails to self-correct against motivated claims, the reading functions closer to Tangled Rope or Snare for the vulnerable-claimant population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_multiplicity, conceptual, 'Whether the three kernel readings are structurally distinct constraints or rhetorical postures over one epistemic fact.').

omega_variable(
    distributed_validation_capture_risk,
    'Does distributed, community-based validation reliably surface minority or dissenting experiential claims within the community, or does it tend to be captured by whichever sub-group holds the most organizing capacity or loudest platform?',
    'Comparative case study of movements where experiential-pluralism validation processes were audited for internal minority representation (e.g. within patient advocacy groups, who set the community''s ''validated'' consensus and who was excluded from it).',
    'If capture is common, the coordination function this reading claims (giving voice to the excluded) is partially illusory for a sub-population within the beneficiary group itself, which would push some seats toward tangled_rope even within this single reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributed_validation_capture_risk, empirical, 'Whether community validation processes are internally democratic or capturable by sub-group power.').

omega_variable(
    sibling_reading_foreclosure_boundary,
    'Does adopting experiential_pluralism_reading as an institution''s operating epistemology logically foreclose credentialed_expertise_reading within that same institution, or can the two coexist as competing but simultaneously-held standards?',
    'Examine institutions (e.g. participatory health research bodies) that formally hold both readings as co-equal decision inputs; if such institutions function coherently without contradiction, coexistence is the correct relation; if they collapse into de facto priority for one reading, foreclosure is closer to the true relation.',
    'Determines whether this reading''s relation to credentialed_expertise_reading should be coexists_with or forecloses; currently authored as coexists_with on the observation that hybrid institutions exist and function, however unstably.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_boundary, conceptual, 'Whether experiential-pluralism and credentialed-expertise readings can coexist within one institution or structurally displace each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__experiential_pluralism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(legi_tr_t6, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(legi_tr_t12, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(legi_tr_t18, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 18, 0.25).
narrative_ontology:measurement(legi_tr_t24, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(legi_tr_t30, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(legi_be_t6, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 6, 0.2).
narrative_ontology:measurement(legi_be_t12, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 12, 0.23).
narrative_ontology:measurement(legi_be_t18, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 18, 0.25).
narrative_ontology:measurement(legi_be_t24, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 24, 0.27).
narrative_ontology:measurement(legi_be_t30, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 30, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(legitimate_knowledge_boundary__experiential_pluralism_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__experiential_pluralism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.1).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the legitimate_knowledge_boundary kernel. credentialed_expertise_reading assigns near-exclusive epistemic authority to methodologically rigorous, peer-reviewed inquiry; experiential_pluralism_reading (this story) assigns co-equal or higher authority to lived experience and community validation; hybrid_coproduction_reading requires integration of both through explicit co-production. Each reading has its own epsilon, its own beneficiary/victim structure, and its own classification; they are not measurement variants of one constraint but three structurally distinct arrangements competing for the same institutional space (funding panels, journals, regulatory bodies, courts) that must choose which epistemic priority to encode into policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_knowledge_boundary__experiential_pluralism_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
