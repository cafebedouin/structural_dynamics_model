% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__experiential_pluralism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: legitimate_knowledge_boundary__experiential_pluralism_reading
 *   human_readable: Experiential-Pluralist Reading of the Legitimate-Knowledge Boundary
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This story instantiates the experiential-pluralism reading of the
 *   contested legitimate-knowledge boundary: the claim that lived experience
 *   and community validation constitute legitimate knowledge in their own
 *   right, with methodological standards demoted to one tool among several
 *   rather than the arbiter of last resort. This reading arose largely as a
 *   corrective to documented histories of methodological gatekeeping that
 *   excluded or actively harmed experiential communities. As authored, ε is
 *   assessed for THIS reading's own standing arrangement — the
 *   community-validation apparatus as it currently operates, with its own
 *   enforcement and internal exclusions — not for the credentialed-expertise
 *   arrangement it displaces and not for the hybrid co-production arrangement
 *   it could become. The two sibling readings
 *   (credentialed_expertise_reading, hybrid_coproduction_reading) are
 *   separate constraint files with their own ε and stakeholder structures;
 *   this file does not average over them or describe their contest
 *   internally, per the ε-invariance and one-reading-per-file rules.
 *
 * KEY AGENTS:
 *   - lived_experience_communities: primary beneficiary and increasingly agenda-setting body (organized/constrained) — gains epistemic standing under this reading
 *   - community_based_advocacy_organizations: agenda-setter administering validation processes (organized/mobile) — controls what counts as certified experiential knowledge
 *   - credentialed_dissenting_researchers: primary target when findings contradict community consensus (moderate/constrained) — bears reputational and access costs
 *   - outvoted_minority_experiential_factions: secondary victim, powerless and trapped — internal dissenters within the credited community whose divergent testimony is suppressed by the same consensus mechanism that elevates the community's authority
 *   - policy_makers_and_funders: observer/incidental beneficiary — adjudicates how much institutional weight the reading's outputs receive
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.42).
domain_priors:suppression_score(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.38).
domain_priors:theater_ratio(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__experiential_pluralism_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__experiential_pluralism_reading, "Experiential-Pluralist Reading of the Legitimate-Knowledge Boundary").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__experiential_pluralism_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__experiential_pluralism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__experiential_pluralism_reading, '0d82db16-170c-4947-8e1e-ff2791bcd78a').
narrative_ontology:cs_kernel_codification('0d82db16-170c-4947-8e1e-ff2791bcd78a', distributed).
narrative_ontology:cs_authority_grounding('0d82db16-170c-4947-8e1e-ff2791bcd78a', practice).
narrative_ontology:cs_interpretation_layer_present('0d82db16-170c-4947-8e1e-ff2791bcd78a').
narrative_ontology:cs_reading_relation('0d82db16-170c-4947-8e1e-ff2791bcd78a', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('0d82db16-170c-4947-8e1e-ff2791bcd78a', legitimate_knowledge_boundary__hybrid_coproduction_reading, influences).
narrative_ontology:cs_axiom('0d82db16-170c-4947-8e1e-ff2791bcd78a', foundational, lived_experience_is_self_validating_knowledge).
narrative_ontology:cs_axiom_status(lived_experience_is_self_validating_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('0d82db16-170c-4947-8e1e-ff2791bcd78a', lived_experience_is_self_validating_knowledge, conventional).
narrative_ontology:cs_axiom('0d82db16-170c-4947-8e1e-ff2791bcd78a', foundational, methodological_standards_are_one_tool_among_many_not_arbiter).
narrative_ontology:cs_axiom_status(methodological_standards_are_one_tool_among_many_not_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('0d82db16-170c-4947-8e1e-ff2791bcd78a', methodological_standards_are_one_tool_among_many_not_arbiter, conventional).
narrative_ontology:cs_reference_frame('0d82db16-170c-4947-8e1e-ff2791bcd78a', testimony_based_communal_epistemic_authority).
narrative_ontology:cs_drift_state('0d82db16-170c-4947-8e1e-ff2791bcd78a', post_institutionalization_of_community_engagement_mandates, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0d82db16-170c-4947-8e1e-ff2791bcd78a', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, lived_experience_communities).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, community_based_advocacy_organizations).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, patient_led_research_networks).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_dissenting_researchers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, outvoted_minority_experiential_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, policy_makers_and_funders).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, standpoint_epistemology).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, situated_knowledge_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups whose direct experience of a condition, harm, or practice (chronic illness, environmental exposure, disability, incarceration) is treated under this reading as itself a validating source of knowledge, not merely as data for outside experts to interpret. They convene community forums, validate claims through consensus and testimony, and increasingly set the terms under which outside researchers may participate. Their standing rises directly as the reading displaces credential gatekeeping.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, lived_experience_communities, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, lived_experience_communities, agenda_setter).

% Organizations that administer participatory validation processes (community review boards, testimony panels, consensus statements) and thereby control which experiential claims are certified as knowledge. They gain funding, platform access, and policy standing from being recognized as legitimate knowledge-producing bodies under this reading; they draft and enforce the participatory criteria other actors must meet.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, community_based_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, community_based_advocacy_organizations, agenda_setter).

% Self-organized groups of patients or affected individuals who produce and circulate knowledge about a condition through shared experience and informal aggregation rather than clinical trial methodology. Under this reading their aggregated testimony carries evidentiary weight equal to or greater than methodologically controlled studies; their exit from the arrangement would mean returning to a system where their claims are discounted as anecdote.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, patient_led_research_networks, beneficiary,
    moderate, biographical, constrained, national).

% Researchers whose methodologically derived findings contradict community-validated experiential consensus. Under this reading, their credentials do not exempt their claims from being weighed against or subordinated to lived-experience testimony, and continuing to press contradictory findings can cost them standing, funding, or publication access within community-linked venues. Their exit is constrained by professional dependence on the same community networks for legitimacy and access to affected populations.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_dissenting_researchers, payer,
    moderate, biographical, constrained, national).

% Individuals within the same experiential community whose testimony diverges from the community's dominant validated narrative (e.g. a patient who does not attribute their symptoms to the community-recognized cause). Community validation processes that elevate collective lived experience can suppress minority experiential accounts that do not fit the consensus; these individuals have no methodological standard to appeal to and no alternative validating community to exit toward.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, outvoted_minority_experiential_factions, payer,
    powerless, biographical, trapped, local).

% Agencies and funding bodies that increasingly require community engagement and lived-experience input as a condition of grants or policy legitimacy. They benefit from being seen as responsive and inclusive but also observe from outside the validation mechanism itself, deciding how much weight to give its outputs relative to methodological evidence.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, policy_makers_and_funders, observer,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, policy_makers_and_funders, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__experiential_pluralism_reading, diffuse).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__experiential_pluralism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the real problem that methodologically rigorous inquiry has historically ignored, mismeasured, or actively harmed populations whose lived experience diverged from expert models (informed-consent violations, diagnostic dismissal, extractive field research) — by making community validation and testimony a recognized, non-derivative source of legitimate knowledge.
% TRANSFER_FUNCTION: Moves epistemic authority and downstream resources (funding priority, publication access, policy standing, media credibility) from credential-gatekept institutions toward community-based validators and testimony aggregators; within communities, it moves standing from individuals whose experience diverges from the community's dominant narrative toward those whose testimony aligns with it.
% ABSENT_VOICES: Individuals inside the credited communities whose experience contradicts the community-validated consensus are structurally underrepresented in the same forums that claim to represent 'the community' — their dissent is treated as evidence of insufficient consciousness-raising rather than as a competing claim to be weighed. Also largely absent: populations without organized advocacy infrastructure, whose lived experience never enters a validation process because no community organization exists to convene one.
% DISAPPEARANCE_RATIONALE: If this reading's authority collapsed overnight, community testimony and consensus statements would lose their current weight in funding decisions, litigation, and policy formation; credentialed methodological findings would regain default priority in contested cases; community-based organizations that currently function as recognized knowledge-validating bodies would lose that institutional standing and the resources tied to it.
% FOUNDING_PROBLEM: Communities with direct experience of harm, illness, or marginalization were routinely excluded from producing knowledge about their own conditions, and methodologically 'objective' research had repeatedly produced findings that were false, exploitative, or actively damaging to those communities (e.g. unethical field studies, dismissal of patient-reported symptoms, extractive ethnography) precisely because it discounted lived testimony as non-evidence.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians of science and bioethicists outside any advocacy organization corroborate the historical founding problem (documented cases of research harm and testimonial exclusion). However, whether the problem remains as acute today — given substantial institutionalization of community-engagement requirements in funding and IRB processes — is disputed by researchers and funders who are not themselves beneficiaries of either reading; some attest the corrective function is now largely achieved and the arrangement persists past its original necessity.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__experiential_pluralism_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__experiential_pluralism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__experiential_pluralism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.42) is moderate rather than low or high: this reading genuinely redresses real historical exclusion (a coordination function), but its own validation apparatus generates a new, milder asymmetric cost — dissenting researchers and internal minority voices pay through loss of standing, funding, or platform access via the same community-validation mechanism that legitimates the majority view. Suppression (0.38) is real but far below a snare's profile: no one is physically coerced, but professional and community standing costs function as effective informal suppression for dissenters who depend on the same networks for access. Accessibility collapse is low (0.3) — methodological alternatives remain visible and are not eliminated, only demoted in weight. Resistance is comparatively high (0.55) because credentialed researchers and some funders actively contest the reading's displacement of methodological priority; this is not a settled, uncontested arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Lived-experience communities and the advocacy organizations that administer validation are structural beneficiaries: the reading transfers epistemic and material standing to them, and their exit options are constrained mainly by dependence on the same standing they've gained (they would not want to exit). Credentialed dissenting researchers are targets: the reading subordinates their claims when in conflict with community consensus, and their exit is constrained by professional dependence on community access for continued relevance. The sharpest asymmetry is internal: outvoted minority experiential factions are powerless and trapped — they cannot appeal to methodological standards (this reading demotes those) and cannot exit to a rival validating community, since the whole point of the arrangement is that community consensus IS the validation mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — systematic exclusion and harm from methodologically gatekept research — was real and is corroborated by independent historians outside the advocacy structure. Whether that problem remains as acute today, given substantial institutionalization of community-engagement requirements, is contested; some observers outside the beneficiary set now read parts of the apparatus as having outlived its originating necessity while continuing to accrue standing and resources. This is precisely the kind of divergence the classification is built to surface without conflating a genuine coordination fix with an extraction mechanism that has crystallized around it — the tangled_rope label holds both truths simultaneously rather than forcing a single verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    corrective_versus_new_gatekeeping,
    'Is the experiential-pluralism reading still functioning primarily as a corrective to historical methodological exclusion, or has its validation apparatus become a new gatekeeping structure with its own extraction dynamics?',
    'Longitudinal tracking of whose claims are certified versus rejected by community validation processes over time, cross-referenced against whether rejected claims correlate with methodological rigor, minority-within-community status, or institutional favor with the validating organization''s leadership.',
    'If validation increasingly tracks alignment with organizational leadership rather than either lived experience breadth or methodological soundness, the reading is drifting from coordination toward tangled-rope or snare dynamics; if it continues to surface genuinely marginalized claims, the coordination function remains dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corrective_versus_new_gatekeeping, empirical, 'Whether the reading''s validation mechanism still serves its corrective function or has become self-serving gatekeeping.').

omega_variable(
    internal_dissent_suppression_mechanism,
    'Is the suppression experienced by outvoted minority experiential factions structural (formal exclusion from validation forums) or internalized (social pressure and fear of being seen as betraying the community), or both?',
    'Interview minority-faction members about whether they were formally barred from testimony processes or self-excluded due to anticipated social costs; track whether suppression persists for individuals who later leave the community entirely.',
    'If suppression is substantially internalized, the reading''s effective suppression on internal dissenters is higher than the structural measure alone suggests, since dissenters carry the silencing effect even after any formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_dissent_suppression_mechanism, empirical, 'Structural vs. internalized suppression of minority voices within validated communities.').

omega_variable(
    reading_versus_topic_epsilon_independence,
    'Does this reading''s authored ε (0.42) reflect the experiential-pluralist arrangement''s actual current operation, independent of the sibling readings'' contest over the same underlying topic of ''legitimate knowledge''?',
    'Cross-check against the credentialed_expertise_reading and hybrid_coproduction_reading files to confirm none of the three files'' ε values were adjusted to average toward a shared ''true'' value for the topic rather than being independently assessed per reading, per the ε-invariance and per-reading OQ-26 discipline.',
    'Confirms this file''s ε describes only this reading''s standing arrangement, not a blended verdict on the contested kernel as a whole.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_versus_topic_epsilon_independence, conceptual, 'Confirms compliance with the one-reading-per-file, reading-indexed ε discipline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__experiential_pluralism_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(legi_tr_t4, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 4, 0.13).
narrative_ontology:measurement(legi_tr_t8, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(legi_tr_t12, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(legi_tr_t16, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(legi_tr_t20, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(legi_tr_t24, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(legi_be_t4, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 4, 0.27).
narrative_ontology:measurement(legi_be_t8, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(legi_be_t12, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(legi_be_t16, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(legi_be_t20, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(legi_be_t24, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(legi_su_t4, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 4, 0.22).
narrative_ontology:measurement(legi_su_t8, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 8, 0.26).
narrative_ontology:measurement(legi_su_t12, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 12, 0.3).
narrative_ontology:measurement(legi_su_t16, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 16, 0.33).
narrative_ontology:measurement(legi_su_t20, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(legi_su_t24, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__experiential_pluralism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.1).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, hybrid_coproduction_reading).

% DUAL FORMULATION NOTE:
% Part of a three-story constraint family decomposing the natural-language concept 'legitimate knowledge boundary' per the ε-invariance principle: credentialed_expertise_reading (methodological rigor as primary validator), experiential_pluralism_reading (this file — lived experience and community validation as primary), and hybrid_coproduction_reading (mandatory integration of both). Each reading has a distinct beneficiary/victim structure and a distinct ε assessed against its own standing arrangement; none is a measurement of the others' arrangement. The credentialed_expertise_reading typically has higher accessibility_collapse and formal barriers to entry; the hybrid_coproduction_reading typically shows lower ε on both axes because integration dilutes the extraction pathway each pure reading carries alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
