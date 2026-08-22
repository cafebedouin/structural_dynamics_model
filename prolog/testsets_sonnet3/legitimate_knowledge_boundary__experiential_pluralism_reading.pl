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
 *   constraint_id: legitimate_knowledge_boundary__experiential_pluralism_reading
 *   human_readable: Experiential-Pluralist Reading of the Legitimate Knowledge Boundary
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This story instantiates one reading of the legitimate_knowledge_boundary
 *   kernel: the experiential-pluralist position, which holds that legitimate
 *   knowledge arises primarily from lived experience and community
 *   validation, with formal methodology as one input among several rather
 *   than the gatekeeping standard. This reading has gained real institutional
 *   traction over the past two decades in public health, disability studies,
 *   environmental policy, and research funding criteria. It is generated here
 *   as a single, ε-stable constraint, not as a summary of the whole kernel
 *   contest; the credentialed_expertise_reading and
 *   hybrid_coproduction_reading are separate constraint stories with their
 *   own ε values and stakeholder structures, linked via
 *   network.affects_constraints. The extraction this reading authors is real
 *   but moderate: it redistributes epistemic authority away from credentialed
 *   review and toward community-validation brokers, and that redistribution
 *   has both a genuine coordination function (correcting for documented
 *   institutional dismissal of marginalized testimony) and an asymmetric cost
 *   (individuals who rely on unvalidated claims, and credentialed
 *   researchers/junior scholars caught on the wrong side of a shifting
 *   institutional standard).
 *
 * KEY AGENTS:
 *   - community_knowledge_holders: primary beneficiary (organized/mobile) — gains epistemic standing previously denied
 *   - patient_advocacy_networks: beneficiary (organized/mobile) — gains funding and regulatory voice
 *   - indigenous_knowledge_stewards: beneficiary (organized/constrained) — restored authority, still institutionally dependent
 *   - participatory_action_researchers: agenda_setter (moderate/constrained) — administers the validation procedures, holds real gatekeeping power
 *   - credentialed_researchers_excluded_from_funding: payer (moderate/constrained) — bears funding and career cost of the shifted standard
 *   - individuals_harmed_by_unvalidated_community_claims: payer (powerless/trapped) — bears the risk of substituted error-correction
 *   - junior_scholars_in_experiential_paradigms: payer/beneficiary (powerless/constrained) — gains standing now, exposed if the standard shifts back
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
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__experiential_pluralism_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__experiential_pluralism_reading, "Experiential-Pluralist Reading of the Legitimate Knowledge Boundary").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__experiential_pluralism_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__experiential_pluralism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__experiential_pluralism_reading, '23db6b86-f381-47ad-b4d9-a143d9a6a10d').
narrative_ontology:cs_kernel_codification('23db6b86-f381-47ad-b4d9-a143d9a6a10d', distributed).
narrative_ontology:cs_authority_grounding('23db6b86-f381-47ad-b4d9-a143d9a6a10d', practice).
narrative_ontology:cs_interpretation_layer_present('23db6b86-f381-47ad-b4d9-a143d9a6a10d').
narrative_ontology:cs_reading_relation('23db6b86-f381-47ad-b4d9-a143d9a6a10d', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('23db6b86-f381-47ad-b4d9-a143d9a6a10d', legitimate_knowledge_boundary__hybrid_coproduction_reading, influences).
narrative_ontology:cs_axiom('23db6b86-f381-47ad-b4d9-a143d9a6a10d', foundational, lived_experience_is_epistemically_primary).
narrative_ontology:cs_axiom_status(lived_experience_is_epistemically_primary, holdable).
narrative_ontology:cs_axiom_grounding('23db6b86-f381-47ad-b4d9-a143d9a6a10d', lived_experience_is_epistemically_primary, conventional).
narrative_ontology:cs_axiom('23db6b86-f381-47ad-b4d9-a143d9a6a10d', secondary, methodology_is_one_tool_among_many).
narrative_ontology:cs_axiom_status(methodology_is_one_tool_among_many, holdable).
narrative_ontology:cs_axiom_grounding('23db6b86-f381-47ad-b4d9-a143d9a6a10d', methodology_is_one_tool_among_many, instrumental).
narrative_ontology:cs_reference_frame('23db6b86-f381-47ad-b4d9-a143d9a6a10d', pre_credentialing_testimonial_authority).
narrative_ontology:cs_drift_state('23db6b86-f381-47ad-b4d9-a143d9a6a10d', contemporary_institutionalized_participatory_research, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('23db6b86-f381-47ad-b4d9-a143d9a6a10d', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, community_knowledge_holders).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, patient_advocacy_networks).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, indigenous_knowledge_stewards).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, participatory_action_researchers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_researchers_excluded_from_funding).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, individuals_harmed_by_unvalidated_community_claims).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, junior_scholars_in_experiential_paradigms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, junior_scholars_in_experiential_paradigms).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, standpoint_epistemology).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, situated_knowledge_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Long-marginalized communities (e.g., disability communities, indigenous groups, patient collectives) whose lived experience of a phenomenon was historically dismissed by credentialed institutions. Under this reading their testimony and collective validation processes are treated as sufficient grounds for legitimate knowledge claims, giving them standing in policy and research agendas they were previously excluded from.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, community_knowledge_holders, beneficiary,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, community_knowledge_holders, agenda_setter).

% Organize around shared illness experience to assert knowledge claims about symptoms, treatment efficacy, and disease mechanisms that clinical trials had not yet captured or had dismissed. Gain funding, media standing, and regulatory input under the pluralist standard without needing to first pass through credentialing gates.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, patient_advocacy_networks, beneficiary,
    organized, biographical, mobile, national).

% Hold multigenerational ecological and medicinal knowledge systems that Western credentialing structures historically refused to recognize as knowledge at all. Under the experiential-pluralist reading, their community-internal validation processes are treated as epistemically sufficient on their own terms, restoring authority long denied them — though they remain structurally dependent on external institutions (courts, universities, NGOs) to translate that authority into resources.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, indigenous_knowledge_stewards, beneficiary,
    organized, civilizational, constrained, regional).

% Academics and practitioners who design and administer community-based participatory research methods, positioning themselves as brokers who certify which experiential claims count as validated knowledge. They set the terms of what 'community validation' means procedurally, giving them real gatekeeping power even while claiming to flatten hierarchy.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, participatory_action_researchers, agenda_setter,
    moderate, biographical, constrained, national).

% Methodologically trained researchers whose grant proposals, journal submissions, or tenure cases are increasingly evaluated against a standard requiring demonstrated community buy-in or lived-experience framing, regardless of the substantive quality of their methods. They bear a real cost when institutional funders adopt the pluralist standard as a formal requirement rather than a complement, and they have limited ability to opt out if their subfield has shifted.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_researchers_excluded_from_funding, payer,
    moderate, biographical, constrained, national).

% People who rely on a community-validated claim (e.g., an alternative treatment protocol, a folk-diagnostic framework) that turns out to be false or harmful in their specific case, without the buffering that methodological cross-checking would have provided. They bear the downside risk of a validation regime that substitutes testimonial consensus for systematic error-correction, and have no realistic way to detect the substitution in advance.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, individuals_harmed_by_unvalidated_community_claims, payer,
    powerless, immediate, trapped, local).

% Early-career researchers who build their scholarly identity around experiential and standpoint methods gain legitimacy and community standing from this reading, but are simultaneously locked into a paradigm that credentialed review committees, funders, and tenure boards outside their subfield may not recognize — leaving them exposed if the institutional pendulum swings back toward the credentialed reading.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, junior_scholars_in_experiential_paradigms, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, junior_scholars_in_experiential_paradigms, beneficiary).

% The sibling reading of the same kernel, treated here as a non-agent structural placeholder rather than a party: it names the rival account of legitimate knowledge that this reading's rise displaces from unquestioned default status in the domains where it takes hold.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_expertise_reading, excluded,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_expertise_reading).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__experiential_pluralism_reading, diffuse).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__experiential_pluralism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates dispersed, often tacit or embodied knowledge that formal methodology structurally cannot easily capture (patient symptom patterns, ecological cycles observed across generations, the lived texture of discrimination) and gives it standing to shape research agendas, policy, and resource allocation without requiring translation through credentialing gates first.
% TRANSFER_FUNCTION: Moves epistemic authority, research funding, media standing, and regulatory influence away from credentialed institutional gatekeepers and toward community-based validation processes and the researchers/brokers who administer them; moves error-correction risk away from formal methodological review and onto individuals who act on community-validated claims.
% ABSENT_VOICES: Individuals harmed by a specific unvalidated community claim rarely have organized representation in the debate over the boundary itself — they surface only after harm occurs, in isolated case reports rather than as a standing constituency. Statisticians and methodologists who work on error-correction across domains (not defenders of any particular credentialed hierarchy) are also largely absent from the political coalition contesting this reading, since the debate is framed as community vs. institution rather than as a debate about failure-detection design.
% DISAPPEARANCE_RATIONALE: Community advocates would say the world rearranges sharply: decades of restored standing for marginalized knowledge-holders would collapse back into institutional gatekeeping, and hard-won funding and policy access would evaporate. Credentialed-reading defenders would say the world mostly reverts to a prior equilibrium that was itself functioning, with methodological review resuming its error-correction role. Because the two camps disagree about whether the pre-existing arrangement was itself legitimate, the disappearance verdict is genuinely contested rather than resolvable from either seat alone.
% FOUNDING_PROBLEM: Formal methodological and credentialing institutions had systematically dismissed or misclassified the testimony of marginalized groups (patients, indigenous communities, disabled people, colonized populations) as anecdote rather than evidence, producing decades of documented harm: misdiagnosis, extractive research, policy blind spots, and erasure of functioning knowledge systems that predated the credentialing institutions that dismissed them.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science and medical sociologists outside the advocacy coalition (e.g., documented cases of clinical dismissal of patient-reported symptoms later vindicated by research, colonial-era suppression of indigenous ecological knowledge later validated) corroborate that the founding problem was real and serious. However, science-and-technology-studies scholars and public health researchers not aligned with either reading also document cases where the same experiential-validation standard, once formalized as a funding requirement, has itself produced harm by displacing systematic error-correction — meaning the founding problem's current status (solved, ongoing, or transformed into a new problem) is actively disputed by observers on all sides, not just by the reading's own beneficiaries.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__experiential_pluralism_reading, contested).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__experiential_pluralism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored moderate (0.42 by interval end) because the reading's redistribution of epistemic authority produces real winners and losers but does not operate through coercive extraction of resources in the way a rent-seeking arrangement would — the harm is mostly displaced risk (individuals acting on unvalidated claims) and displaced institutional advantage (funding and standing moving between researcher classes), not direct expropriation. Suppression is moderate (0.38) because the reading does not typically forbid methodological work outright, but institutions that adopt it as a formal funding or publication requirement do actively disadvantage researchers who decline to frame their work experientially — that is active enforcement, not mere preference-shifting. Theater ratio is low-moderate (0.28) and rising: some community-validation processes are genuine coordination (rigorous participatory design), but a growing share of institutional 'community validation' compliance is procedural box-checking (a co-authorship credit, a single advisory-board meeting) that performs the pluralist standard without doing its substantive work.
 *
 * DIRECTIONALITY LOGIC:
 *   Community knowledge holders, patient networks, and indigenous stewards are structural beneficiaries: the reading directly restores or creates epistemic standing they previously lacked, so their directionality sits near the beneficiary end. Participatory action researchers occupy a genuine dual position — they administer the validation apparatus (agenda-setting power) while also benefiting from the reading's rise in status; this is why they carry only 'agenda_setter' rather than a pure beneficiary role, to avoid understating their gatekeeping function. Credentialed researchers excluded from funding and junior scholars in experiential paradigms are targets of the redistribution — the former lose resources under the new standard, the latter gain standing now but carry an identity-lock risk if institutions later revert. Individuals harmed by unvalidated claims are the clearest targets: powerless, trapped, immediate time horizon, bearing concentrated downside from a systemic validation choice they had no part in making.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — systematic institutional dismissal of marginalized testimony — was real and is corroborated by historians of science and medical sociology outside the advocacy coalition; this reading is not merely capture dressed as coordination. But the founding_problem_status is authored 'contested' rather than 'live' or 'dead' because the reading, once formalized into funding and publication requirements, has itself begun producing a parallel problem: displaced error-correction risk onto individuals who cannot detect when 'community validation' has substituted for systematic checking. Classifying this as tangled_rope rather than snare or rope reflects that both the genuine coordination function and the asymmetric extraction are simultaneously present and require active enforcement to hold — collapsing to either pure category would erase one half of the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    validation_reliability_gap,
    'Does community-based validation, absent methodological cross-checking, produce error rates comparable to, better than, or worse than credentialed peer review for the specific classes of claims this reading treats as legitimate?',
    'Comparative tracking of outcomes for claims validated purely through community consensus versus claims subjected to methodological review, across matched domains (e.g., patient-reported treatment efficacy vs. RCT-confirmed efficacy), with long-run follow-up on harm rates.',
    'If community validation shows comparable or better reliability for the claim classes it is applied to, the extraction attributed to displaced error-correction risk should be revised downward substantially, moving the classification toward rope. If it shows materially worse reliability, the victim-side extraction is understated and the classification should move toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(validation_reliability_gap, empirical, 'Whether experiential/community validation is empirically as reliable as methodological review for the claims it displaces.').

omega_variable(
    gatekeeper_substitution_ambiguity,
    'Does the experiential-pluralist reading actually flatten epistemic hierarchy, or does it substitute a new gatekeeper (the participatory-action-research broker who certifies what counts as valid community validation) for the old one (the credentialed peer reviewer)?',
    'Institutional ethnography of participatory research programs: track who actually decides which community claims are treated as validated, what credentials or institutional position that decider holds, and whether their decisions are themselves subject to any check.',
    'If the broker role functions as a de facto new credentialing gate, the reading is less a democratization of knowledge than a relocation of gatekeeping authority to a differently-credentialed class — this would sharpen the tangled_rope classification and increase the authored extraction for the broker''s beneficiary role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeper_substitution_ambiguity, conceptual, 'Whether community-validation brokers constitute a substitute gatekeeping class rather than a flattening of hierarchy.').

omega_variable(
    kernel_framing_alternative,
    'Is the correct unit of analysis ''the legitimate_knowledge_boundary kernel with three competing readings,'' or is experiential_pluralism_reading itself decomposable into distinct sub-claims (e.g., patient-experience claims about symptom phenomenology vs. indigenous ecological-knowledge claims vs. disability-studies standpoint claims) that should be separate constraints because their ε values plausibly diverge sharply by domain?',
    'Domain-by-domain audit of extraction and suppression metrics: if patient-advocacy contexts show much higher or lower ε than indigenous-knowledge contexts under the same reading, the single-story treatment is masking an ε-invariance violation and should be decomposed further.',
    'If domain-level ε values diverge sharply, this single story is itself an under-decomposed conflation and should be split into per-domain constraint stories, each linked into the same kernel family via network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether this reading is itself a further-decomposable conflation across domains with materially different extraction profiles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__experiential_pluralism_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(legi_tr_t4, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(legi_tr_t8, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(legi_tr_t12, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(legi_tr_t16, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(legi_tr_t20, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 20, 0.26).
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
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(legi_su_t4, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 4, 0.24).
narrative_ontology:measurement(legi_su_t8, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(legi_su_t12, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 12, 0.31).
narrative_ontology:measurement(legi_su_t16, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 16, 0.34).
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
% This story is one of three siblings decomposing the natural-language concept 'legitimate knowledge boundary' per the ε-invariance principle. credentialed_expertise_reading authors a low-extraction, high-accessibility-collapse profile centered on methodological gatekeeping costs to non-credentialed claimants. hybrid_coproduction_reading authors a moderate profile reflecting the transaction costs and negotiated authority of integration processes. This story (experiential_pluralism_reading) authors a moderate, rising-extraction profile centered on displaced error-correction risk and broker gatekeeping. Each story has its own claimed_type, stakeholders, and ε; none averages or references the others' metrics directly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
