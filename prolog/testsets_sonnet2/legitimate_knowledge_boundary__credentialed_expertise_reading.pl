% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__credentialed_expertise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__credentialed_expertise_reading, []).

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
 *   constraint_id: legitimate_knowledge_boundary__credentialed_expertise_reading
 *   human_readable: Credentialed Expertise Reading of the Legitimate Knowledge Boundary
 *   domain: epistemology/science_technology_studies/political_theory
 *
 * SUMMARY:
 *   This story authors the credentialed-expertise reading of a contested
 *   kernel: what counts as legitimate knowledge. Under this reading,
 *   legitimacy is conferred by methodologically rigorous inquiry validated
 *   through credentialed peer review — a real coordination function
 *   (filtering unreliable claims at scale) bundled with asymmetric extraction
 *   (systematic exclusion of accurate non-credentialed knowledge, career and
 *   funding capture by incumbent gatekeepers, and treatment of expert
 *   consensus as a truth-proxy even when consensus is wrong or
 *   industry-captured). The sibling readings — experiential pluralism and
 *   hybrid co-production — are NOT part of this constraint; they are separate
 *   constraints instantiating the same kernel differently, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - credentialed_academic_researchers: Primary beneficiary and co-administrator (institutional/constrained) — collects legitimacy and career capital from the boundary
 *   - peer_review_gatekeeping_institutions: Primary agenda-setter (institutional/arbitrage) — designs and enforces the qualifying pipeline
 *   - professional_licensing_bodies: Secondary beneficiary (institutional/arbitrage) — uses the boundary to control labor-market entry
 *   - community_knowledge_holders: Primary target (powerless/trapped) — generational knowledge dismissed on procedural grounds
 *   - unaffiliated_independent_researchers: Secondary target (moderate/constrained) — rigorous work blocked by affiliation requirements
 *   - affected_lay_populations: Diffuse target (powerless/trapped) — bear consequences of captured or delayed consensus without recourse
 *   - dissenting_credentialed_scientists: Excluded insider (moderate/constrained) — inside the gate but sidelined within it
 *   - regulatory_agencies: Analytical/secondary beneficiary (institutional/analytical) — relies on the boundary for defensible rulings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.58).
domain_priors:suppression_score(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.62).
domain_priors:theater_ratio(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__credentialed_expertise_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__credentialed_expertise_reading, "Credentialed Expertise Reading of the Legitimate Knowledge Boundary").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__credentialed_expertise_reading, "epistemology/science_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__credentialed_expertise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__credentialed_expertise_reading, '37d7f6e9-3045-4c86-bd62-d6ded5b46c2d').
narrative_ontology:cs_kernel_codification('37d7f6e9-3045-4c86-bd62-d6ded5b46c2d', distributed).
narrative_ontology:cs_authority_grounding('37d7f6e9-3045-4c86-bd62-d6ded5b46c2d', expertise).
narrative_ontology:cs_interpretation_layer_present('37d7f6e9-3045-4c86-bd62-d6ded5b46c2d').
narrative_ontology:cs_reading_relation('37d7f6e9-3045-4c86-bd62-d6ded5b46c2d', legitimate_knowledge_boundary__experiential_pluralism_reading, coexists_with).
narrative_ontology:cs_reading_relation('37d7f6e9-3045-4c86-bd62-d6ded5b46c2d', legitimate_knowledge_boundary__hybrid_coproduction_reading, influences).
narrative_ontology:cs_axiom('37d7f6e9-3045-4c86-bd62-d6ded5b46c2d', foundational, methodological_rigor_is_necessary_and_sufficient_for_legitimacy).
narrative_ontology:cs_axiom_status(methodological_rigor_is_necessary_and_sufficient_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('37d7f6e9-3045-4c86-bd62-d6ded5b46c2d', methodological_rigor_is_necessary_and_sufficient_for_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('37d7f6e9-3045-4c86-bd62-d6ded5b46c2d', secondary, credential_conferral_reliably_tracks_methodological_competence).
narrative_ontology:cs_axiom_status(credential_conferral_reliably_tracks_methodological_competence, holdable).
narrative_ontology:cs_axiom_grounding('37d7f6e9-3045-4c86-bd62-d6ded5b46c2d', credential_conferral_reliably_tracks_methodological_competence, instrumental).
narrative_ontology:cs_reference_frame('37d7f6e9-3045-4c86-bd62-d6ded5b46c2d', post_enlightenment_scientific_method_consensus).
narrative_ontology:cs_drift_state('37d7f6e9-3045-4c86-bd62-d6ded5b46c2d', contemporary_replication_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('37d7f6e9-3045-4c86-bd62-d6ded5b46c2d', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_academic_researchers).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, peer_review_gatekeeping_institutions).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, professional_licensing_bodies).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, community_knowledge_holders).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, unaffiliated_independent_researchers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, affected_lay_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, regulatory_agencies).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, methodological_rigor_produces_reliable_knowledge).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold degrees, journal access, and institutional affiliation that qualify their claims for the legitimacy stamp. Their career advancement, funding, and standing depend on the credential-and-peer-review pipeline continuing to be the recognized gate. They both benefit from the arrangement's exclusivity and administer its day-to-day enforcement through editorial boards, tenure committees, and hiring panels.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_academic_researchers, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_academic_researchers, agenda_setter).

% Journals, funding agencies, and accrediting bodies design and operate the review pipeline: who reviews, what counts as rigorous method, which venues confer legitimacy. They set the terms other parties must meet and can revise them, but their institutional survival and prestige depend on the boundary remaining sharply policed.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, peer_review_gatekeeping_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Certify who may practice or claim expert status in medicine, law, engineering, and allied fields, using credentialed peer-reviewed training as the qualifying standard. They collect licensing fees and control market entry; the arrangement directly protects their members' labor-market position.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, professional_licensing_bodies, beneficiary,
    institutional, generational, arbitrage, national).

% Hold generational, place-based, or embodied knowledge (traditional ecological knowledge, patient experience of illness, community-observed environmental harm) that does not pass through peer-reviewed channels. Their claims are routinely dismissed as anecdotal regardless of predictive accuracy, and they have no realistic path to credentialing that would make their knowledge count without years of institutional capture.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, community_knowledge_holders, payer,
    powerless, generational, trapped, local).

% Conduct rigorous inquiry outside university or institutional affiliation — citizen scientists, industry-adjacent analysts without academic posts, self-taught specialists. Their work can meet methodological standards but is frequently blocked from peer-reviewed venues by affiliation requirements, reviewer bias against non-credentialed authors, or lack of institutional access to review infrastructure.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, unaffiliated_independent_researchers, payer,
    moderate, biographical, constrained, national).

% Depend on the knowledge the boundary certifies as legitimate for decisions about their health, environment, and livelihoods, but cannot contest or verify expert consensus themselves. When credentialed consensus is wrong or captured by industry funding, they bear the consequences without recourse, since the very mechanism that would let them challenge it requires the credentials they lack.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, affected_lay_populations, payer,
    powerless, biographical, trapped, national).

% Rely on credentialed peer-reviewed science to justify regulatory decisions, which gives their rulings a defensible evidentiary basis and insulates them from claims of arbitrariness. They benefit from the boundary's authority while also observing where it produces predictably wrong or captured outcomes (e.g., delayed recognition of industry-caused harms).
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, regulatory_agencies, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, regulatory_agencies, beneficiary).

% Hold credentials but advance minority or heterodox positions that peer review structurally disfavors (novel methods, findings threatening funded research programs, or challenges to entrenched paradigms). They are nominally inside the gate but functionally excluded from the conversations that would validate their claims, illustrating that the boundary polices content as well as credentials.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, dissenting_credentialed_scientists, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__credentialed_expertise_reading, diffuse).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__credentialed_expertise_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, checkable standard for distinguishing careful, reproducible inquiry from unfounded assertion, allowing large numbers of strangers (patients, regulators, courts, the public) to rely on claims they cannot personally verify by trusting the process that produced them.
% TRANSFER_FUNCTION: Moves epistemic authority, funding, employability, and the power to define 'what counts as known' from anyone without institutional credentials toward those with them — including toward credentialed voices whose claims are less predictively reliable than excluded community or independent knowledge, when that community knowledge is dismissed on procedural rather than evidentiary grounds.
% ABSENT_VOICES: Community knowledge holders and affected lay populations are almost never present on editorial boards, grant panels, or standard-setting committees; unaffiliated researchers with rigorous but non-institutional work are filtered out before their claims are even evaluated on the merits. Dissenting credentialed scientists are nominally present but structurally sidelined within the review process itself.
% DISAPPEARANCE_RATIONALE: If credential-and-peer-review gatekeeping vanished overnight, professional licensing regimes would lose their qualifying standard, academic hiring and promotion systems would need a wholly different basis, funding agencies would need new criteria for allocating grants, and legitimacy contests over competing knowledge claims (traditional ecological knowledge vs. agency science, patient testimony vs. clinical trial data) would be adjudicated by entirely different, currently marginal mechanisms.
% FOUNDING_PROBLEM: In the absence of any check, claims proliferate that are unreliable, self-serving, or unfalsifiable; peer review and credentialing were built to filter inquiry through people trained to detect methodological error and to create accountability for claims before they reach the public or policy.
% FOUNDING_PROBLEM_CORROBORATION: Historians and sociologists of science outside the credentialing institutions (e.g., STS scholars documenting replication crises, industry-funded research capture, and systematic exclusion of community and indigenous knowledge from environmental and medical science) attest the founding problem of filtering unreliable claims remains partly live, but that the credentialing apparatus has also become a mechanism for excluding accurate, non-credentialed knowledge on structural rather than epistemic grounds — a finding the credentialing institutions themselves do not generally corroborate.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__credentialed_expertise_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__credentialed_expertise_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__credentialed_expertise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is substantial but not extreme: the coordination function is real and produces genuine reliability gains, so this is not authored as a pure snare. Suppression (0.62) is higher than extraction because the boundary's enforcement — journal rejection on affiliation grounds, licensing exclusion, dismissal of testimony as 'anecdotal' — operates independently of whether the excluded knowledge is actually less reliable. Accessibility collapse (0.71) is high: once a community or independent researcher understands the boundary, there are very few workable paths around it short of years of institutional capture. Resistance (0.55) reflects active pushback from STS scholars, indigenous knowledge advocates, patient-experience movements, and dissenting scientists, but this resistance has only partially dented the boundary's operation. Theater ratio (0.28, rising over the interval) captures a growing share of peer review activity that performs rigor-checking without functioning as one (rubber-stamp review, citation-cartel gatekeeping) — rising from 0.15 to 0.28 over 40 years as publication volume outpaced reviewer capacity.
 *
 * PERSPECTIVAL GAP:
 *   From the gatekeeping institutions' seat, this looks like a rope: a necessary quality filter without which knowledge production would collapse into noise. From the community-knowledge-holder seat, the same structure computes as extractive: their claims are held to a standard the boundary itself does not apply evenly (industry-funded credentialed research often receives less scrutiny than accurate non-credentialed claims). The tangled_rope claim captures this: a genuine coordination function (filtering unreliable claims) co-present with asymmetric extraction (excluding accurate claims on procedural grounds) through the same mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed researchers, gatekeeping institutions, and licensing bodies sit near the beneficiary end: they collect standing, funding, and market protection directly from the boundary's operation and could exit into alternative epistemic arrangements only at the cost of losing that standing (hence 'arbitrage' or 'constrained' rather than 'trapped' exit — they retain the option to relocate within the system). Community knowledge holders and affected lay populations sit near the full-target end: trapped exit, generational time horizon, and no credentialing path that would make their knowledge count without essentially becoming the institution that currently excludes them. Unaffiliated independent researchers sit closer to symmetric-but-constrained: they can sometimes publish, but face a structurally higher bar than affiliated peers doing comparable work.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unfiltered, unreliable claims — remains partly live, which is why this is NOT classified as a pure snare or piton: peer review does catch real errors and does provide real accountability. But the mechanism has also drifted into serving a second function its founders did not intend: protecting incumbent credentialed status and excluding accurate non-credentialed knowledge. The mandatrophy question is whether the boundary's current scope (excluding TEK, patient testimony, community environmental monitoring wholesale) is calibrated to the residual risk of unfiltered claims, or has outrun it. This story does not resolve that question — it is carried as an omega — but the contested founding_problem_status flags the drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rigor_gatekeeping_separability,
    'Is the methodological-rigor-checking function of peer review separable from its credentialing-and-affiliation-gatekeeping function, or are they structurally fused?',
    'Compare outcomes in double-blind or credential-masked review pilots (some journals have trialed this) against standard review: if masked review admits comparable rigor from non-credentialed authors at similar rates, the functions are separable and much of the measured extraction is excess gatekeeping rather than necessary quality control.',
    'If separable, a substantial share of the boundary''s extraction is removable without sacrificing the coordination function, supporting reform toward the hybrid_coproduction_reading. If fused, the current extraction level may be closer to the coordination cost floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rigor_gatekeeping_separability, empirical, 'Whether rigor-checking and credential-gatekeeping are one mechanism or two bundled together.').

omega_variable(
    consensus_as_truth_proxy_reliability,
    'How reliable is credentialed expert consensus as a truth-proxy across domains, and does that reliability vary systematically with the presence of funding conflicts or paradigm-defense incentives?',
    'Meta-scientific replication studies and post-hoc audits comparing credentialed consensus positions later reversed against independently-sourced claims that were dismissed and later vindicated (e.g., H. pylori, several environmental exposure cases).',
    'A high reversal rate concentrated in funding-conflicted or paradigm-defensive domains would support classifying the extraction as substantially non-incidental (i.e., the boundary protects incumbents more than it protects truth); a low reversal rate would support the coordination framing dominating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_as_truth_proxy_reliability, empirical, 'Empirical reliability of credentialed consensus as a truth-proxy, and its correlation with capture.').

omega_variable(
    kernel_framing_choice,
    'Is the credentialed-expertise reading the correct primary framing for evaluating ''legitimate knowledge,'' or does treating it as merely one reading among three (alongside experiential pluralism and hybrid co-production) already presuppose the pluralist conclusion this story is meant to leave open?',
    'None fully resolves this — it is a conceptual framing choice. Cross-reference with how policy and legal systems actually adjudicate disputed knowledge claims (which reading do courts, regulators, and funding bodies operationalize in practice) as a partial empirical anchor.',
    'If courts and regulators overwhelmingly operationalize the credentialed-expertise reading as though it were simply correct rather than one reading among several, that itself is evidence of this reading''s institutional dominance — which is a fact about power, not about epistemic validity, and should not be read as vindicating the reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether treating credentialed expertise as one reading among three, rather than as the default correct account, itself needs justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__credentialed_expertise_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(legi_tr_t8, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(legi_tr_t16, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(legi_tr_t24, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(legi_tr_t32, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(legi_tr_t40, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(legi_be_t8, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(legi_be_t16, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(legi_be_t24, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(legi_be_t32, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(legi_be_t40, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(legi_su_t8, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(legi_su_t16, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(legi_su_t24, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(legi_su_t32, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(legi_su_t40, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__credentialed_expertise_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.1).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, experiential_pluralism_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, hybrid_coproduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the legitimate_knowledge_boundary kernel. credentialed_expertise_reading authors the credentialing-and-peer-review arrangement as the standing arrangement under contest, with substantial extraction (0.58) from excluded non-credentialed knowledge holders. experiential_pluralism_reading and hybrid_coproduction_reading are separate constraint stories with their own ε values, beneficiary/victim structures, and classifications — they are not alternative measurements of this constraint but structurally distinct claims about what legitimizes knowledge. The credentialed-expertise reading is authored here as upstream/dominant in current institutional practice (courts, regulators, and funding bodies overwhelmingly operationalize it), which creates downstream pressure on the legitimacy conditions available to the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_knowledge_boundary__credentialed_expertise_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
