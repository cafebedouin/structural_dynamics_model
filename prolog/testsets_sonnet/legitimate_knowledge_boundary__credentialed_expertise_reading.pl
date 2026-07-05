% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__credentialed_expertise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   This story instantiates one reading of the contested 'legitimate
 *   knowledge boundary' kernel: the claim that legitimate knowledge derives
 *   specifically from methodologically rigorous inquiry validated by
 *   credentialed peer review. This is a real, defensible position with
 *   genuine coordination value — it solves an authentic error-correction and
 *   quality-control problem. But the same boundary that filters out
 *   unreliable claims also filters out reliable claims that arrive in the
 *   wrong institutional form, and the filtering apparatus (journals,
 *   credentialing bodies, universities) collects durable rents (funding,
 *   prestige, gatekeeping authority) from administering the filter. That
 *   combination — genuine coordination function plus asymmetric extraction
 *   requiring active enforcement (editorial gatekeeping, licensing
 *   requirements, citation norms that discount non-credentialed sources) — is
 *   the signature of tangled_rope, not a clean rope or a pure snare. The
 *   sibling readings (experiential_pluralism_reading,
 *   hybrid_coproduction_reading) are separate constraint stories with their
 *   own ε, beneficiary structures, and classifications; they are not folded
 *   into this one.
 *
 * KEY AGENTS:
 *   - peer_review_gatekeeping_institutions: agenda_setter (institutional/arbitrage) — designs and administers the boundary
 *   - credentialed_researchers: beneficiary (organized/mobile) — holds standing granted by the boundary
 *   - research_universities: beneficiary/agenda_setter (institutional/arbitrage) — confers credentials, collects institutional rents
 *   - community_knowledge_holders: payer (powerless/trapped) — knowledge discounted absent credentialed translation
 *   - independent_and_unaffiliated_researchers: payer (moderate/constrained) — methodologically sound work penalized for lacking affiliation
 *   - affected_lay_publics: payer/excluded (powerless/trapped) — bears policy consequences without procedural standing
 *   - science_and_technology_studies_scholars: observer (analytical) — documents where rigor functions as claimed vs. as boundary-work
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.58).
domain_priors:suppression_score(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.62).
domain_priors:theater_ratio(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__credentialed_expertise_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__credentialed_expertise_reading, "Credentialed Expertise Reading of the Legitimate Knowledge Boundary").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__credentialed_expertise_reading, "epistemology/science_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__credentialed_expertise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__credentialed_expertise_reading, '02e16ff7-95ca-44e1-9f0b-6297b826e03a').
narrative_ontology:cs_kernel_codification('02e16ff7-95ca-44e1-9f0b-6297b826e03a', distributed).
narrative_ontology:cs_authority_grounding('02e16ff7-95ca-44e1-9f0b-6297b826e03a', expertise).
narrative_ontology:cs_interpretation_layer_present('02e16ff7-95ca-44e1-9f0b-6297b826e03a').
narrative_ontology:cs_reading_relation('02e16ff7-95ca-44e1-9f0b-6297b826e03a', legitimate_knowledge_boundary__experiential_pluralism_reading, coexists_with).
narrative_ontology:cs_reading_relation('02e16ff7-95ca-44e1-9f0b-6297b826e03a', legitimate_knowledge_boundary__hybrid_coproduction_reading, influences).
narrative_ontology:cs_axiom('02e16ff7-95ca-44e1-9f0b-6297b826e03a', foundational, methodological_rigor_is_necessary_and_sufficient_for_legitimacy).
narrative_ontology:cs_axiom_status(methodological_rigor_is_necessary_and_sufficient_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('02e16ff7-95ca-44e1-9f0b-6297b826e03a', methodological_rigor_is_necessary_and_sufficient_for_legitimacy, instrumental).
narrative_ontology:cs_axiom('02e16ff7-95ca-44e1-9f0b-6297b826e03a', secondary, credentialing_is_the_valid_proxy_for_rigor_competence).
narrative_ontology:cs_axiom_status(credentialing_is_the_valid_proxy_for_rigor_competence, holdable).
narrative_ontology:cs_axiom_grounding('02e16ff7-95ca-44e1-9f0b-6297b826e03a', credentialing_is_the_valid_proxy_for_rigor_competence, conventional).
narrative_ontology:cs_reference_frame('02e16ff7-95ca-44e1-9f0b-6297b826e03a', professionalized_peer_review_consensus).
narrative_ontology:cs_drift_state('02e16ff7-95ca-44e1-9f0b-6297b826e03a', post_replication_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('02e16ff7-95ca-44e1-9f0b-6297b826e03a', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_researchers).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, peer_review_gatekeeping_institutions).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, research_universities).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, community_knowledge_holders).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, independent_and_unaffiliated_researchers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, affected_lay_publics).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, methodological_rigor_produces_reliable_knowledge).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Journals, disciplinary societies, and accreditation bodies design and administer the review process that determines what counts as validated knowledge. They set methodological standards, select reviewers from the credentialed pool, and can exclude submissions that do not conform to disciplinary norms regardless of the empirical or practical merit of the claim.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, peer_review_gatekeeping_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold degrees and institutional affiliations that grant standing to publish, review, and adjudicate knowledge claims. Careers, funding, and authority all flow through the credentialing and publication system; they benefit from the boundary even when individually critical of its excesses, and can move between institutions without losing standing.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_researchers, beneficiary,
    organized, biographical, mobile, global).

% Confer the credentials that grant epistemic standing and receive prestige, tuition, and grant overhead as a function of their monopoly on legitimate credential-granting. They co-administer the boundary alongside journals and licensing bodies.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, research_universities, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, research_universities, agenda_setter).

% Possess generations of situated observation (traditional ecological knowledge, lived experience of illness or discrimination, local environmental monitoring) that is discounted or requires translation through a credentialed intermediary before it can enter policy or scientific discourse. Their knowledge claims are treated as anecdote until validated by someone with standing, and they cannot obtain standing without entering the credentialing system on its terms.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, community_knowledge_holders, payer,
    powerless, generational, trapped, regional).

% Conduct methodologically sound inquiry outside university or institutional affiliation and find their work systematically harder to publish, cite, or fund because they lack the institutional letterhead that signals credibility to gatekeepers. They can sometimes buy their way back in through affiliation or partnership, at a real cost in autonomy and time.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, independent_and_unaffiliated_researchers, payer,
    moderate, biographical, constrained, national).

% Bear the consequences of policy built on expert consensus (drug approvals, environmental risk assessments, economic forecasts) but have no standing to contest the consensus through the system's own procedures. When expert consensus later reverses, they absorb the interim harm without recourse.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, affected_lay_publics, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, affected_lay_publics, excluded).

% Study the credentialing and peer-review system as a social institution, documenting where methodological rigor functions as claimed and where it functions as boundary-work that protects disciplinary turf and funding streams.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, science_and_technology_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__credentialed_expertise_reading, peer_review_gatekeeping_institutions).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__credentialed_expertise_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, checkable standard for distinguishing well-tested claims from untested ones, reducing the cost of evaluating every knowledge claim from scratch and giving downstream users (policymakers, clinicians, courts) a workable proxy for reliability.
% TRANSFER_FUNCTION: Moves epistemic authority, funding, and policy influence toward credentialed institutions and away from non-credentialed knowledge holders, even where the latter possess directly relevant observational or experiential knowledge; also moves the cost of translation (the labor of converting local knowledge into publishable form) onto those least equipped to pay it.
% ABSENT_VOICES: Community knowledge holders, patients, indigenous knowledge-keepers, and independent researchers would object that the boundary systematically discounts knowledge that fails to arrive in the approved format, but they are not seated on editorial boards, grant panels, or licensing committees that set the boundary's terms.
% DISAPPEARANCE_RATIONALE: Credentialed researchers and gatekeeping institutions would say the world rearranges catastrophically — quality control collapses and misinformation floods policy-relevant domains. Community knowledge holders and STS scholars would say the underlying knowledge-production activity continues largely unchanged; what disappears is a specific distributional arrangement over whose knowledge counts, not knowledge production itself. The dispute is exactly the site the kernel contest identifies.
% FOUNDING_PROBLEM: Pre-institutional inquiry had no reliable way to distinguish careful, checkable claims from confident but untested ones, and no mechanism to correct error once a claim entered circulation — peer review and credentialing arose to solve genuine problems of quality control and error-correction at scale.
% FOUNDING_PROBLEM_CORROBORATION: Credentialed institutions attest the founding problem remains fully live (replication crises, misinformation) and justifies the current boundary in full. STS scholars and community knowledge advocates, writing from outside the credentialing system, attest that the original quality-control problem is real but has been layered over with boundary-work that protects institutional rents unrelated to error-correction — citing documented cases of credentialed consensus being wrong for extended periods while dissenting non-credentialed observation was correct (e.g., early AIDS activism, industrial contamination whistleblowing) and excluded from the process that could have corrected it faster.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__credentialed_expertise_reading, contested).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__credentialed_expertise_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) reflects genuine but growing rent-capture: credentialing and publication gatekeeping increasingly track institutional affiliation and citation-network position rather than tracking rigor alone, and the temporal series shows this drifting upward across the interval as replication crises and metascience research have documented growing daylight between the process's stated function and its actual selection effects. Suppression (0.62) is meaningfully high because entry into the credentialed pool requires years of costly, path-dependent training with no equivalent fast lane for demonstrated competence acquired otherwise — this is a raw structural property, not scaled by scope. Accessibility collapse is high (0.71) because once a domain is captured by credentialed gatekeeping, alternative validation pathways (community review, practitioner consensus, direct replication by non-credentialed actors) are treated as categorically inadmissible rather than as competing evidence. Resistance is moderate (0.55): open-access movements, citizen science, and STS critique represent real friction against the boundary, but rarely displace it.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats, this looks like rope — pure quality-control coordination that happens to also confer status on those who do it well. From the payer seats (community knowledge holders, independent researchers, affected publics) the same structure computes as extraction: real costs imposed by a boundary they cannot contest through its own procedures, riding on a coordination function they do not deny exists but that does not require the scope of exclusion actually practiced.
 *
 * DIRECTIONALITY LOGIC:
 *   Peer review institutions and universities administer the boundary and collect its rents directly — low d, strong beneficiary position. Credentialed researchers benefit structurally even when they did not design the system, and their mobility (they can move between institutions without losing standing) keeps their d low. Community knowledge holders and affected lay publics are trapped: they cannot exit the domains the boundary governs (health policy, environmental regulation) and cannot enter the credentialing system without absorbing its full cost — high d. Independent researchers sit in between: real but constrained exit (affiliation, collaboration) pulls their d toward the target end but not to the extreme.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reliable error-correction at scale) is not fully dead — it remains a live coordination need, which is why this classifies as tangled_rope rather than snare. But the founding_problem_status is authored contested precisely because credentialed institutions and external critics disagree on how much of the current apparatus still serves that founding function versus how much has calcified into boundary-work that protects institutional position independent of epistemic reliability. Treating this as a pure rope would erase the documented extraction; treating it as a pure snare would erase the genuine and continuing coordination value of methodological standards.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rigor_vs_gatekeeping_boundary,
    'Is the extraction measured here inseparable from genuine methodological rigor, or is it a separable layer of institutional boundary-work riding on top of rigor?',
    'Compare acceptance and citation outcomes for methodologically matched studies that differ only in author credentialing/affiliation; a persistent gap after controlling for methodological quality would isolate the boundary-work component.',
    'If largely inseparable, the high extractiveness score partly reflects the genuine cost of maintaining rigor rather than pure rent; if largely separable, most of the measured extraction is boundary-work unrelated to epistemic function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rigor_vs_gatekeeping_boundary, empirical, 'Whether measured extraction tracks rigor itself or a separable gatekeeping layer.').

omega_variable(
    kernel_reading_selection_stakes,
    'This story is one of three declared readings of the legitimate_knowledge_boundary kernel (credentialed_expertise, experiential_pluralism, hybrid_coproduction). Which reading a given domain or institution adopts materially changes whose knowledge counts and whose costs are borne — is the choice among readings itself an empirical, conceptual, or political question?',
    'Track domains that have shifted reading (e.g., co-production mandates in environmental health research, indigenous knowledge inclusion in climate science) and measure whether outcomes (accuracy, equity, legitimacy) differ by reading in ways that would settle the choice empirically rather than by pure preference.',
    'If outcome differences are measurable and consistent, the reading choice could be partly empirically adjudicated; if not, the kernel contest remains a genuinely political/normative dispute with no single correct resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_stakes, conceptual, 'Whether the choice among kernel readings is empirically resolvable or irreducibly normative.').

omega_variable(
    credentialing_as_natural_vs_constructed,
    'Is the credentialing/peer-review boundary a naturalized feature of how knowledge production must scale, or a historically contingent institutional arrangement that could be reorganized without loss of reliability?',
    'Comparative historical analysis of knowledge-validation regimes that achieved comparable reliability through different institutional forms (e.g., pre-institutional scientific societies, open science / preprint-plus-post-publication-review regimes, indigenous knowledge transmission systems with strong internal error-correction norms).',
    'If comparable reliability is achievable through less exclusionary institutional forms, the current boundary''s exclusion is a policy choice, not a functional necessity, weakening the rope component of the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credentialing_as_natural_vs_constructed, conceptual, 'Whether the credentialing boundary is functionally necessary or one contingent arrangement among viable alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__credentialed_expertise_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(legi_tr_t8, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 8, 0.23).
narrative_ontology:measurement(legi_tr_t16, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(legi_tr_t24, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement(legi_tr_t32, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 32, 0.32).
narrative_ontology:measurement(legi_tr_t40, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 40, 0.34).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(legi_be_t8, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(legi_be_t16, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(legi_be_t24, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(legi_be_t32, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(legi_be_t40, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(legi_su_t8, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 8, 0.52).
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
% This constraint is one of three sibling readings of the legitimate_knowledge_boundary kernel. credentialed_expertise_reading (this story) claims legitimate knowledge derives from rigor validated by credentialed peer review, with high extraction concentrated on non-credentialed knowledge holders. experiential_pluralism_reading claims legitimacy from lived experience and community validation, with a structurally different beneficiary/victim map (likely inverting who is empowered vs. excluded). hybrid_coproduction_reading claims legitimacy requires integration of both, and functions as a structural response to the tension between the other two. The three stories share the same underlying epistemic-authority contest but are NOT the same constraint — each has distinct ε, distinct beneficiaries and victims, and distinct classification, per the ε-invariance principle. Network edges here record that adoption of this reading in a domain structurally constrains and pressures adoption of the sibling readings in the same domain (they compete for the same institutional space).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
