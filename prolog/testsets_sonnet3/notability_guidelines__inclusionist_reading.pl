% ============================================================================
% CONSTRAINT STORY: notability_guidelines__inclusionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__inclusionist_reading, []).

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
 *   constraint_id: notability_guidelines__inclusionist_reading
 *   human_readable: Wikipedia Notability Guideline (WP:N) as Gatekeeping Apparatus (Inclusionist Reading)
 *   domain: digital commons governance / knowledge infrastructure / platform constitutionalism
 *
 * SUMMARY:
 *   Wikipedia's WP:N guideline requires 'significant coverage in reliable,
 *   independent, secondary sources' for a topic to warrant a standalone
 *   article. Read through an inclusionist lens, this criterion is not a
 *   neutral quality filter: it imports the coverage patterns of a commercial
 *   and institutional press that has historically underserved indigenous,
 *   diasporic, minority-language, and grassroots communities, and then treats
 *   the resulting absence of citable coverage as proof the topic is
 *   non-notable rather than as evidence of unequal media access. The
 *   guideline is enforced daily through Articles for Deletion, where
 *   volunteer editors — disproportionately drawn from, and calibrated to,
 *   Western institutional and English-language sourcing norms — nominate and
 *   remove articles on topics that are real, well-attested within their
 *   communities, and simply under-covered by the kind of press the rule
 *   privileges.
 *
 * KEY AGENTS:
 *   - institutional_knowledge_producers: primary beneficiary (institutional/arbitrage) — their existing output converts automatically into notability
 *   - credentialed_academics: beneficiary and co-agenda-setter (organized/mobile) — their disciplinary sourcing norms become default policy
 *   - afd_deletionist_editors: agenda_setter (organized/mobile) — administer and enforce the sourcing test daily
 *   - indigenous_oral_knowledge_communities: primary victim (powerless/trapped) — knowledge base excluded by source-type requirement
 *   - diaspora_and_minority_language_communities: victim (powerless/constrained) — minority-language coverage systematically discounted
 *   - grassroots_activist_and_local_history_groups: victim (powerless/trapped) — alternative-press coverage treated as insufficiently independent
 *   - non_western_regional_topic_editors: victim/excluded (moderate/constrained) — bear repeated defense labor under a foreign calibration
 *   - wikimedia_foundation: observer/agenda_setter (institutional/analytical) — owns platform, delegates adjudication to community
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, 0.71).
domain_priors:suppression_score(notability_guidelines__inclusionist_reading, 0.68).
domain_priors:theater_ratio(notability_guidelines__inclusionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__inclusionist_reading, snare).
narrative_ontology:human_readable(notability_guidelines__inclusionist_reading, "Wikipedia Notability Guideline (WP:N) as Gatekeeping Apparatus (Inclusionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__inclusionist_reading, "digital commons governance / knowledge infrastructure / platform constitutionalism").

domain_priors:requires_active_enforcement(notability_guidelines__inclusionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__inclusionist_reading, '54f214ed-c544-4332-bee0-927bf2e54489').
narrative_ontology:cs_kernel_codification('54f214ed-c544-4332-bee0-927bf2e54489', formalized).
narrative_ontology:cs_authority_grounding('54f214ed-c544-4332-bee0-927bf2e54489', practice).
narrative_ontology:cs_interpretation_layer_present('54f214ed-c544-4332-bee0-927bf2e54489').
narrative_ontology:cs_reading_relation('54f214ed-c544-4332-bee0-927bf2e54489', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('54f214ed-c544-4332-bee0-927bf2e54489', notability_guidelines__deliberative_reading, influences).
narrative_ontology:cs_axiom('54f214ed-c544-4332-bee0-927bf2e54489', foundational, source_type_gatekeeping_tracks_structural_power_not_epistemic_merit).
narrative_ontology:cs_axiom_status(source_type_gatekeeping_tracks_structural_power_not_epistemic_merit, holdable).
narrative_ontology:cs_axiom_grounding('54f214ed-c544-4332-bee0-927bf2e54489', source_type_gatekeeping_tracks_structural_power_not_epistemic_merit, empirically_contingent).
narrative_ontology:cs_axiom('54f214ed-c544-4332-bee0-927bf2e54489', foundational, absence_of_press_coverage_is_evidence_of_unequal_access_not_non_notability).
narrative_ontology:cs_axiom_status(absence_of_press_coverage_is_evidence_of_unequal_access_not_non_notability, holdable).
narrative_ontology:cs_axiom_grounding('54f214ed-c544-4332-bee0-927bf2e54489', absence_of_press_coverage_is_evidence_of_unequal_access_not_non_notability, empirically_contingent).
narrative_ontology:cs_reference_frame('54f214ed-c544-4332-bee0-927bf2e54489', verifiability_against_fabrication_standard).
narrative_ontology:cs_drift_state('54f214ed-c544-4332-bee0-927bf2e54489', contemporary_systemic_bias_documentation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('54f214ed-c544-4332-bee0-927bf2e54489', '').
narrative_ontology:cs_kernel_id(notability_guidelines__inclusionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, legacy_media_publishers).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, credentialed_academics).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, indigenous_oral_knowledge_communities).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, diaspora_and_minority_language_communities).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, grassroots_activist_and_local_history_groups).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, non_western_regional_topic_editors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Newspapers, university presses, and established broadcasters whose coverage decisions are treated by the guideline as the default proof of 'significance.' Their existing archives and editorial output convert almost automatically into Wikipedia notability, entrenching their gatekeeping role in a second venue without any additional effort on their part.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, institutional_knowledge_producers, beneficiary,
    institutional, generational, arbitrage, global).

% Disproportionately represented among long-tenured editors who write and enforce sourcing policy. Their disciplinary norms (peer review, print citation, institutional affiliation) get encoded directly into what counts as a 'reliable source,' giving their epistemic standards default authority over the encyclopedia's inclusion boundary.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, credentialed_academics, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__inclusionist_reading, credentialed_academics, agenda_setter).

% Patrol Articles for Deletion and new-page review, applying WP:N as a bright-line test. They administer the guideline day to day, nominating and voting on borderline articles; they can and do change enforcement intensity but bear none of the cost when a marginalized topic is removed.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, afd_deletionist_editors, agenda_setter,
    organized, biographical, mobile, global).

% Hold generations of documented practice, place-names, and history transmitted orally or through community-controlled archives that never passed through a commercial press. Their entries are routinely deleted or merged away for lacking 'independent, reliable, secondary sources,' even when the underlying knowledge is well-attested within the community. They have no equivalent publishing industry to generate the citations the rule demands.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, indigenous_oral_knowledge_communities, payer,
    powerless, civilizational, trapped, regional).

% Document community institutions, local figures, and minority-language cultural production, but sourcing is concentrated in community newspapers or minority-language outlets that editors unfamiliar with the language or context frequently discount as 'not independent' or 'not reliable.' Articles are deleted at higher rates than comparable majority-language topics.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, diaspora_and_minority_language_communities, payer,
    powerless, generational, constrained, regional).

% Maintain local histories, mutual-aid organizations, and social movements that receive coverage in alternative or community press rather than legacy outlets. Under the guideline this coverage is frequently treated as insufficiently 'significant' or insufficiently 'independent,' and their pages are nominated for deletion faster than comparably obscure but institutionally-covered topics.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, grassroots_activist_and_local_history_groups, payer,
    powerless, biographical, trapped, local).

% Volunteer editors who write about non-Western topics and struggle to defend articles at AfD because the reviewing pool is disproportionately drawn from, and calibrated to, English-language and Western institutional sourcing norms. They bear the labor cost of repeatedly re-litigating notability for topics majority editors accept without question.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, non_western_regional_topic_editors, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__inclusionist_reading, non_western_regional_topic_editors, excluded).

% Owns the platform and funds diversity/equity initiatives while declining to override community-governed content policy, including WP:N. Can adjust the technical and governance substrate the guideline operates within but has structurally delegated notability adjudication to the volunteer editor community.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikimedia_foundation, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__inclusionist_reading, wikimedia_foundation, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:fixing_cost_class(notability_guidelines__inclusionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, citable threshold so editors are not forced to litigate the encyclopedic worth of every topic from first principles — a genuine coordination problem (avoiding endless case-by-case argument) that the guideline nominally solves.
% TRANSFER_FUNCTION: Moves representational visibility and epistemic legitimacy from topics and communities without commercial/institutional press coverage to topics and communities with it — converting pre-existing publishing-industry advantage into encyclopedia-inclusion advantage.
% ABSENT_VOICES: The communities whose knowledge is deleted are rarely present at the AfD discussion that removes it — they are not Wikipedia editors, do not know the deletion occurred, and have no standing in the deliberation that decides their notability. Their objection, when it surfaces at all, arrives after the fact via secondary reporting on 'systemic bias,' not as a party to the discussion.
% DISAPPEARANCE_RATIONALE: If WP:N vanished overnight, the volume of retained articles on marginalized topics, oral-history subjects, and community institutions would rise substantially, AfD as an institution would lose its primary citable rationale, and the editor hierarchy built around sourcing expertise would lose a large share of its gatekeeping function — the encyclopedia's content boundary and its internal power structure would both visibly shift.
% FOUNDING_PROBLEM: Early Wikipedia had no defense against unverifiable claims, vanity articles, and promotional content; WP:N was built to require independent verification so the encyclopedia would not become an undifferentiated directory of self-published claims.
% FOUNDING_PROBLEM_CORROBORATION: AfD-active editors and WMF equity researchers who are not themselves gatekeeping beneficiaries attest that verification against fabrication remains a live problem, but independent academic studies of Wikipedia's systemic bias (documented in peer-reviewed HCI and information-science literature, e.g. research on gender and Global South coverage gaps) attest that the guideline's practical effect has shifted from filtering fabrication to filtering underdocumented-but-real topics — a status the deletionist-aligned editor base does not itself report.
narrative_ontology:disappearance_verdict(notability_guidelines__inclusionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__inclusionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__inclusionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(notability_guidelines__inclusionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__inclusionist_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__inclusionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(notability_guidelines__inclusionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71) and rising over the interval because the guideline's practical bite is measured, from this reading, by its differential deletion effect: topics backed by institutional press pass with minimal scrutiny while comparably significant topics backed by community, oral, or minority-language sources face repeated re-litigation and higher deletion rates. Suppression (0.68) reflects that alternatives to the sourcing test (oral-history citation standards, community-archive verification, local-press equivalence) are available in principle but are actively rejected as 'not reliable' in AfD practice, which functions as the enforcement mechanism holding the exclusion in place. Theater ratio (0.42) is substantial and rising: a meaningful share of guideline invocation increasingly serves to perform rigor and neutrality rather than to prevent actual fabrication, which is the guideline's stated founding purpose. Accessibility collapse (0.58) is moderate rather than near-total because informal workarounds exist (merging into broader articles, off-wiki archives, sister projects) but these are markedly inferior to standalone inclusion. Resistance (0.61) is substantial: WikiProject efforts, GLAM partnerships, and systemic-bias task forces actively contest deletion outcomes, which is itself evidence this is a live, contested extraction rather than settled natural order.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional knowledge producers and credentialed academics sit near the full-beneficiary end of directionality: the guideline converts their pre-existing output into a second form of authority (encyclopedic inclusion) at no marginal cost to them. AfD deletionist editors are agenda-setters whose labor administers the boundary but who do not personally bear the cost of exclusion — they occupy a distinct seat from beneficiary, closer to enforcement than extraction-collection. The four victim groups are trapped or constrained: indigenous oral-knowledge communities and grassroots local-history groups have essentially no path to generate the source-type the guideline demands without first building the commercial/institutional press infrastructure they by definition lack access to, which is why their exit_options are authored as trapped rather than merely constrained. Non-western regional topic editors are differentiated from the other victims by moderate power (they are Wikipedia editors, not outside communities) but still pay a structural cost — the labor tax of relitigating notability under a calibration built around institutions foreign to their subject matter.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unverifiable, fabricated, or promotional content — remains partly live (fabrication risk is real), which is why founding_problem_status is authored 'contested' rather than 'dead.' The mandatrophy concern this reading raises is narrower and sharper: the guideline's ENFORCEMENT SURFACE has drifted from filtering fabrication to filtering under-documentation, while retaining the original justificatory language ('reliable, independent sources prevent bad content') as cover for a distinct and much broader exclusionary effect. Classifying this as snare rather than mountain or rope prevents the mistake of treating a guideline with genuine partial coordination value (some fabrication-prevention function) as either wholly natural/necessary (mountain framing, which the deletionist reading claims) or as a fair, symmetric negotiation (rope/deliberative framing, which the deliberative reading claims) when, from this reading's evidence, its dominant effect is asymmetric exclusion enforced against parties with no seat at the table that decides their exclusion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the deletionist, deliberative, and inclusionist readings of WP:N locate their disagreement — is it about the TEXT of the guideline, its ENFORCEMENT pattern, or the underlying EPISTEMOLOGY of what counts as ''reliable''?',
    'Comparative discourse analysis of AfD closing rationales across topic categories (institutional vs. community-sourced) combined with the deletionist and deliberative readings'' own authored omega variables, to locate whether the three readings share premises about the guideline''s text but diverge on its effect, or diverge on the text''s meaning itself.',
    'If the disagreement is purely about enforcement pattern (not text), this supports treating all three as readings of one stable kernel with divergent χ from shared ε-relevant facts. If the disagreement is about the underlying epistemology of ''reliable source,'' the kernel itself may be under-specified enough that ''notability_guidelines'' should be treated as a distributed rather than formalized kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating the structural disagreement among the three sibling readings of the WP:N kernel.').

omega_variable(
    differential_deletion_rate_causal_attribution,
    'Is the measured differential deletion rate for marginalized-topic articles caused by the guideline''s text (source-type requirement), by the demographic composition of the enforcing editor pool, or by an interaction of both that cannot be cleanly separated?',
    'Controlled comparison of AfD outcomes for matched-notability articles (similar real-world significance, differing source-type composition) reviewed by editor pools with varying demographic and topic-area composition; existing WMF and academic bias-audit datasets could be extended for this purpose.',
    'If the effect is primarily textual (the sourcing requirement itself), fixing it requires rewriting WP:N. If primarily compositional (who enforces it), fixing it requires diversifying the AfD-active editor pool without changing the text — a much cheaper intervention. This materially affects the fixing_cost assessment below.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(differential_deletion_rate_causal_attribution, empirical, 'Whether the exclusionary effect traces to guideline text or to enforcer demographics.').

omega_variable(
    oral_and_community_sourcing_reliability_standard,
    'Could a reliability standard for oral-history and community-archive sourcing be constructed that both prevents fabrication (the founding problem) and admits currently-excluded marginalized knowledge, or is there an irreducible tradeoff between verifiability and inclusion at internet scale?',
    'Pilot programs (already partially attempted via WikiProject Indigenous, GLAM partnerships) establishing alternative verification pathways (community elder attestation protocols, partnership with indigenous-run archives) and measuring both fabrication rate and inclusion rate against the status quo.',
    'If a workable alternative standard exists, this reading''s snare classification strengthens (the exclusion is unforced, not structurally necessary). If no such standard is achievable, part of the measured extraction would need to be re-attributed to genuine coordination cost rather than pure extraction, pushing the classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oral_and_community_sourcing_reliability_standard, empirical, 'Whether a fabrication-resistant, inclusion-compatible sourcing standard is achievable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__inclusionist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__inclusionist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nota_tr_t4, notability_guidelines__inclusionist_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(nota_tr_t8, notability_guidelines__inclusionist_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(nota_tr_t12, notability_guidelines__inclusionist_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(nota_tr_t16, notability_guidelines__inclusionist_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__inclusionist_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__inclusionist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(nota_be_t4, notability_guidelines__inclusionist_reading, base_extractiveness, 4, 0.49).
narrative_ontology:measurement(nota_be_t8, notability_guidelines__inclusionist_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(nota_be_t12, notability_guidelines__inclusionist_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(nota_be_t16, notability_guidelines__inclusionist_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__inclusionist_reading, base_extractiveness, 20, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__inclusionist_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(nota_su_t4, notability_guidelines__inclusionist_reading, suppression_requirement, 4, 0.53).
narrative_ontology:measurement(nota_su_t8, notability_guidelines__inclusionist_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(nota_su_t12, notability_guidelines__inclusionist_reading, suppression_requirement, 12, 0.61).
narrative_ontology:measurement(nota_su_t16, notability_guidelines__inclusionist_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(nota_su_t20, notability_guidelines__inclusionist_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__inclusionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(notability_guidelines__inclusionist_reading, 0.08).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, notability_guidelines__deletionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, notability_guidelines__deliberative_reading).

% DUAL FORMULATION NOTE:
% Three constraints share the kernel notability_guidelines (the WP:N text and its AfD enforcement practice) but instantiate structurally distinct claims: deletionist_reading authors WP:N as a necessary epistemic quality filter (low ε, mountain-adjacent), deliberative_reading authors it as a continuously renegotiated boundary (moderate ε, tangled_rope-adjacent), and this file (inclusionist_reading) authors it as a structural gatekeeping apparatus with a concentrated beneficiary class and identifiable powerless victim classes (high ε, snare). Per the ε-invariance principle, these are three separate stories, not one story measured three ways — each is linked to the other two via affects_constraints, and each documents the kernel relationship in commentary.kernel_context / cs_structure.reading_relations rather than folding the contest into a single averaged ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
