% ============================================================================
% CONSTRAINT STORY: naskh_principle__progressive_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__progressive_restriction, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: naskh_principle__progressive_restriction
 *   human_readable: Naskh Principle: Progressive Restriction Reading
 *   domain: religious/legal/hermeneutical
 *
 * SUMMARY:
 *   The naskh principle in Islamic jurisprudence addresses the interpretive
 *   challenge of apparent contradictions in Quranic law on the same topics.
 *   The progressive restriction reading (this constraint) is one solution:
 *   earlier permissive verses are understood as transitional accommodations
 *   for the emerging Muslim community, later restrictive verses as final
 *   divine intent, and the trajectory as divine pedagogy. This reading has
 *   become institutionally dominant in modern Islamic academic discourse and
 *   official jurisprudential structures, but it competes with classical
 *   abrogation (direct supersession of earlier by later verses) and
 *   contextual harmonization (all verses valid in their contexts, no
 *   supersession). The constraint structure reveals extraction: the reading
 *   systematically advantages evolutionary legal scholars and modernizers
 *   (who control institutional authority) while disadvantaging literalist
 *   permissive interpreters and traditional madhab practitioners (whose
 *   textual citations are reframed as abrogated accommodations). The claim is
 *   tangled_rope because the reading coordinates legal evolution with
 *   scriptural authority while actively enforcing a hermeneutical framework
 *   that suppresses alternatives. Measurement data tracks institutional
 *   consolidation over 70 years: extraction rises from 0.35 to 0.58,
 *   suppression from 0.42 to 0.64, and theater ratio from 0.25 to 0.42,
 *   indicating the reading has become institutionally standard but faces
 *   persistent scholarly resistance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__progressive_restriction, 0.58).
domain_priors:suppression_score(naskh_principle__progressive_restriction, 0.64).
domain_priors:theater_ratio(naskh_principle__progressive_restriction, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, extractiveness, 0.58).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__progressive_restriction, tangled_rope).
narrative_ontology:human_readable(naskh_principle__progressive_restriction, "Naskh Principle: Progressive Restriction Reading").
narrative_ontology:topic_domain(naskh_principle__progressive_restriction, "religious/legal/hermeneutical").

domain_priors:requires_active_enforcement(naskh_principle__progressive_restriction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__progressive_restriction, '894dd34f-1ba2-41eb-9e14-fc635d099ab5').
narrative_ontology:cs_kernel_codification('894dd34f-1ba2-41eb-9e14-fc635d099ab5', formalized).
narrative_ontology:cs_authority_grounding('894dd34f-1ba2-41eb-9e14-fc635d099ab5', extraction).
narrative_ontology:cs_interpretation_layer_present('894dd34f-1ba2-41eb-9e14-fc635d099ab5').
narrative_ontology:cs_reading_relation('894dd34f-1ba2-41eb-9e14-fc635d099ab5', naskh_principle__classical_abrogation_reading, coexists_with).
narrative_ontology:cs_reading_relation('894dd34f-1ba2-41eb-9e14-fc635d099ab5', naskh_principle__contextual_harmonization_reading, influences).
narrative_ontology:cs_axiom('894dd34f-1ba2-41eb-9e14-fc635d099ab5', foundational, divine_pedagogy_manifests_as_restriction).
narrative_ontology:cs_axiom_status(divine_pedagogy_manifests_as_restriction, holdable).
narrative_ontology:cs_axiom_grounding('894dd34f-1ba2-41eb-9e14-fc635d099ab5', divine_pedagogy_manifests_as_restriction, empirically_contingent).
narrative_ontology:cs_axiom('894dd34f-1ba2-41eb-9e14-fc635d099ab5', foundational, earlier_permissive_verses_are_transitional_accommodations).
narrative_ontology:cs_axiom_status(earlier_permissive_verses_are_transitional_accommodations, holdable).
narrative_ontology:cs_axiom_grounding('894dd34f-1ba2-41eb-9e14-fc635d099ab5', earlier_permissive_verses_are_transitional_accommodations, deontological).
narrative_ontology:cs_reference_frame('894dd34f-1ba2-41eb-9e14-fc635d099ab5', divine_pedagogical_progression).
narrative_ontology:cs_drift_state('894dd34f-1ba2-41eb-9e14-fc635d099ab5', contemporary_institutional_standardization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('894dd34f-1ba2-41eb-9e14-fc635d099ab5', '').
narrative_ontology:cs_kernel_id(naskh_principle__progressive_restriction, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, evolutionary_legal_scholars).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, jurisprudential_modernizers).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, literalist_permissive_interpreters).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, traditional_madhab_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, classical_abrogation_advocates).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, contextual_harmonization_advocates).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, islamic_legal_modernizers).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, classical_abrogation_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academics and reformist jurists who construct and defend the progressive restriction reading in modern Islamic studies, university curricula, and official jurisprudential institutions. They set the hermeneutical framework that is taught, published, and recognized as authoritative. They benefit because the reading validates legal modernization while preserving scriptural authority, and their institutional role depends on maintaining interpretive authority.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, evolutionary_legal_scholars, agenda_setter,
    institutional, generational, constrained, global).

% Traditional scholars, community jurists, and practitioners who cite earlier permissive Quranic verses (on temporary marriage, alcohol, usury, fighting rules) as continuing valid law. The progressive restriction reading treats their textual citations as misreadings of abrogated accommodations, delegitimizing their jurisprudential authority. Their professional identity and scholarly standing are constituted through maintaining the validity of their permissive readings; renouncing them is existential loss, not mere methodological adjustment.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, literalist_permissive_interpreters, payer,
    moderate, generational, identity_locked, regional).

% Practitioners of classical Islamic legal schools (Hanafi, Maliki, Shafi'i, Hanbali) whose jurisprudence sometimes relies on permissive readings of earlier verses. The progressive restriction reading forces them to either reframe their jurisprudence as grounded in restriction-phase verses or defend their permissive positions against the charge of invoking abrogated law. Abandoning madhab tradition requires departing centuries of established methodology, institutional networks, and jurisprudential inheritance.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, traditional_madhab_practitioners, payer,
    organized, generational, constrained, global).

% Scholars committed to the classical abrogation doctrine (naskh as direct supersession of void verses by later verses). They benefit from a clear chronological framework but find themselves recharacterized by the progressive restriction reading as theologically crude (treating abrogated verses as void rather than as divine pedagogy). Both readings use the same textual corpus and chronological framework but derive incompatible conclusions about revelation's structure. They maintain institutional authority in some communities but face pressure from the modernist reading's entrenchment in academic and official structures.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, classical_abrogation_advocates, beneficiary,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__progressive_restriction, classical_abrogation_advocates, payer).

% Scholars who resolve apparent contradictions in Quranic law through contextual specification and situational reading rather than chronological supersession. The progressive restriction reading competes with their approach by offering an alternative non-abrogation framework that is simpler and gaining institutional traction. They can exit to other hermeneutical frameworks without identity loss, but the progressive restriction reading encroaches on their interpretive space and authority.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, contextual_harmonization_advocates, beneficiary,
    moderate, generational, mobile, global).

% Contemporary Islamic reformers and legal theorists seeking to adapt Islamic law to modern contexts (women's rights, democratic participation, economic innovation). The progressive restriction reading benefits them by permitting legal change while maintaining that they follow divine pedagogy, not human innovation. They can justify contemporary legal positions by citing later restrictive verses as final divine intent while presenting earlier permissive verses as historical accommodations. They maintain alternative justificatory frameworks (independent reasoning, contextual reinterpretation) but strategically benefit from this reading's institutional legitimacy.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, islamic_legal_modernizers, beneficiary,
    powerful, generational, arbitrage, global).

% Scholars who resist all chronological frameworks applied to the Quran and insist every verse must be read within its revelatory context without external ordering imposed. They are excluded from academic and official institutional discourse on naskh, which presumes chronological revelation order as foundational. Their fundamental challenge to the entire chronological apparatus would undermine both classical abrogation and progressive restriction readings. Institutional barriers prevent their perspectives from being heard as legitimate alternatives.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, quranic_textualists, excluded,
    moderate, generational, trapped, regional).

% Academic observers from outside Islamic jurisprudence studying how different readings of the same revelation emerge from different axioms about divine structure, historical progression, and scriptural authority. They document how institutional authority structures enforce particular readings and exclude others, and how the hermeneutical stakes differ from seat to seat.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, comparative_religious_scholars, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__progressive_restriction, evolutionary_legal_scholars).
narrative_ontology:fixing_cost_class(naskh_principle__progressive_restriction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for interpreting Quranic law that preserves scriptural unity without treating the text as containing errors or void verses. Solves the exegetical coordination problem: how to reconcile apparent contradictions without fragmenting the legal corpus into conflicting domains.
% TRANSFER_FUNCTION: Transfers hermeneutical authority from those who cite permissive earlier verses to those who construct the restrictive-as-final framework. Also transfers legitimacy from traditional madhab-specific jurisprudence to centralized modern institutional standards, and from community-level interpretation to academic institutional authority structures.
% ABSENT_VOICES: Quranic textualists who reject chronological frameworks entirely are excluded; their objection that the entire chronological apparatus is externally imposed would require rethinking the foundational premise of the naskh principle itself. Community practitioners whose permissive interpretations depend on earlier verses are pushed to margins as traditional rather than heard as legitimate contemporary interlocutors. Academic scholars from non-modernist schools are underrepresented in institutional discourse on naskh.
% DISAPPEARANCE_RATIONALE: If the progressive restriction reading were abandoned, Islamic jurisprudence would reorganize around classical abrogation (direct chronological supersession with void text), contextual harmonization (situational validity without supersession), or textualist frameworks (rejection of all chronological ordering). Legal outcomes would shift: permissive earlier verses would regain interpretive standing, traditional madhab jurisprudence would strengthen, and modernist legal evolution would require different justificatory structures (explicit reasoning, contextual reinterpretation, community consultation rather than appeal to divine pedagogy).
% FOUNDING_PROBLEM: Classical Islamic jurisprudence faced the exegetical challenge of apparent contradictions in Quranic law on the same legal topics (e.g., temporary marriage, alcohol, usury, inheritance rules, fighting permissions). The naskh principle was developed to solve this: later verses supersede earlier ones on the same topic. The progressive restriction reading reframed naskh as not abrogation but pedagogical progression, preserving both the chronological framework AND the unity of divine instruction.
% FOUNDING_PROBLEM_CORROBORATION: Classical Islamic jurisprudents (al-Shafi'i, al-Suyuti, Ibn al-Qayyim) and medieval exegetes attest the exegetical problem and propose naskh solutions. Modern comparative law scholars, Islamic studies academics, and historians outside the benefiting parties document the hermeneutical stakes: whether the Quran contains void verses, whether divine pedagogy manifests as progression, and whether contemporary legal adaptation requires textual reinterpretation or independent reasoning. Textualist scholars and traditionalist madhab practitioners attest the foundational problem is overstated and dissolves under proper contextual reading without chronology; they provide counter-testimony that the apparent contradictions are not genuine because the verses address different situations or audiences.
narrative_ontology:disappearance_verdict(naskh_principle__progressive_restriction, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__progressive_restriction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__progressive_restriction, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(naskh_principle__progressive_restriction, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__progressive_restriction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__progressive_restriction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__progressive_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pedagogical_necessity_vs_power,
    'Is the progressive restriction reading grounded in genuine scriptural structure (divine pedagogy as a feature of revelation), or is it a rationalization for institutional power consolidation (modernist scholars enforcing a framework that validates their authority)?',
    'Historical analysis of pre-institutional developments: did the reading emerge from community exegetical practice before academic institutionalization, or did it originate in academic modernist discourse and then diffuse outward? Comparative study of how the reading appears in different institutional contexts (official bodies vs. grassroots communities) and whether outcomes differ.',
    'If grounded in genuine scriptural structure, the reading is a valid hermeneutical framework and its extraction component is the price of coordination. If grounded primarily in power consolidation, the reading is a snare using pedagogical language as cover, and the suppression metrics are the true measure of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_necessity_vs_power, empirical, 'Whether the progressive restriction reading reflects divine structure or institutional authority consolidation.').

omega_variable(
    axiom_overriding_drift,
    'The foundational axiom underlying this reading is that divine pedagogy manifests as progressive restriction. Is this axiom still holdable in contemporary hermeneutics, or has it been substantially overridden by empirical challenge (counter-examples of permissive-to-permissive or restrictive-to-permissive sequences in revelation)?',
    'Systematic analysis of Quranic topic-sequences: do all cases of apparent contradiction show permissive-to-restrictive ordering, or are there documented cases where the ordering is reversed or where both verses are equally restrictive? If counter-examples exist, does the reading''s authority structure acknowledge them or suppress them?',
    'If counter-examples are substantial and suppressed, the axiom has been overridden but institutional enforcement maintains it anyway — signaling the reading has shifted from genuine framework to theatrical performance. If counter-examples are marginal or the reading''s authority acknowledges them, the axiom remains holdable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_overriding_drift, empirical, 'Whether the foundational pedagogical axiom is empirically sustainable or has been overridden by evidence.').

omega_variable(
    identity_locked_suppression,
    'For literalist permissive interpreters (exit_options: identity_locked), is the suppression of their readings structural (institutional barriers to publication and official recognition) or internalized (they have internalized the delegitimizing frame and no longer defend their positions even when barriers are absent)?',
    'Post-institutional relaxation natural experiment: if official institutions stopped enforcing the progressive restriction reading, would permissive literalist interpretations immediately resurface in community discourse, or would they remain suppressed because practitioners have internalized the narrative that earlier verses are abrogated accommodations? Comparative study across different Islamic communities with varying institutional enforcement.',
    'If suppression is primarily structural, the constraint''s effective suppression is the authored 0.64 metric. If suppression is substantially internalized, the constraint''s effective suppression is higher because the target carries the suppression with them after institutional barriers are removed, and the constraint persists through internalized narrative rather than external force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_suppression, empirical, 'Structural vs. internalized suppression mechanism in identity-locked exit contexts.').

omega_variable(
    contextual_harmonization_coexistence,
    'Can the progressive restriction reading and contextual harmonization reading coexist within a single coherent hermeneutical framework, or do they foreclose each other? Does the progressive restriction reading merely compete with contextual harmonization, or does it logically require rejecting the axioms contextual harmonization depends on?',
    'Logical analysis of foundational commitments: does progressive_restriction require that Quranic verses be ordered chronologically, and does contextual_harmonization require that chronological ordering be absent or irrelevant? Can a scholar adopt both frameworks for different textual domains, or is the choice binary?',
    'If the readings foreclose each other, the reading_relation should be forecloses, not coexists_with. If they coexist with different foundational axioms but compatible frameworks, the coexists_with relation is correct. This affects the kernel-level classification of whether the naskh principle itself is contested or genuinely decomposed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contextual_harmonization_coexistence, conceptual, 'Whether progressive_restriction and contextual_harmonization readings are logically compatible or mutually exclusive.').

omega_variable(
    traditional_madhab_reframing,
    'Can traditional Islamic legal schools (madhabs) be reframed to ground their jurisprudence in restriction-phase verses rather than permissive earlier verses, or does such reframing dissolve the coherence of established madhab methodology?',
    'Jurisprudential study attempting to reframe classical madhab positions (e.g., Hanafi jurisprudence on temporary marriage, Maliki jurisprudence on usury exceptions) on the basis of later restrictive verses alone, preserving all outcomes but changing textual grounding. If reframing succeeds, the madhab practitioners'' costs are reduced (they retain methodology, only textual basis shifts); if reframing fails, the constraint forces them to abandon either positions or methodology.',
    'If reframing succeeds, the constraint''s victimization of madhab practitioners is less severe — they can survive by textual re-grounding. If reframing fails and they must choose between positions and method, the effective extraction increases because they face genuine dilemma, not merely shifted authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditional_madhab_reframing, empirical, 'Whether traditional madhab jurisprudence can survive under the progressive restriction reading by re-grounding in later verses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__progressive_restriction, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__progressive_restriction, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(nask_tr_t0, projected).
narrative_ontology:measurement(nask_tr_t10, naskh_principle__progressive_restriction, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(nask_tr_t10, observed).
narrative_ontology:measurement(nask_tr_t20, naskh_principle__progressive_restriction, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(nask_tr_t20, observed).
narrative_ontology:measurement(nask_tr_t30, naskh_principle__progressive_restriction, theater_ratio, 30, 0.39).
narrative_ontology:measurement_basis(nask_tr_t30, observed).
narrative_ontology:measurement(nask_tr_t40, naskh_principle__progressive_restriction, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(nask_tr_t40, observed).
narrative_ontology:measurement(nask_tr_t50, naskh_principle__progressive_restriction, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(nask_tr_t50, observed).
narrative_ontology:measurement(nask_tr_t60, naskh_principle__progressive_restriction, theater_ratio, 60, 0.42).
narrative_ontology:measurement_basis(nask_tr_t60, observed).
narrative_ontology:measurement(nask_tr_t70, naskh_principle__progressive_restriction, theater_ratio, 70, 0.42).
narrative_ontology:measurement_basis(nask_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__progressive_restriction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(nask_be_t0, projected).
narrative_ontology:measurement(nask_be_t10, naskh_principle__progressive_restriction, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(nask_be_t10, observed).
narrative_ontology:measurement(nask_be_t20, naskh_principle__progressive_restriction, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(nask_be_t20, observed).
narrative_ontology:measurement(nask_be_t30, naskh_principle__progressive_restriction, base_extractiveness, 30, 0.54).
narrative_ontology:measurement_basis(nask_be_t30, observed).
narrative_ontology:measurement(nask_be_t40, naskh_principle__progressive_restriction, base_extractiveness, 40, 0.57).
narrative_ontology:measurement_basis(nask_be_t40, observed).
narrative_ontology:measurement(nask_be_t50, naskh_principle__progressive_restriction, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(nask_be_t50, observed).
narrative_ontology:measurement(nask_be_t60, naskh_principle__progressive_restriction, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(nask_be_t60, observed).
narrative_ontology:measurement(nask_be_t70, naskh_principle__progressive_restriction, base_extractiveness, 70, 0.58).
narrative_ontology:measurement_basis(nask_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__progressive_restriction, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(nask_su_t0, projected).
narrative_ontology:measurement(nask_su_t10, naskh_principle__progressive_restriction, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(nask_su_t10, observed).
narrative_ontology:measurement(nask_su_t20, naskh_principle__progressive_restriction, suppression_requirement, 20, 0.54).
narrative_ontology:measurement_basis(nask_su_t20, observed).
narrative_ontology:measurement(nask_su_t30, naskh_principle__progressive_restriction, suppression_requirement, 30, 0.6).
narrative_ontology:measurement_basis(nask_su_t30, observed).
narrative_ontology:measurement(nask_su_t40, naskh_principle__progressive_restriction, suppression_requirement, 40, 0.63).
narrative_ontology:measurement_basis(nask_su_t40, observed).
narrative_ontology:measurement(nask_su_t50, naskh_principle__progressive_restriction, suppression_requirement, 50, 0.64).
narrative_ontology:measurement_basis(nask_su_t50, observed).
narrative_ontology:measurement(nask_su_t60, naskh_principle__progressive_restriction, suppression_requirement, 60, 0.64).
narrative_ontology:measurement_basis(nask_su_t60, observed).
narrative_ontology:measurement(nask_su_t70, naskh_principle__progressive_restriction, suppression_requirement, 70, 0.64).
narrative_ontology:measurement_basis(nask_su_t70, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=70
narrative_ontology:measurement(nask_grid_01, naskh_principle__progressive_restriction, accessibility_collapse(class), 0, 0.42).
narrative_ontology:measurement(nask_grid_02, naskh_principle__progressive_restriction, accessibility_collapse(class), 70, 0.62).
narrative_ontology:measurement(nask_grid_03, naskh_principle__progressive_restriction, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(nask_grid_04, naskh_principle__progressive_restriction, accessibility_collapse(individual), 70, 0.58).
narrative_ontology:measurement(nask_grid_05, naskh_principle__progressive_restriction, accessibility_collapse(organizational), 0, 0.64).
narrative_ontology:measurement(nask_grid_06, naskh_principle__progressive_restriction, accessibility_collapse(organizational), 70, 0.76).
narrative_ontology:measurement(nask_grid_07, naskh_principle__progressive_restriction, accessibility_collapse(structural), 0, 0.58).
narrative_ontology:measurement(nask_grid_08, naskh_principle__progressive_restriction, accessibility_collapse(structural), 70, 0.71).
narrative_ontology:measurement(nask_grid_09, naskh_principle__progressive_restriction, resistance(class), 0, 0.48).
narrative_ontology:measurement(nask_grid_10, naskh_principle__progressive_restriction, resistance(class), 70, 0.4).
narrative_ontology:measurement(nask_grid_11, naskh_principle__progressive_restriction, resistance(individual), 0, 0.42).
narrative_ontology:measurement(nask_grid_12, naskh_principle__progressive_restriction, resistance(individual), 70, 0.34).
narrative_ontology:measurement(nask_grid_13, naskh_principle__progressive_restriction, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(nask_grid_14, naskh_principle__progressive_restriction, resistance(organizational), 70, 0.52).
narrative_ontology:measurement(nask_grid_15, naskh_principle__progressive_restriction, resistance(structural), 0, 0.58).
narrative_ontology:measurement(nask_grid_16, naskh_principle__progressive_restriction, resistance(structural), 70, 0.48).
narrative_ontology:measurement(nask_grid_17, naskh_principle__progressive_restriction, stakes_inflation(class), 0, 0.38).
narrative_ontology:measurement(nask_grid_18, naskh_principle__progressive_restriction, stakes_inflation(class), 70, 0.58).
narrative_ontology:measurement(nask_grid_19, naskh_principle__progressive_restriction, stakes_inflation(individual), 0, 0.32).
narrative_ontology:measurement(nask_grid_20, naskh_principle__progressive_restriction, stakes_inflation(individual), 70, 0.52).
narrative_ontology:measurement(nask_grid_21, naskh_principle__progressive_restriction, stakes_inflation(organizational), 0, 0.58).
narrative_ontology:measurement(nask_grid_22, naskh_principle__progressive_restriction, stakes_inflation(organizational), 70, 0.72).
narrative_ontology:measurement(nask_grid_23, naskh_principle__progressive_restriction, stakes_inflation(structural), 0, 0.52).
narrative_ontology:measurement(nask_grid_24, naskh_principle__progressive_restriction, stakes_inflation(structural), 70, 0.68).
narrative_ontology:measurement(nask_grid_25, naskh_principle__progressive_restriction, suppression(class), 0, 0.4).
narrative_ontology:measurement(nask_grid_26, naskh_principle__progressive_restriction, suppression(class), 70, 0.62).
narrative_ontology:measurement(nask_grid_27, naskh_principle__progressive_restriction, suppression(individual), 0, 0.32).
narrative_ontology:measurement(nask_grid_28, naskh_principle__progressive_restriction, suppression(individual), 70, 0.58).
narrative_ontology:measurement(nask_grid_29, naskh_principle__progressive_restriction, suppression(organizational), 0, 0.5).
narrative_ontology:measurement(nask_grid_30, naskh_principle__progressive_restriction, suppression(organizational), 70, 0.7).
narrative_ontology:measurement(nask_grid_31, naskh_principle__progressive_restriction, suppression(structural), 0, 0.46).
narrative_ontology:measurement(nask_grid_32, naskh_principle__progressive_restriction, suppression(structural), 70, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__progressive_restriction, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(naskh_principle__progressive_restriction, 0.12).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, classical_abrogation_reading).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, contextual_harmonization_reading).

% DUAL FORMULATION NOTE:
% The naskh principle is a kernel with three structurally distinct readings: progressive_restriction (this constraint, understanding permissive-to-restrictive movement as divine pedagogy), classical_abrogation (later verses directly supersede earlier), and contextual_harmonization (all verses valid contextually, no supersession). The three readings share the same textual corpus and the exegetical problem (apparent contradictions) but derive incompatible conclusions. Progressive_restriction influences the other readings by offering an alternative non-abrogation framework; classical_abrogation and contextual_harmonization coexist as competing live positions. Separation into three constraint files follows ε-invariance: each reading has a distinct ε (extraction profile), distinct stakeholders, and distinct institutional embedding. Links via network.affects_constraints preserve the kernel relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(naskh_principle__progressive_restriction, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
