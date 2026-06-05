% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__maliki_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: jurisprudential_method_kernel__maliki_reading
 *   human_readable: Medinan Practice (Urf of Medina) as Independent Jurisprudential Authority
 *   domain: islamic_jurisprudence/legal_theory/institutional_pluralism
 *
 * SUMMARY:
 *   The Maliki school's elevation of Medinan practice (Urf of Medina) as an
 *   independent jurisprudential authority alongside the Quran and Hadith
 *   represents one decisive reading of how Islamic legal sources should be
 *   hierarchized and applied. This constraint instantiates the geographic
 *   privilege granted to the Prophet's city — not merely as the locus of
 *   reliable transmission, but as the location where the Prophet's actual
 *   practices were embedded in community norms and could be observed across
 *   generations. The Maliki reading treats these practices as carrying
 *   independent authoritative weight: a ruling that accords with Quran or
 *   Hadith but contradicts established Medinan practice creates a tension
 *   that must be resolved, often by reinterpreting the text or limiting its
 *   scope. This stands in contrast to the Hanafi school's emphasis on
 *   juristic reasoning and analogical extension (qiyas, istihsan, ra'y), the
 *   Shafi'i school's strict hierarchy (Quran > Hadith > Consensus > Qiyas
 *   with limited role for local practice), and the Hanbali school's
 *   literalist textualism that minimizes juristic discretion entirely. The
 *   constraint exhibits tangled-rope structure: it provides a genuine
 *   coordination mechanism (Medinan practice as a shared reference point for
 *   legal stability across the Islamic world) while simultaneously extracting
 *   authority from non-Medinan communities and restricting their
 *   jurisprudential innovation space. The extractiveness value (0.38)
 *   reflects moderate asymmetric extraction: non-Medinan scholars must
 *   justify deviations, but the constraint does not wholly eliminate their
 *   agency. Over time, theater ratio increases as later jurisprudential
 *   schools codify their own methodologies and cite Medinan practice more
 *   performatively — the constraint shifts from functionally determinative
 *   (early Umayyad period) to increasingly theatrical (Abbasid codification
 *   period).
 *
 * KEY AGENTS:
 *   - Medinan Scholarly Authority (institutional/arbitrage): Primary beneficiary. Custodians of the Prophet's city's practices. Institutional prestige and authority flow toward Medinan scholars. Can selectively canonize which practices count as Urf.
 *   - Non-Medinan Communities (powerless/trapped): Primary victim. Cannot exit the Medinan-authority framework without legal delegitimacy. Local practices must be reconciled with or justified against Medinan norms. Face suppression of direct jurisprudential innovation.
 *   - Emerging Jurisprudential Schools (organized/constrained): Secondary victim-beneficiary hybrid. Benefit from coordination function (shared legal vocabulary, Medinan precedent corpus). Constrained by suppression of unoperated-on innovation without textual justification. Must develop methodologies (qiyas, istihsan, ra'y) to legitimate their deviations.
 *   - Regional Communities (moderate/constrained): Secondary victim. Face extraction through legitimacy cost of dissent. Can develop local jurisprudence but must maintain coordination with Medinan framework.
 *   - Later Institutional Judiciary (institutional/arbitrage): Observers of piton dynamics. By Abbasid period, Medinan practice citation becomes increasingly theatrical — legitimacy tool rather than binding constraint.
 *   - Analytical Observer (analytical/analytical): Sees Medinan authority as potentially naturalized constraint — risks treating geographic privilege as inherent rather than constructed.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, 0.38).
domain_priors:suppression_score(jurisprudential_method_kernel__maliki_reading, 0.48).
domain_priors:theater_ratio(jurisprudential_method_kernel__maliki_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__maliki_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__maliki_reading, "Medinan Practice (Urf of Medina) as Independent Jurisprudential Authority").
narrative_ontology:topic_domain(jurisprudential_method_kernel__maliki_reading, "islamic_jurisprudence/legal_theory/institutional_pluralism").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__maliki_reading, 'd1cf56d3-4ab3-4527-913e-8591ce206640').
narrative_ontology:cs_kernel_codification('d1cf56d3-4ab3-4527-913e-8591ce206640', fixed_text).
narrative_ontology:cs_authority_grounding('d1cf56d3-4ab3-4527-913e-8591ce206640', lineage).
narrative_ontology:cs_interpretation_layer_present('d1cf56d3-4ab3-4527-913e-8591ce206640').
narrative_ontology:cs_reading_relation('d1cf56d3-4ab3-4527-913e-8591ce206640', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('d1cf56d3-4ab3-4527-913e-8591ce206640', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('d1cf56d3-4ab3-4527-913e-8591ce206640', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('d1cf56d3-4ab3-4527-913e-8591ce206640', foundational, medinan_practice_binding_authority).
narrative_ontology:cs_axiom_status(medinan_practice_binding_authority, holdable).
narrative_ontology:cs_axiom_grounding('d1cf56d3-4ab3-4527-913e-8591ce206640', medinan_practice_binding_authority, empirically_contingent).
narrative_ontology:cs_axiom('d1cf56d3-4ab3-4527-913e-8591ce206640', foundational, geographic_privilege_medinan_transmission).
narrative_ontology:cs_axiom_status(geographic_privilege_medinan_transmission, holdable).
narrative_ontology:cs_axiom_grounding('d1cf56d3-4ab3-4527-913e-8591ce206640', geographic_privilege_medinan_transmission, conventional).
narrative_ontology:cs_reference_frame('d1cf56d3-4ab3-4527-913e-8591ce206640', medinan_authority_framework).
narrative_ontology:cs_drift_state('d1cf56d3-4ab3-4527-913e-8591ce206640', abbasid_codification_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d1cf56d3-4ab3-4527-913e-8591ce206640', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_scholars_institutional).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, established_legal_precedent).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, non_medinan_communities).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, emerging_jurisprudential_schools).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-MEDINAN COMMUNITIES (SNARE) — Cannot exit the Medinan-authority framework without legal delegitimacy. Local communities in Egypt, Iraq, Syria face extraction through forced deference to Medinan norms that may not reflect their own established practices. Suppression mechanism: questioning Medinan practice is treated as deviation from the Prophet's city. Maximum experienced extraction — no institutional power, no exit options, constrained mobility.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__maliki_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL JURISPRUDENTIAL COMMUNITIES (TANGLED ROPE) — Benefit from access to the Medinan precedent corpus and methodological clarity, but constrained by having to justify deviations through expensive reconciliation work. Can develop local jurisprudence but must maintain coordination with Medinan framework. Moderate extraction via legitimacy cost of dissent; genuine coordination function via shared legal vocabulary.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__maliki_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MEDINAN SCHOLARLY AUTHORITY (ROPE) — Benefits from institutional position as custodians of the Prophet's city's practices. Experiences the constraint as pure coordination: transmission of Medinan norms solves the problem of legal pluralism across the expanding Islamic empire. Net beneficiary — extraction runs toward this agent through deference and citation. Arbitrage exit (can choose which practices to canonize).
constraint_indexing:constraint_classification(jurisprudential_method_kernel__maliki_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: EMERGING JURISPRUDENTIAL SCHOOLS (TANGLED ROPE) — Organized agents (Hanafi, Shafi'i, Hanbali schools emerging in 8th-9th centuries) face both coordination benefits and extraction. Can develop their own methodologies but must legitimize deviations from Medinan practice through principled jurisprudential argumentation (qiyas, istihsan, ra'y). The constraint structures their innovation space: they must show respect to Medinan precedent while carving out methodological independence. Significant suppression of direct innovation without textual justification.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__maliki_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LATER INSTITUTIONAL JUDICIARY (PITON) — By the Abbasid period and after, citation of Medinan practice becomes increasingly theatrical: scholars cite Urf of Medina to legitimize decisions, but the actual decision-making logic derives from codified school methodologies. The Medinan practice invocation is performative — it lends authority without constraining reasoning. Theater ratio increases as the constraint becomes less functionally determinative and more legitimacy-theater.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__maliki_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, Medinan practice appears as an inherent property of Islamic legal authority: the Prophet lived in Medina, his practices are preserved there, therefore Medinan transmission is naturally the most authoritative. This perspective sees the constraint as emerging naturally from the Prophet's biography and the reliability of Medinan transmission. However, this risks naturalizing what is actually a constructed institutional choice — the engine will identify this as a false summit.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__maliki_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__maliki_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jurisprudential_method_kernel__maliki_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jurisprudential_method_kernel__maliki_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(jurisprudential_method_kernel__maliki_reading, TR),
    TR >= 0.70.

:- end_tests(jurisprudential_method_kernel__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Maliki reading creates asymmetric extraction through legitimacy cost rather than formal prohibition. Non-Medinan scholars must perform reconciliation work (qiyas, istihsan, ra'y) to justify deviations, or accept subordination to Medinan precedent. However, the extraction is not maximal because the schools do develop independent methodologies and carve out jurisprudential space. The trajectory shows increase from 0.22 (Umayyad period, when Medinan practice was one input among many) to 0.38 (Abbasid period, when the Maliki school institutionalizes Medinan authority as a school-level commitment). Suppression (0.48): Moderate-high. Significant barriers to non-Medinan innovation include the authority structure itself (Medinan practice must be treated as binding unless explicitly overridden by stronger sources) and institutional inertia (deference to Medinan precedent becomes habitual). However, suppression is not total — the schools develop recognized methodologies for juristic discretion (istihsan, maslaha) and gradually establish their own institutional prestige. Theater ratio (0.52): Moderate-high. By Abbasid period, citation of Medinan practice becomes increasingly performative as schools codify their own methodologies. Scholars invoke Urf of Medina for legitimacy without allowing it to constrain their reasoning chains. Early period (theater 0.28) shows more functional constraint; later period (theater 0.52) shows more legitimacy theater. The increase reflects institutional maturation of alternative schools — once Hanafi, Shafi'i, and Hanbali schools are fully codified, they can cite Medinan practice without being bound by it.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates maximal perspectival divergence. The Medinan scholarly authority sees a coordination mechanism (Rope) — they are solving the problem of legal unity across the Islamic world. Non-Medinan communities see pure extraction (Snare) — they are forced to defer to external authority. Emerging schools see mixed coordination-extraction (Tangled Rope) — they benefit from the shared framework but are constrained by legitimacy costs. Later institutional judges see theatrical performance (Piton) — they cite Medinan authority for legitimacy but are driven by their own school methodologies. The analytical observer at civilizational timescale sees an immutable natural law (Mountain) — Medinan authority flows from the Prophet's biography. The perspectival gap is structural, not epistemological: each perspective reflects a real difference in power, exit options, and institutional position relative to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary (Medinan scholars): Institutional power + Arbitrage exit → low d → negative effective extractiveness (they experience the constraint as beneficial coordination). Victim (Non-Medinan communities): Powerless + Trapped → high d → high effective extractiveness (they experience maximum extraction). Emerging schools: Organized + Constrained → moderate d → moderate effective extractiveness. The constraint's structure means that agents with geographic proximity to Medina and institutional control over precedent transmission have low d; agents distant from Medina and dependent on Medinan legitimacy have high d. The sigmoid f(d) magnifies this asymmetry in effective extraction (chi). Scope modifier σ(S) at regional scope (0.9) dampens the global calculation slightly, reflecting that Medinan authority is primarily a regional Islamic institution in early periods.
 *
 * MANDATROPHY ANALYSIS:
 *   The Maliki reading resolves the mandatrophy by clarifying which jurisprudential axioms generate the constraint. The core axiom is: 'The Prophet's practices as embedded in Medinan community norms carry independent authority as legal sources.' This axiom is distinct from the Hanafi axiom ('Juristic reason and analogical extension are primary tools') and the Shafi'i axiom ('Hadith authentication and strict source hierarchy constrain juristic discretion'). Each axiom produces a different classification. From the Maliki axiom, the constraint is Tangled Rope (coordination function + asymmetric extraction). From the Hanafi axiom, it becomes Rope or Rope-leaning Tangled Rope (lower suppression of innovation). From the Shafi'i axiom, it becomes Snare (strict hierarchy suppresses non-textual practice). From the Hanbali axiom, it becomes Mountain (textualism as natural law). The mandate resolves: this is Tangled Rope from the Maliki reading specifically because the Maliki school genuinely coordinates legal understanding across the Islamic world AND genuinely extracts authority from non-Medinan communities. Both are true simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    urf_medina_authenticity_threshold,
    'Which Medinan practices count as authentic Urf of the Prophet''s time versus later institutional elaboration or post-hoc rationalization?',
    'Isnad (chain of transmission) analysis; correlation between oldest documented Medinan practices and contemporary hadith corpus; archaeological and historical reconstruction of Medinan social practices',
    'If authenticity threshold is high (strict isnad requirements): fewer Medinan practices qualify, reducing suppression of non-Medinan innovation and shifting constraint toward rope classification. If threshold is low (looser standards): more practices qualify, increasing extraction and maintaining snare classification for non-Medinan communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(urf_medina_authenticity_threshold, empirical, 'Authenticity threshold for distinguishing genuine Urf of Medina from later institutional claims').

omega_variable(
    medinan_authority_geographic_scope,
    'Does Urf of Medina constitute universally binding authority or regionally-specific jurisprudential input?',
    'Systematic analysis of Maliki jurisprudence: frequency of geographic qualifier ''in Medina'' vs. universal application claims; comparison with non-Medinan Maliki practices adopted; historical record of local Maliki schools deviating from Medinan precedents',
    'If regionally-specific: constraint is coordination mechanism among equal schools (Rope from emerging schools'' perspective). If universally binding: constraint is extraction mechanism requiring justification for deviation (Snare from non-Medinan perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(medinan_authority_geographic_scope, conceptual, 'Whether Medinan practice authority is universal or geographic').

omega_variable(
    institutional_reading_divergence,
    'Do later Maliki courts follow Urf of Medina because it is binding authority, or because they cite it for legitimacy while their decisions are actually driven by school methodology?',
    'Comparative analysis of Maliki fatawa (legal opinions): ratio of cases where deviation from Medinan practice is explicitly justified vs. invoked without constraint; correlation between stated Medinan basis and actual reasoning chains in opinions',
    'If Urf of Medina is binding: constraint remains functionally determinative (Tangled Rope). If citation is legitimacy-theater: constraint becomes Piton (inertial performance) by later periods.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_reading_divergence, empirical, 'Whether Urf of Medina citation reflects binding authority or performative legitimacy').

omega_variable(
    readings_as_kernel_contest,
    'Which jurisprudential reading instantiates the most authoritative interpretation of Islamic legal sources?',
    'This is irreducible to empirical fact or logical deduction. It depends on which normative axiom about the Prophet''s authority and transmission reliability one accepts. The four readings (Hanafi, Shafi''i, Hanbali, Maliki) coexist as live positions held by different institutional traditions. The contest is not resolved by evidence — each reading is internally coherent and has empirical support. What varies is the weighting of Quran, Hadith, Medinan practice, Consensus, and Reason.',
    'Each reading produces a different classification type from the same constraint structure. Maliki reading: Tangled Rope (moderate extraction + coordination function). Hanafi reading: Rope or Tangled Rope (higher reliance on reason, lower suppression of innovation). Shafi''i reading: Snare (strict hierarchy suppresses local practice). Hanbali reading: Mountain (textualism appears as inherent legal requirement). The constraint''s true classification depends on which reading''s axioms one accepts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(readings_as_kernel_contest, preference, 'Which jurisprudential reading is most authoritative — irreducible kernel contest').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__maliki_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maliki_theater_umayyad, jurisprudential_method_kernel__maliki_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(maliki_theater_abbasid_early, jurisprudential_method_kernel__maliki_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(maliki_theater_abbasid_stabilized, jurisprudential_method_kernel__maliki_reading, theater_ratio, 100, 0.52).

% Extraction over time
narrative_ontology:measurement(maliki_extractiveness_umayyad, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(maliki_extractiveness_abbasid_early, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(maliki_extractiveness_abbasid_stabilized, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(maliki_suppression_umayyad, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(maliki_suppression_abbasid_early, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 50, 0.48).
narrative_ontology:measurement(maliki_suppression_abbasid_stabilized, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 100, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__maliki_reading, identity_coordination).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is a reading of a single contested kernel with three sibling readings. Each reading generates a separate constraint story with its own ε, beneficiary/victim structure, and classification. The Maliki reading elevates Medinan practice, producing Tangled Rope. The Hanafi reading elevates reason, producing Rope or lower-extraction Tangled Rope. The Shafi'i reading elevates strict hierarchy, producing Snare. The Hanbali reading elevates textualism, producing Mountain. These are not variants of one constraint — they are distinct constraints that instantiate different jurisprudential axioms from the same kernel. Network links show family relationship and mutual influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
