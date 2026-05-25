% ============================================================================
% CONSTRAINT STORY: wikipedia_editing_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wikipedia_editing_authority, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: wikipedia_editing_authority
 *   human_readable: Wikipedia Editing Authority and Notability Gatekeeping
 *   domain: information_governance/digital_commons
 *
 * SUMMARY:
 *   Wikipedia's editing authority structure — embodied in notability
 *   standards, administrative gatekeeping, and the requirement for
 *   secondary-source citations — presents as a coordination mechanism for
 *   maintaining article quality and preventing vandalism. However, empirical
 *   analysis reveals systematic extraction: the authority structure
 *   privileges established editors, institutional knowledge holders
 *   (academia, major media), and English-language sources while suppressing
 *   marginalized contributors, non-Western epistemologies, and voices lacking
 *   institutional legitimacy. The constraint exhibits both genuine
 *   coordination (collaborative article improvement, knowledge aggregation,
 *   spam prevention) and asymmetric extraction (gatekeeping that advantages
 *   certain contributors over others). The theater ratio has increased from
 *   0.45 to 0.64 over 14 years, reflecting that deletion review processes and
 *   notability arbitration are increasingly performative — they feel rigorous
 *   but often reinforce existing power structures rather than enforcing
 *   objective quality standards. The extractiveness has risen from 0.35 to
 *   0.52, showing that the gatekeeping mechanism has become more extractive
 *   relative to its coordination function as the Wikipedia knowledge base has
 *   matured and the resource of editing authority has become more valuable.
 *
 * KEY AGENTS:
 *   - Marginalized Contributors: Primary victims (powerless/trapped) — face deletion, hostile gatekeeping, exclusion from knowledge commons despite legitimate expertise
 *   - Occasional Contributors: Secondary victims (moderate/constrained) — experience friction and learning barriers but can contribute at cost
 *   - Established Editors/Administrators: Primary beneficiaries (institutional/arbitrage) — maintain authority hierarchy, control article direction, build reputation within Wikipedia ecosystem
 *   - Institutional Knowledge Holders: Secondary beneficiaries (powerful/mobile) — academic institutions, news outlets, think tanks whose sources are privileged in notability standards; experience both coordination benefits and structural advantage
 *   - Wikipedia Governance Structure: Institutional actor (organized/constrained) — maintains notability standards and editing authority through inertial institutional processes; sees own gatekeeping as degraded (piton perspective) but continues it
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the specific historical gatekeeping mechanisms as universal properties of collaborative knowledge curation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wikipedia_editing_authority, 0.52).
domain_priors:suppression_score(wikipedia_editing_authority, 0.58).
domain_priors:theater_ratio(wikipedia_editing_authority, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wikipedia_editing_authority, extractiveness, 0.52).
narrative_ontology:constraint_metric(wikipedia_editing_authority, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(wikipedia_editing_authority, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wikipedia_editing_authority, tangled_rope).
narrative_ontology:human_readable(wikipedia_editing_authority, "Wikipedia Editing Authority and Notability Gatekeeping").
narrative_ontology:topic_domain(wikipedia_editing_authority, "information_governance/digital_commons").

domain_priors:requires_active_enforcement(wikipedia_editing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wikipedia_editing_authority, established_editors).
narrative_ontology:constraint_beneficiary(wikipedia_editing_authority, institutional_knowledge_holders).
narrative_ontology:constraint_victim(wikipedia_editing_authority, marginalized_contributors).
narrative_ontology:constraint_victim(wikipedia_editing_authority, emerging_perspectives).
narrative_ontology:constraint_victim(wikipedia_editing_authority, non_english_speakers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED CONTRIBUTOR (SNARE) — A person with legitimate knowledge (local historian, non-Western scholar, practitioner from underrepresented community) attempts to edit Wikipedia but faces deletion of work, hostile talk-page encounters, accusation of conflict-of-interest, and arbitrary application of 'notability' standards that privilege English-language academic and media sources. No appeal mechanism that works. The victim bears full cost of exclusion from the knowledge commons.
constraint_indexing:constraint_classification(wikipedia_editing_authority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OCCASIONAL CONTRIBUTOR (TANGLED ROPE) — Benefits from Wikipedia's coordination function (access to collective knowledge editing, collaborative article improvement, learning from established editors) but faces friction from notability gatekeeping, edit-warring dynamics, and the cost of learning implicit norms. Can contribute but at significant friction cost. Mixed extraction and coordination.
constraint_indexing:constraint_classification(wikipedia_editing_authority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED EDITOR (ROPE) — Experienced Wikipedia contributors and administrators experience the notability standards and editing authority structures as coordination mechanisms that enable their work: they set boundaries, maintain article quality, prevent vandalism, and establish reputation within the knowledge hierarchy. Net beneficiary with arbitrage options (can edit across projects, leverage Wikipedia reputation elsewhere).
constraint_indexing:constraint_classification(wikipedia_editing_authority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL KNOWLEDGE HOLDER (TANGLED ROPE) — University presses, academic journals, major news outlets, and think tanks experience Wikipedia as both a coordination mechanism (distribution of knowledge, public credibility) and an extraction point (their institutional legitimacy is required to pass notability tests; their publications are citations required for inclusion). High coordination value but structured asymmetry: Wikipedia depends on their sources for legitimacy; they depend on Wikipedia for reach. Mixed but asymmetric.
constraint_indexing:constraint_classification(wikipedia_editing_authority, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: WIKIPEDIA GOVERNANCE STRUCTURE (PITON) — The notability standards (requiring substantial coverage in secondary sources, preferring English-language media, privileging academic citation) were appropriate coordination rules when Wikipedia was young and faced vandalism and spam. They persist through institutional inertia despite the knowledge base maturing. The editing authority system is maintained through performative community consensus and deletion review processes that are largely theater — they feel rigorous but often reinforce the original bias. Theater ratio 0.64 reflects that much gatekeeping activity is procedural maintenance rather than active quality control.
constraint_indexing:constraint_classification(wikipedia_editing_authority, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational scale, any collaborative knowledge system must have some boundary-setting mechanism and some way of distinguishing notable from trivial. Some gatekeeping is inherent to the problem of collective knowledge curation. However, the specific mechanisms (notability standards, citation hierarchies, English-language bias) are contingent institutions, not natural laws. The engine should identify this as a false summit.
constraint_indexing:constraint_classification(wikipedia_editing_authority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wikipedia_editing_authority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wikipedia_editing_authority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wikipedia_editing_authority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(wikipedia_editing_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(wikipedia_editing_authority, TR),
    TR >= 0.70.

:- end_tests(wikipedia_editing_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Wikipedia's authority structures do extract value asymmetrically — they grant power to established editors, privilege institutional knowledge sources, and exclude marginalized voices. However, the extraction is not total because the coordination function is genuine: Wikipedia does aggregate knowledge effectively, prevent vandalism, and create usable commons. The rise from 0.35 to 0.52 reflects that as Wikipedia matured, the gatekeeping became more selective and the extraction more conscious (editors defending notability standards explicitly rather than accidentally). Suppression (0.58): Moderate-high. Multiple suppression mechanisms operate: the notability standard itself (requires institutional legitimacy), the edit-war social cost, the deletion review process (biased toward deletion when in doubt), the requirement to learn implicit norms, and language barriers. These are not total (contributors can and do get articles through) but substantial. Theater ratio (0.64): Moderate-high. Much of Wikipedia's deletion and notability arbitration process is performative. Deletion discussions cite policy language but often serve as status maintenance for established editors. The process feels rigorous (formal consensus-building, clear procedures) but outcomes are highly correlated with editor power and social position, not with objective quality measures. Theater has increased as the community has professionalized its procedures without increasing actual accountability to excluded voices.
 *
 * PERSPECTIVAL GAP:
 *   The most significant perspectival gap is between established editors (who see Rope) and marginalized contributors (who see Snare). To an established editor with high reputation, the notability standards are sensible quality gates and the authority structure is legitimate expertise-based hierarchy. To a marginalized contributor with local knowledge but no institutional backing, the same standards are arbitrary gatekeeping that excludes knowledge without good reason. The Wikipedia governance system sees itself as maintaining degraded coordination (Piton) — administrators know that notability standards were designed for a different context and suppress work that they sometimes privately believe is valuable, but continue the system because alternatives are unclear. The institutional knowledge holder (academic presses, news outlets) sees mixed benefits (their work is privileged but Wikipedia depends on them, creating mutual rather than one-way extraction). The false summit perspective naturalizes all gatekeeping as inherent to collaborative knowledge — the engine should identify this as naturalization of contingent institutional arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint creates asymmetric directionality across perspectives. Established editors occupy low-d positions (beneficiaries with arbitrage exit) — they experience the authority structure as coordination that enables their work. Marginalized contributors occupy high-d positions (victims with trapped exit) — they experience it as pure extraction, unable to contest gatekeeping decisions. Occasional contributors occupy middle-d positions (constrained exit with mixed costs and benefits). Institutional knowledge holders occupy complex positions: they appear as beneficiaries (their sources are privileged) but are also partly dependent (they need Wikipedia to distribute their knowledge). This asymmetry is the core feature of the tangled rope classification: genuine coordination function coexists with asymmetric extraction, with different actors experiencing opposite ends of the same mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint clearly resolves the mandatrophy by showing that the classification depends critically on the observer's structural position within the editing hierarchy. The Snare perspective is the victim's experience — a real structural reality for marginalized contributors. The Rope perspective is the beneficiary's experience — equally real for established editors. The Piton perspective captures the institutional system's own sense of degradation. The Tangled Rope primary classification represents the constraint's full structure: coordination (Wikipedia does aggregate knowledge) overlays extraction (that aggregation process privileges certain voices). The false summit (naturalizing all gatekeeping as inevitable) is correctly diagnosed as a misclassification. The mandatrophy is resolved by accepting that all perspectives are empirically accurate descriptions of different positions within the same constraint structure, and that the extraction component is not inevitable but contingent on the specific notability standards and authority mechanisms chosen.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    notability_standard_neutrality,
    'Are notability standards (requiring secondary coverage) neutral coordination rules or mechanisms that systematically privilege certain types of knowledge and sources?',
    'Comparative analysis of article survival rates by: (a) author background (academic vs practitioner vs community member), (b) topic geographic origin, (c) language of available sources, (d) media coverage patterns by region/topic. Quantitative bias audit of deletion decisions.',
    'If neutral: notability standards are coordination mechanisms (Rope from more perspectives). If biased: standards are extraction mechanisms that systematically exclude non-institutional voices (Snare from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(notability_standard_neutrality, empirical, 'Whether notability standards are neutral or biased toward institutional sources').

omega_variable(
    expertise_vs_consensus_authority,
    'When established editors suppress a contribution, is the authority grounded in expertise/quality judgment or in social status within the editing hierarchy?',
    'Analysis of deletion discussions: frequency of explicit quality/accuracy rationale vs invocation of notability/precedent. Follow-up: do suppressed articles re-emerge credibly (in other contexts, other platforms, or after editor turnover) with unmodified content?',
    'If expertise-grounded: suppression is legitimate quality gatekeeping (Rope). If status-grounded: suppression is extraction (Snare/Tangled Rope with downward direction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expertise_vs_consensus_authority, empirical, 'Whether editing authority is grounded in expertise or social status').

omega_variable(
    alternative_curation_effectiveness,
    'Do alternative collaborative knowledge platforms (Wikidata, community wikis, decentralized models) solve the notability gatekeeping problem or recreate it at different scales?',
    'Comparative analysis of article inclusion rates, diversity of contributor backgrounds, and knowledge coverage by platform. Assessment of whether non-platform solutions (local knowledge repositories, oral tradition documentation) achieve equivalent coordination without gatekeeping extraction.',
    'If alternatives are effective: Wikipedia''s constraints are contingent, not universal (scaffold sunset possible). If alternatives recreate gatekeeping: the problem is structural to collaborative knowledge curation itself (mountain or unavoidable rent-seeking).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_curation_effectiveness, empirical, 'Whether alternative platforms solve or recreate notability gatekeeping').

omega_variable(
    english_language_source_privilege,
    'How much of Wikipedia''s notability bias toward English-language academic/media sources reflects genuine knowledge accessibility vs systematic exclusion of non-English epistemologies?',
    'Comparative citation analysis: what percentage of Wikipedia articles cite sources in languages other than English? What proportion of deleted articles had non-English primary sources available? Analysis of whether non-English-speaking editors'' contributions survive at different rates than English-speakers with similar knowledge credentials.',
    'If accessibility-driven: bias is coordination cost (higher verification in minority languages). If exclusionary: bias is extractive (Snare targeting non-English-speaking contributors).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(english_language_source_privilege, empirical, 'Whether English source privilege reflects accessibility or exclusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wikipedia_editing_authority, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wikiedit_tr_t0, wikipedia_editing_authority, theater_ratio, 0, 0.45).
narrative_ontology:measurement(wikiedit_tr_t7, wikipedia_editing_authority, theater_ratio, 7, 0.58).
narrative_ontology:measurement(wikiedit_tr_t14, wikipedia_editing_authority, theater_ratio, 14, 0.64).

% Extraction over time
narrative_ontology:measurement(wikiedit_be_t0, wikipedia_editing_authority, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(wikiedit_be_t7, wikipedia_editing_authority, base_extractiveness, 7, 0.45).
narrative_ontology:measurement(wikiedit_be_t14, wikipedia_editing_authority, base_extractiveness, 14, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wikipedia_editing_authority, information_standard).
narrative_ontology:affects_constraint(wikipedia_editing_authority, knowledge_source_hierarchy).
narrative_ontology:affects_constraint(wikipedia_editing_authority, english_language_bias_digital).
narrative_ontology:affects_constraint(wikipedia_editing_authority, institutional_epistemology_privilege).

% DUAL FORMULATION NOTE:
% Wikipedia editing authority is part of a constraint family with upstream structural biases in knowledge legitimacy (source hierarchy, English-language privilege, institutional epistemology) and downstream consequences for knowledge representation (coverage gaps, non-Western epistemologies underrepresented). Each story in the family has distinct extractiveness values reflecting its specificity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wikipedia_editing_authority, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
