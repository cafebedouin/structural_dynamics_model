% ============================================================================
% CONSTRAINT STORY: notability_guidelines__inclusionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: notability_guidelines__inclusionist_reading
 *   human_readable: Wikipedia Notability Guidelines as Marginalized-Knowledge Gatekeeper (Inclusionist Reading)
 *   domain: digital_commons/epistemology/platform_governance
 *
 * SUMMARY:
 *   Wikipedia's Notability Guideline (WP:N) establishes that topics must
 *   demonstrate 'significant coverage in reliable secondary sources' to
 *   warrant encyclopedia articles. This story presents the inclusionist
 *   reading: WP:N functions as a structural gatekeeping apparatus that
 *   systematically excludes knowledge from marginalized communities,
 *   grassroots documentation projects, and non-English traditions whose
 *   sources lack institutional backing. The reading identifies institutional
 *   knowledge producers (academic publishing, established media,
 *   universities) as beneficiaries and marginalized communities as victims.
 *   The constraint persists because it reifies a particular epistemic
 *   standard as neutral while that standard systematically privileges those
 *   with access to formal publishing infrastructure. The constraint is
 *   CLAIMED as snare (this reading's assertion of extraction and gatekeeping)
 *   and the authored metrics describe high extractiveness and suppression
 *   that support that claim. This is one of three readings of the
 *   notability-guidelines kernel; the deletionist reading frames WP:N as a
 *   necessary quality filter; the deliberative reading frames it as emergent
 *   negotiation. The three readings are separate constraint stories linked by
 *   network edges, each with its own ε-invariant structure and
 *   beneficiary/victim set.
 *
 * KEY AGENTS:
 *   - Wikipedia notability enforcement: institutional agenda-setter administering the exclusionary standard
 *   - Institutional knowledge producers (academic publishers, journals, universities): beneficiaries who gain epistemic authority and visibility from the constraint
 *   - Marginalized communities (indigenous peoples, diaspora groups, activist movements): powerless victims trapped by epistemic gatekeeping
 *   - Non-English knowledge traditions: constrained victims facing compounding language and publication barriers
 *   - Grassroots documentation projects: constrained victims whose own work cannot establish their legitimacy under the constraint
 *   - Deletionist editors: powerful observers enforcing the constraint but not benefiting from it
 *   - Inclusionist editors: powerful but excluded from policy authority; would reshape the constraint but lack institutional backing
 *   - Academic publishing establishment: institutional beneficiary and secondary agenda-setter, whose authority is reified by WP:N
 *   - Wikimedia Foundation governance: institutional observer holding formal power to change WP:N but exercising it minimally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, 0.78).
domain_priors:suppression_score(notability_guidelines__inclusionist_reading, 0.82).
domain_priors:theater_ratio(notability_guidelines__inclusionist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__inclusionist_reading, snare).
narrative_ontology:human_readable(notability_guidelines__inclusionist_reading, "Wikipedia Notability Guidelines as Marginalized-Knowledge Gatekeeper (Inclusionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__inclusionist_reading, "digital_commons/epistemology/platform_governance").

domain_priors:requires_active_enforcement(notability_guidelines__inclusionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__inclusionist_reading, 'd0b979b4-2338-4897-9f64-69e896cdfdc6').
narrative_ontology:cs_kernel_codification('d0b979b4-2338-4897-9f64-69e896cdfdc6', formalized).
narrative_ontology:cs_authority_grounding('d0b979b4-2338-4897-9f64-69e896cdfdc6', extraction).
narrative_ontology:cs_interpretation_layer_present('d0b979b4-2338-4897-9f64-69e896cdfdc6').
narrative_ontology:cs_reading_relation('d0b979b4-2338-4897-9f64-69e896cdfdc6', notability_guidelines__deletionist_reading, forecloses).
narrative_ontology:cs_reading_relation('d0b979b4-2338-4897-9f64-69e896cdfdc6', notability_guidelines__deliberative_reading, coexists_with).
narrative_ontology:cs_axiom('d0b979b4-2338-4897-9f64-69e896cdfdc6', foundational, epistemic_gatekeeping_is_extraction).
narrative_ontology:cs_axiom_status(epistemic_gatekeeping_is_extraction, holdable).
narrative_ontology:cs_axiom_grounding('d0b979b4-2338-4897-9f64-69e896cdfdc6', epistemic_gatekeeping_is_extraction, deontological).
narrative_ontology:cs_axiom('d0b979b4-2338-4897-9f64-69e896cdfdc6', foundational, institutional_sources_systematically_exclude_marginalized_knowledge).
narrative_ontology:cs_axiom_status(institutional_sources_systematically_exclude_marginalized_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('d0b979b4-2338-4897-9f64-69e896cdfdc6', institutional_sources_systematically_exclude_marginalized_knowledge, empirically_contingent).
narrative_ontology:cs_reference_frame('d0b979b4-2338-4897-9f64-69e896cdfdc6', decentralized_epistemic_parity).
narrative_ontology:cs_drift_state('d0b979b4-2338-4897-9f64-69e896cdfdc6', contemporary_institutional_gatekeeping_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d0b979b4-2338-4897-9f64-69e896cdfdc6', '').
narrative_ontology:cs_kernel_id(notability_guidelines__inclusionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, established_academic_publishing).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, marginalized_communities).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, non_english_knowledge_traditions).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, grassroots_documentation_projects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, academic_publishing_establishment).
narrative_ontology:constraint_vindicates(notability_guidelines__inclusionist_reading, reliable_sources_hierarchy).
narrative_ontology:constraint_vindicates(notability_guidelines__inclusionist_reading, academic_gatekeeping_as_quality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers WP:N through policy documents, deletion reviews, article assessments, and editorial culture. Enforces the 'reliable sources' standard by rejecting or deleting articles lacking institutional documentation. Frames the enforcement as epistemic quality control and vandalism prevention.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikipedia_notability_enforcement, agenda_setter,
    institutional, generational, analytical, global).

% Academic journals, university presses, major news organizations, and university-affiliated scholars whose output Wikipedia treats as 'reliable.' Their knowledge pathways (peer review, editorial boards, institutional credentials) are recognized as authoritative. Notability requirements guarantee Wikipedia articles cite and amplify their work. They can publish elsewhere or decline Wikipedia prominence; they have multiple exit points.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, institutional_knowledge_producers, beneficiary,
    institutional, generational, arbitrage, global).

% Indigenous peoples, diaspora communities, informal-economy workers, activist movements, and other groups whose knowledge is documented through oral tradition, community archives, self-published accounts, regional media, or grassroots digital platforms. Their sources are systematically devalued under notability criteria because they lack institutional backing or English-language academic circulation. Their histories and expertise are excluded from Wikipedia. Wikipedia is a near-monopoly knowledge commons with no viable alternatives; they cannot exit.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, marginalized_communities, payer,
    powerless, biographical, trapped, global).

% Languages, scholarly traditions, and knowledge communities outside English-language academic publishing face compounding barriers. Their sources are not 'reliable' by English-centric criteria. Wikipedia's English edition dominates globally. Local-language Wikipedias lack volunteer base and institutional support. A scholar publishing in Japanese historical journals or a medical tradition documented in Arabic cannot easily establish notability.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, non_english_knowledge_traditions, payer,
    moderate, generational, constrained, global).

% Wikipedia editors, wikis, and citizen-journalism projects that document local knowledge, social movements, and marginalized histories. Their sources are devalued as 'unreliable' — self-published, lacking traditional editorial review, lacking institutional authority. They cannot create Wikipedia articles to establish legitimacy for their own documentation; they are trapped by the constraint they help populate.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, grassroots_documentation_projects, payer,
    moderate, biographical, constrained, global).

% Edit-war practitioners and strict-notability advocates who enforce WP:N and drive deletion reviews. They are not the agenda-setter (policy precedes them) and not beneficiaries (they gain no economic rents). They perform the deletions and maintain the boundary. They could exit by leaving the project or adopting permissive stances.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, deletionist_editors, observer,
    powerful, biographical, mobile, global).

% Editors arguing for lower notability thresholds, broader source recognition, and systematic inclusion of marginalized knowledge. They are excluded from policy authority and do not benefit from the current regime. They would argue for notability reform but lack institutional backing to reshape epistemic standards.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, inclusionist_editors, excluded,
    powerful, biographical, mobile, global).

% Major journal publishers, university presses, and academic credentialing systems whose authority Wikipedia's 'reliable sources' standard reifies. Not Wikipedia employees, but WP:N crystallizes and amplifies their gatekeeping. Influence policy through editor communication and public advocacy. Benefit from Wikipedia funneling knowledge-seekers toward academic publications.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, academic_publishing_establishment, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__inclusionist_reading, academic_publishing_establishment, agenda_setter).

% Holds formal authority to alter WP:N but does so only under sustained external pressure. Positions itself as neutral steward. Could change the constraint (has the power) but minimally exercises that power. Observes the dispute without decisive intervention.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikimedia_foundation_governance, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:fixing_cost_class(notability_guidelines__inclusionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, globally applicable epistemic standard for what counts as documented knowledge worthy of encyclopedia coverage. Solves the problem of negotiating proof-of-significance for every article individually by delegating to a principle: only documented in 'reliable sources' counts as notability. Coordinates volunteer editors around a shared legitimacy criterion.
% TRANSFER_FUNCTION: Transfers epistemic authority and visibility from marginalized knowledge traditions to institutional knowledge producers whose output Wikipedia treats as 'reliable.' A person or community seeking notability must funnel their knowledge through institutional channels (academic publishing, journalism, formal organizations) to reach Wikipedia. The constraint moves legitimacy upward to institutional publishers and academic credentialing systems.
% ABSENT_VOICES: Marginalized communities whose knowledge is excluded from Wikipedia cannot easily argue for inclusion — they lack the institutional standing and 'reliable sources' required to achieve policy standing. Indigenous knowledge holders, oral-tradition communities, activist historians, and grassroots documentation movements are structurally absent from policy discussions because the policy that excludes them makes it hard for them to speak about policy.
% DISAPPEARANCE_RATIONALE: If WP:N and its enforcement vanished overnight, Wikipedia's article set would expand rapidly to include grassroots and marginalized knowledge. Editorial attention would shift to documenting previously excluded histories and communities. Institutional knowledge would no longer monopolize the encyclopedia. The knowledge commons would reorganize around more permissive inclusion thresholds.
% FOUNDING_PROBLEM: Early Wikipedia faced degradation from spam, hoaxes, and low-quality original research. Notability requirements were introduced to ensure articles rest on documented evidence rather than speculation or fringe claims.
% FOUNDING_PROBLEM_CORROBORATION: Wikipedia's administrative community attests the founding problem remains live, citing ongoing spam and hoax risks. Academic and community researchers outside Wikipedia (scholars of digital commons, archivists, grassroots historians) attest the founding problem is substantially solved by existing moderation mechanisms and that WP:N persists primarily as a barrier to marginalized knowledge inclusion, not as necessary quality control. Legislative bodies and policy research institutes investigating Wikipedia's coverage gaps document systematic exclusion of minority perspectives, supporting the gatekeeping reading over the quality-control reading.
narrative_ontology:disappearance_verdict(notability_guidelines__inclusionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__inclusionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__inclusionist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(notability_guidelines__inclusionist_reading, 'none', 1).

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
 *   Extractiveness is high (0.78 at interval end) because the constraint transfers epistemic authority from marginalized communities to institutional producers — knowledge must funnel through academic publication to gain Wikipedia legitimacy, guaranteeing that Wikipedia articles cite (and amplify) institutional sources. Suppression is high (0.82) because the constraint is actively enforced: editors delete articles lacking 'reliable sources,' close deletion reviews against inclusionist arguments, and maintain the boundary through policy enforcement and cultural gatekeeping. Theater ratio (0.41, rising from 0.25) reflects that enforcement justification increasingly performs epistemic quality-control while the structural effect is gatekeeping — the ratio rises because inclusionist pushback requires more elaborate quality-defense narratives. Accessibility collapse is high (0.71) because once the 'reliable sources' standard is understood as institutional-publication requirement, marginalized communities see the barrier as near-absolute: they cannot make their knowledge 'notable' without institutional intermediation. Resistance is substantial (0.68) from inclusionist editors and external advocates arguing for source-standard reform and lower thresholds, but this resistance remains structurally weaker than the enforcement power. The measurement series shows base_extractiveness rising over the interval (0.64 → 0.78), indicating that enforcement intensity has increased and the constraint has become more extractive as Wikipedia matured and the knowledge commons monetized — later institutional players discovered the high value of Wikipedia visibility and invested more in gatekeeping. Theater ratio also rises (0.25 → 0.41), signaling that as resistance grows, enforcement must justify itself more elaborately, shifting enforcement effort from deletion to defense-narratives.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter (Wikipedia enforcement) and beneficiary (institutional publishers) seats, WP:N appears as a necessary quality mechanism — without it, Wikipedia degrades into original research and fringe theory. The constraint is experienced as coordination: 'reliable sources' is a shared standard that enables volume editing at scale. From the victim seats (marginalized communities, grassroots documentation), the same constraint appears as extraction and exclusion — a rule designed for institutional knowledge and impossible to satisfy for self-documented knowledge. The engine will compute different types from these two structural positions: the beneficiary seat experiences low directionality (d near 0.0, benefiting from the constraint) and the victim seat experiences high directionality (d near 1.0, extracted from). The cognitive gap is not noise — it is the signature of a snare masquerading as coordination. If both seats experienced the constraint as rope (genuine coordination with symmetric benefit), the directionality would be near 0.5 throughout and the types would align. The measurement and commentary document why they do not.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional knowledge producers sit near the beneficiary pole (d ≈ 0.1–0.2). They hold institutional power, control the publishing infrastructure that defines 'reliable sources,' have mobile exit options (can publish elsewhere, can decline Wikipedia engagement), and collect direct benefit from the constraint — Wikipedia funnels readers to academic citations. Marginalized communities sit near the target pole (d ≈ 0.8–0.9). They are powerless (lack institutional backing), face trapped exit (Wikipedia is a near-monopoly knowledge commons with no viable alternatives), and bear the extraction of epistemic authority — their knowledge is devalued unless filtered through institutions. Grassroots documentation projects sit in the middle-to-target range (d ≈ 0.65–0.75) — they have moderate power (can coordinate community editing efforts), face constrained exit (alternatives exist but are marginal), and bear costs (their documentation is excluded or devalued). The Wikipedia enforcement apparatus and deletionist editors sit near the beneficiary end because they hold institutional power and have mobile exit, but they are classified as observers rather than beneficiaries because they derive no direct extraction — they enforce a policy they believe in, not a rule designed for their enrichment. The directive flow from agenda-setter to victims is clear and unidirectional: the constraint extracts from those without institutional voice toward those with institutional credibility.
 *
 * MANDATROPHY ANALYSIS:
 *   WP:N's founding problem — preventing Wikipedia degradation from spam, hoaxes, and low-quality original research — has substantially resolved. Wikipedia's technical infrastructure (vandal-patrol tools, edit-warring mechanisms, rollback privileges) and community culture (experienced editors, revision history, talk-page deliberation) now suppress vandalism and low-quality insertion without needing restrictive notability thresholds. Yet WP:N persists and is actively enforced, at increasing cost (theater ratio rising from 0.25 to 0.41). The constraint exhibits classic mandatrophy signals: the mandate (prevent degradation) is partially dead (solved by other mechanisms), but the constraint persists because it now functions as an epistemically-justified gatekeeping apparatus benefiting institutional actors who have become invested in defending it. The persistence is maintained by redescribing the founding problem as permanently live ('spam and hoax risks never end') and by redefining 'quality' to mean 'institutional sources' rather than 'accurate information.' From the deletionist reading perspective, mandatrophy is absent — the problem is live and the solution is necessary. From the inclusionist reading perspective (this story), mandatrophy is present and worsening. The six_questions mismatch (founding_problem_status = contested) captures this: Wikipedia defenders say the founding problem is still live; external researchers say it is solved but the constraint persists for gatekeeping reasons.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reliable_sources_neutrality,
    'Is the ''reliable sources'' standard genuinely neutral with respect to knowledge traditions, or does it systematically privilege English-language institutional publication over oral tradition, community documentation, and non-English scholarship?',
    'Bibliometric analysis of Wikipedia''s source citations by language, publication venue, and author institutional affiliation over time. Qualitative interviews with editors from marginalized communities about barriers to inclusion. Audit of which topics are included/excluded controlling for notability under different source standards.',
    'If the standard is institutionally neutral, the high extractiveness score may reflect coordination cost rather than gatekeeping. If the standard systematically privileges institutional sources, the constraint is purely extractive and the ''quality'' justification is ideology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reliable_sources_neutrality, empirical, 'Whether reliable sources standard is epistemically neutral or institutionally biased.').

omega_variable(
    kernel_reading_contest,
    'Is WP:N a structural gatekeeping apparatus that extracts epistemic authority from marginalized communities, or a necessary quality filter that prevents commons degradation, or a deliberative process where boundaries evolve through community negotiation?',
    'Methodologically: Delphi survey of Wikipedia editors, marginalized-community researchers, and platform-governance scholars rating agreement with each reading. Longitudinal analysis: which reading better predicts outcomes if policy is changed? (If deletionist assumptions are correct, loosening notability will degrade quality; if inclusionist assumptions are correct, loosening will enrich coverage without quality loss; if deliberative assumptions are correct, policy change will trigger re-negotiation that converges on a new equilibrium.) Cross-reading edge analysis: if this reading is correct, the sibling readings should show structural pressures that are absent if siblings are correct.',
    'If the inclusionist reading is correct, WP:N is a snare requiring structural reform — marginalized knowledge must find alternative pathways (off-Wikipedia documentation, parallel commons). If the deletionist reading is correct, WP:N is a mountain or rope — the constraint is necessary and changes should be marginal. If the deliberative reading is correct, WP:N is a tangled rope — some coordination function exists but the negotiation process is asymmetric.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The kernel reading contest: WP:N''s identity is disputed across three structurally distinct readings.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the high suppression (0.82) a result of external enforcement (editors deleting articles, closing AfD discussions, blocking alternative source recognition) or internalized suppression (marginalized communities adopting the ''reliable sources'' standard as their own epistemic ceiling, deciding their knowledge is ''not notable'' before attempting inclusion)?',
    'Ethnographic study of Wikipedia editing in marginalized-community projects. Post-deletion trajectory analysis: do excluded communities attempt re-inclusion after policy change, or do they remain inactive (signaling internalized suppression)? Survey of non-Wikipedia digital commons to measure knowledge-production patterns in alternative platforms.',
    'If suppression is external, removing enforcement would enable rapid re-inclusion. If suppression is substantially internalized, even policy change would require community re-engagement and identity reframing. The distribution between structural and internalized suppression affects the cost and timeline of remediation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'External vs. internalized suppression in marginalized-knowledge exclusion.').

omega_variable(
    alternative_pathways_availability,
    'To what extent do alternative knowledge commons (Wikitribune, Everipedia, local-language Wikipedias, community-controlled archives, diaspora platforms) provide viable exit for marginalized communities, or are they too small and low-authority to constitute real alternatives?',
    'Traffic analysis and citation patterns: how much audience do alternatives capture relative to Wikipedia? Authority measurement: do institutions and media cite from alternative commons at rates approaching Wikipedia citations? Accessibility audit: are alternative platforms equally discoverable and usable for knowledge-seeking populations in marginalized communities?',
    'If real alternatives exist, the constraint''s suppression is lower than measured — exit is more available than WP:N''s monopoly suggests. If alternatives are marginal, the trap is structural and suppression reflects real barriers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_pathways_availability, empirical, 'Exit viability: do genuine alternatives exist, or is Wikipedia a near-monopoly knowledge commons?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__inclusionist_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(notability_inclusionist_tr_t0, notability_guidelines__inclusionist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(notability_inclusionist_tr_t0, observed).
narrative_ontology:measurement(notability_inclusionist_tr_t5, notability_guidelines__inclusionist_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(notability_inclusionist_tr_t5, observed).
narrative_ontology:measurement(notability_inclusionist_tr_t10, notability_guidelines__inclusionist_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(notability_inclusionist_tr_t10, observed).
narrative_ontology:measurement(notability_inclusionist_tr_t15, notability_guidelines__inclusionist_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(notability_inclusionist_tr_t15, observed).
narrative_ontology:measurement(notability_inclusionist_tr_t20, notability_guidelines__inclusionist_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(notability_inclusionist_tr_t20, observed).
narrative_ontology:measurement(notability_inclusionist_tr_t25, notability_guidelines__inclusionist_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(notability_inclusionist_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(notability_inclusionist_be_t0, notability_guidelines__inclusionist_reading, base_extractiveness, 0, 0.64).
narrative_ontology:measurement_basis(notability_inclusionist_be_t0, observed).
narrative_ontology:measurement(notability_inclusionist_be_t5, notability_guidelines__inclusionist_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement_basis(notability_inclusionist_be_t5, observed).
narrative_ontology:measurement(notability_inclusionist_be_t10, notability_guidelines__inclusionist_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement_basis(notability_inclusionist_be_t10, observed).
narrative_ontology:measurement(notability_inclusionist_be_t15, notability_guidelines__inclusionist_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement_basis(notability_inclusionist_be_t15, observed).
narrative_ontology:measurement(notability_inclusionist_be_t20, notability_guidelines__inclusionist_reading, base_extractiveness, 20, 0.77).
narrative_ontology:measurement_basis(notability_inclusionist_be_t20, observed).
narrative_ontology:measurement(notability_inclusionist_be_t25, notability_guidelines__inclusionist_reading, base_extractiveness, 25, 0.78).
narrative_ontology:measurement_basis(notability_inclusionist_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(notability_inclusionist_su_t0, notability_guidelines__inclusionist_reading, suppression_requirement, 0, 0.76).
narrative_ontology:measurement_basis(notability_inclusionist_su_t0, observed).
narrative_ontology:measurement(notability_inclusionist_su_t5, notability_guidelines__inclusionist_reading, suppression_requirement, 5, 0.78).
narrative_ontology:measurement_basis(notability_inclusionist_su_t5, observed).
narrative_ontology:measurement(notability_inclusionist_su_t10, notability_guidelines__inclusionist_reading, suppression_requirement, 10, 0.79).
narrative_ontology:measurement_basis(notability_inclusionist_su_t10, observed).
narrative_ontology:measurement(notability_inclusionist_su_t15, notability_guidelines__inclusionist_reading, suppression_requirement, 15, 0.81).
narrative_ontology:measurement_basis(notability_inclusionist_su_t15, observed).
narrative_ontology:measurement(notability_inclusionist_su_t20, notability_guidelines__inclusionist_reading, suppression_requirement, 20, 0.82).
narrative_ontology:measurement_basis(notability_inclusionist_su_t20, observed).
narrative_ontology:measurement(notability_inclusionist_su_t25, notability_guidelines__inclusionist_reading, suppression_requirement, 25, 0.82).
narrative_ontology:measurement_basis(notability_inclusionist_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__inclusionist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(notability_guidelines__inclusionist_reading, 0.12).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, notability_guidelines__deletionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, notability_guidelines__deliberative_reading).

% DUAL FORMULATION NOTE:
% The notability-guidelines kernel decomposes into three constraint stories: deletionist_reading (quality filter, low extraction, rope/mountain), deliberative_reading (negotiated equilibrium, tangled rope), and inclusionist_reading (gatekeeping apparatus, high extraction, snare). Each reading instantiates a different epsilon value, beneficiary structure, and type. The three are linked as a constraint family via affects_constraints edges. No single story captures the kernel's contested identity; the three together model the kernel-reading dispute as three structurally-distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(notability_guidelines__inclusionist_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
