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
 *   human_readable: Wikipedia Notability Guideline (WP:N) as Gatekeeping Apparatus Excluding Marginalized Knowledge
 *   domain: digital commons governance / knowledge infrastructure / platform constitutionalism
 *
 * SUMMARY:
 *   Wikipedia's General Notability Guideline (WP:N) requires 'significant
 *   coverage in reliable, independent secondary sources' for a topic to
 *   warrant a standalone article. This story authors the inclusionist reading
 *   of that kernel: the guideline is not a neutral quality filter but a
 *   structural gatekeeping apparatus that launders a historical asymmetry —
 *   which communities' knowledge was ever mediated through print/broadcast
 *   media recognized as 'reliable' — into a present-day asymmetry in
 *   encyclopedic memory. Under this reading, the coordination story
 *   (preventing spam and unverifiable content) is real but functions as cover
 *   for a deeper extractive pattern: institutional knowledge producers and
 *   the editor corps whose interests already track existing coverage benefit
 *   from a rule that costs nothing to them and everything to communities
 *   whose knowledge circulated outside the legacy press. This is deliberately
 *   ONE of three readings of the shared notability_guidelines kernel; the
 *   deletionist reading (necessary epistemic filter) and the deliberative
 *   reading (AfD as perpetual evolving negotiation) are separate constraints
 *   with their own ε values, not alternative measurements of this one.
 *
 * KEY AGENTS:
 *   - established_wikipedia_editor_corps: agenda_setter/beneficiary (organized/arbitrage) — writes and enforces the guideline, rarely burdened by it
 *   - institutional_knowledge_producers: beneficiary (institutional/arbitrage) — unearned reference standard, collects nothing but sets the bar
 *   - indigenous_knowledge_holders: payer (powerless/trapped) — knowledge never mediated by 'reliable' press, systematically excluded
 *   - diaspora_community_historians: payer (powerless/constrained) — documented in community media the guideline discounts
 *   - global_south_local_topic_editors: payer (moderate/constrained) — thin local press infrastructure penalized as if it were unimportance
 *   - oral_tradition_documentarians: payer (powerless/trapped) — oral sourcing treated as inherently unreliable
 *   - readers_seeking_marginalized_topics: excluded — never learn what's missing or why
 *   - wikimedia_foundation: observer/beneficiary (institutional/analytical) — delegates enforcement, benefits from reputational stability
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
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__inclusionist_reading, snare).
narrative_ontology:human_readable(notability_guidelines__inclusionist_reading, "Wikipedia Notability Guideline (WP:N) as Gatekeeping Apparatus Excluding Marginalized Knowledge").
narrative_ontology:topic_domain(notability_guidelines__inclusionist_reading, "digital commons governance / knowledge infrastructure / platform constitutionalism").

domain_priors:requires_active_enforcement(notability_guidelines__inclusionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__inclusionist_reading, 'ab75a102-5410-43dd-a013-2ad5b15a9195').
narrative_ontology:cs_kernel_codification('ab75a102-5410-43dd-a013-2ad5b15a9195', distributed).
narrative_ontology:cs_authority_grounding('ab75a102-5410-43dd-a013-2ad5b15a9195', practice).
narrative_ontology:cs_interpretation_layer_present('ab75a102-5410-43dd-a013-2ad5b15a9195').
narrative_ontology:cs_reading_relation('ab75a102-5410-43dd-a013-2ad5b15a9195', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab75a102-5410-43dd-a013-2ad5b15a9195', notability_guidelines__deliberative_reading, influences).
narrative_ontology:cs_axiom('ab75a102-5410-43dd-a013-2ad5b15a9195', foundational, source_reliability_is_media_access_dependent).
narrative_ontology:cs_axiom_status(source_reliability_is_media_access_dependent, holdable).
narrative_ontology:cs_axiom_grounding('ab75a102-5410-43dd-a013-2ad5b15a9195', source_reliability_is_media_access_dependent, empirically_contingent).
narrative_ontology:cs_axiom('ab75a102-5410-43dd-a013-2ad5b15a9195', foundational, encyclopedic_absence_constitutes_epistemic_harm).
narrative_ontology:cs_axiom_status(encyclopedic_absence_constitutes_epistemic_harm, holdable).
narrative_ontology:cs_axiom_grounding('ab75a102-5410-43dd-a013-2ad5b15a9195', encyclopedic_absence_constitutes_epistemic_harm, deontological).
narrative_ontology:cs_reference_frame('ab75a102-5410-43dd-a013-2ad5b15a9195', verifiability_through_editorial_mediation).
narrative_ontology:cs_drift_state('ab75a102-5410-43dd-a013-2ad5b15a9195', contemporary_platform_scale_wikipedia, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ab75a102-5410-43dd-a013-2ad5b15a9195', '').
narrative_ontology:cs_kernel_id(notability_guidelines__inclusionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, established_wikipedia_editor_corps).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, legacy_media_publishers).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, indigenous_knowledge_holders).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, diaspora_community_historians).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, global_south_local_topic_editors).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, oral_tradition_documentarians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, wikimedia_foundation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and enforces WP:N through AfD (Articles for Deletion) discussions, drawing on years of accumulated procedural fluency and citation of prior precedent. Their own editing patterns and topic interests are already well-served by existing reliable-source coverage (English-language print and broadcast media), so the guideline rarely threatens articles they care about. They can invoke the guideline against topics they find unfamiliar or uninteresting without needing to argue the underlying merits.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, established_wikipedia_editor_corps, agenda_setter,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__inclusionist_reading, established_wikipedia_editor_corps, beneficiary).

% Newspapers, university presses, and broadcast archives whose past coverage decisions retroactively become the sole admissible evidence of a subject's worth. Every topic they historically covered clears the bar automatically; every topic they never covered (because it was never profitable, safe, or legible to them to cover) stays excluded. They neither administer nor lobby the guideline — they simply sit as its unearned reference standard.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, institutional_knowledge_producers, beneficiary,
    institutional, civilizational, arbitrage, global).

% Hold oral histories, place-name knowledge, and community genealogies that were never recorded by the newspapers and presses the guideline treats as 'reliable.' When they attempt to document this knowledge on Wikipedia, articles are routinely deleted for lacking 'significant coverage in reliable secondary sources' — sourcing that colonial-era and present-day media never produced because it never covered them. Leaving the platform means the knowledge simply has no encyclopedic presence anywhere with comparable reach.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, indigenous_knowledge_holders, payer,
    powerless, civilizational, trapped, regional).

% Document community institutions, local leaders, and events within diaspora communities that are covered extensively in community-language newspapers and radio but rarely in outlets the guideline's practice treats as sufficiently 'reliable' or 'independent.' They can appeal deletions through AfD but must do so in the procedural dialect of established editors, in English, using unfamiliar policy shorthand, at high time cost relative to any single article's stakes.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, diaspora_community_historians, payer,
    powerless, generational, constrained, regional).

% Edit articles about local politicians, businesses, cultural institutions, and events in countries with thin independent press infrastructure. Their articles face disproportionate deletion nomination rates because the sourcing base the guideline demands presupposes a press ecosystem that does not exist at the same density everywhere. They can fork content to other wikis, but those forks carry a fraction of Wikipedia's visibility and search primacy.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, global_south_local_topic_editors, payer,
    moderate, biographical, constrained, global).

% Attempt to transcribe and cite oral tradition as a primary or community-attested source. The guideline's operative practice treats oral sources as inherently unreliable or unverifiable relative to print, so their contributions are deleted or merged into obscurity regardless of the epistemic rigor of the oral tradition itself. There is no alternative venue with Wikipedia's reach where this knowledge could achieve comparable visibility.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, oral_tradition_documentarians, payer,
    powerless, civilizational, trapped, local).

% Search for information on topics that were deleted or never created due to notability enforcement. They are not part of the AfD conversation and never learn that an article existed, was deleted, or that the absence reflects a sourcing-standard artifact rather than the topic's actual unimportance. Their non-encounter with the gap is exactly what makes the gap invisible.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, readers_seeking_marginalized_topics, excluded,
    powerless, immediate, constrained, global).

% Sets platform-wide policy latitude but delegates notability enforcement to community consensus, benefiting from the guideline's function as a liability-limiting, quality-signaling mechanism that sustains Wikipedia's reputation for reliability and its resulting traffic and donation base, without directly authoring or defending individual deletion outcomes.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikimedia_foundation, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__inclusionist_reading, wikimedia_foundation, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__inclusionist_reading, diffuse).
narrative_ontology:fixing_cost_class(notability_guidelines__inclusionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: WP:N genuinely solves a real coordination problem: without SOME threshold for inclusion, an open-editing encyclopedia would be overwhelmed by vanity pages, promotional content, and unverifiable claims, making the whole corpus untrustworthy and unmaintainable at scale.
% TRANSFER_FUNCTION: The guideline transfers encyclopedic visibility and permanence from communities whose knowledge was never mediated through the legacy press (indigenous, diasporic, Global South, oral) to communities and institutions whose history already was — converting a pre-existing historical asymmetry in who got covered into a present-day asymmetry in who gets remembered.
% ABSENT_VOICES: Deleted-article authors from marginalized communities rarely participate in AfD discussions past the first attempt — procedural fluency, language, and time-zone/labor asymmetries keep them out of the room where the guideline's boundaries are actually negotiated. Readers who never encounter the missing article are structurally absent by definition.
% DISAPPEARANCE_RATIONALE: Established editors and institutional beneficiaries would say the encyclopedia's reliability collapses without the guideline (world_rearranges toward chaos). Excluded communities would say the world barely changes for them either way — their knowledge already has no presence, and its absence would just become more visible rather than newly created. The contest between these two claims is the constraint itself.
% FOUNDING_PROBLEM: Early Wikipedia faced a flood of unverifiable, promotional, and vanity content that threatened to make the entire project unusable and unciteable; notability was adopted as a proxy for 'has this been vetted by someone other than the subject.'
% FOUNDING_PROBLEM_CORROBORATION: Wikimedia Foundation research and academic studies of Wikipedia's systemic bias (e.g. gender gap and Global South coverage gap literature, produced by researchers outside the editor corps and outside the Foundation's PR function) corroborate that the sourcing-reliance mechanism reproduces pre-existing media coverage asymmetries rather than measuring topic importance neutrally. No corroboration from outside the beneficiary set has been found for the claim that the guideline's operative practice (as opposed to its stated text) achieves neutral quality filtering across regions and knowledge traditions.
narrative_ontology:disappearance_verdict(notability_guidelines__inclusionist_reading, contested).
narrative_ontology:founding_problem_status(notability_guidelines__inclusionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__inclusionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored substantial (0.71 at interval end) and rising: as Wikipedia's coverage matured and its search-engine primacy grew, the cost of exclusion from it rose in tandem, deepening the asymmetry the guideline enforces. Suppression (0.68) reflects the guideline's coercive force — deletion is not advisory, it is executed, and appeal requires procedural capital concentrated among established editors. Theater ratio (0.42) captures a meaningful and growing performative layer: AfD discussions increasingly cite 'significant coverage' as a settled technical criterion rather than acknowledging the substantive judgment call about whose sources count as reliable that the guideline's application always involves. All three metrics share one time grid (T0-T20) so no metric's rising trajectory is an artifact of endpoint substitution.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter/beneficiary seat, WP:N is coordination working as intended — filtering low-quality content, maintaining institutional trust. From the payer seats, structurally identical citation requirements operate as an exclusion mechanism keyed to a historical accident (who the legacy press covered) rather than to any property of the knowledge itself. The engine should compute these as genuinely different seat-level classifications from the same structural data, not as a difference of opinion about one classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional knowledge producers and the established editor corps sit near the beneficiary end of directionality: their historical/present coverage patterns already satisfy the guideline, so it costs them nothing and protects the value of their existing contributions from dilution. Indigenous knowledge holders, diaspora historians, and oral tradition documentarians sit near the full-target end: trapped exit options (no comparably visible alternative venue), and the guideline's operative practice structurally cannot be satisfied by the kind of documentation their knowledge actually has. Global-south local editors sit closer to target than beneficiary despite moderate power, because their structural problem (thin local press) cannot be solved by individual effort or procedural skill.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing an unverifiable-content flood) is genuinely still partly live — this is why the reading is authored as contested rather than a clean dead-mandate case. But the guideline's operative practice has drifted from 'is this verifiable at all' toward 'was this covered by institutions this community already trusts,' which imports historical media coverage bias wholesale rather than filtering for verifiability as such. The classification as snare (under this reading) reflects that the coordination story, while not fabricated, has become load-bearing cover for an extraction pattern the guideline's authors did not design but its operative practice reliably produces.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Is WP:N''s operative effect on marginalized knowledge an intrinsic property of any notability threshold applied to an unequal media landscape, or a contingent artifact of how THIS guideline''s text and enforcement culture have evolved?',
    'Compare outcomes across the three sibling readings (deletionist, deliberative, inclusionist) and across alternative sourcing-inclusion policies (e.g. WikiProjects that formally admit oral/indigenous sourcing) to see whether exclusion rates track guideline design choices or track the underlying media asymmetry regardless of design.',
    'If exclusion tracks guideline design, this reading''s extraction figure is addressable by policy reform and the tangled_rope frame becomes more apt; if it tracks the underlying media asymmetry regardless of design, the guideline is closer to an unavoidable proxy for a problem it did not create, weakening the snare classification toward tangled_rope or even mountain-adjacent framing of the underlying asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the exclusion effect is intrinsic to any notability threshold or specific to this guideline''s design and enforcement culture.').

omega_variable(
    sibling_reading_divergence_location,
    'Where exactly do the deletionist and deliberative readings locate the disagreement with this inclusionist reading — is it about the FACTS of exclusion rates, the NORMATIVE weight of exclusion versus quality-control benefit, or the EMPIRICAL claim about whether AfD deliberation is genuinely open to revising notability boundaries over time?',
    'Structured comparison of the three constraint files'' beneficiary/victim declarations, ε values, and requires_active_enforcement flags; a genealogical study of AfD outcome patterns for marginalized-topic articles over multiple guideline revision cycles would separate the deliberative reading''s ''boundaries evolve'' claim from this reading''s ''boundaries are fixed exclusion'' claim empirically.',
    'If AfD outcomes for marginalized topics show measurable improvement over guideline revision cycles, the deliberative reading gains support and this reading''s snare classification should be read as a snapshot rather than a stable structural verdict; if outcomes are flat or worsening despite guideline text revisions, this reading''s structural-apparatus framing is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_divergence_location, empirical, 'Locating the specific structural disagreement between the three sibling readings of the notability_guidelines kernel.').

omega_variable(
    reliable_source_definition_contestation,
    'Is the definition of ''reliable secondary source'' itself a neutral epistemic standard, or does it encode a specific (Western, print/broadcast-centric) theory of knowledge verification that structurally cannot be satisfied by oral, community, or indigenous documentary traditions regardless of their internal rigor?',
    'Comparative epistemology analysis of how community-attested oral tradition achieves verification and consensus internally, set against Wikipedia''s sourcing policy text and its practiced application in AfD precedent.',
    'If reliable-source practice is shown to structurally exclude entire verification traditions rather than merely being underused by them, this substantially strengthens the snare classification under this reading and weakens the deletionist reading''s ''neutral quality filter'' framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliable_source_definition_contestation, conceptual, 'Whether the reliable-source standard is a neutral epistemic filter or encodes a specific and non-universal verification tradition.').


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
narrative_ontology:measurement(nota_tr_t8, notability_guidelines__inclusionist_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(nota_tr_t12, notability_guidelines__inclusionist_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(nota_tr_t16, notability_guidelines__inclusionist_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__inclusionist_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__inclusionist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(nota_be_t4, notability_guidelines__inclusionist_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(nota_be_t8, notability_guidelines__inclusionist_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(nota_be_t12, notability_guidelines__inclusionist_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(nota_be_t16, notability_guidelines__inclusionist_reading, base_extractiveness, 16, 0.68).
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
% This constraint is one of three sibling readings of the shared notability_guidelines kernel, decomposed per the epsilon-invariance principle: deletionist_reading (necessary epistemic quality filter; low authored extraction), deliberative_reading (AfD as evolving negotiation; moderate authored extraction with strong coordination framing), and this inclusionist_reading (structural gatekeeping apparatus; high authored extraction, snare classification). Each carries its own epsilon and its own beneficiary/victim structure rather than a shared measurement of one constraint under different observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
