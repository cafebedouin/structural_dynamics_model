% ============================================================================
% CONSTRAINT STORY: notability_guidelines__inclusionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Wikipedia Notability Guidelines as Structural Gatekeeping (Inclusionist Reading)
 *   domain: digital_commons/knowledge_infrastructure/platform_constitutionalism
 *
 * SUMMARY:
 *   The Wikipedia Notability Guidelines represent a critical node in the
 *   digital commons where epistemological authority is negotiated. The
 *   inclusionist reading examines these guidelines as a structural
 *   gatekeeping apparatus that systematically privileges knowledge produced
 *   through institutional channels—academic publishing, mainstream media,
 *   corporate research—while excluding knowledge systems based on oral
 *   transmission, grassroots documentation, indigenous epistemologies, and
 *   non-Western intellectual traditions. The constraint operates by requiring
 *   that all article subjects meet 'notability' criteria, which are
 *   operationalized through 'reliable sources'—a category that maps almost
 *   precisely onto Western institutional publication networks. This reading
 *   argues that the notability gate is not a neutral coordination mechanism
 *   for managing Wikipedia's editorial capacity, but rather a leveraged
 *   extraction mechanism that allows institutional knowledge producers to
 *   control which knowledge enters the global commons. The tension is
 *   genuine: Wikipedia does need verification standards, but the current
 *   standards encode institutional privilege as epistemology. Over the
 *   10-year interval modeled here (2015–2025), extractiveness has risen from
 *   0.52 to 0.68, and suppression has intensified from 0.65 to 0.72,
 *   reflecting increasing marginalization of non-institutional sources as
 *   Wikipedia professionalized its editorial processes and institutional
 *   participation increased.
 *
 * KEY AGENTS:
 *   - Marginalized Knowledge Communities (powerless/trapped): Indigenous communities, Global South researchers, oral historians, grassroots organizers whose knowledge lacks institutional publication pathways. Bear the full cost of the notability gate.
 *   - Academic Publishing Establishment (institutional/arbitrage): Universities, academic journals, peer-review networks. Benefit from notability rules that require academic sourcing. Control the secondary sources Wikipedia depends on.
 *   - Mainstream Media Networks (institutional/arbitrage): News organizations, major publications. Secondary sources under notability rules; benefit from gatekeeping that requires media coverage for notability.
 *   - Wikipedia Editors and Contributors (moderate/constrained): Thousands of volunteer editors globally. Constrained by notability guidelines they enforce; benefit from coordination function of shared standards. Experience tension between inclusivity values and enforcement rules.
 *   - Inclusionist Reform Coalition (organized/mobile): WikiProject Feminism, WikiData initiatives, marginalized-community-focused editing projects, oral history archives. Building alternative pathways (Wikidata community consensus, domain-practitioner peer review, grassroots verification). Mobile—actively moving toward exit from current constraint.
 *   - Deletionist Advocates (institutional/arbitrage): Wikipedia editors and administrators who enforce strict notability standards. See guidelines as essential quality control. Primary enforcers of the gatekeeping mechanism.
 *   - Analytical Observer (analytical/analytical): System-level perspective recognizing both the real coordination function Wikipedia serves and the extractive gatekeeping embedded in its current institutional design.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, 0.68).
domain_priors:suppression_score(notability_guidelines__inclusionist_reading, 0.72).
domain_priors:theater_ratio(notability_guidelines__inclusionist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__inclusionist_reading, snare).
narrative_ontology:human_readable(notability_guidelines__inclusionist_reading, "Wikipedia Notability Guidelines as Structural Gatekeeping (Inclusionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__inclusionist_reading, "digital_commons/knowledge_infrastructure/platform_constitutionalism").

domain_priors:requires_active_enforcement(notability_guidelines__inclusionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__inclusionist_reading, 'd7e3f1a2-9b4c-4e7f-a1c5-2d8b6f9e3c1a').
narrative_ontology:cs_kernel_codification('d7e3f1a2-9b4c-4e7f-a1c5-2d8b6f9e3c1a', formalized).
narrative_ontology:cs_authority_grounding('d7e3f1a2-9b4c-4e7f-a1c5-2d8b6f9e3c1a', extraction).
narrative_ontology:cs_interpretation_layer_present('d7e3f1a2-9b4c-4e7f-a1c5-2d8b6f9e3c1a').
narrative_ontology:cs_reading_relation('d7e3f1a2-9b4c-4e7f-a1c5-2d8b6f9e3c1a', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7e3f1a2-9b4c-4e7f-a1c5-2d8b6f9e3c1a', notability_guidelines__deliberative_reading, influences).
narrative_ontology:cs_axiom('d7e3f1a2-9b4c-4e7f-a1c5-2d8b6f9e3c1a', foundational, non_institutional_knowledge_epistemically_valid).
narrative_ontology:cs_axiom_status(non_institutional_knowledge_epistemically_valid, holdable).
narrative_ontology:cs_axiom_grounding('d7e3f1a2-9b4c-4e7f-a1c5-2d8b6f9e3c1a', non_institutional_knowledge_epistemically_valid, empirically_contingent).
narrative_ontology:cs_axiom('d7e3f1a2-9b4c-4e7f-a1c5-2d8b6f9e3c1a', foundational, reliable_sources_encodes_institutional_privilege).
narrative_ontology:cs_axiom_status(reliable_sources_encodes_institutional_privilege, holdable).
narrative_ontology:cs_axiom_grounding('d7e3f1a2-9b4c-4e7f-a1c5-2d8b6f9e3c1a', reliable_sources_encodes_institutional_privilege, empirically_contingent).
narrative_ontology:cs_reference_frame('d7e3f1a2-9b4c-4e7f-a1c5-2d8b6f9e3c1a', universal_knowledge_commons_epistemically_pluralist).
narrative_ontology:cs_drift_state('d7e3f1a2-9b4c-4e7f-a1c5-2d8b6f9e3c1a', contemporary_institutional_consolidation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d7e3f1a2-9b4c-4e7f-a1c5-2d8b6f9e3c1a', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(notability_guidelines__inclusionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, western_academic_publishing_establishment).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, mainstream_media_sourcing_networks).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, marginalized_communities).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, global_south_knowledge_systems).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, oral_traditions_and_indigenous_epistemologies).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, grassroots_organizing_records).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, underrepresented_biographical_subjects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED KNOWLEDGE COMMONS (SNARE) — Trapped within an infrastructure that systematically excludes knowledge not sourced through institutional publishing networks. Indigenous ecological knowledge, grassroots historical records, and non-Western intellectual traditions cannot accumulate in Wikipedia without secondary coverage in 'reliable sources' (academic journals, mainstream media). No exit available: the notability gate closes the alternative pathways to knowledge legitimacy. Maximum extraction experienced — the entire epistemic commons of marginalized groups is suppressed.
constraint_indexing:constraint_classification(notability_guidelines__inclusionist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ACADEMIC PUBLISHING ESTABLISHMENT (ROPE) — Benefits from notability guidelines that reinforce academic publishing as the canonical epistemology. Wikipedia's dependence on peer-reviewed journals, mainstream media, and institutional sources creates a feedback loop: notability rules require secondary sourcing, which privileges institutions that produce published scholarship. This is experienced as coordination—Wikipedia is legitimately building reliable knowledge infrastructure—but the coordination strongly favors institutional knowledge producers. Net beneficiary with exit options (can operate outside Wikipedia; controls the source material Wikipedia depends on).
constraint_indexing:constraint_classification(notability_guidelines__inclusionist_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: WIKIPEDIA EDITORS AND CONTRIBUTORS (TANGLED ROPE) — Constrained by the notability guidelines they themselves enforce; also benefit from the coordination function of having standardized inclusion criteria. Editors experience genuine coordination (shared rules enable collaborative knowledge-building) alongside extraction (the rules prevent them from including knowledge they recognize as legitimate). Cost is career effort and epistemic frustration; benefit is participation in a major knowledge commons. Exit is possible but costly (spending years rebuilding Wikipedia's notability criteria).
constraint_indexing:constraint_classification(notability_guidelines__inclusionist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INCLUSIONIST REFORM COALITION (SCAFFOLD) — Organized agents (WikiProject Feminism, Wikidata, WikiData Commons, marginalized-community-focused editing projects) see notability guidelines as a temporary coordination failure with an explicit sunset trajectory. The coalition is building alternative verification pathways: Wikidata permits statements with multiple-source agreement regardless of 'reliable sources'; WikiProjects create local notability standards; community-sourced oral history archives are establishing parallel epistemic legitimacy. These are structured as transitional until the normative shift (non-institutional knowledge as valid) becomes mainstream. Low effective extraction because this coalition has agency and exit momentum.
constraint_indexing:constraint_classification(notability_guidelines__inclusionist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: NOTABILITY ENFORCEMENT APPARATUS (PITON) — The deletion and source-policing mechanisms persist through institutional inertia despite eroding functional justification. As Wikipedia editors increasingly recognize that 'reliable sources' is a colloquial code for 'Western academic and corporate institutions,' the original coordination function (ensuring verifiability through institutional citation) has degraded into pure gate-maintenance. Theater_ratio is moderate-high (0.58) because enforcement rhetoric still invokes verifiability and community consensus, but actual practice is institutional gatekeeping. The apparatus is maintained because alternatives haven't fully replaced it and because the institutional beneficiaries have incentive to sustain the gate.
constraint_indexing:constraint_classification(notability_guidelines__inclusionist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — From a civilizational/universal framing, the notability gate appears as an immutable law of knowledge commons: 'Any knowledge system must have verification standards; institutional publication is the only scalable verification mechanism.' This perspective naturalizes the constraint as an inherent feature of collective knowledge infrastructure. However, the structural data contradicts the mountain classification: identifiable beneficiaries exist (academic publishing, institutional knowledge producers), victims are designated (marginalized knowledge systems), and enforcement is active (deletion, source-checking). The false-summit signature fires — this is a manufactured institutional arrangement naturalizing itself as law.
constraint_indexing:constraint_classification(notability_guidelines__inclusionist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__inclusionist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(notability_guidelines__inclusionist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(notability_guidelines__inclusionist_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(notability_guidelines__inclusionist_reading, TR),
    TR >= 0.70.

:- end_tests(notability_guidelines__inclusionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The notability gate systematically extracts epistemic authority from non-institutional knowledge systems. The measurement rising from 0.52 to 0.68 reflects increasing institutional consolidation: as Wikipedia professionalized (hired staff, formalized policies, increased corporate partnership), the gatekeeping function intensified. Institutional beneficiaries (academic publishers, mainstream media) consolidate control over which knowledge enters the global commons. The constraint is not marginal—it shapes what 7 billion people can access as 'known fact' on the world's primary reference platform. Suppression (0.72): High. Multiple mechanisms operate: (1) The deletion mechanism itself—non-notable articles are systematically removed; (2) Source rejection—articles written with legitimate sources outside the 'reliable sources' hierarchy are flagged and deleted; (3) Epistemic closure—editors become institutionalized into 'reliable sources' thinking, preventing recognition of alternative verification methods; (4) Practical barriers—marginalized communities lack institutional publishing infrastructure to generate the secondary coverage required for notability. Exit from this suppression is genuinely difficult: no alternative platform rivals Wikipedia's reach, and building parallel infrastructure at scale requires resources marginalized communities often lack. Theater ratio (0.58): Moderate. The enforcement rhetoric invokes verifiability, quality control, and community consensus—all legitimate-sounding coordination principles. But actual practice reveals institutional gatekeeping: articles on marginalized subjects are deleted not primarily because they are unverifiable, but because their verification comes from non-institutional sources. The performative content is the invocation of 'community standards' while the actual mechanism is institutional source hierarchy. Theater has increased slightly over the interval as enforcement language has professionalized while underlying gatekeeping logic remained constant.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies how the same structural arrangement appears as coordination to beneficiaries and extraction to victims. The academic publishing establishment legitimately sees Wikipedia's notability standards as reasonable coordination—'we need verification standards, and institutional publication is a scalable mechanism.' This is their honest perspective because they control institutional publication. Marginalized communities see gatekeeping—'our knowledge is excluded because it's not institutional, not because it's unverifiable.' Both perspectives are structurally real. The gap reveals the constraint is neither pure coordination (would appear the same to all perspectives) nor pure extraction (victims would have no legitimate coordination function to point to). It is hybrid: the coordination function is real and benefits everyone, but the specific implementation asymmetrically extracts from those outside institutional networks.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (chi) is computed from: (1) base extractiveness (0.68) times (2) the directionality sigmoid f(d) based on the agent's power, exit options, and beneficiary/victim status, times (3) the spatial scope modifier (1.2 for global). Marginalized communities trapped without institutional publishing access have d ≈ 0.95 (full target), f(d) ≈ 1.42, giving experienced chi ≈ 0.68 × 1.42 × 1.2 ≈ 1.16 (high effective extraction). Academic publishers with arbitrage options have d ≈ 0.08 (near beneficiary), f(d) ≈ -0.15, giving experienced chi ≈ 0.68 × (-0.15) × 1.2 ≈ -0.12 (subsidy—the constraint subsidizes them). Wikipedia editors constrained but benefiting have d ≈ 0.65, f(d) ≈ 1.00, giving experienced chi ≈ 0.68 × 1.00 × 1.2 ≈ 0.82 (moderate extraction). Inclusionist reformers with organization and exit mobility have d ≈ 0.48, f(d) ≈ 0.60, giving experienced chi ≈ 0.68 × 0.60 × 1.2 ≈ 0.49 (low-moderate extraction). The beneficiary's negative chi is key—they don't experience extraction because they benefit from the constraint's design.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint resolves the apparent contradiction between 'coordination mechanism' and 'extractive gatekeeping' by showing that both are structurally true. The inclusionist reading establishes that notability standards serve a genuine coordination function (establishing verification norms) while simultaneously enabling asymmetric extraction (institutional gatekeeping). This is the defining signature of Snare that masquerades as Rope. The mandatrophy resolves when we recognize: (1) The coordination function is real but not sufficient to justify the specific design. (2) The design was not inevitable—it emerged from institutional actors' structural position. (3) Alternative designs (Wikidata's permissiveness, community consensus models, domain practitioner peer review) preserve coordination while reducing extraction. (4) The beneficiaries (academic publishers) have incentive to defend the current design precisely because it extracts in their favor. Institutional observers may legitimately see Rope (they benefit and experience low extraction); marginalized observers correctly see Snare (they are trapped and experience maximum extraction). The constraint type is Snare because suppression (0.72) is high and exit options are genuinely limited for marginalized knowledge systems. The theater ratio (0.58) reflects moderate performative content—enforcement language invokes community standards while actual mechanism is institutional gatekeeping. The mandatrophy question ('Is this coordination or extraction?') resolves to: it is institutionalized extraction disguised as necessary coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reliable_sources_epistemological_definition,
    'Is ''reliable sources'' an epistemologically neutral verification criterion, or does it encode institutional privilege as epistemology?',
    'Comparative analysis of Wikipedia''s verification success rates across knowledge domains. Correlation between source type (academic journal vs. grassroots documentation vs. oral testimony) and long-term survival of articles. Cross-platform comparison: How do non-Western Wikipedias resolve this? Do they produce lower-quality knowledge when using localized source hierarchies?',
    'If neutral verification: notability rules produce real epistemic benefit and constraint should reclassify toward Rope. If institutional encoding: rules are extractive gatekeeping and current Snare classification is correct. If mixed: classification shifts to Tangled Rope, indicating both genuine coordination and asymmetric extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reliable_sources_epistemological_definition, empirical, 'Whether ''reliable sources'' criterion is epistemically neutral or institutionally encoded').

omega_variable(
    marginalized_knowledge_system_alternative_pathways,
    'Are alternative verification pathways (Wikidata community consensus, grassroots oral archives, peer review by domain practitioners) actually producing knowledge at sufficient quality and scale to replace institutional sourcing?',
    'Longitudinal tracking of articles created via alternative pathways; citation networks and reuse rates; comparison of correction/deletion rates between institutionally-sourced and community-verified articles across 5-10 year windows.',
    'If pathways are functional: Scaffold perspective is real, not aspirational. If failing: marginalized communities remain trapped regardless of organizational efforts. If partially functional: Tangled Rope classification more accurate than Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_knowledge_system_alternative_pathways, empirical, 'Whether alternative verification pathways can replace institutional sourcing at scale').

omega_variable(
    kernel_reading_contestation_point,
    'Is the notability gate a coordination mechanism for managing Wikipedia''s finite editorial capacity and quality control, or is it primarily a gatekeeping mechanism that extracts epistemic authority from non-institutional knowledge systems?',
    'Analysis of deletion patterns: Are articles deleted primarily because they are unverifiable (suggest Rope reading) or because their sources are non-institutional (support Snare reading)? Historical examination of notability criteria evolution: Do they track capacity constraints (would support coordination framing) or track institutional privilege consolidation (support extraction framing)?',
    'Deletionist reading frames this as legitimate quality control; inclusionist reading (this constraint) frames it as extraction. The resolution determines which reading''s core premise holds. If deletionist is right, this constraint should be Rope or Tangled Rope. If inclusionist is right, Snare is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation_point, conceptual, 'Whether notability gate serves coordination or gatekeeping function').

omega_variable(
    marginalized_knowledge_system_epistemology_mismatch,
    'Does the marginalized knowledge system being excluded (oral tradition, indigenous ecological knowledge, grassroots history) have built-in verification mechanisms that the Wikipedia process simply cannot recognize because they operate on different epistemic principles?',
    'Ethnographic documentation of knowledge validation practices in excluded systems. Comparison of error rates and correction mechanisms in institutional vs. non-institutional knowledge production. Analysis of Wikipedia editor feedback on rejected knowledge: do patterns show epistemic mismatch or simply rejection of non-institutional sources?',
    'If epistemic mismatch: the exclusion may be necessary but should be framed as constraint between incompatible systems rather than gatekeeping of marginalized by institutional. If simple source rejection: gatekeeping is primary mechanism. This shapes mandatrophy analysis—is the constraint a necessary coordination sacrifice or pure extraction?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_knowledge_system_epistemology_mismatch, empirical, 'Whether excluded knowledge systems have incommensurable epistemology or are simply systematically rejected').

omega_variable(
    institutional_knowledge_producer_incentive_structure,
    'Do institutional knowledge producers (academic publishers, mainstream media) actively defend notability standards to maintain their epistemic monopoly, or do they remain indifferent to Wikipedia''s knowledge gatekeeping?',
    'Analysis of institutional responses to Wikipedia citation norms: Do academic publishers incentivize being cited in Wikipedia? Do media organizations optimize for Wikipedia-sourcing potential? Examination of institutional lobby efforts on Wikipedia policy: Are any organizations actively defending notability standards? Historical evolution of institutional publishing relative to Wikipedia''s notability timeline.',
    'If active defense: beneficiary status is confirmed and extraction mechanism is coordinated. If indifference: beneficiaries are passive and extraction is epiphenomenal to other institutional dynamics. If mixed: beneficiaries coordinate on some axes but not others.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_knowledge_producer_incentive_structure, empirical, 'Whether institutional knowledge producers actively defend notability gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__inclusionist_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_incl_theater_t0, notability_guidelines__inclusionist_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(nota_incl_theater_t5, notability_guidelines__inclusionist_reading, theater_ratio, 5, 0.53).
narrative_ontology:measurement(nota_incl_theater_t10, notability_guidelines__inclusionist_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(nota_incl_extract_t0, notability_guidelines__inclusionist_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(nota_incl_extract_t5, notability_guidelines__inclusionist_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(nota_incl_extract_t10, notability_guidelines__inclusionist_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(nota_incl_suppress_t0, notability_guidelines__inclusionist_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(nota_incl_suppress_t5, notability_guidelines__inclusionist_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(nota_incl_suppress_t10, notability_guidelines__inclusionist_reading, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__inclusionist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(notability_guidelines__inclusionist_reading, 0.12).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, notability_guidelines__deletionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, notability_guidelines__deliberative_reading).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, wikipedia_article_deletion_mechanism).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, institutional_publishing_hegemony).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, global_south_knowledge_marginalization).

% DUAL FORMULATION NOTE:
% The notability guidelines kernel has three readings as separate constraint stories, each with its own epsilon and classification. The inclusionist reading (this constraint) treats notability as extractive gatekeeping (Snare, ε=0.68). The deletionist reading treats it as legitimate quality control (Rope or Tangled Rope, lower ε). The deliberative reading treats it as a coordination problem solvable through localized standards (Scaffold, moderate ε). Each reading generates different prescriptions for reform. All three link via network.affects_constraints to acknowledge they are readings of the same kernel, not independent constraints. The ε values differ because the readings operationalize 'what the constraint does' differently: inclusionist measures institutional gatekeeping; deletionist measures verification failure rates; deliberative measures editorial capacity constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(notability_guidelines__inclusionist_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
