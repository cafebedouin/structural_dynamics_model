% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__extraction_permissive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__extraction_permissive, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__extraction_permissive
 *   human_readable: Article II Non-Appropriation (Extraction-Permissive Reading): Resource Access by Technological Capability
 *   domain: international_law/space_law/commons_governance
 *
 * SUMMARY:
 *   Article II of the Outer Space Treaty (1967) declares that 'outer space,
 *   including the moon and other celestial bodies, is not subject to national
 *   appropriation by claim of sovereignty, by means of use or occupation, or
 *   by any other means.' The extraction-permissive reading interprets this
 *   prohibition narrowly: it bars sovereign territorial claims but permits
 *   private actors, licensed by their flag-states, to extract and own
 *   resources. Under this reading, a corporation mining the Moon for
 *   rare-earth elements does not appropriate territory (Article II compliant)
 *   but does appropriate the resources themselves (permitted because Article
 *   II mentions only territorial claims). This reading is held by advanced
 *   spacefaring states (U.S., Luxembourg, UAE in recent national legislation)
 *   and the corporate space-extraction sector. It is contested by
 *   developing-world states, space law scholars advancing conservation and
 *   regime readings, and indigenous-rights advocates, who argue the reading
 *   re-encodes colonialism at the interplanetary scale: technological access
 *   becomes the new gating mechanism for resource capture, disadvantaged
 *   states are permanently excluded, and no compensation mechanism exists for
 *   non-extracting parties. The constraint is the interpretive standard
 *   itself—the reading that permission exists—and the enforcement machinery
 *   (flag-state licensing, corporate property claims, absence of
 *   international override authority) that holds it in place.
 *
 * KEY AGENTS:
 *   - technologically_advanced_spacefaring_states: Institutional power, set the interpretive standard through technical achievement and unilateral licensing regimes; benefit from corporate tax revenue and geopolitical influence.
 *   - private_space_extraction_corporations: Powerful actors with mobile options; own extracted resources outright under this reading; lobby for permissive interpretations and favorable flag-state environments.
 *   - technologically_disadvantaged_states: Moderate power, formally equal under Article II but materially excluded from extraction; trapped by lack of capital and technical capacity; no compensation mechanism.
 *   - indigenous_and_developing_world_populations: Powerless, identity-locked into representation through weak state structures and global-justice commitments; see resources as common heritage but have no enforcement voice.
 *   - space_law_scholarship_community: Analytical observer; debates the reading across empirical (does Article II text permit extraction?) and normative (should it?) axes; non-unanimous.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, 0.78).
domain_priors:suppression_score(ost_article_ii_non_appropriation__extraction_permissive, 0.71).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__extraction_permissive, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, extractiveness, 0.78).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__extraction_permissive, snare).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__extraction_permissive, "Article II Non-Appropriation (Extraction-Permissive Reading): Resource Access by Technological Capability").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__extraction_permissive, "international_law/space_law/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__extraction_permissive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__extraction_permissive, 'd581daa6-ba23-4a37-a3d5-dd72f4fbe06a').
narrative_ontology:cs_kernel_codification('d581daa6-ba23-4a37-a3d5-dd72f4fbe06a', fixed_text).
narrative_ontology:cs_authority_grounding('d581daa6-ba23-4a37-a3d5-dd72f4fbe06a', extraction).
narrative_ontology:cs_interpretation_layer_present('d581daa6-ba23-4a37-a3d5-dd72f4fbe06a').
narrative_ontology:cs_reading_relation('d581daa6-ba23-4a37-a3d5-dd72f4fbe06a', ost_article_ii_non_appropriation__commons_conservation, coexists_with).
narrative_ontology:cs_reading_relation('d581daa6-ba23-4a37-a3d5-dd72f4fbe06a', ost_article_ii_non_appropriation__international_regime, influences).
narrative_ontology:cs_axiom('d581daa6-ba23-4a37-a3d5-dd72f4fbe06a', foundational, article_ii_bars_sovereignty_not_ownership).
narrative_ontology:cs_axiom_status(article_ii_bars_sovereignty_not_ownership, holdable).
narrative_ontology:cs_axiom_grounding('d581daa6-ba23-4a37-a3d5-dd72f4fbe06a', article_ii_bars_sovereignty_not_ownership, conventional).
narrative_ontology:cs_axiom('d581daa6-ba23-4a37-a3d5-dd72f4fbe06a', secondary, private_property_efficient_resource_allocation).
narrative_ontology:cs_axiom_status(private_property_efficient_resource_allocation, holdable).
narrative_ontology:cs_axiom_grounding('d581daa6-ba23-4a37-a3d5-dd72f4fbe06a', private_property_efficient_resource_allocation, empirically_contingent).
narrative_ontology:cs_reference_frame('d581daa6-ba23-4a37-a3d5-dd72f4fbe06a', non_territorial_resource_access_framework).
narrative_ontology:cs_drift_state('d581daa6-ba23-4a37-a3d5-dd72f4fbe06a', contemporary_commercial_extraction_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d581daa6-ba23-4a37-a3d5-dd72f4fbe06a', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, technologically_advanced_spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, private_space_extraction_corporations).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, technologically_disadvantaged_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, future_generations_access_rights).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, indigenous_and_developing_world_populations).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, article_ii_non_appropriation_bars_sovereignty_only).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, private_resource_extraction_rights_coexist_with_non_appropriation).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, technological_capability_determines_access_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess the technical capacity and state resources to mount space extraction missions (lunar mining, asteroid resource collection, planetary surveys). Under this reading, they interpret Article II to permit private operators licensed under their flag to extract and own resources, provided no territorial sovereignty is claimed. They set the interpretive standard through technical achievement and flag-state regulation, effectively controlling who may extract and on what terms. They benefit directly through licensing fees, corporate tax revenue, and geopolitical position as the standard-setters.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, technologically_advanced_spacefaring_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, technologically_advanced_spacefaring_states, beneficiary).

% Extract resources (rare-earth elements, water ice, minerals) from celestial bodies under flag-state authorization. Own the extracted resources outright under this reading. Face no compensation obligation to excluded states or future collective benefit mechanisms. Their business model depends on the extraction-permissive reading; under conservation or regime readings their assets would face international claims or be subject to benefit-sharing mandates. Their exit is to shift operations to compliant flag-states or lobby for favorable interpretations.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, private_space_extraction_corporations, beneficiary,
    powerful, biographical, arbitrage, global).

% Lack the technical capacity and capital to mount independent space extraction missions or to regulate private operators under their own flag. Under the extraction-permissive reading, they have no claim on extraterrestrial resources extracted by advanced states' operators, no compensation mechanism, and no voice in standard-setting. They are formally equal under Article II (no territorial appropriation applies to them either) but materially excluded from benefit-capture. Their options are: form coalitions to demand regime change (politically costly, low leverage against powerful states), develop capacity over decades (capital-constrained), or accept exclusion.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, technologically_disadvantaged_states, payer,
    moderate, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, technologically_disadvantaged_states, excluded).

% Represented by disadvantaged states or civil-society coalitions. See extraterrestrial resources as the 'common heritage of mankind' (per UNCLOS framing, which inspired early OST debates). Under the extraction-permissive reading, they have no mechanism to claim future benefit, no voice in extraction standards, and are locked into accepting advanced states' de facto enclosure. Their identity-lock consists of representation through weak state structures and ideological commitments to global justice that make exit (accepting permanent exclusion) psychologically costly.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, indigenous_and_developing_world_populations, payer,
    powerless, generational, identity_locked, global).

% Debates the reading of Article II across three competing interpretations. Scholars advancing the extraction-permissive reading ground it in textual analysis (Article II bars territorial claims, not resource ownership) and property-rights efficiency arguments (private ownership incentivizes sustainable extraction). They observe the structural advantage this reading grants advanced states and acknowledge the redistribution question but frame it as a policy problem for future regime-building, not a constraint violation.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, space_law_scholarship_community, observer,
    analytical, biographical, analytical, global).

% A non-agent entity: future generations' interest in equitable access to space resources and in preservation of celestial bodies' scientific and exploratory value. Under the extraction-permissive reading, their interests are not represented in current decision-making; irreversible resource depletion or damage to scientific sites faces no legal constraint beyond whatever environmental law Earth-bound courts might apply (low enforcement, territorial bias).
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, future_generations_access_rights, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ost_article_ii_non_appropriation__extraction_permissive, future_generations_access_rights).

% The authorized institutional body for interpreting and evolving space law, but structurally excluded from enforcement authority by the consensus requirement in OST Article XI. Advanced spacefaring states effectively veto any regime that would restrict their extraction, making COPUOS a forum for debate rather than binding authority. Under the extraction-permissive reading, COPUOS's role is observational; it can recommend but not mandate a benefit-sharing mechanism absent unanimity (which advanced states withhold).
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, united_nations_committee_on_peaceful_uses_of_outer_space, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__extraction_permissive, private_space_extraction_corporations).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__extraction_permissive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Article II achieves a single planetary commitment: no state may claim territorial sovereignty over celestial bodies or their resources. This coordinate against a race to territorial annexation that would have fragmented space into nation-state-controlled enclaves and triggered great-power conflict. The extraction-permissive reading preserves this negative coordination (no one claims territory) while permitting private resource capture under flag-state authorization.
% TRANSFER_FUNCTION: Transfers resource ownership and extraction rights from the status of unowned common resources (under a conservation reading) or collective-humanity property (under a regime reading) to private actors licensed by technologically advanced states. Moves wealth concentration from disadvantaged states and future generations to the licensing states and their corporate operators. No explicit compensation mechanism; the transfer is enacted through interpretive fait accompli rather than treaty amendment.
% ABSENT_VOICES: Technologically disadvantaged states, indigenous populations, small island states, and future-generations advocates are formally present in COPUOS but structurally excluded from enforcement—the consensus rule gives veto power to advanced spacefaring states and shields the extraction-permissive reading from multilateral challenge. They would argue for benefit-sharing mechanisms, resource preservation requirements, or regime establishment; their exclusion is what the constraint's enforcement (flag-state authorization, lack of international review) maintains.
% DISAPPEARANCE_RATIONALE: If Article II were read under the conservation interpretation (extraction is appropriation and forbidden) or the regime interpretation (appropriation is deferred pending international framework), the entire structure of private extraction claims would collapse. Operators would lose ownership; states would lose licensing revenue; celestial resources would revert to common-heritage or collective-decision-making status. Geopolitical power would shift away from current spacefaring states. The constraint's disappearance would reorganize who may extract, on what terms, and to whose benefit.
% FOUNDING_PROBLEM: The founding problem (mid-20th century) was preventing territorial annexation of space and celestial bodies by spacefaring powers, which would have created a new arena for great-power conflict and excluded non-spacefaring nations from any interest in space development. Article II was written to prohibit territorial claims.
% FOUNDING_PROBLEM_CORROBORATION: Advanced spacefaring states and their corporate interests affirm the founding problem is solved (no state claims Mars or the Moon as territory, preventing annexation conflicts). Disadvantaged states, international law scholars advancing conservation and regime readings, and indigenous-rights advocates contest whether the problem is solved or merely displaced: the founding problem addressed territorial sovereignty; the extraction-permissive reading permits resource appropriation by private actors, replicating exclusion under a different legal form. UN COPUOS records and space law scholarship outside the advanced-states academy provide non-beneficiary corroboration of the contested status.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__extraction_permissive, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__extraction_permissive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__extraction_permissive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__extraction_permissive, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__extraction_permissive, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.15→0.78 over the interval) because the reading permits resource capture with no compensation, no international review, and no benefit-sharing mechanism—the extraction concentrates wealth in advanced states and their corporations. The measurement series tracks technological-capability acceleration: early decades saw theoretical debate only (Moon race era, minimal extraction capacity); by 2008 the first commercial lunar prospecting contracts were signed; by 2018-2026 multiple states passed national resource-ownership legislation and corporate mining projects advanced toward operational phases. Suppression is high and rising (0.25→0.71) because the extraction-permissive reading must be maintained against active resistance from disadvantaged states (UN votes, COPUOS proposals for benefit-sharing regimes, advocacy coalitions). The consensus rule in Article XI prevents binding multilateral override, so suppression takes the form of veto power: advanced states block regime-establishment negotiations and enforce their preferred reading through unilateral action and corporate licensing. Theater is rising (0.05→0.42) because the constraint increasingly relies on legitimacy narratives (property-rights efficiency, private investment incentivizing sustainable extraction, technological advancement as universal benefit) that mask the redistribution function. The coercion grid shows how suppression operates differently at different social levels: at the structural level (system-level rule-making), advanced states maintain enforcement capacity through COPUOS control and veto; at the organizational level (state coalitions and corporate regulation), suppression intensifies because disadvantaged-state coalitions are forming and must be managed; at the class level (developing vs. developed states), suppression stays high but resistance also persists; at the individual level (scientists, indigenous communities, activists), suppression is lower but resistance is muted by lack of institutional power.
 *
 * PERSPECTIVAL GAP:
 *   From the advanced-states and corporate seats, the extraction-permissive reading is legitimate coordination: Article II prohibits territorial claims (achieved), and private property incentivizes sustainable resource development (efficient). The constraint is a rope (Rope). From the disadvantaged-states and future-generations seats, the reading is enclosure: technological gating replaces territorial gating, resource capture remains exclusionary, and no compensation mechanism exists—a snare. The engine computes divergent classifications from the structural data: beneficiary seats (advanced states, corporations) compute low directionality (near 0.0, beneficiary end) because they control the constraint and exit costlessly to alternative flag-states or corporate jurisdictions; payer seats (disadvantaged states, excluded populations) compute high directionality (near 1.0, target end) because they are locked into exclusion by technical and capital constraints with no remedy mechanism. The authored claim of snare reflects the payer-seat structural reality; a beneficiary-seat analyst might author rope. Both readings are computationally valid; the divergence measures the asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Technologically_advanced_spacefaring_states: d ≈ 0.2 (strong beneficiary, set the rules, arbitrage exit to compliant jurisdictions). Private_space_extraction_corporations: d ≈ 0.15 (pure beneficiary, own resources outright, mobile exit across flag-states). Technologically_disadvantaged_states: d ≈ 0.85 (full target, locked into exclusion by capital and technical constraints, no compensation, trapped exit). Indigenous_and_developing_world_populations: d ≈ 0.88 (identity-locked target, representing future-generations and justice commitments that make exit psychologically impossible). Space_law_scholarship: d ≈ 0.5 (analytical, no structural benefit or harm, observes the constraint's operation). The directionality derivation from beneficiary/victim + exit is straightforward: the reading creates two classes of actors—those with technological capacity and flag-state authorization (low d) and those without (high d). Identity_lock for disadvantaged populations arises from the fusion of national representation and global-justice identity: accepting permanent exclusion from extraterrestrial resources violates their self-conception as equals in a common humanity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing territorial annexation and great-power space conflict, 1967) is still structurally relevant—no state claims Mars or the Moon as sovereign territory. However, the extraction-permissive reading has enabled a functional displacement of the problem: instead of avoiding territorial conflict, the constraint now enables resource enclosure by technical capability and privatizes the conflict over who may extract. The mandatrophy claim would be that Article II's non-appropriation principle has outlived its mandate: it solved territorial annexation but re-enabled resource appropriation through the private-property loophole. This reading contests whether the founding problem remains live or has been solved in form but displaced in substance. The constraint is NOT mandatrophic in the strong sense (the problem is not dead and replaced by pure extraction ritual); rather, the constraint's function has shifted from preventing territorial conflict to enabling private resource capture while maintaining the appearance of non-appropriation (hence theater_ratio rising). This is a boundary case between rope-to-snare degradation and snare persistence—the coordination function (preventing territorial conflict) remains real but is now overmeasured by the extraction function (enabling resource appropriation). The correct classification is snare, not piton, because the extraction is still functional and actively enforced, not merely performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_ii_text_permissiveness_ambiguity,
    'Does Article II''s language (''not subject to national appropriation...by any other means'') grammatically and textually permit or prohibit private resource extraction and ownership?',
    'Strict textual analysis comparing drafting history, treaty-body preparatory work (COPUOS legislative history), and comparative treaty language (UNCLOS common-heritage provisions, which postdate OST by a decade and show alternative non-appropriation language). Canonical interpretation by an International Court of Justice advisory opinion on Article II scope.',
    'If the text permits extraction, the extraction-permissive reading gains structural legitimacy as the default reading (no override required). If the text is ambiguous, deference to later codification (regime reading) or conservation principle becomes stronger. If the text prohibits extraction, the commons-conservation reading becomes canonical and this constraint reclassifies to tangled_rope (genuine coordination + enforced conservation constraint).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_ii_text_permissiveness_ambiguity, conceptual, 'Whether Article II''s ''non-appropriation'' language covers resource extraction or only territorial claims.').

omega_variable(
    technological_access_as_new_gatekeeping_mechanism,
    'Does the extraction-permissive reading merely substitute technological gatekeeping (only states with spacefaring capacity can extract) for territorial gatekeeping (sovereign claims), replicating colonial enclosure structures, or does private-property extraction represent a fundamentally different (and more efficiency-aligned) allocation mechanism?',
    'Empirical track record: compare extraction patterns 2026-2050 between the extraction-permissive regime and counterfactual international-regime governance. Measure: (1) distribution of extraction permits across spacefaring vs. non-spacefaring states; (2) benefit flows (tax revenue, resource rents, local economic activity) to advantaged vs. disadvantaged states; (3) evidence of technology transfer or capacity-building in disadvantaged states under extraction-permissive regime.',
    'If extraction-permissive produces identical or more unequal distribution patterns as historical colonialism, the reading is confirmed as re-encoded appropriation (supports snare classification and strengthens mandatrophy argument). If extraction-permissive produces some benefit-sharing via tax revenue or technology transfer, the reading becomes closer to tangled_rope (genuine coordination + partial extraction). If the regime is abandoned for international framework before 2050, the question remains speculative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_access_as_new_gatekeeping_mechanism, empirical, 'Whether extraction-permissive gatekeeping replicates historical colonial patterns or enables new benefit distribution.').

omega_variable(
    future_generations_discount_rate,
    'Under the extraction-permissive reading, are irreversible resource depletions and scientific-site damage acceptable trade-offs against current-generation profit maximization, or does intergenerational justice impose a conservation obligation independent of Article II text?',
    'Normative legal theory: develop a coherent intergenerational-justice framework for space resources (parallel to Earth environmental law). Empirical: track evidence of resource-depletion trajectories and scientific-site damage under extraction-permissive regime 2026-2050. Comparative: examine whether Earth law imposes stricter conservation standards on terrestrial commons than space law imposes on celestial resources (likely yes); if so, the differential is itself a signal of inequality.',
    'If intergenerational justice is accepted as a binding constraint, the extraction-permissive reading becomes indefensible without conservation additions (forced into tangled_rope with built-in sustainability limits). If profit maximization is accepted as the governing principle, the reading stands but explicitly acknowledges sacrificing future access for current extraction (strengthens snare classification and ''victims include future generations'' premise).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(future_generations_discount_rate, preference, 'Whether intergenerational justice overrides profit-extraction logic in space-resource governance.').

omega_variable(
    commons_conservation_alternative_reading_foreclosure,
    'Does this reading (extraction-permissive) logically foreclose the commons-conservation reading, or do both coexist as live interpretive positions held by different parties?',
    'Logical analysis: if the extraction-permissive reading claims Article II permits private extraction, and the conservation reading claims Article II forbids private extraction, and both are read into the same fixed text with no hierarchy or framework rule to adjudicate, then the readings coexist (different parties hold them) rather than foreclose (neither can hold the text under one interpretation). Foreclosure would require showing that acceptance of one reading''s core premise (e.g., ''property rights maximize efficiency'') logically entails rejection of the other''s core premise (e.g., ''common ownership prevents enclosure'')—which it does NOT; both are coherent frameworks at different normative levels.',
    'Confirms the reading_relations entry: extraction_permissive → commons_conservation is coexists_with, not forecloses. The kernel is genuinely contested, not a binary choice with one reading ruling out the other. Both readings can be held simultaneously by different parties and different states, creating the persistent institutional gridlock at COPUOS (consensus rule prevents regime establishment).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commons_conservation_alternative_reading_foreclosure, conceptual, 'Whether the extraction-permissive and conservation readings are logically coexistent or mutually exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__extraction_permissive, 1967, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1967, 0.05).
narrative_ontology:measurement_basis(ost__tr_t1967, observed).
narrative_ontology:measurement(ost__tr_t1990, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1990, 0.12).
narrative_ontology:measurement_basis(ost__tr_t1990, observed).
narrative_ontology:measurement(ost__tr_t2008, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2008, 0.28).
narrative_ontology:measurement_basis(ost__tr_t2008, observed).
narrative_ontology:measurement(ost__tr_t2018, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2018, 0.35).
narrative_ontology:measurement_basis(ost__tr_t2018, observed).
narrative_ontology:measurement(ost__tr_t2026, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(ost__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1967, 0.15).
narrative_ontology:measurement_basis(ost__be_t1967, observed).
narrative_ontology:measurement(ost__be_t1990, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement_basis(ost__be_t1990, observed).
narrative_ontology:measurement(ost__be_t2008, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2008, 0.58).
narrative_ontology:measurement_basis(ost__be_t2008, observed).
narrative_ontology:measurement(ost__be_t2018, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2018, 0.68).
narrative_ontology:measurement_basis(ost__be_t2018, observed).
narrative_ontology:measurement(ost__be_t2026, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2026, 0.78).
narrative_ontology:measurement_basis(ost__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1967, 0.25).
narrative_ontology:measurement_basis(ost__su_t1967, observed).
narrative_ontology:measurement(ost__su_t1990, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1990, 0.42).
narrative_ontology:measurement_basis(ost__su_t1990, observed).
narrative_ontology:measurement(ost__su_t2008, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2008, 0.58).
narrative_ontology:measurement_basis(ost__su_t2008, observed).
narrative_ontology:measurement(ost__su_t2018, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2018, 0.64).
narrative_ontology:measurement_basis(ost__su_t2018, observed).
narrative_ontology:measurement(ost__su_t2026, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(ost__su_t2026, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1967, tn=2026
narrative_ontology:measurement(ost__grid_01, ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse(class), 1967, 0.2).
narrative_ontology:measurement(ost__grid_02, ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse(class), 2026, 0.68).
narrative_ontology:measurement(ost__grid_03, ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse(individual), 1967, 0.1).
narrative_ontology:measurement(ost__grid_04, ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse(individual), 2026, 0.55).
narrative_ontology:measurement(ost__grid_05, ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse(organizational), 1967, 0.15).
narrative_ontology:measurement(ost__grid_06, ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse(organizational), 2026, 0.58).
narrative_ontology:measurement(ost__grid_07, ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse(structural), 1967, 0.3).
narrative_ontology:measurement(ost__grid_08, ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse(structural), 2026, 0.62).
narrative_ontology:measurement(ost__grid_09, ost_article_ii_non_appropriation__extraction_permissive, resistance(class), 1967, 0.55).
narrative_ontology:measurement(ost__grid_10, ost_article_ii_non_appropriation__extraction_permissive, resistance(class), 2026, 0.48).
narrative_ontology:measurement(ost__grid_11, ost_article_ii_non_appropriation__extraction_permissive, resistance(individual), 1967, 0.4).
narrative_ontology:measurement(ost__grid_12, ost_article_ii_non_appropriation__extraction_permissive, resistance(individual), 2026, 0.38).
narrative_ontology:measurement(ost__grid_13, ost_article_ii_non_appropriation__extraction_permissive, resistance(organizational), 1967, 0.68).
narrative_ontology:measurement(ost__grid_14, ost_article_ii_non_appropriation__extraction_permissive, resistance(organizational), 2026, 0.42).
narrative_ontology:measurement(ost__grid_15, ost_article_ii_non_appropriation__extraction_permissive, resistance(structural), 1967, 0.75).
narrative_ontology:measurement(ost__grid_16, ost_article_ii_non_appropriation__extraction_permissive, resistance(structural), 2026, 0.35).
narrative_ontology:measurement(ost__grid_17, ost_article_ii_non_appropriation__extraction_permissive, stakes_inflation(class), 1967, 0.15).
narrative_ontology:measurement(ost__grid_18, ost_article_ii_non_appropriation__extraction_permissive, stakes_inflation(class), 2026, 0.75).
narrative_ontology:measurement(ost__grid_19, ost_article_ii_non_appropriation__extraction_permissive, stakes_inflation(individual), 1967, 0.08).
narrative_ontology:measurement(ost__grid_20, ost_article_ii_non_appropriation__extraction_permissive, stakes_inflation(individual), 2026, 0.52).
narrative_ontology:measurement(ost__grid_21, ost_article_ii_non_appropriation__extraction_permissive, stakes_inflation(organizational), 1967, 0.1).
narrative_ontology:measurement(ost__grid_22, ost_article_ii_non_appropriation__extraction_permissive, stakes_inflation(organizational), 2026, 0.65).
narrative_ontology:measurement(ost__grid_23, ost_article_ii_non_appropriation__extraction_permissive, stakes_inflation(structural), 1967, 0.2).
narrative_ontology:measurement(ost__grid_24, ost_article_ii_non_appropriation__extraction_permissive, stakes_inflation(structural), 2026, 0.72).
narrative_ontology:measurement(ost__grid_25, ost_article_ii_non_appropriation__extraction_permissive, suppression(class), 1967, 0.35).
narrative_ontology:measurement(ost__grid_26, ost_article_ii_non_appropriation__extraction_permissive, suppression(class), 2026, 0.72).
narrative_ontology:measurement(ost__grid_27, ost_article_ii_non_appropriation__extraction_permissive, suppression(individual), 1967, 0.28).
narrative_ontology:measurement(ost__grid_28, ost_article_ii_non_appropriation__extraction_permissive, suppression(individual), 2026, 0.65).
narrative_ontology:measurement(ost__grid_29, ost_article_ii_non_appropriation__extraction_permissive, suppression(organizational), 1967, 0.2).
narrative_ontology:measurement(ost__grid_30, ost_article_ii_non_appropriation__extraction_permissive, suppression(organizational), 2026, 0.75).
narrative_ontology:measurement(ost__grid_31, ost_article_ii_non_appropriation__extraction_permissive, suppression(structural), 1967, 0.15).
narrative_ontology:measurement(ost__grid_32, ost_article_ii_non_appropriation__extraction_permissive, suppression(structural), 2026, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__extraction_permissive, global_infrastructure).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__extraction_permissive, 0.22).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__international_regime).

% DUAL FORMULATION NOTE:
% The ost_article_ii_non_appropriation kernel is instantiated in three distinct constraints, each representing a reading held by different parties and producing different structural outcomes. The extraction_permissive reading (this constraint) treats Article II as barring only territorial claims, enabling resource appropriation by private actors licensed by spacefaring states. The commons_conservation reading treats Article II as forbidding all appropriation, including private extraction, and would classify the same situation as a tangled_rope or rope constraint defending common-heritage principle. The international_regime reading treats the appropriation question as unresolved pending multilateral agreement and would classify the current situation as a snare (pure enforcement of inaction). All three readings share the kernel (Article II fixed text) but diverge on interpretation, beneficiary/victim structure, and enforcement mechanism. The network relationship captures the fact that adoption of one reading affects the viability and structure of the others: if extraction-permissive becomes institutionalized through widespread national legislation (as U.S., Luxembourg, UAE legislation achieved in 2015-2020), the international_regime reading becomes harder to revive (fait accompli disadvantages agreement-seeking); if conservation reading gains COPUOS consensus, extraction-permissive reading loses legitimacy despite past unilateral actions. Each story carries independent epsilon, stakeholder structure, and classification; the family relationship is documented here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ost_article_ii_non_appropriation__extraction_permissive, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
