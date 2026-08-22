% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__commons_conservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__commons_conservation, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__commons_conservation
 *   human_readable: OST Article II Non-Appropriation Principle (Commons Conservation Reading)
 *   domain: international/legal/space governance
 *
 * SUMMARY:
 *   Article II of the Outer Space Treaty (1967) declares that 'outer space,
 *   including the moon and other celestial bodies, is not subject to national
 *   appropriation by claim of sovereignty, by means of use or occupation, or
 *   by any other means.' The commons-conservation reading interprets 'use or
 *   occupation' to prohibit de facto appropriation via resource extraction:
 *   if a spacefaring state or private actor extracts resources (lunar
 *   minerals, water ice, asteroid metals) and claims ownership, that
 *   extraction constitutes de facto appropriation even without formal
 *   territorial claim. Under this reading, extraction is prohibited unless
 *   multilateral authorization is granted through international regime
 *   mechanisms. This stands in direct tension with extraction-permissive and
 *   international-regime readings that either permit unilateral extraction
 *   and ownership (extraction-permissive) or defer the question to future
 *   negotiation without establishing a current prohibition
 *   (international-regime). The constraint story instantiates the
 *   commons-conservation reading as an ε-invariant commitment: what benefits
 *   non-spacefaring states and future-generation commons access, what costs
 *   first-movers, and how the constraint persists through active enforcement
 *   (treaty interpretation, regime development, orbital monitoring).
 *
 * KEY AGENTS:
 *   - Non-spacefaring states (veto holders over regime design)
 *   - Spacefaring states with mining capability (face stranded-asset risk)
 *   - Private extraction investors (locked into collective-authorization pathway)
 *   - Multilateral governance regime (agenda-setter for authorization and benefit-sharing)
 *   - International legal arbiters (interpret Article II and activate this or sibling readings)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, 0.68).
domain_priors:suppression_score(ost_article_ii_non_appropriation__commons_conservation, 0.72).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__commons_conservation, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, extractiveness, 0.68).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__commons_conservation, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__commons_conservation, "OST Article II Non-Appropriation Principle (Commons Conservation Reading)").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__commons_conservation, "international/legal/space governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__commons_conservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__commons_conservation, '7ecf4f1d-867e-4739-813e-41b6b3fa55c6').
narrative_ontology:cs_kernel_codification('7ecf4f1d-867e-4739-813e-41b6b3fa55c6', fixed_text).
narrative_ontology:cs_authority_grounding('7ecf4f1d-867e-4739-813e-41b6b3fa55c6', lineage).
narrative_ontology:cs_interpretation_layer_present('7ecf4f1d-867e-4739-813e-41b6b3fa55c6').
narrative_ontology:cs_reading_relation('7ecf4f1d-867e-4739-813e-41b6b3fa55c6', ost_article_ii_non_appropriation__extraction_permissive, forecloses).
narrative_ontology:cs_reading_relation('7ecf4f1d-867e-4739-813e-41b6b3fa55c6', ost_article_ii_non_appropriation__international_regime, coexists_with).
narrative_ontology:cs_axiom('7ecf4f1d-867e-4739-813e-41b6b3fa55c6', foundational, extraction_constitutes_use_or_occupation).
narrative_ontology:cs_axiom_status(extraction_constitutes_use_or_occupation, holdable).
narrative_ontology:cs_axiom_grounding('7ecf4f1d-867e-4739-813e-41b6b3fa55c6', extraction_constitutes_use_or_occupation, conventional).
narrative_ontology:cs_axiom('7ecf4f1d-867e-4739-813e-41b6b3fa55c6', secondary, non_appropriation_applies_to_private_actors).
narrative_ontology:cs_axiom_status(non_appropriation_applies_to_private_actors, holdable).
narrative_ontology:cs_axiom_grounding('7ecf4f1d-867e-4739-813e-41b6b3fa55c6', non_appropriation_applies_to_private_actors, deontological).
narrative_ontology:cs_reference_frame('7ecf4f1d-867e-4739-813e-41b6b3fa55c6', treaty_commons_preservation_1967).
narrative_ontology:cs_drift_state('7ecf4f1d-867e-4739-813e-41b6b3fa55c6', extraction_economic_viability_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7ecf4f1d-867e-4739-813e-41b6b3fa55c6', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, future_generations_commons_access).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, first_mover_extraction_investors).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, spacefaring_states_mining_capability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States without independent launch capability or major space infrastructure. Under the commons-conservation reading, they retain a veto over enclosure through multilateral negotiation frameworks (Article XI). Their interest is preserving access to celestial resources and preventing first-mover capture that would exclude them from benefits. They benefit from the non-appropriation constraint by being included in future benefit-sharing regimes rather than facing a fait accompli of extraction-permissive property claims.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states, beneficiary,
    organized, generational, constrained, universal).

% States with demonstrated space capability (launch infrastructure, robotics, orbital mechanics expertise): primarily Russia, US, China, Japan, ESA members. Under this reading, they bear the cost of the non-appropriation constraint by being unable to claim sovereign or private ownership of extracted resources absent multilateral authorization. Their first-mover mining investments are stranded—resources they extract belong to no entity until a regime is negotiated. The constraint forces them into collective-action frameworks rather than unilateral appropriation.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, spacefaring_states_mining_capability, payer,
    powerful, generational, constrained, universal).

% Private corporations and state enterprises planning or executing resource extraction (mining operations on asteroids, lunar regolith extraction, water-ice harvesting). Under the commons-conservation reading, they face stranded assets: the resources they extract cannot be treated as their property until multilateral consent is established. They must negotiate benefit-sharing arrangements with non-spacefaring states and competing extractors before extraction becomes economically viable. Their competitive advantage (first-mover capability) is nullified by the constraint's requirement of multilateral authorization.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, first_mover_extraction_investors, payer,
    institutional, biographical, trapped, universal).

% The collective institutional structure (UN Committee on Peaceful Uses of Outer Space, potential future International Space Resources Authority modeled on ISA) that would oversee benefit-sharing and authorization of extraction. Under this reading, the regime must exist to operationalize the non-appropriation principle. The agenda-setter sets the terms on which extraction is permitted, who receives benefits, and how resources are distributed. Without active regime capacity, the constraint persists as a veto but offers no alternative coordination path.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, multilateral_space_governance_regime, agenda_setter,
    institutional, generational, analytical, universal).

% A non-agent entity (abstract future interest in preserved access to common resources). Listed for completeness: this reading vindicates the principle that celestial resources belong to humankind as a whole, not to any state or corporation. Future generations benefit from the constraint's preservation of the commons against enclosure, enabling them to participate in space resource utilization rather than inheriting a carved-up domain.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, future_generations_commons_access, beneficiary,
    powerless, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ost_article_ii_non_appropriation__commons_conservation, future_generations_commons_access).

% States and private actors who interpret Article II as permitting private ownership of extracted resources (the extraction-permissive sibling reading). They would benefit from unconstrained appropriation but are structurally excluded by this reading's adoption: their preferred legal framework is ruled out by the operative commitment to non-appropriation. Their voice—that extraction should be unregulated and resources should belong to extractors—is present in treaty debates but foreclosed by this reading's core axiom.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, extraction_permissive_reading_adherents, excluded,
    powerful, biographical, constrained, universal).

% International Court of Justice, arbitration panels, treaty interpretation bodies. They adjudicate whether Article II's 'use or occupation' language prohibits de facto appropriation. Their verdicts determine whether the commons-conservation reading becomes operative or whether extraction-permissive or international-regime readings are activated. They occupy an analytical seat—they do not benefit or pay directly but their interpretive authority structures the constraint's actual operation.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, international_legal_arbiters, observer,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__commons_conservation, multilateral_space_governance_regime).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__commons_conservation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal commons boundary that prevents any state or private actor from unilaterally claiming property rights in celestial resources. The coordination problem solved: without non-appropriation, first-movers create a race-to-extract dynamic that encodes capability as ownership, excluding non-spacefaring states from any share of space resources. The constraint coordinates plural access by requiring multilateral authorization before extraction yields property claims.
% TRANSFER_FUNCTION: Moves the legal authority to authorize resource extraction from individual first-movers (under extraction-permissive reading) to multilateral regimes (under this reading). In material terms: benefit flows from extraction capability (spacefaring states, first-mover investors) to negotiation participation (all states through UN frameworks, future generations through preservation of commons). The transfer mechanism is authorization denial: extraction is prohibited absent multilateral consent, which must be purchased by benefit-sharing commitments.
% ABSENT_VOICES: Private actors not yet invested in space extraction (future entrants who would face property claims in an extraction-permissive regime); non-spacefaring developing states whose diplomats are overwhelmed by technical space law debates; indigenous cosmologies and non-Western legal traditions that ground resource governance in different premises (relational kinship to celestial bodies rather than commodification). These voices would argue for expanded commons protection, but they are marginal to the formal treaty interpretation process.
% DISAPPEARANCE_RATIONALE: If the non-appropriation constraint vanished, spacefaring states and first-mover investors would immediately claim property rights in lunar resources, asteroid minerals, and water ice. Extraction would become economically viable without sharing obligations. Within years, the celestial commons would be partitioned by extraction claims; non-spacefaring states would be excluded from benefits despite being party to the OST. The governance structure that currently frames celestial resources as shared would collapse into a race-to-claim dynamic structured by technical capability.
% FOUNDING_PROBLEM: The Outer Space Treaty was drafted in the shadow of territorial competition (Cold War space race). The founding problem was: how do we prevent space from becoming colonized territory, subject to sovereign territorial claims that would exclude non-spacefaring nations and create a new frontier of inequality? Article II's non-appropriation language was meant to shut down that scenario by declaring that no state or actor could claim territorial sovereignty. The commons-conservation reading extends this founding problem to resource extraction: de facto appropriation via extraction must be prohibited the same way de jure territorial claims are prohibited, or inequality will be encoded in resource ownership rather than territory.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (preventing space colonization) is attested by non-spacefaring states, the UN General Assembly's resolution framework on space and development, and independent scholars of international space law writing outside the spacefaring-state consensus. Spacefaring states acknowledge the founding problem historically but argue it is now obsolete—they contend that space has proven too costly and technically demanding to support colonization, so prevention is unnecessary. The contest is about whether the founding rationale still applies to resource extraction.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__commons_conservation, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__commons_conservation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__commons_conservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__commons_conservation, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__commons_conservation, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The commons-conservation reading produces high extractiveness (0.68 at 2026) because it strands first-mover competitive advantage—the natural profit from being first to extract is eliminated by the requirement for multilateral authorization and benefit-sharing. Suppression is high (0.72) because the constraint must actively prevent extraction without authorization; enforcement involves orbital monitoring, technology export controls on launch systems, and treaty compliance verification. Theater ratio is moderate (0.41) and rising: early in the interval (1967–1980), the constraint was genuinely understood as a natural-law principle (mining was too expensive to be realistic); as extraction became technically feasible (2010 onward), enforcement shifted toward active prohibition—regime development, compliance narratives, and monitoring grew while the underlying coordination principle remained constant. The measurement series tracks rising extractiveness because the constraint's operative force increased as extraction became technologically viable. At 1967, extraction was theoretical; by 2026, it is capital-intensive but feasible, so the constraint's suppression of extraction-without-authorization is now a real mechanism, not theoretical. Theater ratio rises because more effort is devoted to maintaining the prohibition narrative as extraction temptation grows. The interval runs from OST signature (1967) to present (2026).
 *
 * PERSPECTIVAL GAP:
 *   Spacefaring states and first-mover investors see the constraint as high-extraction, coercive, and unjust (it denies them the fruits of their technical achievement and investment). Non-spacefaring states see the constraint as protective (it prevents enclosure and ensures future access through negotiation). The engine should compute these as different directionalities: spacefaring states approach d near 1.0 (full targets of the extraction prohibition), while non-spacefaring states approach d near 0.0 (beneficiaries protected by the veto). The international legal arbiters occupy an analytical seat (d=0.5 canonical, may override)—they benefit from the constraint's existence (legitimacy, relevance) but do not directly pay or collect from extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Spacefaring states and first-mover investors are targets because the constraint directly prevents them from capturing extraction benefits without multilateral consent. Their technical capability (high power, mobile exit options in many domains) is nullified by this specific constraint's requirement for authorization. Non-spacefaring states are beneficiaries because the constraint preserves their negotiating power and future access (though their individual exit options are constrained by their dependence on multilateral frameworks). The directionality gradient reflects the asymmetry: extractive capability is powerful and would normally dominate, but this constraint redistributes power toward collective decision-making. The beneficiary directionality is low (~0.15–0.25 range) because the benefit is preservation of veto and future negotiation position, not direct collection. The victim directionality is high (~0.75–0.85 range) because the cost is prohibition of what would otherwise be the highest-profit activity.
 *
 * MANDATROPHY ANALYSIS:
 *   The commons-conservation reading claims tangled_rope: it coordinates access to celestial resources (the coordination function is preventing enclosure and enabling multilateral negotiation), and it extracts from first-movers by denying them unilateral appropriation rights. The coordination is genuine—absent this constraint, each state would race to extract with no guarantee that others could participate or negotiate. The extraction is also genuine—the constraint's primary effect is denying private gain. The constraint is not pure extraction (snare) because the non-spacefaring states genuinely coordinate through collective authorization mechanisms; it is not pure coordination (rope) because extraction-capable states bear a substantial asymmetric cost. Tangled_rope classification holds: both coordination (multilateral authorization pathway) and extraction (denial of unilateral appropriation) are structural and irreducible. The mandatrophy question does not apply—the constraint is not suffering from mandate obsolescence; rather, it is shifting from theoretical principle (1967–2000) to operationally enforced constraint (2010 onward) as extraction became feasible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_ii_textual_interpretation_ambiguity,
    'Does Article II''s ''use or occupation'' language extend to resource extraction, or does it apply only to territorial and military presence?',
    'International Court of Justice advisory opinion or binding arbitration on whether extraction constitutes ''use or occupation'' under Article II; treaty interpretation following Vienna Convention on the Law of Treaties (ordinary meaning, context, object, purpose).',
    'If extraction IS ''use or occupation,'' the commons-conservation reading holds and extraction is prohibited absent authorization. If extraction is NOT covered, extraction-permissive or international-regime readings would be activated, eliminating the non-appropriation constraint''s application to resource economics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_ii_textual_interpretation_ambiguity, empirical, 'Textual interpretation of Article II scope.').

omega_variable(
    private_actor_coverage_scope,
    'Does Article II''s prohibition on appropriation apply to private corporations or only to states? Does ''state responsibility'' doctrine extend the state''s Article II obligations to corporations it licenses or incentivizes?',
    'State practice: do spacefaring states license private extraction without treating themselves as violating Article II? Do non-spacefaring states protest and invoke Article II? Does international arbitration find private extraction permits to be state-sponsored appropriation?',
    'If private actors are subject to the non-appropriation principle, the constraint applies to all extraction (high enforcement burden). If only states are bound, private extraction could proceed provided the state officially disavows ownership claims (low effective enforcement, theater_ratio spike). The commons-conservation reading asserts private coverage; extracted-permissive reading allows private actors to own extracted resources; international-regime reading defers this distinction to regime negotiation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(private_actor_coverage_scope, conceptual, 'Whether Article II''s non-appropriation applies to private actors or only states.').

omega_variable(
    multilateral_regime_institutional_form,
    'What institutional structure would operationalize the commons-conservation reading''s requirement for ''multilateral authorization''? Does such an institution currently exist, and if not, does its absence constitute non-enforcement of the reading or merely a deferred implementation?',
    'Creation of an International Space Resources Authority or equivalent treaty regime; observation of whether extraction proceeds without authorization (regime absence =non-enforcement); tracking whether spacefaring states accept that regime decisions constrain their extraction.',
    'If no regime exists, the commons-conservation reading cannot operationally enforce authorization (theater_ratio dominates; suppression is nominal). Creation of an effective regime would operationalize enforcement (suppression rises, theater_ratio stabilizes). The international-regime reading depends on this uncertainty—it argues that Article II defers this institutional question rather than answering it now.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multilateral_regime_institutional_form, empirical, 'Institutional infrastructure for operationalizing the commons-conservation principle.').

omega_variable(
    enforcement_capacity_drift,
    'As extraction technology matures and becomes economically viable, will spacefaring states accept enforcement of the non-appropriation constraint, or will they treat it as obsolete and extract unilaterally?',
    'Historical observation: track whether states licensed private extraction; whether other states protested; whether extraction proceeds without authorization. Post-2025 conduct will determine whether the commons-conservation reading remains live or whether extraction-permissive practice displaces it.',
    'If enforcement holds, the commons-conservation reading persists as a binding constraint. If enforcement fails (extraction proceeds unprotested), the reading becomes theater (high theater_ratio, suppression collapses)—the extraction-permissive reading would have become de facto operative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_capacity_drift, empirical, 'Whether enforcement capacity for non-appropriation survives the transition from theoretical principle to economic reality.').

omega_variable(
    commons_vs_regime_benefit_distribution,
    'If multilateral authorization is required for extraction, how will benefits be distributed? Equal sharing (commons principle), or weighted by capacity/negotiating power (regime principle)? This determines whether the constraint genuinely protects non-spacefaring states or merely delays enclosure.',
    'Regime negotiation outcomes; observation of whether benefit-sharing arrangements include non-spacefaring states as equal principals or subordinate recipients; tracking whether non-spacefaring states accept the distribution scheme or renounce it.',
    'If benefits are equally shared, the commons-conservation reading produces genuine protection for all parties. If benefits are weighted toward spacefaring states, the constraint becomes a theater for coordination without actual redistribution (high theater_ratio, suppression remains but extraction proceeds under regime permission). The international-regime reading depends on this ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_vs_regime_benefit_distribution, preference, 'Whether multilateral authorization distributes benefits equitably or reproduces spacefaring-state dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__commons_conservation, 1967, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1967, 0.08).
narrative_ontology:measurement(ost__tr_t1980, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(ost__tr_t1995, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(ost__tr_t2010, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(ost__tr_t2018, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2018, 0.35).
narrative_ontology:measurement(ost__tr_t2026, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 2026, 0.41).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1967, 0.15).
narrative_ontology:measurement(ost__be_t1980, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement(ost__be_t1995, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(ost__be_t2010, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement(ost__be_t2018, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2018, 0.58).
narrative_ontology:measurement(ost__be_t2026, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1967, 0.42).
narrative_ontology:measurement(ost__su_t1980, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1980, 0.51).
narrative_ontology:measurement(ost__su_t1995, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(ost__su_t2010, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2010, 0.64).
narrative_ontology:measurement(ost__su_t2018, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2018, 0.69).
narrative_ontology:measurement(ost__su_t2026, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 2026, 0.72).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1967, tn=2026
narrative_ontology:measurement(ost__grid_01, ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse(class), 1967, 0.12).
narrative_ontology:measurement(ost__grid_02, ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse(class), 2026, 0.55).
narrative_ontology:measurement(ost__grid_03, ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse(individual), 1967, 0.08).
narrative_ontology:measurement(ost__grid_04, ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse(individual), 2026, 0.22).
narrative_ontology:measurement(ost__grid_05, ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse(organizational), 1967, 0.45).
narrative_ontology:measurement(ost__grid_06, ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse(organizational), 2026, 0.68).
narrative_ontology:measurement(ost__grid_07, ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse(structural), 1967, 0.82).
narrative_ontology:measurement(ost__grid_08, ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse(structural), 2026, 0.79).
narrative_ontology:measurement(ost__grid_09, ost_article_ii_non_appropriation__commons_conservation, resistance(class), 1967, 0.38).
narrative_ontology:measurement(ost__grid_10, ost_article_ii_non_appropriation__commons_conservation, resistance(class), 2026, 0.35).
narrative_ontology:measurement(ost__grid_11, ost_article_ii_non_appropriation__commons_conservation, resistance(individual), 1967, 0.18).
narrative_ontology:measurement(ost__grid_12, ost_article_ii_non_appropriation__commons_conservation, resistance(individual), 2026, 0.12).
narrative_ontology:measurement(ost__grid_13, ost_article_ii_non_appropriation__commons_conservation, resistance(organizational), 1967, 0.68).
narrative_ontology:measurement(ost__grid_14, ost_article_ii_non_appropriation__commons_conservation, resistance(organizational), 2026, 0.52).
narrative_ontology:measurement(ost__grid_15, ost_article_ii_non_appropriation__commons_conservation, resistance(structural), 1967, 0.72).
narrative_ontology:measurement(ost__grid_16, ost_article_ii_non_appropriation__commons_conservation, resistance(structural), 2026, 0.58).
narrative_ontology:measurement(ost__grid_17, ost_article_ii_non_appropriation__commons_conservation, stakes_inflation(class), 1967, 0.05).
narrative_ontology:measurement(ost__grid_18, ost_article_ii_non_appropriation__commons_conservation, stakes_inflation(class), 2026, 0.42).
narrative_ontology:measurement(ost__grid_19, ost_article_ii_non_appropriation__commons_conservation, stakes_inflation(individual), 1967, 0.02).
narrative_ontology:measurement(ost__grid_20, ost_article_ii_non_appropriation__commons_conservation, stakes_inflation(individual), 2026, 0.18).
narrative_ontology:measurement(ost__grid_21, ost_article_ii_non_appropriation__commons_conservation, stakes_inflation(organizational), 1967, 0.08).
narrative_ontology:measurement(ost__grid_22, ost_article_ii_non_appropriation__commons_conservation, stakes_inflation(organizational), 2026, 0.64).
narrative_ontology:measurement(ost__grid_23, ost_article_ii_non_appropriation__commons_conservation, stakes_inflation(structural), 1967, 0.18).
narrative_ontology:measurement(ost__grid_24, ost_article_ii_non_appropriation__commons_conservation, stakes_inflation(structural), 2026, 0.71).
narrative_ontology:measurement(ost__grid_25, ost_article_ii_non_appropriation__commons_conservation, suppression(class), 1967, 0.15).
narrative_ontology:measurement(ost__grid_26, ost_article_ii_non_appropriation__commons_conservation, suppression(class), 2026, 0.48).
narrative_ontology:measurement(ost__grid_27, ost_article_ii_non_appropriation__commons_conservation, suppression(individual), 1967, 0.08).
narrative_ontology:measurement(ost__grid_28, ost_article_ii_non_appropriation__commons_conservation, suppression(individual), 2026, 0.22).
narrative_ontology:measurement(ost__grid_29, ost_article_ii_non_appropriation__commons_conservation, suppression(organizational), 1967, 0.32).
narrative_ontology:measurement(ost__grid_30, ost_article_ii_non_appropriation__commons_conservation, suppression(organizational), 2026, 0.68).
narrative_ontology:measurement(ost__grid_31, ost_article_ii_non_appropriation__commons_conservation, suppression(structural), 1967, 0.38).
narrative_ontology:measurement(ost__grid_32, ost_article_ii_non_appropriation__commons_conservation, suppression(structural), 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__commons_conservation, global_infrastructure).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__commons_conservation, 0.22).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__international_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, outer_space_treaty_article_xi_governance).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, lunar_mining_authorization_regime).

% DUAL FORMULATION NOTE:
% The OST Article II non-appropriation principle decomposes into three structurally distinct constraint stories: commons_conservation (this file, prohibits extraction absent multilateral authorization), extraction_permissive (permits private ownership of extracted resources despite Article II language), and international_regime (defers appropriation question to future multilateral regime negotiation). The three readings have different ε values, different beneficiary/victim structures, and different persistence mechanisms. They are not the same constraint viewed from different angles—they are competing interpretations of a contested kernel. The commons-conservation reading assumes 'use or occupation' covers extraction; extraction-permissive assumes it does not; international-regime assumes Article II does not settle the question. Each reading operationalizes a different functional distribution: commons-conservation distributes through multilateral negotiation, extraction-permissive through first-mover advantage, international-regime through deferred regime decision. Network edges link all three stories; metrics and beneficiary/victim declarations are independent for each reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
