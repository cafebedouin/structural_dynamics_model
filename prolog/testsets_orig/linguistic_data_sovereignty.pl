% ============================================================================
% CONSTRAINT STORY: linguistic_data_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_linguistic_data_sovereignty, []).

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
 *   constraint_id: linguistic_data_sovereignty
 *   human_readable: Linguistic Data Sovereignty and Language Documentation
 *   domain: linguistics/cultural_preservation/technology_policy
 *
 * SUMMARY:
 *   Linguistic data sovereignty describes the structural constraint between
 *   indigenous and minority language communities' interests in controlling
 *   how their language data is used, and the economic incentives of large
 *   technology companies to harvest and incorporate that data into commercial
 *   machine learning systems without compensation or meaningful consent. The
 *   constraint exhibits the full spectrum of DR classification: from the
 *   perspective of powerless speakers, it appears as pure extraction (snare);
 *   from the perspective of organized advocacy coalitions, it appears as a
 *   solvable coordination problem with policy solutions (scaffold); from the
 *   perspective of tech companies, it appears as legitimate coordination
 *   (rope). The theater ratio (0.48) reflects that much of the data
 *   extraction is justified through scientific and preservation rhetoric ('we
 *   are helping document endangered languages,' 'this enables universal
 *   translation') that obscures the economic extraction mechanism. The
 *   extractiveness has increased from 0.28 to 0.58 over the interval as the
 *   economic value of multilingual training data has grown and the scale of
 *   uncompensated data collection has expanded. The constraint is generative
 *   of secondary constraints: language technology accessibility (does
 *   documentation technology serve communities or extract their expertise?),
 *   AI representational colonialism (how are minority languages represented
 *   in language models?), and epistemic sovereignty (who controls the
 *   metadata and categorization frameworks for linguistic data?).
 *
 * KEY AGENTS:
 *   - Indigenous Language Community: Primary victim (powerless/trapped) — speakers of languages targeted for data extraction without consent or compensation; structurally unable to negotiate or exit
 *   - Endangered Language Documentation Project: Secondary victim (moderate/constrained) — funded research groups that experience genuine coordination benefit (access to tools, collaborators) alongside extraction (their curated data becomes proprietary training material)
 *   - Language Technology Company: Primary beneficiary (institutional/arbitrage) — builds commercial value from aggregated linguistic data; can arbitrage across licensing, product integration, and platform partnerships
 *   - Language Rights Advocacy Coalition: Organized agent (organized/constrained) — UNESCO, WIPO, indigenous organizations building policy frameworks and alternative infrastructure with sunset logic
 *   - Linguistic Data Stewardship Institution: Emerging institutional actor (institutional/constrained) — community-controlled repositories and data governance frameworks attempting to reduce extraction by shifting control
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing language extinction and data extraction as inevitable processes rather than contingent policy outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(linguistic_data_sovereignty, 0.58).
domain_priors:suppression_score(linguistic_data_sovereignty, 0.65).
domain_priors:theater_ratio(linguistic_data_sovereignty, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(linguistic_data_sovereignty, extractiveness, 0.58).
narrative_ontology:constraint_metric(linguistic_data_sovereignty, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(linguistic_data_sovereignty, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(linguistic_data_sovereignty, tangled_rope).
narrative_ontology:human_readable(linguistic_data_sovereignty, "Linguistic Data Sovereignty and Language Documentation").
narrative_ontology:topic_domain(linguistic_data_sovereignty, "linguistics/cultural_preservation/technology_policy").

domain_priors:requires_active_enforcement(linguistic_data_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(linguistic_data_sovereignty, language_technology_companies).
narrative_ontology:constraint_beneficiary(linguistic_data_sovereignty, large_language_model_developers).
narrative_ontology:constraint_beneficiary(linguistic_data_sovereignty, machine_translation_platforms).
narrative_ontology:constraint_victim(linguistic_data_sovereignty, indigenous_language_communities).
narrative_ontology:constraint_victim(linguistic_data_sovereignty, minority_language_speakers).
narrative_ontology:constraint_victim(linguistic_data_sovereignty, endangered_language_documentation_efforts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS LANGUAGE COMMUNITY (SNARE) — Trapped speakers of minority languages have no exit from the extraction mechanism: their linguistic data is harvested for training large language models without consent or compensation, their language documentation is scraped by tech companies to build commercial systems, and they lack institutional capacity to negotiate or enforce restrictions. The constraint offers no coordination benefit to the trapped community — only extraction disguised as 'inclusion in global technology.'
constraint_indexing:constraint_classification(linguistic_data_sovereignty, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENDANGERED LANGUAGE DOCUMENTATION PROJECT (TANGLED ROPE) — Constrained by funding limitations and institutional dependence on tech platforms for computational resources. Projects experience genuine coordination benefit (access to computational linguistics tools, distribution channels, international collaborators) alongside substantial extraction (their curated linguistic data is incorporated into commercial LLMs without attribution or benefit-sharing, their documentation labor increases the value of proprietary models). Active enforcement maintains both functions simultaneously.
constraint_indexing:constraint_classification(linguistic_data_sovereignty, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LANGUAGE TECHNOLOGY COMPANY (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: aggregating linguistic data from diverse communities creates network effects in machine translation and multilingual AI systems. The company can arbitrage: licensing standardized datasets, building proprietary training data pipelines, or negotiating platform partnerships. The coordination function is genuine and valued (cross-language interoperability) but massively asymmetric in benefit distribution.
constraint_indexing:constraint_classification(linguistic_data_sovereignty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LANGUAGE RIGHTS ADVOCACY COALITION (SCAFFOLD) — Organized agents (UNESCO, WIPO, indigenous rights organizations) see linguistic data sovereignty as a temporary coordination failure solvable through policy instruments: data stewardship agreements, community-controlled language repositories, benefit-sharing frameworks, and linguistic privacy regulations. The coalition is building alternative infrastructure (Community Linguist platforms, decentralized language archives) with explicit sunset logic — as indigenous communities develop institutional capacity and policy frameworks mature, the current unconstrained extraction will lose force. Moderate suppression because organized agents retain negotiating power.
constraint_indexing:constraint_classification(linguistic_data_sovereignty, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LINGUISTIC COLONIALISM INSTITUTIONAL FRAME (PITON) — The normative frame justifying linguistic data extraction (that language technology benefits 'all humanity' and that documentation serves 'scientific preservation') persists through institutional inertia despite degraded function. The frame was once genuine (early computational linguistics focused on knowledge advancement) but now largely serves as theatrical justification for proprietary data capture. Theater ratio is lower (0.48) than typical pitons because some genuine scientific documentation still occurs; the degradation is partial rather than total.
constraint_indexing:constraint_classification(linguistic_data_sovereignty, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational scale, some language loss is mathematically inevitable: as dominant languages expand and economic incentives concentrate, minority languages experience inevitable speaker attrition. This perspective sees linguistic data extraction as a natural consequence of linguistic evolution — minority languages are fated to disappear, and capturing their data before extinction is a form of preservation, not predation. The classification risks naturalizing what structural analysis reveals as contingent institutional arrangements: language shift is an observable phenomenon, but the specific extraction mechanisms (uncompensated data harvesting, IP concentration) are policy-contingent, not natural.
constraint_indexing:constraint_classification(linguistic_data_sovereignty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(linguistic_data_sovereignty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(linguistic_data_sovereignty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(linguistic_data_sovereignty, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(linguistic_data_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(linguistic_data_sovereignty, TR),
    TR >= 0.70.

:- end_tests(linguistic_data_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The base extraction reflects that language technology companies capture substantial economic and strategic value from linguistic data without compensating source communities, while offering limited coordination benefit to those communities. The 0.58 value (rather than higher) acknowledges that some genuine documentation and computational advancement occurs; the extraction is not accompanied by zero benefit, but the benefit concentration is severely asymmetric. The measurement trajectory (0.28 → 0.58) shows rapid accumulation as the commercial value of multilingual AI has grown. Suppression (0.65): High. Indigenous language communities face substantial structural barriers to controlling their linguistic data: they lack legal/institutional frameworks for data governance, have limited technical capacity to enforce restrictions, face asymmetric power against global technology companies, experience pressure to participate in international documentation initiatives (framed as beneficial), and may lack awareness of how their data is being used. The suppression is not total (organized coalitions can and do negotiate), but it is severe. Theater ratio (0.48): Moderate. The constraint involves substantial performative justification ('this is preservation,' 'this benefits humanity,' 'science requires open data') but maintains partial genuine function (language documentation does advance linguistic science; computational tools do enable research). The theater has remained relatively stable because the preservation rhetoric is partially sincere — the constraint is not pure extraction with a thin veneer, but genuine mixed coordination and extraction where the performance exaggerates the coordination benefit relative to the actual outcome distribution.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same objective structural fact (the existence of linguistic data in commercial ML systems) classifies as mountain, rope, scaffold, piton, tangled rope, and snare depending on observer position. The indigenous speaker experiences snare because they have no meaningful exit and bear all costs. The documentation project experiences tangled rope because they have constrained exit and mixed costs/benefits. The tech company experiences rope because they have arbitrage options and net benefit. The advocacy coalition experiences scaffold because they have organized agency and a policy exit path. The analytical observer risks false summit by naturalizing language loss, treating the extraction mechanism as an immutable consequence of economic evolution rather than a policy-contingent institutional arrangement. The span of six different classifications from one constraint is the diagnostic signature of genuine structural complexity — not ambiguity in measurement, but genuinely different structural relationships to the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from the agent's beneficiary/victim status and exit options. Indigenous speakers with no exit (trapped) and victim status experience maximal d (→1.0), producing high f(d) → high experienced extraction. Documentation projects with constrained exit and mixed victim/beneficiary status experience moderate d (→0.55), producing moderate f(d) → moderate chi. Tech companies with arbitrage exit and beneficiary status experience low d (→0.20), producing negative f(d) → negative experienced extraction (subsidy). The advocacy coalition's organized status with constrained exit produces moderate-low d, allowing them to perceive the constraint as solvable rather than fixed. The measurement of suppression at 0.65 (unscaled, structural property) combines with these directionality values to produce the final effective extraction χ = 0.58 × f(d) × σ(global) that agents experience. Suppression does not scale with scope or power — it is the same for all agents — but its impact on perception varies with exit options: trapped agents experience suppression as total; organized agents experience it as high but negotiable.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The mandatrophy is resolved by recognizing that linguistic data sovereignty is a genuine tangled rope (not a pure snare or pure rope) and that the perspectival gap is the feature, not a bug. The constraint provides authentic coordination (enabling cross-language NLP, supporting documentation work, building shared linguistic resources) alongside genuine extraction (uncompensated data capture, IP concentration, economic asymmetry). The mandate of the constraint (coordinate language data for technical progress) is not betrayed by extraction; the extraction is embedded in the coordination structure itself. The false summit at the analytical context (treating extinction as natural law) is identifiable precisely because the structural data contradicts it: the constraint is policy-contingent and negotiable, not immutable. Mandatrophy is resolved when the system acknowledges that legitimate coordination can coexist with asymmetric extraction, and that policy interventions should target the extraction distribution mechanisms (benefit-sharing, community control, IPR reform) rather than attempting to eliminate the coordination function itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_and_benefit_sharing_mechanism,
    'What institutional mechanism would constitute genuine consent and equitable benefit-sharing for indigenous language data used in commercial AI systems?',
    'Pilot projects implementing community-controlled data stewardship with transparent benefit-sharing; comparison of outcomes across consent models (prior-informed, ongoing, extractive)',
    'If workable mechanisms exist: constraint downgrades to Rope or Scaffold (pure coordination or temporary problem). If mechanisms prove administratively impossible: extraction is structural, and the snare classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_and_benefit_sharing_mechanism, empirical, 'Feasibility of community-controlled benefit-sharing for language data').

omega_variable(
    language_preservation_paradox,
    'Does computational linguistic research actually preserve endangered languages, or does it accelerate their extinction by making documentation a substitute for transmission?',
    'Longitudinal tracking of speaker communities where language data is incorporated into AI systems vs communities where documentation remains isolated; measurement of intergenerational transmission rates',
    'If AI integration increases transmission: the constraint functions as genuine preservation coordination (Rope from community perspective). If transmission declines: the constraint is extraction disguised as preservation (Snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(language_preservation_paradox, empirical, 'Whether computational documentation accelerates or decelerates language extinction').

omega_variable(
    open_source_mitigation_effectiveness,
    'Do open-source language models and community-controlled NLP tools actually reduce the extractiveness of linguistic data use, or do they merely distribute extraction across more actors?',
    'Comparison of data reuse patterns: proprietary vs open-source model training pipelines; analysis of whether open-source models increase or decrease community agency over their linguistic data',
    'If open-source reduces extraction: organizational alternatives exist, and policy interventions could shift power. If open-source merely distributes extraction: the structural constraint is deeper than licensing models.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_mitigation_effectiveness, empirical, 'Whether open-source models reduce linguistic data extraction').

omega_variable(
    linguistic_sovereignty_definition_ambiguity,
    'Does linguistic sovereignty mean community control over data (institutional), community benefit-sharing from commercial use (economic), community agency in how their language is represented in technology (epistemic), or restoration of intergenerational transmission (cultural)?',
    'Community consensus-building exercises; mapping which definition resonates with different stakeholder groups; tracking whether policy implementations target the same dimension',
    'If definitions conflict: policy solutions optimizing for one dimension may worsen outcomes on another (e.g., maximizing data profits while decimating transmission). If convergent: aligned policy becomes possible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(linguistic_sovereignty_definition_ambiguity, conceptual, 'Definitional ambiguity in what linguistic sovereignty means').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(linguistic_data_sovereignty, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ling_ds_tr_t0, linguistic_data_sovereignty, theater_ratio, 0, 0.32).
narrative_ontology:measurement(ling_ds_tr_t5, linguistic_data_sovereignty, theater_ratio, 5, 0.4).
narrative_ontology:measurement(ling_ds_tr_t10, linguistic_data_sovereignty, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(ling_ds_be_t0, linguistic_data_sovereignty, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ling_ds_be_t5, linguistic_data_sovereignty, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(ling_ds_be_t10, linguistic_data_sovereignty, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(linguistic_data_sovereignty, resource_allocation).
narrative_ontology:affects_constraint(linguistic_data_sovereignty, ai_representational_colonialism).
narrative_ontology:affects_constraint(linguistic_data_sovereignty, language_technology_accessibility).
narrative_ontology:affects_constraint(linguistic_data_sovereignty, epistemic_sovereignty_over_linguistic_data).

% DUAL FORMULATION NOTE:
% Linguistic data sovereignty decomposes into three structurally distinct constraints: (1) the resource allocation/economic benefit-sharing problem (this story, ε=0.58); (2) the representational/epistemic problem of how minority languages are modeled in AI systems (higher ε, primarily extraction); (3) the technological accessibility problem of whether tools enable or depend on extracting community expertise. Each has different epsilon values reflecting different observable-dependent failure modes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(linguistic_data_sovereignty, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
