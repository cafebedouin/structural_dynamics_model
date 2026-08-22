% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__welfare_reading, []).

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
 *   constraint_id: animal_status_kernel__welfare_reading
 *   human_readable: Animal Sentience + Regulated Use (Welfare Reading)
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint instantiates the welfare reading of the contested
 *   animal_status_kernel. Under this reading, animals are recognized as
 *   sentient beings whose capacity to suffer is morally relevant, but their
 *   property status is retained and use is permitted if regulated to minimize
 *   suffering. The welfare reading creates a hybrid structure: it includes
 *   animals in the moral status framework (acknowledging sentience) while
 *   excluding them from rights frameworks (retaining property status). This
 *   generates the characteristic tangled_rope structure: genuine coordination
 *   (suffering-minimization) combined with asymmetric extraction (continued
 *   use despite recognized capacity to suffer). The constraint sits between
 *   the property reading (which treats animal interests as irrelevant) and
 *   the abolitionist reading (which treats property status itself as the
 *   injustice). The welfare reading is contested—abolitionists argue it
 *   legitimizes continued use under a false moral cover ('new welfarism');
 *   property theorists argue it is conceptually incoherent (property status
 *   is categorically exempt from the obligations welfare imposes).
 *
 * KEY AGENTS:
 *   - domesticated_animals: sentient victims whose suffering is acknowledged but use permitted (power: powerless, exit: trapped)
 *   - animal_agriculture_industry: institutional agenda-setter that enforces welfare standards while retaining use profits (power: institutional, exit: constrained)
 *   - animal_welfare_advocates: organized beneficiary locked in perpetual reform cycles (power: organized, exit: mobile)
 *   - abolitionist_moral_theorists: excluded analytical seat that argues the reading is false compromise (power: analytical, exit: analytical)
 *   - consumers_of_animal_products: organized beneficiaries who resolve moral discomfort through welfare certification (power: organized, exit: mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, 0.58).
domain_priors:suppression_score(animal_status_kernel__welfare_reading, 0.42).
domain_priors:theater_ratio(animal_status_kernel__welfare_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status_kernel__welfare_reading, "Animal Sentience + Regulated Use (Welfare Reading)").
narrative_ontology:topic_domain(animal_status_kernel__welfare_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__welfare_reading, '8f3ad50b-5b33-4ccf-83d6-1409890eee2a').
narrative_ontology:cs_kernel_codification('8f3ad50b-5b33-4ccf-83d6-1409890eee2a', distributed).
narrative_ontology:cs_authority_grounding('8f3ad50b-5b33-4ccf-83d6-1409890eee2a', distributed).
narrative_ontology:cs_reading_relation('8f3ad50b-5b33-4ccf-83d6-1409890eee2a', animal_status_kernel__property_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f3ad50b-5b33-4ccf-83d6-1409890eee2a', animal_status_kernel__abolitionist_reading, influences).
narrative_ontology:cs_axiom('8f3ad50b-5b33-4ccf-83d6-1409890eee2a', foundational, sentience_generates_moral_obligation).
narrative_ontology:cs_axiom_status(sentience_generates_moral_obligation, holdable).
narrative_ontology:cs_axiom_grounding('8f3ad50b-5b33-4ccf-83d6-1409890eee2a', sentience_generates_moral_obligation, deontological).
narrative_ontology:cs_axiom('8f3ad50b-5b33-4ccf-83d6-1409890eee2a', foundational, property_status_compatible_with_moral_constraint).
narrative_ontology:cs_axiom_status(property_status_compatible_with_moral_constraint, holdable).
narrative_ontology:cs_axiom_grounding('8f3ad50b-5b33-4ccf-83d6-1409890eee2a', property_status_compatible_with_moral_constraint, deontological).
narrative_ontology:cs_axiom('8f3ad50b-5b33-4ccf-83d6-1409890eee2a', secondary, suffering_minimization_implementable_middle_path).
narrative_ontology:cs_axiom_status(suffering_minimization_implementable_middle_path, holdable).
narrative_ontology:cs_axiom_grounding('8f3ad50b-5b33-4ccf-83d6-1409890eee2a', suffering_minimization_implementable_middle_path, instrumental).
narrative_ontology:cs_reference_frame('8f3ad50b-5b33-4ccf-83d6-1409890eee2a', sentience_constrained_property).
narrative_ontology:cs_drift_state('8f3ad50b-5b33-4ccf-83d6-1409890eee2a', contemporary_regulation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8f3ad50b-5b33-4ccf-83d6-1409890eee2a', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__welfare_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, animal_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, consumers_of_animal_products).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, domesticated_animals).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, wild_animals_displaced_by_agriculture).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, animal_welfare_advocates).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, consumers_of_animal_products).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, animal_welfare_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sentient beings whose suffering is recognized as morally relevant under this reading, but whose property status remains unchanged. Welfare regulations constrain the conditions of their use but do not prohibit use itself. They cannot exit the relationship; their suffering is acknowledged but instrumentally subordinated to human interests.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, domesticated_animals, payer,
    powerless, biographical, trapped, global).

% Sets welfare standards, enforces compliance through certification and audit, and argues that regulated use is the pragmatic alternative to prohibition. Bears the costs of welfare compliance (larger enclosures, slower growth rates, less dense stocking) but retains the primary economic benefit of animal commodification. Collects rents from labeling animals as 'humanely' produced.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_agriculture_industry, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from continued animal research under welfare constraints (institutional review boards, pain-minimization protocols). Retain research productivity while operating within acknowledged suffering-minimization frameworks. Face reduced regulatory pressure compared to prohibition scenarios.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_research_institutions, beneficiary,
    institutional, generational, constrained, global).

% Benefit from continued access to animal products at reduced moral cost through welfare labeling and assurance. Pay marginally higher prices for 'humanely' produced goods, which resolves their moral discomfort without requiring dietary change. Their consumption practices remain substantively unchanged.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, consumers_of_animal_products, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, consumers_of_animal_products, payer).

% Win recognition of sentience and welfare constraints, which validates their advocacy and resources their organizational work. But their victory is constrained: use persists, suffering continues. They remain locked in perpetual reform cycles (larger cages, better slaughter methods) rather than achieving categorical change.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_welfare_advocates, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, animal_welfare_advocates, payer).

% Bear the costs of agricultural expansion (habitat destruction, displacement, starvation) that the welfare reading does not address. The sentience-based framework recognizes their capacity to suffer but welfare regulations focus on managed animals, leaving wild-animal suffering unregulated and often invisible in welfare discourse.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, wild_animals_displaced_by_agriculture, payer,
    powerless, biographical, trapped, global).

% Are structurally excluded from the welfare reading's authority structure. They argue the reading is a false compromise that legitimates continued use under the guise of sentience recognition ('new welfarism'). Their core claim—that property status itself is the injustice—is ruled out of order in welfare-constrained frameworks.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, abolitionist_moral_theorists, excluded,
    analytical, generational, analytical, global).

% Also excluded from the welfare reading's framework. They argue welfare constraints on property are incoherent—that sentience-based constraints on use constitute a category mistake (property relations do not carry moral obligations to the property itself). Their position denies the reading's foundational premise.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, property_theorists, excluded,
    analytical, generational, analytical, global).

% Codify and enforce welfare standards (animal protection laws, slaughter regulations, research protocols). Operate within the welfare reading's framework, treating sentience as morally relevant but property status as retained. Enforce the boundary between permissible and impermissible use.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, regulatory_authorities, agenda_setter,
    institutional, generational, constrained, national).

% The welfare reading vindicates and institutionalizes the proposition that animal sentience is morally relevant. This consensus object does not collect rents but legitimates the entire constraint structure.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, sentience_moral_relevance, beneficiary,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(animal_status_kernel__welfare_reading, sentience_moral_relevance).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__welfare_reading, animal_agriculture_industry).
narrative_ontology:fixing_cost_class(animal_status_kernel__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves a moral coordination problem: how to acknowledge animal sentience as morally relevant while preserving the economic and cultural systems that depend on animal use. The welfare reading coordinates around suffering-minimization as the implementable middle ground between prohibition and unrestricted use.
% TRANSFER_FUNCTION: Moves moral acknowledgment (recognition of sentience, suffering-capacity as relevant) from animals to the use framework itself. Animals gain formal recognition as subjects of moral concern; humans retain the authority to determine whether and how that concern is overridable by other interests. Use permissions flow from industry to consumers; welfare costs flow from industry (and marginally from consumers) back to compliance infrastructure.
% ABSENT_VOICES: Abolitionists and property theorists are excluded from the welfare reading's authority structure—they would argue the reading is either false compromise (abolitionists) or category mistake (property theorists). Non-human animals themselves cannot voice whether welfare constraints adequately address their interests; their suffering is recognized but they remain unable to consent to continued use or refuse it.
% DISAPPEARANCE_RATIONALE: If the welfare reading and its enforcement infrastructure disappeared, the constraint structure would shift: property law would revert to unqualified ownership (property reading) or move toward rights-based abolition (abolitionist reading). The recognition of sentience as morally relevant would be suspended, animal agriculture would operate under fewer welfare obligations, and the moral validation consumers receive from 'humane' labels would evaporate. Institutional practices in research and agriculture would reorganize around whichever reading replaced this one.
% FOUNDING_PROBLEM: How can a society that has come to recognize animal sentience morally continue practices that cause animal suffering? The welfare reading was constructed to resolve the cognitive dissonance between sentience-recognition and continued use: by acknowledging suffering as morally relevant while establishing that use is acceptable under welfare constraints.
% FOUNDING_PROBLEM_CORROBORATION: Welfare advocates and parts of the regulatory apparatus attest the founding problem is live and the reading solves it. Abolitionists attest the reading solves only the cognitive dissonance for humans, not the underlying injustice—and that welfare reforms may have intensified the problem by making continued use more socially acceptable. Property theorists attest there is no foundational problem (sentience does not generate moral obligation on property). Independent philosophical analysis (Singer, Regan, Adams) documents the historical emergence of the problem in late-20th-century ethics; corroborating sources outside the welfare benefiting parties point to the problem's reality.
narrative_ontology:disappearance_verdict(animal_status_kernel__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status_kernel__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__welfare_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__welfare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) because the welfare reading acknowledges animal suffering as morally relevant and imposes costs on the industry (larger enclosures, slower growth, welfare compliance infrastructure). However, the constraint PERMITS continued use, which prevents extractiveness from reaching the high levels of pure property readings or pure predation. The trajectory shows slight increase from 0.45 to 0.58 over the interval, reflecting intensifying industry costs as welfare standards tighten and consumer expectations rise—but the curve flattens after t=25, suggesting a plateau where welfare costs stabilize. Suppression is moderate-low (0.42) because the constraint does not require hiding animal use; rather, it requires acknowledging it within welfare parameters. The suppression is primarily directed at excluding abolitionist readings (keeping them out of the regulatory frame) and at suppressing industry resistance to welfare costs. Theater_ratio rises from 0.32 to a plateau around 0.48, reflecting the growth of welfare labeling and public-facing narratives ('humane,' 'ethical,' 'sustainable') that perform moral seriousness while use persists. This is neither pure theater (welfare constraints are real, impose costs) nor purely functional (much energy goes to consumer reassurance rather than animal outcome improvement). The measurement series is authored on a single shared time grid: every metric is valued at every time point, enabling phase-coherent analysis.
 *
 * PERSPECTIVAL GAP:
 *   The industry seat (agenda-setter) and the animal victims seat compute sharply different types from the structural data. From the industry perspective, the welfare reading is genuine coordination: it solved the moral problem that was beginning to constrain their business model, provided a framework for continued operation, and transformed moral pressure into a market segment ('premium humane products'). From the trapped-animal perspective, sentience recognition without use permission is false benefit—the reading creates the appearance of moral consideration while use persists. From the welfare-advocate perspective (excluded but organized), the reading is half-victory: sentience is vindicated but use is not prohibited, leaving them in perpetual lobbying cycles. The engine computes these divergent d values from the structural data (beneficiary vs. victim vs. excluded roles, power atoms, exit options)—the authored claim does not presuppose which computation is correct; it names the reading that the story instantiates.
 *
 * DIRECTIONALITY LOGIC:
 *   The domesticated_animals bear the asymmetry: they are the identified victims (included in the victim-set via sentience-recognition) but lack directionality toward change (trapped exit, powerless position). Their d is near 1.0 (full target). The animal_agriculture_industry benefits from the welfare reading (it solved their moral legitimacy crisis) while bearing regulatory costs—their d is intermediate, between moderate and powerful positions, perhaps 0.35-0.45. Welfare_advocates sit in the beneficiary role (sentience vindicated, their positions institutionalized) but remain structurally excluded from agenda-setting—their d is moderate, perhaps 0.4-0.5 (they won something but retained no power to enforce it). Consumers gain cheap moral resolution without behavioral change—their d is low, near 0.2-0.3 (subsidized beneficiaries). The abolitionist_theorists are excluded from the frame entirely (role: excluded, power: analytical), so their d is not computed from the structural data—they sit outside the constraint's derivation chain. This mosaic of d values emerges from the beneficiary/victim declarations and exit modulation without additional override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (how to acknowledge sentience while continuing use) is still LIVE and the reading still addresses it—but the reading's success at resolving the problem is CONTESTED. Abolitionists argue the reading doesn't solve the problem but camouflages it ('new welfarism'—welfare reforms make people comfortable with continued use, thereby increasing consumption and total suffering). If abolitionists are correct and welfare reforms increase total suffering, the founding_problem has not actually been solved, just restyled. This mandatrophy candidate (a mandate to minimize suffering that results in increased suffering via consumption expansion) drives the new_welfarism_mechanism omega. The constraint avoids full mandatrophy because welfare regulations DO impose costs on the industry and DO constrain the worst practices—they are not purely theatrical. But the gap between mandated outcome (minimized suffering) and actual outcome (possibly increased total suffering) is real and unresolved, which justifies the contested status on founding_problem_status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the welfare reading a distinct philosophical position or a unstable compromise between property and abolitionist readings?',
    'Philosophical analysis of whether sentience-recognition plus property-retention form a stable equilibrium or necessarily collapse into one of the sibling readings under pressure. Empirical observation of whether welfare frameworks expand toward abolition or contract toward property-dominance over time.',
    'If the welfare reading is inherently unstable, the constraint may reclassify as a transitional framework (scaffold) rather than a stable tangled-rope. This would affect long-term classification and policy interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether welfare reading is philosophically stable or inherently transitional.').

omega_variable(
    new_welfarism_mechanism,
    'Do welfare regulations legitimize continued use in a way that increases social comfort with animal agriculture (''happy meat'' effect), thereby actually increasing total animal suffering by expanding consumption?',
    'Empirical time-series analysis: does welfare certification correlate with expansion of animal agriculture, or does it reduce consumption relative to counterfactual unregulated industry? Experimental studies on consumer behavior and moral disengagement.',
    'If welfare reforms increase consumption more than they decrease suffering-per-animal, the constraint''s effective extractiveness could be substantially higher than the authored 0.58—the regulation mechanism would amplify rather than constrain harm. This would argue for reclassification as snare (extraction camouflaged as welfare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(new_welfarism_mechanism, empirical, 'Whether welfare regulations reduce or increase total animal suffering through consumption effects.').

omega_variable(
    wild_animal_scope_gap,
    'Does the welfare reading''s focus on domesticated animals systematically exclude consideration of wild animals displaced by agriculture, creating a hidden victim set?',
    'Scope expansion: does the welfare reading''s sentience-based framework logically commit to addressing wild-animal suffering from agricultural habitat destruction, or does it treat wild animals as outside the domain of welfare regulation?',
    'If wild-animal suffering is systematically excluded despite falling within the sentience framework, the reading''s coherence is compromised. This would support an omega about the reading''s internal logical consistency and whether it should be reclassified as snare (selective inclusion of victims).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wild_animal_scope_gap, conceptual, 'Whether sentience-based framework includes wild animals or systematically excludes them.').

omega_variable(
    suppression_internalization,
    'Is the measured suppression (0.42) structural (external enforcement, legal barriers to exit) or partly internalized (animals have been shaped by domestication to accept confinement, humans have internalized the acceptability of use)?',
    'Post-domestication trajectory analysis: if animals were released from agricultural systems, would they resist or accept confinement? Cognitive science on learned helplessness in confined animals; historical analysis of how domestication shapes behavioral adaptation.',
    'If suppression is substantially internalized, the constraint''s effective suppression may be lower than 0.42 (the scaffolding is internal, not external), which could lower the tangled-rope classification toward rope. If suppression is structural, the 0.42 is appropriate. This affects whether the constraint is self-maintaining or dependent on external enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Degree of structural vs. internalized suppression in animal confinement.').

omega_variable(
    welfare_reading_vs_sibling_coverage,
    'This constraint instantiates one reading of the animal_status_kernel. The property_reading (animals are property with no intrinsic moral obligations) and abolitionist_reading (animals have rights not to be property) are OTHER constraints. Does the welfare reading coexist with the property reading in contemporary regulatory practice, or has welfare recognition begun to foreclose property-absolutism in actual institutions?',
    'Institutional analysis of regulatory frameworks: do jurisdictions hold both welfare constraints AND unqualified property-law status simultaneously, or is property status being legally modified by welfare obligations? Compare legal doctrine across jurisdictions (EU, US, emerging animal-law scholarship).',
    'If welfare recognition is beginning to foreclose unqualified property status in practice, the reading_relations should shift from coexists_with to influences on the property reading. This would affect how the constraint family evolves and whether the welfare reading is a stable equilibrium or a ratchet toward abolition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_reading_vs_sibling_coverage, conceptual, 'Whether welfare reading coexists with property reading or has begun to foreclose it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__welfare_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__welfare_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(anim_tr_t5, animal_status_kernel__welfare_reading, theater_ratio, 5, 0.37).
narrative_ontology:measurement(anim_tr_t10, animal_status_kernel__welfare_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(anim_tr_t15, animal_status_kernel__welfare_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement(anim_tr_t20, animal_status_kernel__welfare_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement(anim_tr_t25, animal_status_kernel__welfare_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement(anim_tr_t30, animal_status_kernel__welfare_reading, theater_ratio, 30, 0.49).
narrative_ontology:measurement(anim_tr_t40, animal_status_kernel__welfare_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__welfare_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(anim_be_t5, animal_status_kernel__welfare_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(anim_be_t10, animal_status_kernel__welfare_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(anim_be_t15, animal_status_kernel__welfare_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(anim_be_t20, animal_status_kernel__welfare_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(anim_be_t25, animal_status_kernel__welfare_reading, base_extractiveness, 25, 0.59).
narrative_ontology:measurement(anim_be_t30, animal_status_kernel__welfare_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(anim_be_t40, animal_status_kernel__welfare_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__welfare_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(anim_su_t5, animal_status_kernel__welfare_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(anim_su_t10, animal_status_kernel__welfare_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(anim_su_t15, animal_status_kernel__welfare_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(anim_su_t20, animal_status_kernel__welfare_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(anim_su_t25, animal_status_kernel__welfare_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(anim_su_t30, animal_status_kernel__welfare_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(anim_su_t40, animal_status_kernel__welfare_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__welfare_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__welfare_reading, 0.12).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The animal_status_kernel decomposes into three readings, each a distinct constraint with different ε values and victim/beneficiary structures. property_reading has low ε (property interests override animal interests, no extraction from the property frame itself). abolitionist_reading has high ε (property status is the injustice, use categorically impermissible, complete extraction from the animal perspective). welfare_reading has moderate ε (sentience acknowledged, use constrained but permitted, partial extraction). These are NOT the same constraint viewed from different angles—their ε values differ substantially because they have different assessments of what the standing arrangement permits and requires. The welfare reading coexists with the property reading in practice and influences the abolitionist reading by expanding sentience-recognition pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status_kernel__welfare_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
