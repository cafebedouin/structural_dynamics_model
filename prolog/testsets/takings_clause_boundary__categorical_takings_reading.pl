% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__categorical_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__categorical_takings_reading, []).

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
 *   constraint_id: takings_clause_boundary__categorical_takings_reading
 *   human_readable: Categorical Takings Rule: Per Se Categories + Penn Central Balancing
 *   domain: constitutional/property
 *
 * SUMMARY:
 *   The takings clause ('nor shall private property be taken for public use,
 *   without just compensation') is grounded in a contested kernel about when
 *   regulations become takings. This story instantiates the CATEGORICAL
 *   TAKINGS READING: permanent physical occupations and total economic value
 *   eliminations trigger automatic per se takings (bright-line certainty at
 *   the poles), while all other regulations face multifactorial Penn Central
 *   balancing (contextual analysis in the middle ground). This reading
 *   emerges from Lucas v. South Carolina Coastal Council (1992) and
 *   represents the dominant modern doctrine, structurally distinct from the
 *   PHYSICAL APPROPRIATION READING (only direct seizure counts) and the
 *   REGULATORY TAKINGS READING (degree-of-diminishment alone determines
 *   taking status without categorical tiers). The categorical reading
 *   attempts to stabilize property owner expectations at the extremes while
 *   preserving regulatory flexibility in between—a hybrid
 *   coordination/extraction structure that benefits those at the poles while
 *   imposing uncertainty and litigation costs on those in the contested
 *   middle.
 *
 * KEY AGENTS:
 *   - Property owners with permanent occupations or total value elimination: gain automatic per se protection under categorical rule
 *   - Property owners in middle ground (reduced but not eliminated value): face Penn Central balancing with uncertain outcomes
 *   - Regulatory agencies: operate under bright-line constraints at poles, discretionary authority in middle
 *   - Property rights advocates: litigate to expand categorical protection or establish pro-owner Penn Central doctrine
 *   - Environmental and planning advocates: constrained by categorical rules but can work in middle ground
 *   - Courts: apply automatic compensation rule at poles, fact-intensive balancing in middle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, 0.58).
domain_priors:suppression_score(takings_clause_boundary__categorical_takings_reading, 0.41).
domain_priors:theater_ratio(takings_clause_boundary__categorical_takings_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__categorical_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__categorical_takings_reading, "Categorical Takings Rule: Per Se Categories + Penn Central Balancing").
narrative_ontology:topic_domain(takings_clause_boundary__categorical_takings_reading, "constitutional/property").

domain_priors:requires_active_enforcement(takings_clause_boundary__categorical_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__categorical_takings_reading, '5752bfab-9a27-4dcf-979b-dc08dc65a16a').
narrative_ontology:cs_kernel_codification('5752bfab-9a27-4dcf-979b-dc08dc65a16a', fixed_text).
narrative_ontology:cs_authority_grounding('5752bfab-9a27-4dcf-979b-dc08dc65a16a', lineage).
narrative_ontology:cs_interpretation_layer_present('5752bfab-9a27-4dcf-979b-dc08dc65a16a').
narrative_ontology:cs_reading_relation('5752bfab-9a27-4dcf-979b-dc08dc65a16a', takings_clause_boundary__physical_appropriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('5752bfab-9a27-4dcf-979b-dc08dc65a16a', takings_clause_boundary__regulatory_takings_reading, influences).
narrative_ontology:cs_axiom('5752bfab-9a27-4dcf-979b-dc08dc65a16a', foundational, bright_line_categorical_poles).
narrative_ontology:cs_axiom_status(bright_line_categorical_poles, holdable).
narrative_ontology:cs_axiom_grounding('5752bfab-9a27-4dcf-979b-dc08dc65a16a', bright_line_categorical_poles, deontological).
narrative_ontology:cs_axiom('5752bfab-9a27-4dcf-979b-dc08dc65a16a', secondary, penn_central_contextual_middle_ground).
narrative_ontology:cs_axiom_status(penn_central_contextual_middle_ground, holdable).
narrative_ontology:cs_axiom_grounding('5752bfab-9a27-4dcf-979b-dc08dc65a16a', penn_central_contextual_middle_ground, instrumental).
narrative_ontology:cs_reference_frame('5752bfab-9a27-4dcf-979b-dc08dc65a16a', constitutional_property_protection_framework).
narrative_ontology:cs_drift_state('5752bfab-9a27-4dcf-979b-dc08dc65a16a', contemporary_environmental_and_regulatory_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5752bfab-9a27-4dcf-979b-dc08dc65a16a', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, property_owners_at_poles).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, regulatory_predictability_beneficiaries).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, property_owners_in_middle_ground).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, regulatory_experimenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, property_owners_with_permanent_occupation).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, property_owners_with_total_value_elimination).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, property_rights_litigators_and_advocates).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, environmental_and_land_planning_advocates).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, lower_courts_and_administrative_tribunals).
narrative_ontology:constraint_vindicates(takings_clause_boundary__categorical_takings_reading, takings_clause_constitutional_protection).
narrative_ontology:constraint_vindicates(takings_clause_boundary__categorical_takings_reading, regulatory_takings_doctrine_existence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own property subject to permanent physical occupation (transmission easements, permanent infrastructure, utility corridors). Receive categorical per se takings protection: occupation automatically triggers compensation without requirement to prove economic loss or satisfy Penn Central factors. Their position is brightened by the categorical rule—they know their entitlement and can enforce it without expensive litigation over multifactorial balancing.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners_with_permanent_occupation, beneficiary,
    moderate, generational, trapped, national).

% Own property regulated to total economic worthlessness (conservation lands rendered developmentally impossible, environmental regulations eliminating all economic use). Receive categorical per se takings protection: total value elimination automatically triggers compensation. Their position gains certainty through the bright-line rule—they are protected from complete regulatory confiscation without compensation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners_with_total_value_elimination, beneficiary,
    moderate, generational, trapped, national).

% Own property subject to regulations that reduce but do not eliminate economic value and involve no permanent physical occupation (typical zoning restrictions, environmental setback requirements, development limitations that preserve some residual use). Must prove a taking through Penn Central multifactorial analysis: examining character of governmental action, economic impact on the property owner, and interference with investment-backed expectations. Face substantial litigation risk; outcomes are uncertain and fact-dependent. The categorical rule provides them no protection.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners_in_middle_ground, payer,
    moderate, biographical, constrained, national).

% Implement land-use, environmental, conservation, and development regulations. The categorical rule creates bright-line exposure: regulations that involve permanent physical occupations or eliminate all economic value automatically trigger compensation obligations. They preserve flexibility in the middle ground where Penn Central balancing applies. They must either avoid the categorical poles, budget compensation when regulations hit them, or defend middle-ground regulations through factual development showing takings factors weigh against compensation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, regulatory_agencies, agenda_setter,
    institutional, generational, mobile, national).

% Litigate takings claims on behalf of property owners, advance property protection through case law development, and advocate for expansion of categorical and regulatory takings doctrine. The categorical reading creates clear winning positions at the poles (automatic compensation for permanent occupations and total value elimination) and contestable but structurable claims in the middle ground (Penn Central factors allow factual development favoring owners). They organize strategy around the categorical boundaries.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_rights_litigators_and_advocates, beneficiary,
    organized, generational, mobile, national).

% Support environmental protection, conservation, and land-use planning. The categorical per se rule for total value elimination constrains them: conservation regulations that eliminate all development use trigger automatic compensation. They must either structure regulations to preserve some economic use (reducing conservation effectiveness), budget compensation for landowners, or litigate under Penn Central arguing the regulation serves compelling state interests and does not eliminate all value. The categorical rule's bright-line exposure to total-value takings increases regulatory costs.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, environmental_and_land_planning_advocates, payer,
    organized, generational, mobile, national).

% Apply takings doctrine in property disputes and administrative review. The categorical rule creates clear decision rules at the poles: they must award compensation for permanent occupations and total value eliminations without Penn Central inquiry. The rule delegates the middle ground to multifactorial balancing under Penn Central factors. This distributes their decisional load: bright certainty at the extremes, open-ended fact-dependent analysis in between. Each judge must interpret three multifactorial elements, creating potential for inconsistency.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, lower_courts_and_administrative_tribunals, payer,
    powerful, biographical, constrained, national).

% Enact statutes and constitutional provisions governing property, regulation, and takings. Observe the categorical reading's operation through takings claims and compensation awards under their jurisdictions. Can modify the rule through amendment, can clarify what constitutes permanent occupation or total value elimination, can specify compensation standards, or can override the categorical rule entirely through legislation. Their legislative choice affects whether the categorical rule persists.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, state_legislatures_and_congress, observer,
    institutional, generational, mobile, national).

% Analyze, critique, and theorize takings doctrine. Debate whether the categorical rule is coherent, whether Penn Central factors are predictable, whether the two-tier system appropriately balances property protection with regulatory authority. Provide normative and doctrinal analysis that feeds back into litigation strategy and judicial decision-making. Observe the constraint's operation through the lens of rule-of-law values, federalism, and property theory.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, legal_academy_and_constitutional_commentators, observer,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__categorical_takings_reading, regulatory_agencies).
narrative_ontology:fixing_cost_class(takings_clause_boundary__categorical_takings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a two-tier framework for evaluating takings claims: (1) categorical per se rules that provide automatic compensation entitlements for permanent physical occupations and total economic value eliminations, and (2) multifactorial Penn Central balancing for all other regulations. The coordination problem solved is reconciling two conflicting demands: property owners need predictable rules about when regulation becomes a taking (so they can plan and invest), and government needs regulatory flexibility to pursue legitimate public goals without triggering takings liability for every significant regulation. The categorical rule attempts to satisfy both: bright-line certainty at the poles (property owners gain predictability; government knows automatic compensation applies), regulatory flexibility in the middle (government can regulate without automatic liability; property owners must litigate).
% TRANSFER_FUNCTION: Moves compensation obligations from property owners to government treasuries at the categorical poles (permanent occupations and total value eliminations automatically trigger compensation without property owners bearing proof burden). In the middle ground, the transfer is conditional and uncertain: property owners must litigate and prove a taking through Penn Central factors; government must defend the regulation's character, economic impact, and effect on investment-backed expectations. The transfer is asymmetric: categorical winners (pole property owners) receive automatic entitlements; middle-ground claimants bear litigation costs and face uncertain outcomes.
% ABSENT_VOICES: Property owners who benefit from regulations (adjacent landowners protected by setbacks, communities gaining environmental amenities, development-prevented residents opposing gentrification) are not seats in takings analysis. Renters and non-property-owning stakeholders affected by regulations (housing costs, development patterns, environmental protection) are wholly outside the takings framework. Indigenous communities and nations whose land relationship conflicts with property-owner takings claims are structurally excluded. Future generations and non-human ecological stakeholders are absent. The takings clause frames property owners as the exclusive constituency; all other affected parties appear only if they organize opposition to compensation claims.
% DISAPPEARANCE_RATIONALE: If the categorical takings rule disappeared, property owners would lose automatic compensation rights for permanent occupations and total value eliminations, facing Penn Central balancing for all regulations. Government would gain broader regulatory freedom (fewer automatic obligations). Compensation litigation would increase as property owners litigate every significant regulation. Development patterns would shift as investment expectations changed. Regulatory budgets and takings-defense capacity would reallocate across jurisdictions. The entire constitutional property landscape would reorganize around pure contextual takings analysis without categorical anchors.
% FOUNDING_PROBLEM: Early takings doctrine left property owners uncertain about which regulations triggered compensation obligations, creating investment risk and discouraging efficient property use. Regulatory agencies operated without clear guidance about compensation triggers. Courts faced fact-intensive takings analysis for every regulation with no stable doctrine. The categorical rule was developed to provide bright-line clarity for extreme cases (permanent occupation has long been a taking; total value elimination was recognized as per se in Lucas 1992) while preserving contextual analysis for regulations in between.
% FOUNDING_PROBLEM_CORROBORATION: Property rights litigators and landowner organizations attest the founding problem remains live: middle-ground regulations still create uncertainty about takings outcomes. Environmental and planning advocates contest whether the founding problem is real, arguing that regulatory uncertainty is appropriate (discouraging excessive restrictions) or that categorical rules create worse problems (forcing compensation for legitimate regulation). Courts have reaffirmed categorical poles' continued necessity (permanent occupation unanimously recognized; total value elimination strongly supported post-Lucas). No external corroboration from non-legal-system sources; international comparative property law shows different approaches (some jurisdictions have categorical rules, others pure contextual analysis, still others no takings doctrine), but no consensus on which approach better solves the founding problem.
narrative_ontology:disappearance_verdict(takings_clause_boundary__categorical_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__categorical_takings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__categorical_takings_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(takings_clause_boundary__categorical_takings_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__categorical_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__categorical_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) and rising over the interval. The constraint extracts from property owners in the middle ground who must litigate uncertain Penn Central claims while receiving no categorical protection; it extracts from regulatory agencies who must either avoid the categorical poles or budget compensation. The constraint coordinates a two-tier system that stabilizes poles while preserving regulatory flexibility, but this coordination comes at the cost of middle-ground uncertainty. Suppression is modest (0.41) and stable: courts enforce the categorical poles actively, and middle-ground cases are genuinely contestable (neither party is systematically suppressed). Theater is low (0.22) and rising: the categorical rule genuinely does clarify law at the poles, but as the corpus of middle-ground Penn Central cases has grown, more of the rule's apparent function becomes theater—the multifactorial balancing leaves outcomes unpredictable, and the categorical framework obscures rather than clarifies the decisive factors. Rising theater ratio reflects the growing gap between the rule's appearance of clarity and its actual operation in practice.
 *
 * PERSPECTIVAL GAP:
 *   From a property owner at a categorical pole (permanent occupation), the constraint is a rope: clear coordination rule that protects them. From a property owner in the middle ground, the constraint is a snare: the categorical rule's existence suggests a coherent doctrine, but Penn Central's multifactorial character means they must litigate extensively without knowing the outcome. From a regulatory agency, the constraint appears as tangled rope: they coordinate on the categorical poles (automatic compensation for permanent occupations) but extract uncertainty costs from middle-ground property owners.
 *
 * DIRECTIONALITY LOGIC:
 *   Categorical-pole property owners (permanent occupations, total value elimination) have low directionality (d near 0.2): they benefit from clear, automatic compensation rights without litigation risk or burden of proof. Property owners in the middle ground have high directionality (d near 0.8): they bear the cost of uncertainty, face multifactorial balancing that requires expensive litigation, and have no guarantee of compensation even when regulations significantly impair value. Regulatory agencies have mixed directionality: they benefit from middle-ground flexibility (d ~0.3) but bear categorical compensation costs (d ~0.7). The structural beneficiary group consists of categorical-pole property owners and those regulatory agencies exploiting middle-ground flexibility; the victim group consists of middle-ground property owners and agencies forced to compensate at the poles. This creates the tangled-rope signature: genuine coordination function (stabilizing expectations) paired with asymmetric extraction (middle-ground property owners subsidizing categorical certainty and regulatory flexibility).
 *
 * MANDATROPHY ANALYSIS:
 *   The categorical reading shows no mandatrophy: the founding problem (uncertainty about takings triggers) remains live and contested. The categorical rule was built to solve that problem, and the rule itself remains operative and enforced. Whether it solves the problem is contested: property rights advocates say it clarifies law and should be expanded; regulatory advocates say multifactorial balancing is appropriate and the categorical rule should be narrowed. The constraint has not outlived its mandate; rather, the mandate itself (defining which regulations are takings) has proven harder to execute than the categorical framework initially suggested. This is tangled-rope persistence with rising theater: the rule maintains appearance of clarity while operational uncertainty grows.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_contextual_coherence,
    'Is the two-tier system of categorical per se rules plus Penn Central balancing theoretically coherent, or does it embed an unresolved tension between formalism and contextualism?',
    'Jurisprudential analysis of whether the categorical poles are justified by intrinsic features of property rights or by administrative convenience; review of whether Penn Central factors can be applied consistently across diverse property and regulatory contexts.',
    'If incoherent, the framework masks rather than solves the underlying takings question; the reading should be reclassified toward snare (theatrical categorical clarity over buried uncertainty). If coherent, the framework successfully balances predictability with flexibility; the reading remains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_contextual_coherence, conceptual, 'Whether the categorical/contextual split is theoretically defensible or ad-hoc.').

omega_variable(
    penn_central_predictability,
    'Can property owners and regulatory agencies reliably predict Penn Central outcomes across the middle ground, or does the three-factor test''s open-ended character render it essentially unpredictable?',
    'Empirical analysis of Penn Central litigation outcomes: do courts apply consistent weights to the three factors, or do outcomes correlate more with court ideology or judge identity than with objective factor evaluation?',
    'If predictable, the middle ground provides workable guidance and the constraint serves its coordination function; middle-ground extractiveness drops. If unpredictable, the categorical rule''s appearance of clarity masks underlying arbitrariness in the middle; extractiveness is higher than metrics reflect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(penn_central_predictability, empirical, 'Degree of predictability in Penn Central multifactorial balancing.').

omega_variable(
    total_value_elimination_boundary,
    'What constitutes ''total economic loss'' for purposes of the per se rule—must a property have ZERO economic value, or is severe diminishment (99% loss, minimal residual use) sufficient to trigger the categorical rule?',
    'Analysis of post-Lucas case law applying the total-value rule; review of how courts distinguish between total loss and near-total loss, and whether the boundary is stable or shifting.',
    'If the boundary is permeable or undefined, the categorical rule bleeds into the middle ground; uncertainty expands and the coordination function degrades. If the boundary is crisp, the categorical rule maintains clarity. This is a reading-internal coherence question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(total_value_elimination_boundary, empirical, 'Operationalization of ''total economic loss'' in per se takings rule.').

omega_variable(
    permanent_occupation_scope,
    'How far does ''permanent physical occupation'' extend—does it include temporary occupations, easements for infrastructure that don''t occupy the surface, or invisible occupation (underground conduits)? Where does the boundary lie?',
    'Review of case law on what physical occupations trigger per se rule; analysis of whether courts have clarified or muddied the boundary since Loretto.',
    'If the boundary is stable and well-defined, the categorical rule provides clear protection for occupations; if muddied, regulatory uncertainty spreads into this supposedly categorical domain. Boundary clarity directly affects whether the rule''s apparent coordination function is real.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(permanent_occupation_scope, empirical, 'Scope and operationalization of permanent physical occupation doctrine.').

omega_variable(
    regulatory_takings_kernel_contest,
    'Is this reading genuinely a single constraint, or does it embed an unresolved contest between two incompatible understandings of the takings clause—one categorical/formalist and one regulatory/contextual?',
    'Analysis of whether the categorical and contextual tiers can coexist in a single coherent doctrine, or whether courts applying the reading effectively choose between them depending on ideological preference.',
    'If the tiers are separable into genuinely different constraints (one categorical, one regulatory), this story should decompose into two; if they are inseparable, the reading remains intact but with acknowledged internal tension (omega resolves via acknowledged framework contest). This is a reading-identity question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_takings_kernel_contest, conceptual, 'Whether categorical and regulatory takings are a single unified reading or two incompatible frameworks unified only by label.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__categorical_takings_reading, 1987, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1987, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1987, 0.08).
narrative_ontology:measurement_basis(taki_tr_t1987, observed).
narrative_ontology:measurement(taki_tr_t1992, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1992, 0.1).
narrative_ontology:measurement_basis(taki_tr_t1992, observed).
narrative_ontology:measurement(taki_tr_t2000, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement_basis(taki_tr_t2000, observed).
narrative_ontology:measurement(taki_tr_t2010, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement_basis(taki_tr_t2010, observed).
narrative_ontology:measurement(taki_tr_t2018, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2018, 0.21).
narrative_ontology:measurement_basis(taki_tr_t2018, observed).
narrative_ontology:measurement(taki_tr_t2024, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(taki_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(taki_be_t1987, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1987, 0.42).
narrative_ontology:measurement_basis(taki_be_t1987, observed).
narrative_ontology:measurement(taki_be_t1992, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1992, 0.48).
narrative_ontology:measurement_basis(taki_be_t1992, observed).
narrative_ontology:measurement(taki_be_t2000, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2000, 0.54).
narrative_ontology:measurement_basis(taki_be_t2000, observed).
narrative_ontology:measurement(taki_be_t2010, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2010, 0.56).
narrative_ontology:measurement_basis(taki_be_t2010, observed).
narrative_ontology:measurement(taki_be_t2018, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2018, 0.57).
narrative_ontology:measurement_basis(taki_be_t2018, observed).
narrative_ontology:measurement(taki_be_t2024, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement_basis(taki_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1987, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1987, 0.38).
narrative_ontology:measurement_basis(taki_su_t1987, observed).
narrative_ontology:measurement(taki_su_t1992, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1992, 0.39).
narrative_ontology:measurement_basis(taki_su_t1992, observed).
narrative_ontology:measurement(taki_su_t2000, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement_basis(taki_su_t2000, observed).
narrative_ontology:measurement(taki_su_t2010, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement_basis(taki_su_t2010, observed).
narrative_ontology:measurement(taki_su_t2018, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2018, 0.41).
narrative_ontology:measurement_basis(taki_su_t2018, observed).
narrative_ontology:measurement(taki_su_t2024, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2024, 0.41).
narrative_ontology:measurement_basis(taki_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__categorical_takings_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(takings_clause_boundary__categorical_takings_reading, 0.18).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__regulatory_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, lucas_total_value_elimination_doctrine).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, loretto_permanent_occupation_doctrine).

% DUAL FORMULATION NOTE:
% The takings_clause_boundary kernel contains three distinct constraint readings: categorical_takings_reading (this story), physical_appropriation_reading, and regulatory_takings_reading. Each reading instantiates a different operative doctrine about when regulations become takings. The categorical reading sits between the other two: it claims categorical rules exist (like physical-appropriation) but applies them to different triggers (permanent occupation and total loss, not all seizures), and it uses contextual balancing (like regulatory-takings) for middle cases but denies balancing determines outcomes at the poles. All three readings remain live in contemporary doctrine; they are held by different judicial coalitions and interact through precedent and statutory overrides.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(takings_clause_boundary__categorical_takings_reading, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
