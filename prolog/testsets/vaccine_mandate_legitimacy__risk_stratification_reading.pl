% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__risk_stratification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__risk_stratification_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vaccine_mandate_legitimacy__risk_stratification_reading
 *   human_readable: Vaccine Mandate Legitimacy — Risk Stratification Reading
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   A state imposes vaccine mandates conditional on actuarial risk
 *   stratification: high-risk populations (elderly, immunocompromised,
 *   healthcare workers) face mandates justified by externality of severe
 *   disease transmission; lower-risk populations face no mandate, or face it
 *   only under narrower conditions (e.g., occupational exposure). This
 *   reading instantiates the middle position in a three-way legal and ethical
 *   contest over vaccine mandate legitimacy. The bodily_autonomy_primacy
 *   reading denies mandate authority categorically; the public_health_primacy
 *   reading justifies blanket mandates by collective harm prevention; this
 *   reading accepts mandate legitimacy CONTINGENT on risk-proportionality,
 *   repudiating blanket mandates but defending targeted ones. The claim is
 *   tangled_rope because the constraint coordinates genuine public health
 *   risk reduction AND extracts compliance from populations whose individual
 *   risk is below the threshold that would justify intervention — the
 *   coordination and extraction components ride on the same enforcement
 *   mechanism.
 *
 * KEY AGENTS:
 *   - high_risk_populations (elderly, immunocompromised): benefit from mandate protection and healthcare system capacity preservation; face low personal autonomy cost because mandate targets the genuinely high-risk group
 *   - healthcare_workers: mandated; benefit from system capacity preservation and occupational disease reduction; bear compliance cost but rationally accept it given occupational risk exposure
 *   - low_risk_unvaccinated_individuals: face mandate or employment/access restrictions despite low individual disease risk; lose autonomy to bodily choice; bear the cost of state enforcement
 *   - vaccine_hesitant_moderate_risk_populations: face mandate that their risk profile does not justify; caught between the reading's proportionality standard and the reading's enforcement machinery (suppression requirement stays constant even where mandate scope should be narrower)
 *   - public_health_authorities: agenda-setters who define the threshold, enforce it, and defend the proportionality framing; benefit from enhanced legitimacy compared to blanket mandates but must defend threshold choices against both extremes
 *   - courts_and_constitutional_arbiters: observers who rule whether risk-stratification is constitutionally coherent and whether authorities actually implement it proportionately
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, 0.58).
domain_priors:suppression_score(vaccine_mandate_legitimacy__risk_stratification_reading, 0.62).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__risk_stratification_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__risk_stratification_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__risk_stratification_reading, "Vaccine Mandate Legitimacy — Risk Stratification Reading").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__risk_stratification_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__risk_stratification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__risk_stratification_reading, '092ce9a9-819e-40aa-b72e-251ad6122d08').
narrative_ontology:cs_kernel_codification('092ce9a9-819e-40aa-b72e-251ad6122d08', distributed).
narrative_ontology:cs_authority_grounding('092ce9a9-819e-40aa-b72e-251ad6122d08', extraction).
narrative_ontology:cs_interpretation_layer_present('092ce9a9-819e-40aa-b72e-251ad6122d08').
narrative_ontology:cs_reading_relation('092ce9a9-819e-40aa-b72e-251ad6122d08', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('092ce9a9-819e-40aa-b72e-251ad6122d08', vaccine_mandate_legitimacy__public_health_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('092ce9a9-819e-40aa-b72e-251ad6122d08', foundational, mandate_legitimacy_contingent_on_actuarial_proportionality).
narrative_ontology:cs_axiom_status(mandate_legitimacy_contingent_on_actuarial_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('092ce9a9-819e-40aa-b72e-251ad6122d08', mandate_legitimacy_contingent_on_actuarial_proportionality, deontological).
narrative_ontology:cs_axiom('092ce9a9-819e-40aa-b72e-251ad6122d08', foundational, blanket_mandates_fail_proportionality_requirement).
narrative_ontology:cs_axiom_status(blanket_mandates_fail_proportionality_requirement, holdable).
narrative_ontology:cs_axiom_grounding('092ce9a9-819e-40aa-b72e-251ad6122d08', blanket_mandates_fail_proportionality_requirement, deontological).
narrative_ontology:cs_axiom('092ce9a9-819e-40aa-b72e-251ad6122d08', secondary, unvaccinated_status_constitutes_legitimizable_externality_above_risk_threshold).
narrative_ontology:cs_axiom_status(unvaccinated_status_constitutes_legitimizable_externality_above_risk_threshold, holdable).
narrative_ontology:cs_axiom_grounding('092ce9a9-819e-40aa-b72e-251ad6122d08', unvaccinated_status_constitutes_legitimizable_externality_above_risk_threshold, empirically_contingent).
narrative_ontology:cs_reference_frame('092ce9a9-819e-40aa-b72e-251ad6122d08', proportionality_constrained_mandate_authority).
narrative_ontology:cs_drift_state('092ce9a9-819e-40aa-b72e-251ad6122d08', contemporary_post_pandemic_enforcement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('092ce9a9-819e-40aa-b72e-251ad6122d08', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, high_risk_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, healthcare_system_capacity).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_unvaccinated_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_hesitant_moderate_risk_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, healthcare_workers).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_vaccinated_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, healthcare_workers).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__risk_stratification_reading, proportionality_doctrine_in_public_health).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__risk_stratification_reading, risk_based_differentiation_in_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elderly, immunocompromised, and other epidemiologically high-risk individuals benefit from mandate requirement that surrounding populations achieve vaccination. They receive protection from disease transmission and healthcare system preservation (capacity not consumed by preventable severe disease). They have organized voice through patient advocacy, medical societies, and elderly advocacy organizations. Their exit option is mobility: they can choose to receive vaccination or to avoid high-contact activities; the mandate does not force them into a role they resist.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, high_risk_populations, beneficiary,
    organized, biographical, mobile, national).

% The capacity of healthcare systems to treat severe disease without overwhelming resources benefits from vaccination mandates. This is not an agent but a systemic good — included here for completeness because the mandate is justified partly by this benefit (preventing healthcare collapse). The constraint coordinates around preserving system capacity.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, healthcare_system_capacity, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(vaccine_mandate_legitimacy__risk_stratification_reading, healthcare_system_capacity).

% Face mandates due to high occupational disease exposure. They benefit from the mandate through reduced occupational disease risk and from system-wide vaccination increasing safety in patient populations. They bear compliance cost (vaccination requirement) but rationally accept it because occupational exposure is genuine and substantial. Their exit options are constrained (leaving healthcare work is costly; accepting vaccination is structurally cheaper).
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, healthcare_workers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__risk_stratification_reading, healthcare_workers, payer).

% Define the risk threshold, set mandate boundaries, enforce the proportionality requirement, and defend the reading against both bodily_autonomy_primacy and public_health_primacy challenges. They set the agenda by deciding which populations fall above the threshold and which do not. They benefit from the enhanced legitimacy risk-stratification provides (compared to blanket mandates) but must continuously defend threshold choices and resist drift toward blanket enforcement.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Face mandates despite low individual actuarial risk of severe disease (young, healthy, no comorbidities). They lose bodily autonomy to the state's vaccination requirement. They may lose employment, education access, or travel ability if they refuse. Their exit option is nominally to get vaccinated, but for vaccine-hesitant individuals with identity-based vaccination resistance, that is not a true exit — vaccination crosses an identity boundary. Structurally trapped: state enforcement mechanism closes alternatives.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_unvaccinated_individuals, payer,
    powerless, biographical, trapped, national).

% Moderate-risk populations (middle-aged, some comorbidities, but not high-risk by actuarial standard) who refuse vaccination based on identity, distrust, or principled autonomy concerns. They face mandates that their risk profile does not justify by the reading's own proportionality standard, yet the enforcement mechanism catches them equally with higher-risk groups. Their exit is identity_locked: vaccination would require crossing a deeply held belief boundary about bodily autonomy or trust in institutions, which feels impossible from within that identity frame.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_hesitant_moderate_risk_populations, payer,
    moderate, biographical, identity_locked, national).

% Benefit from the mandate by gaining employment, education, and travel access that requires vaccination proof. They voluntarily accepted vaccination (or faced no resistance to it), so the mandate validates their choice rather than constraining it. They benefit from a system where vaccination status carries institutional weight. Their exit is mobile: they can maintain their vaccinated status and participate, or choose to unvaccinate if they change their mind (though re-entry to mandated spaces becomes difficult).
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_vaccinated_individuals, beneficiary,
    organized, biographical, mobile, national).

% Medical autonomy, informed consent, and bodily self-determination organizations that argue mandate authority violates human rights regardless of outcome or risk profile. They are excluded from the conversation when this reading dominates policy (risk-stratification framing pre-empts their axioms). They would oppose mandates at any risk threshold, reading this constraint as insufficient protection of autonomy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, bodily_autonomy_advocates, excluded,
    moderate, biographical, analytical, national).

% Public health officials, epidemiologists, and collective health advocates who argue that mandate authority should extend blanket to all populations because any unvaccinated person is an externality and disease vector. They are excluded from effective agenda-setting when risk-stratification reading dominates. They see this constraint as insufficiently protective of collective health.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_maximization_advocates, excluded,
    moderate, biographical, analytical, national).

% Constitutional courts, appellate bodies, and human rights arbiters that adjudicate whether risk-stratification reading is coherent, whether authorities actually implement proportionately, and whether the framework respects both autonomy and public health within constitutional bounds. They are observers in the sense that they do not collect from the constraint; they arbitrate its legitimacy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, courts_and_constitutional_arbiters, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__risk_stratification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates disease prevention and healthcare system preservation by requiring vaccination in high-risk populations and occupational exposure contexts, reducing severe disease transmission and system overload. The genuine coordination problem is collective vulnerability to overwhelmed healthcare capacity during disease surges — solved by distributing vaccination burden with proportionality constraint rather than as universal conscription.
% TRANSFER_FUNCTION: Transfers bodily autonomy from unvaccinated lower-risk individuals and vaccine-hesitant moderate-risk populations to the state apparatus (in the form of vaccination requirement or access restriction). Transfers disease risk reduction benefit to high-risk populations and healthcare system. Transfers legitimacy benefit to public health authorities (proportionality framing enhances their constitutional standing compared to blanket mandates).
% ABSENT_VOICES: Bodily autonomy advocates and public health maximization advocates would object to this reading but are structurally excluded from agenda-setting when risk-stratification dominates. Vaccine-hesitant individuals have no seat at the threshold-definition table — the risk level is set by authorities, not by consent of those it governs. Unvaccinated low-risk individuals are not consulted about whether their risk justifies mandate scope.
% DISAPPEARANCE_RATIONALE: If this constraint and its enforcement vanished, high-risk populations would face reduced vaccination rates among surrounding groups, healthcare systems would see higher severe disease burden in surges, and public health authorities would lose a primary tool for coordinated disease prevention. The political and legal landscape would reorganize around either blanket mandates (public_health_primacy reading) or categorical autonomy protection (bodily_autonomy_primacy reading). The moderate position would collapse without institutional enforcement.
% FOUNDING_PROBLEM: Uncontrolled disease transmission in populations unable to vaccinate themselves (elderly, immunocompromised) due to high prevalence of vaccine-refusal among surrounding lower-risk populations, creating cascading healthcare system failure during surge periods and preventable severe disease and death in vulnerable groups.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities and epidemiologists attest the problem remains live (variant emergence, ongoing healthcare capacity stress in unvaccinated regions). High-risk population advocates attest to ongoing protection need. Bodily autonomy advocates argue the problem is artificially constructed to justify mandates (people have always faced disease risk; the reading frames normal risk as an intolerable externality). Post-pandemic jurisdictions show mixed evidence: some maintained high-vaccination rates without mandates; others saw rapid decline after mandate removal, suggesting mandate-dependent behavior change. The corroboration is contested between the parties; no authoritative independent voice settles whether the founding problem remains live or has transitioned to a post-pandemic political identity issue.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__risk_stratification_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__risk_stratification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__risk_stratification_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because the constraint does extract compliance from lower-risk groups not justified by external harm, yet the extraction is bounded by the reading's own proportionality requirement — unbounded extraction would violate the reading's core premise. Suppression is substantial (0.62) because the constraint operates through employment, travel, and access restrictions that leave little structural exit for those caught under the mandate; the mechanisms are legal, not merely coercive, which makes them harder to challenge than extra-legal suppression. Theater ratio rises significantly (0.22 → 0.48) in early intervals as authorities defend the proportionality framing against both extremes, then plateaus once the boundary is established — the performative component stabilizes once the reading settles into practice. Accessibility of alternatives remains low (0.71) because unvaccinated low-risk individuals face structural integration of the mandate into employment, healthcare access, and social participation — they cannot simply opt out without significant life disruption. Resistance is high (0.73) because this reading faces opposition from both extremes: bodily_autonomy advocates reject it as insufficient protection of autonomy; public_health advocates reject it as insufficiently protective of public health. The measurement series charts the constraint's lifecycle: initial negotiation phase (extractiveness and suppression rising as mandates are enacted and enforcement hardens), stabilization phase (plateau at 12–16 time points as the risk-stratified framework becomes the settled legal position), and sustained operation phase (values hold at 16–24 as the constraint becomes institutionalized).
 *
 * PERSPECTIVAL GAP:
 *   From the public_health_authority seat, this reading is a compromise that preserves mandate legitimacy while respecting proportionality; from the high_risk_population seat, the same constraint provides justifiable protection; but from the low_risk_unvaccinated seat, the constraint is extraction dressed in proportionality language — the threshold is opaque, the risk calculation is out of their hands, and the enforcement is real regardless of whether their risk justifies it. The bodily_autonomy_primacy observer sees false compromise: any mandate violates autonomy absolutely, proportionality language is rhetorical cover. The public_health_primacy observer sees insufficient protection: targeting mandates to risk-stratified populations leaves a dangerous pool of unvaccinated moderate-risk people. The engine computes these different type-classifications from the structural data — the authority seat computes rope-like (genuine coordination), the low-risk-unvaccinated seat computes snare-like (extraction with cover), the bodily_autonomy observer computes snare (pure violation), the public_health observer computes rope insufficiently deployed. The authored metrics reflect the structural situation (both coordination and extraction present, both real); divergence from the claim signals the reading's contested nature.
 *
 * DIRECTIONALITY LOGIC:
 *   High_risk_populations and healthcare_system_capacity beneficiary seats derive low directionality (d ≈ 0.15–0.25) because they benefit from the mandate without bearing extraction costs; they have high power (institutional/organized) and high exit (they can organize politically to support the mandate). Public_health_authorities have moderate directionality (d ≈ 0.4–0.5) as agenda-setters — they benefit from the legitimacy the reading provides but must defend threshold choices continuously against both extremes, which creates constraint on their power. Low_risk_unvaccinated individuals and vaccine_hesitant_moderate_risk populations have high directionality (d ≈ 0.7–0.85) as targets — they bear the extraction cost (forced vaccination or access restriction), have trapped or identity_locked exit (vaccination hesitancy is identity-constituted, and regulatory barriers block exit), and face suppression requirement holding at 0.62 regardless of their actual risk. The asymmetry drives the tangled_rope classification: genuine coordination function (risk-stratified disease prevention) rides on asymmetric extraction (lower-risk groups forced to comply despite proportionality doctrine).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy only if authorities actually enforce proportionality and risk-stratification. If threshold definition becomes opaque, threshold shifts post-hoc to include ever-wider populations, or enforcement drifts toward blanket mandates, the reading's core proposition (proportionality as legitimating principle) dies while the constraint persists. The theater ratio rising from 0.22 to 0.48 signals growing performative defense of proportionality rather than actual proportional operation — authorities must constantly describe the constraint as risk-stratified even as enforcement scope drifts. If theater ratio rises above 0.55 and plateau time extends, the constraint should be reclassified as piton (the reading is the performance, the actual constraint is blanket extraction). The founded-problem analysis below examines whether the problem (uncontrolled spread among high-risk groups) remains live or has transitioned to a post-pandemic political-identity problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_definition_indeterminacy,
    'What actuarial risk threshold legitimates mandate scope? Who defines the threshold, and is the definition contestable?',
    'Transparent actuarial methodology published and defended in peer review; threshold reviewed by independent epidemiologists and legal scholars outside the mandating authority.',
    'If threshold is vague, opaque, or shifted post-hoc, the constraint collapses into blanket extraction (victim set expands to all unvaccinated). If threshold is rigorously defined and externally audited, the constraint holds as proportionate coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_definition_indeterminacy, empirical, 'The operational definition of ''actuarial risk'' that separates permissible targeted mandates from impermissible blanket mandates.').

omega_variable(
    reading_kernel_bifurcation,
    'Does this reading occupy a genuinely intermediate position between bodily_autonomy_primacy and public_health_primacy, or does it collapse under pressure into one of the extremes?',
    'Observational study of mandate implementation: do authorities actually enforce proportionality and risk-stratification, or do they drift toward blanket enforcement? Do autonomy advocates accept this reading or repudiate it as cover for public_health_primacy?',
    'If the reading holds operationally (authorities enforce proportionality, both extremes recognize it as a distinct position), it is a genuine compromise framing. If implementation drifts toward blanket enforcement and autonomy advocates reject it as false equivalence, the reading forecloses into public_health_primacy or becomes theoretically orphaned.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_bifurcation, conceptual, 'Whether this reading is a sustainable third position or a rhetorical covering for one of the extreme readings.').

omega_variable(
    externality_boundary_contestation,
    'At what transmission risk and population-disease-burden level does unvaccinated status constitute a legitimately externalizeable harm, versus a risk each person owns individually?',
    'Epidemiological consensus on transmission thresholds and externality magnitude, cross-checked against historical precedent for what counts as a public harm justifying bodily intervention in democratic legal systems.',
    'A narrow externality boundary (only very high-transmission, severe-disease conditions trigger mandate authority) supports targeted mandates and restrains this reading toward bodily_autonomy_primacy. A broad boundary supports blanket mandates and shifts this reading toward public_health_primacy. The boundary is not empirically determined — it is a normative commitment about whose risk counts as ''external.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_boundary_contestation, preference, 'The implicit normative commitment about when individual unvaccinated status becomes a legitimate externality subject to state intervention.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the measured suppression (0.62) structural (legal barriers to employment/travel without vaccination) or internalized (vaccination-hesitant individuals have internalized state authority and fear, independent of external enforcement)?',
    'Post-mandate-removal trajectory: if suppression persists after structural barriers are lifted, it signals internalized mechanisms (ideological capture, identity fusion with vaccination resistance). If suppression rapidly decays, it was primarily structural.',
    'Structural suppression is a feature of the constraint''s design; internalized suppression suggests the constraint has colonized the target population''s self-understanding. The effective suppression burden is higher if internalized, because targets carry it regardless of enforcement infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether suppression operates through external legal sanctions or through internalized fear/identity fusion in the target population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__risk_stratification_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(vacc_tr_t0, observed).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement_basis(vacc_tr_t4, observed).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement_basis(vacc_tr_t8, observed).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement_basis(vacc_tr_t12, observed).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 16, 0.46).
narrative_ontology:measurement_basis(vacc_tr_t16, observed).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(vacc_tr_t20, observed).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement_basis(vacc_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(vacc_be_t0, observed).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement_basis(vacc_be_t4, observed).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(vacc_be_t8, observed).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement_basis(vacc_be_t12, observed).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement_basis(vacc_be_t16, observed).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(vacc_be_t20, observed).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement_basis(vacc_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(vacc_su_t0, observed).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 4, 0.51).
narrative_ontology:measurement_basis(vacc_su_t4, observed).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement_basis(vacc_su_t8, observed).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement_basis(vacc_su_t12, observed).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement_basis(vacc_su_t16, observed).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(vacc_su_t20, observed).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement_basis(vacc_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__risk_stratification_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_legitimacy__risk_stratification_reading, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel vaccine_mandate_legitimacy. The sibling readings bodily_autonomy_primacy_reading and public_health_primacy_reading instantiate the other interpretations. All three are linked via network.affects_constraints. The ε-invariance principle requires separate constraint files because the three readings have structurally distinct victim sets, beneficiary structures, and enforcement logics. Risk_stratification reading has a bounded victim set (low-risk unvaccinated); bodily_autonomy reading has a universal victim set (anyone facing mandate authority); public_health reading has a minimal victim set (only truly autonomous refusers, treating unvaccinated as externality generators). The family decomposition preserves the kernel contest while respecting ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
