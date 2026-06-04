% ============================================================================
% CONSTRAINT STORY: intermediate_scrutiny_tier__tier_drift_question
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intermediate_scrutiny_tier__tier_drift_question, []).

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
 *   constraint_id: intermediate_scrutiny_tier__tier_drift_question
 *   human_readable: Intermediate Scrutiny as Doctrinal Tier Drift (Sex, Illegitimacy, Variable Rigor)
 *   domain: constitutional_law/equal_protection_doctrine
 *
 * SUMMARY:
 *   The intermediate scrutiny tier in equal protection doctrine emerged as a
 *   formal doctrinal innovation in Craig v. Boren (1976). It promised a
 *   middle ground between strict scrutiny (reserved for suspect
 *   classifications and fundamental rights) and rational basis review
 *   (minimum scrutiny requiring only minimal fit). The doctrine stated that
 *   intermediate scrutiny applied to quasi-suspect classifications like sex
 *   and illegitimacy: governments must demonstrate a substantial, not merely
 *   legitimate, interest and a means that is substantially related to that
 *   interest — not just rational or minimally rational. Over five decades,
 *   the doctrine has been applied with variable rigor. Some decisions apply
 *   intermediate scrutiny with genuine teeth: requiring detailed
 *   justification, skepticism of sex-based generalizations, and close-fit
 *   analysis. Others apply it with deference barely distinguishable from
 *   rational basis. The variation does not follow formal doctrinal boundaries
 *   — courts do not declare that they are relaxing intermediate scrutiny for
 *   illegitimacy cases or tightening it for sex cases in some principled way.
 *   Instead, the rigor appears to drift contextually: Justice Ginsburg's
 *   application in VMI was stringent; many subsequent sex-classification
 *   cases have been more deferential. Illegitimacy cases have received
 *   notoriously inconsistent treatment. The constraint captures this
 *   structure: intermediate scrutiny as a formal tier has generated an
 *   extraction mechanism where its formal promise (predictable standard)
 *   conflicts with its actual practice (calibrated case-by-case).
 *   Beneficiaries are judges who can invoke the tier vocabulary while
 *   adjusting rigor to reach context-specific outcomes. Victims are litigants
 *   and the tier-formalism itself, whose predictive value degrades. The
 *   doctrine proves that the tiers are a dial.
 *
 * KEY AGENTS:
 *   - Judiciary (courts applying intermediate scrutiny): Institutional/arbitrage beneficiary — preserves doctrinal flexibility and legitimacy vocabulary while avoiding mechanical rules; experiences constraint as enabling, not extractive
 *   - Litigant in sex or illegitimacy classification case: Powerless/trapped victim — faces unpredictable doctrinal bar; cannot predict whether court will apply strict or permissive intermediate scrutiny; suppressed from appealing to stable standard
 *   - Appellate practitioner specializing in equal protection: Moderate/constrained actor — benefits from tier-based coordination (the framework itself) but bears extraction from rigor variation (local expertise required, precedent less fungible across circuits)
 *   - Legal formalist / tier-precision advocate: Organized/constrained victim of tier-drift — perceives the drift as doctrinal failure; advocates for either hardening (VMI-style specificity) or collapse (unified test); constrained by judicial institutional power
 *   - Government litigant defending classification: Powerful/constrained actor — benefits from judicial flexibility (rigor varies; some courts apply light review) but constrained by need to articulate legitimate interest
 *   - Analytical observer (civilizational view): Sees tier-drift as immutable feature of judicial reasoning; naturalizes drift as inherent to human judgment; risks false-summit fallacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intermediate_scrutiny_tier__tier_drift_question, 0.54).
domain_priors:suppression_score(intermediate_scrutiny_tier__tier_drift_question, 0.48).
domain_priors:theater_ratio(intermediate_scrutiny_tier__tier_drift_question, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intermediate_scrutiny_tier__tier_drift_question, extractiveness, 0.54).
narrative_ontology:constraint_metric(intermediate_scrutiny_tier__tier_drift_question, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(intermediate_scrutiny_tier__tier_drift_question, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intermediate_scrutiny_tier__tier_drift_question, tangled_rope).
narrative_ontology:human_readable(intermediate_scrutiny_tier__tier_drift_question, "Intermediate Scrutiny as Doctrinal Tier Drift (Sex, Illegitimacy, Variable Rigor)").
narrative_ontology:topic_domain(intermediate_scrutiny_tier__tier_drift_question, "constitutional_law/equal_protection_doctrine").

domain_priors:requires_active_enforcement(intermediate_scrutiny_tier__tier_drift_question).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(intermediate_scrutiny_tier__tier_drift_question, 'e25b902d-f0e1-4dfd-9f3f-b5c544a74615').
narrative_ontology:cs_kernel_codification('e25b902d-f0e1-4dfd-9f3f-b5c544a74615', formalized).
narrative_ontology:cs_authority_grounding('e25b902d-f0e1-4dfd-9f3f-b5c544a74615', lineage).
narrative_ontology:cs_interpretation_layer_present('e25b902d-f0e1-4dfd-9f3f-b5c544a74615').
narrative_ontology:cs_reading_relation('e25b902d-f0e1-4dfd-9f3f-b5c544a74615', intermediate_scrutiny_tier__real_differences_doctrine, influences).
narrative_ontology:cs_reading_relation('e25b902d-f0e1-4dfd-9f3f-b5c544a74615', intermediate_scrutiny_tier__vmi_exceedingly_persuasive, influences).
narrative_ontology:cs_axiom('e25b902d-f0e1-4dfd-9f3f-b5c544a74615', foundational, formal_tiers_operationalize_as_continuous_dial).
narrative_ontology:cs_axiom_status(formal_tiers_operationalize_as_continuous_dial, holdable).
narrative_ontology:cs_axiom_grounding('e25b902d-f0e1-4dfd-9f3f-b5c544a74615', formal_tiers_operationalize_as_continuous_dial, empirically_contingent).
narrative_ontology:cs_axiom('e25b902d-f0e1-4dfd-9f3f-b5c544a74615', secondary, doctrinal_flexibility_requires_unpredictability_cost).
narrative_ontology:cs_axiom_status(doctrinal_flexibility_requires_unpredictability_cost, holdable).
narrative_ontology:cs_axiom_grounding('e25b902d-f0e1-4dfd-9f3f-b5c544a74615', doctrinal_flexibility_requires_unpredictability_cost, instrumental).
narrative_ontology:cs_reference_frame('e25b902d-f0e1-4dfd-9f3f-b5c544a74615', craig_v_boren_three_tier_structure).
narrative_ontology:cs_drift_state('e25b902d-f0e1-4dfd-9f3f-b5c544a74615', contemporary_circuit_fragmentation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e25b902d-f0e1-4dfd-9f3f-b5c544a74615', '').
narrative_ontology:cs_kernel_id(intermediate_scrutiny_tier__tier_drift_question, intermediate_scrutiny_tier).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(intermediate_scrutiny_tier__tier_drift_question, judiciary_doctrinal_flexibility).
narrative_ontology:constraint_victim(intermediate_scrutiny_tier__tier_drift_question, litigant_predictability).
narrative_ontology:constraint_victim(intermediate_scrutiny_tier__tier_drift_question, tier_formalism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LITIGANT UNDER VARIABLE RIGOR (SNARE) — A party seeking equal protection vindication cannot predict the doctrinal bar. Intermediate scrutiny promised consistency across sex and illegitimacy classifications, but the actual rigor drifts by bench, era, and fact pattern. The litigant is trapped: no exit from the classification system, no predictability of how rigor will be applied, suppressed from appealing to a stable doctrinal standard. Maximum experienced extraction.
constraint_indexing:constraint_classification(intermediate_scrutiny_tier__tier_drift_question, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: APPELLATE PRACTITIONER (TANGLED ROPE) — Practitioners benefit from tier-based coordination: the three-tier structure provides a coordinating framework and signals about what justifications courts will accept. But they also bear extraction: rigor varies unpredictably across circuits and judges, requiring extensive local factual development and limiting ability to rely on precedent across jurisdictions. Suppression is moderate: the practitioner can sometimes shop forums or develop specialized expertise, but exit from the tier system itself is not available.
constraint_indexing:constraint_classification(intermediate_scrutiny_tier__tier_drift_question, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JUDICIARY (INSTITUTIONAL FLEXIBILITY) (ROPE) — Courts benefit from intermediate scrutiny as a coordination mechanism: it provides a vocabulary for justification and signals legitimacy while preserving operational flexibility. The tier system enables judges to reach results calibrated to factual context without explicitly announcing result-oriented reasoning. Suppression is low: courts can adjust rigor, cite precedent selectively, and distinguish competing doctrinal lines. Beneficiary status derives from preserved doctrinal flexibility and reduced pressure to lock into rigid mechanical rules.
constraint_indexing:constraint_classification(intermediate_scrutiny_tier__tier_drift_question, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TIER FORMALISM IDEAL (PITON) — From a generational view, the formal tier structure (strict scrutiny, intermediate, rational basis) is substantially theater. The doctrine promises that each tier has stable meaning and predictable application. In practice, the actual behavior is calibrated case-by-case with rigor drifting across contexts, judges, and eras. The tier vocabulary persists through institutional inertia — it provides legitimacy and coordination value — but its formalism has degraded. Theater ratio is high because courts invoke tiers as anchors while their actual deployment varies continuously.
constraint_indexing:constraint_classification(intermediate_scrutiny_tier__tier_drift_question, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: GOVERNMENT LITIGANT (TANGLED_ROPE) — Government actors benefit from intermediate scrutiny's flexibility: the doctrine permits context-specific justifications (real differences, administrative efficiency) that rigid formal tiers would disallow. But government is constrained by the need to articulate some legitimate governmental interest and demonstrate fit. Suppression is moderate: the tier system both enables (permits flexibility) and constrains (requires justification). The government experiences mixed coordination and extraction depending on whether the particular court is applying rigor or deference.
constraint_indexing:constraint_classification(intermediate_scrutiny_tier__tier_drift_question, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGAL REFORM MOVEMENT (SCAFFOLD) — Organized advocates for doctrinal clarity see intermediate scrutiny's drift as a structural problem with a sunset: as consciousness of tier-drift spreads, pressure for either formalization (locking rigor) or collapse (unified test) increases. This perspective expects the drift problem to resolve over time as courts either clarify the tier or abandon it for a unitary test. Suppression is low: advocates can voice the formalism critique and influence doctrine through strategic litigation. The scaffold perspective treats the drift as temporary institutional incoherence with a correction mechanism.
constraint_indexing:constraint_classification(intermediate_scrutiny_tier__tier_drift_question, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — From a civilizational/universal analytical view, tier drift is structurally inherent to judicial review of classification: any bright-line test will be calibrated by judges to factual context, producing an effective sliding scale regardless of formal doctrine. The tiers are necessarily a dial because human judgment cannot be fully mechanized. This perspective naturalizes the drift as an immutable feature of how law works. Engine classification: FALSE SUMMIT. The structural data reveals this as naturalization of a contingent institutional choice (three-tier formalism) rather than a law of judicial reasoning.
constraint_indexing:constraint_classification(intermediate_scrutiny_tier__tier_drift_question, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intermediate_scrutiny_tier__tier_drift_question_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(intermediate_scrutiny_tier__tier_drift_question, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intermediate_scrutiny_tier__tier_drift_question, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(intermediate_scrutiny_tier__tier_drift_question, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(intermediate_scrutiny_tier__tier_drift_question, TR),
    TR >= 0.70.

:- end_tests(intermediate_scrutiny_tier__tier_drift_question_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.54): Moderate-high. The constraint exhibits genuine extraction because the formal promise (predictable tier with stable meaning) differs systematically from actual practice (rigor calibrated contextually, varying across judges and eras). Litigants cannot predict the doctrinal bar. Courts extract value through doctrinal flexibility without explicit acknowledgment. However, extractiveness is not maximal because intermediate scrutiny does provide some coordination value — it gives courts a vocabulary and signals legitimacy, and it does impose some constraint on deference (governments cannot simply assert any interest). The extraction is embedded within a coordination mechanism. Suppression (0.48): Moderate. Litigants are suppressed from appealing to a stable doctrinal standard — rigor variation is not acknowledged as doctrinal variation but presented as application of the same tier. Alternative dispute resolution is unavailable (litigants cannot opt out of equal protection doctrine). But suppression is not total: litigants can pursue forum shopping, can invest in detailed factual records, can appeal to different circuits. Theater ratio (0.62): Moderate-high. The tier structure invokes formal vocabulary (intermediate scrutiny, exceedingly persuasive justification) that suggests mechanical precision. Actual judicial practice involves context-sensitive calibration that does not follow the formal box boundaries. The theater has increased over time: as the actual variation became more visible, courts doubled down on tier vocabulary (VMI's attempted hardening) rather than acknowledging drift. Contemporary doctrine wears the formal costume of tiers while operating as a dial.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a three-way perspectival gap. Judges see intermediate scrutiny as a coordination tool that permits context-sensitive review (Rope from institutional perspective with arbitrage exit). Litigants see it as an extractive unpredictability machine (Snare from powerless/trapped perspective). Legal formalists see it as a failed tier-system that degrades into theater (Piton from organized advocates for clarity). The analytical observer risks naturalizing the drift as immutable judicial behavior (false-summit Mountain). The government litigant experiences mixed coordination and extraction depending on which bench applies the doctrine (Tangled Rope from powerful but constrained perspective). Appellate practitioners benefit from tier-based coordination while bearing variable rigor costs (Tangled Rope from moderate/constrained perspective). The reform movement sees the drift problem as temporary, expecting eventual doctrinal clarification or collapse (Scaffold from organized/constrained agents with a sunset mechanism). Each perspective is generated by a distinct structural position relative to the tier system.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit options. Judiciary as institutional actor with arbitrage exit (can select cases, calibrate opinions, work within broad doctrinal discretion) occupies a low-d position: beneficiary status, arbitrage options → d ≈ 0.15 → f(d) ≈ 0.02 → minimal experienced extraction, constraint experienced as enabling. Litigant as powerless actor with trapped exit (must proceed through the courts, cannot opt out of equal protection doctrine, cannot predict doctrinal bar) occupies a high-d position: victim status, trapped options → d ≈ 0.95 → f(d) ≈ 1.42 → maximum experienced extraction, constraint experienced as coercive. Appellate practitioner as moderate actor with constrained exit (can specialize, research, and develop expertise, but faces resource constraints and limited forum choice) occupies a mid-d position: mixed beneficiary/victim status (benefits from tier coordination, bears rigor variation costs), constrained options → d ≈ 0.55 → f(d) ≈ 0.75 → moderate experienced extraction. The d values are derived from these structural features, not stipulated independently.
 *
 * MANDATROPHY ANALYSIS:
 *   The tier-drift constraint exhibits mandatrophy (coordination-extraction confusion) at the structural level. The three-tier formalism appears to be a coordination mechanism: it provides a shared vocabulary for justification, signals to parties and courts what the standards are, and reduces reasoning burden through categorization. But it simultaneously functions as an extraction mechanism: the formal promise of precision enables judges to invoke the tier while calibrating rigor contextually, producing unpredictability that extracts legitimacy value and flexibility for the judiciary while imposing costly uncertainty on litigants. The mandatrophy is resolved (not dissolved) by the tier_drift reading: it acknowledges that the coordinate and extractive functions coexist. Intermediate scrutiny genuinely provides coordination (the tier vocabulary, the framework for justification); it simultaneously enables extraction (doctrinal flexibility, unpredictable rigor). The constraint is tangled_rope precisely because both functions are present. The sibling readings attempt different resolutions: real_differences_doctrine tries to reduce extraction by specifying the boundary conditions (classification category determines rigor); vmi_exceedingly_persuasive tries to reduce extraction by hardening rigor; tier_drift_question acknowledges that extraction persists despite these attempts because the extraction mechanism is not an error but inherent to how formalism and human judgment interact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    real_differences_boundary,
    'Where is the line between legitimate real-differences doctrine (differential treatment justified by biological or social fact) and pretextual generalization that tier-drift obscures?',
    'Cross-temporal analysis of real-differences holdings in sex and illegitimacy cases; comparison of stated justifications with outcomes; investigation of whether courts apply real-differences equally or use it selectively to reach desired results',
    'If real-differences doctrine is consistently applied: tier structure gains coherence and is less extractive (classification predictable from stated difference category). If applied selectively: the doctrine becomes a cover for result-orientation, and tier extraction increases (classification unpredictable from doctrine).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(real_differences_boundary, empirical, 'Boundary between legitimate real-differences and pretextual generalization').

omega_variable(
    circuit_variation_sources,
    'Is intermediate scrutiny''s variable rigor across circuits and eras attributable to genuine doctrinal disagreement, differential caseloads, or institutional drift without conscious doctrinal choice?',
    'Citation pattern analysis (do circuits cite each other and distinguish or ignore precedent?); judicial opinion analysis of justification quality and articulation depth; temporal correlation between appointments/turnover and rigor shifts',
    'If genuine disagreement: circuits have different legitimate readings of the intermediate standard (coexists_with dynamics between sibling readings). If institutional drift: the variation is arbitrary and extractive (tier-formalism fails). If appointment-driven: the variation is political (suppression mechanism becomes visible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(circuit_variation_sources, empirical, 'Sources of intermediate scrutiny''s variable rigor across circuits and eras').

omega_variable(
    dial_versus_tiers_false_binary,
    'Is the choice between discrete tiers and a continuous dial the true dichotomy, or does the actual doctrinal practice involve multiple overlapping scales (rigor, deference, legitimate interest baseline) that tiers fail to capture?',
    'Structural analysis of what dimensions courts actually use in equal protection review; mapping of decision patterns to candidate doctrinal models (three tiers, continuous scale, multi-dimensional calibration); comparison of explanatory power',
    'If true dichotomy: tier-drift is evidence of dial structure underlying formal tiers. If false binary: the real doctrinal structure is more complex (multiple scales), and tier reform must address multidimensionality, not just rigor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dial_versus_tiers_false_binary, conceptual, 'Whether tiers-versus-dial is the true dimensionality or obscures multidimensional calibration').

omega_variable(
    vmi_hardening_sustainability,
    'Can VMI-style heightened rigor (exceedingly persuasive justification standard) be sustained across all intermediate scrutiny domains, or does case-by-case pressure force judges to calibrate downward in some contexts?',
    'Post-VMI citation analysis: tracking whether courts apply exceedingly persuasive standard consistently to sex, illegitimacy, and other classifications; measuring frequency of successful government defenses pre- and post-VMI; investigating whether courts distinguish VMI sex cases from non-sex intermediate scrutiny',
    'If sustainable: VMI reading represents genuine tier specification and reduces drift. If degraded: VMI''s attempted hardening fails in practice, and drift continues under a new label (exceedingly persuasive becomes another dial setting).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vmi_hardening_sustainability, empirical, 'Sustainability of VMI-style heightened rigor across intermediate scrutiny domains').

omega_variable(
    kernel_reading_committer_ambiguity,
    'This constraint is the tier_drift_question reading of the intermediate_scrutiny_tier kernel. Sibling readings (real_differences_doctrine, vmi_exceedingly_persuasive) offer competing accounts of how the tier should be interpreted. Which reading''s core premise best describes the actual judicial behavior?',
    'Structural drift analysis across all three readings'' base_properties metrics; citation and outcome patterns showing which reading''s framework courts actually use; observation of whether courts invoke real-differences escape hatch, apply VMI rigor uniformly, or allow rigor to drift unpredictably',
    'If real_differences reading dominates: the tier''s variation is explained by legitimate classification boundaries (sibling forecloses tier_drift). If vmi reading dominates: courts are attempting doctrinal hardening (tier_drift coexists as temporary regime). If tier_drift reading dominates: rigor varies without principled anchor (tier_drift forecloses formal precision claims).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, empirical, 'Committer-frame ambiguity: which sibling reading''s premise best describes actual judicial behavior').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intermediate_scrutiny_tier__tier_drift_question, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_t0_explicit_three_tier_era, intermediate_scrutiny_tier__tier_drift_question, theater_ratio, 0, 0.48).
narrative_ontology:measurement(theater_t10_dial_becoming_visible, intermediate_scrutiny_tier__tier_drift_question, theater_ratio, 10, 0.58).
narrative_ontology:measurement(theater_t20_formalism_worn_thin, intermediate_scrutiny_tier__tier_drift_question, theater_ratio, 20, 0.62).

% Extraction over time
narrative_ontology:measurement(extract_t0_equal_protection_baseline, intermediate_scrutiny_tier__tier_drift_question, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(extract_t10_post_craig_intensification, intermediate_scrutiny_tier__tier_drift_question, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(extract_t20_contemporary_circuit_variation, intermediate_scrutiny_tier__tier_drift_question, base_extractiveness, 20, 0.54).

% Suppression requirement over time
narrative_ontology:measurement(supp_t0_craig_clarity, intermediate_scrutiny_tier__tier_drift_question, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(supp_t10_circuit_fragmentation, intermediate_scrutiny_tier__tier_drift_question, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(supp_t20_unpredictability_entrenched, intermediate_scrutiny_tier__tier_drift_question, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(intermediate_scrutiny_tier__tier_drift_question, enforcement_mechanism).
narrative_ontology:affects_constraint(intermediate_scrutiny_tier__tier_drift_question, equal_protection_strict_scrutiny__tier_specification).
narrative_ontology:affects_constraint(intermediate_scrutiny_tier__tier_drift_question, rational_basis_review__doctrinal_floor).
narrative_ontology:affects_constraint(intermediate_scrutiny_tier__tier_drift_question, sex_classification_justification__real_differences).

% DUAL FORMULATION NOTE:
% Intermediate scrutiny is part of the three-tier equal protection system. This story documents the tier-drift reading specifically: rigor variability as a structural feature. Sibling constraint stories (real_differences_doctrine, vmi_exceedingly_persuasive) offer competing accounts of how this variability should be understood. All three are readings of the same kernel (the three-tier doctrine). This story's network links show how intermediate scrutiny affects strict scrutiny (if intermediate drifts, the boundary between strict and intermediate becomes fuzzy) and rational basis (if intermediate drifts toward deference, it converges on rational basis in practice).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(intermediate_scrutiny_tier__tier_drift_question, powerful, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
