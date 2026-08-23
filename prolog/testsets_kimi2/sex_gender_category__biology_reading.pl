% ============================================================================
% CONSTRAINT STORY: sex_gender_category__biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__biology_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: sex_gender_category__biology_reading
 *   human_readable: Immutable Biological Sex Category Determination
 *   domain: social/legal/identity
 *
 * SUMMARY:
 *   This constraint story instantiates the biology_reading of the contested
 *   kernel sex_gender_category. It models the standing arrangement under
 *   which legal and social sex category membership is determined by immutable
 *   reproductive biologyâchromosomes and anatomy at birth. This reading
 *   excludes trans women from the 'woman' category, renders intersex
 *   variations administratively invisible through forced binary assignment,
 *   and positions cis women as the sole legitimate victim class for sex-based
 *   harms. The constraint carries high boundary enforcement costs (sports
 *   testing, birth certificate policing, medical normalization protocols) and
 *   is actively contested by identity and hybrid readings that are
 *   structurally foreclosed by its core premise of biological immutability.
 *
 * KEY AGENTS:
 *   - trans_women: Primary target (moderate/identity_locked) â excluded from categorical recognition and sex-segregated protections.
 *   - intersex_individuals: Secondary target (powerless/trapped) â subjected to non-consensual normalization to fit binary schema.
 *   - cis_women: Nominal beneficiary (organized/constrained) â afforded categorical protection and recognition under the reading, though subject to boundary policing.
 *   - state_bureaucracy_sex_registry: Agenda setter (institutional/analytical) â administers legal sex classification and sets definitional standards.
 *   - gender_critical_advocacy: Beneficiary (organized/mobile) â gains institutional legitimacy from state adoption of biological immutability.
 *   - sports_governing_bodies: Beneficiary (institutional/constrained) â receives a bright-line segregation rule that reduces legal ambiguity.
 *   - trans_rights_advocacy: Excluded voice (organized/constrained) â advocates for identity-based classification but is kept off policy tables.
 *   - medical_sex_assignment_practitioners: Agenda setter (institutional/constrained) â enforces binary through surgical and hormonal protocols on intersex infants.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__biology_reading, 0.72).
domain_priors:suppression_score(sex_gender_category__biology_reading, 0.78).
domain_priors:theater_ratio(sex_gender_category__biology_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__biology_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__biology_reading, "Immutable Biological Sex Category Determination").
narrative_ontology:topic_domain(sex_gender_category__biology_reading, "social/legal/identity").

domain_priors:requires_active_enforcement(sex_gender_category__biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__biology_reading, '419ad2ef-ccdd-45ba-b8ec-19708d1281be').
narrative_ontology:cs_kernel_codification('419ad2ef-ccdd-45ba-b8ec-19708d1281be', formalized).
narrative_ontology:cs_authority_grounding('419ad2ef-ccdd-45ba-b8ec-19708d1281be', lineage).
narrative_ontology:cs_interpretation_layer_present('419ad2ef-ccdd-45ba-b8ec-19708d1281be').
narrative_ontology:cs_reading_relation('419ad2ef-ccdd-45ba-b8ec-19708d1281be', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_reading_relation('419ad2ef-ccdd-45ba-b8ec-19708d1281be', sex_gender_category__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('419ad2ef-ccdd-45ba-b8ec-19708d1281be', foundational, sex_category_immutable_biological).
narrative_ontology:cs_axiom_status(sex_category_immutable_biological, holdable).
narrative_ontology:cs_axiom_grounding('419ad2ef-ccdd-45ba-b8ec-19708d1281be', sex_category_immutable_biological, empirically_contingent).
narrative_ontology:cs_axiom('419ad2ef-ccdd-45ba-b8ec-19708d1281be', foundational, binary_sex_sufficient_for_all_persons).
narrative_ontology:cs_axiom_status(binary_sex_sufficient_for_all_persons, holdable).
narrative_ontology:cs_axiom_grounding('419ad2ef-ccdd-45ba-b8ec-19708d1281be', binary_sex_sufficient_for_all_persons, empirically_contingent).
narrative_ontology:cs_reference_frame('419ad2ef-ccdd-45ba-b8ec-19708d1281be', biological_binary_taxonomy).
narrative_ontology:cs_drift_state('419ad2ef-ccdd-45ba-b8ec-19708d1281be', contemporary_gender_politics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('419ad2ef-ccdd-45ba-b8ec-19708d1281be', '').
narrative_ontology:cs_kernel_id(sex_gender_category__biology_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, cis_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, sports_governing_bodies).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, gender_critical_advocacy).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, trans_women).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, intersex_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers legal sex classification on birth certificates and identity documents using immutable reproductive criteria; sets the definitional standard that schools, prisons, and healthcare systems adopt.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, state_bureaucracy_sex_registry, agenda_setter,
    institutional, generational, analytical, national).

% Perform surgeries and hormone protocols on intersex infants to render bodies conformant with binary male/female categories; their professional authority and billing practices are validated by the biological-classification mandate.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, medical_sex_assignment_practitioners, agenda_setter,
    institutional, generational, constrained, national).

% Recognized as the sole legitimate occupants of the 'woman' legal and social category under this reading; afforded access to sex-segregated spaces and sex-based protections, though also subject to documentation checks and boundary policing.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, cis_women, beneficiary,
    organized, biographical, constrained, national).

% Excluded from the 'woman' category regardless of social transition, medical intervention, or self-identification; denied access to sex-segregated protections and facing legal erasure. Exit from this classification requires changing the entire legal framework, not individual choice.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_women, payer,
    moderate, biographical, identity_locked, national).

% Forced into binary male/female categories through non-consensual medical normalization, hormone treatments, or surgical assignment; biological variation is rendered administratively invisible and medically pathologized.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, intersex_individuals, payer,
    powerless, biographical, trapped, national).

% Rely on biological sex categories to segregate athletic competition; benefit from a bright-line rule that reduces legal ambiguity but requires expensive and invasive testing to enforce at category boundaries.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, sports_governing_bodies, beneficiary,
    institutional, generational, constrained, global).

% Advances the position that biological sex is immutable and binary; gains institutional legitimacy, policy influence, and mobilization resources when the state adopts this reading.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, gender_critical_advocacy, beneficiary,
    organized, generational, mobile, national).

% Argues for gender identity-based classification; structurally excluded from policy-making tables where the biology reading dominates, though present in broader public discourse and litigation.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_rights_advocacy, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes unambiguous, administratively tractable sex categories for legal, medical, and protective segregation, ostensibly to protect cis women from male violence and maintain statistical or clinical clarity.
% TRANSFER_FUNCTION: Moves legal recognition, social legitimacy, and access to sex-segregated spaces away from trans women and intersex individuals toward cis women and sex-segregated institutions, while imposing medical and bureaucratic compliance costs on intersex and trans populations.
% ABSENT_VOICES: Trans rights advocates arguing for self-identification and intersex human rights advocates arguing against non-consensual normalization are systematically excluded from policy tables where this reading dominates; their objections are treated as outside the frame of biological reality.
% DISAPPEARANCE_RATIONALE: If biological sex classification vanished overnight, access protocols for shelters, sports, prisons, and medical statistics would lose their current organizing principle; trans women would gain categorical inclusion, intersex individuals would no longer be forcibly normalized, and the current sex-segregated institutional order would require renegotiation.
% FOUNDING_PROBLEM: The need to categorize human bodies for medical treatment, statistical tracking, and sex-based protection from male violence in a patriarchal context.
% FOUNDING_PROBLEM_CORROBORATION: Medical historians and feminist scholars outside the gender-critical movement attest that the binary medical model was constructed over the 20th century and is contested by intersex activism; trans health researchers attest that the protection rationale no longer requires immutable biological classification. The gender-critical movement and some second-wave feminist institutions assert the problem is still live. No neutral consensus exists.
narrative_ontology:disappearance_verdict(sex_gender_category__biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sex_gender_category__biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__biology_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint denies legal recognition and social category membership to trans women, forces intersex individuals into unwanted medical protocols, and imposes pervasive documentation and testing burdens. Suppression (0.78) is higher still because the constraint can only persist by actively suppressing self-identification alternatives and medically gatekept hybrid models; the foreclosure of sibling readings is enforced through legal and medical gatekeeping. Theater_ratio (0.55) reflects that a growing share of enforcement activity (sports sex testing, bathroom policing, birth certificate audit campaigns) performs categorical purity rather than delivering measurable protection to cis women. Accessibility_collapse (0.70) is high because once the biological criterion is institutionalized, non-biological alternatives become administratively invisible. Resistance (0.75) is high and rising because trans and intersex movements mount sustained legal and social challenge. The measurement grid is aligned: all three tracked metrics are authored at six shared time points to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (state bureaucracy, medical practitioners) experience the constraint as administrative necessity and medical standard-of-care; the beneficiary seats (cis women, gender critical advocacy, sports bodies) experience it as protection or legitimacy; the payer seats (trans women, intersex individuals) experience it as erasure and coerced normalization. The engine computes these divergent classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women and intersex individuals are structural payers: the constraint extracts recognition, bodily autonomy, and legal status from them (high d). Cis women, sports governing bodies, and gender critical advocacy are structural beneficiaries: the constraint subsidizes their access to segregated spaces, political mobilization, and administrative clarity (low d). The state bureaucracy sits near symmetric: it bears enforcement costs and political contention while gaining administrative legibility.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâprotecting cis women from male violence and enabling medical/statistical categorizationâis contested and arguably dead in its original binary form. The constraint persists despite high enforcement costs and accumulating empirical challenges (intersex prevalence, neurological sex diversity), suggesting mandatrophy risk. However, because concentrated beneficiaries (gender critical movements, sex-segregated institutions) continue to mobilize around the constraint, it does not yet read as pure piton inertia; the active beneficiary capture keeps it in tangled_rope territory rather than snare or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the biology reading''s foreclosure of identity and hybrid readings sustainable under accumulating empirical challenges to binary sex sufficiency?',
    'Track legal and medical precedent: if jurisdictions abandon immutable biological criteria in favor of identity or hybrid models, the foreclosure is structurally unsustainable.',
    'If the foreclosure collapses, the biology reading reclassifies from holdable to overridden within its own tradition, shifting the kernel''s dominant reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Sustainability of biology reading''s foreclosure of sibling readings').

omega_variable(
    boundary_enforcement_cost_sustainability,
    'Are the high boundary enforcement costs a transient political mobilization or a structural feature of immutable biological classification?',
    'Measure enforcement expenditure and error rates (false positives in sex testing, intersex normalization rates) across jurisdictions with varying political climates.',
    'If costs remain high regardless of political context, the constraint is structurally extractive; if they fluctuate with political cycles, the extraction is contingent on specific beneficiary mobilization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_enforcement_cost_sustainability, empirical, 'Whether enforcement costs are structural or politically contingent').

omega_variable(
    authority_framing_ambiguity,
    'Does the authority of this constraint rest on medical expertise (empirical claim) or on lineage/tradition (unchanging natural order)?',
    'Examine legal justifications: when courts defend sex-based classification, do they cite empirical medical necessity or traditional understanding of sex?',
    'If grounded in expertise, empirical refutation can override it; if grounded in lineage/tradition, it behaves like a commitment system with extraction-based authority maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_framing_ambiguity, conceptual, 'Whether the authority claim is expertise or lineage').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__biology_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__biology_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sex__tr_t10, sex_gender_category__biology_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__biology_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(sex__tr_t30, sex_gender_category__biology_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(sex__tr_t40, sex_gender_category__biology_reading, theater_ratio, 40, 0.53).
narrative_ontology:measurement(sex__tr_t50, sex_gender_category__biology_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__biology_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sex__be_t10, sex_gender_category__biology_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__biology_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(sex__be_t30, sex_gender_category__biology_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(sex__be_t40, sex_gender_category__biology_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(sex__be_t50, sex_gender_category__biology_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__biology_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(sex__su_t10, sex_gender_category__biology_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__biology_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(sex__su_t30, sex_gender_category__biology_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(sex__su_t40, sex_gender_category__biology_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement(sex__su_t50, sex_gender_category__biology_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__biology_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__identity_reading).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% The sex_gender_category kernel decomposes into three structurally distinct constraints: biology_reading (immutable biological criteria), hybrid_reading (medical gatekeeping model), and identity_reading (subjective self-identification). Each reading has a different epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
