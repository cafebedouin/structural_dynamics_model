% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__colorblind_reading, []).

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
 *   constraint_id: equal_protection_kernel__colorblind_reading
 *   human_readable: Colorblind Equal Protection Reading: Categorical Prohibition on State Racial Classifications
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   The colorblind reading of the Equal Protection Clause is one of three
 *   structurally distinct constraints instantiated by the same foundational
 *   constitutional text. This reading interprets the Fourteenth Amendment as
 *   categorically forbidding all state use of racial classifications,
 *   regardless of their remedial or diversity purpose. Any affirmative action
 *   program, whether designed to remedy past exclusion or achieve
 *   institutional diversity, is per se unconstitutional under this reading.
 *   The constraint operates at the interface between constitutional doctrine
 *   (what the law says) and institutional practice (what universities,
 *   employers, and public agencies are permitted to do). The reading's
 *   beneficiaries are white applicants to competitive selective institutions
 *   and the intellectual tradition of formal-equality constitutionalism; its
 *   primary victims are historically excluded racial groups who lose access
 *   to remedial pathways, and admissions officials constrained to implement
 *   facially neutral policies. The constraint is actively enforced through
 *   judicial review, with rising theater ratios reflecting increasing
 *   rhetorical work (opinion-writing, doctrine-clarification) required to
 *   maintain the rule as empirical evidence mounts that colorblind policies
 *   correlate with persistent access disparities.
 *
 * KEY AGENTS:
 *   - white_applicants_to_competitive_selective_institutions — beneficiary, institutional/powerful seat, geographical scope global (US higher education dominates), low directionality toward costs (constraint subsidizes their application profiles)
 *   - historically_excluded_racial_groups — victim, powerless/organized seat (politically organized but institutionally excluded), biographical time horizon, trapped exit (cannot access the institutions the constraint governs without consent), national scope. High directionality toward costs (constraint extracts their remedial pathway access).
 *   - selective_institution_admissions_officers — payer/secondary_agenda_setter, institutional seat, biographical horizon, constrained exit (courts enforce the colorblind rule; deviation invites litigation), national scope. Medium-high directionality: they pay the enforcement and reputational cost of managing color-blind admissions against diversity pressure.
 *   - constitutional_doctrine_adherents — beneficiary, analytical/professional seat, generational time horizon, arbitrage exit (can shift to alternative doctrinal frameworks). Zero or negative directionality (the constraint subsidizes their interpretive tradition).
 *   - civil_rights_enforcement_agencies — excluded, institutional seat, generational horizon, constrained exit (federal agencies are bound by Supreme Court doctrine), national scope. Would object to the categorical prohibition but lack standing to overturn it.
 *   - Supreme_Court_majority_adopting_colorblind_doctrine — agenda_setter, institutional/powerful seat, generational horizon, arbitrage exit (can revise doctrine, though at reputational cost), national scope. Near-zero directionality (the constraint is the doctrine they set).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, 0.68).
domain_priors:suppression_score(equal_protection_kernel__colorblind_reading, 0.55).
domain_priors:theater_ratio(equal_protection_kernel__colorblind_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__colorblind_reading, "Colorblind Equal Protection Reading: Categorical Prohibition on State Racial Classifications").
narrative_ontology:topic_domain(equal_protection_kernel__colorblind_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__colorblind_reading, 'e0fb873f-3ba6-4786-86e3-6756def171ff').
narrative_ontology:cs_kernel_codification('e0fb873f-3ba6-4786-86e3-6756def171ff', fixed_text).
narrative_ontology:cs_authority_grounding('e0fb873f-3ba6-4786-86e3-6756def171ff', lineage).
narrative_ontology:cs_interpretation_layer_present('e0fb873f-3ba6-4786-86e3-6756def171ff').
narrative_ontology:cs_reading_relation('e0fb873f-3ba6-4786-86e3-6756def171ff', equal_protection_kernel__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('e0fb873f-3ba6-4786-86e3-6756def171ff', equal_protection_kernel__antisubordination_reading, coexists_with).
narrative_ontology:cs_axiom('e0fb873f-3ba6-4786-86e3-6756def171ff', foundational, state_racial_classification_categorically_prohibited).
narrative_ontology:cs_axiom_status(state_racial_classification_categorically_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('e0fb873f-3ba6-4786-86e3-6756def171ff', state_racial_classification_categorically_prohibited, deontological).
narrative_ontology:cs_axiom('e0fb873f-3ba6-4786-86e3-6756def171ff', foundational, formal_equality_sufficient_for_equal_protection).
narrative_ontology:cs_axiom_status(formal_equality_sufficient_for_equal_protection, holdable).
narrative_ontology:cs_axiom_grounding('e0fb873f-3ba6-4786-86e3-6756def171ff', formal_equality_sufficient_for_equal_protection, deontological).
narrative_ontology:cs_reference_frame('e0fb873f-3ba6-4786-86e3-6756def171ff', colorblind_formal_equality_standard).
narrative_ontology:cs_drift_state('e0fb873f-3ba6-4786-86e3-6756def171ff', contemporary_empirical_disparity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e0fb873f-3ba6-4786-86e3-6756def171ff', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__colorblind_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, white_applicants_to_competitive_selective_institutions).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, colorblind_constitutional_doctrine_adherents).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, historically_excluded_racial_groups).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, admissions_officers_implementing_diversity_programs).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__colorblind_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(equal_protection_kernel__colorblind_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__colorblind_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_kernel__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 at interval end, reflecting the asymmetric distribution of burden: the constraint's operation denies remedial access to historically excluded groups (high cost to them) while maintaining preferential access regimes for privileged groups (de facto colorblind doctrine permits facially neutral legacy preferences, geographic diversity emphasis that correlates with privilege, etc.). Suppression is moderate (0.55) because the constraint operates through doctrine and legal rule rather than direct coercion, but it is sustained by the threat of judicial remedies against non-compliance; alternatives (remedial race-consciousness, explicit caste-remediation programs) are foreclosed by the rule itself. Theater ratio rises over the interval (0.22 to 0.38) as the colorblind reading faces growing empirical pressure: demographic data showing persistent racial disparities, institutional statements about diversity that conflict with the colorblind rule, and increasingly elaborate judicial opinions defending colorblindness despite this evidence. The rising theater suggests the constraint's functional purpose (remedial action) has atrophied while its structural maintenance (the rule-as-itself) becomes the justificatory focus. Accessibility collapse is high (0.72) because once the colorblind reading is institutionalized in constitutional law, alternatives are rendered legally unavailable — admissions officials cannot use race-conscious frameworks without violating binding doctrine. Resistance is also elevated (0.71) because historically excluded groups, civil rights organizations, and institutional administrators implement workarounds and challenge the doctrine, generating steady pressure against the constraint. The measurement series documents extraction accumulation over the 30-year interval: as demographic divergence between colorblind law and racial reality widens, the measured extractiveness rises, suggesting the constraint is less about coordination (a foundational principle) and increasingly about the systematic advantage of one group relative to another.
 *
 * PERSPECTIVAL GAP:
 *   The Supreme Court majority (agenda_setter seat) perceives this constraint as a neutral, formally-equal application of constitutional principle — colorblindness as an imperative to treat all applicants identically regardless of race. From this seat, the constraint is rope (solving a coordination problem: how to avoid racial discrimination in admissions) with no extraction. The victim seat (historically excluded groups) perceives the same constraint as actively preventing remedial action and entrenching historical subordination under the guise of formal equality — the constraint is snare, extractive, suppressive. Admissions officers (payer seat) experience the constraint as a binding legal rule they must implement regardless of institutional values, creating cognitive and bureaucratic costs. The engine computes these divergences from the structural data: beneficiary/victim declarations, power differentials (Supreme Court institutional power vs. powerless groups), exit options (institutional actors bound by doctrine vs. excluded groups with no access), and directionality overrides where necessary. The claimed type is tangled_rope (coordination + extraction), reflecting the reading's simultaneous claim to solve the formal-equality coordination problem while imposing asymmetric costs on those it governs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the declared beneficiaries and victims and the exit options of each agent. White applicants to competitive institutions are beneficiaries with moderate-to-powerful power, mobile exit (they can attend non-selective institutions or apply elsewhere), and no structural dependence on remedial pathways — their directionality is near 0.0 (full beneficiary). Historically excluded racial groups are declared victims with powerless-to-organized power, trapped exit (the institutions governed by this constraint are the primary path to social mobility and professional credentialing in the US system; alternative institutions are lower-status; exit is not meaningful), and direct structural harm from the constraint's operation — their directionality is near 1.0 (full target). Admissions officers experience the constraint as a mandatory rule, but they are not the constraint's primary beneficiaries or victims; they are payers of compliance cost. Their directionality sits around 0.6-0.7 (constrained operators bearing enforcement burden). Constitutional doctrine adherents (the intellectual tradition) benefit from the constraint subsidizing their interpretive framework, but they bear no direct institutional cost — directionality near 0.1.
 *
 * MANDATROPHY ANALYSIS:
 *   The colorblind reading claims to solve a coordination problem: how to prevent racial discrimination in admissions and ensure equal consideration. The founding problem is real (documented historical racial discrimination in higher education) and was live when the colorblind reading gained doctrinal prominence (1990s-2000s). However, the constraint's operation has diverged from the founding problem it was built to address. The founding problem was discrimination — intentional or structural barriers that barred qualified applicants based on race. The colorblind solution was formal equality: treat all applicants by the same criteria regardless of race. This would solve discrimination if the antecedent conditions were equal (all applicants had equal access to resources, preparation, and institutional knowledge). They were not and do not. The consequence is that formal equality of process produces substantive inequality of outcome, and the constraint persists despite the founding problem shifting from 'how do we prevent discrimination?' to 'how do we address the effects of past discrimination and ongoing subordination?' The colorblind reading forecloses the second question by doctrine, but the first question is no longer the operative constraint — the operative constraint is 'who gets access to selective institutions?' and the colorblind rule produces a stable answer: primarily white and Asian applicants, with historically excluded groups underrepresented. The constraint's theater ratio rises because an increasing share of judicial and doctrinal labor is devoted to defending the rule against the empirical evidence that it does not solve the problem it claims to address. This is mandatrophy: the founding problem is dead (or contested), the founding constraint persists, and the gap between original function and current operation widened into observable divergence. The commentary on whether the founding problem is 'live,' 'dead,' or 'contested' is complicated: the colorblind reading treats discrimination itself as dead (law now forbids it) and therefore the colorblind remedy as sufficient; civil rights constituencies and institutional actors increasingly treat subordination effects as live and the colorblind remedy as insufficient. This divergence is exactly what the mandatrophy detection system flags.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colorblind_vs_antisubordination_kernel_contest,
    'Is the Equal Protection Clause fundamentally about categorical blindness to race, or about preventing caste-like subordination regardless of classification method?',
    'Historical reconstruction of the Fourteenth Amendment''s original public meaning (contested between colorblind and historical-subordination framings) combined with doctrinal archaeology of Supreme Court holdings from Strauder (1880) through contemporary cases.',
    'If the clause is fundamentally about subordination prevention, the colorblind reading forecloses remedial race-consciousness and misses violations that operate through facially neutral means; if it is fundamentally about classification elimination, the antisubordination reading permits state-imposed hierarchies provided they avoid explicit classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(colorblind_vs_antisubordination_kernel_contest, conceptual, 'Whether the Fourteenth Amendment targets classifications or caste outcomes.').

omega_variable(
    remedial_action_necessity_empirical,
    'Does the categorical prohibition on race-conscious admissions prevent documented harms to historically excluded groups, or does it merely defer the harm to a future generation while preserving the appearance of colorblindness?',
    'Long-term empirical study of institutional access, wealth accumulation, and intergenerational outcomes comparing jurisdictions with colorblind prohibitions to those permitting remedial race-consciousness, controlling for other policy variations.',
    'Evidence that colorblind policy substantially reduces access for historically excluded groups would support the antisubordination reading (the policy entrenches subordination despite formal equality); evidence that access converges over time would support colorblind reading (formal equality is sufficient).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_action_necessity_empirical, empirical, 'Whether colorblind formal equality produces substantive equality outcomes.').

omega_variable(
    formal_vs_substantive_equality_framework,
    'Does ''equal protection'' inhere in the treat-alike rule (formal equality) or in the achieved outcome distribution (substantive equality)?',
    'Jurisprudential analysis of what ''protection'' means in context: if it means ''immunity from classification,'' formal equality suffices; if it means ''protection from subordination outcomes,'' formal equality may be insufficient.',
    'This is the foundational axis separating colorblind and antisubordination readings. If formal equality is what the clause protects, colorblind reading stands; if outcome-protection is the clause''s work, colorblind reading forecloses the mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(formal_vs_substantive_equality_framework, conceptual, 'The semantic range of ''protection'' in equal protection doctrine.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the colorblind reading''s suppression of remedial policies a structural barrier (the reading forecloses a mechanism by doctrinal rule) or internalized (acceptance of colorblind framing makes agents believe alternative frameworks are impossible)?',
    'Examination of decision-making by admissions officials and legislators when faced with colorblind constraints: do they accept the constraint as legitimate law (structural acceptance, internalized norm) or view it as an illegitimate prohibition they would circumvent if possible (structural barrier only)?',
    'If suppression is largely structural, the constraint persists via enforcement; if largely internalized, the constraint persists through belief-alignment and would persist even if enforcement were suspended — making the measured suppression a floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of remedial race-consciousness is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__colorblind_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_kernel__colorblind_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(equa_tr_t6, equal_protection_kernel__colorblind_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(equa_tr_t12, equal_protection_kernel__colorblind_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(equa_tr_t18, equal_protection_kernel__colorblind_reading, theater_ratio, 18, 0.34).
narrative_ontology:measurement(equa_tr_t24, equal_protection_kernel__colorblind_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(equa_tr_t30, equal_protection_kernel__colorblind_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_kernel__colorblind_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(equa_be_t6, equal_protection_kernel__colorblind_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(equa_be_t12, equal_protection_kernel__colorblind_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(equa_be_t18, equal_protection_kernel__colorblind_reading, base_extractiveness, 18, 0.59).
narrative_ontology:measurement(equa_be_t24, equal_protection_kernel__colorblind_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(equa_be_t30, equal_protection_kernel__colorblind_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_kernel__colorblind_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(equa_su_t6, equal_protection_kernel__colorblind_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(equa_su_t12, equal_protection_kernel__colorblind_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(equa_su_t18, equal_protection_kernel__colorblind_reading, suppression_requirement, 18, 0.54).
narrative_ontology:measurement(equa_su_t24, equal_protection_kernel__colorblind_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(equa_su_t30, equal_protection_kernel__colorblind_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__colorblind_reading, identity_coordination).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% The Equal Protection Clause kernel generates three constraint stories: colorblind_reading (categorical prohibition on state racial classification), remedial_reading (permits narrowly tailored race-conscious remedies), and antisubordination_reading (targets caste subordination regardless of classification). Each reading is a structurally complete constraint with its own ε, beneficiary/victim structure, and classification. They coexist as institutionally live readings held by different Supreme Court coalitions and civil rights traditions. The readings are linked via network.affects_constraints: colorblind_reading forecloses the remedial reading (if colorblind is law, remedial race-consciousness violates it) and influences the antisubordination reading (colorblind doctrine makes antisubordination framing incoherent in the same framework). Decomposition is necessary because the three readings have different empirical status: colorblind is current Supreme Court doctrine (high institutional power), remedial is a competing reading with significant institutional support (Justice Kagan dissents, academic consensus), antisubordination is a re-emergent reading gaining momentum in critical race theory and social-movement constituencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
