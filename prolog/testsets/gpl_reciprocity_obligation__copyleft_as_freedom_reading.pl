% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_freedom_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_freedom_reading
 *   human_readable: GPL Reciprocity Obligation (Copyleft as Freedom Reading)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   GPL's reciprocal obligation (copyleft) requires that any software derived
 *   from GPL-licensed code must also be released under GPL. This constraint
 *   is read by its advocates as protecting user freedoms by preventing
 *   proprietary enclosure of shared code improvements; they frame the
 *   obligation as a freedom-preserving mechanism. By competitors and
 *   proprietary vendors, it is read as a restriction on business model
 *   freedom and as coercive licensing rather than freedom. This story
 *   instantiates the freedom-preservation reading: the constraint's
 *   beneficiaries are downstream users and open source contributors who gain
 *   assurance that improvements will flow back to them, and the payers are
 *   proprietary integrators who cannot enclose GPL code in closed products.
 *   The founding problem—commons enclosure by proprietary vendors—is
 *   contested: GPL advocates say it was real and remains a threat;
 *   proprietary advocates say it was overstated or that permissive licenses
 *   solve it better.
 *
 * KEY AGENTS:
 *   - downstream_users (beneficiary, powerless/mobile) — gain access and modification rights
 *   - open_source_contributors (beneficiary, moderate/arbitrage) — receive improvements and retain control
 *   - proprietary_software_integrators (payer, powerful/constrained) — forbidden from enclosing GPL code
 *   - license_enforcement_organizations (agenda_setter, organized/analytical) — monitor and litigate compliance
 *   - proprietary_license_advocates (excluded, powerful/constrained) — would contest the freedom framing
 *   - permissive_license_communities (observer, organized/mobile) — represent alternative governance model
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.38).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.72).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "GPL Reciprocity Obligation (Copyleft as Freedom Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'c5b797ae-8e8a-4657-b167-14af46119659').
narrative_ontology:cs_kernel_codification('c5b797ae-8e8a-4657-b167-14af46119659', fixed_text).
narrative_ontology:cs_authority_grounding('c5b797ae-8e8a-4657-b167-14af46119659', lineage).
narrative_ontology:cs_interpretation_layer_present('c5b797ae-8e8a-4657-b167-14af46119659').
narrative_ontology:cs_reading_relation('c5b797ae-8e8a-4657-b167-14af46119659', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5b797ae-8e8a-4657-b167-14af46119659', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('c5b797ae-8e8a-4657-b167-14af46119659', foundational, user_freedom_as_foundational).
narrative_ontology:cs_axiom_status(user_freedom_as_foundational, holdable).
narrative_ontology:cs_axiom_grounding('c5b797ae-8e8a-4657-b167-14af46119659', user_freedom_as_foundational, deontological).
narrative_ontology:cs_axiom('c5b797ae-8e8a-4657-b167-14af46119659', secondary, proprietary_capture_as_freedom_violation).
narrative_ontology:cs_axiom_status(proprietary_capture_as_freedom_violation, holdable).
narrative_ontology:cs_axiom_grounding('c5b797ae-8e8a-4657-b167-14af46119659', proprietary_capture_as_freedom_violation, empirically_contingent).
narrative_ontology:cs_reference_frame('c5b797ae-8e8a-4657-b167-14af46119659', user_freedom_preservation_mandate).
narrative_ontology:cs_drift_state('c5b797ae-8e8a-4657-b167-14af46119659', contemporary_proprietary_integration_landscape, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c5b797ae-8e8a-4657-b167-14af46119659', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, derivative_open_source_developers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_software_integrators).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).
:- end_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the constraint does impose real costs on proprietary integrators—they cannot build proprietary extensions on GPL code without releasing modifications. However, the extraction is not as high as a snare because: (a) the constraint solves a genuine coordination problem (feedback loop, commons preservation), (b) exit options for proprietary integrators exist (rewrite, permissive relicense, dual-license), and (c) the beneficiaries (downstream users) gain genuine coordination benefits alongside the constraint's operation. Suppression is higher (0.72) because enforcement of copyleft requires: active legal monitoring, contested interpretation of derivative-work boundaries, and suppression of alternative framings (permissive licensing). The high accessibility_collapse (0.81) reflects that once GPL's reciprocal obligation is understood, proprietary integrators see few viable alternatives to compliance or abandonment; the constraint's operation is transparent and binding. Resistance (0.58) reflects ongoing legal and policy challenges from proprietary advocates and permissive-license advocates, but no organized counterforce has overturned the constraint. Theater is low (0.18) because the constraint's operation is largely functional: it genuinely enforces reciprocal disclosure and does coordinate improvements back to open projects; the performative component is modest (rhetorical framing battles, negotiation theater around license interpretation).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (license enforcement organizations) and beneficiaries (downstream users, contributors) experience this as a freedom-preserving coordination mechanism. Proprietary integrators experience it as coercive enclosure of their business model freedom. Permissive-license advocates see it as unnecessarily restrictive when permissive alternatives exist. The engine's per-seat computation should reveal: (1) agenda-setters and beneficiaries compute as rope-beneficiary seats (coordination function is genuine, costs are coordination cost, not extraction); (2) proprietary integrators compute as snare-payer seats (they bear costs, have constrained exit, the constraint is enforced against their preference); (3) permissive advocates compute as observer seats (they see an alternative coordination form that would be superior). The reading boundary is crucial: this story claims GPL is fundamentally freedom-preserving; the restriction reading claims it is fundamentally restrictive; the commons reading claims it is institutionally necessary. The three readings have different ε values precisely because they measure different constraint aspects.
 *
 * DIRECTIONALITY LOGIC:
 *   Downstream users are beneficiaries with low directionality (d near 0.0): they collect the benefit (access to source, improvements) without enforcement burden or coercive cost. Open source contributors are beneficiaries with slightly higher directionality (d ≈ 0.15): they gain improvements flowing back, but they also experience some enforcement cost in maintaining GPL compliance. Proprietary integrators are victims with high directionality (d near 1.0): they bear the cost (must disclose or avoid GPL), have constrained exit (cannot build proprietary extensions), and the constraint operates against their preference. No directionality override is needed; the derivation from beneficiary/victim + exit + power captures the structure accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   GPL reciprocity obligation avoids mandatrophy classification in this reading because: (1) the founding problem (commons enclosure) remains contested but not universally accepted as dead—proprietary capture remains a stated concern in open source governance; (2) the constraint's coordination function is structural and ongoing—improvements genuinely flow back to GPL projects at higher rates than they would under permissive licensing; (3) the beneficiary set is broad (all downstream users) and diffuse (global open source ecosystem), not a narrow rent-collecting coalition. However, the (founding_problem_status=contested, disappearance_verdict=world_rearranges) pair does create a mandatrophy flag risk: if the constraint were to disappear, the world would rearrange significantly, yet the founding problem's continued existence is contested. This is resolved by noting that the contestation is over INTERPRETATION not FACT—proprietary advocates acknowledge the enclosure threat exists but deny it justifies GPL's solution. The flag should trigger a review of whether the constraint's persistence is justified by ongoing need (yes, per open source governance consensus) or by beneficiary capture (limited—the enforcement organizations are non-profits; contributors and users are broad-based).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Is the founding problem (proprietary enclosure of shared improvements) still a live threat in contemporary software ecosystems, or has it been substantially solved by market evolution, permissive licenses, and open source maturation?',
    'Empirical analysis of proprietary software products incorporating GPL code and failing to release modifications; survey of open source maintainers on the frequency and impact of proprietary capture; comparative analysis of improvement flow-back rates under GPL vs. permissive licenses.',
    'If the founding problem is dead, the constraint becomes mandatrophy candidate (persisting mechanism with solved problem). If it remains live, the rope/snare classification hinges on whether GPL''s solution is proportionate and necessary or overly restrictive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether commons enclosure remains an active threat GPL must address.').

omega_variable(
    restriction_vs_freedom_frame,
    'Is GPL''s reciprocal obligation fundamentally a freedom-protecting mechanism (this reading) or a restriction on business model freedom (sibling reading)? Can both framings be simultaneously true?',
    'Philosophical and legal analysis of what ''freedom'' means in software licensing context: freedom-to-use vs. freedom-to-enclose. Empirical study of whether proprietary integrators report GPL as restricting their freedom to-build or merely to-capture.',
    'If restriction-framing is equally valid, this reading''s claimed type (rope, freedom-preserving) would be contested and the engine should compute differently from proprietary integrators'' seats. If freedom-framing is foundational to the GPL''s legitimacy, this reading holds and restriction-reading is the alternative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restriction_vs_freedom_frame, conceptual, 'Whether GPL''s reciprocity is fundamentally freedom-preserving or restrictive.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the high suppression (0.72) primarily structural (legal enforcement, violation litigation, monitoring overhead) or has it become internalized in proprietary developers'' norms (avoiding GPL even when legally permitted to use it)?',
    'Post-GPL-adoption behavior studies: if proprietary developers freely choose permissive licenses when GPL-derived functionality could achieve the same outcome, suppression is partly internalized. If they only avoid GPL when legal enforcement is visible, suppression is structural.',
    'Internalized suppression indicates the constraint has shifted from external coercion to self-imposed restraint, which would lower the measured extraction (suppression becomes absorbed into legitimate licensing choice) but raise the theater ratio (compliance becomes norm-adherence rather than compliance-with-rule).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether GPL''s suppressive force is structural or internalized in proprietary developer norms.').

omega_variable(
    kernel_reading_foreclosure,
    'Do the three sibling readings (freedom, restriction, commons) represent genuinely coexisting framings, or does one reading logically foreclose the others within GPL''s own legitimacy tradition?',
    'Deep reading of GPL preamble and FSF''s governance documents: does the FSF acknowledge all three readings as live, or does it assert one as foundational and others as misreadings? Legal precedent from GPL enforcement actions: do courts treat the readings as equally valid or privileged one?',
    'If one reading is foundational to GPL''s self-understanding, the other readings should be reclassified as readings of a different kernel (proprietary_licensing, perhaps) rather than as siblings. If all three coexist, the family structure is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether GPL sibling readings coexist or one is foundational and others are foreclosed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 1991, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1991, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 1991, 0.08).
narrative_ontology:measurement(gpl__tr_t2000, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2000, 0.11).
narrative_ontology:measurement(gpl__tr_t2008, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2008, 0.13).
narrative_ontology:measurement(gpl__tr_t2015, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2015, 0.16).
narrative_ontology:measurement(gpl__tr_t2020, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2020, 0.17).
narrative_ontology:measurement(gpl__tr_t2026, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2026, 0.18).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1991, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 1991, 0.22).
narrative_ontology:measurement(gpl__be_t2000, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(gpl__be_t2008, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2008, 0.32).
narrative_ontology:measurement(gpl__be_t2015, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2015, 0.36).
narrative_ontology:measurement(gpl__be_t2020, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2020, 0.37).
narrative_ontology:measurement(gpl__be_t2026, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2026, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1991, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 1991, 0.52).
narrative_ontology:measurement(gpl__su_t2000, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2000, 0.61).
narrative_ontology:measurement(gpl__su_t2008, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2008, 0.66).
narrative_ontology:measurement(gpl__su_t2015, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(gpl__su_t2020, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement(gpl__su_t2026, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_freedom_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.12).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% The GPL reciprocity obligation decomposes into three structurally distinct constraints based on which reading of the kernel is instantiated. The freedom reading (this story) emphasizes downstream user benefit and proprietary integrator cost; ε=0.38 reflects moderate extraction justified by coordination function. The restriction reading emphasizes the cost to proprietary developers and competing license philosophies; that reading would show higher ε and lower accessibility_collapse. The commons reading emphasizes institutional necessity for commons preservation; that reading emphasizes beneficiary as the open source ecosystem collectively. These three readings are incompatible in a single constraint story (per ε-invariance, OQ-76: different ε values indicate different constraints), so GPL_reciprocity decomposes into a family of three constraint stories. Each story is a clean, ε-invariant constraint instantiating one reading. The three stories are linked via network.affects_constraints: the freedom reading influences the other two (FSF's primary framing), while the other two coexist as alternative legitimate readings held by different parties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
