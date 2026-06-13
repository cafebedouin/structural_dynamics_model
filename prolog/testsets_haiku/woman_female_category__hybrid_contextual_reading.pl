% ============================================================================
% CONSTRAINT STORY: woman_female_category__hybrid_contextual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__hybrid_contextual_reading, []).

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
 *   constraint_id: woman_female_category__hybrid_contextual_reading
 *   human_readable: Hybrid Contextual Reading: Category Membership Varies by Domain
 *   domain: political_philosophy/bioethics/law
 *
 * SUMMARY:
 *   The hybrid contextual reading of the woman/female category holds that
 *   biological sex is the relevant categorization frame in medical,
 *   reproductive, and sports contexts, while gender identity is the relevant
 *   frame in legal and social-recognition contexts. This reading instantiates
 *   one answer to the contested kernel: instead of choosing a universal
 *   principle (sex-only or identity-only), it asserts that legitimacy varies
 *   by domain. The constraint enforces this domain-switching rule by
 *   requiring institutions to maintain dual categorization systems, apply
 *   different criteria in different spaces, and defend the inconsistency as
 *   pragmatic rather than principled. The reading reflects a real
 *   institutional accommodation that emerged from regulatory and judicial
 *   precedent in the 2010s–2020s; it is the reading most institutions have
 *   adopted operationally, though it remains contested by advocates of both
 *   sibling readings. The extraction is moderate (0.58) because the
 *   constraint requires active institutional labor to maintain and generates
 *   genuine conflict that no party fully accepts; suppression is higher
 *   (0.71) because the constraint's persistence depends on institutional
 *   actors enforcing domain-switching rules against resistance from both
 *   trans-rights and sex-essentialist advocates.
 *
 * KEY AGENTS:
 *   - institutional_policy_administrators: agenda-setters operating the dual-categorization machinery; powerful, entranced in the bureaucratic cost of managing two parallel tracks (d~0.15, structural beneficiary)
 *   - trans_women_in_biological_sex_contexts: payers bearing subordination in medical/sports domains; powerless, identity-locked, facing repeated misclassification (d~0.85, structural target)
 *   - cisgender_women_in_gender_identity_contexts: payers bearing dilution of sex-based category in legal/social domains; organized but constrained by institutional consensus (d~0.60, structural co-target)
 *   - sex_essentialist_advocates: payers bearing subordination of sex-only principle in legal domains; moderate power, mobile exit but constrained by institutional consensus (d~0.65, structural co-target)
 *   - gender_identity_advocates: partial beneficiaries in legal/social domains, subordinated in medical/sports; moderate power, mobile (d~0.35, mixed directionality)
 *   - medical_professionals: agenda-setters (implementing dual categorization); institutional power, analytical frame (d~0.20, structural beneficiary)
 *   - sports_governing_bodies: agenda-setters (domain-specific rules); powerful, managing the boundary between sex-based and identity-based competition (d~0.15, structural beneficiary)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, 0.58).
domain_priors:suppression_score(woman_female_category__hybrid_contextual_reading, 0.71).
domain_priors:theater_ratio(woman_female_category__hybrid_contextual_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__hybrid_contextual_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__hybrid_contextual_reading, "Hybrid Contextual Reading: Category Membership Varies by Domain").
narrative_ontology:topic_domain(woman_female_category__hybrid_contextual_reading, "political_philosophy/bioethics/law").

domain_priors:requires_active_enforcement(woman_female_category__hybrid_contextual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__hybrid_contextual_reading, '589493da-f84a-42e3-a4f5-2380fec31407').
narrative_ontology:cs_kernel_codification('589493da-f84a-42e3-a4f5-2380fec31407', distributed).
narrative_ontology:cs_authority_grounding('589493da-f84a-42e3-a4f5-2380fec31407', extraction).
narrative_ontology:cs_interpretation_layer_present('589493da-f84a-42e3-a4f5-2380fec31407').
narrative_ontology:cs_reading_relation('589493da-f84a-42e3-a4f5-2380fec31407', woman_female_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('589493da-f84a-42e3-a4f5-2380fec31407', woman_female_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('589493da-f84a-42e3-a4f5-2380fec31407', foundational, context_utility_determines_category).
narrative_ontology:cs_axiom_status(context_utility_determines_category, holdable).
narrative_ontology:cs_axiom_grounding('589493da-f84a-42e3-a4f5-2380fec31407', context_utility_determines_category, instrumental).
narrative_ontology:cs_axiom('589493da-f84a-42e3-a4f5-2380fec31407', secondary, institutional_domain_authority_resolves_category_disputes).
narrative_ontology:cs_axiom_status(institutional_domain_authority_resolves_category_disputes, holdable).
narrative_ontology:cs_axiom_grounding('589493da-f84a-42e3-a4f5-2380fec31407', institutional_domain_authority_resolves_category_disputes, conventional).
narrative_ontology:cs_reference_frame('589493da-f84a-42e3-a4f5-2380fec31407', unified_universal_category_principle).
narrative_ontology:cs_drift_state('589493da-f84a-42e3-a4f5-2380fec31407', contemporary_multi_context_institutional_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('589493da-f84a-42e3-a4f5-2380fec31407', '').
narrative_ontology:cs_kernel_id(woman_female_category__hybrid_contextual_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, institutional_policy_administrators).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, legal_recognition_authorities).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, trans_women_in_biological_sex_contexts).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, cisgender_women_in_gender_identity_contexts).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, sex_essentialist_advocates_in_legal_contexts).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__hybrid_contextual_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(woman_female_category__hybrid_contextual_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__hybrid_contextual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__hybrid_contextual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The hybrid reading extracts through ADMINISTRATIVE BURDEN and INSTITUTIONAL POWER ASYMMETRY. Extractiveness measures the degree to which the constraint transfers authority over category membership from individuals or universal standards to domain-specific administrators. At 0.58, this is substantial but not total—it reflects that while institutions control the switching rules, the underlying categories (biological sex, gender identity) remain partially self-determined by individuals. Extractiveness rises from 0.48 to 0.58 over the interval, suggesting increasing institutional hardening of the hybrid framework (more formalized dual-categorization systems, more explicit policy codification). Theater-ratio (0.44) reflects that institutional administrators present the hybrid reading as a pragmatic compromise serving everyone, when in reality it concentrates decision-making power in their hands and generates real conflict for trans people and sex-essentialists. The ratio rises from 0.28 to 0.44, suggesting the performative aspect of 'pragmatic compromise' has intensified as resistance has grown. Suppression (0.71) is high because maintaining the hybrid reading requires suppressing both the sex-essentialist argument (that sex should apply everywhere) and the pure gender-identity argument (that identity should apply everywhere). The suppression works through INSTITUTIONAL CONSENSUS—by framing the hybrid reading as the only reasonable middle ground, institutions discourage serious advocacy for either alternative. Resistance (0.73) is correspondingly high because both sibling readings remain live: sex-essentialist movements in sports and some medical contexts actively resist gender-identity recognition; trans-rights movements resist the subordination of identity in medical/sports domains. The constraint persists not because either side accepts it but because institutional administrators have enough power to enforce it despite opposition. All measurements share one temporal grid (every metric at every time point) so the trajectory is coherent: the constraint is consolidating (extraction/suppression/theater rising, reaching plateaus by t=20), suggesting a shift from emerging compromise to institutionalized settlement.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional-administrator seat, this is a genuine coordination solution—managing two legitimate but different coordination problems (medical safety, legal dignity) through appropriate domain-switching. From the trans-women-in-medical-contexts seat, it is pure extraction: their identity is recognized legally but subordinated medically, precisely where they most need affirmation (reproductive health, hormone management). From the cisgender-women seat concerned about sex-based rights, it is pure extraction in the opposite direction: their biological-category claims are subordinated in legal/social contexts, which is where they matter for sex-based protections. The engine computes these divergent classifications from the structural data (power, exit_options, role): institutional administrators sit at d~0.15 (beneficiaries of decision-making authority); trans women sit at d~0.85 (targets of subordination in medical domains where they are trapped by identity-lock); cisgender women sit at d~0.60 (moderate targets, less trapped because organized resistance is possible). The constraint's type appears different from each seat: administrators perceive rope (coordination without extraction); targets perceive snare (extraction defended as coordination). The engine's per-seat classification captures this perspectival divergence without reconciling it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from four atoms: power (institutional > organized > moderate > powerless), time_horizon (generational > biographical > immediate), exit_options (analytical > arbitrage > mobile > constrained > identity_locked > trapped), and spatial_scope (universal > global > continental > national > regional > local). The hybrid reading concentrates categorization power in institutional hands (medical, legal, sports administrators), which shifts d-values downward (toward beneficiary) for institutional actors and upward (toward target) for powerless/moderate/identity-locked groups. Trans women in medical contexts have powerless + identity_locked + biographical, which positions them at d~0.85 (full targets). Institutional administrators have institutional power + analytical time_horizon + analytical exit, which positions them at d~0.15 (near beneficiary). Cisgender women organized for sex-based rights have organized power + biographical time_horizon + constrained exit (can organize but cannot exit the category), which positions them at d~0.60 (moderate co-targets, less trapped than trans women but subordinated in legal domains where their arguments are suppressed). The derivation chain flows: beneficiary/victim declarations → power/exit atoms → directionality → effective extraction (χ). The hybrid reading's beneficiaries are institutional administrators (gain decision-making authority) and legal-recognition authorities (gain consensus consensus framework). Victims are trans women (subordinated in medical/sports) and sex-essentialists (subordinated in legal domains).
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading emerged as a MANDATROPHY RESOLUTION: the founding problem (accommodate both biological sex relevance and gender identity validity) no longer generates the same coordination needs it once did. In the 2010s, the coordination problem was acute—institutions genuinely needed a way to recognize gender identity in legal contexts while maintaining sex-based categorization in medical contexts, because the prior pure-sex reading was causing harm to trans people and the pure-identity reading was causing political backlash. By the 2020s, the mandatrophy surfaces: the constraint persists as institutional inertia (it's now 'how we do things'), but the original founding problem has been partly solved by advances in affirming medical care (many clinicians now ask both sex and gender, using both for appropriate purposes), legal recognition (most jurisdictions allow identity-based legal markers), and sports science (sex-hormone-level testing offers alternatives to categorical sex-based sorting). The constraint should have decomposed into more precise domain-specific rules rather than remaining a meta-rule about context-switching. Instead, the constraint has theater-ified: institutions defend the hybrid framework not as a temporary compromise but as the principled position (which it is not—the principle is 'use whatever categories matter for this domain's function,' not 'switch rules by context'). Theater_ratio rising from 0.28 to 0.44 captures this mandatrophy dynamic: increasing proportion of the constraint's operation is defending the compromise itself rather than solving the coordination problems. A Tangled Rope (not a pure Rope) because: (1) genuine coordination function exists (managing real differences in what 'woman' means across domains), (2) asymmetric extraction exists (institutional administrators gain decision-making power while trans people and sex-essentialists lose it), (3) active enforcement required (institutions must suppress arguments that the rule is arbitrary). The classification would collapse into pure Snare if the coordination function genuinely dissolved; it remains Tangled Rope because domain-specific category-use does serve real coordination needs—the problem is that those needs could be met through more transparent, less extractive mechanisms (explicit domain-specific criteria rather than meta-rule-switching).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medical_materiality_of_sex,
    'What medical facts are genuinely dependent on biological sex (chromosomal markers, reproductive anatomy, hormone profiles, developmental history) such that sex-based categorization serves diagnosis and treatment, versus which facts are conflated with sex but actually depend on other variables?',
    'Systematic medical evidence review: for each medical condition commonly attributed to sex category, determine whether the condition''s causal mechanism involves the sex category itself or some downstream consequence of it (e.g., ovarian cancer depends on ovarian tissue, not on XX chromosomes per se; hormone profiles depend on endocrine history, not on natal sex per se).',
    'If medical materiality is narrower than the hybrid reading assumes (sex-based categorization is useful only for a few specific conditions), the medical-context justification for domain-switching weakens and the reading shifts toward less-justified extraction in those domains. If medical materiality is broader (sex-based categorization is clinically necessary in many contexts), the hybrid reading''s medical-domain framework is more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_materiality_of_sex, empirical, 'The scope of medical conditions for which biological sex is the causally relevant categorization.').

omega_variable(
    sports_fairness_criterion_ambiguity,
    'What makes athletic competition fair or unfair—equal opportunity to win, or something else? If equal opportunity, what physical variables determine it (muscle mass, bone density, hormone profiles, athletic training history), and does sex category track those variables or does sex category conflate multiple distinct variables?',
    'Sports science evidence on fairness criteria; case studies from sports that have adopted hormone-level or other non-categorical tests; empirical data on competitive outcomes under different eligibility rules.',
    'If sex-category sorting is the most cogent fairness criterion, the hybrid reading''s sports-domain framework is justified. If fairness is better served by context-specific metrics (hormone profiles, weight classes, etc.) that don''t reduce to categorical sex, the hybrid reading''s blanket sex-based categorization in sports is less justified and shifts toward unjustified extraction/suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sports_fairness_criterion_ambiguity, conceptual, 'Whether athletic fairness is best served by categorical sex or by more granular criteria.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.71) structural (institutional rules enforced against external resistance) or internalized (the affected groups have partially accepted the hybrid reading as legitimate, so suppression feels less coercive)?',
    'Post-resistance suppression trajectory: measure changes in advocacy intensity, public opinion, political mobilization over time. If suppression persists at current levels despite institutional consensus, it is partly internalized. If suppression must rise to maintain the constraint against growing resistance, it is mostly structural.',
    'If internalized, the constraint''s effective suppression is lower than the structural measure suggests, and its trajectory is toward institutionalization (moving toward Piton). If structural, suppression must be maintained by active enforcement and the trajectory is toward visible conflict (moving toward Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression operates through internalized acceptance or external institutional enforcement.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the three readings (sex-biology, gender-identity, hybrid-contextual) genuinely incommensurable—applying to different aspects of a multi-dimensional phenomenon—or do they make competing claims about a single fact that has a unique correct answer?',
    'Philosophical analysis and empirical case studies: do the three readings coexist peacefully when applied to specific cases (e.g., a trans woman''s reproductive health needs both sex and identity data), or do they necessarily conflict (forcing a choice about which reading determines official category membership)?',
    'If incommensurable, the hybrid reading is epistemically justified—it acknowledges different dimensions. If they compete for a single fact, the hybrid reading is evasive and one reading must be privileged as more fundamentally true.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the three readings capture different legitimate dimensions or make competing truth-claims about one dimension.').

omega_variable(
    identity_lock_heterogeneity,
    'Is the exit_options value ''identity_locked'' uniform across all trans women, or does it vary by degree and type of identity fusion—some trans women experiencing absolute identity-lock (being a woman is non-negotiable), others experiencing contextual identity-flexibility (willing to engage with biological categories in medical contexts if it serves their health)?',
    'Qualitative research on trans women''s perspectives on medical categorization: variation in willingness to use biological-sex markers for specific medical purposes (e.g., reproductive health screening) versus resistance to sex-based categorization across all contexts.',
    'High heterogeneity would suggest the constraint should be differentiated—some trans women might choose biological-sex categorization in narrowly medical contexts if done respectfully, while others would refuse. This would lower the effective extraction for those with more flexibility and raise it for those with absolute identity-lock. Current constraint assumes uniform identity-lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_heterogeneity, empirical, 'Variation in identity-lock intensity and context-flexibility among trans women.').

omega_variable(
    committer_frame_axiom_contested,
    'Is ''context_utility_determines_category'' actually the foundational axiom of the hybrid reading, or is the reading grounded in a different, less visible axiom—such as ''institutional authority should resolve philosophical disputes by pragmatic institutional consensus''? If the latter, the reading''s legitimacy rests on institutional power rather than principled justification.',
    'Textual and institutional analysis: examine the justifications institutions offer for domain-switching (do they cite context-utility or institutional pragmatism?). Examine whether the hybrid reading would be equally acceptable if the institutional power balance shifted (would advocates defend the reading on principle if they lost institutional support for it?)',
    'If context_utility is the true grounding, the hybrid reading is a principled epistemic position. If institutional-pragmatism is the true grounding, the reading is a form of extraction disguised as principle—it benefits institutional administrators by giving them authority over category disputes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_axiom_contested, conceptual, 'Whether the hybrid reading is grounded in context-specific functional utility or in institutional-authority resolution of disputes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__hybrid_contextual_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__hybrid_contextual_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(woma_tr_t5, woman_female_category__hybrid_contextual_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(woma_tr_t10, woman_female_category__hybrid_contextual_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(woma_tr_t15, woman_female_category__hybrid_contextual_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__hybrid_contextual_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement(woma_tr_t25, woman_female_category__hybrid_contextual_reading, theater_ratio, 25, 0.44).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__hybrid_contextual_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(woma_be_t5, woman_female_category__hybrid_contextual_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(woma_be_t10, woman_female_category__hybrid_contextual_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(woma_be_t15, woman_female_category__hybrid_contextual_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(woma_be_t20, woman_female_category__hybrid_contextual_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(woma_be_t25, woman_female_category__hybrid_contextual_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__hybrid_contextual_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(woma_su_t5, woman_female_category__hybrid_contextual_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(woma_su_t10, woman_female_category__hybrid_contextual_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(woma_su_t15, woman_female_category__hybrid_contextual_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(woma_su_t20, woman_female_category__hybrid_contextual_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(woma_su_t25, woman_female_category__hybrid_contextual_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__hybrid_contextual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__hybrid_contextual_reading, 0.12).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'woman_female_category.' The kernel has three live readings, each instantiating a different constraint: sex_biology_reading (biological sex determines category across all contexts), gender_identity_reading (gender identity determines category across all contexts), and hybrid_contextual_reading (this constraint: biological sex applies in medical/sports contexts, gender identity applies in legal/social contexts). Each reading has a structurally distinct epsilon, victim set, and beneficiary structure. The ε values differ substantially: sex_biology_reading assumes low extraction (natural biological fact), gender_identity_reading assumes moderate extraction (identity claim requiring institutional validation), hybrid_contextual_reading assumes moderate extraction (institutional domain-switching costs). Victim sets differ: sex-biology reading victimizes those whose identity diverges from biological sex; gender_identity reading victimizes those who argue for biological-basis category claims; hybrid_contextual reading victimizes both groups contextually—trans women in medical domains, sex-essentialists in legal domains. All three readings are live institutional positions; none forecloses the others because they rest on different principled grounds and different institutional actors advocate for each. The network edges link all three as a constraint family; the omegas in this reading document the kernel-contestation and the incommensurability question.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_female_category__hybrid_contextual_reading, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
