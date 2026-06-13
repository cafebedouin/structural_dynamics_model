% ============================================================================
% CONSTRAINT STORY: woman_category__intersex_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__intersex_accommodation_reading, []).

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
 *   constraint_id: woman_category__intersex_accommodation_reading
 *   human_readable: Woman Category: Intersex-Accommodation Reading
 *   domain: political_philosophy/law/bioethics
 *
 * SUMMARY:
 *   The intersex-accommodation reading of the contested 'woman' category
 *   holds that biological sex is non-binary and that legal category
 *   membership should recognize female-typical and ambiguous intersex biology
 *   as legitimate variations within 'woman.' This reading emerged from
 *   intersex-rights advocacy and medical evidence that DSD and intersex
 *   conditions exist on a spectrum rather than as pathological anomalies. It
 *   differs structurally from the sex-biology reading (which insists on
 *   binary chromosomal/anatomical criteria) and the gender-identity reading
 *   (which makes identity the sole criterion). The intersex-accommodation
 *   reading retains biological reference but reframes what counts as female
 *   embodiment. In most legal and social domains, extractiveness is low
 *   because the population is small and the coordination need is narrow:
 *   formal recognition and non-interference. In elite sports, extractiveness
 *   rises sharply because the reading collides with performance-boundary
 *   enforcement, creating a high-stakes coercive medical-testing apparatus.
 *   The reading vindicates the proposition that 'biological sex is not
 *   binary' but faces the persistent problem that any boundary-drawing—even
 *   around a spectrum—will exclude some people from recognition. This is the
 *   constraint's core tragedy: acknowledging variation does not eliminate the
 *   need to make category determinations, and those determinations continue
 *   to bear on identity-locked people whose exclusion carries material
 *   consequences.
 *
 * KEY AGENTS:
 *   - intersex_people_female_typical_biology: beneficiaries of legal recognition; identity-locked position enables direct identification with category
 *   - intersex_people_ambiguous_biology: victims of ongoing medical gatekeeping despite nominal spectrum acknowledgment; administrative boundary-determination replaces surgical coercion but remains identity-locking
 *   - elite_female_athletes_with_dsd: payers in sports domain; nominal legal recognition paired with coercive testosterone-suppression medical requirements
 *   - feminist_legal_frameworks_recognizing_embodied_variance: organized beneficiaries; use the reading to advance non-pathologization doctrine and intersex rights in policy
 *   - sports_regulatory_bodies: agenda-setters for sports domain; respond to the reading by layering performance criteria and enforcing medical gatekeeping
 *   - sex_biology_reading_adherents: excluded from enforcement conversation; contest the reading's coherence and scientific validity in public discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, 0.38).
domain_priors:suppression_score(woman_category__intersex_accommodation_reading, 0.52).
domain_priors:theater_ratio(woman_category__intersex_accommodation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__intersex_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__intersex_accommodation_reading, "Woman Category: Intersex-Accommodation Reading").
narrative_ontology:topic_domain(woman_category__intersex_accommodation_reading, "political_philosophy/law/bioethics").

domain_priors:requires_active_enforcement(woman_category__intersex_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__intersex_accommodation_reading, '4df40b90-810e-4648-aaad-dc95c259b4ab').
narrative_ontology:cs_kernel_codification('4df40b90-810e-4648-aaad-dc95c259b4ab', distributed).
narrative_ontology:cs_authority_grounding('4df40b90-810e-4648-aaad-dc95c259b4ab', distributed).
narrative_ontology:cs_reading_relation('4df40b90-810e-4648-aaad-dc95c259b4ab', woman_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('4df40b90-810e-4648-aaad-dc95c259b4ab', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('4df40b90-810e-4648-aaad-dc95c259b4ab', foundational, biological_sex_is_spectrum_not_binary).
narrative_ontology:cs_axiom_status(biological_sex_is_spectrum_not_binary, holdable).
narrative_ontology:cs_axiom_grounding('4df40b90-810e-4648-aaad-dc95c259b4ab', biological_sex_is_spectrum_not_binary, empirically_contingent).
narrative_ontology:cs_axiom('4df40b90-810e-4648-aaad-dc95c259b4ab', foundational, female_embodied_variation_is_legitimate).
narrative_ontology:cs_axiom_status(female_embodied_variation_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('4df40b90-810e-4648-aaad-dc95c259b4ab', female_embodied_variation_is_legitimate, deontological).
narrative_ontology:cs_reference_frame('4df40b90-810e-4648-aaad-dc95c259b4ab', spectrum_based_sex_category_acknowledging_dsd_variation).
narrative_ontology:cs_drift_state('4df40b90-810e-4648-aaad-dc95c259b4ab', contemporary_sports_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4df40b90-810e-4648-aaad-dc95c259b4ab', '').
narrative_ontology:cs_kernel_id(woman_category__intersex_accommodation_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, intersex_people_female_typical_biology).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, feminist_legal_frameworks_recognizing_embodied_variance).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, intersex_people_ambiguous_biology).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, elite_female_athletes_with_dsd).
narrative_ontology:constraint_vindicates(woman_category__intersex_accommodation_reading, biological_sex_is_not_binary).
narrative_ontology:constraint_vindicates(woman_category__intersex_accommodation_reading, female_embodiment_has_legitimate_variation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% People with intersex conditions whose biology aligns sufficiently with female-typical patterns (e.g., androgen-insensitivity syndrome, some congenital adrenal hyperplasia presentations) to be accommodated within an expanded 'woman' category. Under this reading, legal recognition of their female identity becomes possible without requiring identity-assertion-only frameworks. They gain documentary, social, and legal standing without needing to claim pure gender identity as the sole basis for category membership.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_people_female_typical_biology, beneficiary,
    powerless, biographical, identity_locked, global).

% People with intersex conditions whose biological presentation does not fit clearly into either the traditional male or female categories (e.g., some DSD presentations with genuinely ambiguous gonadal or chromosomal patterns). The intersex-accommodation reading creates a new administrative boundary: they must now be classified as either within or outside the expanded female category based on medical assessment. Those judged outside the category remain excluded; this reading shifts the burden of proof and medical gatekeeping rather than eliminating it.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_people_ambiguous_biology, payer,
    powerless, biographical, identity_locked, global).

% Athletes with differences of sex development (DSD) competing in women's elite sports (Caster Semenya case exemplar). The intersex-accommodation reading acknowledges their biology as legitimately within a spectrum, but in competitive sports contexts this creates a high-stakes boundary problem: acknowledging DSD as part of female biology without additional performance criteria opens the category to individuals with naturally elevated androgen levels. Sports organizations respond by adding performance-based testing (serum testosterone limits), which medical advocates argue is a form of coercive medical intervention. They face the constraint as enforced medical gatekeeping despite nominal legal recognition.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, elite_female_athletes_with_dsd, payer,
    moderate, biographical, constrained, global).

% Legal and policy frameworks (particularly in human-rights law, some medical ethics councils, and intersex-advocacy legal organizations) that adopt the biological-spectrum reading as grounds for category membership. This reading vindicates their theoretical commitment that embodied female variation is legitimate and non-pathological. It enables legal reform arguments (certificate of sex/gender change on non-identity grounds, medical non-intervention policies) and provides institutional backing for intersex rights advocacy.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, feminist_legal_frameworks_recognizing_embodied_variance, beneficiary,
    organized, generational, mobile, global).

% International sports federations (IOC, World Athletics, etc.) that must operationalize category definitions. The intersex-accommodation reading creates a direct conflict with their other mandate: fair competition. They respond by layering performance criteria (testosterone thresholds, hormone suppression requirements) onto the biological-spectrum reading, effectively creating a medical-intervention enforcement apparatus. They are not passive implementers of the reading but active boundary-maintainers who transform it into coercive medical gatekeeping in the sports domain.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sports_regulatory_bodies, agenda_setter,
    institutional, generational, mobile, global).

% Legal and policy advocates who hold that biological sex is a discrete binary and that 'woman' must map to typical female chromosomes and anatomy. They argue the intersex-accommodation reading is incoherent (it claims to acknowledge spectrum while still drawing a boundary somewhere) and that it obscures the material reality of sexual dimorphism. They are not part of the constraint's enforcement conversation, but their presence in public discourse about sex categories shapes how the reading is understood and challenged.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sex_biology_reading_adherents, excluded,
    organized, generational, mobile, global).

% Clinicians and researchers who understand the biological reality of DSD and intersex variation. They observe how the intersex-accommodation reading is implemented—sometimes as genuine recognition of variation, sometimes as cover for continued pathologization. They provide expert testimony in policy-setting and regulatory contexts, and their consensus on the biological non-binary nature of sex variation supports the reading's legitimacy in principle, even as they document its enforcement problems in practice.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, medical_practitioners_and_endocrinologists, observer,
    institutional, biographical, mobile, global).

% Legal and policy advocates who hold that gender identity is the primary ground for category membership. They occupy a potentially unstable position relative to the intersex-accommodation reading: the reading acknowledges biological diversity, which can support their anti-essentialist critique of biology as destiny, but the reading still USES biology as a classification ground (the spectrum itself), whereas identity-based reading uses internal gender identity. They are not enforcing this constraint but shape its interpretation through policy advocacy.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, gender_identity_reading_adherents, observer,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__intersex_accommodation_reading, sports_regulatory_bodies).
narrative_ontology:fixing_cost_class(woman_category__intersex_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a coherent legal and social category ('woman') that acknowledges biological diversity and provides documentary and institutional recognition for people with female-typical or ambiguous intersex biology without requiring them to claim gender identity as the sole ground for membership. Solves the coordination problem of how to recognize legitimate variation in female embodiment without collapsing 'woman' into 'anyone who identifies as a woman' or restricting it to a narrow chromosomal binary.
% TRANSFER_FUNCTION: Transfers recognition and institutional access (legal documents, healthcare, social legitimacy) to people with female-typical intersex conditions, while imposing on people with ambiguous-biology intersex conditions a requirement for medical and administrative boundary-determination. In sports domains, transfers the burden of legitimacy-proof from intersex athletes to sports organizations, which respond by imposing performance-based (hormone) medical intervention requirements.
% ABSENT_VOICES: Sex-binary adherents and gender-identity-primary advocates are excluded from the primary enforcement conversation, though they contest the reading in public discourse. Intersex people themselves are rarely in the agenda-setting seat; most are subjects of the category determination, not authors of its terms.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and sex categories reverted to binary sex-biology determinism, institutional recognition of intersex people would collapse in most jurisdictions; they would revert to undocumented status, healthcare barriers, and legal exclusion. In sports, the performance-boundary problem would persist but would be framed as excluding intersex athletes from women's categories rather than medically managing them within it.
% FOUNDING_PROBLEM: Legal and medical institutions treated intersex people as pathological anomalies requiring correction (surgical 'normalizing' procedures), and category-definition frameworks (law, medicine, sports) forced them into a false binary choice. The reading was developed to recognize that biological sex itself is non-binary and that legal categories should accommodate this fact without requiring intersex people to claim gender identity as their sole ground for recognition.
% FOUNDING_PROBLEM_CORROBORATION: Medical endocrinologists and intersex advocacy organizations (ISNA, Intersex Campaign for Equality) document ongoing practices of non-consensual genital surgery and enforced binary assignment in childhood. Human-rights organizations (UN, Amnesty International) have issued statements recognizing intersex people as a rights-bearing group. Sports regulatory bodies' continued use of testosterone testing in women's categories demonstrates the ongoing boundary-definitional problem.
narrative_ontology:disappearance_verdict(woman_category__intersex_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__intersex_accommodation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__intersex_accommodation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(woman_category__intersex_accommodation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__intersex_accommodation_reading_tests).
:- end_tests(woman_category__intersex_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low to moderate (0.38 endpoint) because the constraint provides genuine recognition for a marginalized population in most domains. Suppression is also moderate (0.52) because the recognition is not purely coercive—many intersex people value legal acknowledgment on biological grounds. However, suppression rises in contexts where boundary-enforcement becomes salient (sports, medical access, legal documentation), as captured in the grid's organizational and class levels. The measurement series shows extractiveness rising early (t0 to t24) as enforcement practices solidify, then plateauing—the constraint has reached a steady state where some people benefit from the spectrum acknowledgment and others bear the cost of boundary-determination. Theater is moderate (0.28 endpoint) because the reading performs the function it claims—acknowledging variation—while simultaneously hiding the boundary-drawing problem it does not solve. The coercion grid captures this: individual-level stakes inflation is high (intersex people must navigate medical and legal gatekeeping), organizational suppression is lower (healthcare and legal organizations often implement the reading with good faith), class-level resistance is high (intersex communities actively contest the reading's implementation, pushing for full decriminalization and non-intervention), and structural-level suppression is moderate (no systematic state coercion doctrine, but institutional practices accumulate coercive effect). The claim is tangled_rope because the reading coordinates genuine recognition of biological variation (coordination function) while extracting boundary-compliance costs from those who fall outside or between categories (asymmetric extraction).
 *
 * PERSPECTIVAL GAP:
 *   From the seat of intersex-rights advocates and supportive legal organizations, this reading represents a major victory—biological science now backs legal recognition, and pathologization narratives are weakening. From the seat of intersex people with ambiguous biology, the reading is a mixed good: they gain some recognition but remain subject to medical and administrative determination. From sports organizations' seats, the reading creates an enforcement problem and they respond by adding medical-testing apparatus, which shifts suppression from legal exclusion to medical gatekeeping. The engine should compute different types from different seats: an advocacy organization's seat might compute rope (genuine coordination, low extraction from them), while an athlete with DSD computes tangled_rope (some recognition, coercive medical requirements), and a policymaker's seat computes the constraint as coordinating institutional-sex-categorization practice (moderate extraction for the sake of workable administration). The authored metrics describe the population-weighted average; per-seat types will diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for intersex_people_female_typical_biology is near-beneficiary (d ≈ 0.15–0.25) because they gain legal and social recognition without ongoing coercive intervention; their exit options are constrained (they cannot leave their bodies or identity), but the constraint benefits them relative to binary exclusion. Directionality for intersex_people_ambiguous_biology is near-target (d ≈ 0.65–0.75) because the reading provides nominal recognition but maintains boundary-gatekeeping, and their identity is locked by the requirement to undergo medical assessment to determine category eligibility. Elite_female_athletes_with_dsd face directional drift: the reading nominally includes them (d ≈ 0.35 in legal recognition), but sports enforcement adds coercive medical intervention (d ≈ 0.65–0.70 in sports domain). Feminist_legal_frameworks sit at moderate-to-beneficiary (d ≈ 0.25) because the reading advances their theoretical commitments; they can exit if defeated (move to other policy domains or to the identity reading), but the constraint supports their institutional position. Sports_regulatory_bodies sit at low-beneficiary (d ≈ 0.15) relative to the constraint itself (they extract some administrative clarity), but they face pressure from other constraints (gender-identity reading, antidiscrimination law) that push them to soften the boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The intersex-accommodation reading is not mandatrophic in its intent—it was designed to solve an active problem (non-consensual surgery, legal exclusion, medical pathologization). However, mandatrophy risk is present: if the founding problem (surgical coercion, pathologization) is substantially solved in a jurisdiction while the category-enforcement apparatus persists, the reading could become theater—a nominal acknowledgment of variation layered over continued boundary gatekeeping. In sports, this is already visible: the reading provides nominal legal recognition while performance criteria and hormone suppression requirements enforce the boundary more finely. The measurement trajectory (theater_ratio rising from 0.12 to 0.28, resistance remaining high) suggests emerging mandatrophy—the reading is still functional but increasingly theatrical. The constraint avoids classification as piton because organized actors (intersex advocacy, feminist legal frameworks) continue to actively defend and advance it; it has not atrophied into pure inertia. But the theater ratio warrants monitoring: if suppression continues to rise while extractiveness plateaus, the constraint is becoming a legitimacy cover for boundary enforcement rather than a genuine spectrum acknowledgment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_problem_irreducibility,
    'Is the boundary-determination problem (some intersex people remain excluded or over-determined) an artifact of imperfect implementation, or is it structurally inherent to any spectrum-based category that still must draw a line somewhere?',
    'Comparative analysis of jurisdictions that adopt spectrum-based definitions: do all of them eventually layer administrative or medical gatekeeping, or do some achieve genuine non-gatekeeping spectrum categories? If all do, the problem is structural.',
    'If structural, the reading cannot avoid extracting compliance costs from boundary cases; the classification would be tangled_rope regardless of implementation effort. If accidental, better implementation (medical non-intervention, self-identification options) could shift the constraint toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_problem_irreducibility, conceptual, 'Whether acknowledging spectrum biology necessarily requires boundary-gatekeeping that reintroduces coercion.').

omega_variable(
    population_scale_variation_in_extractiveness,
    'Does extractiveness vary dramatically by domain (low in most legal domains, high in elite sports) because the sports domain has genuinely higher stakes, or because sports is the only domain where the constraint is actively enforced against high-power actors?',
    'Implementation analysis across domains: compare enforcement intensity, medical testing, documentation barriers, and institutional attention in legal sex-marker change, healthcare access, military service, and sports. Measure enforcement intensity as proxy for actual extractiveness experienced.',
    'If extractiveness truly varies by domain, the constraint may not be a single constraint but multiple domain-specific constraints with the same nominal reading. If variation is only in enforcement intensity, the constraint''s core extractiveness is higher than the low population-weighted average suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(population_scale_variation_in_extractiveness, empirical, 'Domain-specific variation in constraint extractiveness and enforcement.').

omega_variable(
    medical_legitimacy_vs_coercion,
    'Are the medical-testing and testosterone-suppression requirements in sports (e.g., World Athletics DSD regulations) coercive enforcement of the intersex-accommodation reading, or are they a separate constraint (sports-fairness-and-verification constraint) layered onto this reading?',
    'Regulatory-history analysis: do the medical requirements exist as part of the sex-category definition itself, or are they added as enforcement mechanisms by sports organizations? If the latter, they are a separate constraint.',
    'If separate, extractiveness of this reading is lower than measured (0.38) because the high-stakes medical coercion is not intrinsic to the reading but to sports governance. If integral, the reading is more extractive than acknowledged because it enables the medical apparatus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(medical_legitimacy_vs_coercion, empirical, 'Whether medical testing is internal to the reading or a separate enforcement mechanism.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression measured (0.52) primarily structural (institutional gatekeeping, medical requirements) or internalized (intersex people''s internalized shame, doubts about legitimacy of claiming the category)?',
    'Post-decriminalization trajectory: do intersex people report decreased suppression-burden after access to legal recognition without medical gatekeeping, or do internalized barriers persist? Qualitative testimony from intersex people navigating the category in low-gatekeeping jurisdictions.',
    'If structural dominates, reducing institutional gatekeeping would substantially lower effective suppression. If internalized, the constraint carries suppression with people even after institutional barriers are removed, making the classification more extractive than the scalar suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Decomposition of suppression into structural and internalized components.').

omega_variable(
    vitiated_consent_in_sports_context,
    'When elite female athletes with DSD undergo hormone suppression to compete in women''s categories, is this choice meaningfully voluntary (within a constrained choice set) or does the structure constitute vitiated consent (the choice set is so constrained—compete under suppression or abandon elite sport entirely—that apparent choice masks coercion)?',
    'Athlete testimony from before, during, and after suppression; comparison of reported autonomy/choice experience with other coercive-context models (conscription, debt traps); legal doctrines of vitiated consent.',
    'If consent is vitiated, suppression should be measured higher in sports domain (0.70+) despite nominal category-inclusion. If consent is constrained but not vitiated, measured suppression is accurate and reflects the athlete''s actual lived constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vitiated_consent_in_sports_context, preference, 'Ethical status of apparent choice under constrained institutional circumstances.').

omega_variable(
    kernel_reading_committer_identification,
    'This constraint instantiates the intersex-accommodation reading of the woman-category kernel. The sibling readings (sex-biology and gender-identity) embody different committer frameworks. What evidence would show this reading is being implemented in a fundamentally different framework than intended (e.g., as biological essentialism with minor spectrum gesture, rather than as genuine non-binary biology + legitimate variation)?',
    'Discourse analysis of how the reading is justified in policy documents, medical guidelines, and legal briefs. Do framers invoke spectrum biology and variation legitimacy, or do they invoke pragmatic accommodation of medical anomaly? Do they defend the reading''s axioms or downplay them?',
    'If implementation rests on different axioms than authored (e.g., ''medical accommodation'' vs. ''biological legitimacy''), the reading is being corrupted into a different framing, and the classification would shift. This is the core omega for kernel readings: does the constraint embody the committer''s core premise, or has it been absorbed into a neighboring framework?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_identification, conceptual, 'Verification that the intersex-accommodation axioms are actually grounding implementation, not being substituted by neighboring frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__intersex_accommodation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__intersex_accommodation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(woma_tr_t0, observed).
narrative_ontology:measurement(woma_tr_t8, woman_category__intersex_accommodation_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement_basis(woma_tr_t8, observed).
narrative_ontology:measurement(woma_tr_t16, woman_category__intersex_accommodation_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement_basis(woma_tr_t16, observed).
narrative_ontology:measurement(woma_tr_t24, woman_category__intersex_accommodation_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement_basis(woma_tr_t24, observed).
narrative_ontology:measurement(woma_tr_t32, woman_category__intersex_accommodation_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement_basis(woma_tr_t32, observed).
narrative_ontology:measurement(woma_tr_t40, woman_category__intersex_accommodation_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement_basis(woma_tr_t40, observed).
narrative_ontology:measurement(woma_tr_t50, woman_category__intersex_accommodation_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(woma_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__intersex_accommodation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(woma_be_t0, observed).
narrative_ontology:measurement(woma_be_t8, woman_category__intersex_accommodation_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement_basis(woma_be_t8, observed).
narrative_ontology:measurement(woma_be_t16, woman_category__intersex_accommodation_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement_basis(woma_be_t16, observed).
narrative_ontology:measurement(woma_be_t24, woman_category__intersex_accommodation_reading, base_extractiveness, 24, 0.38).
narrative_ontology:measurement_basis(woma_be_t24, observed).
narrative_ontology:measurement(woma_be_t32, woman_category__intersex_accommodation_reading, base_extractiveness, 32, 0.39).
narrative_ontology:measurement_basis(woma_be_t32, observed).
narrative_ontology:measurement(woma_be_t40, woman_category__intersex_accommodation_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(woma_be_t40, observed).
narrative_ontology:measurement(woma_be_t50, woman_category__intersex_accommodation_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement_basis(woma_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__intersex_accommodation_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(woma_su_t0, observed).
narrative_ontology:measurement(woma_su_t8, woman_category__intersex_accommodation_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement_basis(woma_su_t8, observed).
narrative_ontology:measurement(woma_su_t16, woman_category__intersex_accommodation_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement_basis(woma_su_t16, observed).
narrative_ontology:measurement(woma_su_t24, woman_category__intersex_accommodation_reading, suppression_requirement, 24, 0.51).
narrative_ontology:measurement_basis(woma_su_t24, observed).
narrative_ontology:measurement(woma_su_t32, woman_category__intersex_accommodation_reading, suppression_requirement, 32, 0.52).
narrative_ontology:measurement_basis(woma_su_t32, observed).
narrative_ontology:measurement(woma_su_t40, woman_category__intersex_accommodation_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(woma_su_t40, observed).
narrative_ontology:measurement(woma_su_t50, woman_category__intersex_accommodation_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement_basis(woma_su_t50, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(woma_grid_01, woman_category__intersex_accommodation_reading, accessibility_collapse(class), 0, 0.52).
narrative_ontology:measurement(woma_grid_02, woman_category__intersex_accommodation_reading, accessibility_collapse(class), 50, 0.58).
narrative_ontology:measurement(woma_grid_03, woman_category__intersex_accommodation_reading, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(woma_grid_04, woman_category__intersex_accommodation_reading, accessibility_collapse(individual), 50, 0.42).
narrative_ontology:measurement(woma_grid_05, woman_category__intersex_accommodation_reading, accessibility_collapse(organizational), 0, 0.48).
narrative_ontology:measurement(woma_grid_06, woman_category__intersex_accommodation_reading, accessibility_collapse(organizational), 50, 0.54).
narrative_ontology:measurement(woma_grid_07, woman_category__intersex_accommodation_reading, accessibility_collapse(structural), 0, 0.58).
narrative_ontology:measurement(woma_grid_08, woman_category__intersex_accommodation_reading, accessibility_collapse(structural), 50, 0.64).
narrative_ontology:measurement(woma_grid_09, woman_category__intersex_accommodation_reading, resistance(class), 0, 0.78).
narrative_ontology:measurement(woma_grid_10, woman_category__intersex_accommodation_reading, resistance(class), 50, 0.75).
narrative_ontology:measurement(woma_grid_11, woman_category__intersex_accommodation_reading, resistance(individual), 0, 0.68).
narrative_ontology:measurement(woma_grid_12, woman_category__intersex_accommodation_reading, resistance(individual), 50, 0.72).
narrative_ontology:measurement(woma_grid_13, woman_category__intersex_accommodation_reading, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(woma_grid_14, woman_category__intersex_accommodation_reading, resistance(organizational), 50, 0.68).
narrative_ontology:measurement(woma_grid_15, woman_category__intersex_accommodation_reading, resistance(structural), 0, 0.52).
narrative_ontology:measurement(woma_grid_16, woman_category__intersex_accommodation_reading, resistance(structural), 50, 0.58).
narrative_ontology:measurement(woma_grid_17, woman_category__intersex_accommodation_reading, stakes_inflation(class), 0, 0.55).
narrative_ontology:measurement(woma_grid_18, woman_category__intersex_accommodation_reading, stakes_inflation(class), 50, 0.62).
narrative_ontology:measurement(woma_grid_19, woman_category__intersex_accommodation_reading, stakes_inflation(individual), 0, 0.62).
narrative_ontology:measurement(woma_grid_20, woman_category__intersex_accommodation_reading, stakes_inflation(individual), 50, 0.68).
narrative_ontology:measurement(woma_grid_21, woman_category__intersex_accommodation_reading, stakes_inflation(organizational), 0, 0.48).
narrative_ontology:measurement(woma_grid_22, woman_category__intersex_accommodation_reading, stakes_inflation(organizational), 50, 0.52).
narrative_ontology:measurement(woma_grid_23, woman_category__intersex_accommodation_reading, stakes_inflation(structural), 0, 0.38).
narrative_ontology:measurement(woma_grid_24, woman_category__intersex_accommodation_reading, stakes_inflation(structural), 50, 0.42).
narrative_ontology:measurement(woma_grid_25, woman_category__intersex_accommodation_reading, suppression(class), 0, 0.52).
narrative_ontology:measurement(woma_grid_26, woman_category__intersex_accommodation_reading, suppression(class), 50, 0.58).
narrative_ontology:measurement(woma_grid_27, woman_category__intersex_accommodation_reading, suppression(individual), 0, 0.45).
narrative_ontology:measurement(woma_grid_28, woman_category__intersex_accommodation_reading, suppression(individual), 50, 0.52).
narrative_ontology:measurement(woma_grid_29, woman_category__intersex_accommodation_reading, suppression(organizational), 0, 0.38).
narrative_ontology:measurement(woma_grid_30, woman_category__intersex_accommodation_reading, suppression(organizational), 50, 0.42).
narrative_ontology:measurement(woma_grid_31, woman_category__intersex_accommodation_reading, suppression(structural), 0, 0.48).
narrative_ontology:measurement(woma_grid_32, woman_category__intersex_accommodation_reading, suppression(structural), 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__intersex_accommodation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_category__intersex_accommodation_reading, 0.12).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, elite_sports_sex_categorization).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, medical_self_determination_intersex).

% DUAL FORMULATION NOTE:
% The 'woman' category is a contested kernel with three readings: sex-biology, gender-identity, and intersex-accommodation. Each reading instantiates a structurally distinct constraint with different ε values, different beneficiary/victim sets, and different classification types. The three stories are linked by network.affects_constraints as a constraint family. The intersex-accommodation reading's ε is low-to-moderate (0.38) in most domains but rises sharply in elite sports (where it interfaces with performance-boundary enforcement). The sex-biology reading's ε is low in jurisdictions where it is consensus and high where it is contested. The gender-identity reading's ε depends on whether it is implemented as pure identity or layered with biological or medical requirements. Each reading should be generated as a separate story with its own stakeholders, six-questions answers, and temporal measurements, linked through this network-affects-constraints array.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_category__intersex_accommodation_reading, organized, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
