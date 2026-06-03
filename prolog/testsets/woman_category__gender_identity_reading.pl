% ============================================================================
% CONSTRAINT STORY: woman_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category_gender_identity_reading, []).

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
 *   constraint_id: woman_category__gender_identity_reading
 *   human_readable: Woman Category Definition via Gender Identity (Reading: Identity-First)
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   The gender-identity reading of the woman category determines membership
 *   by internal gender identity rather than biological sex characteristics.
 *   This reading instantiates one axis of a contested kernel that has
 *   fragmented contemporary law and policy into incompatible institutional
 *   regimes. The reading extends the category of 'woman' to include all
 *   people who identify as women, including transgender women whose sex was
 *   assigned as male at birth. This generates structural conflict at multiple
 *   institutional boundaries: sex-segregated spaces (shelters, prisons,
 *   bathrooms, intimate medical care) where the reading's inclusion logic
 *   collides with exclusion-based protections; competitive sports where
 *   category membership determines fairness conditions; and medical systems
 *   where sex-based screening protocols conflict with identity-based
 *   categorization. The constraint exhibits tangled rope structure because it
 *   simultaneously solves a coordination problem (identity document
 *   alignment, legal personhood recognition, institutional efficiency in
 *   administrative systems) and creates asymmetric extraction (access rights
 *   for transgender women imposed over exclusion-based protections for sexual
 *   assault survivors, fairness protections for female athletes). Theater has
 *   accumulated over the interval as institutions adopted self-identification
 *   policies without addressing the underlying conflict between identity
 *   recognition and sex-segregated space protections — suppression increased
 *   as dissent from the reading became institutionally costly to express.
 *
 * KEY AGENTS:
 *   - Transgender Women and Gender-Nonconforming Agents: Primary beneficiaries (organized/mobile) — benefit from legal recognition, document alignment, institutional inclusion, avoiding coercive denial of identity
 *   - Sex-Segregated Space Users: Primary victims (powerless/trapped) — sexual assault survivors in shelters, prisoners in carceral systems, medical patients requiring intimate care; depend on reliably single-sex environments for safety and dignity; cannot exit without losing protection
 *   - Female Athletes: Primary victims (powerless/trapped) — experience category boundary shifts that undermine fairness protections; cannot exit competitive sport without losing opportunity; trapped by institutional policy changes
 *   - Biological Sex Category Maintainers: Secondary actors (moderate/constrained) — medical professionals, researchers, sports scientists working with sex-based concepts; face reputational cost and institutional pressure; also benefit from clarity in identity documents
 *   - Sports Governing Bodies: Institutional actor (institutional/constrained) — caught between inclusion and fairness imperatives; enforce performative testosterone testing; face lawsuits and reputational risk regardless of choice
 *   - Healthcare & Legal Documentation Systems: Institutional actor (institutional/constrained) — experience moderate extraction from conflicting category systems; genuine coordination function in administrative simplification; high theater in medical application
 *   - Transgender Rights Advocacy (Organized): Beneficiary coalition (organized/mobile) — shapes institutional interpretation; frames the reading as simple inclusion; defines dissent as bigotry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__gender_identity_reading, 0.52).
domain_priors:suppression_score(woman_category__gender_identity_reading, 0.48).
domain_priors:theater_ratio(woman_category__gender_identity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__gender_identity_reading, "Woman Category Definition via Gender Identity (Reading: Identity-First)").
narrative_ontology:topic_domain(woman_category__gender_identity_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__gender_identity_reading, '3bb4edf4-a282-4323-b8d2-1ffe50eb6750').
narrative_ontology:cs_kernel_codification('3bb4edf4-a282-4323-b8d2-1ffe50eb6750', distributed).
narrative_ontology:cs_authority_grounding('3bb4edf4-a282-4323-b8d2-1ffe50eb6750', distributed).
narrative_ontology:cs_reading_relation('3bb4edf4-a282-4323-b8d2-1ffe50eb6750', woman_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('3bb4edf4-a282-4323-b8d2-1ffe50eb6750', woman_category__intersex_accommodation_reading, influences).
narrative_ontology:cs_axiom('3bb4edf4-a282-4323-b8d2-1ffe50eb6750', foundational, gender_identity_is_legitimate_category_basis).
narrative_ontology:cs_axiom_status(gender_identity_is_legitimate_category_basis, holdable).
narrative_ontology:cs_axiom_grounding('3bb4edf4-a282-4323-b8d2-1ffe50eb6750', gender_identity_is_legitimate_category_basis, deontological).
narrative_ontology:cs_axiom('3bb4edf4-a282-4323-b8d2-1ffe50eb6750', foundational, biological_sex_categorization_is_discriminatory_basis).
narrative_ontology:cs_axiom_status(biological_sex_categorization_is_discriminatory_basis, holdable).
narrative_ontology:cs_axiom_grounding('3bb4edf4-a282-4323-b8d2-1ffe50eb6750', biological_sex_categorization_is_discriminatory_basis, deontological).
narrative_ontology:cs_reference_frame('3bb4edf4-a282-4323-b8d2-1ffe50eb6750', identity_recognition_framework).
narrative_ontology:cs_drift_state('3bb4edf4-a282-4323-b8d2-1ffe50eb6750', contemporary_institutional_adoption, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3bb4edf4-a282-4323-b8d2-1ffe50eb6750', '').
narrative_ontology:cs_kernel_id(woman_category__gender_identity_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, genderqueer_agents).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, sex_segregated_space_users).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, sports_participants_with_sex_based_advantages).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, biological_sex_category_maintainers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SEX-SEGREGATED SPACE USERS (SNARE) — Agents whose safety and dignity depend on reliably single-sex environments (women's shelters, sexual assault survivors' healing spaces, prisons, intimate medical care) experience this reading as extraction: access rights for transgender women are imposed without exit or recourse. Cannot exit the shared space without losing the protection itself. High suppression: dissent is framed as bigotry, alternatives are dismissed as discriminatory. No coordination function — pure imposition of access rights over exclusion rights.
constraint_indexing:constraint_classification(woman_category__gender_identity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SEX-BASED SPORTS COMPETITORS (SNARE) — Female athletes competing in sex-segregated categories experience this reading as extraction: testosterone advantage or skeletal structure advantages (if the transgender woman transitioned post-puberty) are denied or minimized; the category boundary shifts without their consent or agency. Cannot exit competitive sport without losing opportunity itself. High suppression: physiological data is dismissed as bigoted, alternative categorization schemes are rejected as discriminatory. Access rights for some collide with fairness protections for others.
constraint_indexing:constraint_classification(woman_category__gender_identity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: BIOLOGICAL SEX CATEGORY MAINTAINERS (TANGLED ROPE) — Scholars, medical professionals, and institutional actors who work with sex-based categories (reproductive endocrinologists, sex-based medical research, sports governing bodies) experience mixed extraction and coordination. They face costs: reputational damage, loss of research funding, institutional pressure to revise terminology. But they also benefit from the gender-identity reading's institutional adoption — it provides clarity in documentation (identity-document policy), reduces ambiguity in legal person identity, enables institutional efficiency in some domains. Constrained exit: can revise terminology under pressure, but face career consequences. Moderate extraction, genuine coordination function in administrative domains.
constraint_indexing:constraint_classification(woman_category__gender_identity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TRANSGENDER RIGHTS ADVOCACY (ROPE) — Organized advocacy coalitions see this reading as coordination: enabling transgender women to self-identify as women solves the collective action problem of category recognition, reduces bureaucratic friction in identity documents, and prevents the coercive denial of identity. Benefits are genuine: institutional recognition, legal personhood, access to gender-affirming care, documented identity. Minimal experienced extraction because advocacy has power to shape institutional interpretation and exit options are available (organizational resources, media platforms, legal strategy). Sees the constraint as solving a coordination problem at acceptable cost.
constraint_indexing:constraint_classification(woman_category__gender_identity_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: SPORTS GOVERNING BODIES (TANGLED ROPE) — IOC, World Athletics, national sports organizations face extraction and coordination simultaneously. Coordination function: self-identification enables inclusion of transgender athletes, reduces bureaucratic verification requirements, avoids reputational damage from exclusion policies. Extraction: lawsuits, regulatory pressure from multiple directions (inclusion advocates vs fairness advocates), loss of legitimacy with different constituencies depending on policy choice. Constrained exit: cannot simply exclude (legal/reputational risk) or simply include (fairness concerns undermine sport integrity). Active enforcement required: hormone testing, documentation verification, case-by-case adjudication. High theater: policy decisions are performative substitutes for measuring actual athletic advantage (testosterone tests have imperfect correlation with performance; documentation is treated as transparent when it is socially constructed).
constraint_indexing:constraint_classification(woman_category__gender_identity_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: HEALTHCARE & LEGAL DOCUMENTATION SYSTEMS (TANGLED ROPE) — Medical providers, government agencies, and legal systems experience moderate extraction and genuine coordination function. Coordination: self-identification-based category membership reduces documentation burden, aligns legal identity with lived experience, enables faster access to services. Extraction: conflict between sex-based medical screening (e.g., gynecological screening protocols that assume XX chromosome holders have cervixes) and identity-document-based assumption of category membership; forced integration of conflicting category systems (sex for medical purposes, gender identity for legal/social purposes). Constrained exit: systems cannot fully decompose sex and gender identity without rebuilding infrastructure; cannot maintain both parallel systems without friction. Active enforcement: policy documents, training, system redesign. Low theater in documentation (self-identification is administratively simple); high theater in medical context (misalignment between identity category and screening protocols is suppressed rather than addressed).
constraint_indexing:constraint_classification(woman_category__gender_identity_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective grounded in cognitive neuroscience and social psychology, gender identity might be framed as an irreducible feature of human consciousness: people have an intrinsic sense of gender identity that emerges early in development, is stable across contexts, and is not subject to volition. Under this view, the gender-identity reading reflects a natural category boundary in human experience, not a contingent institutional choice. This perspective risks being a FALSE SUMMIT: the 'naturalness' of gender identity is real as lived experience, but the institutional consequence (category boundaries for sports, sex-segregated spaces, medical screening) is contestable and constructed. The engine will detect this as a false summit when beneficiary presence is paired with accessibility_collapse metrics — the accessibility of refusing transgender women category membership is not zero (many frameworks maintain it); the resistance is not minimal (institutional pressure has been required to shift norms).
constraint_indexing:constraint_classification(woman_category__gender_identity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__gender_identity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(woman_category__gender_identity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(woman_category__gender_identity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The reading distributes benefits and costs asymmetrically. Transgender women gain access rights and identity recognition (genuine benefit). Sex-segregated space users lose exclusion protections without consent or exit option (genuine extraction). The trajectory over the interval (0.28→0.52) reflects institutional capture: early adoption in identity documents (low extraction, high coordination) evolved into imposition in sports and spaces (high extraction, low coordination) as institutional actors faced reputational pressure. Suppression (0.48): Moderate-high. Dissent from the reading is increasingly costly to express. Professional consequences for maintaining sex-based categories; institutional pressure on researchers and clinicians; reframing of biological sex as bigoted proxy. Alternative framings (sex-biology reading, intersex accommodation reading) are institutionally suppressed. Suppression has risen over the interval (0.22→0.48) as the reading consolidated institutional power. Theater ratio (0.55): Moderate. Sports policy shows high theater (testosterone testing as legible biological proxy that suppresses complexity). Medical documentation is lower theater (self-identification is simple and administratively functional). Identity documents are low theater (alignment with lived experience is genuine). Average across domains yields 0.55. The trajectory (0.35→0.55) reflects rising theater as policy became performative substitute for resolving underlying conflict.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. Powerless agents (space users, athletes) classify as snare — pure extraction with high suppression and no coordination function. Organized agents (advocacy coalitions) classify as rope — pure coordination with minimal extraction. Institutional actors classify as tangled rope — genuine coordination function (identity documents, administrative efficiency) paired with asymmetric extraction (policy imposed despite competing claims). The beneficiary perspective (transgender rights advocates) sees inclusive coordination. The victim perspectives see coercive imposition. The analytical observer risks a false summit: framing gender identity as a natural category that is simply being recognized, rather than as a social/psychological property whose institutional consequence (category boundary for exclusion and fairness) is contingent. The perspectival gap is not epistemic disagreement about facts (all sides acknowledge that gender identity exists, that biological sex differences exist, that transgender people face real harms from denial of identity). The gap is normative and structural: competing claims on the woman category cannot be simultaneously satisfied. The constraint is not resolvable by better information — it is a genuine collision between valid normative imperatives (recognize identity, protect sex-segregated spaces, maintain sports fairness).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position relative to the extraction flow. Beneficiaries (transgender women, advocacy coalitions) with mobile/arbitrage exit options derive low d (they can exit the claim-making process by accepting non-recognition, but choose to claim it as legitimate). Victims (space users, athletes) with trapped/constrained exit options derive high d (they cannot exit the shared space or sport without losing the institution itself). Institutional actors with constrained exit derive moderate d (caught between incompatible normative demands). The beneficiary directionality is pushed downward by the fact that this is a reading choice, not a structural inevitability — they could adopt the sex-biology reading instead. The victim directionality is pushed upward by trapped exit: space users cannot exit single-sex environments without losing protection; athletes cannot exit sport without losing opportunity. The institutional actors' directionality reflects that they are forced to choose a reading and enforce it on others, incurring costs regardless of choice. No directionality override is necessary — the structural derivation from beneficiary/victim + exit options accurately captures the positions.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing a genuine normative collision rather than a measurement ambiguity. All perspectives are accurate: the reading does solve an inclusion problem (coordination function); it does impose costs on people with competing claims (extraction function); it does suppress alternative framings (suppression function); and it has accumulated theater as it has become institutionalized without resolving the underlying conflict. The mandatrophy is not resolved by picking the 'correct' reading — all three readings are internally coherent and defensible. The constraint is a genuine structural problem: any institutional regime that adopts one reading will extract from agents who hold competing readings. The gender-identity reading is not uniquely problematic in this regard — the sex-biology reading would extract from transgender women equally. The constraint is irresolvable because the woman category has been used simultaneously as (a) a medical/biological category, (b) a social/legal category, (c) an identity category, and (d) a fairness/protection category, and these four uses have different optimal definitions. The DR framework's function here is diagnostic: to clarify which agents experience extraction under this reading, which under alternative readings, and where the true policy choice lies: not which reading is correct, but which distribution of costs is normatively acceptable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_vs_institutional_boundary_ambiguity,
    'Is the boundary between ''gender identity as internal phenomenology'' and ''woman category as institutional membership'' coherent, or do they constitute two different structural categories being conflated?',
    'Structural analysis: distinguish (a) the claim that gender identity is a real psychological property from (b) the claim that institutional category membership should be determined by self-identification of that property. Examine whether rejecting (b) requires rejecting (a) or vice versa.',
    'If distinction is sustainable: the constraint can be decomposed into two stories with different ε (identity recognition, institutional membership). If conflation is necessary: the readings'' dispute is genuinely dichotomous (forecloses relation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_vs_institutional_boundary_ambiguity, conceptual, 'Whether identity phenomenology and institutional membership determination are distinct or conflated').

omega_variable(
    sex_based_harms_vs_exclusionary_harms_empirical_asymmetry,
    'Are harms from sex-based exclusion policies empirically comparable to harms from access-based inclusion in sex-segregated spaces? Which is more severe, more frequent, more structurally entrenched?',
    'Longitudinal data on exclusion-based harm (mental health outcomes for excluded transgender people, documented cases of identity denial causing distress) vs inclusion-based harm (safety incidents in shelters and prisons, documented cases of access causing distress in single-sex spaces). Severity and frequency comparison.',
    'If exclusion-based harms are demonstrably greater: the gender-identity reading''s beneficiary-victim classification is accurate; ε may be lower than 0.52 (more coordination-like). If inclusion-based harms are comparable or greater: the snare classifications for space users and sports competitors are justified; ε may be higher (more extraction-like). If both are substantial and incommensurable: the constraint is genuinely irresolvable (mandatrophy is structural, not eliminable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sex_based_harms_vs_exclusionary_harms_empirical_asymmetry, empirical, 'Comparative severity of sex-based exclusion harms vs access-based inclusion harms').

omega_variable(
    sports_testosterone_proxy_validity,
    'Does testosterone level accurately proxy athletic advantage sufficient to justify category inclusion decisions, or is it a performative biological marker that suppresses the true complexity of athletic advantage (skeletal structure, lung capacity, hemoglobin, training history, muscle fiber type)?',
    'Meta-analysis of correlation between testosterone and athletic performance in specific sports; identification of athletes with low testosterone who retain significant advantages; examination of cases where testosterone-suppressed transgender athletes perform at elite levels.',
    'If testosterone is valid proxy: sports category decisions have moderate justification; snare classification may overstate extraction (theater_ratio should be lower, extractiveness negotiable). If testosterone is performative: snare classification confirmed; the constraint suppresses true athletic complexity in favor of a legible biochemical marker; high theater detected.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sports_testosterone_proxy_validity, empirical, 'Whether testosterone level adequately proxies athletic advantage for category determination').

omega_variable(
    sex_segregated_space_safety_mechanistic_alternative,
    'What proportion of sex-segregated space safety depends on the actual statistical distribution of sex (e.g., reduced rate of male-perpetrated sexual violence in women''s shelters) versus the social/psychological mechanism (trust, vulnerability expression, absence of patriarchal power dynamics) that is mechanistically independent of sexual category?',
    'Comparative analysis of safety incidents and trust outcomes in single-sex vs mixed-sex spaces with equivalent security infrastructure; interviews with users about whether safety feelings are tied to category membership or to institutional protections and trusted peer presence; examination of spaces where mixed-sex cohorts function safely (e.g., gender-neutral supportive housing with strong community norms).',
    'If safety depends primarily on actual statistical distribution: sex-segregated spaces'' category boundary is mechanistically justified; snare classification from this perspective is accurate. If safety depends primarily on social trust and norms: the boundary is culturally contingent; alternative architectures (gender-neutral with strong accountability) could replace exclusion; snare classification may overstate necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sex_segregated_space_safety_mechanistic_alternative, empirical, 'Whether sex-segregated space safety depends on demographic distribution or social mechanisms').

omega_variable(
    reading_foreclosure_test,
    'Does the gender-identity reading''s foundational axiom (gender identity is the legitimate basis for category membership) logically foreclose the sex-biology reading''s core premise (biological sex is the legitimate basis), or do both readings coexist as different legitimate framing choices?',
    'Examine whether one reading requires denying facts the other reading depends on (e.g., does identity-first reading require denying that sex differences in athletic performance exist?). Distinguish denial of fact from rejection of normative consequence (one can accept sex differences exist while rejecting that they justify exclusion).',
    'If foreclosure: the readings are genuinely incompatible within a single framework; one must be chosen. If coexistence: multiple frameworks can hold both readings simultaneously; the constraint is a coordination problem between frameworks, not a definitional truth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether identity-first and sex-biology readings logically foreclose one another').

omega_variable(
    institutional_capture_in_sports_policy,
    'Are sports governing bodies'' adoption of identity-first category determination driven by principled reasoning about fairness and inclusion, or by institutional capture by advocacy coalitions with reputational incentives (avoiding accusation of bigotry, maximizing institutional legitimacy among progressive constituencies)?',
    'Process tracing: examine decision-making records of IOC, World Athletics, etc. for evidence of independent cost-benefit analysis vs adoption of advocacy positions; compare policy choices to scientific evidence on athletic advantage; identify whether contrary evidence prompted policy revision or was ignored.',
    'If driven by principled reasoning: institutional perspective (tangled_rope) is accurate; extraction is genuine coordination cost. If driven by institutional capture: institutional perspective should classify as snare (extraction masked as coordination); chi should be higher; mandatrophy signal should be stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_capture_in_sports_policy, empirical, 'Whether sports policy choices reflect principled reasoning or institutional capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__gender_identity_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wcgir_theater_t0, woman_category__gender_identity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(wcgir_theater_t5, woman_category__gender_identity_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(wcgir_theater_t10, woman_category__gender_identity_reading, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(wcgir_extract_t0, woman_category__gender_identity_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(wcgir_extract_t5, woman_category__gender_identity_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(wcgir_extract_t10, woman_category__gender_identity_reading, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(wcgir_suppress_t0, woman_category__gender_identity_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(wcgir_suppress_t5, woman_category__gender_identity_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(wcgir_suppress_t10, woman_category__gender_identity_reading, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__intersex_accommodation_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, sex_segregated_space_access_justification).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, sports_category_fairness_definition).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, sex_based_medical_screening_conflict).

% DUAL FORMULATION NOTE:
% The woman_category kernel decomposes into three constraint stories, each with distinct ε values and victim/beneficiary structures. This story (gender_identity_reading) has ε=0.52 and frames transgender women and advocacy coalitions as beneficiaries; sex-segregated space users and female athletes as victims. The sex_biology_reading has different beneficiary/victim assignments and will show lower ε in identity-document policy (higher coordination) but higher ε in spaces and sports (different collision dynamics). The intersex_accommodation_reading bridges the biological and identity axes and will show different extraction patterns in medical contexts. All three readings are linked via network.affects_constraints to show kernel-family structure. Downstream constraints (sex-segregated space, sports category, medical screening) are affected by the reading choice: choosing gender-identity reading activates inclusion/access conflict in those domains; choosing sex-biology reading activates exclusion/identity-denial conflict. The network structure enables contamination analysis: drift in this reading toward higher suppression will propagate downstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
