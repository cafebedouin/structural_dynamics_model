% ============================================================================
% CONSTRAINT STORY: woman_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__sex_biology_reading, []).

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
 *   constraint_id: woman_category__sex_biology_reading
 *   human_readable: Woman Category — Sex/Biology Reading
 *   domain: political_philosophy/law/bioethics
 *
 * SUMMARY:
 *   The 'woman' category is contested across three readings: sex-biology
 *   (chromosomal/anatomical/reproductive), gender-identity (internal identity
 *   regardless of assigned sex), and intersex-accommodation (acknowledging
 *   spectrum variation). This constraint instantiates the SEX-BIOLOGY
 *   READING, which defines woman as adult human female with XX chromosomes
 *   and female reproductive anatomy (typical case). The reading generates a
 *   tangled-rope structure: it coordinates genuine institutional interests
 *   (sex-segregated sports, violence-against-women data) while simultaneously
 *   excluding transgender women, leaving intersex people in categorical
 *   limbo, and suppressing nonconforming people with female biology. The
 *   extractiveness has risen from 0.35 (early 2000s, when the reading faced
 *   minimal organized challenge) to 0.58 (2015–present, as the reading
 *   becomes openly contested and enforced against organized opposition).
 *   Suppression peaked around 2015 (0.68) as institutions tightened
 *   categorical enforcement in response to trans visibility, and has since
 *   moderated slightly (0.62) as some accommodations emerged without full
 *   reading adoption. Theater ratio tracks the same arc: the reading was
 *   presented as transparent biological fact (low theater ~0.40) until the
 *   contest intensified, forcing explicit defense (theater rising to 0.58 by
 *   2015), and has stabilized (0.55) as debate becomes institutionalized.
 *
 * KEY AGENTS:
 *   - Transgender women: Primary victims (powerless/trapped) — structurally excluded from the 'woman' category, losing access to sex-segregated protections, legal recognition, community resources. Cannot exit this classification through their own agency within the reading's framework.
 *   - Intersex people: Secondary victims (powerless/identity_locked) — structurally ambiguous under the biological definition. The reading provides no coherent mechanism for their categorization; they are left in unresolved categorical limbo. Identity-locked: cannot choose a classification, yet the reading offers no valid category for their actual biology.
 *   - Cisgender women with female biology: Primary beneficiaries (moderate/constrained) — experience genuine coordination benefits (sex-segregated sports, violence-against-women data collection) alongside the extraction required to enforce and maintain the category. Organized through feminist advocacy but facing organized opposition.
 *   - Sports governing bodies: Institutional beneficiary (institutional/arbitrage) — use the biological definition to maintain competitive fairness frameworks. Experience the constraint as pure coordination with exit options available but undesirable.
 *   - Violence-against-women data systems: Institutional beneficiary (institutional/arbitrage) — use sex-disaggregated data to track violence patterns. Benefit from the biological definition's clarity, though this benefit depends on the contested premise that violence patterns correlate with biology rather than social structure.
 *   - Medical/scientific classification systems: Institutional actor (institutional/arbitrage) — maintain the XX/XY binary through professional convention and institutional inertia. Increasingly challenged by evidence of intersex variation; classification is degrading (piton perspective).
 *   - The analytical observer: Cross-reading perspective (analytical/analytical) — sees the constraint as structurally mixed: real coordination + real extraction, neither reducible to the other. Resolution requires cross-reading analysis.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__sex_biology_reading, 0.58).
domain_priors:suppression_score(woman_category__sex_biology_reading, 0.62).
domain_priors:theater_ratio(woman_category__sex_biology_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__sex_biology_reading, "Woman Category — Sex/Biology Reading").
narrative_ontology:topic_domain(woman_category__sex_biology_reading, "political_philosophy/law/bioethics").

domain_priors:requires_active_enforcement(woman_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__sex_biology_reading, 'ac261c08-3e94-4bac-aecb-15b8877d97ff').
narrative_ontology:cs_kernel_codification('ac261c08-3e94-4bac-aecb-15b8877d97ff', distributed).
narrative_ontology:cs_authority_grounding('ac261c08-3e94-4bac-aecb-15b8877d97ff', distributed).
narrative_ontology:cs_reading_relation('ac261c08-3e94-4bac-aecb-15b8877d97ff', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_reading_relation('ac261c08-3e94-4bac-aecb-15b8877d97ff', woman_category__intersex_accommodation_reading, influences).
narrative_ontology:cs_axiom('ac261c08-3e94-4bac-aecb-15b8877d97ff', foundational, female_biology_categorical_coherence).
narrative_ontology:cs_axiom_status(female_biology_categorical_coherence, holdable).
narrative_ontology:cs_axiom_grounding('ac261c08-3e94-4bac-aecb-15b8877d97ff', female_biology_categorical_coherence, empirically_contingent).
narrative_ontology:cs_axiom('ac261c08-3e94-4bac-aecb-15b8877d97ff', secondary, sex_based_harm_pattern_correlation).
narrative_ontology:cs_axiom_status(sex_based_harm_pattern_correlation, holdable).
narrative_ontology:cs_axiom_grounding('ac261c08-3e94-4bac-aecb-15b8877d97ff', sex_based_harm_pattern_correlation, empirically_contingent).
narrative_ontology:cs_reference_frame('ac261c08-3e94-4bac-aecb-15b8877d97ff', biomedical_sex_dimorphism).
narrative_ontology:cs_drift_state('ac261c08-3e94-4bac-aecb-15b8877d97ff', contemporary_intersex_visibility, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ac261c08-3e94-4bac-aecb-15b8877d97ff', '').
narrative_ontology:cs_kernel_id(woman_category__sex_biology_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, cisgender_women_with_female_biology).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, sex_segregated_protection_advocates).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, transgender_women).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, intersex_people).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, nonconforming_people_with_female_biology).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRANSGENDER WOMEN (SNARE) — Structurally excluded from the category 'woman' under the biological definition, losing access to sex-segregated protections, healthcare, legal recognition, and community resources. Cannot exit this classification through biology. Experience this as a pure extraction mechanism: they bear the cost of categorical exclusion with no organizational power and no path to reclassification within this reading's framework. Maximum experienced extraction.
constraint_indexing:constraint_classification(woman_category__sex_biology_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERSEX PEOPLE (SNARE) — Structurally ambiguous under the biological definition. Those with XX chromosomes but atypical anatomy, or atypical chromosome configurations, face unresolved categorical status. The definition provides no mechanism for resolution. Identity-locked: they cannot simply choose a classification, yet the reading offers no coherent category for their actual biology. High suppression from the ambiguity itself — uncertainty about legal status, healthcare access, documentation.
constraint_indexing:constraint_classification(woman_category__sex_biology_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: CISGENDER WOMEN WITH FEMALE BIOLOGY (TANGLED ROPE) — Primary beneficiaries who experience genuine coordination benefits (sex-segregated sports, violence-against-women data collection, reproductive health policy) alongside extraction. The constraint coordinates their interests relative to other groups, but does so through categorical exclusion that carries suppressive force. They benefit from the category's stability and from the data collection that acknowledges their biological differences, but also pay costs: being reduced to biology, loss of solidarity with transgender women and intersex people, enforcement overhead (documentation, verification), and ideological contestation. Moderate agent power — organized through feminist advocacy but facing organized opposition.
constraint_indexing:constraint_classification(woman_category__sex_biology_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SPORTS GOVERNING BODIES (ROPE) — Institutional actors that benefit from clear categorical definitions of 'woman' in competitive sports. The biological definition provides an administratively clean distinction (chromosome testing, medical verification) that these bodies can use to maintain competitive fairness frameworks. They experience the constraint as pure coordination: the definition solves their categorical problem. Exit options exist (use other criteria, allow self-identification) but involve coordination costs they wish to avoid. Zero victims from their perspective — a genuine coordination mechanism.
constraint_indexing:constraint_classification(woman_category__sex_biology_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: VIOLENCE-AGAINST-WOMEN DATA SYSTEMS (ROPE) — Government and NGO data collection systems (crime statistics, healthcare surveillance, epidemiology) benefit from the biological definition's clarity. Sex-disaggregated data on violence enables pattern recognition: women are the primary targets of sexual and domestic violence at rates correlated with female biology/reproductive capacity. The definition allows these systems to track material harms. They experience the constraint as coordination: stable categorical data collection. However, this perspective's beneficiary status depends on viewing the violence pattern as intrinsic to female biology rather than to social structure — a contested premise embedded in the axioms.
constraint_indexing:constraint_classification(woman_category__sex_biology_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: MEDICAL/SCIENTIFIC SYSTEMS (PITON) — Historically authoritative classification systems (sex/gender distinction in medical literature, binomial sex categorization in biology) are increasingly challenged by intersex biology, endocrine variation, and social research. These systems maintain the biological definition through institutional inertia and professional convention, not because it cleanly captures biological reality (intersex conditions exist across 1–2% of human populations). The theater ratio is high: much of the 'scientific' support for the clean XX/XY binary is performative, sustained through professional gatekeeping rather than empirical clarity. The systems have degraded function — they cannot account for actual biological variation — but persist due to established authority and professional investment.
constraint_indexing:constraint_classification(woman_category__sex_biology_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational analytical view, the sex-biology reading instantiates a genuine coordination mechanism (organizing around material biological differences, enabling sex-disaggregated data collection on violence) alongside real extraction (exclusion of non-conforming people, suppression of intersex complexity, enforcement of a category that mismatches actual biology). This perspective sees the constraint as structurally mixed: coordination + exclusion. The reading is internally coherent but comes at the cost of excluding or misplacing real human beings. The contradiction is not resolvable within the reading itself — it requires cross-reading analysis.
constraint_indexing:constraint_classification(woman_category__sex_biology_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__sex_biology_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(woman_category__sex_biology_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(woman_category__sex_biology_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(woman_category__sex_biology_reading, TR),
    TR >= 0.70.

:- end_tests(woman_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The sex-biology reading coordinates genuine institutional interests and benefits primary beneficiaries (cisgender women), but accomplishes this through categorical exclusion that extracts from transgender women (no exit options, total exclusion from category) and creates ambiguity for intersex people. The extractiveness is not as high as a pure snare (which would have no coordination function) because the reading genuinely solves institutional coordination problems. But it is not low because the exclusion is structural, not incidental. The 0.35→0.58 trajectory reflects increasing enforcement and contestation: as the reading faces organized challenge, institutions have tightened enforcement and invested in maintaining the boundary, raising the experienced extraction. Suppression (0.62): Moderate-high. The reading suppresses alternatives (transgender women's identity claims, intersex variation) through categorical closure. The peak at 0.68 (2015) reflects maximum enforcement pressure; the moderation to 0.62 reflects some institutional accommodation (recognition of transgender women in some domains, intersex pathways in some legal systems) without full reading adoption. Theater ratio (0.55): Moderate. The reading's coordination benefits are substantive (violence-against-women data does track material differences; athletic advantage does correlate with female biology), so theater is not dominant. However, the claim that XX/XY provides a clean biological boundary is performative — intersex variation is suppressed from the classification, making the 'biological' boundary partly theatrical (maintained through professional gatekeeping rather than empirical clarity). Claimed type (tangled_rope): The reading coordinates (sex-segregated protections, data collection) and extracts (categorical exclusion, suppression of alternatives) simultaneously. The tangled-rope classification correctly captures the hybrid structure.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiaries and victims is maximal. Cisgender women see a coordination mechanism that solves real problems (violence patterns, athletic fairness). Transgender women see pure exclusion with no exit path. Intersex people see categorical ambiguity. Sports bodies see coordination; violence-data systems see coordination; medical systems see a degraded but institutionalized classification. The analytical observer sees that the reading is neither pure coordination (it excludes and suppresses) nor pure extraction (it genuinely coordinates institutional interests) — it is structurally mixed. The gap reflects different structural positions: agents who fit the category see it as solving problems; agents who don't fit see it as a mechanism of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) is derived from their structural relationship to the reading. Transgender women and intersex people have no exit options (trapped or identity_locked) and bear costs (victims) — d approaches 1.0 (full targets). Cisgender women have moderate exit options (constrained — they could in principle adopt the identity reading, but face significant costs) and are beneficiaries — d is moderate (around 0.55). Sports bodies and data systems are institutional beneficiaries with arbitrage exit options (they could adopt alternative criteria) — d is low (around 0.20). The analytical observer applies the canonical analytical d (0.72). The sigmoid f(d) converts these to experienced extractiveness chi, which varies substantially across perspectives: victims experience high chi (extraction feels severe); beneficiaries experience low/negative chi (coordination feels natural); organized institutional actors experience moderate chi (they have agency). The directionality derivation explains why perspectives classify differently: the same structural mechanism (the biological definition) produces snare (powerless), tangled rope (moderate/constrained), rope (institutional), and tangled rope (analytical) from different vantage points.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by the reading relations and axioms. This reading (sex-biology) does not logically FORECLOSE the identity-reading (they cannot coexist in a single authoritative framework, but can coexist as different institutions apply different criteria in different domains). It INFLUENCES the intersex-accommodation reading (recognizing biological variation creates pressure to expand the category). The foundational axiom is 'female_biology_categorical_coherence' — the claim that biological sex forms a coherent category sufficient for legal and policy purposes. This axiom is HOLDABLE (many actors hold it) but depends on the contested premise that biological variation clusters around a center (XX/XY) rather than forming a spectrum. If this premise is falsified (if intersex variation is shown to be significant and spectrum-distributed), the axiom does not foreclose — it remains holdable as a policy choice — but loses its grounding in 'biological clarity.' The reading remains coherent as a choice, but not as a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_advantage_measurability,
    'In sports, is the performance advantage of testosterone-typical physiology uniformly large enough to justify categorical exclusion of all transgender women, or does individual variation create a spectrum where some transgender women fall within the competitive range of cisgender women?',
    'Longitudinal athletic data: compare post-transition testosterone-suppressed transgender women''s performance trajectories to cisgender women''s performance distributions across sports; assess whether advantage persists uniformly after transition duration thresholds',
    'If advantage is uniform across cases: categorical exclusion is justified and the biological definition''s extractiveness in sports is moderate (pure coordination). If advantage is variable/individual: the definition oversimplifies and the extractiveness in sports rises sharply (extraction beyond pure coordination) — a snare classification becomes more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_advantage_measurability, empirical, 'Whether testosterone-physiology advantage in sports is uniform across transgender women or individually variable').

omega_variable(
    intersex_category_resolution,
    'Does the sex-biology reading include a coherent mechanism for categorizing intersex people, or does it leave them in unresolved categorical limbo?',
    'Analysis of actual policy implementations: how do institutional actors (sports, healthcare, legal systems) currently categorize intersex people under sex-biology frameworks? Do they use a primary criterion (chromosomes, gonads, external anatomy, hormone profiles) or leave categorization case-by-case (indicating the reading lacks closure)?',
    'If mechanism exists: the reading''s extractiveness from the intersex perspective drops and snare classification may not apply. If mechanism is absent: the reading is structurally incomplete and creates suppressive ambiguity for intersex people — supporting snare/high-suppression classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intersex_category_resolution, empirical, 'Whether the sex-biology reading provides a coherent categorization mechanism for intersex people').

omega_variable(
    biological_dimorphism_assumption,
    'Is the assumption of clear XX/XY dimorphism tenable given documented chromosomal and physiological variation (XY/XX mosaics, XX male syndrome, androgen insensitivity syndrome, etc.), or is the reading''s biological foundation already compromised by human variation?',
    'Population genetics and medical literature: quantify the prevalence and distribution of atypical chromosome/endocrine configurations; assess whether they form a spectrum or distinct categories; determine whether existing institutional categorizations already accommodate or deny variation',
    'If variation is rare, clear-cut, and institutionally resolvable: the reading''s biological foundation is sound. If variation is significant, distributed across a spectrum, and institutionally unresolved: the biological reading naturalizes a false simplicity — supporting false-summit recognition and reclassification toward intersex_accommodation_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_dimorphism_assumption, empirical, 'Whether the assumption of XX/XY dimorphism holds given documented human biological variation').

omega_variable(
    reading_relationship_to_gender_identity,
    'Does the sex-biology reading logically foreclose the gender-identity reading (one reading''s core premise directly contradicts the other in any single framework), or can both coexist as different institutional framings applied in different domains?',
    'Logical analysis: test whether a single institutional authority (e.g., a legal system, medical system, sports body) can simultaneously maintain both criteria for different purposes. If same authority uses biology for one domain and identity for another, coexistence is empirically true. If the readings are presented as mutually exclusive, analyze whether this mutual exclusivity is logical or merely political.',
    'If readings logically foreclose each other: declare reading_relations as ''forecloses'' — only one reading can be correct within any unified framework. If readings can coexist across different institutional domains or policies: declare as ''coexists_with'' — both are live options held by different parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relationship_to_gender_identity, conceptual, 'Whether sex-biology reading logically forecloses gender-identity reading or can coexist with it').

omega_variable(
    coordination_function_vs_extraction_cover,
    'Is the sex-biology reading''s claimed coordination function (organizing around material biological differences, enabling sex-disaggregated violence data) genuine, or is it a cover story for extraction mechanisms (exclusion, enforcement of simplified categories, suppression of intersex/nonconforming people)?',
    'Structural analysis: compare extractiveness with and without the coordination benefits. Measure whether removing the category would reduce the stated harms (violence against women, unfair athletic competition) or whether it would merely transfer them (violence would still occur, just without sex-disaggregated tracking; athletic advantage would still exist, just unmanaged). If benefits genuinely depend on the category, coordination is real. If benefits are incidental to the category, extraction is primary.',
    'If coordination is primary: the tangled_rope classification is correct and the reading is defensible. If extraction is primary: the reading is a snare with ideological cover — extractiveness increases substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_vs_extraction_cover, conceptual, 'Whether the sex-biology reading''s coordination function is genuine or covers extraction').

omega_variable(
    reading_vs_kernel_identity,
    'Is this constraint a READING of the contested ''woman'' kernel (one instantiation among multiple live alternatives), or is it a CLAIM about what the woman category truly IS (naturalizing one reading as the correct one)?',
    'Meta-analytical: check whether the constraint''s narrative, axioms, and commentary present the sex-biology reading as one defensible option among others, or as the true/natural/scientific definition. If presented as one reading among others, it instantiates the kernel logic. If presented as the correct answer, it commits a scope error — the constraint should then be re-authored to acknowledge sibling readings as live alternatives rather than refutable competitors.',
    'If presented as one reading: kernel structure applies; committer frame is correct; engine uses reading_relations and axioms to analyze. If presented as the correct answer: the constraint misrepresents the nature of the contest; authorial revision required to acknowledge that all three readings (sex-biology, gender-identity, intersex-accommodation) remain live positions in contemporary discourse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_kernel_identity, conceptual, 'Whether this constraint represents a reading of the woman kernel or claims to be the true definition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__sex_biology_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woman_biology_theater_t0_2000s, woman_category__sex_biology_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(woman_biology_theater_t1_2015, woman_category__sex_biology_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(woman_biology_theater_t2_2025, woman_category__sex_biology_reading, theater_ratio, 25, 0.55).

% Extraction over time
narrative_ontology:measurement(woman_biology_extractiveness_t0_2000s, woman_category__sex_biology_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(woman_biology_extractiveness_t1_2015, woman_category__sex_biology_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(woman_biology_extractiveness_t2_2025, woman_category__sex_biology_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(woman_biology_suppression_t0_2000s, woman_category__sex_biology_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(woman_biology_suppression_t1_2015, woman_category__sex_biology_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(woman_biology_suppression_t2_2025, woman_category__sex_biology_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__sex_biology_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__intersex_accommodation_reading).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, sex_segregated_sports_eligibility).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, violence_against_women_data_protocols).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, reproductive_healthcare_policy).

% DUAL FORMULATION NOTE:
% The 'woman' category decomposes into three structurally distinct constraints, each with its own ε value and victim/beneficiary structure. The sex-biology reading (this file) has ε=0.58; the gender-identity reading has a different ε (likely lower, around 0.40–0.45, with different victims); the intersex-accommodation reading has yet another ε (likely intermediate). These are not alternative measurements of the same constraint — they are different kernels producing different constraints. They are linked via network.affects_constraints to show their mutual structural influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
