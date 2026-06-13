% ============================================================================
% CONSTRAINT STORY: woman_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: woman_category__sex_biology_reading
 *   human_readable: Sex-Biology-Based Category Definition for 'Woman'
 *   domain: political/legal/social/bioethical
 *
 * SUMMARY:
 *   This constraint instantiates one reading of a contested kernel: the
 *   category 'woman' defined by chromosomal/anatomical/reproductive biology.
 *   The sex-biology reading dominates institutional contexts including sports
 *   governance, reproductive health data collection, and some legal systems.
 *   Its structure is tangled: it solves a genuine coordination problem
 *   (measuring sex-specific health and violence patterns) while extracting
 *   institutional recognition and legal status from transgender women and
 *   some intersex people. The reading is actively defended through legal
 *   codification, medical gatekeeping, and institutional enforcement; it also
 *   encounters active resistance from gender-identity and intersex advocates.
 *   The constraint's persistence depends on suppressing alternative readings,
 *   not on voluntary adoption by all stakeholders.
 *
 * KEY AGENTS:
 *   - cisgender_women — beneficiary (legal category membership, sex-segregated protections, vindication of category's relevance)
 *   - transgender_women — victim (excluded from category, from sex-segregated protections, identity-locked to exclusion)
 *   - intersex_people — victim/ambiguous payer (boundary status, medical medicalization, identity fusion with exclusion)
 *   - sex_essentialist_advocates — agenda_setter (author and defend the reading in law and policy)
 *   - sports_governing_bodies — agenda_setter/beneficiary (enforce sex segregation, gain legitimacy from biological reading)
 *   - public_health_statisticians — agenda_setter (collect sex-disaggregated data, gain measurement authority)
 *   - legal_systems — agenda_setter (codify the reading in statute and case law)
 *   - gender_identity_advocates — excluded (would oppose the reading, suppressed in jurisdictions where it dominates)
 *   - intersex_advocates — excluded (would propose spectrum-based recognition, suppressed by binary gatekeeping)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__sex_biology_reading, 0.68).
domain_priors:suppression_score(woman_category__sex_biology_reading, 0.71).
domain_priors:theater_ratio(woman_category__sex_biology_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__sex_biology_reading, "Sex-Biology-Based Category Definition for 'Woman'").
narrative_ontology:topic_domain(woman_category__sex_biology_reading, "political/legal/social/bioethical").

domain_priors:requires_active_enforcement(woman_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__sex_biology_reading, '65e2b4a6-76fa-46c8-940b-386e8e1a5845').
narrative_ontology:cs_kernel_codification('65e2b4a6-76fa-46c8-940b-386e8e1a5845', fixed_text).
narrative_ontology:cs_authority_grounding('65e2b4a6-76fa-46c8-940b-386e8e1a5845', extraction).
narrative_ontology:cs_interpretation_layer_present('65e2b4a6-76fa-46c8-940b-386e8e1a5845').
narrative_ontology:cs_reading_relation('65e2b4a6-76fa-46c8-940b-386e8e1a5845', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_reading_relation('65e2b4a6-76fa-46c8-940b-386e8e1a5845', woman_category__intersex_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('65e2b4a6-76fa-46c8-940b-386e8e1a5845', foundational, biological_sex_category_determinism).
narrative_ontology:cs_axiom_status(biological_sex_category_determinism, holdable).
narrative_ontology:cs_axiom_grounding('65e2b4a6-76fa-46c8-940b-386e8e1a5845', biological_sex_category_determinism, empirically_contingent).
narrative_ontology:cs_axiom('65e2b4a6-76fa-46c8-940b-386e8e1a5845', secondary, binary_sex_classification_sufficiency).
narrative_ontology:cs_axiom_status(binary_sex_classification_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('65e2b4a6-76fa-46c8-940b-386e8e1a5845', binary_sex_classification_sufficiency, instrumental).
narrative_ontology:cs_reference_frame('65e2b4a6-76fa-46c8-940b-386e8e1a5845', biological_sex_as_legal_foundation).
narrative_ontology:cs_drift_state('65e2b4a6-76fa-46c8-940b-386e8e1a5845', contemporary_identity_recognition_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('65e2b4a6-76fa-46c8-940b-386e8e1a5845', '').
narrative_ontology:cs_kernel_id(woman_category__sex_biology_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, cisgender_women).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, female_biology_advocates).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, transgender_women).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, intersex_people).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, sports_governing_bodies).
narrative_ontology:constraint_vindicates(woman_category__sex_biology_reading, biological_sex_determinism).
narrative_ontology:constraint_vindicates(woman_category__sex_biology_reading, chromosomal_identity_immutability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Women assigned female at birth and identifying as women benefit from sex-segregated protections (reproductive healthcare data, violence-against-women statistics, single-sex spaces in certain contexts) framed around female biology. Their biological category receives explicit legal and policy recognition. They also may bear costs where the definition excludes recognition of intersex conditions or transgender experience as politically significant.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, cisgender_women, beneficiary,
    organized, generational, constrained, global).

% Excluded from sex-segregated protections and spaces justified by biological sex category (women's shelters, single-sex sports, reproductive health data collection coded as 'female'). Cannot exit the constraint by changing bodies or legal status in jurisdictions where this reading dominates; identity fusion with 'woman' category makes the exclusion personally costly. Access to women's spaces and sex-segregated protections hinges on accepting exclusion or litigating on gender-identity grounds outside this constraint's frame.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, transgender_women, payer,
    powerless, biographical, identity_locked, global).

% Occupy an ambiguous position: some intersex people fit within 'XX chromosomes and female anatomy' (typical case) but many have atypical chromosomal or anatomical combinations. The definition's 'typical case' clause excludes them ambiguously — they are neither clearly protected nor clearly excluded. Their bodies are medicalized and their category membership treated as requiring clarification or correction, rather than recognized as a valid biological category in its own right.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, intersex_people, payer,
    powerless, biographical, identity_locked, global).

% Actively defend and legislate the biological sex reading: they write legal definitions, propose sex-segregated policies, conduct public advocacy, and litigate to exclude other readings. They frame biological sex as an objective fact that should determine legal category membership. They collect no direct economic rent but gain institutional authority and ideological vindication.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, sex_essentialist_advocates, agenda_setter,
    organized, generational, mobile, global).

% Would argue for gender-identity-based categorization and would oppose the sex-biology reading as exclusionary. They are excluded from the enforcement machinery that maintains this reading's institutional dominance in certain jurisdictions. Their alternative framing is structurally suppressed in contexts where the sex-biology reading has legal codification.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, gender_identity_advocates, excluded,
    organized, generational, mobile, global).

% Would advocate for spectrum-based category membership recognizing intersex variation as valid biological diversity rather than exception. They are structurally excluded from the category-definition conversation; the constraint treats intersex conditions as medical problems to be clarified within binary categories rather than as evidence that binary categorization itself is inadequate.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, intersex_advocates, excluded,
    moderate, generational, constrained, global).

% Implement and enforce sex-segregated sports categories justified by performance-advantage frameworks tied to biological sex. They collect legitimacy and operational simplicity from the biological reading; it provides a metric (chromosomes, anatomy, hormone levels) that appears objective and verifiable, sidestepping questions about which advantages matter. They exclude transgender athletes on the basis of this reading.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, sports_governing_bodies, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(woman_category__sex_biology_reading, sports_governing_bodies, beneficiary).

% Collect and publish sex-disaggregated data on reproductive health, violence, and disease burden, coded to biological sex categories. The biological reading provides a measurement framework; it enables data collection that vindicates female-pattern disease burdens and violence rates. They are not extracting rent but do gain jurisdictional authority over health surveillance and policy framing.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, public_health_statisticians, agenda_setter,
    institutional, generational, mobile, global).

% Codify the biological sex reading in statute, regulation, and case law; enforce it through legal exclusion and institutional gatekeeping. Different legal systems adopt different readings with different enforcement intensity, but wherever the sex-biology reading dominates, it is embedded in civil law, family law, and anti-discrimination frameworks. Courts apply the reading to determine legal status and access to rights.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, legal_systems, agenda_setter,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__sex_biology_reading, sex_essentialist_advocates).
narrative_ontology:fixing_cost_class(woman_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates individuals into a sex-based category enabling data collection, policy design, and resource allocation around reproductive health, violence patterns, and biological variation. In principle, knowing who has female biology enables targeted healthcare, safety research, and evidence-based policy responding to sex-specific patterns (e.g., pregnancy risk, sexual violence epidemiology). The coordination solves: how do we collect evidence about female-pattern harms and health needs at the population level?
% TRANSFER_FUNCTION: Moves legal category membership, institutional recognition, and access to sex-segregated protections and spaces from people whose biological sex is not female (transgender women, some intersex people) to people with female biology (cisgender women, some intersex people). In sports, it moves competitive slots and prestige from potential transgender athletes to cisgender women and cisgender men in sex-segregated competition.
% ABSENT_VOICES: Transgender women and intersex people are structurally excluded from the agenda-setting seats; they can testify and litigate but do not author the category definition under this reading. Gender-identity advocates and intersex advocates are excluded from the enforcement machinery in jurisdictions where the biological reading dominates. Their alternative readings and objections are suppressed by institutional gatekeeping (legislatures, sports bodies, medical boards, courts), not by transparent contestation.
% DISAPPEARANCE_RATIONALE: If the sex-biology reading and its enforcement disappeared, category membership for 'woman' would be renegotiated, likely toward gender-identity or intersex-accommodating readings. Sex-disaggregated health data collection would need to reformulate its measurement strategy. Sports categories would face recategorization. Legal access to women's spaces and protections would hinge on different criteria. The institutional architecture of reproductive health, anti-violence policy, and sex-segregated competition would reorganize around one of the sibling readings or a hybrid approach.
% FOUNDING_PROBLEM: Women as a biological class face sex-specific harms (reproductive coercion, sexual violence, pregnancy complications, biological variation in disease burden) requiring targeted policy, data collection, and resource allocation. The biological reading was established to create a category stable enough to measure these patterns and design interventions addressing them.
% FOUNDING_PROBLEM_CORROBORATION: Reproductive health epidemiologists and violence-against-women researchers attest that tracking female-pattern harms requires a measurement category; they note the founding problem is live for cisgender women and some intersex people. Transgender women and gender-identity advocates attest that the founding problem can be solved while recognizing gender identity, and that biological-category rigidity has created new harms (exclusion from protection, medical erasure). Sports scientists dispute whether the performance-advantage framing still warrants exclusion given modern hormone-suppression protocols. No single external party attests the founding problem in settled terms; the dispute itself is part of the constraint's current operation.
narrative_ontology:disappearance_verdict(woman_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__sex_biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__sex_biology_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(woman_category__sex_biology_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the reading transfers institutional recognition and legal status from one group to another based on biological classification, and the transfer is enforced despite contestation. The biological reading is not universally accepted; its persistence depends on institutional power, not on agreement from affected parties. Suppression is similarly high (0.71) because enforcement requires actively excluding and delegitimizing alternative readings (gender-identity, intersex accommodation) and because transgender women and intersex people lack institutional voice in category definition. Theater ratio is moderate (0.42) and rising: the health coordination and sports-advantage framing are real but increasingly performative as the reading is used to defend institutional boundaries that precede any genuine coordination analysis. The suppression_requirement trajectory shows intensification over time (0.58 to 0.71) as contestation increased, forcing greater enforcement effort to maintain the reading's dominance.
 *
 * PERSPECTIVAL GAP:
 *   Cisgender women benefit from the reading (organized power, recognition of their category, sex-segregated protections). Transgender women are victims of exclusion and identity-lock (powerless, unable to exit the constraint by changing self-understanding). Intersex people occupy an ambiguous victim position (powerless, identity-locked, boundary status leaves them potentially excluded). Sex-essentialist advocates are agenda-setters who defend the reading (organized power, mobile exit — they can change positions but choose not to). Sports bodies and health institutions are institutional agenda-setters (institutional power, mobile exit — they could adopt alternative readings but have institutional investment in the biological reading). The asymmetry is stark: those who wrote and enforce the reading have much greater power and exit options than those it excludes.
 *
 * DIRECTIONALITY LOGIC:
 *   Cisgender women have directionality near 0.3 (partial beneficiary): they benefit from category recognition and protection, but also carry diffuse costs where the reading excludes intersex variations or where they must defend the reading against contestation. Transgender women have directionality near 0.95 (near-complete target): identity-locked exit, powerless power atom, excluded from the category they identify with, and no alternative category membership available. Intersex people have directionality near 0.8 (high target): ambiguous boundary status leaves them neither clearly protected nor clearly excluded; powerless atom; identity-locked (their body is the basis of the exclusion). Beneficiaries (cisgender_women, sex_essentialist_advocates) have derived low-d values (0.2–0.35), pulling their effective extraction negative (subsidy). Victims (transgender_women, intersex_people) have derived high-d values (0.8–0.95), pulling their effective extraction high. The directionality derivation chain runs from beneficiary/victim declaration → exit options → power atom → d value → χ computation; no overrides needed because the structural data captures the relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The sex-biology reading is classified as tangled_rope: it has a genuine coordination function (measuring sex-specific population-health patterns, designing targeted interventions) AND asymmetric extraction (excluding people from the category based on biological criteria). The classification prevents mislabeling it as pure extraction (snare) by recognizing the health-data coordination; it also prevents mislabeling it as pure coordination (rope) by recognizing the enforcement asymmetry and victim set. The founding problem (sex-specific harms requiring targeted policy) was live when the reading was established. The founding problem status is now contested: cisgender women and health researchers argue the problem is still live; gender-identity advocates argue the same harms can be addressed without biological exclusivity; intersex advocates argue the binary framework misses their health needs. The disappearance verdict is world_rearranges because alternative readings (gender-identity, intersex-accommodation) would reorganize legal access, policy framing, and institutional enforcement. The constraint is not a false natural law (mountain) because the category boundary is drawn by humans and enforced by institutions, not discovered in nature; the 'typical case' language reveals the boundary is a choice, not given.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biology_vs_identity_kernel_contest,
    'Is ''woman'' fundamentally a biological category (the premise of this reading) or fundamentally a social/identity category (the premise of the gender-identity reading), or are both legitimate partial answers to different questions?',
    'This is a kernel question: no empirical discovery settles it because the readings instantiate different commitments about what KIND of category ''woman'' is. Resolution would require explicit normative/political agreement on which purpose the category should serve — legal protection, sports fairness, health surveillance, identity recognition — and whether one purpose should dominate all others.',
    'If identity is deemed the fundamental category, the sex-biology reading is demoted and the gender-identity reading gains institutional dominance; victim and beneficiary sets invert. If biology is deemed fundamental, the sex-biology reading persists but faces ongoing pressure from intersex cases and transgender contestation. The constraint''s type could shift: if the biological reading is reframed as a false natural law rather than a genuine coordination mechanism, it would reclassify from tangled_rope to snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(biology_vs_identity_kernel_contest, conceptual, 'The kernel contest: whether ''woman'' is fundamentally biological or identity-based').

omega_variable(
    intersex_boundary_ambiguity,
    'Where does the sex-biology category boundary actually lie for intersex people — what chromosomal, anatomical, and reproductive combinations count as ''female biology'' and which fall outside?',
    'Medical and genetic research could clarify the empirical spectrum, but the reading does not provide an algorithmic rule for borderline cases (e.g., XX person with androgen insensitivity syndrome, XY person with female anatomy due to hormonal development). The ''typical case'' language defers the boundary question rather than answering it.',
    'If intersex people are included in the category, the victim set shrinks and the coordination function is preserved; if excluded, the category becomes more biologically precise but more explicitly exclusionary. How this boundary is drawn is often decided by sports bodies and medical gatekeepers with little intersex input.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_boundary_ambiguity, empirical, 'Operational boundary definition for intersex people within the sex-biology category').

omega_variable(
    performance_advantage_framework_contestation,
    'In sports, is performance advantage attributable to sex-based biology (the justification for sex segregation under this reading) or to prior hormone exposure, training, and complex multivariate factors that may not correlate cleanly with sex category?',
    'Sports science research on hormone-suppression protocols, fine-grained performance metrics across transgender and intersex athletes, and comparative analysis of advantage distributions within sex-segregated categories. However, the reading''s assertion of advantage attributable to female/male biology is contestable empirically and may not survive scrutiny for all sports.',
    'If performance advantage is decoupled from biological sex category, the sex-segregation justification weakens; other readings (gender-identity or spectrum-based) might win adoption in sports contexts. If advantage correlates strongly with sex biology even post-hormone-suppression, the reading strengthens in sports and justifies continued exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_advantage_framework_contestation, empirical, 'Whether sex-based performance advantage justifies sex segregation in sports').

omega_variable(
    measurement_vs_identity_conflation,
    'Does the sex-biology reading conflate a measurement category (biological sex as a clinical/epidemiological variable) with an identity/rights category (woman as a legal and social status)?',
    'Careful distinction between how health data collection might use biological variables and how legal category membership is defined. Some frameworks separate them (collecting female-pattern health data without requiring exclusive ''woman'' status); others merge them. The reading as instantiated here merges them, which may be a source of extraction beyond genuine coordination.',
    'If the categories are separable, a subset of the constraint''s measured extraction is revealed as non-functional overhead — enforcement of identity-based exclusion that does not contribute to health coordination. Reclassification would move toward snare. If inseparable, the coordination and extraction are bound together.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_vs_identity_conflation, conceptual, 'Conflation of measurement category with identity/rights category').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__sex_biology_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__sex_biology_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(woma_tr_t0, projected).
narrative_ontology:measurement(woma_tr_t5, woman_category__sex_biology_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(woma_tr_t5, observed).
narrative_ontology:measurement(woma_tr_t10, woman_category__sex_biology_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(woma_tr_t10, observed).
narrative_ontology:measurement(woma_tr_t15, woman_category__sex_biology_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(woma_tr_t15, observed).
narrative_ontology:measurement(woma_tr_t20, woman_category__sex_biology_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(woma_tr_t20, observed).
narrative_ontology:measurement(woma_tr_t25, woman_category__sex_biology_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(woma_tr_t25, projected).
narrative_ontology:measurement(woma_tr_t30, woman_category__sex_biology_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(woma_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__sex_biology_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(woma_be_t0, projected).
narrative_ontology:measurement(woma_be_t5, woman_category__sex_biology_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement_basis(woma_be_t5, observed).
narrative_ontology:measurement(woma_be_t10, woman_category__sex_biology_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(woma_be_t10, observed).
narrative_ontology:measurement(woma_be_t15, woman_category__sex_biology_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(woma_be_t15, observed).
narrative_ontology:measurement(woma_be_t20, woman_category__sex_biology_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(woma_be_t20, observed).
narrative_ontology:measurement(woma_be_t25, woman_category__sex_biology_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(woma_be_t25, projected).
narrative_ontology:measurement(woma_be_t30, woman_category__sex_biology_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(woma_be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__sex_biology_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(woma_su_t0, projected).
narrative_ontology:measurement(woma_su_t5, woman_category__sex_biology_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement_basis(woma_su_t5, observed).
narrative_ontology:measurement(woma_su_t10, woman_category__sex_biology_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(woma_su_t10, observed).
narrative_ontology:measurement(woma_su_t15, woman_category__sex_biology_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(woma_su_t15, observed).
narrative_ontology:measurement(woma_su_t20, woman_category__sex_biology_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(woma_su_t20, observed).
narrative_ontology:measurement(woma_su_t25, woman_category__sex_biology_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(woma_su_t25, projected).
narrative_ontology:measurement(woma_su_t30, woman_category__sex_biology_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(woma_su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__sex_biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_category__sex_biology_reading, 0.12).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__intersex_accommodation_reading).

% DUAL FORMULATION NOTE:
% The woman_category kernel decomposes into three constraint stories instantiating different readings. The sex_biology_reading (this story) defines category membership by chromosomal/anatomical/reproductive biology; it shows high extraction (0.68) because it excludes transgender women and some intersex people from category membership despite contestation. The gender_identity_reading defines membership by internal gender identity and would invert the victim/beneficiary sets, excluding cisgender women and sex-essentialist beneficiaries. The intersex_accommodation_reading defines membership via biological spectrum recognition and would expand the category to include non-binary intersex variations. Each reading instantiates the same kernel via different axioms and produces different ε values. They are linked because legal and institutional dominance of one reading suppresses the others — the three stories model the kernel contest structurally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
