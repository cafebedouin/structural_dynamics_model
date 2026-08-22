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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: woman_category__sex_biology_reading
 *   human_readable: Woman Category (Sex-Biology Reading)
 *   domain: political_philosophy/law/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested 'woman'
 *   category: the sex-biology reading, under which 'woman' is defined by
 *   chromosomal/anatomical/reproductive characteristics (typically XX
 *   chromosomes and female reproductive anatomy). This reading grounds
 *   sex-segregated systems (athletics, shelters, bathrooms, medical
 *   protocols, violence-against-women data collection) in biological fact,
 *   presented as natural and measurable. The constraint benefits people with
 *   female biology and the institutions that operate sex-segregated systems;
 *   it imposes costs on transgender women (excluded from sex-specific
 *   protections and category membership) and intersex people without typical
 *   female anatomy (ambiguously situated). The sibling
 *   readings—gender-identity reading and intersex-accommodation reading—would
 *   redefine the boundary and reclassify many stakeholders' positions. This
 *   story is the sex-biology reading alone; the siblings are separate
 *   constraints with their own ε values, beneficiary/victim sets, and
 *   classifications.
 *
 * KEY AGENTS:
 *   - cisgender_women: primary beneficiaries of sex-based protections, shelters, athletic categories, medical protocols, and harm data collection
 *   - transgender_women: excluded from sex-category membership and protections under this reading; bear the cost of non-recognition
 *   - intersex_people_without_typical_female_anatomy: ambiguously positioned; partly excluded, partly included depending on institutional interpretation
 *   - sex_segregated_institution_operators: agenda-setters; enforce the biological boundary through policy, screening, and legal defense; justify enforcement as safety/fairness/privacy
 *   - data_collection_authorities: agenda-setters; use this reading's boundary to measure sex-based violence and reproductive health; maintain population-level accountability to female-bodied people
 *   - transgender_rights_advocates: excluded from institutional conversations; would argue this boundary is discriminatory and contradicts gender-identity law
 *   - medical_professionals: observers; see that biological sex is causally relevant to some conditions but that gender identity and social experience matter for care
 *   - feminist scholars and advocates: split—some defend this reading as necessary to protect female-bodied people; others argue it overgeneralizes and excludes vulnerable transgender women
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__sex_biology_reading, 0.68).
domain_priors:suppression_score(woman_category__sex_biology_reading, 0.72).
domain_priors:theater_ratio(woman_category__sex_biology_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__sex_biology_reading, "Woman Category (Sex-Biology Reading)").
narrative_ontology:topic_domain(woman_category__sex_biology_reading, "political_philosophy/law/bioethics").

domain_priors:requires_active_enforcement(woman_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__sex_biology_reading, 'ad535fa7-e00b-4d5d-b9c7-214e684aa2b8').
narrative_ontology:cs_kernel_codification('ad535fa7-e00b-4d5d-b9c7-214e684aa2b8', distributed).
narrative_ontology:cs_authority_grounding('ad535fa7-e00b-4d5d-b9c7-214e684aa2b8', distributed).
narrative_ontology:cs_reading_relation('ad535fa7-e00b-4d5d-b9c7-214e684aa2b8', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_reading_relation('ad535fa7-e00b-4d5d-b9c7-214e684aa2b8', woman_category__intersex_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('ad535fa7-e00b-4d5d-b9c7-214e684aa2b8', foundational, biological_sex_is_woman_boundary).
narrative_ontology:cs_axiom_status(biological_sex_is_woman_boundary, holdable).
narrative_ontology:cs_axiom_grounding('ad535fa7-e00b-4d5d-b9c7-214e684aa2b8', biological_sex_is_woman_boundary, empirically_contingent).
narrative_ontology:cs_axiom('ad535fa7-e00b-4d5d-b9c7-214e684aa2b8', secondary, sex_identity_distinction_valid).
narrative_ontology:cs_axiom_status(sex_identity_distinction_valid, holdable).
narrative_ontology:cs_axiom_grounding('ad535fa7-e00b-4d5d-b9c7-214e684aa2b8', sex_identity_distinction_valid, empirically_contingent).
narrative_ontology:cs_reference_frame('ad535fa7-e00b-4d5d-b9c7-214e684aa2b8', objective_biological_sex_framework).
narrative_ontology:cs_drift_state('ad535fa7-e00b-4d5d-b9c7-214e684aa2b8', contemporary_antidiscrimination_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ad535fa7-e00b-4d5d-b9c7-214e684aa2b8', '').
narrative_ontology:cs_kernel_id(woman_category__sex_biology_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, cisgender_women).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, sex_segregated_institution_operators).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, data_collection_authorities).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, transgender_women).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, intersex_people_without_typical_female_anatomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals born with female reproductive anatomy and XX chromosomes who identify as women. Benefit from sex-segregated athletic categories (preventing performance-advantage competition), single-sex shelter spaces during crisis, medical protocols acknowledging reproductive-system physiology, and sex-disaggregated violence-against-women data collection that documents their population's distinct harm patterns. Their exit is constrained by embodied sex characteristics that are not chosen and cannot be easily concealed or altered.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, cisgender_women, beneficiary,
    organized, generational, constrained, universal).

% Adults who identify as women but do not have the anatomical/chromosomal profile this reading defines as 'woman.' Under this constraint, they are excluded from or conditionally admitted to sex-segregated spaces, sports categories, and benefits predicated on biological female classification. They cannot exit by adopting the chromosomal or anatomical form this reading requires (the requirements are presented as biological fact, not policy choice). Shelter and safety access depend on institutional decision-making about whether to apply this reading's boundary.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, transgender_women, payer,
    moderate, biographical, constrained, universal).

% Individuals with sex-development variations (chromosomal, gonadal, or anatomical) who do not fit the 'XX + typical female anatomy' profile but identify as women or have female social assignment. This reading's boundary is ambiguous for them: they may be excluded from protections intended for cisgender women while also being excluded from male-category protections. Their identity and medical reality are both at stake. Exit via boundary-shifting is theoretically possible but practically locked by medical complexity and social identity already established.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, intersex_people_without_typical_female_anatomy, payer,
    powerless, biographical, identity_locked, universal).

% Sports organizations, shelter providers, correctional facilities, military academies, bathrooms, and other systems that allocate access by sex category. Under this reading, they are the enforcers: they set membership criteria by biological specification, conduct (or require) documentation of sex status, and defend the boundary through policy, screening, and sometimes legal action. They justify enforcement as necessary for safety, fairness, or privacy. Pressure to change (from antidiscrimination law, institutional liability, or political pressure) costs enforcement resources; maintaining the boundary also costs resources.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, sex_segregated_institution_operators, agenda_setter,
    institutional, generational, constrained, universal).

% Public health agencies, violence-victimization researchers, medical systems, and census bodies that collect and report disaggregated data on violence against women, reproductive health, and sex-based discrimination. This reading supports their use of biological sex as the data axis: 'woman' = female-biology category enables population-level harm assessment specific to female-bodied people (pregnancy-related mortality, obstetric violence, sex-specific disease prevalence). The constraint provides the categorical boundary they use; changing it shifts what populations they measure.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, data_collection_authorities, agenda_setter,
    institutional, generational, constrained, universal).

% Activists and organizations arguing that gender identity, not chromosomal/anatomical sex, is the legitimate boundary for woman-category membership. They are structurally excluded from the institutional conversations that set sex-based protections and category boundaries. If admitted, they would argue that this reading's boundary is discriminatory, reifies biological essentialism, and contradicts gender-identity-based antidiscrimination law.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, transgender_rights_advocates, excluded,
    moderate, generational, constrained, universal).

% Physicians, endocrinologists, reproductive health specialists who treat patients across all these category memberships. They observe that sex biology is causally relevant to some medical conditions (reproductive-system physiology, for example) but that gender identity and social experience are relevant to others (mental health, care-seeking patterns, discrimination exposure). They see the constraint both as enabling important sex-specific research and as creating complexity when patient identity does not align with this reading's boundary.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, medical_professionals, observer,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__sex_biology_reading, sex_segregated_institution_operators).
narrative_ontology:fixing_cost_class(woman_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables sex-segregated institutional systems (athletics, shelters, bathrooms, medical protocols, data collection) by providing a clear, measurable boundary: 'woman' = adult human female. Without a boundary, these systems would require case-by-case assessment or would collapse into mixed-sex operation. The coordination problem solved: allocating access to spaces and services justified by privacy, safety (violence prevention), competitive fairness, and medical relevance.
% TRANSFER_FUNCTION: Moves institutional legitimacy and protection access to cisgender women and institution operators (who receive simplified enforcement), at the cost of excluding transgender women and intersex people from sex-based protections and categorical recognition. Also moves data-collection clarity (measuring violence against 'women' defined biologically) at the cost of not measuring violence against transgender women under the same category.
% ABSENT_VOICES: Transgender rights organizations and intersex advocacy groups are excluded from institutional settings where sex-segregated systems are operated (sports organizations, shelter providers, bathrooms, medical standard-setting bodies). These voices would object that the reading is discriminatory, medically incoherent, and contradicts gender-identity-based antidiscrimination law. Their absence from day-to-day enforcement settings means this reading's assumptions often go unexamined within institutions.
% DISAPPEARANCE_RATIONALE: If institutional enforcement of the biological-sex boundary as the definition of 'woman' disappeared, sex-segregated systems would reorganize: some would adopt gender-identity boundaries, some would use multiple axes, some would become mixed-sex, and some would dissolve. Violence-against-women data collection would either adopt identity-based categories, maintain separate axes for biological sex and gender identity, or lose population granularity. The coordination function—allocating access to sex-segregated spaces—would not disappear, but the boundary used to allocate access would change.
% FOUNDING_PROBLEM: As feminist movements in the 1960s–1990s demanded sex-segregated protections against violence and discrimination, institutional systems needed a boundary to allocate access to single-sex spaces (shelters, bathrooms, athletic categories, etc.). Biological sex (chromosomal/anatomical) was chosen as the boundary because it appeared natural, measurable, and stable—a proxy for the vulnerable population that experiences sex-specific violence.
% FOUNDING_PROBLEM_CORROBORATION: Feminist advocates for violence-against-women services and medical professionals specializing in female reproductive health attest the founding problem was live: sex-segregated protections were necessary to address violence targeting people with female bodies. Cisgender women advocates attest the problem remains partially live because violence-against-women persists. However, transgender rights organizations, intersex advocates, and an increasing number of medical and legal professionals attest the founding problem has shifted: the original vulnerability is now substantially addressed (services exist, law protects); the problem now is how to extend protections to transgender women and intersex people without abandoning accountability to female-bodied populations. Epidemiological data show violence against transgender women at rates comparable to cisgender women, suggesting the founding problem's scope was always broader than the biological-sex boundary captures.
narrative_ontology:disappearance_verdict(woman_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__sex_biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__sex_biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__sex_biology_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68 at interval end) because the constraint excludes a defined population from protections, categorical recognition, and institutional legitimacy without their consent and despite their need for the same protections (e.g., transgender women experience violence at rates comparable to cisgender women but are not reliably counted in violence-against-women data under this reading). The measurement series shows rising extractiveness from 0.42 (t=0, projected) to 0.71 (t=40, observed), then slight decline to 0.68 (t=50), reflecting increased institutional enforcement and clarification of this reading's boundary through law and policy, followed by modest pressure from antidiscrimination law and medical complexity. Suppression is high (0.72) because the constraint's persistence depends on actively defending the biological boundary against challenge: institutions must screen, verify sex status, handle intersex cases (ambiguously), and defend the boundary against legal and political pressure. Theater ratio rises from 0.12 to 0.42, indicating growing share of enforcement activity devoted to boundary defense (rhetoric about 'biological reality,' legal briefs asserting naturalness) rather than the original coordination function (enabling single-sex spaces). The constraint is classified as tangled_rope because it coordinates a genuine function (sex-segregated spaces solve real safety and privacy coordination problems) AND asymmetrically extracts from excluded populations (transgender women, some intersex people). Coercion-grid data show individual-level accessibility collapse rising (transgender women's exit options narrow as institutions clarify the boundary) and organizational-level suppression rising (institutional resistance to alternative readings hardens). Resistance to the constraint rises as well (from 0.62 to 0.81), indicating transgender rights advocacy and intersex activism gaining visibility and mounting challenge.
 *
 * PERSPECTIVAL GAP:
 *   Cisgender women and institution operators see this reading as natural, protective, and based on objective biology—they experience the constraint as enabling safety and enabling fair measurement of their population's distinct harms. Transgender women see the same constraint as exclusionary, denying them recognition and protection despite comparable vulnerability. Intersex people without typical female anatomy see it as medically incoherent (sex biology is not a simple binary) and personally erasing. The engine should compute per-seat classifications from this structural data: cisgender women should compute as net beneficiaries (moderate d toward benefit); transgender women as net targets (high d toward extraction, constrained exit); institution operators as beneficiaries of enforcement simplicity (high d toward benefit, powerful position); data authorities as beneficiaries of categorical clarity (high d toward benefit). The asymmetry is the constraint's defining feature.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (cisgender women, institution operators) get d near the beneficiary end because they collect from the constraint's operation: cisgender women access protections and categorical recognition; operators access institutional simplicity and legal clarity. Victims (transgender women, intersex people) get d near the target end because the constraint extracts from them: non-access to sex-based protections, non-recognition as category members, forced disclosure or concealment of sex status. The key directionality driver is exit: cisgender women have no good exit (their biological sex cannot be concealed or changed without medical/legal complexity); transgender women have constrained exit (they can transition, but transition does not change how institutional systems apply this reading, and non-disclosure risks harm). Intersex people have identity_locked exit (their biology does not fit the category, and identity is already established socially). Institutional operators have powerful positions and significant exit cost (changing the boundary costs institutional reorganization), so their d is driven by benefit collection.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint risks mandatrophy because the founding problem (creating sex-segregated protections for people with female anatomy to address sex-based violence) was solved decades ago: comprehensive sex-based antidiscrimination law, domestic violence shelters, reproductive health services, and violence-against-women data collection all exist and are well-established. The institutional purpose that justified the constraint is largely satisfied. Yet the constraint persists, and enforcement has intensified (theater ratio rose from 0.12 to 0.42). The mandatrophy signal is present: institution operators continue defending the biological boundary against alternative readings not because the original problem demands it but because the boundary's clarity serves institutional interests (simplifies screening, allocates liability, maintains an established system). The founding problem status is contested (cisgender feminist advocates say the problem remains live; transgender and intersex advocates say the problem is solved but the constraint persists for rent-seeking). This is the mandatrophy pattern: a coordination function that succeeded becoming institutional inertia defended rhetorically as natural fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_sex_boundary_clarity,
    'Is biological sex a clear, binary, measurable boundary, or is it a spectrum with contested edges (intersex variations, chromosomal complexity, endocrine variation)?',
    'Medical and genetic analysis of sex-development variation; assessment of how many people fall into unclear categories; institutional audit of how sex-verification is actually performed in practice.',
    'If sex is a clear binary, the constraint is defensible as based on objective fact; if it is a spectrum with significant unclear cases, the constraint''s presentation as ''natural'' is undermined and the exclusion of intersex people becomes transparently normative rather than descriptive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_sex_boundary_clarity, empirical, 'Whether sex biology is a clear binary or a spectrum with ambiguous edges.').

omega_variable(
    sex_vs_gender_identity_commensurability,
    'Can a single framework hold both the sex-biology reading and the gender-identity reading as live options, or does one logically foreclose the other?',
    'Philosophical analysis of whether ''woman'' can simultaneously mean ''person with female biology'' (sex-biology reading) and ''person who identifies as a woman'' (gender-identity reading) without contradiction. Test whether a jurisdiction can adopt both readings in different legal contexts without logical inconsistency.',
    'If the readings foreclose each other, the constraint is engaged in a foundational conflict with sibling readings and the contest is not about balancing interests but about competing truth claims; if they coexist, the contest is about which context each applies in (sports context vs. shelter context) and the constraint is contingently chosen, not necessarily true.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sex_vs_gender_identity_commensurability, conceptual, 'Logical relationship between sex-biology and gender-identity readings of the woman category.').

omega_variable(
    protective_function_sufficiency,
    'Is defining ''woman'' by biological sex necessary for violence-against-women data collection and protection, or can these functions operate with multiple category axes (biological sex, gender identity, perceived social gender, self-identification)?',
    'Comparison of epidemiological data collection systems in jurisdictions using different category definitions; assessment of whether violence-prevention services can operate with identity-based boundaries without loss of accountability to female-bodied populations.',
    'If biological sex is strictly necessary, the constraint enables accountability to a population that experiences distinct harms; if the function can operate with multiple axes, the constraint''s exclusion of transgender women becomes a choice, not a requirement, and shifts toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protective_function_sufficiency, empirical, 'Whether the protective and epidemiological functions require biological-sex definition or can operate with alternative category axes.').

omega_variable(
    institutional_stability_vs_inclusive_boundary,
    'Does enforcement of the biological-sex boundary primarily serve the protection of people with female biology, or does it primarily serve institutional simplicity and cost-reduction in sex-segregated systems?',
    'Institutional cost-benefit analysis: measure enforcement cost (screening, verification, legal defense, handling intersex cases) against protective benefit (did violence decrease, did care improve, did accountability increase?). Compare cost-benefit profiles between jurisdictions using biological-sex boundaries and jurisdictions using identity-based boundaries.',
    'If enforcement primarily serves protection, the constraint is well-justified and the extraction is coordination cost; if it primarily serves institutional simplicity, the constraint is an extractive rent-seeking device where beneficiaries collect institutional convenience at the cost of excluded populations'' non-recognition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_stability_vs_inclusive_boundary, empirical, 'Whether institutional enforcement of the biological-sex boundary is justified by protective function or primarily serves institutional convenience.').

omega_variable(
    alternative_readings_foreclose_analysis,
    'Is this reading''s core premise (biological sex is the defining feature of ''woman'') logically foreclosed by the gender-identity reading, or do they coexist as genuinely live alternatives?',
    'Test whether an institutional actor (a judge, a legislator, a medical body) could rationally endorse the sex-biology reading after examining the gender-identity reading, or whether the gender-identity reading logically precludes acceptance of the sex-biology reading''s core claim.',
    'If foreclosed, the constraint is not merely contingently chosen but is in foundational conflict with sibling readings; if coexisting, the contest is about institutional choice and the constraint remains contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_readings_foreclose_analysis, conceptual, 'Whether sex-biology and gender-identity readings logically foreclose each other or coexist as live options.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__sex_biology_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__sex_biology_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(woma_tr_t0, projected).
narrative_ontology:measurement(woma_tr_t10, woman_category__sex_biology_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(woma_tr_t10, observed).
narrative_ontology:measurement(woma_tr_t20, woman_category__sex_biology_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement_basis(woma_tr_t20, observed).
narrative_ontology:measurement(woma_tr_t30, woman_category__sex_biology_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(woma_tr_t30, observed).
narrative_ontology:measurement(woma_tr_t40, woman_category__sex_biology_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(woma_tr_t40, observed).
narrative_ontology:measurement(woma_tr_t50, woman_category__sex_biology_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(woma_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__sex_biology_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(woma_be_t0, projected).
narrative_ontology:measurement(woma_be_t10, woman_category__sex_biology_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement_basis(woma_be_t10, observed).
narrative_ontology:measurement(woma_be_t20, woman_category__sex_biology_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(woma_be_t20, observed).
narrative_ontology:measurement(woma_be_t30, woman_category__sex_biology_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(woma_be_t30, observed).
narrative_ontology:measurement(woma_be_t40, woman_category__sex_biology_reading, base_extractiveness, 40, 0.71).
narrative_ontology:measurement_basis(woma_be_t40, observed).
narrative_ontology:measurement(woma_be_t50, woman_category__sex_biology_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(woma_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__sex_biology_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(woma_su_t0, projected).
narrative_ontology:measurement(woma_su_t10, woman_category__sex_biology_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(woma_su_t10, observed).
narrative_ontology:measurement(woma_su_t20, woman_category__sex_biology_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement_basis(woma_su_t20, observed).
narrative_ontology:measurement(woma_su_t30, woman_category__sex_biology_reading, suppression_requirement, 30, 0.74).
narrative_ontology:measurement_basis(woma_su_t30, observed).
narrative_ontology:measurement(woma_su_t40, woman_category__sex_biology_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement_basis(woma_su_t40, observed).
narrative_ontology:measurement(woma_su_t50, woman_category__sex_biology_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(woma_su_t50, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(woma_grid_01, woman_category__sex_biology_reading, accessibility_collapse(class), 0, 0.68).
narrative_ontology:measurement(woma_grid_02, woman_category__sex_biology_reading, accessibility_collapse(class), 50, 0.82).
narrative_ontology:measurement(woma_grid_03, woman_category__sex_biology_reading, accessibility_collapse(individual), 0, 0.62).
narrative_ontology:measurement(woma_grid_04, woman_category__sex_biology_reading, accessibility_collapse(individual), 50, 0.74).
narrative_ontology:measurement(woma_grid_05, woman_category__sex_biology_reading, accessibility_collapse(organizational), 0, 0.71).
narrative_ontology:measurement(woma_grid_06, woman_category__sex_biology_reading, accessibility_collapse(organizational), 50, 0.81).
narrative_ontology:measurement(woma_grid_07, woman_category__sex_biology_reading, accessibility_collapse(structural), 0, 0.75).
narrative_ontology:measurement(woma_grid_08, woman_category__sex_biology_reading, accessibility_collapse(structural), 50, 0.88).
narrative_ontology:measurement(woma_grid_09, woman_category__sex_biology_reading, resistance(class), 0, 0.71).
narrative_ontology:measurement(woma_grid_10, woman_category__sex_biology_reading, resistance(class), 50, 0.84).
narrative_ontology:measurement(woma_grid_11, woman_category__sex_biology_reading, resistance(individual), 0, 0.62).
narrative_ontology:measurement(woma_grid_12, woman_category__sex_biology_reading, resistance(individual), 50, 0.81).
narrative_ontology:measurement(woma_grid_13, woman_category__sex_biology_reading, resistance(organizational), 0, 0.58).
narrative_ontology:measurement(woma_grid_14, woman_category__sex_biology_reading, resistance(organizational), 50, 0.79).
narrative_ontology:measurement(woma_grid_15, woman_category__sex_biology_reading, resistance(structural), 0, 0.64).
narrative_ontology:measurement(woma_grid_16, woman_category__sex_biology_reading, resistance(structural), 50, 0.78).
narrative_ontology:measurement(woma_grid_17, woman_category__sex_biology_reading, stakes_inflation(class), 0, 0.44).
narrative_ontology:measurement(woma_grid_18, woman_category__sex_biology_reading, stakes_inflation(class), 50, 0.68).
narrative_ontology:measurement(woma_grid_19, woman_category__sex_biology_reading, stakes_inflation(individual), 0, 0.48).
narrative_ontology:measurement(woma_grid_20, woman_category__sex_biology_reading, stakes_inflation(individual), 50, 0.72).
narrative_ontology:measurement(woma_grid_21, woman_category__sex_biology_reading, stakes_inflation(organizational), 0, 0.52).
narrative_ontology:measurement(woma_grid_22, woman_category__sex_biology_reading, stakes_inflation(organizational), 50, 0.78).
narrative_ontology:measurement(woma_grid_23, woman_category__sex_biology_reading, stakes_inflation(structural), 0, 0.38).
narrative_ontology:measurement(woma_grid_24, woman_category__sex_biology_reading, stakes_inflation(structural), 50, 0.62).
narrative_ontology:measurement(woma_grid_25, woman_category__sex_biology_reading, suppression(class), 0, 0.48).
narrative_ontology:measurement(woma_grid_26, woman_category__sex_biology_reading, suppression(class), 50, 0.72).
narrative_ontology:measurement(woma_grid_27, woman_category__sex_biology_reading, suppression(individual), 0, 0.42).
narrative_ontology:measurement(woma_grid_28, woman_category__sex_biology_reading, suppression(individual), 50, 0.68).
narrative_ontology:measurement(woma_grid_29, woman_category__sex_biology_reading, suppression(organizational), 0, 0.51).
narrative_ontology:measurement(woma_grid_30, woman_category__sex_biology_reading, suppression(organizational), 50, 0.76).
narrative_ontology:measurement(woma_grid_31, woman_category__sex_biology_reading, suppression(structural), 0, 0.54).
narrative_ontology:measurement(woma_grid_32, woman_category__sex_biology_reading, suppression(structural), 50, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__sex_biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_category__sex_biology_reading, 0.12).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__intersex_accommodation_reading).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, sports_eligibility_sex_based).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, violence_against_women_data_collection).

% DUAL FORMULATION NOTE:
% The 'woman' category kernel decomposes into three structurally distinct readings: sex-biology (this constraint, ε=0.68, tangled_rope), gender-identity (sibling, ε differs), and intersex-accommodation (sibling, ε differs). Each reading grounds a different set of institutions (athletic bodies use sex-biology; antidiscrimination law increasingly uses gender-identity; medical systems navigate both). The readings have different beneficiary/victim structures and are best modeled as separate constraints in the same family, linked via network.affects_constraints. The constraint family's internal dynamics reflect a foundational contest about how sex/gender categories are legitimately defined.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_category__sex_biology_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
