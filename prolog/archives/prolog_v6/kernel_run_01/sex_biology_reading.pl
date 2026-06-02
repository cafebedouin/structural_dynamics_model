% ============================================================================
% CONSTRAINT STORY: sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_biology_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sex_biology_reading
 *   human_readable: Sex-Based Category Membership (Biological Reading)
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'woman_female_category': the sex-biological reading, which grounds
 *   female-category membership in chromosomal sex (XX), reproductive anatomy
 *   (ovaries/uterus capacity), and developmental biology that produces gamete
 *   capacity for ova production. Under this reading, 'woman' and 'female'
 *   refer to a natural biological category defined by these markers, and
 *   protections designated 'sex-based' apply to natal females (those assigned
 *   female at birth based on these biological traits). This reading is in
 *   active contestation with the gender-identity reading (which grounds
 *   category membership in self-identified gender regardless of chromosomal
 *   or reproductive status) and a hybrid contextual reading (which applies
 *   different criteria in different institutional domains). The constraint
 *   describes not a mere difference of opinion but a structural
 *   incompatibility: the enforcement of sex-biological boundaries necessarily
 *   excludes trans women from sex-segregated spaces, creating asymmetric
 *   extraction. The sex-biological reading's binding mechanism is
 *   institutional enforcement (policy, law, administrative procedure) that
 *   requires continuous boundary policing as edge cases (intersex
 *   individuals, hormone-replacement recipients, non-binary persons)
 *   challenge the simplicity of the XX/XY classification.
 *
 * KEY AGENTS:
 *   - Natal Females (XX-chromosome individuals with female reproductive anatomy): Primary beneficiaries (institutional/arbitrage) — benefit from sex-segregated spaces designed for their biological capacity (pregnancy, lactation, menstruation) and size/strength differences in contact sports
 *   - Trans Women (assigned male at birth, now gender-identified as female): Primary victims (powerless/trapped) — excluded from sex-segregated spaces, cannot change chromosomal classification, face suppression of alternative categorization frameworks
 *   - Sex-Based Protection Advocates (policy framers, legal organizations): Institutional beneficiaries (institutional/arbitrage) — benefit from clear biological boundary that is easy to police and enforce
 *   - Sex Category as Administrative Concept: Victim (analytical perspective) — the more the constraint enforces, the more institutional energy goes to boundary maintenance; the category itself is destabilized by edge cases and intersex variation
 *   - Institutional Enforcement Apparatus (hospitals, prisons, schools, athletic bodies): Executor (organized/constrained) — must continuously defend and police the biological boundary, producing increasing theater as the boundary becomes contested
 *   - Analytical Observer: Sees natural law frame (chromosomal sex as immutable) but risks naturalizing a contestable institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_biology_reading, 0.58).
domain_priors:suppression_score(sex_biology_reading, 0.65).
domain_priors:theater_ratio(sex_biology_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_biology_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(sex_biology_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sex_biology_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(sex_biology_reading, "Sex-Based Category Membership (Biological Reading)").
narrative_ontology:topic_domain(sex_biology_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_biology_reading, '6cbb0d57-2ac0-4e2d-9642-04425f6949ed').
narrative_ontology:cs_created_at('6cbb0d57-2ac0-4e2d-9642-04425f6949ed', '').
narrative_ontology:cs_kernel_codification('6cbb0d57-2ac0-4e2d-9642-04425f6949ed', formalized).
narrative_ontology:cs_authority_grounding('6cbb0d57-2ac0-4e2d-9642-04425f6949ed', lineage).
narrative_ontology:cs_interpretation_layer_present('6cbb0d57-2ac0-4e2d-9642-04425f6949ed').
narrative_ontology:cs_kernel_id(sex_biology_reading, woman_female_category).
narrative_ontology:cs_reading_relation('6cbb0d57-2ac0-4e2d-9642-04425f6949ed', gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('6cbb0d57-2ac0-4e2d-9642-04425f6949ed', hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('6cbb0d57-2ac0-4e2d-9642-04425f6949ed', foundational, chromosomal_sex_determines_legal_category).
narrative_ontology:cs_axiom_status(chromosomal_sex_determines_legal_category, holdable).
narrative_ontology:cs_axiom('6cbb0d57-2ac0-4e2d-9642-04425f6949ed', foundational, biological_category_supersedes_identity_claim).
narrative_ontology:cs_axiom_status(biological_category_supersedes_identity_claim, holdable).
narrative_ontology:cs_reference_frame('6cbb0d57-2ac0-4e2d-9642-04425f6949ed', stable_biological_dimorphism_framework).
narrative_ontology:cs_drift_state('6cbb0d57-2ac0-4e2d-9642-04425f6949ed', contemporary_gender_identity_contestation_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_biology_reading, natal_females).
narrative_ontology:constraint_beneficiary(sex_biology_reading, sex_segregated_protection_advocates).
narrative_ontology:constraint_victim(sex_biology_reading, trans_women_excluded_from_spaces).
narrative_ontology:constraint_victim(sex_biology_reading, sex_category_destabilization).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRANS WOMEN IN BIOLOGICAL EXCLUSION (SNARE) — Structurally trapped: cannot change chromosomal classification; cannot exit sex-segregated spaces (shelters, prisons, bathrooms, athletic competition) once excluded; bear full extraction cost of being categorized as outsiders to female category despite gender identity. No coordination benefit to exclusion; maximum suppression. Birth-documented sex is treated as permanent, immutable legal barrier.
constraint_indexing:constraint_classification(sex_biology_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NATAL FEMALES BENEFICIARY (TANGLED ROPE) — Genuine coordination function: aggregating all individuals with XX chromosomes and female reproductive anatomy into protected spaces serves legitimate physical safety coordination (pregnancy vulnerability, lactation, menstrual health access, capacity-based athleticism). Asymmetric extraction toward this agent: enforcement benefits them disproportionately. Constrained by ongoing legal contestation and cultural pressure to include trans women — high suppression of competing frameworks required to maintain sex-segregation.
constraint_indexing:constraint_classification(sex_biology_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL ENFORCER (ROPE) — Sees the constraint as pure coordination: aggregating by biological sex enables rational allocation of limited resources (shelter beds, prison security, athletic fairness structures). The institutional enforcer (policy framers, legal advocates for sex-based rights) experiences low extraction — they are solving a coordination problem. High arbitrage options: can choose which definitions of woman/female to enforce, which spaces to segregate, which measurements of sex to prioritize.
constraint_indexing:constraint_classification(sex_biology_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL INERTIA (PITON) — Many sex-segregated spaces (school bathrooms, sports classifications, medical record systems) persist through institutional inertia and routine practice even as the biological basis for sex-segregation is increasingly contested. Theater rises (0.38 → increasing) as enforcement requires explicit justification rather than implicit acceptance. The coordination function (protecting physical safety) may persist, but the performative content (rituals of exclusion, documentation procedures) increases as the biological boundary becomes less self-evident.
constraint_indexing:constraint_classification(sex_biology_reading, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / BIOLOGICAL NATURALISM (MOUNTAIN) — From a civilizational perspective, XX/XY chromosome status and reproductive anatomy are material biological facts that cannot be chosen or changed via law or policy. This perspective treats the sex-based category as natural law: an emergent property of mammalian reproductive biology that no institutional decision can alter. However, this classification is perspectival — the engine's false-summit detection will flag that the 'unchangeable biology' framing naturalizes what is actually a contestable institutional choice about which biological markers matter for legal/social purposes.
constraint_indexing:constraint_classification(sex_biology_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: SEX CATEGORY AS DESTABILIZED LEGAL FACT (SNARE, ANALYTICAL) — From the vantage of institutional law and administrative classification, the sex-based category itself becomes a victim: the constraint's operation (enforcement of biological definition) extracts from the category's stability and coherence. Institutional actors must continuously defend the biological boundary, define which markers count, and exclude edge cases (intersex individuals, non-binary persons, hormone-replacement patients). The more the constraint enforces, the more institutional resources go to boundary maintenance rather than coordination. Pure extraction from the perspective of the category's own coherence as a useful administrative tool.
constraint_indexing:constraint_classification(sex_biology_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_biology_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sex_biology_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sex_biology_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sex_biology_reading, TR),
    TR >= 0.70.

:- end_tests(sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The sex-biological reading benefits natal females and sex-based protection advocates through exclusive access to sex-segregated spaces (shelters, prisons, bathrooms, sports categories, medical services). But this is not pure extraction — genuine coordination functions exist: sex-segregated spaces do reduce some forms of violence, provide medical privacy, and acknowledge genuine biological differences in reproductive capacity and (on average) size/strength. The extraction is the enforcement cost borne by trans women and intersex individuals who are excluded from these spaces or forced to navigate administrative boundary disputes. Extractiveness has risen from 0.42 to 0.58 over the 20-year interval, reflecting increasing contestation: as more trans women assert gender identity claims, the sex-biological reading requires higher enforcement effort and suppression of alternatives. Suppression (0.65): High. The constraint requires active suppression of the gender-identity reading and hybrid-contextual reading. Administrative procedures (birth certificate sex markers, chromosome testing, reproductive anatomy verification) enforce the biological boundary. Alternative framings are treated as invalid in most sex-segregated contexts. Theater (0.38, rising): Moderate and increasing. The constraint has low initial theater (sex segregation was implicit and unquestioned), but theater rises as contestation increases. Boundary enforcement now requires explicit justification (why chromosomes matter more than identity? why hormone levels don't override chromosomal status?), documentation procedures, and defense against alternative framings. Rising theater reflects Goodhart drift: the performative content of sex-segregation (the rituals and justifications) increases as its functional legitimacy is challenged.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. Natal females seeking sex-based protections see a coordination mechanism (Rope/Tangled Rope) — their legitimate protective interests are served. Trans women see a pure extraction mechanism (Snare) — they cannot exit the trapped status of being chromosomally classified as male regardless of gender identity. The institutional enforcer sees coordination (Rope) — defining sex biologically is a clean operational rule. The category itself (viewed analytically) sees destabilization and extraction (Snare) — boundary enforcement extracts institutional resources. The biological-naturalism observer sees immutable natural law (Mountain), while the institutional analysis reveals this as a false summit: the choice to use XX/XY as the determinant of legal status is an institutional choice, not a law of nature. The core perspectival split is between those who benefit from the biological boundary (natal females, sex-based advocates) and those excluded by it (trans women). At biographical time horizons with constrained exit, trans women and natal females occupy opposite positions in the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation runs from beneficiary/victim status + exit options. Natal females are beneficiaries with arbitrage options (can choose which spaces to use, can exit sex-segregated spaces without legal penalty) → derive low d → experience low/negative effective extraction. Trans women are victims with trapped exit (cannot change chromosomal classification, excluded from spaces, cannot legally contest sex markers in most jurisdictions) → derive high d → experience maximum extraction. The institutional enforcer is beneficiary with arbitrage (can choose which markers to police, which spaces to designate) → low d. The constraint's effective extractiveness χ is scaled by f(d) and scope: at global scope, the extraction is amplified (σ=1.2). The perspectival divergence is structurally necessary: the same constraint produces snare (for victims), tangled rope (for beneficiaries), and rope (for institutional enforcers) because their positions in the extraction flow differ fundamentally.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that this constraint is ONE READING of a contested kernel, and different readings produce different structural positions and classifications. The mandatrophy question 'Is this Rope (coordination) or Snare (extraction)?' has no single answer — both are true from different perspectives. The constraint IS coordination (for those who benefit from sex segregation) AND extraction (for those excluded from it). The resolution is not to declare one true and one false, but to recognize that the reading itself — the choice to ground female-category membership in XX/XY biological markers — determines who benefits and who is victimized. If a different reading (gender-identity) were instantiated, the roles would reverse: trans women would be beneficiaries, natal females would be victims of category destabilization. This constraint resolves mandatrophy by exhibiting the full presheaf structure: multiple mutually inconsistent classifications from different observations of the same structure, all valid from their respective contexts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_markers_irreducible_ambiguity,
    'Which biological markers (chromosomes, gonads, reproductive anatomy, hormone levels, developmental trajectory) constitute ''biological sex'' for legal and protective purposes? Do these markers co-vary consistently, or do edge cases and intersex variation undermine a unified biological definition?',
    'Empirical: comprehensive medical literature review of intersex variation, hormone-replacement effects on athletic performance, chromosomal mosaicism. Definitional: policy analysis of which jurisdictions use which markers (chromosomal, anatomical, hormonal) and whether their choices correlate with stated protective rationales.',
    'If markers co-vary reliably: sex-biological boundary is stable and ε ≤ 0.40 (higher coordination legitimacy). If markers diverge in significant edge cases: ε ≥ 0.60 (extraction mechanism relying on simplification that excludes documented biological variation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_markers_irreducible_ambiguity, empirical, 'Whether biological sex markers co-vary consistently or diverge in edge cases').

omega_variable(
    protective_rationale_scope_alignment,
    'Does the biological sex boundary (XX/XY, reproductive anatomy) actually map onto the legitimate protective interests (physical safety in prisons/shelters, athletic fairness, menstrual health access) that justify sex segregation? Or does enforcement persist because the biological boundary is easier to police than the actual vulnerability or fairness criteria?',
    'Empirical: correlate actual risks (violence in prisons, injury in contact sports, health interventions requiring menstrual knowledge) against agent characteristics; determine whether XX chromosomes or female reproductive anatomy predict the risk better than other factors (size, muscle mass, prior violent history, hormone levels). Policy analysis: compare sex-segregation policies with outcome metrics — do segregation policies actually reduce violence/injury/health disparity, or are the claimed protective benefits aspirational?',
    'If alignment is strong: the constraint is genuine coordination (ε ≤ 0.40, Rope from more perspectives). If alignment is weak: the constraint is extraction masquerading as coordination (ε ≥ 0.65, Snare/Tangled Rope — the biological marker is convenient for enforcement but decoupled from the actual protective rationale).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protective_rationale_scope_alignment, empirical, 'Whether sex-segregation boundary aligns with stated protective rationales').

omega_variable(
    trans_women_comparable_risk_profile,
    'Do trans women (individuals with XY chromosomes assigned male at birth, now identifying as female) pose risk profiles comparable to cisgender men in sex-segregated spaces (prisons, shelters, contact sports)? Or do hormone-replacement therapy, social integration, and psychological factors make their risk profile more comparable to natal females after transition?',
    'Empirical: longitudinal data on violence/injury rates in integrated vs sex-segregated settings; comparison of trans women''s behavior/health outcomes pre- and post-transition; analysis of specific populations (prison violence, sports injury, shelter safety). Causal: identify which factors drive the risks (testosterone levels, prior socialization, behavioral patterns, institutional housing practices) and whether these change with transition.',
    'If trans women = comparable to cis men: current sex-segregation enforces legitimate protection (ε ≤ 0.45). If trans women post-transition ≈ comparable to cis women: sex-segregation is extraction mechanism masquerading as protection (ε ≥ 0.65); alternate risk-assessment frameworks would be more tailored and less extractive. If risk profile is context-specific (safer in shelters, higher in contact sports): ε varies by institutional context — decompose into separate constraints per domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trans_women_comparable_risk_profile, empirical, 'Whether trans women''s risk profile matches cis men or cis women post-transition').

omega_variable(
    reading_coexistence_within_liberal_framework,
    'Can the sex-biological reading (XX/XY defines woman) and the gender-identity reading (self-identified gender defines woman) coexist within a single liberal-democratic institutional framework, or do they logically foreclose each other?',
    'Normative analysis: explore hybrid frameworks that assign different authority to sex-biological and gender-identity criteria in different institutional domains (e.g., biological sex for sports/medicine, gender identity for social recognition/bathrooms). Empirical: examine actual policy jurisdictions that claim to honor both readings and assess whether the integration is coherent or produces systematic contradictions that force one reading to be suppressed.',
    'If coexistence is possible: readings are coexists_with (both live simultaneously in different domains). If coexistence forces systematic suppression: readings foreclose each other — the sex-biological reading requires trans women to be excluded; the gender-identity reading requires that exclusion to be invalid. This determines the cs_structure.reading_relations edge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_within_liberal_framework, conceptual, 'Whether sex-biological and gender-identity readings can coexist in one framework').

omega_variable(
    false_summit_natural_law_claim,
    'Is the sex-biological boundary an immutable natural law (XX/XY chromosomes are unchangeable facts of biology), or a contested institutional choice about which biological markers matter for legal purposes?',
    'Distinguish between immutable biological fact (chromosomal sex cannot be chosen) and institutional framing (whether chromosomal sex determines legal/social status in any given domain). Document cases where chromosome-based classification conflicts with stated protective aims, where medical/scientific classification uses different sex markers (hormone levels, reproductive capacity, phenotype), or where intersex variation undermines simple binary classification. If the false-summit signature fires, it signals that ''unchangeable biology'' is being used as cover for a contestable institutional choice about category membership.',
    'If true natural law: mountain classification stands (ε ≤ 0.25, no beneficiaries needed). If false summit: reclassifies to tangled_rope or snare (ε ≥ 0.46), with identifiable beneficiaries who benefit from naturalizing the institutional choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether sex-biological boundary is natural law or constructed institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_biology_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex_bio_theater_t0, sex_biology_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(sex_bio_theater_t10, sex_biology_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(sex_bio_theater_t20, sex_biology_reading, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(sex_bio_extract_t0, sex_biology_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sex_bio_extract_t10, sex_biology_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(sex_bio_extract_t20, sex_biology_reading, base_extractiveness, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_biology_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_biology_reading, gender_identity_reading).
narrative_ontology:affects_constraint(sex_biology_reading, hybrid_contextual_reading).
narrative_ontology:affects_constraint(sex_biology_reading, sex_segregated_shelter_access).
narrative_ontology:affects_constraint(sex_biology_reading, athletic_sex_category_determination).
narrative_ontology:affects_constraint(sex_biology_reading, prison_sex_segregation_policy).

% DUAL FORMULATION NOTE:
% The sex-biological reading is upstream of domain-specific constraints (shelters, athletics, prisons) that inherit its category definitions. Each domain instantiates the same reading with context-specific extractiveness values. The reading itself has ε=0.58 as a general institutional commitment; specific applications (athletic competition, prison assignment, shelter access) may have different ε values reflecting different protective rationales and edge-case prevalence. Sibling readings (gender-identity, hybrid-contextual) are separate constraint stories with different beneficiary/victim structures and different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sex_biology_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
