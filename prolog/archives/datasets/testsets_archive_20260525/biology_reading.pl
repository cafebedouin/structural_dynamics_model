% ============================================================================
% CONSTRAINT STORY: biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biology_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: biology_reading
 *   human_readable: Sex Category Membership via Reproductive Biology (Biology Reading)
 *   domain: social_ontology/identity_politics/legal_classification
 *
 * SUMMARY:
 *   Sex category membership determined by reproductive biology is ONE READING
 *   of the contested kernel 'sex_gender_category.' This reading instantiates
 *   a classification system where legal sex category (male, female, and no
 *   legal third category in most jurisdictions) is assigned at birth based on
 *   reproductive anatomy or chromosomal configuration and remains legally
 *   immutable regardless of identity, medical transition, or lived gender.
 *   The biology reading operates as a snare for trans and intersex
 *   individuals who experience the constraint as a trap with no exit
 *   mechanism: they are forced into categories that contradict their identity
 *   and medical reality, with high suppression (legal documentation barriers,
 *   healthcare access friction, social sanction) and extraction (legal
 *   vulnerability, reputational harm, forced misclassification). For cis
 *   women, the constraint functions partially as coordination (sex-based harm
 *   protection) and partially as extraction (forced political labor defending
 *   category boundaries). For institutional actors (medical systems, legal
 *   authorities), the constraint appears as pure coordination: stable, easily
 *   verifiable, administratively efficient. For organized trans rights
 *   actors, the constraint appears as a temporary institutional arrangement
 *   with an emerging exit pathway through identity-based legal recognition.
 *   For the analytical observer from a civilizational vantage, the constraint
 *   risks appearing as natural law — reproductive biology as an immutable
 *   fact — but the structural data reveals this as a false summit:
 *   beneficiaries exist, enforcement is required, and alternative readings
 *   are coherent.
 *
 * KEY AGENTS:
 *   - Trans Women: Primary victims (powerless/trapped) — forced into legal male category despite identity and often medical transition; experience maximum extraction through legal and social vulnerability
 *   - Intersex Individuals: Primary victims (powerless/trapped) — forced into binary categories despite atypical reproductive markers; experience extraction through arbitrary classification and often unwanted medical intervention
 *   - Cis Women: Mixed position (moderate/constrained) — victimized by sex-based harms and beneficiary of sex-category-based legal protections; experience moderate extraction through boundary-defense labor and political conflict
 *   - Medical-Legal Classification Authority: Beneficiary (institutional/arbitrage) — reproductive anatomy provides stable classification basis; experiences constraint as coordination mechanism
 *   - Institutional Male Gatekeepers: Secondary beneficiary (powerful/arbitrage) — reproductive biology reading preserves institutional male dominance in spaces (sports, prisons, military) where sex classification determines access
 *   - Trans Rights Coalition: Organized agents (organized/constrained) — perceive emerging exit pathway through identity-based legal recognition; see constraint as temporary scaffold
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biology_reading, 0.58).
domain_priors:suppression_score(biology_reading, 0.72).
domain_priors:theater_ratio(biology_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biology_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(biology_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(biology_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biology_reading, snare).
narrative_ontology:human_readable(biology_reading, "Sex Category Membership via Reproductive Biology (Biology Reading)").
narrative_ontology:topic_domain(biology_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(biology_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biology_reading, institutional_classification_authority).
narrative_ontology:constraint_beneficiary(biology_reading, cis_male_gatekeepers).
narrative_ontology:constraint_victim(biology_reading, trans_individuals).
narrative_ontology:constraint_victim(biology_reading, intersex_individuals).
narrative_ontology:constraint_victim(biology_reading, legal_autonomy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRANS WOMAN (SNARE) — Structural immobility. Reproductive anatomy at birth assigns permanent legal sex status regardless of identity, medical transition, or social role. No exit mechanism. High suppression: legal documentation barriers, healthcare system friction, social sanction. Extraction: forced misclassification generates legal vulnerability (bathroom access, sports eligibility, prison assignment) and reputational harm. Maximum experienced extraction because the trap is constitutive — the agent cannot escape the biological-reproductive assignment without challenging the kernel itself.
constraint_indexing:constraint_classification(biology_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERSEX INDIVIDUAL (SNARE) — Reproductive biology reading classifies all ambiguous/non-binary anatomies into the binary categories. Atypical sex development is forced into 'male' or 'female' by legal assignment at birth or early medical intervention. No exit mechanism short of legal battle. Suppression: medical system gatekeeping, legal non-recognition of variation, social sanction. Extraction: forced categorization erases identity, produces legal misclassification, justifies medical protocols (hormone treatment, genital surgery) that may violate autonomy.
constraint_indexing:constraint_classification(biology_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CIS WOMAN (TANGLED ROPE) — Genuinely victimized by sex-based harms (reproductive coercion, sexual violence, wage discrimination) for which biology-based category membership is analytically relevant for harm documentation. But also derives category benefit: legal recognition, institutional services designed for reproductive health, sports eligibility. The constraint coordinates sex-based harm protection (genuine function) while extracting the cost of boundary enforcement from trans and intersex agents. Medium extraction experienced because the agent benefits from the coordination function but is forced to defend category boundaries, producing social friction and political labor.
constraint_indexing:constraint_classification(biology_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MEDICAL-LEGAL AUTHORITY (ROPE) — Experiences the constraint as pure coordination: reproductive biology provides a stable, easily verifiable basis for category assignment and institutional service delivery. Birth certificate assignment, medical record coding, insurance classification all flow from reproductive anatomy. Net beneficiary: classification authority reduces administrative ambiguity and legal liability. Extraction is minimal from this perspective — the constraint appears as a legitimate coordination mechanism.
constraint_indexing:constraint_classification(biology_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: TRANS RIGHTS COALITION (SCAFFOLD) — Organized agents see the reproductive biology reading as a temporary institutional arrangement with a sunset horizon. Legal gender recognition procedures, identity-based documentation, and hospital admissions policies based on lived gender rather than reproductive status are creating parallel classification pathways that bypass the biology-based bottleneck. Low effective extraction because the coalition has agency, political power, and perceives concrete exit mechanisms emerging over 20-30 years as norms shift toward identity-based classification. The scaffold perspective requires institutional commitment to sunset (legal gender recognition) — without it, the classification collapses to snare.
constraint_indexing:constraint_classification(biology_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, reproductive biology is presented as an immutable natural fact: chromosomes, anatomy, endocrine profiles are fixed at conception and unchangeable by social will. This perspective treats sex category membership as a natural law rather than a contingent institutional reading. However, the structural data contradicts the mountain classification. The constraint has identifiable beneficiaries (medical-legal authority, cis institutional actors), victims (trans and intersex individuals), and requires active enforcement (legal documentation, medical gatekeeping, social sanction). The engine's false summit detector will identify this as naturalization of a contested institutional arrangement, not as a law of physics.
constraint_indexing:constraint_classification(biology_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biology_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biology_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biology_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from trans and intersex individuals through forced legal misclassification that generates ongoing legal vulnerability (bathroom access, sports eligibility, prison assignment, medical decision-making authority). The extraction is not absolute (0.72) because some jurisdictions have created legal gender recognition procedures and identity documents that partially bypass the biological reading. The 0.58 value reflects the current institutional state where reproductive biology reading remains dominant but is experiencing institutional erosion. Suppression (0.72): High. Trans and intersex individuals face severe barriers to exit the constraint: legal documentation barriers (birth certificate change requires legal proceedings, often requiring proof of surgery or medical history); healthcare system friction (some providers refuse to serve trans patients, create unnecessary documentation requirements, or delay care); social sanction (family estrangement, employment discrimination, bathroom confrontation, violence). Intersex individuals face additional suppression through medical gatekeeping (assignment of reproductive anatomy to binary categories often followed by medical intervention without consent). Theater ratio (0.45): Moderate-low. The reproductive biology reading has lower theater than identity-based readings because it relies on ostensibly observable biological fact rather than subjective identity claim. However, theater increases when boundaries are contested: medical classification of intersex conditions requires discretionary judgment; the claim that reproductive anatomy is immutable is undermined by hormone replacement therapy, surgical transition, and biological variation; enforcement requires institutional theater (legal procedures, documentation requirements) beyond what biology alone would demand.
 *
 * PERSPECTIVAL GAP:
 *   The reproductive biology reading generates a sharp perspectival divide. Trans and intersex victims perceive a trap (Snare) with no exit and maximum extraction. Cis women perceive mixed coordination (sex-based harm protection) and extraction (boundary-defense labor) — Tangled Rope. Institutional authorities perceive pure coordination (stable classification) — Rope. Organized trans rights actors perceive a temporary institutional arrangement with emerging exit mechanisms — Scaffold. The analytical observer risks perceiving natural law (Mountain) but the structural data reveals false summit: beneficiaries (institutional actors, male gatekeepers), victims (trans, intersex, autonomy commons), and active enforcement (legal documentation, medical gatekeeping, social sanction) all indicate constructed constraint, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by structural position: reproductive biology reading classifies trans individuals as target beneficiaries of institutional male status they do not want (d ≈ 0.95, full target), intersex individuals as forced participants in binary classification (d ≈ 0.92, near-full target), cis women as partial targets of sex-based harm and partial beneficiaries of category recognition (d ≈ 0.55, mixed), institutional authorities as beneficiaries of classification stability (d ≈ 0.10, beneficiary with arbitrage exit). The sigmoid f(d) maps these d values to effective extraction multipliers. High-d agents (trapped trans/intersex) experience amplified extraction; low-d agents (institutional beneficiaries with arbitrage) experience suppressed extraction. Suppression remains constant at 0.72 across all indices because it is a structural property of the constraint (legal barriers, medical gatekeeping, social sanction) independent of agent position.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the snare classification (for trans/intersex victims) and rope/tangled_rope classifications (for cis women and institutional authorities) are not contradictory — they are legitimate perspectival readings of the same structural data. The mandatrophy is not 'which type is correct?' but 'which perspective reveals the constraint's essential function?' The reproductive biology reading's essential function is institutional classification stability (Rope from the authority perspective) and sex-based harm protection (Tangled Rope from the cis woman perspective). But this function is achieved by imposing uncompensated costs on trans and intersex individuals (Snare). The snare perspective is not an alternative reading but a revelatory perspective: it shows what the rope/tangled_rope perspectives hide. The scaffold perspective (trans rights coalition) projects a sunset: as identity-based legal recognition procedures mature, the biology reading loses functional force, and the snare will collapse. The mountain perspective is a false summit: the reading naturalizes as biology what is actually an institutional choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is sex category membership determined by reproductive biology (this reading), by identity/self-recognition (identity_reading), or by a hybrid framework that uses biology for some purposes and identity for others (hybrid_reading)?',
    'Comparative legal analysis: which criterion courts and administrative agencies apply in different legal contexts (bathroom access, sports eligibility, prisoner housing, medical decision-making, legal name change). Cross-national comparison of how different jurisdictions instantiate the kernel. Temporal tracking of which reading''s institutional salience is increasing or decreasing.',
    'If biology_reading is correct: trans exclusion from legal categories is structurally justified; extractiveness remains ~0.58 (Snare). If identity_reading prevails: biology reading reclassifies as a degraded/inertial constraint (Piton or Mountain fallacy with high theater). If hybrid_reading prevails: extractiveness shifts to ~0.35-0.45 (Tangled Rope with lower asymmetry) because both frameworks maintain some institutional legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which kernel reading (biology, identity, or hybrid) determines sex category membership').

omega_variable(
    reproductive_biology_stability,
    'Is the reproductive anatomy / chromosome configuration that determines category membership truly stable and immutable, or do medical and biological realities show sufficient plasticity to destabilize the biology reading''s foundation?',
    'Biological inventory: What biological markers define ''reproductive biology''? (Chromosomes? Gonads? Genitalia? Hormone production capacity? Secondary sex characteristics?) Do all these markers align in all individuals, or are there frequent misalignments (XY with female anatomy, XX with male anatomy, hormonal conditions producing atypical development)? What percentage of the population has non-standard markers? Does hormone replacement therapy change relevant markers? Does this change the stability claim?',
    'If truly stable and universal: biology reading is valid and snare classification justified. If markers frequently misalign or are plastic: the reading''s foundation is shaky; extractiveness may underestimate the cost of boundary enforcement because the boundary is less natural than claimed. Hybrid reading gains plausibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reproductive_biology_stability, empirical, 'Stability and universality of reproductive biology markers').

omega_variable(
    sex_based_harm_causality,
    'To what degree are documented sex-based harms (reproductive coercion, sexual violence, wage discrimination) caused by biological reproductive difference itself versus caused by social norms, institutional arrangements, and power dynamics that historically used reproductive difference as justification?',
    'Harm-mechanism analysis: disaggregate sex-based harms by causative factor. Which harms require knowledge of reproductive anatomy to occur? (E.g., reproductive coercion requires knowing reproductive capacity; sexual violence of trans women by cis men suggests gender/power dynamics rather than reproductive biology). Which harms persist across sex-diverse populations independent of reproductive anatomy? Comparative study of harm profiles in cultures with different gender category systems.',
    'If reproductive biology is the primary cause: biology reading''s beneficiary claim (cis women need category protection) is justified; tangled_rope classification with strong coordination function. If social dynamics are primary: category membership based on reproductive biology may be addressing the wrong level of causation; the extractiveness from trans/intersex individuals is unjustified overhead; snare classification becomes more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sex_based_harm_causality, conceptual, 'Causal role of reproductive biology in sex-based harms versus social/institutional factors').

omega_variable(
    intersex_category_coherence,
    'If intersex individuals (with atypical reproductive markers) are forced into binary categories, does the biology reading retain internal coherence, or does forced binary classification reveal the reading as an artificial institutional imposition rather than a natural categorization?',
    'Categorical analysis: What happens at the boundary? (1) If intersex individuals are routinely classified into binary categories despite atypical markers, the reading is pragmatically enforced but not logically derived from biology. (2) If medical personnel struggle with classification decisions and use social/legal criteria as tiebreakers, the reading''s biological basis is demonstrably insufficient. (3) If some jurisdictions recognize a third category or identity-based classification for intersex individuals while maintaining biology-based classification for cis populations, the inconsistency reveals the reading as selective naturalization.',
    'If the reading loses coherence at boundaries: the mountain perspective becomes even more clearly a false summit. Theater ratio may increase (more effort required to maintain boundary coherence). Extractiveness from intersex individuals increases because enforcement becomes more arbitrary. Snare classification strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intersex_category_coherence, empirical, 'Internal coherence of biology reading at category boundaries (especially intersex classification)').

omega_variable(
    false_summit_natural_law_vs_constructed,
    'Is this constraint a law of biology (genuinely natural, immutable, requiring no enforcement) or a constructed institutional reading of biology (contingent, enforced, serving institutional interests)?',
    'Comparison test: (1) In societies without state sex category systems, do people naturally organize into reproductive-biology-based categories, or do categories depend on institutional infrastructure? (2) Does the constraint persist without active enforcement (legal documentation, medical gatekeeping, social sanction), or does it collapse if enforcement withdraws? (3) Are there coherent alternative readings of the biological facts that produce different category systems (identity_reading, hybrid_reading)? If (1) no natural emergence, (2) collapse without enforcement, and (3) coherent alternatives exist: this is a constructed institutional arrangement naturalizing itself as biology.',
    'If constructed: the mountain classification is a false summit. Extractiveness remains high (~0.58 Snare) but the mechanism is revealed as institutional power, not natural law. The beneficiary claim becomes more visible: the constraint serves institutional actors'' need for stable classification, not a biological imperative. The victims'' perspective gains credibility as evidence that the reading is chosen, not inevitable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_vs_constructed, conceptual, 'Whether reproductive biology reading is natural law or constructed institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biology_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(biol_tr_t0, biology_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(biol_tr_t5, biology_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(biol_tr_t10, biology_reading, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(biol_be_t0, biology_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(biol_be_t5, biology_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(biol_be_t10, biology_reading, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biology_reading, identity_coordination).
narrative_ontology:affects_constraint(biology_reading, identity_reading).
narrative_ontology:affects_constraint(biology_reading, hybrid_reading).
narrative_ontology:affects_constraint(biology_reading, sports_eligibility_sex_category).
narrative_ontology:affects_constraint(biology_reading, prison_assignment_sex_category).
narrative_ontology:affects_constraint(biology_reading, healthcare_informed_consent).

% DUAL FORMULATION NOTE:
% The sex_gender_category kernel has at least three structurally distinct readings: biology_reading (this file), identity_reading, and hybrid_reading. Each reading has different ε values and beneficiary/victim structures. All three readings are institutionally active in different jurisdictions and legal contexts. This file instantiates biology_reading exclusively. The network edges link to sibling readings and to domain-specific applications of sex category membership (sports, prison, healthcare) that inherit structural properties from whichever kernel reading is institutionally dominant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
