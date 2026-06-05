% ============================================================================
% CONSTRAINT STORY: pathologization_of_poverty_and_marginalization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pathologization_of_poverty_and_marginalization, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pathologization_of_poverty_and_marginalization
 *   human_readable: Pathologization of Poverty and Marginalization
 *   domain: social_policy/structural_inequality
 *
 * SUMMARY:
 *   Pathologization of poverty is a structural constraint that reframes
 *   systemic inequality as individual dysfunction. The constraint operates
 *   across institutional domains — clinical psychiatry, pharmaceutical
 *   markets, criminal justice, social services, and educational systems — to
 *   shift causal attribution from structural (distribution of resources,
 *   access to power, historical injustice) to individual (personal pathology,
 *   defective character, behavioral dysfunction, genetic predisposition).
 *   Over the 70-year measurement interval, the constraint has intensified
 *   significantly: base extractiveness increased from 0.35 to 0.68, and
 *   theater ratio increased from 0.38 to 0.64, indicating that the
 *   performative component has grown relative to any legitimate diagnostic
 *   function. Suppression has risen from 0.48 to 0.62, reflecting expanded
 *   institutional enforcement mechanisms (psychiatric diagnosis expansion,
 *   pharmaceutical prescription, carceral labeling, and credentialing
 *   gatekeeping) that prevent structural attribution from gaining traction.
 *   The constraint is a snare from the perspective of the powerless — it
 *   creates a dual extraction mechanism: (1) resource extraction through
 *   mandatory therapy, medication, and compliance infrastructure paid for by
 *   the impoverished themselves, and (2) epistemic extraction — the capacity
 *   to attribute poverty to structure is suppressed, replaced by pathology
 *   narratives that locate responsibility in the individual. The constraint
 *   benefits a coalition of institutional actors: the privileged class (who
 *   benefit from inequality invisibility), pharmaceutical corporations (who
 *   profit from expanded diagnostic markets), the carceral system (which uses
 *   psychiatric diagnosis to justify incarceration), therapeutic
 *   credentialing bodies (who maintain market gatekeeping through licensure),
 *   and institutional managers (who use pathology frames to avoid structural
 *   accountability). The false summit perspective (mountain classification)
 *   risks naturalizing what is actually a constructed constraint — treating
 *   poverty-adjacent behavioral variations as medical discoveries rather than
 *   as rational adaptations to scarcity or as symptoms of structural
 *   deprivation misdiagnosed as disease.
 *
 * KEY AGENTS:
 *   - Poor and Marginalized Populations: Primary victim (powerless/trapped or identity_locked) — bears extraction through both therapeutic costs and epistemic suppression. Dual bind: required to seek costly remediation for a condition framed as individual pathology, simultaneously prevented from recognizing structural causation.
 *   - Privileged Class: Primary beneficiary (institutional/arbitrage) — benefits from inequality invisibility. Pathology frame allows inequality to persist while appearing scientifically legitimate and morally neutral.
 *   - Pharmaceutical Industry: Secondary beneficiary (institutional/arbitrage) — profits from expanded diagnostic categories, market expansion into poverty-adjacent populations, direct-to-consumer marketing.
 *   - Carceral System: Secondary beneficiary (institutional/arbitrage) — uses psychiatric diagnosis to justify incarceration, criminalize poverty-adjacent behaviors, and expand enforcement capacity.
 *   - Therapeutic Credentialing Bodies: Secondary beneficiary (institutional/arbitrage) — maintain market gatekeeping through licensure requirements tied to diagnostic frameworks; professional identity and income tied to pathology expansion.
 *   - Clinical Psychiatry and Psychology Institutions: Mixed (institutional/constrained) — genuine coordination function (treating actual distress) layered with extraction (market gatekeeping, pharmaceutical alignment, professional interest in expanding diagnoses).
 *   - Structural Critique Coalition: Organized victim (organized/constrained) — social scientists, disability justice advocates, abolitionist movements insisting on structural causation; constrained by institutional gatekeeping and marginalization from policy influence.
 *   - Systemic Inequality Visibility: Abstract victim (powerless/trapped) — the epistemic commons that would recognize structural inequality as the primary causal factor; rendered invisible by pathology framing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pathologization_of_poverty_and_marginalization, 0.68).
domain_priors:suppression_score(pathologization_of_poverty_and_marginalization, 0.62).
domain_priors:theater_ratio(pathologization_of_poverty_and_marginalization, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pathologization_of_poverty_and_marginalization, extractiveness, 0.68).
narrative_ontology:constraint_metric(pathologization_of_poverty_and_marginalization, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(pathologization_of_poverty_and_marginalization, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pathologization_of_poverty_and_marginalization, snare).
narrative_ontology:human_readable(pathologization_of_poverty_and_marginalization, "Pathologization of Poverty and Marginalization").
narrative_ontology:topic_domain(pathologization_of_poverty_and_marginalization, "social_policy/structural_inequality").

domain_priors:requires_active_enforcement(pathologization_of_poverty_and_marginalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pathologization_of_poverty_and_marginalization, privileged_class).
narrative_ontology:constraint_beneficiary(pathologization_of_poverty_and_marginalization, institutional_managers).
narrative_ontology:constraint_beneficiary(pathologization_of_poverty_and_marginalization, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(pathologization_of_poverty_and_marginalization, carceral_system).
narrative_ontology:constraint_beneficiary(pathologization_of_poverty_and_marginalization, therapeutic_credentialing_bodies).
narrative_ontology:constraint_victim(pathologization_of_poverty_and_marginalization, poor_and_marginalized_populations).
narrative_ontology:constraint_victim(pathologization_of_poverty_and_marginalization, systemic_inequality_visibility).
narrative_ontology:constraint_victim(pathologization_of_poverty_and_marginalization, structural_attribution_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PATHOLOGIZED SUBJECT (SNARE) — Trapped in dual bind: structural deprivation attributed to personal defect, then required to seek therapeutic/pharmaceutical remediation at personal cost. No exit without accepting the pathology frame or bearing severe social penalties. Maximum extraction through internalized blame combined with resource extraction (therapy costs, medication, compliance infrastructure).
constraint_indexing:constraint_classification(pathologization_of_poverty_and_marginalization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE MARGINALIZED COMMUNITY (SNARE via identity lock) — Structurally mobile (could exit poverty through collective action or systemic change) but identity-fused with the pathology narrative across generations. Community self-concept becomes constituted through the constraint — 'we are the problem' replaces 'the system is the problem.' Exit would require not just material change but identity reconstruction. High suppression through narrative capture.
constraint_indexing:constraint_classification(pathologization_of_poverty_and_marginalization, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: PRIVILEGED CLASS AND INSTITUTIONAL BENEFICIARIES (ROPE) — Experiences the constraint as coordination: reframing structural inequality as individual pathology solves the collective action problem of maintaining inequality without appearing to cause it. Net beneficiary. Extraction is invisible because framed as scientific/therapeutic necessity rather than extraction.
constraint_indexing:constraint_classification(pathologization_of_poverty_and_marginalization, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THERAPEUTIC/CREDENTIALED PRACTITIONER (TANGLED ROPE) — Constrained by professional licensure and institutional employment requirements; also benefits from the demand generated by pathologization (patients seeking treatment, research funding, therapeutic markets). Genuine coordination function (therapeutic care) layered with extraction (credentialing gatekeeping, market capture). High suppression because practitioners who refuse the pathology frame lose credentials and income.
constraint_indexing:constraint_classification(pathologization_of_poverty_and_marginalization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE CLINICAL DIAGNOSTIC SYSTEM (PITON) — Diagnostic categories (depression, anxiety, behavioral disorder, personality dysfunction) originally designed to identify treatable conditions are now applied to structural deprivation. Theater ratio is high (0.64) because diagnostic rituals (DSM classification, pharmaceutical trials) perform medical legitimacy while obscuring that the 'disorder' is structural poverty. The system maintains itself through institutional inertia — diagnostic frameworks persist despite their poor predictive validity for poverty-adjacent conditions.
constraint_indexing:constraint_classification(pathologization_of_poverty_and_marginalization, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: STRUCTURAL CRITIQUE COALITION (TANGLED ROPE) — Organized agents (social scientists, disability justice advocates, abolitionist movements) reject the pathology frame and insist on structural causation. Constrained by institutional gatekeeping (journal rejection, funding denial, marginalization from mainstream policy). Also coordinating real alternative framings. Sees extraction in the constraint but also real coordination opportunity (building counter-narrative infrastructure).
constraint_indexing:constraint_classification(pathologization_of_poverty_and_marginalization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: NATURALIZED PATHOLOGY VIEW (MOUNTAIN) — From a civilizational perspective, poverty may appear to naturally produce measurable behavioral and psychological variations (stress response, trauma adaptation, time-preference shifts under scarcity). This perspective risks naturalizing what is actually a contingent framing choice — the measurement tools themselves encode pathology assumptions. False summit candidate.
constraint_indexing:constraint_classification(pathologization_of_poverty_and_marginalization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pathologization_of_poverty_and_marginalization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pathologization_of_poverty_and_marginalization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pathologization_of_poverty_and_marginalization, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pathologization_of_poverty_and_marginalization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pathologization_of_poverty_and_marginalization, TR),
    TR >= 0.70.

:- end_tests(pathologization_of_poverty_and_marginalization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The constraint extracts through multiple mechanisms: (1) direct resource extraction via therapy costs, medication, and compliance infrastructure; (2) epistemic extraction via suppression of structural attribution; (3) labor extraction via credentialing requirements and professional gatekeeping; (4) carceral extraction via incarceration justified through psychiatric diagnosis. The 70-year upward trajectory reflects the expansion of psychiatric diagnostics into poverty-adjacent domains and the growth of pharmaceutical and therapeutic markets. Theater ratio (0.64): High and increasing. Diagnostic rituals (DSM classification, pharmaceutical trials, therapy protocols) perform medical legitimacy while the actual mechanism is misattribution of structural deprivation to individual pathology. The rise from 0.38 to 0.64 indicates increasing divergence between the functional purpose (correct diagnosis and treatment) and the actual purpose (justify inequality and extract resources). Suppression (0.62): Moderately high and increasing. The suppression is both external (institutional gatekeeping, credentialing barriers, funding denial for structural research) and internal (identity fusion in marginalized communities, internalized blame, cognitive capture by pathology narratives). The rise from 0.48 to 0.62 reflects intensified enforcement across clinical, carceral, and social service systems.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal and reveals the constraint's extractive character. The powerless victim sees pure snare — a trap in which they are required to accept responsibility for structural deprivation and pay for remediation. The identity-locked marginal community sees snare with cognitive capture — they have become what the constraint says they are. The privileged class and institutional beneficiaries see rope — a coordination mechanism that solves the problem of how to maintain inequality without appearing to cause it. The therapeutic practitioner sees tangled rope — genuine care alongside extraction and market gatekeeping. The credentialing system sees piton — a degraded diagnostic apparatus maintained through institutional inertia despite poor validity for poverty-adjacent conditions. The structural critique coalition sees tangled rope — the same framework that performs pathology also coordinates real alternative analysis, but constrained by institutional marginalization. The naturalized pathology view (mountain) risks treating contingent institutional arrangements as immutable laws of human nature. The perspectival gap between snare (powerless view) and rope (beneficiary view) of the same constraint is the diagnostic signature of the constraint's extractive function.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each perspective are derived from structural position: (1) Powerless/trapped victims have d ≈ 0.95 (full targets of extraction), producing high f(d) ≈ 1.42 and high effective extraction chi despite moderate base extractiveness. (2) Powerless/identity_locked victims have d ≈ 0.89 (high targets but with cognitive capture), producing f(d) ≈ 1.28, lower than trapped but still high. (3) Institutional beneficiaries with arbitrage exit have d ≈ 0.05 (full beneficiaries), producing f(d) ≈ -0.12 and negative chi (they experience the constraint as beneficial). (4) Moderate practitioners with constrained exit have d ≈ 0.55-0.65 (mixed position), producing f(d) ≈ 0.75-1.00 and moderate chi. (5) Organized victims with constrained exit have d ≈ 0.60 (moderate targets with agency), producing f(d) ≈ 0.85. The engine's automatied derivation from beneficiary/victim declarations and exit options produces these values; the commentary documents the structural reasoning. The high d values for powerless agents reflect their lack of exit options and victim status; the low d values for institutional beneficiaries reflect their arbitrage options and beneficiary status. The perspectival gap (snare from victim perspective, rope from beneficiary perspective) emerges directly from this directionality structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates the mandatrophy at high extractiveness (0.68 > 0.46, requiring omegas and measurements; 0.68 < 0.70, not requiring mandatrophy_resolved). The constraint cannot be classified as pure rope (coordination) because beneficiary and victim declarations exist and are asymmetric — genuine coordination function does not exist independently of the extraction mechanism. The beneficiary class benefits precisely because the pathology frame obscures inequality; there is no coordination benefit to them from the pathology narrative itself, only extractive benefit. However, the constraint cannot be classified as pure snare without acknowledging that therapeutic practitioners provide real care within the constraint's frame, and that some individuals benefit from psychiatric treatment despite the broader extractive function of pathologization. The tangled rope classification for practitioners and the piton classification for the diagnostic system capture these nuances. The constraint is best described as a snare with elaborate institutional scaffolding (tangled rope at institutional level, piton for degraded diagnosis) that makes the pure extraction appear as legitimate medical science. The mandatrophy is resolved through perspectival differentiation: the constraint is a snare from the powerless victim's structural position, rope from the beneficiary's position, and exhibits piton degradation in its diagnostic apparatus. No single type captures the full structure; the perspectival field itself is the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_individual_attribution,
    'Where does causal responsibility truly lie: in structural distribution of resources and power, or in individual behavioral/psychological differences that correlate with poverty?',
    'Intervention studies: do systemic resource redistribution (UBI, housing-first programs) reduce measured ''pathology'' markers without therapeutic intervention? Cross-national comparison of poverty-adjacent symptoms in countries with vs without inequality redistribution. Longitudinal tracking of individuals moving between structural contexts.',
    'If structural: the constraint is a pure extraction mechanism disguised as diagnosis. Reclassify mountain to snare, piton to snare. If individual: some pathology classification is defensible, though the distribution mechanism may still be extractive. If both: tangled rope is correct classification — genuine disorder exists but is amplified by structural deprivation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_vs_individual_attribution, empirical, 'Causal attribution: structure vs individual in poverty-adjacent pathology').

omega_variable(
    diagnostic_measurement_validity,
    'Do clinical diagnostic instruments (DSM categories, psychological screening tools) measure pathology or measure structural deprivation? Are the same symptoms considered normal in wealthy contexts and pathological in poor contexts?',
    'Cross-cultural comparison of symptom interpretation. Study of symptom expression and clinician diagnosis when demographic markers (poverty signals) are concealed. Historical analysis of which symptoms were diagnosed as pathological in different socioeconomic contexts.',
    'If measurement is context-insensitive: diagnostics are valid across contexts, some pathology classification is legitimate. If context-sensitive: the same symptoms are pathologized when associated with poverty and normalized when associated with wealth — the instruments measure social position, not disease. Theater ratio would increase; snare classification would be confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diagnostic_measurement_validity, empirical, 'Whether diagnostic instruments measure pathology or encode structural position').

omega_variable(
    therapeutic_outcome_differential,
    'Do therapeutic interventions (psychotherapy, medication, behavioral coaching) show differential efficacy depending on whether structural deprivation is simultaneously addressed?',
    'Meta-analysis of therapy outcome studies stratified by income level. Comparison of therapy outcomes in populations with vs without concurrent income support, housing stability, or access to power. RCT comparing therapy-alone vs therapy-plus-resource-access.',
    'If therapy works equally without structural change: individual pathology is at least partially independent of structure. If therapy efficacy is negligible without structural change: the constraint''s extraction mechanism is confirmed — therapy is theater, and structural change is the actual requirement. High-confidence evidence would raise extractiveness estimate and increase theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(therapeutic_outcome_differential, empirical, 'Whether therapy efficacy depends on concurrent structural resource access').

omega_variable(
    historical_contingency_of_pathology_categories,
    'Are current diagnostic categories timeless medical discoveries or historically contingent social constructs that serve institutional interests?',
    'Genealogical analysis: trace the emergence of specific diagnoses (depression, anxiety, ADHD, personality disorder) alongside shifts in poverty policy, pharmaceutical markets, and institutional professionalization. Identify material interests that benefited from category adoption.',
    'If historical contingency is demonstrated: the pathology frame is a constructed constraint, not a natural law. The constraint becomes a clear snare with institutional beneficiaries (pharma, therapeutics, credentialing bodies) and powerless victims. If categories are timeless: some natural law element exists, though institutional deployment may still be extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_contingency_of_pathology_categories, conceptual, 'Historical emergence and material interests in diagnostic category adoption').

omega_variable(
    identity_lock_mechanism_in_marginalized_communities,
    'To what degree is the marginalized community''s inability to exit the pathology frame due to external suppression (barriers to structural change) versus internal identity fusion (community self-concept constituted through the pathology narrative)?',
    'Study of community self-narratives and identity work. Analysis of resistance movements: when structural barriers are partially lifted (policy change, resource access), do communities quickly adopt structural attribution, or does identity-lock require additional cognitive/narrative work? Longitudinal tracking of identity shift following structural interventions.',
    'If structural suppression dominates: the constraint is snare (powerless/trapped). If identity lock dominates: the constraint is snare (powerless/identity_locked), requiring different intervention strategies (narrative reframing, identity work, not just material redistribution). If both: structural and psychological dimensions require simultaneous intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_marginalized_communities, empirical, 'Relative weight of external suppression vs internal identity fusion in constraint persistence').

omega_variable(
    pharmaceutical_industry_incentive_alignment,
    'To what degree does pharmaceutical industry profit motive align with pathologization of poverty? Do industry funding, marketing, and diagnostic expansion serve to maximize profit or to serve patient welfare?',
    'Analysis of pharmaceutical company funding flows to diagnostic research, psychiatric institutions, and patient advocacy. Correlation between diagnostic category expansion and pharmaceutical market development. Study of countries with vs without pharmaceutical industry influence on psychiatric standards.',
    'If strong alignment: pathologization serves identifiable extraction beneficiaries (pharma) at the expense of powerless victims. Beneficiary list should expand to explicitly include pharmaceutical_industry. If weak alignment: industry plays a supporting role but is not the primary driver. Theater ratio may remain as-is or increase.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pharmaceutical_industry_incentive_alignment, empirical, 'Pharmaceutical industry incentive alignment with pathology expansion').

omega_variable(
    carceral_system_pathology_coupling,
    'How does pathologization of poverty serve the carceral system''s extraction and expansion? Are behavioral/psychiatric diagnoses used to justify criminalizing poverty?',
    'Analysis of diagnostic rates in criminal justice populations vs general population. Study of how psychiatric diagnoses are used in sentencing and incarceration justification. Comparison of incarceration rates with pathology diagnosis rates over time.',
    'If strong coupling: the carceral system is an explicit beneficiary of pathologization. Constraint extraction flows to both therapeutic and punitive institutions. Suppression increases (multiple enforcement mechanisms). If weak coupling: carceral use of diagnosis is secondary. Theater ratio and suppression estimates may shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carceral_system_pathology_coupling, empirical, 'Carceral system coupling to pathology narratives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pathologization_of_poverty_and_marginalization, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pathol_tr_t0, pathologization_of_poverty_and_marginalization, theater_ratio, 0, 0.38).
narrative_ontology:measurement(pathol_tr_t14, pathologization_of_poverty_and_marginalization, theater_ratio, 14, 0.42).
narrative_ontology:measurement(pathol_tr_t28, pathologization_of_poverty_and_marginalization, theater_ratio, 28, 0.48).
narrative_ontology:measurement(pathol_tr_t42, pathologization_of_poverty_and_marginalization, theater_ratio, 42, 0.54).
narrative_ontology:measurement(pathol_tr_t56, pathologization_of_poverty_and_marginalization, theater_ratio, 56, 0.6).
narrative_ontology:measurement(pathol_tr_t70, pathologization_of_poverty_and_marginalization, theater_ratio, 70, 0.64).

% Extraction over time
narrative_ontology:measurement(pathol_be_t0, pathologization_of_poverty_and_marginalization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pathol_be_t14, pathologization_of_poverty_and_marginalization, base_extractiveness, 14, 0.42).
narrative_ontology:measurement(pathol_be_t28, pathologization_of_poverty_and_marginalization, base_extractiveness, 28, 0.48).
narrative_ontology:measurement(pathol_be_t42, pathologization_of_poverty_and_marginalization, base_extractiveness, 42, 0.58).
narrative_ontology:measurement(pathol_be_t56, pathologization_of_poverty_and_marginalization, base_extractiveness, 56, 0.64).
narrative_ontology:measurement(pathol_be_t70, pathologization_of_poverty_and_marginalization, base_extractiveness, 70, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(pathol_su_t0, pathologization_of_poverty_and_marginalization, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(pathol_su_t35, pathologization_of_poverty_and_marginalization, suppression_requirement, 35, 0.55).
narrative_ontology:measurement(pathol_su_t70, pathologization_of_poverty_and_marginalization, suppression_requirement, 70, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pathologization_of_poverty_and_marginalization, identity_coordination).
narrative_ontology:affects_constraint(pathologization_of_poverty_and_marginalization, carceral_labeling_and_psychiatric_justification).
narrative_ontology:affects_constraint(pathologization_of_poverty_and_marginalization, pharmaceutical_market_expansion_in_poverty_populations).
narrative_ontology:affects_constraint(pathologization_of_poverty_and_marginalization, therapeutic_credentialing_gatekeeping).
narrative_ontology:affects_constraint(pathologization_of_poverty_and_marginalization, stigma_driven_exit_barrier_amplification).

% DUAL FORMULATION NOTE:
% The pathologization constraint is upstream of several domain-specific extraction mechanisms: carceral systems use psychiatric diagnosis to justify incarceration; pharmaceutical markets expand diagnostic categories to increase drug consumption; credentialing bodies maintain professional gatekeeping through diagnostic alignment; stigma systems amplify psychological barriers to structural change. Each downstream constraint has its own extractiveness value and perspectival structure, but all depend on the pathologization frame that treats poverty as individual pathology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pathologization_of_poverty_and_marginalization, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
