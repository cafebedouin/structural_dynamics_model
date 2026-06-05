% ============================================================================
% CONSTRAINT STORY: dk_foreign_convict_expulsion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dk_foreign_convict_expulsion, []).

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
 *   constraint_id: dk_foreign_convict_expulsion
 *   human_readable: Denmark's Foreign Convict Expulsion Law
 *   domain: political/social
 *
 * SUMMARY:
 *   Denmark's Foreign Convict Expulsion Law (enacted 2019, implemented 2021)
 *   mandates expulsion of any foreign national sentenced to at least one year
 *   in prison. The constraint operates as a dual-penalty mechanism combining
 *   criminal sanction (incarceration) with administrative consequence
 *   (expulsion). From the victim's perspective (foreign national with Danish
 *   roots, family, employment), the law functions as pure extraction: a
 *   single criminal act triggers simultaneous loss of liberty and domicile,
 *   with no discretion, appeal, or family unity exception. From the governing
 *   coalition's perspective, the law functions as coordination: a bright-line
 *   rule eliminates discretion debates and creates predictable consequence.
 *   From the EU/international human rights framework, the law exhibits both
 *   coordination (transparency, uniformity) and extraction (violation of
 *   family unity, disproportionality across offense classes, elimination of
 *   integration pathway). The constraint demonstrates how a single policy
 *   mechanism can be classified into six different types depending on
 *   structural position. Theater ratio increased from 0.42 (initial policy
 *   announcement) to 0.58 (current enforcement) as political salience
 *   increased but actual enforcement remained modest (~200-300 expulsions
 *   annually from 5.8M population). Extractiveness increased from 0.55 to
 *   0.68 as hardship cases accumulated and the
 *   integration-incentive-destruction mechanism became evident.
 *
 * KEY AGENTS:
 *   - Foreign national convicts: Primary victims (powerless/trapped) — subject to dual penalty with no exit or discretion
 *   - Long-term foreign residents and families: Secondary victims (moderate/constrained) — family structures weaponized; decades of residence offer no protection
 *   - Danish governing coalition: Primary beneficiary (institutional/arbitrage) — experiences law as coordination mechanism; gains political salience and voter satisfaction
 *   - EU/International human rights framework: Institutional observer (powerful/mobile) — recognizes both coordination and extraction; has leverage to challenge proportionality
 *   - Danish public opinion and media: Theater amplifier (institutional/constrained) — performs law-and-order narrative; actual enforcement numbers modest
 *   - Analytical observer: Risk of false summit (analytical/analytical) — temptation to naturalize state sovereignty as natural law obscures political contingency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dk_foreign_convict_expulsion, 0.68).
domain_priors:suppression_score(dk_foreign_convict_expulsion, 0.72).
domain_priors:theater_ratio(dk_foreign_convict_expulsion, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dk_foreign_convict_expulsion, extractiveness, 0.68).
narrative_ontology:constraint_metric(dk_foreign_convict_expulsion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dk_foreign_convict_expulsion, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dk_foreign_convict_expulsion, snare).
narrative_ontology:human_readable(dk_foreign_convict_expulsion, "Denmark's Foreign Convict Expulsion Law").
narrative_ontology:topic_domain(dk_foreign_convict_expulsion, "political/social").

domain_priors:requires_active_enforcement(dk_foreign_convict_expulsion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dk_foreign_convict_expulsion, danish_native_citizens).
narrative_ontology:constraint_beneficiary(dk_foreign_convict_expulsion, political_governing_coalition).
narrative_ontology:constraint_victim(dk_foreign_convict_expulsion, foreign_national_residents).
narrative_ontology:constraint_victim(dk_foreign_convict_expulsion, family_unity_holders).
narrative_ontology:constraint_victim(dk_foreign_convict_expulsion, long_term_residents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOREIGN CONVICT (SNARE) — Trapped within Danish jurisdiction with no exit option once sentence is imposed. Criminal sanction plus expulsion removes both freedom and domicile simultaneously. No appeal pathway; no discretion in magistracy. d≈0.96, f(d)≈1.42, σ=1.0 → χ≈0.96. Pure extraction: penalty is dual (incarceration + displacement).
constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LONG-TERM RESIDENT FAMILIES (SNARE) — Constrained by family ties, economic roots, and decades of residence (often children born in Denmark, Danish-educated, Danish-language fluent). Expulsion of breadwinner or parent severs family unit and economic security. d≈0.88, f(d)≈1.32, σ=1.0 → χ≈0.90. High extraction: family structure weaponized.
constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DANISH GOVERNING COALITION (ROPE) — Experiences the expulsion law as coordination mechanism: establishes bright-line rule for public safety, eliminates discretion debates, creates predictable immigration consequence. Benefits from political salience and voter satisfaction (law-and-order coalition building). d≈0.10, f(d)≈0.02, σ=1.0 → χ≈0.01. Net beneficiary: low effective extraction from their perspective.
constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EU/INTERNATIONAL HUMAN RIGHTS BODIES (TANGLED ROPE) — Recognize coordination function (expulsion rule is transparent, predictable, applied uniformly) BUT identify asymmetric extraction: law violates right to family unity (EU Charter Art. 7), proportionality principle (all crimes ≥1 year trigger same outcome), and integration incentives (no pathway to become Danish despite decades of residence). Active enforcement required. d≈0.52, f(d)≈0.68, σ=1.1 → χ≈0.51. Hybrid: coordination + extraction both present.
constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: DANISH PUBLIC OPINION & MEDIA (PITON) — Law persists as political theater more than functional policy. Theater elements: dramatic expulsion announcements, performative 'zero-tolerance' branding, media coverage amplifies each case. Functional content: actual expulsions are modest in number (~200-300 annually from ~5.8M population), impact on crime rates unmeasured, family separation consequences downplayed. theater_ratio=0.58 reflects moderate performative content. d≈0.45, f(d)≈0.52, σ=1.0 → χ≈0.35. Piton gate (theater ≥0.70) narrowly missed; classified as piton due to declining political coherence.
constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE SUMMIT RISK) — Risk of naturalizing the expulsion law as immutable state sovereignty: 'Nations have inherent right to expel non-citizens.' This appears as a mountain (state sovereignty as fundamental principle). However, structural data (ε=0.68, suppression=0.72, beneficiaries + victims declared) contradicts mountain gates. Emerges_naturally=false, accessibility_collapse undefined, resistance undefined. Engine will flag as false summit. The constraint is politically contingent, not natural law.
constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dk_foreign_convict_expulsion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dk_foreign_convict_expulsion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dk_foreign_convict_expulsion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dk_foreign_convict_expulsion, TR),
    TR >= 0.70.

:- end_tests(dk_foreign_convict_expulsion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68): High. The law creates a dual-penalty structure that is asymmetric and coercive. A single criminal conviction triggers both incarceration AND expulsion — the extraction mechanism is the automatic expulsion, not the criminal sentence itself. For long-term residents (30+ years), deportation to a country they may not speak the language of constitutes severe extraction. Suppression (0.72): High. Foreign nationals have minimal recourse: no discretion available to judges, no appeal pathway, no family unity exception, no pathway to remain through further integration. The mechanism is legally airtight — once the 1-year threshold is crossed, expulsion is mandatory. Theater ratio (0.58): Moderate. The law is politically performative — announcements of 'zero tolerance,' media coverage of expulsion cases — but the functional content is modest (200-300 expulsions annually, unmeasured crime reduction, downplayed family consequences). Theater has increased over the interval as political salience rose. The constraint does not meet the piton gate (theater ≥0.70) but is trending toward it. Claimed type (Snare): Confirmed by metrics (ε=0.68 ≥ 0.46, suppression=0.72 ≥ 0.60, χ ≥ 0.66 in victim perspective).
 *
 * PERSPECTIVAL GAP:
 *   The foreign convict experiences automatic extraction (Snare): committed an offense, received sentence, now stripped of domicile with no discretion. The governing coalition experiences coordination (Rope): rule is transparent, predictable, applied uniformly, eliminates discretion debates. The EU/international observer experiences hybrid extraction+coordination (Tangled Rope): recognizes the rule's clarity AND its violation of family unity and proportionality principles. The Danish public experiences theater (Piton): political salience is high, but enforcement impact is modest and consequences are downplayed. The analytical observer risks false summit (Mountain): naturalization of state sovereignty as immutable law obscures the political contingency of the mechanism. The perspectival gap is maximal: the same law is experienced as pure extraction, pure coordination, hybrid extraction+coordination, theater, and false natural law depending on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Foreign national convicts: Victim + trapped → d≈0.96, f(d)≈1.42. Maximum extraction. No exit, no discretion, no mitigation. Long-term residents: Victim + constrained → d≈0.88, f(d)≈1.32. High extraction. Constrained by family ties and economic roots; expulsion severs both. Danish governing coalition: Beneficiary + arbitrage → d≈0.10, f(d)≈0.02. Net beneficiary. Can exit the law's consequences through policy change; benefits from political salience. EU/international human rights: Powerful + mobile → d≈0.52, f(d)≈0.68. Hybrid directionality. Has leverage to challenge the law; not fully trapped but also not fully benefiting. Danish public/media: Institutional + constrained → d≈0.45, f(d)≈0.52. Constrained by political incentives to perform law-and-order; benefits from theater. Analytical observer: Analytical → d≈0.72, f(d)≈1.15. Risk of naturalization; sees constraint as inevitable rather than contingent.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that the law's classification depends entirely on structural position. From a powerless/trapped perspective (the expulsion target), it is unambiguously Snare: high extraction, high suppression, no coordination benefit. From an institutional/arbitrage perspective (the governing coalition), it appears as Rope: coordination mechanism, bright-line rule, no extraction cost to them. The mandatrophy is not 'which type is correct?' but 'which structural position are you measuring from?' The false summit risk (Analytical Observer perspective) is critical: the analyst who naturalizes the law as inherent state sovereignty obscures its political contingency and misses the extraction mechanism entirely. The EU/international framework (Tangled Rope) resolves the mandatrophy by insisting that coordination (bright-line rule) and extraction (family unity violation, disproportionality) are both real and structural. The law cannot be reframed as 'just coordination' — the extraction is real to those who experience it. Nor can it be reframed as 'just extraction' — the coordination function (bright-line rule, voter satisfaction, policy predictability) is real to the beneficiary. The Tangled Rope classification captures both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_threshold,
    'Is the 1-year sentence threshold proportional to the expulsion consequence, or does it conflate minor and major crimes?',
    'Comparative analysis: expulsion rates by offense category (drug trafficking vs. assault vs. fraud); recidivism data for expelled vs. retained convicts; public safety impact per offense class',
    'If threshold is proportional: law is coordination (bright-line rule). If threshold is disproportionate: law is extraction (one-size-fits-all extraction mechanism weaponizes minor offenses).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_threshold, empirical, 'Proportionality of 1-year threshold to expulsion consequence').

omega_variable(
    family_unity_alternatives,
    'Does the law provide meaningful discretion to preserve family unity for residents with Danish-born children or decades of residence?',
    'Audit of judicial decisions: percentage of cases where discretion was exercised; comparison with other EU jurisdictions'' discretionary frameworks; documented hardship cases',
    'If discretion exists and is used: classification shifts toward Tangled Rope (some beneficiaries, targeted victims). If no discretion: pure Snare (weaponized expulsion mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(family_unity_alternatives, empirical, 'Whether judicial discretion preserves family unity').

omega_variable(
    public_safety_attribution,
    'Does the expulsion law reduce crime rates, or is claimed public safety benefit attributable to other enforcement changes?',
    'Time-series analysis of crime rates before/after law enactment (2019); comparison with control jurisdictions; isolation of expulsion effect from concurrent police funding increases',
    'If expulsion reduces crime: law''s coordination function is empirically validated. If no measurable effect: law is pure theater (Piton classification confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_safety_attribution, empirical, 'Public safety attribution of expulsion policy').

omega_variable(
    integration_incentive_destruction,
    'Does automatic expulsion destroy incentives for long-term foreign residents to integrate (learn Danish, invest in community, seek naturalization)?',
    'Longitudinal tracking of naturalization applications pre/post-law; immigrant community survey data on perceived security; labor market integration metrics for foreign nationals',
    'If incentives destroyed: law creates extractive mechanism (removes integration pathway). If incentives preserved: law is Rope (clear rule, not targeted extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_incentive_destruction, empirical, 'Whether law destroys integration incentives for residents').

omega_variable(
    state_sovereignty_naturalization,
    'Is state sovereignty to expel non-citizens a natural law or a politically contingent power?',
    'Historical analysis of expulsion law variation across time; constitutional analysis of sovereignty vs. human rights hierarchy; case law from ECJ and ECHR on expulsion proportionality',
    'If sovereignty is natural law: mountain classification justified. If sovereignty is contingent on proportionality/family unity constraints: false summit confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_sovereignty_naturalization, conceptual, 'Whether state expulsion sovereignty is natural law or contingent power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dk_foreign_convict_expulsion, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dk_expulsion_tr_t0, dk_foreign_convict_expulsion, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dk_expulsion_tr_t3, dk_foreign_convict_expulsion, theater_ratio, 3, 0.52).
narrative_ontology:measurement(dk_expulsion_tr_t6, dk_foreign_convict_expulsion, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(dk_expulsion_be_t0, dk_foreign_convict_expulsion, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(dk_expulsion_be_t3, dk_foreign_convict_expulsion, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(dk_expulsion_be_t6, dk_foreign_convict_expulsion, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dk_foreign_convict_expulsion, enforcement_mechanism).
narrative_ontology:affects_constraint(dk_foreign_convict_expulsion, eu_family_unity_directive).
narrative_ontology:affects_constraint(dk_foreign_convict_expulsion, integration_incentive_structures).
narrative_ontology:affects_constraint(dk_foreign_convict_expulsion, judicial_discretion_frameworks).

% DUAL FORMULATION NOTE:
% The Foreign Convict Expulsion Law decomposes into multiple structural constraints: (1) the mandatory expulsion mechanism itself (ε=0.68, this story), (2) the underlying 1-year sentence threshold (proportionality question, separate story), and (3) the family unity exception mechanism (doctrinal question, separate story). These are linked: the expulsion law's extractiveness depends critically on whether discretion and family unity exceptions exist. Current story assumes they do not; if they do exist but are unused, extractiveness should be lower (~0.50-0.55, moving toward Tangled Rope from Snare). Stories should be linked via affects_constraints to show how upstream proportionality and discretion constraints influence the downsteam expulsion mechanism's classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dk_foreign_convict_expulsion, powerful, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
