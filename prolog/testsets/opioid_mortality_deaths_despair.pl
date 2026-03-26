% ============================================================================
% CONSTRAINT STORY: opioid_mortality_deaths_despair
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_opioid_mortality_deaths_despair, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: opioid_mortality_deaths_despair
 *   human_readable: Opioid Mortality Deaths Despair Cycle
 *   domain: public_health/economic/social
 *
 * SUMMARY:
 *   The opioid mortality crisis represents a structural extraction mechanism
 *   operating across pharmaceutical, regulatory, treatment, and individual
 *   behavioral levels. What begins as pharmaceutical marketing of pain relief
 *   becomes a multi-generational trap of addiction, despair, and premature
 *   mortality. The constraint exhibits characteristics of pure snare: high
 *   extractiveness (0.68) reflecting the asymmetric mortality burden on
 *   dependent individuals and working-class communities; high suppression
 *   (0.72) through the combination of physical dependence, economic
 *   immobility, regulatory barriers to treatment, and social stigma; and
 *   moderate theater (0.58) reflecting the disconnect between regulatory
 *   messaging about addiction risk and actual pharmaceutical marketing,
 *   between treatment sector claims of helping and financial incentives for
 *   sustained dependency, and between public health framing and actual policy
 *   outcomes. Extractiveness has increased from 0.35 to 0.68 over the 15-year
 *   interval as the epidemic matured: early phases focused on pharmaceutical
 *   profit capture during the marketing expansion; later phases show
 *   extraction consolidation as despair deepens, community bonds dissolve,
 *   and dependent individuals become trapped in cycles of overdose,
 *   treatment, relapse, and death. Theater has increased from 0.42 to 0.65 as
 *   regulatory responses have grown more performative — regulatory actions,
 *   warning labels, prescription monitoring, and treatment expansion efforts
 *   increase in visibility while mortality continues to climb, indicating the
 *   theater is substituting for functional intervention.
 *
 * KEY AGENTS:
 *   - Opioid-dependent individuals: Primary victims (powerless/trapped) — bears maximum extraction through overdose risk, withdrawal pain, criminalization, and economic devastation
 *   - Working-class communities: Primary victims (powerless/trapped) — geographically and economically immobilized communities where opioid use becomes normalized as pain response to precarity
 *   - Pharmaceutical manufacturers: Primary beneficiaries (institutional/arbitrage) — billions in revenue from opioid sales during addiction expansion; arbitrage options available to shift products, exit markets, or lobby regulatory change
 *   - Opioid distributors and pharmaceutical middlemen: Secondary beneficiaries (institutional/arbitrage) — profit from supply chain expansion and logistical coordination of opioid distribution
 *   - Treatment provision sector: Mixed victim-beneficiary (moderate/constrained) — genuine coordination function (MAT saves lives) but incentive structures reward sustained clientele rather than permanent recovery; constrained by insurance, regulation, and resource availability
 *   - Regulatory agencies (DEA, FDA, state boards): Degraded institutional actors (institutional/constrained) — theater performers maintaining form while substance atrophies; enforcement actions ineffective, warning labels disregarded, pharmaceutical lobbying dominates policy
 *   - Family networks of affected individuals: Secondary victims (moderate/trapped) — absorbed into caregiving burden, economic support, and grief from mortality; often trapped by relational dependence on affected members
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(opioid_mortality_deaths_despair, 0.68).
domain_priors:suppression_score(opioid_mortality_deaths_despair, 0.72).
domain_priors:theater_ratio(opioid_mortality_deaths_despair, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(opioid_mortality_deaths_despair, extractiveness, 0.68).
narrative_ontology:constraint_metric(opioid_mortality_deaths_despair, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(opioid_mortality_deaths_despair, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(opioid_mortality_deaths_despair, snare).
narrative_ontology:human_readable(opioid_mortality_deaths_despair, "Opioid Mortality Deaths Despair Cycle").
narrative_ontology:topic_domain(opioid_mortality_deaths_despair, "public_health/economic/social").

domain_priors:requires_active_enforcement(opioid_mortality_deaths_despair).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(opioid_mortality_deaths_despair, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(opioid_mortality_deaths_despair, opioid_distributors).
narrative_ontology:constraint_beneficiary(opioid_mortality_deaths_despair, addiction_treatment_providers).
narrative_ontology:constraint_victim(opioid_mortality_deaths_despair, opioid_dependent_individuals).
narrative_ontology:constraint_victim(opioid_mortality_deaths_despair, working_class_communities).
narrative_ontology:constraint_victim(opioid_mortality_deaths_despair, public_health_systems).
narrative_ontology:constraint_victim(opioid_mortality_deaths_despair, family_networks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPIOID-DEPENDENT INDIVIDUAL (SNARE) — Trapped by physical dependence, economic destitution, and limited access to evidence-based treatment. No exit available. Maximum suppression through withdrawal pain, social stigma, criminalization, and economic barriers to treatment. Bears full cost of the constraint while experiencing maximal extraction.
constraint_indexing:constraint_classification(opioid_mortality_deaths_despair, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKING-CLASS COMMUNITIES (SNARE) — Structurally trapped by deindustrialization, geographic immobility, economic desperation, and social disintegration. The constraint extracts through mortality, despair, and social fragmentation. Suppression operates through limited economic opportunity, inadequate healthcare access, and normalization of opioid use as pain management in communities lacking alternatives. No exit mechanism for the community collective.
constraint_indexing:constraint_classification(opioid_mortality_deaths_despair, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: ADDICTION TREATMENT PROVIDERS (TANGLED ROPE) — Moderate power constrained by regulatory barriers, insurance reimbursement structures, and resource scarcity. Genuine coordination function: medication-assisted treatment (MAT) and behavioral interventions save lives. Simultaneous extraction: treatment sector benefits from sustained demand, incentive structures reward continuous client enrollment rather than permanent recovery, and insurance billing perpetuates client dependency on institutional support. Active enforcement through licensing, DEA regulation, and insurance networks maintains the arrangement.
constraint_indexing:constraint_classification(opioid_mortality_deaths_despair, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PHARMACEUTICAL MANUFACTURERS AND DISTRIBUTORS (ROPE) — Institutional actors with maximum arbitrage options. Frame the constraint as a coordination mechanism: pharmaceutical supply meets legitimate pain management demand. The beneficiary perspective is self-evident — opioid manufacturers and their distributors capture billions in revenue during the addiction cycle. From their framing, the constraint coordinates pain relief with economic incentive, positioning extraction as merely appropriate compensation for managing a difficult medical problem.
constraint_indexing:constraint_classification(opioid_mortality_deaths_despair, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY-INDUSTRIAL COMPLEX (PITON) — Degraded institutional mechanism where regulatory oversight (DEA, FDA) has atrophied in its enforcement role while maintaining performative regulatory theater. The regulatory apparatus continues operations (scheduling meetings, enforcement actions, compliance monitoring) but fails to prevent the extraction mechanism. Theater ratio is elevated: enforcement actions against manufacturers are infrequent and largely ineffective; warning labels persist despite known inefficacy; prescription monitoring networks exist but lack coordination; regulatory messaging about addiction risk contradicts pharmaceutical marketing. The constraint persists through institutional inertia despite acknowledged dysfunction.
constraint_indexing:constraint_classification(opioid_mortality_deaths_despair, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZED DESPAIR (MOUNTAIN) — From a civilizational view, some argue opioid despair is a natural law: economic desperation + effective pain medication + addictive pharmacology = inevitable mortality cascade. This perspective naturalizes the constraint as an immutable consequence of human neurobiology and economic inequality. However, this is a false summit. The base properties reveal a contingent institutional arrangement (pharmaceutical incentives, regulatory capture, treatment access asymmetry) rather than a law of nature. The engine's false summit detector identifies the naturalization as misclassification.
constraint_indexing:constraint_classification(opioid_mortality_deaths_despair, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(opioid_mortality_deaths_despair_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(opioid_mortality_deaths_despair, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(opioid_mortality_deaths_despair, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(opioid_mortality_deaths_despair, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(opioid_mortality_deaths_despair, TR),
    TR >= 0.70.

:- end_tests(opioid_mortality_deaths_despair_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The constraint generates asymmetric value extraction: pharmaceutical companies and distributors capture billions in revenue; treatment providers capture billions in insurance/government payment while maintaining incentives for sustained enrollment; dependent individuals and communities bear costs in mortality, despair, and community disintegration. The trajectory from 0.35 to 0.68 reflects the maturation of the extraction mechanism — early phases rely on growth (new market expansion); later phases rely on deepening (addiction consolidation in trapped populations). Suppression (0.72): High. Multiple suppression mechanisms operate simultaneously: physical dependence creates biological barrier to exit; economic desperation makes opioids attractive compared to unavailable alternatives; geographic immobility traps communities in locales with limited treatment access and high opioid availability; criminalization adds legal barriers; social stigma creates psychological barriers; insurance and regulatory structures create access barriers to evidence-based treatment. Treatment access is restricted not by clinical contraindication but by policy barriers (DEA opioid antagonist requirements, Medicaid restrictions, geographic treatment deserts). Theater ratio (0.58): Moderate and increasing. Regulatory theater includes prescription monitoring programs (PMPs) that track but do not prevent opioid distribution; DEA enforcement actions that are infrequent relative to opioid volume; FDA warning labels that fail to reduce prescribing; treatment expansion mandates that provide access to inadequate/ineffective modalities; harm reduction theater that substitutes needle distribution for supply reduction. Marketing theater from treatment sector includes promotion of MAT as solution despite evidence showing MAT requires complementary social support (housing, employment, community reintegration) that remains unavailable.
 *
 * PERSPECTIVAL GAP:
 *   The snare classification is robust across the two victim perspectives (dependent individual and community). Both perceive maximum, unchangeable extraction with no coordination benefit and no exit option. The treatment sector's tangled rope is structurally accurate — they provide lifesaving medication-assisted treatment (coordination function) while operating within incentive structures that reward sustained enrollment rather than recovery completion (extraction function). The pharmaceutical beneficiary rope perspective claims coordination but is contradicted by empirical evidence of marketing deception, targeting non-pain populations, and knowing suppression of addiction risk data. The regulatory piton classification reveals that the institutional mechanism for constraint prevention (FDA oversight, DEA enforcement) has degraded into performative theater while maintaining the formal apparatus. The analytical mountain perspective (naturalizing opioid despair as inherent to human neurobiology) is a false summit — the constraint is contingent on regulatory policy, pharmaceutical marketing rules, treatment access structures, and economic precarity, not on immutable laws of nature. Countries with identical opioid pharmacology but different policy frameworks show mortality differences of 10-40x, which invalidates the mountain naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status and exit options. Dependent individuals are victims with trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 (maximum experienced extraction). Working-class communities are victims with trapped exit → d ≈ 0.95 → similar maximum extraction. Treatment providers are mixed: moderate power with constrained exit, benefiting from demand but also providing genuine care → d ≈ 0.50-0.60 → f(d) ≈ 0.65-0.85 (experienced as mixed). Pharmaceutical manufacturers are beneficiaries with arbitrage exit → d ≈ 0.10 → f(d) ≈ -0.01 (negative or minimal experienced extraction; extraction flows toward them). Regulators claim institutional power but face constrained exits due to industry capture and resource limitations → d ≈ 0.35-0.45 (moderate extraction if viewing regulation as a constraint they bear) or d ≈ 0.15 if viewing them as beneficiary-adjacent due to regulatory theater self-interest. Scope modifier σ(S) = 1.0 (national scope), so χ = ε × f(d) × 1.0. For dependent individuals: χ = 0.68 × 1.42 × 1.0 ≈ 0.96 (maximum effective extraction). For manufacturers: χ = 0.68 × (-0.01) × 1.0 ≈ -0.007 (extraction flows toward them; their net is positive). This differential is the core structural reality the constraint encodes.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by identifying the constraint as a structurally clear snare with a degraded regulatory response system (piton). The snare classification is not questioned because the base properties (ε=0.68, suppression=0.72, high mortality burden on trapped agents) and perspectival evidence (powerless agents trapped with no exit) are unambiguous. The mandatrophy-relevant question is whether the constraint is 'merely' an emergent property of market incentives (snare requiring market correction) or a deliberately designed extraction mechanism (snare requiring criminal liability). The pharmaceutical intentionality omega addresses this: if intentional design, pharmaceutical actors should be reclassified as active malicious extractors rather than market participants; if emergent from profit incentive without deliberate harm design, the appropriate response is structural (change regulatory, marketing, and reimbursement rules) rather than purely criminal. The constraint's classification as snare is robust across both scenarios.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_capture_mechanism,
    'To what degree is regulatory failure (DEA enforcement, FDA oversight, state boards) structural (captured by industry interest) versus incidental (resource constraints, institutional inertia)?',
    'Analysis of regulatory decision patterns, funding sources, revolving-door employment, and enforcement action timelines compared to industry lobbying and campaign contributions. Historical comparison with periods of stronger enforcement (1950s-1980s) vs present.',
    'If structural capture: regulatory actors shift from constrained to identity_locked (their institutional identity is fused with industry interests). Regulatory perspective would reclassify as beneficiary rather than independent observer. If incidental: regulatory constraint type remains moderate. If captured: the piton classification becomes a rope for regulators (they see coordination, not dysfunction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Regulatory capture vs incidental enforcement failure').

omega_variable(
    treatment_access_asymmetry,
    'Is the treatment provision sector extracting rent through genuine supply scarcity (limited MAT availability, therapist shortage) or through artificial scarcity (insurance restrictions, licensing barriers, geographic maldistribution)?',
    'Comparative analysis of MAT availability in high-regulation vs lower-regulation jurisdictions; measurement of treatment capacity gaps vs demand; analysis of insurance coverage restrictions vs clinical guidelines; cost comparison between prevention/early intervention vs late-stage treatment provision.',
    'If genuine scarcity: treatment sector faces real coordination constraints and tangled_rope classification stands. If artificial: treatment providers are active extractors and the sector reclassifies closer to snare. If mixed: directionality overrides needed to differentiate treatment providers benefiting from artificial scarcity vs those genuinely resource-constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treatment_access_asymmetry, empirical, 'Treatment access artificial vs genuine scarcity').

omega_variable(
    despair_causality_chain,
    'Does opioid addiction drive despair and mortality, or does despair drive opioid seeking, with addiction as consequence rather than cause? What is the causal primacy?',
    'Longitudinal studies tracking pre-addiction despair levels, economic opportunity, community social capital vs opioid initiation; analysis of regional despair indicators (suicide rates, life expectancy decline, community disintegration) preceding vs following opioid epidemics; intervention studies separating economic opportunity improvement from addiction treatment outcomes.',
    'If addiction drives despair: constraint model treats opioid access as primary extraction mechanism. If despair drives addiction: opioid system is secondary mechanism enabling extraction that originates in economic structure. Classification could shift from snare (addiction trap) to snare of a different primary constraint (economic precarity). Impacts beneficiary identity: pharmaceutical actors are secondary extractors if despair is primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(despair_causality_chain, empirical, 'Causal primacy: addiction vs despair in mortality chain').

omega_variable(
    identity_lock_mechanism_intergenerational,
    'For second and third-generation opioid-exposed families, has opioid dependency become fused with identity in ways that persist even when structural exit options improve?',
    'Post-treatment outcome studies comparing recovery rates in individuals with family history of opioid use vs first-generation users; analysis of identity narratives in support groups and clinical populations; comparison of recovery outcomes in cohorts experiencing improved economic opportunity vs those in stable despair.',
    'If identity lock is significant: second-generation individuals reclassify from trapped to identity_locked exit category even if structural mobility improves. Psychological interventions (identity reframing, narrative reconstruction) would be required alongside material/pharmacological interventions. If minimal: structural improvements alone sufficient for recovery pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_intergenerational, empirical, 'Identity lock persistence across generations').

omega_variable(
    pharmaceutical_design_intentionality,
    'To what degree did pharmaceutical manufacturers knowingly design, market, and distribute opioids specifically to maximize addiction (extraction intentionality) vs pursue profit from legitimate pain management with addiction as unintended consequence?',
    'Analysis of internal pharmaceutical communications, marketing strategy documents, research suppression patterns, medical liaison compensation structures; comparison of known addiction risk vs marketing representations; testimony and litigation discovery; comparison of pharmaceutical behavior in opioid domain vs other drug classes.',
    'If intentional: manufacturers are active malicious extractors, classification shifts toward maximally exploitative snare. If unintentional: extraction is emergent from profit incentive structure without deliberate harm design. Affects attribution of malevolence and appropriate policy response (criminal liability vs structural reform vs market correction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pharmaceutical_design_intentionality, empirical, 'Pharmaceutical extraction intentionality degree').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(opioid_mortality_deaths_despair, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(opioid_tr_t0, opioid_mortality_deaths_despair, theater_ratio, 0, 0.42).
narrative_ontology:measurement(opioid_tr_t5, opioid_mortality_deaths_despair, theater_ratio, 5, 0.5).
narrative_ontology:measurement(opioid_tr_t10, opioid_mortality_deaths_despair, theater_ratio, 10, 0.58).
narrative_ontology:measurement(opioid_tr_t15, opioid_mortality_deaths_despair, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(opioid_be_t0, opioid_mortality_deaths_despair, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(opioid_be_t5, opioid_mortality_deaths_despair, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(opioid_be_t10, opioid_mortality_deaths_despair, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(opioid_be_t15, opioid_mortality_deaths_despair, base_extractiveness, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(opioid_mortality_deaths_despair, resource_allocation).
narrative_ontology:boltzmann_floor_override(opioid_mortality_deaths_despair, 0.1).
narrative_ontology:affects_constraint(opioid_mortality_deaths_despair, economic_precarity_immobility).
narrative_ontology:affects_constraint(opioid_mortality_deaths_despair, pharmaceutical_regulatory_capture).
narrative_ontology:affects_constraint(opioid_mortality_deaths_despair, treatment_access_asymmetry).

% DUAL FORMULATION NOTE:
% The opioid constraint family decomposes into three structurally distinct constraints: (1) economic_precarity_immobility (ε=0.55, foundational despair mechanism), (2) pharmaceutical_regulatory_capture (ε=0.62, enables opioid expansion), (3) treatment_access_asymmetry (ε=0.45, restricts recovery pathways). Each has distinct ε values reflecting different observables: precarity measures economic immobility; regulatory capture measures policy capture by industry; treatment access measures distribution of recovery resources. The present story (opioid_mortality_deaths_despair, ε=0.68) is downstream of all three and represents their combined effect. Separate stories enable diagnosis of which constraint family member dominates in specific geographic and temporal contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(opioid_mortality_deaths_despair, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
