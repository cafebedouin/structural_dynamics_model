% ============================================================================
% CONSTRAINT STORY: sotu_2000_clinton_community_policing_brady_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_2000_clinton_community_policing_brady_law, []).

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
 *   constraint_id: sotu_2000_clinton_community_policing_brady_law
 *   human_readable: Community Policing + Brady Law Dual-Lever Public Safety Constraint
 *   domain: criminal_justice/public_safety/firearms_regulation
 *
 * SUMMARY:
 *   The 1994 Crime Bill and 2000 SOTU period consolidated two complementary
 *   but structurally distinct mechanisms: (1) Community Policing Initiative
 *   deploying 100,000 new officers to high-crime neighborhoods, and (2) Brady
 *   Handgun Violence Prevention Act establishing federal background checks
 *   for firearm purchases via the National Instant Criminal Background Check
 *   System (NICS). Together, these create a dual-lever constraint that
 *   combines preventive policing (reduce crime through presence and community
 *   engagement) with gatekeeping (reduce armed recidivism through access
 *   control). The constraint exhibits all six DR types from different
 *   observer perspectives, revealing a fundamental tension: the system
 *   produces genuine public safety coordination benefits (crime reduction,
 *   armed recidivism prevention) while simultaneously imposing asymmetric
 *   extraction on over-policed communities and gun purchasers subject to
 *   delays or denials. The constraint's theater ratio (0.52) reflects
 *   moderate performativity — community policing relies partly on symbolic
 *   presence and relationship-building (genuine coordination) but also on
 *   deployment schedules and federal funding commitments independent of
 *   actual crime reduction (theater). Brady background checks have lower
 *   theater at the mechanism level (they check databases) but higher theater
 *   in implementation (false positives, data quality issues, opaque denial
 *   criteria). Base extractiveness (0.38) is moderate: the system extracts
 *   legitimate costs (police budgets from taxpayers, delay costs from
 *   purchasers, surveillance burden from over-policed communities) while
 *   producing documented benefits (crime reduction, armed interdiction).
 *
 * KEY AGENTS:
 *   - Crime-Reduction Constituencies: Primary beneficiary (institutional/arbitrage) — crime victims, public health advocates, communities experiencing declining violence benefit from the constraint without bearing direct costs
 *   - Law Enforcement Agencies: Primary beneficiary (institutional/arbitrage) — federal funding, officer deployments, NICS database infrastructure, jurisdictional expansion; experience constraint as pure coordination
 *   - Over-Policed Communities: Primary victim (powerless/trapped) — absorbed expanded police presence regardless of community preference; face surveillance burden, stop-and-frisk normalization, incarceration risk; cannot exit or refuse the constraint
 *   - Gun Purchasers: Secondary victim (moderate/constrained) — Brady background checks impose delay costs, false positive denials, procedural burden; can exit by moving jurisdiction or legal challenge at high cost
 *   - Community Safety Coalition: Organized beneficiary (organized/constrained) — victim advocates, crime-prevention organizations benefit from measurable outcomes but constrained by implementation gaps and community trust deficits
 *   - Federal Criminal Justice Infrastructure: Institutional actor (institutional/arbitrage) — NICS system, federal funding mechanisms, data integration enable scaling of both mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_2000_clinton_community_policing_brady_law, 0.38).
domain_priors:suppression_score(sotu_2000_clinton_community_policing_brady_law, 0.48).
domain_priors:theater_ratio(sotu_2000_clinton_community_policing_brady_law, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_2000_clinton_community_policing_brady_law, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_2000_clinton_community_policing_brady_law, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_2000_clinton_community_policing_brady_law, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_2000_clinton_community_policing_brady_law, tangled_rope).
narrative_ontology:human_readable(sotu_2000_clinton_community_policing_brady_law, "Community Policing + Brady Law Dual-Lever Public Safety Constraint").
narrative_ontology:topic_domain(sotu_2000_clinton_community_policing_brady_law, "criminal_justice/public_safety/firearms_regulation").

domain_priors:requires_active_enforcement(sotu_2000_clinton_community_policing_brady_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_2000_clinton_community_policing_brady_law, crime_reduction_constituencies).
narrative_ontology:constraint_beneficiary(sotu_2000_clinton_community_policing_brady_law, communities_with_reduced_violent_crime).
narrative_ontology:constraint_beneficiary(sotu_2000_clinton_community_policing_brady_law, law_enforcement_agencies).
narrative_ontology:constraint_victim(sotu_2000_clinton_community_policing_brady_law, gun_purchasers_with_delays).
narrative_ontology:constraint_victim(sotu_2000_clinton_community_policing_brady_law, over_policed_communities).
narrative_ontology:constraint_victim(sotu_2000_clinton_community_policing_brady_law, due_process_concerns).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OVER-POLICED COMMUNITY (SNARE) — Communities designated as high-crime bear the full extraction of expanded police presence: surveillance burden, stop-and-frisk normalization, incarceration risk for minor infractions. No exit option — the constraint is imposed regardless of community preference. Experiences pure extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(sotu_2000_clinton_community_policing_brady_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GUN PURCHASER (TANGLED ROPE) — Brady background checks create coordination benefit (genuine reduction in armed recidivism and crime prevention) alongside extraction (delay costs, denial for false positives or technical errors, expanded NICS burden). Constrained by the legal framework; can exit only at significant cost (moving jurisdictions, legal challenge). Mixed benefit-cost experience.
constraint_indexing:constraint_classification(sotu_2000_clinton_community_policing_brady_law, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LAW ENFORCEMENT (ROPE) — Benefits from expanded budgets, officer deployments, and data infrastructure (NICS database). Experiences the constraint as pure coordination: deploying officers solves the collective action problem of crime prevention; NICS enables data-driven interdiction. Net beneficiary with arbitrage options (agency autonomy in implementation, federal funding leverage).
constraint_indexing:constraint_classification(sotu_2000_clinton_community_policing_brady_law, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMMUNITY SAFETY COALITION (TANGLED ROPE) — Organized constituencies (crime victims, community organizations, public health advocates) benefit from measurable crime reduction and prevention coordination. Constrained by political backlash and implementation gaps in community trust. Asymmetric extraction: benefits concentrate in low-crime outcomes; costs concentrate in procedural fairness concerns and surveillance burden.
constraint_indexing:constraint_classification(sotu_2000_clinton_community_policing_brady_law, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COMMUNITY POLICING INFRASTRUCTURE (PITON) — The 100,000-officer program is substantially performative: community presence is maintained through budget commitment and deployment schedules, but actual community-police relationship quality is highly variable and theater-dependent. Many communities experience the police presence as occupational rather than collaborative. Institutional inertia maintains the program despite uneven functional coordination outcomes.
constraint_indexing:constraint_classification(sotu_2000_clinton_community_policing_brady_law, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, crime prevention through resource concentration and information gatekeeping is an immutable structural feature of large-scale public order. The constraint appears as a natural law: more officers and better data reduce crime inevitably. However, the structural data contradicts this — identifiable beneficiaries (law enforcement, crime-reduction constituencies) profit from the constraint, suggesting this is a constructed institutional arrangement naturalized as necessity.
constraint_indexing:constraint_classification(sotu_2000_clinton_community_policing_brady_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_2000_clinton_community_policing_brady_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_2000_clinton_community_policing_brady_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_2000_clinton_community_policing_brady_law, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_2000_clinton_community_policing_brady_law, TR),
    TR >= 0.70.

:- end_tests(sotu_2000_clinton_community_policing_brady_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The system produces measurable coordination benefits (crime reduction documented at 5-15% in deployment zones; armed recidivism prevention via Brady mechanism varies by jurisdiction 2-8%). Extraction is real but not total — gun purchasers face delays (avg 3-5 days) and false positive denials (~0.5-2% depending on data quality), over-policed communities bear surveillance burden and differential enforcement. The extractiveness value reflects that both benefit and cost are genuine, neither dominant. Rising from 0.28 to 0.38 over 6 years indicates increasing enforcement burden relative to initial deployment benefits. Suppression (0.48): Moderate-high. Brady denials are suppressive — prohibited persons cannot easily challenge denials (limited appeal process) or acquire through legal channels (background check requirement); geographic variation in implementation reduces transparency. Over-policing suppresses alternative dispute resolution (formal enforcement replaces community negotiation). Suppression is not total because community trust coalitions can organize, gun rights groups can lobby for appeal procedures, and multiple entry points exist for circumventing both mechanisms (straw purchases, unlicensed dealers). Theater ratio (0.52): Moderate-high. Community policing theater is substantial: federal commitment to 100,000 officers is performative (actual deployment, retention, and community engagement vary; funding tied to hiring targets not outcomes). Brady theater exists in the NICS delay process (background checks are data-driven but denial criteria are opaque; false positives persist due to data quality). True coordination components: crime reduction is partially real (measured); armed interdiction has documented effect. The theater reflects that performance metrics (officer count, check volume) are easier to report than outcome metrics (community trust, crime causation).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival divergence. Law enforcement sees pure coordination (Rope) — deploying officers and checking background data solve legitimate collective action problems and are experienced as positive institutional mandate. The over-policed community sees extraction (Snare) — expanded police presence is imposed, not chosen, and costs (surveillance, stops, incarceration risk) are borne disproportionately. Gun purchasers see mixed coordination-extraction (Tangled Rope) — Brady background checks provide genuine crime prevention benefit alongside procedural burden and false positive risk. Community safety coalitions see temporary coordination with trust deficits (Tangled Rope leaning toward Scaffold if trust can be rebuilt). The community policing infrastructure itself sees its own function as partially degraded (Piton) — officers maintain presence through budget and deployment mandates, but actual community-police relationships are theater-dependent. The analytical observer at civilizational scope risks seeing immutable law (Mountain: crime prevention through resource concentration and gatekeeping) but structural data reveals identifiable beneficiaries profiting from the arrangement, suggesting false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries and victims drive the directionality computation. Crime-reduction constituencies and law enforcement are beneficiaries with low cost and high benefit — their d values are low (~0.15-0.25), producing negative or minimal f(d) → negative or minimal χ. Over-policed communities are victims with high cost and minimal benefit (they did not choose the constraint) — their d values are high (~0.85-0.95), producing high f(d) → high χ. Gun purchasers are moderate beneficiaries of reduced crime but victims of Brady delays/denials — their d values are moderate (~0.50-0.60), producing moderate f(d) → moderate χ. Organized coalitions have both beneficiary (crime reduction) and victim (community trust burden) status with partial exit options — their d values are constrained (~0.55-0.65), producing moderate-high f(d) → tangled rope. The piton perspective derives from high theater (community policing infrastructure maintained through inertia and funding commitment despite variable outcomes) combined with low effective extraction for the institutional actor (law enforcement benefits either way). The mountain perspective risks naturalizing contingent institutional arrangements (the idea that crime prevention through police presence is inevitable) as immutable law — the engine's FSM will flag this if beneficiaries are declared.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint operates through genuine coordination (crime reduction, armed recidivism prevention) alongside real extraction (surveillance burden, false positive denials, police funding extraction from general budgets). The constraint is not pure extraction (Snare) because measurable public goods are produced. It is not pure coordination (Rope) because asymmetric extraction is severe for over-policed communities and gun purchasers. Tangled Rope is the correct classification because both functions are authentic: the system genuinely coordinates crime prevention AND genuinely extracts resources and surveillance burden in asymmetric patterns. The perspectival gap (Snare from the over-policed community, Rope from law enforcement, Tangled Rope from moderate agents) reveals this mandatrophy. The resolution is that all three perspectives are correct from their structural position. The tension is not eliminated — it is diagnosed: the constraint requires both coordination (crime prevention) and extraction (enforcement burden) to function, and the distribution of both is asymmetric. Policy resolution would require increasing community control over police deployment (giving trapped agents some exit option, moving them toward constrained) and improving Brady appeal procedures (reducing gun purchaser denials and false positives, lowering their d value).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    brady_false_positive_rate_threshold,
    'What proportion of Brady denials are erroneous (incorrect matches, sealed convictions, data quality failures) versus legitimate prohibition targets?',
    'Audit of denied applications: cross-reference with court records, DNA exoneration databases, sealed conviction records; comparison with post-purchase conduct of approved applicants in marginal cases',
    'If false positive rate > 10%: Brady mechanism is primarily extraction from innocent purchasers (reclassify gun purchaser perspective toward snare). If < 2%: coordination mechanism is genuine (tangled rope holds). If 5-10%: mixed extraction-coordination (tangled rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brady_false_positive_rate_threshold, empirical, 'False positive rate of Brady background checks').

omega_variable(
    community_policing_crime_causation,
    'Does expanded officer presence causally reduce crime, or does crime reduction drive deployment strategy (reverse causation)?',
    'Quasi-experimental analysis: staggered rollout of 100,000 officers program; synthetic control comparison with demographically similar jurisdictions; instrumental variables analysis using federal funding variation; analysis of crime trends 12 months pre- and post-deployment',
    'If causal effect > 5% reduction: community policing provides genuine coordination benefit (tangled rope valid). If effect < 2% or reversed: extraction is not producing claimed benefit (reclassify toward snare). If confounded: cannot determine coordination vs extraction from baseline data alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(community_policing_crime_causation, empirical, 'Causal effect of community policing deployment on crime reduction').

omega_variable(
    over_policing_surveillance_burden_asymmetry,
    'Is the surveillance burden and stop-and-frisk intensity distributed equally across racial/ethnic/socioeconomic strata, or concentrated in over-policed communities?',
    'Analysis of stop data, arrest data, use-of-force data by deployment zone and demographic: if stop rates per capita are 3-5x higher in certain communities, asymmetry is confirmed. Comparison with community preference surveys (would this community choose this police level?).',
    'If high asymmetry: extraction of over-policed communities is severe (snare perspective confirmed). If symmetrical: constraint distributes burden evenly (tangled rope shifts toward rope for community perspective). If asymmetry correlates with crime rates only: extraction is proportional to genuine need (tangled rope justified).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(over_policing_surveillance_burden_asymmetry, empirical, 'Distribution of police surveillance burden and enforcement intensity').

omega_variable(
    community_trust_and_cooperation_baseline,
    'Does increased police presence improve or degrade community-police trust and voluntary cooperation in crime prevention?',
    'Pre-post surveys of community perception in deployment zones; analysis of 911 call rates and witness cooperation rates; arrest-to-conviction ratio trends; community participation in citizen patrol programs',
    'If cooperation increases: coordination mechanism is functioning (tangled rope confirmed). If decreases: extraction mechanism is dominant (snare for community perspective). Trust dynamics determine whether the constraint succeeds or fails as a dual-lever system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_trust_and_cooperation_baseline, empirical, 'Community-police trust and voluntary cooperation dynamics').

omega_variable(
    armed_recidivism_prevention_attribution,
    'Does Brady law access denial reduce armed recidivism specifically, or do prohibited persons simply acquire firearms through alternative channels (unlicensed dealers, straw purchases)?',
    'Comparison of armed crime rates in Brady-covered states vs pre-Brady or non-Brady jurisdictions; tracking of denied applicants'' post-application conduct (arrest, victimization); cross-national comparison with high-prosecution jurisdictions',
    'If Brady denial prevents armed recidivism: coordination benefit is real (tangled rope extraction justified as cost of gatekeeping). If alternative acquisition bypasses Brady: extraction without benefit (reclassify toward snare for purchaser perspective). If mixed: partial coordination (tangled rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(armed_recidivism_prevention_attribution, empirical, 'Effectiveness of Brady law in preventing armed recidivism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_2000_clinton_community_policing_brady_law, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_tr_t0, sotu_2000_clinton_community_policing_brady_law, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sotu_tr_t3, sotu_2000_clinton_community_policing_brady_law, theater_ratio, 3, 0.48).
narrative_ontology:measurement(sotu_tr_t6, sotu_2000_clinton_community_policing_brady_law, theater_ratio, 6, 0.52).

% Extraction over time
narrative_ontology:measurement(sotu_be_t0, sotu_2000_clinton_community_policing_brady_law, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(sotu_be_t3, sotu_2000_clinton_community_policing_brady_law, base_extractiveness, 3, 0.34).
narrative_ontology:measurement(sotu_be_t6, sotu_2000_clinton_community_policing_brady_law, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_2000_clinton_community_policing_brady_law, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_2000_clinton_community_policing_brady_law, mass_incarceration_system).
narrative_ontology:affects_constraint(sotu_2000_clinton_community_policing_brady_law, gun_rights_advocacy_litigation).
narrative_ontology:affects_constraint(sotu_2000_clinton_community_policing_brady_law, police_militarization).

% DUAL FORMULATION NOTE:
% The community policing constraint and Brady law constraint are structurally distinct (different ε values, different beneficiary-victim structures) but operationalized together in the 1994 Crime Bill. Community policing coordination: ε ≈ 0.30 (crime reduction mechanism). Brady gatekeeping: ε ≈ 0.35 (access control mechanism). When bundled as a single policy package, emergent extractiveness rises to 0.38 because asymmetric benefit distribution (law enforcement gains budget and data infrastructure; over-policed communities bear surveillance burden) creates coupling between mechanisms. Write separate stories for each mechanism if analyzing their independent effects; the unified story captures their bundled institutional impact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_2000_clinton_community_policing_brady_law, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
