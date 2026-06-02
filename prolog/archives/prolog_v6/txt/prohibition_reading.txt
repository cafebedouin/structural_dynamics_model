% ============================================================================
% CONSTRAINT STORY: prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_prohibition_reading, []).

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
 *   constraint_id: prohibition_reading
 *   human_readable: Substance Prohibition as State-Enforced Moral Harm Prevention
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The prohibition reading treats substance criminalization as
 *   state-enforced moral harm prevention — a constraint where the state
 *   exercises authority to prevent drug-related harms through legal
 *   prohibition, enforcement, and carceral punishment. This reading
 *   instantiates ONE framing of a contested kernel:
 *   'substance_control_legitimacy'. The sibling readings
 *   (harm_reduction_reading, legalization_reading) use the same
 *   epidemiological data but justify different institutional arrangements.
 *   This story models ONLY the prohibition reading: users enter the victim
 *   set via criminalization; extraction flows from carceral apparatus,
 *   enforcement agencies, and pharmaceutical gatekeepers who benefit from
 *   prohibition's market-protection function. The constraint exhibits high
 *   extractiveness (0.68) and suppression (0.74) because criminalization
 *   creates multiple extraction mechanisms: direct punishment
 *   (incarceration), collateral punishment (employment discrimination,
 *   housing barriers, family separation), black market violence exposure, and
 *   health service avoidance (users cannot seek medical help without legal
 *   liability). The theater ratio (0.58) reflects that the moral framing ('we
 *   prevent harm to vulnerable people') conceals institutional extraction —
 *   the apparatus maintains public legitimacy through the harm-prevention
 *   narrative while extracting resources and carceral power.
 *
 * KEY AGENTS:
 *   - Substance Users (Criminalized): Primary victims (powerless/trapped) — face criminal liability, employment stigma, housing discrimination, family separation; trapped by carceral entanglement with no legal exit
 *   - Criminalized Communities: Structural victims (powerless/trapped, generational) — subjected to disproportionate enforcement, inherited criminalization, community infrastructure degradation
 *   - Harm Reduction Organizations: Secondary agents (moderate/constrained) — experience constraint as both coordination (responding to overdose crisis) and extraction (legal liability, criminalization of their practices)
 *   - Carceral State Apparatus: Primary beneficiary (institutional/arbitrage) — derives budget, staffing, surveillance infrastructure, plea-bargain mechanisms from prohibition; frames constraint as coordination of public safety
 *   - Enforcement Agencies (Police/Prosecution): Institutional beneficiaries (institutional/arbitrage) — derive resource allocation, operational authority, career advancement from prohibition enforcement
 *   - Pharmaceutical Industry: Secondary beneficiary (institutional/arbitrage) — prohibition creates regulatory moat protecting patent-based pharmaceuticals from criminalized alternatives; benefits from market segmentation
 *   - Legalization/Reform Coalition: Organized agents (organized/constrained) — view prohibition as temporary policy with sunset; alternative frameworks (harm reduction, legalization) becoming politically viable
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing prohibition as inherent necessity; FSM detector identifies false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(prohibition_reading, 0.68).
domain_priors:suppression_score(prohibition_reading, 0.74).
domain_priors:theater_ratio(prohibition_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prohibition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(prohibition_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(prohibition_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(prohibition_reading, snare).
narrative_ontology:human_readable(prohibition_reading, "Substance Prohibition as State-Enforced Moral Harm Prevention").
narrative_ontology:topic_domain(prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(prohibition_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(prohibition_reading, carceral_state_apparatus).
narrative_ontology:constraint_beneficiary(prohibition_reading, enforcement_agencies).
narrative_ontology:constraint_beneficiary(prohibition_reading, pharmaceutical_regulatory_gatekeepers).
narrative_ontology:constraint_victim(prohibition_reading, substance_users).
narrative_ontology:constraint_victim(prohibition_reading, criminalized_populations).
narrative_ontology:constraint_victim(prohibition_reading, black_market_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CRIMINALIZED USER (SNARE) — No exit option: legal liability, employment stigma, housing discrimination, family separation persist independent of actual substance use status. The agent is trapped by carceral entanglement. Maximum extraction: loses freedom, income, family, health services, civil rights. The moral framing ('we're preventing harm to you') provides cover for extraction. Pure snare from this structural position.
constraint_indexing:constraint_classification(prohibition_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STREET-LEVEL DRUG USER (SNARE) — Constrained by addiction dynamics and street economics, but constrained exit options, not trapped ones — can access treatment if they pay or navigate bureaucracy, can migrate to more permissive jurisdictions at high cost. Still experiences high extraction: criminalization drives them toward dealers (black market violence exposure), prevents access to medical-grade supplies (overdose risk), creates health service barriers. Effective extraction remains severe despite marginally more agency than perspective 1.
constraint_indexing:constraint_classification(prohibition_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HARM REDUCTION COMMUNITY (TANGLED ROPE) — Community organizations (needle exchanges, naloxone distribution, supervised consumption sites) experience the constraint as both coordination and extraction. The coordination function: responding to actual overdose crisis, preventing disease transmission. The extraction: criminalization creates legal liability for their operations, forces underground operation in some jurisdictions, drains resources that would go to treatment infrastructure. Active enforcement required against their practices — contradicts the 'preventing harm' rationale. Constrained exit: cannot work in open legal framework; must navigate legal precarity.
constraint_indexing:constraint_classification(prohibition_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CARCERAL STATE APPARATUS (ROPE) — Benefits substantially from prohibition: captures budget allocation, employs enforcement agencies, justifies surveillance infrastructure, maintains plea-bargain extraction mechanisms. Experiences the constraint as coordination: criminalizing substances enables government to coordinate public response, justify policy intervention, maintain moral legitimacy of criminal justice systems. Net beneficiary with maximal arbitrage options — can exit (decriminalize) but derives substantial institutional advantage from maintaining prohibition. The moral framing enables continuous resource capture.
constraint_indexing:constraint_classification(prohibition_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PHARMACEUTICAL REGULATORY GATEKEEPERS (ROPE) — Prohibition creates a regulatory moat: criminalization of certain substances protects patent-based pharmaceutical markets by preventing generic/herbal alternatives from competing. State-sanctioned medication (alcohol, nicotine absent in some contexts; prescription opioids with market control) is protected by excluding criminalized alternatives. Coordination function: the regulatory framework enables controlled distribution of medical-grade pharmaceuticals. Extraction: blackmarket exclusion maintains pricing power and patent rents. Net beneficiary with high arbitrage options — benefits from the system's continuation but can navigate regulatory change.
constraint_indexing:constraint_classification(prohibition_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: CRIMINALIZED COMMUNITIES (SNARE) — Prohibition has been disproportionately enforced against racial and economically marginalized communities across generational timescales. Trapped by institutional targeting: police deployment, prosecution rates, sentencing disparities persist even when controlling for behavior. Young people in over-policed neighborhoods inherit criminal records as default status before substance use occurs. Maximum extraction: loss of intergenerational wealth accumulation, political disenfranchisement, community infrastructure degradation. Extraction mechanism is carceral, not pharmacological.
constraint_indexing:constraint_classification(prohibition_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 7: LEGALIZATION/REFORM COALITION (SCAFFOLD) — Organized agents (harm reduction advocacy, social justice movements, some policy entrepreneurs, Portugal's decriminalization model, Canada's cannabis legalization) see prohibition as a temporary policy framework with an identifiable sunset: evidence of harms from criminalization exceeding harms from substances themselves; epidemiological data from jurisdictions with alternative approaches. Constrained exit: faces organized opposition from carceral and pharmaceutical interests, but has structural arguments and emerging empirical data. Theater is lower than from state perspective because the functional argument (preventing substance-related harm) is decoupled from the mechanism (criminalization). Scaffold classification derives from sunset logic: alternative frameworks (treatment, harm reduction, legalization) are becoming politically viable within generational timescales.
constraint_indexing:constraint_classification(prohibition_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a civilizational/universal perspective, the view claims substance use is inherently harmful and state authority to prevent harm is immutable and necessary. This perspective treats drug policy as a natural law: all human societies must enforce some substance restrictions due to inherent human vulnerability to addiction. However, the structural data contradicts the mountain classification — identifiable beneficiaries (carceral state, pharmaceutical gatekeepers) exist, extraction mechanisms are institutional rather than natural, and sibling readings (harm reduction, legalization) show the constraint as contingent policy, not natural law. The engine will compute this as a false summit, revealing naturalization of a political commitment system.
constraint_indexing:constraint_classification(prohibition_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prohibition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(prohibition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prohibition_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The constraint's base extraction is substantial because criminalization converts substance use into a carceral extraction mechanism. The measurement trajectory shows increasing extractiveness from 0.52 to 0.68 across the interval, reflecting both intensification of enforcement (police budgets, incarceration rates) and accumulation of collateral consequences (employment barriers, housing discrimination, family destabilization). The 0.68 final value reflects that extractiveness continues to grow as the carceral system consolidates authority — each enforcement action creates barriers to exit (criminal record prevents employment, which pushes toward re-engagement in criminalized markets, which increases re-arrest probability). This is extraction via structural pathway rather than direct confiscation, but it is extraction nonetheless: wealth and opportunity flow from criminalized populations toward carceral apparatus. Suppression (0.74): High. Criminalization creates multiple suppression mechanisms: (a) direct legal barriers (criminal penalties, imprisonment prevent exit via legal means), (b) collateral barriers (criminal record prevents legitimate employment, housing, family reunification, effectively trapping users in criminalized markets), (c) health service avoidance (users cannot seek treatment without legal liability, preventing the medical exit path), (d) addiction-mediated compulsion (neurochemical dependence reduces volitional capacity to exit). The 0.74 value reflects that both external barriers (legal/economic) and internal barriers (addiction-mediated compulsion) operate simultaneously — suppression is robust across multiple mechanisms. Theater ratio (0.58): Moderate-high and rising. The public moral framing ('we prevent harm to vulnerable people') provides legitimacy cover for extraction. The actual mechanism (criminalization, carceral punishment, black market violence) does not reduce substance use prevalence in most jurisdictions — the constraint's functional output is carceral expansion and extraction, not harm reduction. The rising trajectory (0.38 → 0.58) reflects increasing gap between stated function (harm prevention) and actual mechanism (institutional extraction). The 0.58 value indicates that most of the visible activity (police enforcement, prosecution, incarceration) is theater — activity that maintains institutional legitimacy while not achieving stated goals.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates severe perspectival gaps across structural positions. The criminalized user sees pure snare: trapped by legal liability, employment stigma, health service barriers, with no coordination benefit and maximum extraction. The carceral state sees rope: coordination of public safety, legitimate use of enforcement authority, institutional stability. The harm reduction community sees tangled rope: real coordination function (preventing overdose, disease) alongside real extraction (legal liability, resource constraints). The legalization coalition sees scaffold: temporary policy framework with identifiable sunset, evidence that alternative frameworks reduce total harms, emerging political viability. The analytical observer risks seeing mountain: treating prohibition as natural law ('all societies must restrict substances'), but the structural data (identifiable beneficiaries, institutional extraction mechanisms, policy cycling across jurisdictions) contradicts the mountain classification — this is a false summit. The perspectival gap between the powerless user (snare) and the institutional beneficiary (rope) is maximal: the same constraint enables the state's coordination function and disables the user's freedom simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   The prohibition reading structures directionality around the beneficiary/victim split encoded in the constraint's institutional logic. Substance users are declared as victims because criminalization converts them into carceral extraction targets — their structural relationship to the constraint flows extraction toward the carceral apparatus. The carceral state, enforcement agencies, and pharmaceutical gatekeepers are declared as beneficiaries because they derive institutional advantage from prohibition's persistence: budget allocation, operational authority, market-protection rents. The directionality derivation maps these structural relationships to the sigmoid f(d): victims with trapped/constrained exit options generate high d (0.85-0.95) → high f(d) → high effective extraction (χ); beneficiaries with arbitrage exit generate low d (0.05-0.20) → low f(d) → negative/minimal effective extraction. The perspectival gap between the powerless victim (d≈0.90, f(d)≈1.30) and the institutional beneficiary (d≈0.10, f(d)≈-0.05) illustrates why snare and rope emerge from the same base properties with different observer positions. The directionality principle: follow the extraction flow. Users pay (criminal liability, lost freedom, health deterioration); state collects (budget, authority, institutional legitimacy). The constraint's structure encodes this relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The prohibition reading resolves mandatrophy by explicitly modeling one reading of a contested kernel. The constraint is NOT 'what is the correct drug policy?' (that is a preference question, not a structural question). The constraint IS 'given the prohibition reading's institutional commitments and beneficiary/victim structure, what is the classification?' Answer: Snare from the victim perspective; Rope from the beneficiary perspective; Scaffold from the legalization coalition perspective; Mountain (false summit) from the analytical perspective risking naturalization. The mandatrophy resolves by recognizing that (a) the readings are competing framing choices, not empirical discoveries, and (b) each reading produces coherent classifications when the structural data are correctly attributed. The prohibition reading attributes extracted value to the carceral apparatus and pharmaceutical gatekeepers; it declares substance users as victims; it structures the moral framing ('we prevent harm') as potential theater. Alternative readings would re-attribute: harm-reduction-reading treats users as agents deserving medical access (classification shifts); legalization-reading treats criminalization as primary harm source (classification shifts further). The shared empirical facts (addiction prevalence, enforcement intensity, health outcomes) remain constant; the classification differences reflect different readings of the kernel 'substance_control_legitimacy'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_alternate_framings,
    'This constraint instantiates ONE READING of the contested kernel ''substance_control_legitimacy''. How would the snare/rope/scaffold classifications change under sibling readings?',
    'Generate sibling constraint stories for ''harm_reduction_reading'' and ''legalization_reading''. Compare beneficiary/victim sets, extractiveness values, and perspectival gaps across readings. The kernel is the persistent institutional commitment to substance control; the readings are the alternative legitimacy claims for that control.',
    'Prohibition-reading treats substance users as victims of harm (criminalization prevents access to safe supplies, drives black market violence). Harm-reduction-reading treats users as agents deserving medical access (classification flips toward rope/tangled_rope). Legalization-reading treats criminalization as primary harm source (users move from snare to mobile). The difference is NOT empirical (all readings use the same epidemiological data) but framing-dependent: which institutional commitment structure is legitimate?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternate_framings, conceptual, 'Competing readings of substance control legitimacy kernel').

omega_variable(
    criminalization_harm_attribution,
    'What proportion of observed substance-related harms (overdose, disease transmission, violence, health deterioration) is attributable to the substance itself versus to criminalization-driven externalities (black market contamination, supply uncertainty, avoidance of health services)?',
    'Comparative epidemiology across jurisdictions with different policy regimes (Portugal decriminalization data, Canada cannabis legalization outcomes, Netherlands harm reduction outcomes, versus US/UK prohibition-centric approaches). Decompose harm sources: substance-specific (pharmacological, addiction), criminalization-specific (overdose risk from adulterants, disease from unsterile supply), economic (poverty, social stress), structural (racism, inequality). Attribute extractiveness to criminalization mechanisms separately from substance-intrinsic harms.',
    'If criminalization-driven harms exceed substance-intrinsic harms: prohibition_reading''s extractiveness is overstated — the constraint''s primary function is extraction, not harm prevention. Reclassify snare to even purer form. If substance-intrinsic harms dominate: the moral framing (''preventing harm'') better reflects actual constraint function — reclassify toward tangled_rope with legitimate coordination component. Current extractiveness (0.68) assumes criminalization-driven harms constitute 50-60% of total observed harms; empirical attribution will calibrate this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(criminalization_harm_attribution, empirical, 'Attribution of substance harms to criminalization vs substance pharmacology').

omega_variable(
    moral_authority_legit_ambiguity,
    'Does state authority to prevent harm through criminalization derive from moral duty, legal mandate, public preference, or institutional entrenchment?',
    'Analyze justificatory discourse in policy documents, court decisions, and enforcement practice. Distinguish between (a) explicit moral claims (''we prevent harm''), (b) legal authority claims (''statutes mandate enforcement''), (c) preference aggregation (''public wants drug-free society''), (d) path dependence (''enforcement apparatus exists, continues by default''). Empirically test whether enforcement correlates with stated goals or with institutional incentives (carceral budget, police staffing, pharmaceutical lobbying expenditure).',
    'If moral duty is genuine primary motivator: extractiveness may be lower than 0.68 — some portion is real benefit (prevented addiction in marginal cases), not pure extraction. If moral claims are cover story for institutional extraction: extractiveness should be higher (0.75+). If public preference is primary: the constraint''s legitimacy depends on preference stability (cyclical swings in drug policy suggest preference is unstable). If path dependence dominates: snare classification is correct — the constraint persists because removal is harder than continuation, not because benefits justify costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_authority_legit_ambiguity, conceptual, 'Source of state moral authority for harm-prevention criminalization').

omega_variable(
    black_market_violence_externality,
    'What is the causal pathway from prohibition → black market violence → additional harm costs? How much of criminalized-population victimization is externality (violence from market structure) versus direct state carceral extraction?',
    'Isolate markets where prohibition removed (e.g., alcohol post-1933 US, cannabis post-legalization Canada/Uruguay): measure violence before/after legalization. Compare violence intensity in prohibited versus decriminalized/legalized regimes controlling for substance, jurisdiction size, enforcement intensity. Distinguish seller-on-seller violence (market competition) from seller-on-consumer violence (market consolidation/coercion) from third-party violence (cartel territorial warfare).',
    'If black market violence is high and prohibition-attributable: extractiveness and suppression both increase (violence acts as coercive mechanism beyond legal punishment). Snare classification strengthens. If legalization reduces violence without increasing adverse health outcomes: scaffold''s sunset claim strengthens — alternative frameworks reduce total harms. If violence persists post-legalization due to other structural factors: suggests violence is not primarily prohibition-attributable — extractiveness may decrease, classification shifts toward tangled_rope (legitimate harm-reduction coordination component, lower extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_violence_externality, empirical, 'Black market violence as prohibition-dependent externality').

omega_variable(
    addiction_as_trap_versus_choice,
    'Does addiction constitute a structural trap (neurochemical compulsion overriding volition) or a rational choice under constrained options (engaging in risky behavior given available alternatives)?',
    'Neuroscience literature on dopamine pathways, decision-making impairment, and recovery trajectories. Behavioral economics of intertemporal choice under uncertainty. Compare addiction prevalence and trajectory across social contexts: does addiction primarily occur in high-stress/low-opportunity environments (structural trap hypothesis) or uniformly across socioeconomic contexts (individual vulnerability hypothesis)? Analyze recovery data: what proportion of users exit without intervention? What intervention types show sustained change?',
    'If addiction is primarily neurochemical trap: users are genuinely powerless; exit_options:trapped is correct; snare classification with 0.68 extractiveness is appropriate — they cannot exercise choice. If addiction is primarily choice under constraint: exit_options should shift to constrained or mobile; classification might shift toward tangled_rope — the system does coordinate substance distribution, but also constrains access to alternatives. The moral framing (''preventing harm to vulnerable people'') is more defensible under trap hypothesis; less defensible if constraint is primarily choice-driven.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(addiction_as_trap_versus_choice, empirical, 'Addiction as structural trap vs constrained rational choice').

omega_variable(
    false_summit_natural_law_test,
    'Is the mountain perspective (''substance use is inherently harmful; state must prevent it'') a genuine natural law or a naturalized policy commitment?',
    'Historical analysis: across how many human societies and time periods has substance prohibition existed? How stable is the commitment? Does the constraint appear in pre-state societies? Do jurisdictions show persistent policy cycling (prohibition → decriminalization → re-prohibition) suggesting the constraint is institutional rather than natural? Are there logical/physical grounds for the constraint (speed of light) or primarily institutional ones (enforcement structures, political coalitions)?',
    'If mountain is genuine natural law: beneficiaries declaration was error; remove beneficiaries; reclassify as pure natural law (all perspectives become mountain). If mountain is false summit: beneficiaries are correct; engine''s FSM detector flags the constraint; snare classification is correct interpretation. Current high extractiveness and suppression values suggest false summit — the constraint''s persistence derives from institutional incentives, not natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_test, empirical, 'Is prohibition a natural law or naturalized policy commitment?').

omega_variable(
    suppression_mechanism_carceral_vs_medical,
    'What proportion of the measured suppression (0.74) is carceral (legal barriers, criminal penalties, imprisonment) versus medical/health (addiction-related compulsion, health service barriers)?',
    'Decompose suppression into sources: (a) direct carceral (criminal liability, incarceration, surveillance), (b) collateral carceral (housing discrimination, employment barriers, family separation from incarceration), (c) addiction-mediated (inability to exit due to neurochemical dependence), (d) health system (barriers to treatment, stigma in medical settings), (e) economic (cost of treatment, cost of legal defense, lost income during incarceration). Calculate proportion attributable to each. Measure suppression in decriminalized contexts (Portugal, parts of Canada) to establish baseline for addiction-mediated suppression independent of carceral suppression.',
    'If carceral suppression is 40%+ of total: the constraint''s function is partially extraction through punishment, not pure harm prevention. If carceral suppression is <20%: addiction-mediated mechanisms dominate, and the constraint''s moral framing (''we prevent harm'') is more aligned with its actual mechanism. Decomposition informs whether extractiveness should increase (higher carceral suppression) or decrease (higher addiction-mediated suppression represents genuine constraint rather than institutional extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_carceral_vs_medical, empirical, 'Composition of suppression: carceral vs medical mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(prohibition_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prohb_tr_t0, prohibition_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(prohb_tr_t10, prohibition_reading, theater_ratio, 10, 0.47).
narrative_ontology:measurement(prohb_tr_t20, prohibition_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(prohb_tr_t30, prohibition_reading, theater_ratio, 30, 0.63).

% Extraction over time
narrative_ontology:measurement(prohb_be_t0, prohibition_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(prohb_be_t10, prohibition_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(prohb_be_t20, prohibition_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(prohb_be_t30, prohibition_reading, base_extractiveness, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(prohibition_reading, harm_reduction_reading).
narrative_ontology:affects_constraint(prohibition_reading, legalization_reading).
narrative_ontology:affects_constraint(prohibition_reading, carceral_expansion_pipeline).
narrative_ontology:affects_constraint(prohibition_reading, pharmaceutical_market_protection).

% DUAL FORMULATION NOTE:
% The prohibition_reading is one of three competing readings of the kernel 'substance_control_legitimacy'. Each reading produces distinct extractiveness values and beneficiary/victim structures from the same epidemiological facts. The network links prohibition_reading to its sibling readings (harm_reduction_reading, legalization_reading) to enable cross-reading comparison. The constraint also affects downstream institutional dynamics: carceral_expansion_pipeline (how prohibition drives incarceration growth) and pharmaceutical_market_protection (how prohibition protects patent-based pharmaceutical markets). These are separate constraints with their own ε values; they are downstream of the reading choice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(prohibition_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
