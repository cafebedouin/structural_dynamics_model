% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__prohibition_reading, []).

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
 *   constraint_id: substance_control_kernel__prohibition_reading
 *   human_readable: Substance Use as Moral Transgression: Prohibition Reading
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The prohibition reading of substance control is a specific kernel reading
 *   that frames substance use as a moral transgression requiring state
 *   punishment to defend social order. This is ONE of three structurally
 *   distinct constraint stories arising from the substance_control_kernel —
 *   the other readings (harm_reduction, legalization) make different
 *   empirical and normative claims about what substance use is and what
 *   response serves public welfare. The prohibition reading instantiates a
 *   Snare: it extracts compliance from users through criminalization while
 *   conferring beneficiary status on law enforcement, incarceration
 *   industries, and pharmaceutical monopolies. The constraint's theater has
 *   risen over the 30-year interval (0.48 → 0.65) as enforcement intensity
 *   increased while drug use prevalence remained stable or rose, revealing
 *   that enforcement is increasingly performative rather than functional.
 *   Suppression (coercive capacity) has similarly intensified (0.65 → 0.78)
 *   as arrest, incarceration, and asset seizure machinery expanded. Base
 *   extractiveness (0.42 → 0.68) rose as enforcement scope broadened beyond
 *   direct users to include family members, financial associates, and
 *   community infrastructure. The rising extractiveness trajectory is the
 *   signature of a constraint transitioning from coordination (early drug
 *   policy emphasized public health) to pure extraction (contemporary
 *   prohibition emphasizes criminal prosecution regardless of public health
 *   outcomes).
 *
 * KEY AGENTS:
 *   - Substance Users: Primary victim (powerless/trapped) — face criminal liability, incarceration, permanent legal disability, family separation. Exit requires identity dissolution (ceasing use entirely) or fleeing jurisdiction.
 *   - Marginalized Communities: Primary victim (powerless/trapped, generational) — enforcement is racially stratified; incarceration creates intergenerational poverty and trauma-based vulnerability.
 *   - Law Enforcement Apparatus: Primary beneficiary (institutional/arbitrage) — receives budget, personnel authority, legal immunity. Experiences constraint as coordination mechanism.
 *   - Incarceration Industry: Beneficiary (institutional/arbitrage) — private prison operators, bail bondsmen, commissary providers, treatment vendors extract from incarcerated population.
 *   - Pharmaceutical Industry: Beneficiary (institutional/arbitrage) — patent monopolies on regulated medications (oxycodone, methadone, buprenorphine); black market elimination.
 *   - Treatment Providers: Mixed position (moderate/constrained) — benefit from funding but constrained by criminal law override of clinical judgment.
 *   - Harm Reduction Organizations: Secondary victim (organized/constrained) — operate at legal margins; advocacy for alternative readings.
 *   - International Drug Control Regime: Maintains institutional (piton) — UN conventions enforce prohibition nominally; actual function degraded.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, 0.68).
domain_priors:suppression_score(substance_control_kernel__prohibition_reading, 0.78).
domain_priors:theater_ratio(substance_control_kernel__prohibition_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_kernel__prohibition_reading, "Substance Use as Moral Transgression: Prohibition Reading").
narrative_ontology:topic_domain(substance_control_kernel__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__prohibition_reading, 'bb33d040-b701-4852-879b-517a906eadc5').
narrative_ontology:cs_kernel_codification('bb33d040-b701-4852-879b-517a906eadc5', formalized).
narrative_ontology:cs_authority_grounding('bb33d040-b701-4852-879b-517a906eadc5', extraction).
narrative_ontology:cs_interpretation_layer_present('bb33d040-b701-4852-879b-517a906eadc5').
narrative_ontology:cs_reading_relation('bb33d040-b701-4852-879b-517a906eadc5', substance_control_kernel__harm_reduction_reading, forecloses).
narrative_ontology:cs_reading_relation('bb33d040-b701-4852-879b-517a906eadc5', substance_control_kernel__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('bb33d040-b701-4852-879b-517a906eadc5', foundational, moral_transgression_framing).
narrative_ontology:cs_axiom_status(moral_transgression_framing, overridden).
narrative_ontology:cs_axiom_grounding('bb33d040-b701-4852-879b-517a906eadc5', moral_transgression_framing, empirically_contingent).
narrative_ontology:cs_axiom('bb33d040-b701-4852-879b-517a906eadc5', secondary, state_punishment_necessary_for_social_order).
narrative_ontology:cs_axiom_status(state_punishment_necessary_for_social_order, holdable).
narrative_ontology:cs_axiom_grounding('bb33d040-b701-4852-879b-517a906eadc5', state_punishment_necessary_for_social_order, conventional).
narrative_ontology:cs_reference_frame('bb33d040-b701-4852-879b-517a906eadc5', moral_order_preservation_via_criminal_punishment).
narrative_ontology:cs_drift_state('bb33d040-b701-4852-879b-517a906eadc5', contemporary_evidence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bb33d040-b701-4852-879b-517a906eadc5', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__prohibition_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, enforcement_apparatus).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, private_prison_operators).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, marginalized_communities).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, public_health_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBSTANCE USER (SNARE) — Faces criminalization, incarceration, and permanent legal disability. No exit from constraint without ceasing use entirely (identity-based barrier) or fleeing jurisdiction. Suppression enforced through criminal law, asset seizure, family separation. Maximum extraction: loses income, liberty, social standing, and reproductive autonomy (via family court judgments). The constraint exists to extract compliance from this agent.
constraint_indexing:constraint_classification(substance_control_kernel__prohibition_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITIES (SNARE) — Enforcement is racially and economically stratified. Black and Latino users face incarceration at 3-5x rates despite equivalent or lower use. Poor communities experience intensive policing; wealthy communities receive treatment referrals. Trapped intergenerationally — parental incarceration creates poverty, limited opportunity, and trauma-based vulnerability to use. No collective exit; individual exit requires leaving community or radical identity shift.
constraint_indexing:constraint_classification(substance_control_kernel__prohibition_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: TREATMENT PROVIDERS (TANGLED ROPE) — Experience coordination function (preventing overdose, treating comorbidities) combined with extraction (restricted prescribing protocols, liability for patient use, mandatory reporting obligations). Genuine benefit exists — stable funding, professional authority — but constrained by criminal law's override of clinical judgment. Exit is costly (licensing loss, malpractice liability) but possible (relocate to harm-reduction jurisdiction). Moderate experienced extraction.
constraint_indexing:constraint_classification(substance_control_kernel__prohibition_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ENFORCEMENT APPARATUS (ROPE) — Primary beneficiary. Experiences constraint as coordination mechanism solving a 'drug problem' through criminalization. Receives budget, personnel authority, and legal immunity. Arbitrage exit available — can shift enforcement priorities or relocate resources. Pure coordination from this perspective: the apparatus exists to execute the prohibition reading, and does so effectively. No experienced extraction because apparatus IS the beneficiary.
constraint_indexing:constraint_classification(substance_control_kernel__prohibition_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PHARMA/REGULATORY COMPLEX (ROPE) — Benefit from prohibition by eliminating black-market competition and maintaining patent monopolies on opioid medications (morphine, oxycodone) and addiction treatments (methadone, buprenorphine). Exit available through political influence. Coordination function: managing substance supply through regulated distribution. Pure benefit — no experienced extraction because the apparatus serves their interests.
constraint_indexing:constraint_classification(substance_control_kernel__prohibition_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: HARM REDUCTION ADVOCATES (SCAFFOLD) — See prohibition as a temporary constraint being dismantled through evidence-based policy (syringe services programs, medication-assisted treatment, supervised consumption sites, drug policy reform). Constrained by legal barriers and funding limits, but organized coalitions are building alternative authorization structures. Theater is present (harm reduction programs operate at margins, performing compliance while subverting core logic). Sunset logic: as evidence accumulates and legalization spreads, prohibition loses legitimacy. Low effective extraction from this perspective because advocates have coalition power and see an exit pathway.
constraint_indexing:constraint_classification(substance_control_kernel__prohibition_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: INTERNATIONAL REGIME (PITON) — UN Single Convention on Narcotic Drugs (1961) enshrines prohibition as international law. Institutional inertia maintains the regime despite decades of evidence showing it produces worse public health outcomes than legalization/decriminalization. Theater is high: international compliance largely performative (countries maintain nominal prohibition while tolerating de facto drug markets). Authority derives from lineage (treaty inheritance) not current function. Primary function (controlling drug supply) has failed; maintained through institutional pathway dependence and mutual enforcement obligations. Degraded constraint.
constraint_indexing:constraint_classification(substance_control_kernel__prohibition_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURALIZED MORALITY (MOUNTAIN) — Risk classification as natural law when viewing from civilizational scope. The prohibition reading grounds itself in a claim about moral order: certain substances are inherently corrupting, and state punishment is inherent to maintaining social cohesion. This perspective naturalizes the constraint as arising from immutable facts about human nature and social necessity. However, the beneficiary/victim structure contradicts the mountain classification — the constraint produces selective extraction from powerless agents while benefiting institutional actors. False summit candidate: the 'natural law' framing conceals a political choice with identifiable winners and losers.
constraint_indexing:constraint_classification(substance_control_kernel__prohibition_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__prohibition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(substance_control_kernel__prohibition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(substance_control_kernel__prohibition_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(substance_control_kernel__prohibition_reading, TR),
    TR >= 0.70.

:- end_tests(substance_control_kernel__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The prohibition reading systematically extracts from users through criminalization, incarceration, asset seizure, and permanent legal disability. The constraint targets identity (substance use defines the victim set), not behavior alone — users cannot stop being 'substance abusers' in the eyes of law even after abstinence. Extraction extends beyond the primary victim (users) to secondary victims (families, communities, treatment professionals) through surveillance obligations, asset civil forfeiture, and child custody loss. The rising trajectory (0.42 → 0.68 over 30 years) reflects acceleration of enforcement scope and intensity while core drug prevalence remained stable — the increase is not driven by increased drug use but by policy choice to criminalize more thoroughly. Suppression (0.78): High. Criminal penalties (incarceration, asset seizure), social penalties (employment discrimination, housing denial, voting restrictions), medical penalties (restricted pain medication access), and family law penalties (custody loss, parental rights termination) create multiple redundant suppression mechanisms. Alternatives to compliance are structurally eliminated — treatment access is restricted (medications like buprenorphine tightly controlled), harm reduction is criminalized (syringe possession), and use itself is made punishable regardless of harm or risk. Theater (0.65): Moderate-high. Enforcement intensity has increased while drug use prevalence has not declined, and 'drug war' successes are increasingly announced absent corresponding public health improvement. Enforcement focuses on quantity metrics (arrests, seizures, convictions) rather than outcome metrics (overdose reduction, treatment access, recidivism). Asset forfeiture operates under 'civil forfeiture' theater — framing property seizure as non-punitive despite functioning as punishment without due process. Mandatory minimums and three-strikes sentencing perform 'toughness' without empirical deterrence justification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The enforcement apparatus sees pure coordination (Rope) — executing the policy as designed. Marginalized communities see pure extraction (Snare) — criminal targeting without correlation to harm. Treatment providers see mixed burden and benefit (Tangled Rope) — genuine coordination function (preventing overdose) constrained by prosecution liability. Harm reduction advocates see a temporary constraint with a sunset (Scaffold) — policy momentum is shifting toward treatment and legalization as evidence accumulates. The international regime sees institutional inertia (Piton) — enforcement is nominal compliance, not functional control. The analytical observer risks seeing natural law (Mountain) — framing prohibition as inherent to social order — but the structural data reveals false summit: identifiable institutional beneficiaries (police, incarceration industries, pharmaceutical monopolies) profit from the constraint, which is incompatible with natural law classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's position in the extraction flow. Users are full victims (d → 1.0) with no exit options (trapped) — maximum f(d). Their experienced extractiveness is highest. Marginalized communities are full victims (d → 1.0) with generational lock-in (trapped) — equivalently high extraction. Treatment providers are mixed (d → 0.55): they benefit from funding and professional authority but constrained by clinical restrictions and liability risk. Law enforcement and incarceration industries are beneficiaries (d → 0.05 to 0.15): they profit from the constraint and have arbitrage options. The derivation chain produces high χ for powerless agents (victim status + trapped exit) and low χ for institutional beneficiaries (beneficiary status + arbitrage exit). This perspectival gap is the diagnostic signature of a Snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the three readings are ALTERNATIVE KERNELS, not perspectival variations of a single constraint. The Deferential Realism framework handles this through the reading_relations structure: each reading makes a claim about what the kernel IS (substance use as moral transgression vs. medical condition vs. personal choice). If the empirical question 'is substance use a medical condition?' is resolved (high confidence: yes), then the harm_reduction_reading gains authority (axiom_overriding drift in favor of treatment framing) and the prohibition reading's moral_transgression_framing axiom is overridden. The engine computes which reading's authority structure survives drift accumulation. This is not a perspectival gap within one constraint but a sequence of competing kernels. The prohibition reading remains internally coherent — it is not self-contradictory. Its vulnerability is axiom-level: the founding moral_transgression_framing has been empirically undermined by decades of neuroscientific evidence, public health data showing treatment is more effective than punishment, and jurisdictional evidence (Portugal, Canada) showing decriminalization reduces harms. The mandate is resolved by tracking axiom_overriding drift (empirical challenges to the moral transgression premise) and authority_erosion (medical, public health, and legal authorities shifting away from the prohibition reading).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_transgression_versus_medical_condition,
    'Is substance use fundamentally a moral failure requiring punishment, or a medical/neurobiological condition requiring treatment?',
    'Neuroscientific evidence on addiction pathophysiology; comparison of public health outcomes (mortality, morbidity, recidivism) between prohibition-based systems and treatment-based systems; examination of whether punishment-based deterrence works for substance use behavior',
    'If moral transgression: prohibition reading remains coherent and ε remains ~0.68. If medical condition: constraint reclassifies to harm_reduction_reading (ε ~0.35, Tangled Rope) because coercion becomes instrumentally incoherent (punishment does not treat disease). This is axiom_overriding drift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_transgression_versus_medical_condition, empirical, 'Foundational premise: substance use as moral transgression vs. medical condition').

omega_variable(
    black_market_violence_structural_or_contingent,
    'Is black market violence (cartel formation, gang activity, turf wars) inherent to prohibition enforcement, or contingent on enforcement intensity?',
    'Comparative analysis of violence rates in prohibition vs. legalization regimes (Portugal, Netherlands, Canada); examination of violence dynamics when enforcement intensity increases vs. decreases; analysis of violence in legal black markets (alcohol Prohibition era)',
    'If structural: prohibition reading acknowledges violence as necessary externality, reclassifying to pure Snare (ε ~0.78, suppression ~0.85) because harm-creation becomes explicit goal. If contingent: harm reduction reading gains legitimacy — lower enforcement intensity and treatment focus reduces violence without abandoning all constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_violence_structural_or_contingent, empirical, 'Whether black market violence is structural feature or enforcement-intensity dependent').

omega_variable(
    state_monopoly_versus_market_efficiency,
    'Can the state enforce uniform prohibition across all jurisdictions and populations, or does the prohibition reading structurally require tolerance for de facto markets?',
    'Empirical tracking of enforcement capacity: ratio of incarcerated population to total users; examination of persistent drug markets in high-enforcement regimes; analysis of enforcement disparities by jurisdiction and community',
    'If state monopoly is impossible: prohibition reading fails at empirical delivery — the constraint cannot achieve its stated goal and relies on theatrical enforcement. Theater_ratio should increase, potentially triggering Piton classification. If possible: enforcement apparatus confirmation and higher extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_monopoly_versus_market_efficiency, empirical, 'Whether state monopoly enforcement of prohibition is structurally achievable').

omega_variable(
    alternative_kernel_readings_foreclosure,
    'Does the prohibition reading''s core axiom (moral_transgression_framing) logically foreclose the harm_reduction_reading and legalization_reading, or do they coexist as live alternatives?',
    'Logical analysis: does acceptance of moral transgression premise require rejection of either harm reduction or legalization? Can a framework hold that use is morally problematic AND treatment/decriminalization is the appropriate response? Can a framework hold that use is morally problematic AND legalization serves social order better than prohibition?',
    'If forecloses: reading_relations are ''forecloses'' (rare, strong claim). If coexists: reading_relations are ''coexists_with'' (standard for live normative disputes). Engine computes terminal attractor based on axiom_overriding drift and authority_erosion to determine which reading''s kernel wins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_kernel_readings_foreclosure, conceptual, 'Logical relationship between prohibition and alternative substance control readings').

omega_variable(
    identity_locked_bifurcation_in_enforcement,
    'Are enforcement agents (police, prosecutors, judges) caught in identity_locked status where their professional identity depends on prohibition framing, preventing perception of alternatives?',
    'Interviews/surveys of enforcement professionals; analysis of career trajectories of those who shift to harm reduction; examination of institutional culture resistance to policy change; comparison of jurisdictional shift rates after legalization adoption',
    'If significant identity lock: enforcement perspective should shift from institutional/arbitrage to institutional/identity_locked, changing directionality derivation and potentially lowering perceived χ for this agent (the agent experiences constraint as less extractive because their identity IS the constraint). This would refine the beneficiary profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_bifurcation_in_enforcement, empirical, 'Degree of identity fusion in enforcement personnel relative to prohibition reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__prohibition_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sckpr_tr_t0, substance_control_kernel__prohibition_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(sckpr_tr_t15, substance_control_kernel__prohibition_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(sckpr_tr_t30, substance_control_kernel__prohibition_reading, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(sckpr_be_t0, substance_control_kernel__prohibition_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sckpr_be_t15, substance_control_kernel__prohibition_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(sckpr_be_t30, substance_control_kernel__prohibition_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sckpr_su_t0, substance_control_kernel__prohibition_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(sckpr_su_t15, substance_control_kernel__prohibition_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(sckpr_su_t30, substance_control_kernel__prohibition_reading, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__legalization_reading).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, incarceration_extraction_mechanism).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, pharmaceutical_monopoly_opioid).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, racial_disparities_criminal_enforcement).

% DUAL FORMULATION NOTE:
% The substance_control_kernel contains three structurally distinct readings, each corresponding to a separate constraint story. The prohibition reading (this file, ε=0.68, Snare) is upstream of the harm_reduction_reading (ε=0.35, Tangled Rope) and legalization_reading (ε=0.15, Rope) in the sense that prohibition policy is what harm reduction advocates react against and what legalization advocates seek to replace. All three stories share the same kernel (the contested claim about what substance use is and what response serves public welfare) but differ in their axioms, authority grounding, and derived constraint structure. They do not offer different perspectives on the same constraint — they are different constraints arising from different readings of the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_kernel__prohibition_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
