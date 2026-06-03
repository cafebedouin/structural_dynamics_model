% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__prohibition_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substance_control_legitimacy__prohibition_reading
 *   human_readable: Substance Control Legitimacy (Prohibition Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The prohibition reading of substance control legitimacy grounds state
 *   authority in a moral duty to prevent the inherent harms of substance use
 *   through criminalization. This constraint story instantiates ONE reading
 *   of a contested kernel — the claim that substance use is a problem
 *   requiring state intervention. Three structurally distinct readings
 *   coexist: (1) the prohibition_reading (this story) claims criminalization
 *   is the proper authority mechanism; (2) the harm_reduction_reading claims
 *   public health is the proper mechanism without criminalization; (3) the
 *   legalization_reading claims individual autonomy limits state authority to
 *   third-party harms. Each reading presupposes different beneficiaries,
 *   victims, and extraction flows. The prohibition reading classifies as
 *   SNARE from the user and community perspectives (high extraction, minimal
 *   coordination benefit, high suppression) and ROPE from the enforcement
 *   apparatus perspective (coordination of agency interests, arbitrage
 *   options). The classification profile reveals that the prohibition
 *   constraint extracts heavily from criminalized populations while
 *   coordinating the interests of enforcement institutions and industries
 *   that benefit from criminalization. The analytical observer risks viewing
 *   this as a natural law (immutable governance requirement) but the
 *   structural data demonstrates false-summit characteristics: identifiable
 *   beneficiaries (enforcement apparatus, pharmaceutical monopolies, carceral
 *   industries), high suppression maintained through coercive machinery, and
 *   alternative control mechanisms that function without criminalization
 *   (harm reduction, treatment, legalization).
 *
 * KEY AGENTS:
 *   - Substance Users: Primary victim (powerless/trapped) — maximum extraction via criminal penalties, incarceration, asset seizure, collateral consequences. No exit except use discontinuation (medically difficult) or evasion (requires mobility).
 *   - Criminalized Communities: Secondary victim (moderate/constrained) — targeted enforcement, mass incarceration, family dissolution, police violence, economic disinvestment. Exit via geographic mobility (costly) or organizing (constrained by enforcement itself).
 *   - Enforcement Apparatus: Primary beneficiary (institutional/arbitrage) — law enforcement, prosecutors, prisons, regulatory agencies benefit through budget expansion, organizational legitimacy, career advancement. Coordinates agency interests; arbitrage options available.
 *   - Pharmaceutical and Carceral Industries: Powerful beneficiary (powerful/arbitrage) — private prisons, pharmaceutical manufacturers, security contractors extract from criminalized populations and protected markets. Genuinely profit from both enforcement and criminalization.
 *   - Drug Policy Reform Coalitions: Organized actors (organized/constrained) — harm reduction advocates, public health agencies, some law enforcement perceive prohibition as temporary/solvable through policy change. Constrained by political opposition but have exit logic.
 *   - Moral Condemnation Ritual: Institutional maintenance (institutional/arbitrage) — the rhetorical and bureaucratic machinery maintaining the claim that criminalization prevents harm despite contrary evidence. Persists through inertia.
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent institutional architecture as immutable governance necessity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, 0.68).
domain_priors:suppression_score(substance_control_legitimacy__prohibition_reading, 0.72).
domain_priors:theater_ratio(substance_control_legitimacy__prohibition_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_legitimacy__prohibition_reading, "Substance Control Legitimacy (Prohibition Reading)").
narrative_ontology:topic_domain(substance_control_legitimacy__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__prohibition_reading, 'edfea4dc-f367-4d6e-bd97-3b62bd3dec42').
narrative_ontology:cs_kernel_codification('edfea4dc-f367-4d6e-bd97-3b62bd3dec42', distributed).
narrative_ontology:cs_authority_grounding('edfea4dc-f367-4d6e-bd97-3b62bd3dec42', extraction).
narrative_ontology:cs_reading_relation('edfea4dc-f367-4d6e-bd97-3b62bd3dec42', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('edfea4dc-f367-4d6e-bd97-3b62bd3dec42', substance_control_legitimacy__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('edfea4dc-f367-4d6e-bd97-3b62bd3dec42', foundational, substance_use_inherently_harmful).
narrative_ontology:cs_axiom_status(substance_use_inherently_harmful, overridden).
narrative_ontology:cs_axiom_grounding('edfea4dc-f367-4d6e-bd97-3b62bd3dec42', substance_use_inherently_harmful, empirically_contingent).
narrative_ontology:cs_axiom('edfea4dc-f367-4d6e-bd97-3b62bd3dec42', foundational, moral_duty_criminalization).
narrative_ontology:cs_axiom_status(moral_duty_criminalization, holdable).
narrative_ontology:cs_axiom_grounding('edfea4dc-f367-4d6e-bd97-3b62bd3dec42', moral_duty_criminalization, deontological).
narrative_ontology:cs_reference_frame('edfea4dc-f367-4d6e-bd97-3b62bd3dec42', harm_prevention_through_criminalization).
narrative_ontology:cs_drift_state('edfea4dc-f367-4d6e-bd97-3b62bd3dec42', contemporary_evidence_accumulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('edfea4dc-f367-4d6e-bd97-3b62bd3dec42', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, enforcement_apparatus).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, pharmaceutical_monopolies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, private_prison_operators).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, certain_political_coalitions).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, criminalized_populations).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, black_market_violence_casualties).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, public_health_outcomes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CRIMINALIZED USER (SNARE) — Substance use disorder is treated as a criminal matter. Users face maximum coercive extraction: criminal penalties, incarceration, asset seizure, collateral consequences (employment, housing, family). Exit from the constraint requires either discontinuing use (medically difficult without treatment) or evading law enforcement (requires geographic mobility or corruption). Most users lack both. The constraint extracts through enforcement machinery with minimal coordination benefit — the user sees only punishment, not care.
constraint_indexing:constraint_classification(substance_control_legitimacy__prohibition_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AFFECTED COMMUNITY (SNARE) — Communities targeted by enforcement (low-income neighborhoods, communities of color, rural areas experiencing opioid mortality) bear the costs of criminalization: mass incarceration, family dissolution, police violence, economic disinvestment. Communities can exit through geographic mobility (costly, requires capital) or political organizing (constrained by enforcement itself). The extraction is severe but not absolute — some organized resistance exists.
constraint_indexing:constraint_classification(substance_control_legitimacy__prohibition_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ENFORCEMENT APPARATUS (ROPE) — Law enforcement, prosecutors, prisons, and related institutions benefit from the constraint through budget justification, organizational expansion, political legitimacy, and career advancement. From this institutional perspective, the constraint solves a coordination problem: enforcing drug laws requires coordination across agencies, standardization of penalties, and institutional turf-sharing. The apparatus experiences the constraint as a functional mechanism, not as extraction — its beneficiaries have arbitrage options (can redirect enforcement capacity elsewhere) and see the system as legitimate.
constraint_indexing:constraint_classification(substance_control_legitimacy__prohibition_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PHARMACEUTICAL & CARCERAL INDUSTRIES (TANGLED ROPE) — Powerful institutional actors (pharmaceutical manufacturers, private prison operators, security contractors) benefit from criminalization. The constraint coordinates their interests (protecting prescription opioid markets from generic competition, maintaining incarceration rates) while extracting from users through monopoly pricing and captive populations. These actors have genuine arbitrage options and exit capacity but are deeply invested in prohibition architecture. Their perspective exhibits both coordination of their own interests and extraction from dependent populations.
constraint_indexing:constraint_classification(substance_control_legitimacy__prohibition_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DRUG POLICY REFORM COALITIONS (SCAFFOLD) — Organized movements (harm reduction advocates, public health agencies, criminal justice reformers, some law enforcement) perceive the prohibition constraint as temporary and solvable through policy change. They frame criminalization as a sunset mechanism: as treatment infrastructure scales and public health narratives displace criminal narratives, prohibition loses legitimacy and enforcement capacity. This perspective is constrained by political opposition but has exit logic — the organizations can transition to alternative roles if prohibition ends. Theater remains moderate because the debate is openly contested.
constraint_indexing:constraint_classification(substance_control_legitimacy__prohibition_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MORAL CONDEMNATION RITUAL (PITON) — Viewed civilizationally, the prohibition reading rests on an increasingly degraded moral claim: that substance use is inherently harmful and state criminalization prevents harm. Empirical evidence contradicts this (harm reduction works; criminalization increases overdose deaths, incarceration does not reduce use). Yet the moral condemnation ritual persists through institutional inertia, conservative framing, and political coalitions invested in maintenance. The theater ratio is moderate (0.55) because the claim is still actively contested — not fully performative like a fully inert ritual, but substantially maintained against contradictory evidence.
constraint_indexing:constraint_classification(substance_control_legitimacy__prohibition_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, substance use disorder might appear as an immutable problem requiring state intervention: humans are biologically susceptible to addiction, addiction causes suffering, preventing addiction is a natural state function. This perspective sees criminalization as inherent to governance. However, the structural data contradicts the mountain classification: powerful beneficiaries exist (enforcement apparatus, pharmaceutical industries), suppression is high and maintained through coercive machinery, and alternative control mechanisms exist and function (treatment, harm reduction). This is a false summit — the constraint is legitimate institutional architecture, not natural law.
constraint_indexing:constraint_classification(substance_control_legitimacy__prohibition_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__prohibition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(substance_control_legitimacy__prohibition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(substance_control_legitimacy__prohibition_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(substance_control_legitimacy__prohibition_reading, TR),
    TR >= 0.70.

:- end_tests(substance_control_legitimacy__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Users and criminalized communities face severe extraction through criminal penalties, incarceration (the U.S. incarcerates ~450,000 people for drug offenses), asset seizure, employment/housing discrimination, family separation, and healthcare barriers. The extraction is not absolute (some users avoid detection, some communities resist) but is systematically directed at the population denominator. Suppression (0.72): Very high. Multiple suppression mechanisms operate: criminal penalties create fear and compliance; lack of treatment access (many jurisdictions prioritize enforcement over treatment); geographic/economic barriers to exit; legal prohibition on alternative harm reduction services (needle exchanges, supervised consumption sites); propaganda against alternative readings (drug users are criminals, not patients). Enforcement machinery is designed and funded to suppress alternatives. Theater ratio (0.55): Moderate. The prohibition reading is still actively contested — not fully performative like a completely inert ritual, but significantly theatrical. The moral claim (substance use is inherently harmful, criminalization prevents harm) is maintained against substantial contrary evidence. The theater has increased over the measurement interval as empirical contradictions accumulate but institutional maintenance persists. The measurements show rising extractiveness (0.45→0.68) and rising suppression (0.58→0.72) over 20 years, indicating that as alternative readings become more salient, the prohibition reading requires more aggressive enforcement to maintain its legitimacy — a characteristic sign of a constraint that is increasingly recognized as extraction rather than natural governance.
 *
 * PERSPECTIVAL GAP:
 *   The prohibition_reading exhibits a profound perspectival gap. Users see criminalization as pure extraction (snare) with no coordination benefit — the constraint exists to punish them, not to solve a problem they share. The enforcement apparatus sees the same constraint as solving a genuine coordination problem (rope) — how should law enforcement be organized, funded, and legitimized? Both observations are structurally sound: criminalization DOES extract from users AND DOES coordinate enforcement agency interests. These are not contradictory — they are extractive coordination (tangled_rope). The false summit emerges when the analytical observer naturalizes this arrangement as an immutable requirement of governance. The constraint IS the institutional architecture (criminalization as authority mechanism); it is not a law of nature. The perspectival gap reveals that the question 'does prohibition prevent harm?' cannot be answered from the enforcement apparatus perspective (they experience coordination, not verification) — the answer requires the user perspective (does it prevent MY harm? no, it causes harm) or the population health perspective (does criminalization reduce population-level substance use disorder? no, it increases overdose mortality by preventing treatment access). The gap routes truth about the constraint through the victim perspective, not the beneficiary perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   The prohibition_reading distributes directionality across perspectives based on structural position and exit capacity. Users with no exit (trapped) and no beneficiary status experience high d → high f(d) → high χ. Communities with constrained exit and victim status experience moderate-high d. Enforcement apparatus with arbitrage exit and beneficiary status experiences low d → negative/low f(d) → they experience the constraint as coordination (rope). Powerful industries with arbitrage options and strong beneficiary status experience very low d but have high absolute power, so they coordinate around the constraint. The analytical observer at civilizational scope with analytical exit experiences high d (as observer of the constraint, not participant in it) and high f(d). The perspectival gap is maximal: users experience snare (extraction), enforcement experiences rope (coordination), industries experience tangled_rope (coordination of their interests + extraction from others), reform coalitions experience scaffold (temporary problem), and the moral ritual appears as piton (degraded performance). No single directionality value unifies these perspectives — the gap IS the diagnostic signal.
 *
 * MANDATROPHY ANALYSIS:
 *   The prohibition_reading triggers mandatrophy because the core axiom (substance_use_is_inherently_harmful) and the authority grounding (moral_duty_to_prevent_harm_through_criminalization) are empirically contested at high confidence. The mandatrophy resolves through acknowledging that the prohibition reading and harm_reduction reading represent genuinely different COMMITMENTS about what state authority should do, not just different empirical predictions. If the axiom (inherent harm) were empirically false but the reading remained committed to criminalization anyway, that would expose the reading as extractive rationalization — the constraint would reclassify toward pure snare. The resolution path: (1) acknowledge that empirical evidence (40+ years of comparative policy data from Portugal, Switzerland, indigenous practices, modern legalization outcomes) consistently contradicts the inherent-harm axiom; (2) recognize that harm reduction produces better health outcomes at lower cost and with less extraction; (3) allow that different parties may hold different commitments about whether autonomy or harm-prevention should be primary — this is a value choice, not an empirical one; (4) classify the prohibition_reading as a choice by some institutional actors to prioritize harm-prevention-by-criminalization despite its empirical ineffectiveness, which makes it a snare. The mandatrophy is resolved by separating empirical claims (what works) from normative commitments (what we prioritize) and recognizing that the prohibition reading's empirical claims are overridden while its normative commitment remains a live position for certain actors. This makes it an overridden-axiom constraint — still active but intellectually indefensible on its own grounds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_causation_mechanism,
    'Is substance use inherently harmful, or is harm primarily caused by criminalization, contamination of unregulated supply, and lack of medical supervision?',
    'Comparative outcome analysis: mortality, overdose, infectious disease, and incarceration rates in jurisdictions with prohibition vs. harm reduction vs. legalization models; longitudinal tracking of same populations under policy shifts (Portugal, Switzerland, Colorado post-legalization); clinical studies of medical supervised use vs. street use.',
    'If harm is inherent: prohibition reading maintains coherence. If harm is primarily criminalization-caused: classification shifts to snare across all perspectives (high extraction without coordination benefit); false summit in analytical view becomes explicit; prohibition reading''s core axiom (inherent_harm) is overridden by empirical evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_causation_mechanism, empirical, 'Whether substance use harm is inherent or criminalization-caused').

omega_variable(
    state_duty_grounding,
    'Does state authority to criminalize rest on a moral duty to prevent self-harm, and if so, is this duty consistently applied across substances and harms?',
    'Doctrinal analysis: examine legal/constitutional principles of paternalism and state duty in jurisdictions claiming prohibition authority; cross-domain comparison (alcohol, tobacco, overeating, sedentary lifestyle, extreme sports); identify whether prohibition is consistently applied or selectively targeted.',
    'If state duty to prevent harm is genuine and consistent: prohibition reading''s authority_grounding (moral_duty_to_prevent_harm) is defensible. If applied inconsistently (criminalizing some substances/harms while permitting others): grounding is exposed as pretextual; classification shifts toward snare (extraction disguised as harm prevention).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_duty_grounding, conceptual, 'Whether state paternalism duty grounds prohibition consistently').

omega_variable(
    enforcement_apparatus_extraction,
    'How much of the enforcement apparatus''s interest in maintaining prohibition derives from genuine public health goals vs. institutional survival and budgetary incentives?',
    'Comparative analysis: jurisdictions with genuine harm reduction funding vs. enforcement budgets; historical tracking of enforcement spending and officer counts during prohibition implementation vs. post-legalization transition; surveys and interviews with enforcement leadership about institutional priorities.',
    'If enforcement is extractive (driven primarily by institutional incentives): snare classification across all victim perspectives is confirmed; tangled_rope classification for powerful industries shifts toward pure snare. If enforcement is genuinely harm-reduction motivated: prohibition reading''s beneficiary set is incorrect (enforcement apparatus is not extractive beneficiary but harm-prevention coordinator).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_apparatus_extraction, empirical, 'Enforcement apparatus extractive motivation vs. public health motivation').

omega_variable(
    black_market_externality_causation,
    'Is black market violence a side effect of prohibition or an inherent feature of unregulated substance markets?',
    'Comparison of black market violence rates in legalized vs. criminalized jurisdictions; analysis of supply chain violence in regulated (alcohol, tobacco) vs. unregulated markets; price and purity volatility data comparing prohibition vs. legalization.',
    'If black market violence is prohibition-caused: suppression value increases (enforcement creates secondary victims); snare classification is reinforced. If violence is inherent to unregulated markets: prohibition reading''s harm prevention goal is partially validated (criminalization prevents worse outcomes); classification shifts toward tangled_rope (genuine coordination function plus extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_externality_causation, empirical, 'Black market violence causation: prohibition-caused vs. inherent').

omega_variable(
    reading_foreclosure_under_harm_reduction,
    'Can the prohibition reading coexist with the harm_reduction_reading within a single governance framework, or does adopting harm reduction logically foreclose prohibition?',
    'Institutional analysis: examine jurisdictions attempting both frameworks simultaneously (e.g., Portugal''s partial decriminalization + enforcement for dealers; some U.S. cities with harm reduction + prosecution); identify whether they represent distinct policy layers or genuine coexistence, or whether adoption of harm reduction empirically undermines prohibition''s legitimacy.',
    'If readings coexist: they are sibling positions competing in pluralistic governance. If harm reduction logically forecloses prohibition (by demonstrating it unnecessary and counterproductive): reading_relations value shifts from coexists_with to forecloses; axiom_overriding mechanism activates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_under_harm_reduction, conceptual, 'Whether harm_reduction_reading forecloses prohibition_reading').

omega_variable(
    axiom_empirical_override,
    'Has empirical evidence (40+ years of comparative policy data) sufficiently overridden the foundational axiom that criminalization prevents harm?',
    'Meta-analysis of drug policy research; policy effectiveness literature (Portugal outcomes, Swiss heroin-assisted treatment, U.S. state-level legalization impacts, global WHO guidance); tracking of axiom_status within policy institutions and academic consensus.',
    'If axiom is overridden: axiom status shifts from holdable to overridden; prohibition_reading_axiom_override drift_state activates at substantial/severe magnitude; authority_grounding erodes from moral_duty to extraction-driven maintenance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_empirical_override, empirical, 'Empirical override of criminalization-prevents-harm axiom').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__prohibition_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subst_proh_tr_t0, substance_control_legitimacy__prohibition_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(subst_proh_tr_t10, substance_control_legitimacy__prohibition_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(subst_proh_tr_t20, substance_control_legitimacy__prohibition_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(subst_proh_be_t0, substance_control_legitimacy__prohibition_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(subst_proh_be_t10, substance_control_legitimacy__prohibition_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(subst_proh_be_t20, substance_control_legitimacy__prohibition_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(subst_proh_su_t0, substance_control_legitimacy__prohibition_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(subst_proh_su_t10, substance_control_legitimacy__prohibition_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(subst_proh_su_t20, substance_control_legitimacy__prohibition_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__legalization_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, mass_incarceration_economics).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, pharmaceutical_opioid_monopoly).

% DUAL FORMULATION NOTE:
% The substance_control_legitimacy kernel decomposes into three structurally distinct constraints: prohibition_reading (this story, ε=0.68, snare), harm_reduction_reading (ε~0.30, rope/scaffold), legalization_reading (ε~0.22, rope). Each reading presupposes different beneficiary/victim sets and different authority groundings. They are not different perspectives on one constraint — they are different constraints sharing a contested kernel. All three are linked as a family via network.affects_constraints to enable contamination analysis: as one reading gains institutional authority, it exerts pressure on the others' classification and legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
