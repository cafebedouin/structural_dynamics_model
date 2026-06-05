% ============================================================================
% CONSTRAINT STORY: criminal_discovery_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_criminal_discovery_asymmetry, []).

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
 *   constraint_id: criminal_discovery_asymmetry
 *   human_readable: Criminal Discovery Asymmetry in Adversarial Justice
 *   domain: law/criminal_procedure/institutional_asymmetry
 *
 * SUMMARY:
 *   Criminal discovery asymmetry in adversarial justice systems creates a
 *   structural tension between the state's investigative authority and the
 *   defendant's right to access exculpatory evidence. The prosecution
 *   possesses institutional advantages in evidence gathering (subpoena power,
 *   forensic resources, law enforcement access), evidence analysis (crime
 *   laboratories, expert networks), and evidence withholding (prosecutorial
 *   discretion, classification authority). The defendant, especially if
 *   indigent, cannot match these investigative resources and depends on the
 *   state to provide access to exculpatory evidence through discovery
 *   obligations. This constraint exhibits characteristics of a Tangled Rope:
 *   genuine coordination need exists (adversarial process requires both sides
 *   to present evidence for truth-seeking) alongside asymmetric extraction
 *   (prosecution benefits from information advantage and can withhold
 *   evidence under weak materiality standards). The theater ratio reflects
 *   that formal discovery compliance (Brady disclosures, discovery rules)
 *   creates the appearance of fairness without ensuring information parity —
 *   prosecutors can comply formally while withholding substantively through
 *   classification, timing, or materiality disputes. The constraint's
 *   extractiveness has increased over the measurement interval as case
 *   complexity has grown and public defender funding has stagnated relative
 *   to prosecution resources.
 *
 * KEY AGENTS:
 *   - Prosecution State Apparatus: Primary beneficiary (institutional/arbitrage) — retains investigative advantage, evidence-withholding discretion, and prosecutorial charging leverage
 *   - Indigent Defendant: Primary victim (powerless/trapped) — cannot afford investigative resources, cannot compel discovery, constrained by state information asymmetry
 *   - Adequately Resourced Defendant: Secondary victim (moderate/constrained) — can hire investigators and challenge discovery but faces higher costs and legal risk than prosecution
 *   - Trial Fairness Principle: Structural victim (powerless/trapped) — abstract ideal of adversarial truth-seeking that cannot organize or exit; undermined by asymmetric evidence access
 *   - Defense Resource Parity: Victim category (powerless/trapped) — practical goal of ensuring defendants can assess evidence against them; constrained by funding and expertise asymmetry
 *   - Criminal Defense Bar: Organized reform agent (organized/constrained) — advocates for discovery reform through Brady precedents, innocence projects, and conviction integrity mechanisms
 *   - Analytical Observer: Civilization view (analytical/analytical) — risks naturalizing prosecutorial advantage as inherent to state-individual dynamics rather than institutional design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(criminal_discovery_asymmetry, 0.58).
domain_priors:suppression_score(criminal_discovery_asymmetry, 0.68).
domain_priors:theater_ratio(criminal_discovery_asymmetry, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(criminal_discovery_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(criminal_discovery_asymmetry, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(criminal_discovery_asymmetry, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(criminal_discovery_asymmetry, tangled_rope).
narrative_ontology:human_readable(criminal_discovery_asymmetry, "Criminal Discovery Asymmetry in Adversarial Justice").
narrative_ontology:topic_domain(criminal_discovery_asymmetry, "law/criminal_procedure/institutional_asymmetry").

domain_priors:requires_active_enforcement(criminal_discovery_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(criminal_discovery_asymmetry, prosecution_state_apparatus).
narrative_ontology:constraint_victim(criminal_discovery_asymmetry, defendant_access_to_exculpatory_evidence).
narrative_ontology:constraint_victim(criminal_discovery_asymmetry, defense_resource_parity).
narrative_ontology:constraint_victim(criminal_discovery_asymmetry, trial_fairness_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENT DEFENDANT (SNARE) — Structurally trapped: cannot exit the criminal process, cannot afford private counsel with investigative resources, cannot compel discovery or challenge suppression without legal expertise. Bears maximum extraction: denied access to exculpatory evidence, faces conviction based on incomplete information, constrained by state resource asymmetry. The constraint operates as pure extraction with minimal coordination benefit — the defendant experiences only the coercive machinery.
constraint_indexing:constraint_classification(criminal_discovery_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ADEQUATELY RESOURCED DEFENDANT (TANGLED ROPE) — Constrained by legal risk and costs but has resources to hire investigators, discovery specialists, and appellate counsel. Can partially access exculpatory evidence through discovery motions and expert analysis. Experiences genuine coordination (adversarial process functions when both sides can participate) alongside asymmetric extraction (prosecution retains investigative and withholding advantages). Mixed structure: some extraction, some coordination.
constraint_indexing:constraint_classification(criminal_discovery_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROSECUTION STATE APPARATUS (ROPE) — Experiences the discovery asymmetry as coordination mechanism: the unequal information access enables state prosecutors to conduct efficient criminal investigation without the overhead of sharing all findings. Arbitrage exit: state can appeal to different jurisdictions, negotiate charges, or shift resources. Net beneficiary of the constraint — extraction flows toward state apparatus. From this perspective, the asymmetry solves a real coordination problem: investigative resource concentration and prosecutorial discretion.
constraint_indexing:constraint_classification(criminal_discovery_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CRIMINAL DEFENSE BAR AND REFORM MOVEMENT (SCAFFOLD) — Organized agents (public defender associations, innocence projects, criminal justice reform coalitions) perceive discovery asymmetry as a temporary institutional failure with a sunset. Brady and Giglio precedents establish reciprocal discovery obligations; digital evidence preservation, open-source forensics, and prosecutorial discipline mechanisms create pathways toward parity. Reform organizations see the asymmetry as a degraded state of an otherwise viable system, with structural solutions emerging: open case files, conviction integrity units, data exoneration databases. Constrained by legal precedent but have agency through legislation and norm-setting.
constraint_indexing:constraint_classification(criminal_discovery_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DISCOVERY DISCLOSURE RITUAL (PITON) — The formal discovery process (Brady disclosures, discovery rules, prosecutor obligations) is substantially performative. Prosecutors can comply with discovery rules while withholding exculpatory evidence through classification, discretionary framing ('not material'), or simple delay. The ritual persists through institutional inertia — court calendars, practice habits, bar association training — despite low functional effectiveness at equalizing information access. High theater: formal compliance without substantive parity. Primary function has atrophied (ensuring fair trial) while the ritual form remains.
constraint_indexing:constraint_classification(criminal_discovery_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some investigative asymmetry may appear inherent to the criminal justice structure itself: the state has subpoena power, arrest authority, and forensic resources that private defendants cannot replicate. Information asymmetry could be naturalized as an immutable structural feature of state-vs-individual dynamics. However, the structural data contradicts this mountain classification — empirical examples of more equitable discovery systems (Japan, Germany, many Commonwealth jurisdictions) demonstrate that investigative asymmetry is contingent, not immutable. The mountain framing naturalizes a particular institutional choice, not a law of nature.
constraint_indexing:constraint_classification(criminal_discovery_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(criminal_discovery_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(criminal_discovery_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(criminal_discovery_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(criminal_discovery_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(criminal_discovery_asymmetry, TR),
    TR >= 0.70.

:- end_tests(criminal_discovery_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The prosecution captures significant extraction through information advantage, evidence withholding discretion, and plea bargaining leverage enabled by discovery asymmetry. However, the extraction is not absolute — defense counsel with adequate resources can access substantial evidence through discovery motions, expert analysis, and appeals. The asymmetry is structural rather than total. Suppression (0.68): Moderate-high. Significant barriers to defense access include: resource requirements for independent investigation, discovery rules that enable prosecutorial withholding under materiality standards, case complexity outpacing public defender capacity, and plea pressure that forecloses trial where discovery gaps would become visible. But suppression is not total — appellate review, Brady precedents, and reform organizations are reducing barriers. Theater ratio (0.64): Moderate-high. Formal discovery compliance (Brady disclosures, discovery scheduling orders, prosecutor certifications) creates appearance of fairness without ensuring substantive parity. Prosecutors can comply with discovery rules while withholding through classification, framing, or delay. The ritual persists through legal procedure and practice norms despite low functional effectiveness at equalizing evidence access.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival disagreement on type classification. The prosecution experiences the asymmetry as a coordination mechanism enabling efficient investigation — from their perspective, discovery rules balance the need to investigate without compromising ongoing cases. The adequately resourced defendant experiences mixed coordination and extraction — the adversarial process works when both sides have investigative resources, but the asymmetry still favors prosecution. The indigent defendant experiences pure extraction — they cannot assess evidence against them, cannot mount a defense, and face conviction based on incomplete information. The reform bar sees a degraded but solvable system: Brady precedents, innocence projects, and open-file policies represent a sunset for discovery asymmetry, with structural solutions emerging. The discovery ritual itself (formal Brady compliance, discovery scheduling) is largely performative — it creates legal appearance of fairness while allowing substantive withholding. The analytical observer risks naturalizing prosecutorial advantage as inherent to state power rather than a contingent institutional design choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The prosecution's structural position yields low directionality (d ≈ 0.15-0.25): beneficiary status with institutional power and arbitrage exit options means the state experiences negative effective extraction — the asymmetry flows toward the state as benefit. The indigent defendant's position yields high directionality (d ≈ 0.90-0.95): victim status with powerless position and trapped exit options (cannot exit the criminal process) means maximum experienced extraction — the asymmetry flows away from the defendant as cost. The adequately resourced defendant occupies a middle position (d ≈ 0.60-0.70): victim status but with constrained rather than trapped exit (can hire counsel, appeal) and moderate power (can pay for investigation). The discovery asymmetry creates perspectival gaps: the prosecution sees coordination (necessity of state efficiency), the defense bar sees a solvable temporary problem (scaffold with reform sunset), and the indigent defendant sees pure extraction with no escape.
 *
 * MANDATROPHY ANALYSIS:
 *   Criminal discovery asymmetry resolves the mandatrophy by showing that classification depends entirely on the observer's structural position relative to the extraction flow and their power to shape outcomes. The prosecution's rope classification reflects genuine coordination need: adversarial truth-seeking requires both sides to present evidence. The indigent defendant's snare classification reflects structural reality: they cannot exit, cannot compel discovery, and bear full cost of information asymmetry. The scaffold perspective identifies real structural reform (open-file policies, conviction integrity units, prosecutorial discipline) that would reduce extraction as norms mature. The piton perspective correctly identifies performative compliance (formal discovery rules that enable informal withholding). The mountain perspective is a false summit: it naturalizes institutional choice (prosecutorial discretion, case complexity, resource inequality) as inherent to criminal justice rather than recognizing these as design variables subject to reform. The mandatrophy is not resolved by picking a single type but by recognizing that discovery asymmetry is genuinely a Tangled Rope — it coordinates adversarial truth-seeking while extracting from defendants with asymmetric resources.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exculpatory_materiality_definition,
    'What standard determines whether withheld evidence is materially exculpatory? Does the Giglio/Brady materiality threshold enable prosecutors to withhold evidence that would change outcomes in specific cases?',
    'Empirical analysis of case outcomes when withheld evidence is later disclosed; comparison of conviction rates under different materiality standards across jurisdictions; DNA exoneration database correlation with discovery violations',
    'If materiality threshold is too high: prosecutors can withhold evidence under color of compliance, maintaining asymmetry. If threshold is lower: discovery obligations meaningfully constrain state advantage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exculpatory_materiality_definition, empirical, 'Materiality standard for exculpatory evidence disclosure').

omega_variable(
    prosecutorial_discipline_effectiveness,
    'Do existing disciplinary mechanisms (bar sanctions, case dismissal, Brady sanctions) actually deter prosecutorial discovery violations, or are penalties too weak relative to the extraction benefit?',
    'Analysis of prosecutor discipline records; correlation between penalty severity and violation frequency; comparison of jurisdictions with strong vs weak discovery enforcement',
    'If discipline is ineffective: extraction mechanism persists despite formal rules. If effective: constraint may be transitioning from Snare to Scaffold as enforcement improves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prosecutorial_discipline_effectiveness, empirical, 'Whether prosecutorial discipline mechanisms deter discovery violations').

omega_variable(
    resource_parity_threshold,
    'What level of defense resource investment achieves meaningful discovery parity with prosecution, and what percentage of defendants can afford it?',
    'Cost accounting of adequate defense investigation; correlation between defense spending and trial outcomes; accessibility analysis of public defender funding vs private counsel spending',
    'If parity requires high spending: discovery asymmetry persists as class-based extraction. If parity is achievable with moderate spending: reform mechanisms could address through public defender funding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_parity_threshold, empirical, 'Resource investment required for discovery parity').

omega_variable(
    plea_pressure_mechanism,
    'To what degree does discovery asymmetry function as a plea-bargaining coercion mechanism? Do defendants accept guilty pleas because they cannot assess the evidence against them rather than because the evidence is strong?',
    'Comparative analysis of plea rates under different discovery regimes; defendant interviews on plea decision factors; study of outcomes when defendants gain full discovery access before plea negotiations',
    'If discovery asymmetry drives plea coercion: constraint mechanism is primarily extractive suppression, not legitimate prosecution coordination. If plea decisions are independent of discovery access: extraction component is lower than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plea_pressure_mechanism, empirical, 'Whether discovery asymmetry functions as plea coercion mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(criminal_discovery_asymmetry, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crda_tr_t0, criminal_discovery_asymmetry, theater_ratio, 0, 0.55).
narrative_ontology:measurement(crda_tr_t15, criminal_discovery_asymmetry, theater_ratio, 15, 0.6).
narrative_ontology:measurement(crda_tr_t30, criminal_discovery_asymmetry, theater_ratio, 30, 0.64).

% Extraction over time
narrative_ontology:measurement(crda_be_t0, criminal_discovery_asymmetry, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(crda_be_t15, criminal_discovery_asymmetry, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(crda_be_t30, criminal_discovery_asymmetry, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(criminal_discovery_asymmetry, enforcement_mechanism).
narrative_ontology:affects_constraint(criminal_discovery_asymmetry, plea_bargaining_coercion).
narrative_ontology:affects_constraint(criminal_discovery_asymmetry, indigent_defense_underfunding).
narrative_ontology:affects_constraint(criminal_discovery_asymmetry, prosecutorial_discretion_concentration).

% DUAL FORMULATION NOTE:
% Criminal discovery asymmetry is structurally upstream of plea bargaining coercion (discovery asymmetry enables plea pressure) and prosecutorial discretion concentration (discovery asymmetry enables selective prosecution). Distinct from indigent defense underfunding (which is a separate resource allocation constraint) but causally linked: discovery asymmetry is worse when defense resources are scarce.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(criminal_discovery_asymmetry, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
