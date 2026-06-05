% ============================================================================
% CONSTRAINT STORY: warrant_preference_reading__exclusionary_rule_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_warrant_exclusionary_rule, []).

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
 *   constraint_id: warrant_preference_reading__exclusionary_rule_reading
 *   human_readable: The Exclusionary Rule as Constitutional Enforcement Mechanism
 *   domain: constitutional_law/fourth_amendment
 *
 * SUMMARY:
 *   This constraint story instantiates the exclusionary rule reading of the
 *   warrant preference kernel. The exclusionary rule is the Fourth
 *   Amendment's only self-executing remedy: when police conduct an
 *   unconstitutional search, courts suppress the evidence obtained, rendering
 *   it inadmissible at trial. This reading holds that suppression is not
 *   merely a remedial option but THE mechanism through which the Fourth
 *   Amendment becomes enforceable against state actors. The reading emerged
 *   from Weeks v. United States (1914) and was nationalized by Mapp v. Ohio
 *   (1961). It is contested by two sibling readings: the digital carpenter
 *   reading (Carpenter v. United States, 2018 — aggregation of digital data
 *   constitutes a difference in kind triggering higher Fourth Amendment
 *   protection despite third-party doctrine) and the good-faith exception
 *   reading (United States v. Leon, 1984 — officers acting in good faith
 *   reliance on a warrant do not trigger suppression because suppression
 *   punishes only when it would deter). The exclusionary rule reading
 *   constitutes itself through the structural claim that suppression costs
 *   (loss of probative evidence, case dismissals) create the deterrent
 *   incentive for police to obtain warrants, and that this deterrent is both
 *   necessary and sufficient for Fourth Amendment enforcement.
 *
 * KEY AGENTS:
 *   - Defendants in suppression motions (powerless/trapped): Primary beneficiary of exclusionary rule — the rule is their only remedy. No alternative mechanism exists to undo the search or compensate the violation.
 *   - Law enforcement agencies (organized/constrained): Both victim and beneficiary. Victim: suppression costs them probative evidence and convictions. Beneficiary: suppression doctrine incentivizes institutional adoption of constitutional protocols, training, and warrant procedures.
 *   - Probative evidence from unconstitutional search (powerless/trapped): Primary victim — suppressed entirely from trial; no exit option.
 *   - Appellate courts (institutional/arbitrage): Authority structure enforcing the reading. Has arbitrage options: narrow suppression doctrine via exceptions, expand standing requirements, adjust remedy scope.
 *   - Prosecution / case strength (powerless/trapped): Victim — reduced conviction rates due to evidence suppression; no exit except through changing police conduct (which is the intended effect).
 *   - Exclusionary rule reform advocates (organized/constrained): Scaffold perspective — see suppression as temporary mechanism with sunset path toward alternative remedies (civil damages, administrative discipline, federal exclusion standards).
 *   - Analytical observer (analytical/analytical): Doctrinal perspective — sees the constraint as necessarily tangled coordination-and-extraction mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(warrant_preference_reading__exclusionary_rule_reading, 0.58).
domain_priors:suppression_score(warrant_preference_reading__exclusionary_rule_reading, 0.72).
domain_priors:theater_ratio(warrant_preference_reading__exclusionary_rule_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(warrant_preference_reading__exclusionary_rule_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(warrant_preference_reading__exclusionary_rule_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(warrant_preference_reading__exclusionary_rule_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(warrant_preference_reading__exclusionary_rule_reading, tangled_rope).
narrative_ontology:human_readable(warrant_preference_reading__exclusionary_rule_reading, "The Exclusionary Rule as Constitutional Enforcement Mechanism").
narrative_ontology:topic_domain(warrant_preference_reading__exclusionary_rule_reading, "constitutional_law/fourth_amendment").

domain_priors:requires_active_enforcement(warrant_preference_reading__exclusionary_rule_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(warrant_preference_reading__exclusionary_rule_reading, 'b4640b3c-8ebc-4ef3-a5b3-0f3bfdf93934').
narrative_ontology:cs_kernel_codification('b4640b3c-8ebc-4ef3-a5b3-0f3bfdf93934', formalized).
narrative_ontology:cs_authority_grounding('b4640b3c-8ebc-4ef3-a5b3-0f3bfdf93934', lineage).
narrative_ontology:cs_interpretation_layer_present('b4640b3c-8ebc-4ef3-a5b3-0f3bfdf93934').
narrative_ontology:cs_reading_relation('b4640b3c-8ebc-4ef3-a5b3-0f3bfdf93934', warrant_preference_reading__digital_carpenter_reading, coexists_with).
narrative_ontology:cs_reading_relation('b4640b3c-8ebc-4ef3-a5b3-0f3bfdf93934', warrant_preference_reading__good_faith_exception_reading, coexists_with).
narrative_ontology:cs_axiom('b4640b3c-8ebc-4ef3-a5b3-0f3bfdf93934', foundational, suppression_necessary_for_fourth_amendment_enforcement).
narrative_ontology:cs_axiom_status(suppression_necessary_for_fourth_amendment_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('b4640b3c-8ebc-4ef3-a5b3-0f3bfdf93934', suppression_necessary_for_fourth_amendment_enforcement, instrumental).
narrative_ontology:cs_axiom('b4640b3c-8ebc-4ef3-a5b3-0f3bfdf93934', foundational, evidence_suppression_creates_deterrent_for_unconstitutional_search).
narrative_ontology:cs_axiom_status(evidence_suppression_creates_deterrent_for_unconstitutional_search, holdable).
narrative_ontology:cs_axiom_grounding('b4640b3c-8ebc-4ef3-a5b3-0f3bfdf93934', evidence_suppression_creates_deterrent_for_unconstitutional_search, empirically_contingent).
narrative_ontology:cs_reference_frame('b4640b3c-8ebc-4ef3-a5b3-0f3bfdf93934', mapp_v_ohio_suppression_doctrine).
narrative_ontology:cs_drift_state('b4640b3c-8ebc-4ef3-a5b3-0f3bfdf93934', contemporary_post_leon_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b4640b3c-8ebc-4ef3-a5b3-0f3bfdf93934', '2026-02-27T14:32:15Z').
narrative_ontology:cs_kernel_id(warrant_preference_reading__exclusionary_rule_reading, warrant_preference_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(warrant_preference_reading__exclusionary_rule_reading, defendants_in_suppression_motions).
narrative_ontology:constraint_beneficiary(warrant_preference_reading__exclusionary_rule_reading, law_enforcement_agencies_deterred_by_suppression_cost).
narrative_ontology:constraint_victim(warrant_preference_reading__exclusionary_rule_reading, probative_evidence_users).
narrative_ontology:constraint_victim(warrant_preference_reading__exclusionary_rule_reading, prosecution_case_strength).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEFENDANT IN SUPPRESSION MOTION (SNARE) — Trapped within the criminal process; the exclusionary rule is their only remedy for unconstitutional search. No alternative exit mechanism exists. Maximum experienced extraction of the violation itself — the unconstitutional seizure has occurred, the evidence has been obtained, and the defendant must prove the violation AND demonstrate standing AND file a suppression motion to activate any remedy. Theater ratio reflects the suppression hearing's courtroom ritual, but the underlying extraction (privacy violation, bodily intrusion, home invasion, etc.) is non-performative and irreversible.
constraint_indexing:constraint_classification(warrant_preference_reading__exclusionary_rule_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LAW ENFORCEMENT AGENCIES (TANGLED ROPE) — Constrained by suppression doctrine: violations cost them probative evidence and case dismissals. But the doctrine also coordinates law enforcement practice — the threat of suppression (and public embarrassment, institutional scrutiny) provides the incentive to develop constitutional alternatives and train officers on warrant requirements. The constraint operates as both extraction (suppression penalties) and coordination (deterrence incentive to institutionalize constitutional compliance). Not pure snare because the extraction mechanism (loss of evidence) is also the coordination mechanism (creates incentive for constitutional protocol adoption).
constraint_indexing:constraint_classification(warrant_preference_reading__exclusionary_rule_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROBATIVE EVIDENCE / PROSECUTION'S INVESTIGATIVE FRUITS (SNARE) — The evidence obtained through unconstitutional search has no exit option; suppression removes it from the factfinding process entirely. The constraint's core mechanism is extracting probative value from the trial by rendering constitutionally tainted evidence inadmissible. This is the literal trap: once seized unlawfully, the evidence cannot be un-seized. Suppression is the only remedy, but it operates by suppressing the evidence itself, not by restoring the epistemic status quo ante.
constraint_indexing:constraint_classification(warrant_preference_reading__exclusionary_rule_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: APPELLATE COURTS / DOCTRINE ENFORCEMENT (ROPE) — Courts experience the exclusionary rule as a coordination mechanism for stabilizing constitutional doctrine. Suppression decisions establish precedent, train lower courts, and signal to law enforcement which search practices trigger exclusion. Courts have arbitrage options: they can adjust suppression doctrine through exceptions (good faith, inevitable discovery), can narrowly construe standing requirements, or can expand warrant categories. Courts see the constraint as coordination of constitutional practice, not as extraction from them.
constraint_indexing:constraint_classification(warrant_preference_reading__exclusionary_rule_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: EXCLUSIONARY RULE REFORM ADVOCATES (SCAFFOLD) — Organized actors (criminal procedure reformers, federal rulemakers, some law enforcement leadership) see the exclusionary rule as a temporary mechanism with a sunset path: civil remedies (Bivens actions, state tort law, administrative discipline), deterrence alternatives (federal exclusion standards, state constitutional protections with different remedy structures), and restorative approaches (compensation funds, systematic monitoring). The scaffold perspective sees exclusion as effective at its moment of enactment (1961 via Mapp v. Ohio) but now facing alternatives that might achieve deterrence without trial suppression. Theater ratio is low because the underlying mechanism (suppression deterrent) is genuinely functional, but the sunset logic is aspirational — alternative remedies have proven structurally weak.
constraint_indexing:constraint_classification(warrant_preference_reading__exclusionary_rule_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SUPPRESSION HEARING AS INSTITUTIONAL INERTIA (PITON) — The suppression motion has become a formalized courtroom ritual (suppression hearing, Franks hearing on officer credibility, evidentiary challenges) that persists largely through institutional momentum. The actual barrier-to-use (filing deadline, procedural complexity, requirement to establish standing and prove causation) means many defendants never invoke it. The hearing is performative — it involves elaborate testimony about search procedures, warrant affidavits, and police practices, but the outcome is often predetermined by doctrine (good faith exception, qualified immunity doctrines borrowed into suppression contexts). Theater ratio high; functional verification capacity low.
constraint_indexing:constraint_classification(warrant_preference_reading__exclusionary_rule_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / DOCTRINAL STRUCTURE (TANGLED ROPE) — From a civilizational/doctrinal perspective, the exclusionary rule coordinates two distinct constitutional functions: it deters unconstitutional search (coordination mechanism) and it removes poisoned evidence from trials (extraction mechanism). The rule's extractiveness is the price of constitutional enforcement — suppressing valid probative evidence to create incentives for compliance. The constraint is tangled because both functions are necessary and genuine, and they cannot be separated. Pure coordination (deterrence without suppression) would lack teeth; pure extraction (suppression without coordination purpose) would be arbitrary. The analytical observer sees the constraint as necessarily hybrid.
constraint_indexing:constraint_classification(warrant_preference_reading__exclusionary_rule_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(warrant_preference_reading__exclusionary_rule_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(warrant_preference_reading__exclusionary_rule_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(warrant_preference_reading__exclusionary_rule_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(warrant_preference_reading__exclusionary_rule_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(warrant_preference_reading__exclusionary_rule_reading, TR),
    TR >= 0.70.

:- end_tests(warrant_preference_reading__exclusionary_rule_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts probative evidence from trials (literal suppression), removes it from jury consideration, and creates case dismissals when suppression is granted. However, the extraction is not maximal because (1) many alleged Fourth Amendment violations never reach suppression hearing due to standing doctrine, guilty plea rates, and procedural barriers; (2) the extraction mechanism is intended to create a coordination incentive (deterrence), not pure rent-seeking; (3) good-faith exception and other doctrinal carve-outs limit the extraction scope. The rising trajectory (0.42 → 0.58 over 20 years) reflects doctrinal narrowing of suppression scope through good-faith exceptions, standing doctrine tightening, and institutional pressure on courts to limit suppression (post-Leon expansion of good-faith defense, Herring v. United States inevitable discovery doctrine). Suppression (0.72): High. Suppression doctrine operates through strong barriers: defendants must file suppression motions (procedural requirement), prove standing (reasonable expectation of privacy in place searched or object seized), prove violation (demonstrate search was unconstitutional), demonstrate causation (taint of evidence flows from the violation). The suppression mechanisms are both legal (procedural gates) and structural (institutional inertia, prosecutorial resistance to suppression, appellate court narrowing of the doctrine). Theater ratio (0.55): Moderate. Suppression hearings involve extensive courtroom ritual (Franks hearings on officer credibility, testimony on warrant affidavits, questioning about search procedures) with genuine functional content (courts must actually verify whether search was unconstitutional and whether officer relied on valid warrant). Theater has increased over time as good-faith exception doctrine relieves courts of close scrutiny — if officer relied on facially valid warrant, courts often skip detailed Fourth Amendment analysis (Leon test for objective reasonableness of reliance on warrant).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a sharp perspectival gap between victim and authority perspectives. Defendants and probative evidence classify as Snare (pure extraction, no escape) because suppression is their only remedy and it operates by destroying the evidence entirely. Law enforcement classifies as Tangled Rope because suppression doctrine both extracts (evidence loss) and coordinates (deters unconstitutional conduct through institutional incentive). Appellate courts classify as Rope because suppression doctrine is a coordination mechanism from their perspective — they use it to establish precedent and guide law enforcement practice. The piton perspective recognizes that suppression hearings have become ritualized with high theater and declining functional verification (good-faith exception allows courts to skip detailed Fourth Amendment analysis). The scaffold perspective sees suppression as a temporary mechanism with a sunset path toward alternative remedies. The analytical observer sees Tangled Rope — the constraint is structurally both extraction and coordination, and they cannot be separated without destroying the doctrinal function. This perspectival gap reveals the central tension in the exclusionary rule: it appears as pure extraction from the victim's position (evidence suppressed; case dismissed) and as pure coordination from the authority's position (deterrent incentive for police); neither appearance captures the hybrid structural function.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural relationship to suppression doctrine. Defendants: d ≈ 0.95 (victim of the violation; maximum d because the constraint extracts evidence from prosecution but suppression is the defendant's only remedy — the victim's benefit is the extracted evidence, but the defendant does not control this extraction). Law enforcement: d ≈ 0.50 (symmetric — suppression costs them evidence, but deterrence incentive benefits their institutional practice; medium d because the extraction-coordination hybrid operates on this agent). Probative evidence: d ≈ 1.0 (full victim — extracted entirely from trial; no agency). Courts: d ≈ 0.15 (beneficiary of institutional authority; low d because courts enforce the doctrine and have arbitrage options). Prosecution: d ≈ 0.85 (victim of evidence suppression; high d). Analytical observer: d ≈ 0.72 (canonical analytical d, derived from neutral observation position). The ascending extractiveness trajectory reflects that the good-faith exception (Leon 1984) and subsequent doctrinal narrowing have gradually reduced defendants' ability to invoke suppression (moving d downward for defendants, reducing experienced extraction benefit), while prosecution and police experience less suppression cost (moving d upward for enforcement actors, reducing deterrent pressure).
 *
 * MANDATROPHY ANALYSIS:
 *   The exclusionary rule reading resolves mandatrophy by explicitly declaring that suppression is both extraction and coordination — they are not in tension but in unity. Suppressing evidence (extraction) IS the mechanism that creates deterrence (coordination). The reading cannot avoid one mechanism without destroying the other. A pure-extraction reading would classify as Snare; a pure-coordination reading would classify as Rope. But neither is accurate because the constraint's entire function depends on the unity of the two mechanisms. The good-faith exception reading contests this by claiming suppression is pure extraction (loss of evidence) without coordination (officers relying on a warrant face no deterrent); this distinction is what allows Leon to carve out good-faith reliance. The exclusionary rule reading counters that good faith exception weakens both mechanisms: it reduces the deterrent (officers know they can rely on any facially valid warrant) and it extracts without coordinating (evidence is still suppressed, but police behavior is not changed). The mandate is resolved in the tangled_rope classification precisely because mandatrophy asks: does this constraint coordinate behavior (yes, deterrence through suppression cost) while extracting from some parties (yes, evidence extraction from prosecution and defendants)? The exclusionary rule reading says yes to both, unified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_empirical_gap,
    'Does suppression actually deter unconstitutional police conduct, or do officers and prosecutors discount the threat because defendants rarely know about or invoke suppression motions, and many violations never reach trial?',
    'Controlled comparison of suppression rates across jurisdictions with different enforcement regimes; officer surveys on how suppression doctrine shapes search decisions; correlation analysis of Fourth Amendment violation rates post-Mapp (1961) and post-Leon (1984)',
    'If suppression effectively deters: Tangled Rope classification confirmed across all perspectives. If deterrence is weak: classification shifts toward Snare (pure extraction from probative evidence) and Piton (ritual without function) from most perspectives. If deterrence is jurisdiction-dependent: constraint family decomposition required (different ε values for high-enforcement vs low-enforcement jurisdictions).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_efficacy_empirical_gap, empirical, 'Empirical evidence of suppression deterrence on police conduct').

omega_variable(
    reading_contest_digital_aggregation,
    'Does Carpenter v. United States (cell-site location history warranting despite third-party rule) foreclose the exclusionary rule reading''s foundation, or do they coexist as different doctrinal mechanisms enforcing the same Fourth Amendment core?',
    'Doctrinal analysis: Carpenter''s ''aggregation as difference in kind'' premise versus exclusionary rule''s ''remedy through suppression'' premise. Do both readings require warrant access to digital data, or does Carpenter''s premise (digital aggregation triggers heightened protection) presuppose that exclusion is the enforcement mechanism?',
    'If Carpenter forecloses exclusion (aggregation itself prevents unconstitutional search): reading_relations shift to forecloses. If Carpenter presupposes exclusion as enforcement backstop (aggregation triggers warrant requirement; exclusion enforces it): relation remains coexists_with. Affects sibling-reading network topology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_digital_aggregation, conceptual, 'Whether Carpenter reading''s aggregation premise forecloses or presupposes exclusionary rule').

omega_variable(
    axiom_overriding_good_faith_exception,
    'Has the good-faith exception (United States v. Leon, 1984) substantially overridden the exclusionary rule reading''s foundational axiom (suppression is the necessary and sufficient remedy for Fourth Amendment violation)?',
    'Doctrinal history: pre-Leon exclusionary rule doctrine vs post-Leon suppression rates; empirical measure of how frequently good-faith exception prevents suppression even when warrant is deficient or absent; judicial acknowledgment within the good-faith-exception reading that suppression is no longer the inevitable remedy.',
    'If axiom substantially overridden: drift_state direction = axiom_overriding, magnitude = substantial. If good faith exception is a narrow carve-out: direction = minor practice_drift. Affects cs_structure.axioms[].status for the foundational suppression axiom.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_overriding_good_faith_exception, empirical, 'Whether good-faith exception has overridden suppression as necessary remedy').

omega_variable(
    standing_doctrine_contingency,
    'Does the exclusionary rule reading''s efficacy depend on favorable standing doctrine (Rakas, Karo precedent on possessory interest), such that restrictive standing rules foreclose the reading''s constitutional enforcement function?',
    'Standing doctrine history: Olmstead (no privacy in contraband) → Katz (reasonable expectation of privacy) → Rakas (standing for whom?). Identify whether standing doctrine is structurally internal to the exclusionary rule mechanism or a separate gate. Measure: percentage of alleged Fourth Amendment violations that fail suppression because defendant lacks standing (no possessory interest, no reasonable expectation of privacy in location searched).',
    'If standing gate forecloses suppression for majority of claims: exclusionary rule reading becomes partially inert (Piton characteristics), and classification shifts toward Snare (extraction from probative evidence) at some perspectives. If standing doctrine is aligned with exclusionary rule''s protection scope: no drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standing_doctrine_contingency, conceptual, 'Whether standing doctrine requirements foreclose exclusionary rule''s application scope').

omega_variable(
    institutional_reading_conflict,
    'The exclusionary rule reading constitutes itself through the authority of appellate courts enforcing suppression doctrine. Does this create a built-in conflict: the authority structure (courts) depends on the remedy (suppression) for legitimacy, but the remedy''s effectiveness depends on the authority structure''s enforcement vigor?',
    'Track evolution of good-faith exception doctrine (Leon 1984, Heather v. Mack 2010, post-cellphone-location cases): Do courts progressively narrow suppression scope as the remedy''s deterrent value becomes contested? Does institutional self-interest in not excluding evidence (closing cases, supporting conviction rates) override the authority structure''s commitment to Fourth Amendment enforcement?',
    'If institutional conflict drives narrowing: drift_state direction = authority_erosion + axiom_overriding (the founding axiom of suppression as necessary remedy is eroded by the authority structure''s own interest in not using it). Reading''s reference frame is under sustained structural pressure, creating foreclosure dynamics with good-faith-exception reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_reading_conflict, conceptual, 'Institutional conflict between courts enforcing suppression and courts'' interest in not suppressing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(warrant_preference_reading__exclusionary_rule_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(warr_excl_theater_t0, warrant_preference_reading__exclusionary_rule_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(warr_excl_theater_t10, warrant_preference_reading__exclusionary_rule_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement(warr_excl_theater_t20, warrant_preference_reading__exclusionary_rule_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(warr_excl_extract_t0, warrant_preference_reading__exclusionary_rule_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(warr_excl_extract_t10, warrant_preference_reading__exclusionary_rule_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(warr_excl_extract_t20, warrant_preference_reading__exclusionary_rule_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(warr_excl_suppress_t0, warrant_preference_reading__exclusionary_rule_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(warr_excl_suppress_t10, warrant_preference_reading__exclusionary_rule_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(warr_excl_suppress_t20, warrant_preference_reading__exclusionary_rule_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(warrant_preference_reading__exclusionary_rule_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(warrant_preference_reading__exclusionary_rule_reading, digital_carpenter_reading).
narrative_ontology:affects_constraint(warrant_preference_reading__exclusionary_rule_reading, good_faith_exception_reading).
narrative_ontology:affects_constraint(warrant_preference_reading__exclusionary_rule_reading, standing_doctrine_rakas_gap).
narrative_ontology:affects_constraint(warrant_preference_reading__exclusionary_rule_reading, fourth_amendment_deterrence_empirics).

% DUAL FORMULATION NOTE:
% The warrant preference kernel contains three distinct constraint stories (readings): (1) exclusionary_rule_reading (this file) — suppression as self-executing remedy, ε=0.58, Tangled Rope; (2) digital_carpenter_reading — aggregation as difference in kind, ε=0.35, Rope/Tangled Rope boundary, enforcement through warrant requirement rather than suppression; (3) good_faith_exception_reading — officers relying on facially valid warrant cause no deterrent loss, ε=0.48, Tangled Rope with lower suppression cost. These readings have different ε values because they measure different aspects of Fourth Amendment enforcement: exclusionary rule measures suppression mechanism cost-benefit; Carpenter measures digital aggregation protection scope; good faith exception measures deterrent efficacy given officer reliance norms. Network edges link them as coexisting/contesting siblings within the same kernel. Standing doctrine and fourth amendment deterrence empirics are upstream constraints affecting all three readings' classification stability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(warrant_preference_reading__exclusionary_rule_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
