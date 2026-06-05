% ============================================================================
% CONSTRAINT STORY: twenty_second_amendment__lame_duck_cost_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_twenty_second_amendment__lame_duck_cost_reading, []).

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
 *   constraint_id: twenty_second_amendment__lame_duck_cost_reading
 *   human_readable: Twenty-Second Amendment Lame Duck Cost Reading: Loss of Electoral Discipline in Second Term
 *   domain: constitutional_law/executive_power
 *
 * SUMMARY:
 *   This constraint is the lame-duck-cost reading of the Twenty-Second
 *   Amendment: the structural claim that presidential ineligibility for a
 *   third term removes the re-election incentive, thereby suppressing a
 *   mechanism of electoral accountability precisely when presidential power
 *   peaks. The amendment trades away presidential responsiveness to voters in
 *   exchange for a term limit that prevents any single executive from
 *   accumulating unlimited power. This reading treats the amendment not as a
 *   safeguard against dictatorship (the anti-Caesarism reading) nor as a
 *   violation of voter choice (the democratic-choice-objection reading), but
 *   as a specific institutional mechanism that extracts a cost: Congress
 *   gains leverage late-term when the president cannot appeal to future
 *   election outcomes; the president's second-term agenda loses negotiating
 *   capacity; and the electorate loses the re-election threat as an
 *   accountability tool. The measurement trajectory shows extractiveness
 *   rising sharply after re-election (time=8) as Congressional leverage
 *   increases and the president's options narrow. Suppression rises
 *   correspondingly: once the president's ineligibility is public and
 *   certain, alternative mechanisms that depend on the re-election incentive
 *   cease to function.
 *
 * KEY AGENTS:
 *   - Second-Term Presidential Agenda (Victim): The set of policy priorities the president aims to achieve in years 5-8. Structurally trapped by loss of electoral leverage.
 *   - Congress (Beneficiary): Institutional actor that gains negotiating leverage as re-election incentive disappears. Can extract concessions or block agenda with lower cost.
 *   - Voter Electorate (Mixed): Gains anti-Caesarism safeguard but loses the re-election threat as a mechanism for enforcing presidential responsiveness to campaign promises.
 *   - Democratic Reform Coalition (Organized agent): Advocates for or against constitutional change; sees the amendment as revisable through Article V (scaffold logic).
 *   - Post-War Constitutional Order (Institutional): The regime that inherited the amendment as a vestigial enforcement of the Washington norm, now maintained through institutional inertia.
 *   - Analytical Observer (Civilizational): Risk of naturalizing a contingent constitutional choice as immutable law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(twenty_second_amendment__lame_duck_cost_reading, 0.52).
domain_priors:suppression_score(twenty_second_amendment__lame_duck_cost_reading, 0.48).
domain_priors:theater_ratio(twenty_second_amendment__lame_duck_cost_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(twenty_second_amendment__lame_duck_cost_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(twenty_second_amendment__lame_duck_cost_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(twenty_second_amendment__lame_duck_cost_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(twenty_second_amendment__lame_duck_cost_reading, tangled_rope).
narrative_ontology:human_readable(twenty_second_amendment__lame_duck_cost_reading, "Twenty-Second Amendment Lame Duck Cost Reading: Loss of Electoral Discipline in Second Term").
narrative_ontology:topic_domain(twenty_second_amendment__lame_duck_cost_reading, "constitutional_law/executive_power").

domain_priors:requires_active_enforcement(twenty_second_amendment__lame_duck_cost_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(twenty_second_amendment__lame_duck_cost_reading, '252bfcf1-ce01-4fb5-b6f8-828ab55f01c7').
narrative_ontology:cs_kernel_codification('252bfcf1-ce01-4fb5-b6f8-828ab55f01c7', formalized).
narrative_ontology:cs_authority_grounding('252bfcf1-ce01-4fb5-b6f8-828ab55f01c7', lineage).
narrative_ontology:cs_interpretation_layer_present('252bfcf1-ce01-4fb5-b6f8-828ab55f01c7').
narrative_ontology:cs_reading_relation('252bfcf1-ce01-4fb5-b6f8-828ab55f01c7', twenty_second_amendment__anti_caesarism_reading, coexists_with).
narrative_ontology:cs_reading_relation('252bfcf1-ce01-4fb5-b6f8-828ab55f01c7', twenty_second_amendment__democratic_choice_objection_reading, coexists_with).
narrative_ontology:cs_axiom('252bfcf1-ce01-4fb5-b6f8-828ab55f01c7', foundational, electoral_incentive_is_accountability_mechanism).
narrative_ontology:cs_axiom_status(electoral_incentive_is_accountability_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('252bfcf1-ce01-4fb5-b6f8-828ab55f01c7', electoral_incentive_is_accountability_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('252bfcf1-ce01-4fb5-b6f8-828ab55f01c7', foundational, suppression_of_reelection_threat_extracts_from_second_term_agenda).
narrative_ontology:cs_axiom_status(suppression_of_reelection_threat_extracts_from_second_term_agenda, holdable).
narrative_ontology:cs_axiom_grounding('252bfcf1-ce01-4fb5-b6f8-828ab55f01c7', suppression_of_reelection_threat_extracts_from_second_term_agenda, deontological).
narrative_ontology:cs_reference_frame('252bfcf1-ce01-4fb5-b6f8-828ab55f01c7', electoral_accountability_regime).
narrative_ontology:cs_drift_state('252bfcf1-ce01-4fb5-b6f8-828ab55f01c7', contemporary_executive_power, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('252bfcf1-ce01-4fb5-b6f8-828ab55f01c7', '').
narrative_ontology:cs_kernel_id(twenty_second_amendment__lame_duck_cost_reading, twenty_second_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(twenty_second_amendment__lame_duck_cost_reading, congress_late_term_leverage).
narrative_ontology:constraint_victim(twenty_second_amendment__lame_duck_cost_reading, second_term_agenda).
narrative_ontology:constraint_victim(twenty_second_amendment__lame_duck_cost_reading, electoral_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SECOND-TERM AGENDA (SNARE) — The president's legislative priorities are structurally trapped once re-election is impossible. The extraction mechanism is the removal of the re-election threat: Congress negotiates knowing the president cannot appeal to voters in the next election. The agenda bears the full cost of electoral discipline's removal.
constraint_indexing:constraint_classification(twenty_second_amendment__lame_duck_cost_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONGRESSIONAL LEADERSHIP LATE-TERM (ROPE) — Congress experiences the amendment as pure coordination: the term limit creates a predictable window when presidential leverage is lowest, enabling legislators to pursue independent agendas without fear of electoral retaliation. Congress benefits from coordination enabled by the term limit's timing.
constraint_indexing:constraint_classification(twenty_second_amendment__lame_duck_cost_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: VOTER ELECTORATE IN ELECTION YEAR (TANGLED ROPE) — Voters can remove the president via election (exit option: mobile in theory). But the amendment suppresses the re-election incentive, removing one mechanism through which presidents are accountable to voter preferences. Voters benefit from the term limit's anti-Caesarism safeguard but lose leverage after the second election.
constraint_indexing:constraint_classification(twenty_second_amendment__lame_duck_cost_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DEMOCRATIC REFORM COALITION (SCAFFOLD) — The two-term limit, like all constitutional structures, is revisable through Article V amendment (sunset logic: amendment is procedurally available, even if politically rare). Low effective extraction because the mechanism has an exit pathway through democratic process, even if the pathway is costly.
constraint_indexing:constraint_classification(twenty_second_amendment__lame_duck_cost_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: POST-WAR CONSTITUTIONAL ORDER (PITON) — The amendment persists as a vestigial enforcement of a norm that once broke (the Washington tradition against third terms, shattered by Roosevelt). The limit maintains itself through inertia—the original anti-Caesarism justification is now decoupled from its active enforcement. The text performs a role (preventing future Roosevelts) that has become largely theatrical, sustained by the founding myth rather than by current structural necessity.
constraint_indexing:constraint_classification(twenty_second_amendment__lame_duck_cost_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER NATURAL LAW VIEW (MOUNTAIN) — From the civilizational view, the two-term limit appears as an immutable constitutional law: written into the text, entrenched through amendment, impossible to change without another amendment, therefore effectively unchangeable. This perspective naturalizes the amendment as a fixed feature of the American political structure. However, the structural data—identifiable beneficiaries (Congress), identifiable victims (second-term agendas), and measurable suppression (removal of re-election incentive)—indicates a false summit: the 'immutability' is constitutional convention, not natural law.
constraint_indexing:constraint_classification(twenty_second_amendment__lame_duck_cost_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(twenty_second_amendment__lame_duck_cost_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(twenty_second_amendment__lame_duck_cost_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(twenty_second_amendment__lame_duck_cost_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(twenty_second_amendment__lame_duck_cost_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(twenty_second_amendment__lame_duck_cost_reading, TR),
    TR >= 0.70.

:- end_tests(twenty_second_amendment__lame_duck_cost_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The amendment's core mechanism is the removal of the re-election incentive, which measurably increases Congressional leverage in the second term. The measurement trajectory shows extractiveness rising from 0.18 at inauguration to 0.58 at the end of term, sharply accelerating after re-election (time=8). This is not maximal extraction (which would approach 0.70+) because the president retains other tools: direct executive action, judicial appointments, control over bureaucratic resources. But the extraction is real and structural: Congress negotiates from a stronger position when the president cannot threaten electoral retaliation. Suppression (0.48): Moderate. The re-election incentive is suppressed entirely once ineligibility is constitutionally certain. But suppression is not total: the president retains other forms of leverage (party loyalty, executive authority, appointive power). Theater ratio (0.38): Relatively low. The mechanism is functional rather than performative—the loss of leverage is a direct consequence of constitutional ineligibility, not a theatrical ritual. The text performs its intended function: preventing a third term actually does remove one source of presidential negotiating power.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a substantial perspectival gap between the institutional view (Congress as beneficiary, rope classification) and the second-term view (agenda as victim, snare classification). Congress sees the amendment as enabling coordination—they can pursue independent priorities once the presidential re-election threat is gone. The second-term agenda sees the same structure as extraction: the president's bargaining power evaporates at the moment their legislative agenda is most ambitious. The voter electorate occupies a middle position, gaining the anti-Caesarism safeguard but losing one tool of electoral accountability. The analytical observer risks naturalizing the amendment as an immutable constitutional law, obscuring the fact that the 'natural' two-term limit was a contingent Washington norm that broke under Roosevelt and had to be written into law—a constructed institutional choice, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   The mechanism of directed extraction: The amendment's structural function is to create a moment—the second term—when Congressional negotiating leverage peaks because the president cannot appeal to future election outcomes. This is not hidden or performative; it is the direct consequence of the rule. Congress is the beneficiary because it gains leverage. The second-term agenda is the victim because it loses negotiating capacity. The electorate is in an ambiguous position: they gain a safeguard against Caesarism but lose a tool of accountability. The directionality computation reflects this asymmetry: institutional agents with arbitrage options (Congress, able to find leverage elsewhere) experience low d; the second-term agenda, which exists only through the president and cannot exit, experiences high d.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by showing that Tangled Rope is the appropriate classification. The amendment contains both a genuine coordination function (anti-Caesarism safeguard, preventing the concentration of power in a single executive across unlimited terms) AND asymmetric extraction (Congress gains leverage, the second-term agenda loses it, electoral accountability is suppressed). The reading is not trying to claim the amendment is pure extraction—it acknowledges the coordination benefit. But it insists that the cost of the coordination (the loss of re-election incentive as an accountability tool) is asymmetrically borne, and that this asymmetry rises to the level of extraction. The snare classification from the second-term perspective is also defensible—from the agenda's point of view, the constraint is purely extractive (all downside, no upside). The rope classification from Congress's perspective is also defensible—from Congress's point of view, the constraint is pure coordination benefit. The tangled rope is the systemic claim: the amendment exhibits both functions simultaneously, and both are real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lame_duck_severity_threshold,
    'At what point in a second term does the president''s leverage collapse below the first-term baseline?',
    'Comparative historical analysis of legislative success rates, veto overrides, and Congressional defection rates in first vs second terms; isolation of the announcement effect (does leverage decline immediately after re-election, or gradually?)',
    'If collapse is immediate and total: extractiveness approaches 0.70+ (snare). If gradual: extractiveness is accurately modeled at 0.52 (tangled rope with functional presidency mid-term). If the president retains leverage (popularity, fundraising for allies), the extraction cost is lower than modeled.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lame_duck_severity_threshold, empirical, 'Timeline and severity of second-term leverage collapse').

omega_variable(
    electoral_vs_constitutional_accountability,
    'Is electoral accountability (threat of non-re-election) a structural component of presidential accountability, or merely one instrument among several?',
    'Comparative constitutional analysis: correlation between removal of re-election incentive and measurable changes in Congressional oversight, impeachment risk, executive restraint, and legislative cooperation. Does the removal of the electoral check produce measurable shifts in accountability structures?',
    'If electoral accountability is structural: the amendment''s removal of the re-election threat constitutes genuine extraction from the accountability regime (victimizing the system of checks and balances). If accountability is robust without the electoral threat: the extraction cost is lower or misidentified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_vs_constitutional_accountability, conceptual, 'Whether electoral accountability is essential or supplementary to constitutional oversight').

omega_variable(
    reading_contest_boundary,
    'Does this reading (lame duck cost) logically foreclose the anti-Caesarism reading, or do they coexist as different frames on the same institutional fact?',
    'Philosophical analysis: can one framework simultaneously hold that (a) term limits are necessary anti-Caesarism measures AND (b) term limits extract accountability by removing re-election incentive? Or does accepting (b) require rejecting the core justification (a)?',
    'If they foreclose: only one reading can be authoritative. If they coexist: the amendment instantiates a real tradeoff that anti-Caesarism advocates must acknowledge. The classification of the relation (forecloses vs coexists_with) hinges on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_boundary, conceptual, 'Whether lame-duck-cost and anti-Caesarism readings are logically incompatible or genuinely distinct frames').

omega_variable(
    amendment_revision_barrier,
    'Is the Article V amendment procedure sufficiently accessible that the term limit is genuinely revisable, or is it effectively entrenched beyond democratic change?',
    'Historical analysis of Article V success rates; modeling of coalition requirements for term-limit repeal; comparison to other super-majoritarian barriers. Assess whether ''revisable through amendment'' is a meaningful exit pathway or a theoretical fiction.',
    'If revisable: scaffold perspective is accurate, and the constraint has genuine sunset logic. If effectively entrenched: the constraint is closer to mountain (immutable) than scaffold (temporary). This affects the classification of the Constitutional Reform Coalition perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_revision_barrier, empirical, 'Practical revisability of the Twenty-Second Amendment through Article V').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(twenty_second_amendment__lame_duck_cost_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsa_lame_theater_inauguration, twenty_second_amendment__lame_duck_cost_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(tsa_lame_theater_midterm, twenty_second_amendment__lame_duck_cost_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(tsa_lame_theater_endterm, twenty_second_amendment__lame_duck_cost_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(tsa_lame_extractiveness_inauguration, twenty_second_amendment__lame_duck_cost_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(tsa_lame_extractiveness_midterm, twenty_second_amendment__lame_duck_cost_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(tsa_lame_extractiveness_reelection, twenty_second_amendment__lame_duck_cost_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(tsa_lame_extractiveness_endterm, twenty_second_amendment__lame_duck_cost_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tsa_lame_suppression_inauguration, twenty_second_amendment__lame_duck_cost_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(tsa_lame_suppression_reelection, twenty_second_amendment__lame_duck_cost_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(tsa_lame_suppression_endterm, twenty_second_amendment__lame_duck_cost_reading, suppression_requirement, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(twenty_second_amendment__lame_duck_cost_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(twenty_second_amendment__lame_duck_cost_reading, twenty_second_amendment__anti_caesarism_reading).
narrative_ontology:affects_constraint(twenty_second_amendment__lame_duck_cost_reading, twenty_second_amendment__democratic_choice_objection_reading).

% DUAL FORMULATION NOTE:
% The Twenty-Second Amendment kernel has three constraint stories, one for each reading. The lame_duck_cost_reading focuses on the loss of electoral accountability in the second term, decomposing the amendment into a specific extraction mechanism. The anti_caesarism_reading focuses on the safeguard against unlimited power accumulation. The democratic_choice_objection_reading focuses on the violation of voter sovereignty. All three operate on the same text; each identifies a different structural effect and a different beneficiary/victim set. They are linked as siblings within the kernel_id Twenty-Second Amendment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
