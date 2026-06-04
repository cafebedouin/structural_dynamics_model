% ============================================================================
% CONSTRAINT STORY: free_press_clause__reporters_privilege_question
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_free_press_clause__reporters_privilege_question, []).

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
 *   constraint_id: free_press_clause__reporters_privilege_question
 *   human_readable: Reporters' Privilege Question: Federal Constitutional Void, State Law Workaround
 *   domain: constitutional_law/press_freedom/evidence
 *
 * SUMMARY:
 *   Branzburg v. Hayes (1972) established that the First Amendment does not
 *   provide a blanket constitutional privilege protecting journalists from
 *   compelled testimony before grand juries about confidential sources. Yet
 *   in the forty-eight years since Branzburg, forty-nine states enacted
 *   reporters' privilege statutes, the Branzburg concurrence (Justice Powell)
 *   has functionally become the operative federal doctrine in many circuits,
 *   and federal courts regularly apply state-law privileges in federal
 *   prosecutions. This reading of the Free Press Clause instantiates a
 *   structural constraint: the constitutional void (no federal reporters'
 *   privilege) that creates maximum extraction risk for confidential sources
 *   and investigative journalists operating in federal jurisdiction, while
 *   the state-law patchwork provides partial, uneven protection. The
 *   constraint is not primarily about how much protection exists (some does,
 *   via states), but about the constitutional-level suppression of source
 *   confidentiality as a recognized federal right — the extraction mechanism
 *   is the absence of a constitutional floor, creating leverage for federal
 *   prosecutors and enabling compelled disclosure of what sources believed
 *   would be confidential. The measurement trajectory shows rising
 *   extractiveness and theater ratio from 1972 to 2023: as the federal
 *   constitutional void persists but state protections proliferate, the
 *   formal rule (Branzburg) increasingly operates as theater — the actual
 *   governing regime (state law + federal balancing tests + Branzburg
 *   concurrence) has displaced the Branzburg majority holding from practical
 *   authority while maintaining it as constitutional doctrine.
 *
 * KEY AGENTS:
 *   - Confidential Sources: Primary victims (powerless/trapped) — face compelled disclosure risk under federal jurisdiction with no constitutional protection
 *   - Investigative Journalists: Primary victims (moderate/constrained) — bear burden of protecting sources and face prosecutorial leverage to compel testimony
 *   - Grand Jury / Prosecutorial Authority: Primary beneficiary (institutional/arbitrage) — gains compulsory evidentiary access without source protection negotiation
 *   - State Legislatures: Secondary beneficiary (powerful/mobile) — enacted shield laws filling constitutional void, creating state-level coordination around source protection
 *   - Press Associations and Shield Law Coalition: Organized victims (organized/constrained) — coordinate around patchwork state protections, bear advocacy costs to maintain and expand coverage
 *   - Branzburg Precedent as Doctrine: Institutional inertia actor (institutional/arbitrage) — persists as formal constitutional law despite functional displacement by state law and balancing tests
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(free_press_clause__reporters_privilege_question, 0.52).
domain_priors:suppression_score(free_press_clause__reporters_privilege_question, 0.68).
domain_priors:theater_ratio(free_press_clause__reporters_privilege_question, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(free_press_clause__reporters_privilege_question, extractiveness, 0.52).
narrative_ontology:constraint_metric(free_press_clause__reporters_privilege_question, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(free_press_clause__reporters_privilege_question, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(free_press_clause__reporters_privilege_question, tangled_rope).
narrative_ontology:human_readable(free_press_clause__reporters_privilege_question, "Reporters' Privilege Question: Federal Constitutional Void, State Law Workaround").
narrative_ontology:topic_domain(free_press_clause__reporters_privilege_question, "constitutional_law/press_freedom/evidence").

domain_priors:requires_active_enforcement(free_press_clause__reporters_privilege_question).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(free_press_clause__reporters_privilege_question, '50eb656d-66d1-44ac-ad2b-c3e8533ea5b3').
narrative_ontology:cs_kernel_codification('50eb656d-66d1-44ac-ad2b-c3e8533ea5b3', fixed_text).
narrative_ontology:cs_authority_grounding('50eb656d-66d1-44ac-ad2b-c3e8533ea5b3', lineage).
narrative_ontology:cs_interpretation_layer_present('50eb656d-66d1-44ac-ad2b-c3e8533ea5b3').
narrative_ontology:cs_reading_relation('50eb656d-66d1-44ac-ad2b-c3e8533ea5b3', free_press_clause__press_as_technology_reading, coexists_with).
narrative_ontology:cs_reading_relation('50eb656d-66d1-44ac-ad2b-c3e8533ea5b3', free_press_clause__prior_restraint_doctrine, coexists_with).
narrative_ontology:cs_axiom('50eb656d-66d1-44ac-ad2b-c3e8533ea5b3', foundational, source_confidentiality_not_constitutional_right).
narrative_ontology:cs_axiom_status(source_confidentiality_not_constitutional_right, holdable).
narrative_ontology:cs_axiom_grounding('50eb656d-66d1-44ac-ad2b-c3e8533ea5b3', source_confidentiality_not_constitutional_right, deontological).
narrative_ontology:cs_axiom('50eb656d-66d1-44ac-ad2b-c3e8533ea5b3', foundational, grand_jury_truth_seeking_superior_to_source_protection).
narrative_ontology:cs_axiom_status(grand_jury_truth_seeking_superior_to_source_protection, holdable).
narrative_ontology:cs_axiom_grounding('50eb656d-66d1-44ac-ad2b-c3e8533ea5b3', grand_jury_truth_seeking_superior_to_source_protection, deontological).
narrative_ontology:cs_reference_frame('50eb656d-66d1-44ac-ad2b-c3e8533ea5b3', constitutional_silence_on_source_privilege).
narrative_ontology:cs_drift_state('50eb656d-66d1-44ac-ad2b-c3e8533ea5b3', contemporary_state_law_displacement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('50eb656d-66d1-44ac-ad2b-c3e8533ea5b3', '').
narrative_ontology:cs_kernel_id(free_press_clause__reporters_privilege_question, free_press_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(free_press_clause__reporters_privilege_question, grand_jury_process).
narrative_ontology:constraint_beneficiary(free_press_clause__reporters_privilege_question, prosecutorial_oversight).
narrative_ontology:constraint_victim(free_press_clause__reporters_privilege_question, confidential_source_journalism).
narrative_ontology:constraint_victim(free_press_clause__reporters_privilege_question, investigative_reporting).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONFIDENTIAL SOURCE (SNARE) — A source providing sensitive information to a reporter operating in federal jurisdiction faces an inescapable choice: risk compelled disclosure before a grand jury with no constitutional shield, or refuse to speak to the press entirely. The source has no exit option — they are structurally trapped between silence and exposure. Branzburg explicitly rejected federal constitutional protection for source confidentiality, leaving sources at the maximum structural risk when federal prosecutorial power is exercised. This perspective experiences pure extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(free_press_clause__reporters_privilege_question, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INVESTIGATIVE JOURNALIST (SNARE) — A journalist who builds reporting on confidential sources and operates under federal jurisdiction faces significant barriers to protecting source identity: Branzburg's constitutional void means reliance on judge-made common law, federal statute (shield laws where they exist), or federal common law balancing tests. The journalist can theoretically exit federal jurisdiction or change reporting methods, but at high cost — moving operations, losing source networks, abandoning story types. Effective extraction is substantial: the threat of compelled testimony chills source recruitment and creates leverage for prosecutors seeking information.
constraint_indexing:constraint_classification(free_press_clause__reporters_privilege_question, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GRAND JURY / PROSECUTORIAL AUTHORITY (ROPE) — The grand jury and prosecutorial system experience the constraint as a coordination mechanism: compelled journalist testimony enables grand jury access to information necessary for criminal investigation oversight. The constraint solves a coordination problem (gathering evidence) and does so with relatively low overhead for prosecutors — they can exercise subpoena power without negotiating or compensating journalists. From the prosecutorial perspective, this is pure coordination with a clear function. Prosecutors have arbitrage options (investigative subpoena of other witnesses, documents, surveillance) and benefit from the constraint's existence by gaining evidentiary access.
constraint_indexing:constraint_classification(free_press_clause__reporters_privilege_question, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE-LEVEL PRESS FREEDOM (TANGLED ROPE) — Forty-nine states have enacted reporters' privilege statutes, creating a genuinely mixed constraint. These statutes coordinate with constitutional press freedom (providing the protection Branzburg refused) while asymmetrically extracting from the federal prosecutorial power — federal prosecutors must navigate state-law privilege claims, and federal courts apply state privilege law in diversity or federal question contexts. States have mobile options (adjusting shield law scope, depth, and exceptions) and benefit from the coordination mechanism (a federal privilege would preempt their policy space). This perspective sees the constraint as hybrid: genuine coordination (protecting sources, enabling investigative journalism to operate) coupled with asymmetric extraction (prosecutors lose evidentiary access they would have at the constitutional level).
constraint_indexing:constraint_classification(free_press_clause__reporters_privilege_question, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: BRANZBURG PRECEDENT / INSTITUTIONAL INERTIA (PITON) — From a civilizational/institutional perspective, Branzburg represents a degraded constraint: the Supreme Court articulated a principle (no federal constitutional reporters' privilege) that the legal system has largely functionally rejected through state law. The precedent persists as formal constitutional law — binding on federal courts, cited in constitutional doctrine — but its practical force is substantially eroded. Federal courts apply state-law privileges routinely, and the Branzburg concurrence (recognizing balancing tests for non-routine cases) has become more operative than the majority holding in many circuits. The precedent is maintained through institutional inertia (constitutional precedent changes slowly) rather than because Branzburg's rule is functionally superior. Theater ratio is elevated (0.58) because Branzburg's formal constitutional pronouncement is theater — the substantive law has moved elsewhere, and the precedent no longer governs how the constraint actually operates.
constraint_indexing:constraint_classification(free_press_clause__reporters_privilege_question, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: PRESS ASSOCIATIONS / SHIELD LAW COALITION (TANGLED ROPE) — Professional press organizations (SPJ, ASNE, Reporters Committee) experience this constraint as a coordination problem with embedded extraction. They coordinate around state-law shield laws and federal privilege doctrines, and they bear significant advocacy costs to maintain and expand the patchwork of protection. They benefit from the coordination mechanism (state laws provide real protection to members) but are extractively constrained by the federal constitutional void — federal prosecutors still operate with substantive leverage, and the absence of a constitutional floor means state protections remain vulnerable to federal preemption or limiting interpretation. Organizations have constrained exit options (they can advocate for federal statute, but constitutional amendment or Supreme Court reversal is asymptotically unlikely).
constraint_indexing:constraint_classification(free_press_clause__reporters_privilege_question, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CONSTITUTIONAL IMMUTABILITY (MOUNTAIN) — A civilizational-scale analytical perspective might treat the constitutional silence on reporters' privilege as an immutable structural feature: the First Amendment does not textually guarantee source protection, the Framers did not articulate one, and the constitutional structure (with grand jury investigation as a constitutional right in the Sixth Amendment) is interpreted as creating inherent tension that cannot be resolved at the constitutional level — only at the statutory level. This perspective risks naturalizing what is actually a contested institutional choice: treating Branzburg's constitutional outcome as an inescapable implication of text rather than one reading among defensible alternatives. The constraint appears as mountain because the constitutional text and structure appear to support the outcome, but the structural data reveals this as a false summit — the constraint's persistence depends on path dependence (precedent inertia) and institutional capture (prosecutorial influence over federal doctrine), not immutable law.
constraint_indexing:constraint_classification(free_press_clause__reporters_privilege_question, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(free_press_clause__reporters_privilege_question_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(free_press_clause__reporters_privilege_question, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(free_press_clause__reporters_privilege_question, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(free_press_clause__reporters_privilege_question, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(free_press_clause__reporters_privilege_question, TR),
    TR >= 0.70.

:- end_tests(free_press_clause__reporters_privilege_question_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, at the tangled rope / snare boundary. The constraint extracts in multiple dimensions: (1) sources face compelled disclosure risk, reducing willingness to provide information; (2) journalists must invest in legal defense and privilege assertion; (3) prosecutors gain leverage to pressure journalists into testimony. However, extractiveness is not maximal (0.66+) because state-law protections provide meaningful mitigation, the Branzburg concurrence offers balancing-test protection in some circuits, and investigative journalism continues despite the constitutional void — suggesting the extraction is real but not totalizing. Suppression (0.68): High. Barriers to source confidentiality in federal jurisdiction include: constitutional doctrine denies privilege, grand jury subpoena power is broadly exercised, contempt sanctions deter resistance, and prosecutorial pressure on journalists is substantial. However, suppression is not absolute (0.85+) because state law provides workarounds, federal judges exercise discretion to limit fishing expeditions, and organizational press has leverage to resist in high-profile cases. Theater ratio (0.58): Moderate-high. The Branzburg precedent operates increasingly as theater — the formal holding (no constitutional privilege) is cited but functionally displaced by state law and federal balancing tests. Federal courts apply state-law privileges routinely, and Branzburg's majority rule is invoked primarily in cases where other doctrines (grand jury materiality, investigative necessity) already provide protection. The theater has increased over time as the gap between the formal rule and actual governing law has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival disagreement. Confidential sources and journalists see snare (pure extraction with no coordination benefit). Prosecutors see rope (pure coordination solving an investigation access problem). State legislatures see tangled rope (creating their own coordination mechanism while extractively displacing federal authority). The Branzburg precedent itself appears as piton (formal constitutional law sustained by institutional inertia despite functional obsolescence). The analytical observer risks mountain classification (treating the constitutional void as an immutable feature of constitutional text and structure) but structural data reveals false summit — the constraint's persistence depends on precedent path-dependence and institutional capture, not constitutional necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural relationship to the constraint: sources and journalists are victims with no exit (high d, high extraction), prosecutors are beneficiaries with alternative investigative methods (low d, negative extraction), state legislatures are secondary beneficiaries with mobile options for shield law scope (low-moderate d). The analytical perspective's mountain classification is perspectival but not immutable — the constraint is a constitutional doctrine (appearing as natural law) that benefits identifiable institutional actors (prosecutors, grand jury authority) over others (sources, journalists). This is diagnostic of false summit: a constraint presented as an inescapable constitutional implication but grounded in institutional choice.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    branzburg_majority_vs_concurrence_scope,
    'Does Branzburg''s majority holding (no blanket constitutional privilege) foreclose the Branzburg concurrence''s balancing test (privilege recognized in routine news gathering but not grand jury investigation), or are both readings live doctrinal options?',
    'Circuit-by-circuit doctrine tracking; empirical survey of how federal courts resolve Branzburg conflicts (majority rule vs. concurrence balancing); Supreme Court clarification (unlikely absent new case)',
    'If concurrence is live option: extractiveness drops to ~0.35 (tangled_rope floor), suppression drops to ~0.50. If majority forecloses concurrence: extractiveness remains at 0.52 (snare boundary).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(branzburg_majority_vs_concurrence_scope, conceptual, 'Whether Branzburg concurrence remains a live doctrinal option or is foreclosed by the majority').

omega_variable(
    state_shield_law_federal_preemption_risk,
    'Can Congress or federal courts preempt state reporters'' privilege laws through federal statute (Evidence Rule 501) or constitutional interpretation, or are state shield laws structurally protected from federal override?',
    'Legislative history of Evidence Rule 501 and any federal shield bill; case law on Erie vs. Federal Common Law in privilege contexts; Supreme Court doctrine on state police power over evidence rules',
    'If state laws are preemption-vulnerable: the entire state-law patchwork represents a contingent reprieve, not a structural settlement. Extractiveness would revert to 0.65+ (snare). If state laws are protected: extractiveness settles at 0.52 (tangled_rope stable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_shield_law_federal_preemption_risk, empirical, 'Whether state shield laws are structurally protected from federal preemption').

omega_variable(
    confidential_source_supply_elasticity,
    'How much does the Branzburg constitutional void reduce confidential source availability to journalists — i.e., do sources actually withhold information due to Branzburg risk, or is the suppression mechanical rather than behavioral?',
    'Comparative study of source recruitment in federal vs. state-regulated journalism; analysis of investigations abandoned or modified due to source confidentiality risk; interviews with journalists and sources in federal vs. state contexts',
    'If elasticity is high (sources respond strongly to Branzburg risk): suppression is real and structural (0.68 is accurate). If elasticity is low (sources provide information despite risk): suppression is overstated, and extractiveness drops to 0.35-0.40 (rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(confidential_source_supply_elasticity, empirical, 'How sensitive source availability is to Branzburg constitutional void').

omega_variable(
    false_summit_natural_law_claim,
    'Is Branzburg''s constitutional outcome a natural law of the constitutional text (mountain), or a contingent institutional choice that benefits grand jury authority and prosecutorial interests (tangled_rope / snare)?',
    'Originalist analysis of First Amendment ratification history and Framers'' intent on source protection; comparative constitutional law (how other democracies with press freedom protections handle reporters'' privilege); counterfactual doctrine under alternative holdings',
    'If natural law: constraint is immutable, false summit detector fires. If contingent: constraint is a reading of the free press clause that benefits identifiable institutional actors (prosecutors, grand jury system) and is vulnerable to state-law displacement or federal reversal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether Branzburg outcome is constitutional necessity or institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(free_press_clause__reporters_privilege_question, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(frpc_tr_t0, free_press_clause__reporters_privilege_question, theater_ratio, 0, 0.35).
narrative_ontology:measurement(frpc_tr_t20, free_press_clause__reporters_privilege_question, theater_ratio, 20, 0.48).
narrative_ontology:measurement(frpc_tr_t40, free_press_clause__reporters_privilege_question, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(frpc_be_t0, free_press_clause__reporters_privilege_question, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(frpc_be_t20, free_press_clause__reporters_privilege_question, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(frpc_be_t40, free_press_clause__reporters_privilege_question, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(free_press_clause__reporters_privilege_question, enforcement_mechanism).
narrative_ontology:affects_constraint(free_press_clause__reporters_privilege_question, free_press_clause__press_as_technology_reading).
narrative_ontology:affects_constraint(free_press_clause__reporters_privilege_question, free_press_clause__prior_restraint_doctrine).
narrative_ontology:affects_constraint(free_press_clause__reporters_privilege_question, state_reporters_privilege_patchwork).
narrative_ontology:affects_constraint(free_press_clause__reporters_privilege_question, grand_jury_investigative_authority).

% DUAL FORMULATION NOTE:
% The reporters_privilege_question reading decomposes the free_press_clause kernel from two sibling readings (technology/guild-neutrality and prior-restraint doctrine). Each reading has structurally distinct epsilon values reflecting different suppression mechanisms and beneficiary sets. This constraint specifically addresses source confidentiality extraction at the federal constitutional level; the press_as_technology reading addresses professional exclusivity; prior_restraint addresses publication restraint. All three link through their shared kernel (the Free Press Clause) but have different victims, beneficiaries, and extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
