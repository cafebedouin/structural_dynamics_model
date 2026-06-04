% ============================================================================
% CONSTRAINT STORY: incorporation_doctrine__selective_incorporation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_incorporation_doctrine__selective_incorporation_reading, []).

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
 *   constraint_id: incorporation_doctrine__selective_incorporation_reading
 *   human_readable: Selective Incorporation Doctrine: Right-by-Right Judicial Absorption of Bill of Rights Guarantees
 *   domain: constitutional_law/doctrinal_interpretation
 *
 * SUMMARY:
 *   The selective incorporation doctrine represents a constitutional
 *   constraint that emerged from the Supreme Court's post-Civil War project
 *   of binding state procedures to federal guarantees. Rather than apply the
 *   entire Bill of Rights wholesale to the states (total incorporation,
 *   Black's position), or reverse-engineer federal limits through Fifth
 *   Amendment due process (reverse incorporation, Bolling logic), the Court
 *   proceeded guarantee by guarantee, testing each for whether it was
 *   'fundamental to our scheme of ordered liberty' or 'deeply rooted in this
 *   nation's history and tradition.' This constraint absorbs state procedural
 *   autonomy piecemeal while preserving the theatrical form of federalism —
 *   each case appears to be a discrete constitutional question ('Is jury
 *   trial fundamental?') rather than an inevitable unification of federal and
 *   state constitutional regimes. The constraint exhibits tangled_rope
 *   characteristics: genuine coordination function (clarifying which rights
 *   bind states, solving the collective action problem of constitutional
 *   interpretation) combined with asymmetric extraction (courts gain
 *   interpretive authority, states lose procedural autonomy, judicial
 *   selection of 'fundamental' rights reflects institutional interests
 *   alongside constitutional truth). The selectivity itself — the
 *   right-by-right testing mechanism — is both a coordination device (forces
 *   deliberation about which rights truly matter) and an extraction mechanism
 *   (preserves judicial agenda-setting power and delays state compliance).
 *
 * KEY AGENTS:
 *   - Rights-Holders (Federal Reach): Primary beneficiary (institutional/arbitrage) — gain expanding federal constitutional protection as each right incorporates; experience is pure coordination and benefit
 *   - State Procedural Systems: Primary victim (powerless/trapped) — cannot exit piecemeal incorporation regime; each case creates binding precedent that suppresses state autonomy; no alternative once Supreme Court selects a right
 *   - Appellants in Incorporation Cases: Secondary actor (moderate/constrained) — face litigation barriers but drive the testing mechanism; both constrained by and benefit from the doctrinal process
 *   - The Supreme Court as Selective Gatekeeper: Beneficiary and enforcer (institutional/constrained) — controls which rights are tested for fundamentality; gains jurisdiction over state procedures; faces institutional constraints from federalism doctrine and prior precedent
 *   - Federalism Doctrine Form: Vestigial actor (institutional/constrained) — maintains performative adherence to federalism principle while systematic suppression occurs; the piton perspective; survives through theatrical invocation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent doctrinal choice (the right-by-right method) as immutable constitutional law; sees incorporation as inevitable unification rather than contingent institutional process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(incorporation_doctrine__selective_incorporation_reading, 0.48).
domain_priors:suppression_score(incorporation_doctrine__selective_incorporation_reading, 0.62).
domain_priors:theater_ratio(incorporation_doctrine__selective_incorporation_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(incorporation_doctrine__selective_incorporation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(incorporation_doctrine__selective_incorporation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(incorporation_doctrine__selective_incorporation_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(incorporation_doctrine__selective_incorporation_reading, tangled_rope).
narrative_ontology:human_readable(incorporation_doctrine__selective_incorporation_reading, "Selective Incorporation Doctrine: Right-by-Right Judicial Absorption of Bill of Rights Guarantees").
narrative_ontology:topic_domain(incorporation_doctrine__selective_incorporation_reading, "constitutional_law/doctrinal_interpretation").

domain_priors:requires_active_enforcement(incorporation_doctrine__selective_incorporation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(incorporation_doctrine__selective_incorporation_reading, '1c6d7364-3338-4d2f-afc9-740adcacf262').
narrative_ontology:cs_kernel_codification('1c6d7364-3338-4d2f-afc9-740adcacf262', fixed_text).
narrative_ontology:cs_authority_grounding('1c6d7364-3338-4d2f-afc9-740adcacf262', lineage).
narrative_ontology:cs_interpretation_layer_present('1c6d7364-3338-4d2f-afc9-740adcacf262').
narrative_ontology:cs_reading_relation('1c6d7364-3338-4d2f-afc9-740adcacf262', incorporation_doctrine__reverse_incorporation_reading, influences).
narrative_ontology:cs_reading_relation('1c6d7364-3338-4d2f-afc9-740adcacf262', incorporation_doctrine__total_incorporation_reading, coexists_with).
narrative_ontology:cs_axiom('1c6d7364-3338-4d2f-afc9-740adcacf262', foundational, tested_fundamentality_criterion).
narrative_ontology:cs_axiom_status(tested_fundamentality_criterion, holdable).
narrative_ontology:cs_axiom_grounding('1c6d7364-3338-4d2f-afc9-740adcacf262', tested_fundamentality_criterion, deontological).
narrative_ontology:cs_axiom('1c6d7364-3338-4d2f-afc9-740adcacf262', foundational, incremental_federalism_form).
narrative_ontology:cs_axiom_status(incremental_federalism_form, holdable).
narrative_ontology:cs_axiom_grounding('1c6d7364-3338-4d2f-afc9-740adcacf262', incremental_federalism_form, deontological).
narrative_ontology:cs_reference_frame('1c6d7364-3338-4d2f-afc9-740adcacf262', federalism_with_tested_fundamental_rights).
narrative_ontology:cs_drift_state('1c6d7364-3338-4d2f-afc9-740adcacf262', contemporary_recognition_of_piecemeal_absorption, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1c6d7364-3338-4d2f-afc9-740adcacf262', '').
narrative_ontology:cs_kernel_id(incorporation_doctrine__selective_incorporation_reading, incorporation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(incorporation_doctrine__selective_incorporation_reading, rights_holders_federal_reach).
narrative_ontology:constraint_beneficiary(incorporation_doctrine__selective_incorporation_reading, judiciary_interpretive_authority).
narrative_ontology:constraint_victim(incorporation_doctrine__selective_incorporation_reading, state_procedural_autonomy).
narrative_ontology:constraint_victim(incorporation_doctrine__selective_incorporation_reading, doctrinal_stability_and_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATE PROCEDURAL SYSTEMS (SNARE) — States cannot exit the piecemeal incorporation regime; each case creates binding precedent that another guarantee lands and suppresses state autonomy. No alternative exists once Supreme Court selects a right for incorporation. Maximum experienced extraction: states bear full cost of doctrinal evolution without consent or predictability mechanism.
constraint_indexing:constraint_classification(incorporation_doctrine__selective_incorporation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: APPELLANTS TESTING NEW RIGHTS (TANGLED ROPE) — Appellants in landmark incorporation cases face barriers (litigation costs, uncertain outcomes) but also benefit from the coordination mechanism: the case-by-case testing process creates genuine legal progress and clarifies which rights truly matter. Mixed extraction and coordination. The appellants drive the system but depend on judicial willingness to incorporate.
constraint_indexing:constraint_classification(incorporation_doctrine__selective_incorporation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RIGHTS-HOLDERS GAINING FEDERAL PROTECTION (ROPE) — As each right lands through incorporation, the beneficiary population expands. This is pure coordination: the constraint solves the collective action problem of establishing which federal guarantees bind states. Rights-holders experience net benefit — their protections grow with each successful incorporation case. No extraction from this perspective.
constraint_indexing:constraint_classification(incorporation_doctrine__selective_incorporation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE JUDICIARY AS SELECTIVE GATEKEEPER (TANGLED ROPE) — The Supreme Court benefits from the case-by-case incorporation mechanism (interpretive authority, case selection power, agenda control over which rights matter). But it also faces constraints: the doctrine requires judicial labor to litigate each guarantee, creates doctrinal bottlenecks when cases slow down, and generates resistance from federalism advocates and state actors. Active enforcement of the right-by-right testing creates extracted value for the Court (jurisdiction over state procedure) alongside genuine coordination (clarifying which rights are 'fundamental').
constraint_indexing:constraint_classification(incorporation_doctrine__selective_incorporation_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERALISM DOCTRINE AS PITON (PITON) — The rhetoric of federalism and state sovereignty persists in selective incorporation doctrine despite serving minimal functional role. The doctrine maintains the theatrical form of right-by-right testing as a concession to federalism concerns, but the underlying trajectory is inevitable absorption. The court performs federalism respect through the case-by-case mechanism while systematically suppressing state autonomy. Theater_ratio reflects the performative quality of testing for 'fundamentality' when the real driver is constitutional evolution and rights expansion.
constraint_indexing:constraint_classification(incorporation_doctrine__selective_incorporation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational view, constitutional guarantees must eventually achieve uniform binding across all jurisdictions; the right-by-right incorporation process is an immutable feature of how federal constitutions mature. No alternative exists — either rights apply federally or federalism fragments the constitutional order. This perspective naturalizes selective incorporation as inherent structural necessity. However, the presence of identifiable beneficiaries (rights-holders, judiciary) and victims (state autonomy) suggests this is a false summit — the constraint is structurally contingent, not naturally necessitated.
constraint_indexing:constraint_classification(incorporation_doctrine__selective_incorporation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(incorporation_doctrine__selective_incorporation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(incorporation_doctrine__selective_incorporation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(incorporation_doctrine__selective_incorporation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(incorporation_doctrine__selective_incorporation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(incorporation_doctrine__selective_incorporation_reading, TR),
    TR >= 0.70.

:- end_tests(incorporation_doctrine__selective_incorporation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high, reflecting that selective incorporation creates genuine coordination (clarifying which rights are fundamental, solving the constitutional interpretation problem) alongside real extraction (courts preserve agenda-setting power through right-by-right selection, states lose procedural autonomy incrementally rather than wholesale). The extractiveness increased over the 60-year interval from 0.15 (early incorporation cases, when the mechanism was seen as genuine federalism negotiation) to 0.48 (contemporary view recognizes the mechanism as extraction of state autonomy under cover of coordination). Suppression (0.62): High. States face substantial barriers to exemption once the Supreme Court incorporates a right: no opt-out mechanism, no alternative constitutional path, no negotiation over compliance timeline. The suppression requirement increased from 0.30 (early cases, where states could argue incorporation was premature) to 0.62 (contemporary cases, where incorporation is expected and states have limited standing to resist). Theater ratio (0.55): Moderate-high. The right-by-right testing mechanism includes genuine deliberation (whether a right is truly fundamental) but also performative elements (invocation of federalism values that the mechanism systematically suppresses, citation of state autonomy while expanding federal reach). Theater increased from 0.40 to 0.55 as the pattern became evident — each case follows the template of 'is X fundamental?' without genuine uncertainty about the eventual answer. Beneficiaries: Rights-holders gain federal reach; the judiciary gains interpretive authority. Victims: State procedural autonomy (suppressed piecemeal), doctrinal clarity (the case-by-case method delays and fragments constitutional clarity for the sake of preserving the appearance of federalism respect).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates sharp perspectival divergence. States see a snare: trapped in a system where Supreme Court decisions absorb their autonomy without consent. Appellants see tangled_rope: the case-by-case litigation mechanism both enables and constrains their efforts. Rights-holders see rope: pure coordination of which guarantees bind states federally. The Supreme Court sees tangled_rope: genuine constitutional problem-solving (which rights are fundamental?) mixed with institutional benefit (preserved agenda-setting authority). Federalism doctrine sees piton: its own form is performative; federalism values are invoked while systematically suppressed. The analytical observer risks seeing mountain: incorporation is an inexorable constitutional law. But the structural data (identifiable beneficiaries and victims, suppression costs, institutional interests) reveals this as a false summit — the constraint is contingent institutional arrangement, not natural law. The perspectival gap reveals that 'fundamentality' is not an objective constitutional property discovered case by case, but rather a framework that conceals the piecemeal transfer of state authority to federal courts.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives directionality from the agent's structural relationship to the incorporation flow. Rights-holders benefit incrementally as each guarantee incorporates (low d, negative f(d)). States face net suppression of autonomy as the mechanism proceeds (high d, high f(d)). The Court gains interpretive authority while preserving the institutional fiction of federalism negotiation (moderate d, balanced extraction and coordination). Appellants face barriers to litigation but benefit from eventual rights clarification (moderate d, mixed extraction and benefit). The piton perspective reflects degraded federalism doctrine — the form persists through institutional inertia while the function (protecting state autonomy) has been absorbed into the piecemeal incorporation mechanism. The mountain perspective at analytical context risks naturalizing institutional contingency as constitutional necessity; the engine's false summit detector identifies it as beneficiary naturalization (courts and rights-holders benefit from seeing incorporation as 'natural' rather than contingent institutional choice).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in selective incorporation doctrine is the false dichotomy between 'federalism respects state autonomy' (federalism narrative) and 'federal rights must bind states uniformly' (rights protection narrative). Selective incorporation resolves this by conceding on form (right-by-right testing, invoking federalism values) while extracting on substance (systematic absorption of state procedural autonomy). The coordination function is real: clarifying which rights are fundamental. The extraction mechanism is real: preserving judicial agenda-setting authority and delaying state compliance through the piecemeal mechanism. The classification as tangled_rope resolves the mandatrophy by acknowledging that both are structurally present. The alternative reading (total incorporation, Black's position) would maximize coordination and minimize extraction: if the whole Bill of Rights incorporated in 1868, there would be no ambiguity, no case-by-case testing, no judicial discretion over 'fundamentality.' The selective reading trades clarity for institutional authority: courts preserve agenda-setting power by maintaining the appearance of federalism negotiation while achieving the substance of uniform federal rights.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fundamentality_criterion_under_determination,
    'What makes a right ''fundamental'' for incorporation purposes — originalist fidelity to the Fourteenth Amendment''s meaning, evolutionary contemporary consensus, or something else?',
    'Historical-doctrinal analysis of incorporation cases; comparison of stated fundamentality rationales across decades; examination of dissenting opinions claiming fundamentality was manipulated',
    'If fundamentality is genuinely objective: selective incorporation is mineral (natural law of constitutional maturation). If fundamentality is judicial discretion: extraction mechanism is exposed (courts choose which rights to absorb based on institutional interests). If fundamentality is evolutionary consensus: the doctrine is legitimate coordination, not extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fundamentality_criterion_under_determination, conceptual, 'What criterion determines if a right is ''fundamental'' for incorporation').

omega_variable(
    piecemeal_vs_total_incorporation_counterfactual,
    'Would total incorporation at the Fourteenth Amendment''s ratification (Black''s position) have produced better or worse doctrinal clarity, state compliance, and rights protection than the selective piecemeal path?',
    'Comparative constitutional law analysis; counterfactual reconstruction of legal landscape if total incorporation occurred in 1868; empirical comparison with jurisdictions using total incorporation equivalents',
    'If piecemeal was worse: the selective doctrine represents extraction mechanism for judicial control (court preserves agenda-setting power through slow absorption). If piecemeal was better: the constraint is legitimate coordination (case-by-case testing clarifies rights). If equivalent: doctrine is a false choice (either method produces same outcome; selectivity masks contingency).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piecemeal_vs_total_incorporation_counterfactual, conceptual, 'Whether selective incorporation was necessary or contingent choice').

omega_variable(
    state_procedural_diversity_value,
    'Did the suppression of state procedural autonomy through selective incorporation destroy valuable experimentation and local adaptation, or eliminate unjustified variation that harmed rights-holders?',
    'Historical analysis of state-level procedural variation pre-incorporation; empirical comparison of state compliance trajectories with incorporation timing; examination of whether state innovations in procedure occurred that were lost to federalization',
    'If state diversity was valuable: suppression represents real extraction cost (victim is state systems). If variation was indefensible: suppression is legitimate rights protection (victim classification disappears). If mixed: some state autonomy was legitimately suppressed (extraction justified by rights protection), some was unjustifiably lost (extraction without compensation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_procedural_diversity_value, empirical, 'Whether suppressed state procedural diversity had independent value').

omega_variable(
    kernel_contest_relation_ambiguity,
    'Does selective incorporation foreclose total incorporation (Black''s doctrine), or can both readings coexist within constitutional interpretation?',
    'Doctrinal analysis of whether selective incorporation axioms logically entail rejection of total incorporation''s core premises; examination of whether a court could coherently hold both doctrines simultaneously; comparative case law on which reading controls interpretation',
    'If foreclosed: the readings are logically incompatible; one must dominate. If coexist: both remain live interpretive options across different contexts (some cases apply selective, some apply total logic). If influences: selective incorporation creates pressure that devalues total incorporation without logically eliminating it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_relation_ambiguity, conceptual, 'Whether selective incorporation and total incorporation are logically compatible readings').

omega_variable(
    reverse_incorporation_structural_dependence,
    'Does selective incorporation depend on the logic of reverse incorporation (that equal protection can bind the federal government through Fifth Amendment due process), or are these independent lines of constitutional expansion?',
    'Doctrinal genealogy of Bolling v. Sharpe and its relationship to selective incorporation doctrine; examination of whether reverse incorporation cases cite selective incorporation rationales; analysis of whether suppressing reverse incorporation would collapse selective incorporation',
    'If dependent: selective incorporation is downstream of reverse incorporation; the readings influence each other. If independent: each reading stands on its own doctrinal foundation. If symbiotic: both readings reinforce each other; strengthening one strengthens the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reverse_incorporation_structural_dependence, conceptual, 'Whether selective incorporation doctrine depends on reverse incorporation logic').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(incorporation_doctrine__selective_incorporation_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, incorporation_doctrine__selective_incorporation_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(inco_tr_t30, incorporation_doctrine__selective_incorporation_reading, theater_ratio, 30, 0.5).
narrative_ontology:measurement(inco_tr_t60, incorporation_doctrine__selective_incorporation_reading, theater_ratio, 60, 0.55).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, incorporation_doctrine__selective_incorporation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(inco_be_t30, incorporation_doctrine__selective_incorporation_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(inco_be_t60, incorporation_doctrine__selective_incorporation_reading, base_extractiveness, 60, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, incorporation_doctrine__selective_incorporation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(inco_su_t30, incorporation_doctrine__selective_incorporation_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(inco_su_t60, incorporation_doctrine__selective_incorporation_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(incorporation_doctrine__selective_incorporation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(incorporation_doctrine__selective_incorporation_reading, incorporation_doctrine__reverse_incorporation_reading).
narrative_ontology:affects_constraint(incorporation_doctrine__selective_incorporation_reading, incorporation_doctrine__total_incorporation_reading).

% DUAL FORMULATION NOTE:
% The incorporation doctrine kernel supports three constraint readings with structurally distinct epsilon values and beneficiary/victim structures. This reading (selective incorporation) has ε ≈ 0.48 and treats states as primary victims. The total incorporation reading would have lower ε (higher coordination, less extraction) because the right-by-right selection mechanism is eliminated. The reverse incorporation reading has different victim set (federal government subject to equal protection reasoning) and different ε reflecting the symmetry argument.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(incorporation_doctrine__selective_incorporation_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
