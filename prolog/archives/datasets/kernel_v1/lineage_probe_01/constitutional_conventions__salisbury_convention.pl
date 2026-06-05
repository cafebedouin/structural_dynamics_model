% ============================================================================
% CONSTRAINT STORY: constitutional_conventions__salisbury_convention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salisbury_convention, []).

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
 *   constraint_id: constitutional_conventions__salisbury_convention
 *   human_readable: The Salisbury Convention: Lords defer to manifesto legislation at second reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The Salisbury Convention — the unelected House of Lords does not block
 *   bills that formed part of the government's manifesto at second reading —
 *   is a foundational rule of Westminster democracy that encodes a
 *   fundamental settlement: elected chambers dominate unelected ones, and
 *   electoral mandates bind the system. Established after 1945 when the
 *   Labour government faced potential Lords obstruction of radical
 *   nationalizations, the convention suppresses the Lords' historic veto
 *   power over manifesto legislation in exchange for institutional survival.
 *   The Lords retain significant legislative functions (revision of
 *   non-manifesto bills, scrutiny, delay mechanisms) within the suppressed
 *   authority structure, making this constraint a hybrid of genuine
 *   coordination (the Lords do meaningfully revise other legislation) and
 *   extraction (the veto is forfeited as a condition of not being abolished).
 *   The constraint operates entirely through convention — unstated norm, not
 *   law — which has held for 76 years despite repeated pressure from
 *   majoritarian governments and periodic Lords reform debates. Theater has
 *   increased over the interval as the convention has shifted from explicit
 *   negotiation to ritualized deference, with the Lords treating
 *   second-reading manifesto silence as taboo rather than examining whether
 *   deference is justified in each case. This reading of constitutional
 *   conventions emphasizes how electoral mandate, once conferred, becomes an
 *   asymmetric claim over the unelected chamber's legislative capacity.
 *
 * KEY AGENTS:
 *   - Unelected House of Lords: Primary victim (powerless/trapped) — forfeited veto power over manifesto legislation as survival condition; cannot exit appointed chamber or refuse deference without institutional risk
 *   - Elected House of Commons: Primary beneficiary (institutional/arbitrage) — captures right to pass manifesto legislation without Lords obstruction; threatens Lords with abolition or radical reform if convention is broken
 *   - Manifesto Democracy (abstract): Beneficiary (powerful/mobile) — principle that electoral mandates bind the system; convention coordinates voter expectations with institutional implementation
 *   - Reform-Minded Legislators: Secondary actors (moderate/constrained) — benefit from convention's stabilization of manifesto passage but experience costs from loss of revising chamber depth; constrained by political economy of reform
 *   - Westminster System Practitioners: Institutional observers (institutional/arbitrage) — maintain convention through unstated norms and taboo rather than explicit rule; benefit from convention's stability even as its functionality degrades into ritual
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent political settlement as immutable democratic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_conventions__salisbury_convention, 0.38).
domain_priors:suppression_score(constitutional_conventions__salisbury_convention, 0.62).
domain_priors:theater_ratio(constitutional_conventions__salisbury_convention, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_conventions__salisbury_convention, extractiveness, 0.38).
narrative_ontology:constraint_metric(constitutional_conventions__salisbury_convention, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(constitutional_conventions__salisbury_convention, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_conventions__salisbury_convention, tangled_rope).
narrative_ontology:human_readable(constitutional_conventions__salisbury_convention, "The Salisbury Convention: Lords defer to manifesto legislation at second reading").
narrative_ontology:topic_domain(constitutional_conventions__salisbury_convention, "constitutional/political").

domain_priors:requires_active_enforcement(constitutional_conventions__salisbury_convention).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_conventions__salisbury_convention, '1bf4df23-105f-4558-b530-81119851336e').
narrative_ontology:cs_kernel_codification('1bf4df23-105f-4558-b530-81119851336e', fixed_text).
narrative_ontology:cs_authority_grounding('1bf4df23-105f-4558-b530-81119851336e', lineage).
narrative_ontology:cs_interpretation_layer_present('1bf4df23-105f-4558-b530-81119851336e').
narrative_ontology:cs_reading_relation('1bf4df23-105f-4558-b530-81119851336e', constitutional_conventions__collective_responsibility, coexists_with).
narrative_ontology:cs_reading_relation('1bf4df23-105f-4558-b530-81119851336e', constitutional_conventions__ministerial_responsibility, influences).
narrative_ontology:cs_reading_relation('1bf4df23-105f-4558-b530-81119851336e', constitutional_conventions__royal_assent_convention, coexists_with).
narrative_ontology:cs_axiom('1bf4df23-105f-4558-b530-81119851336e', foundational, electoral_mandate_binds_unelected_chambers).
narrative_ontology:cs_axiom_status(electoral_mandate_binds_unelected_chambers, holdable).
narrative_ontology:cs_axiom_grounding('1bf4df23-105f-4558-b530-81119851336e', electoral_mandate_binds_unelected_chambers, deontological).
narrative_ontology:cs_axiom('1bf4df23-105f-4558-b530-81119851336e', foundational, second_chamber_veto_forfeiture_as_survival_condition).
narrative_ontology:cs_axiom_status(second_chamber_veto_forfeiture_as_survival_condition, holdable).
narrative_ontology:cs_axiom_grounding('1bf4df23-105f-4558-b530-81119851336e', second_chamber_veto_forfeiture_as_survival_condition, instrumental).
narrative_ontology:cs_reference_frame('1bf4df23-105f-4558-b530-81119851336e', parliamentary_sovereignty_constrained_by_electoral_mandate).
narrative_ontology:cs_drift_state('1bf4df23-105f-4558-b530-81119851336e', contemporary_post_brexit_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1bf4df23-105f-4558-b530-81119851336e', '').
narrative_ontology:cs_kernel_id(constitutional_conventions__salisbury_convention, constitutional_conventions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_conventions__salisbury_convention, elected_commons).
narrative_ontology:constraint_beneficiary(constitutional_conventions__salisbury_convention, manifesto_democracy).
narrative_ontology:constraint_victim(constitutional_conventions__salisbury_convention, unelected_lords).
narrative_ontology:constraint_victim(constitutional_conventions__salisbury_convention, second_chamber_legislative_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNELECTED PEER (SNARE) — Trapped in a chamber that has ceded its veto power over manifesto legislation via convention. The peer cannot exit (remains appointed for life), cannot block endorsed programs (suppressed by the convention itself), and experiences the constraint as pure extraction of legislative authority with no coordination benefit. The Lords' historic power to revise and delay is forfeited as a condition of institutional survival. Maximum experienced extraction — no alternatives, no agency, no compensation.
constraint_indexing:constraint_classification(constitutional_conventions__salisbury_convention, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SECOND CHAMBER INSTITUTION (TANGLED ROPE) — The Lords retain significant coordination function: revising non-manifesto bills, drafting improvements, delaying controversial measures to force reconsideration. But this coordination exists *within* the suppressed authority structure — the chamber's power is explicitly subordinate to the Commons over mandated legislation. The Lords experience both genuine coordination (revision, delay, drafting) and asymmetric extraction (veto forfeited as condition of survival). The constraint requires active enforcement via institutional expectation and convention, not legal rule. Exit is constrained: reform attempts face electoral risk and House of Commons resistance.
constraint_indexing:constraint_classification(constitutional_conventions__salisbury_convention, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELECTED COMMONS (ROPE) — Experiences the convention as pure coordination: the Lords defer to manifesto mandates, enabling the Commons to pass endorsed programs without obstruction. No extraction from the Commons' perspective — the constraint solves a collective action problem (ensuring unelected branch does not thwart electoral mandates) with minimal coercive overhead. The Commons can arbitrage this position by threatening reform if the convention is broken; the threat need not be exercised because the convention itself is self-enforcing through the Lords' internalized understanding that breaking it risks abolition.
constraint_indexing:constraint_classification(constitutional_conventions__salisbury_convention, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MANIFESTO DEMOCRACY (ROPE) — The abstract beneficiary of the convention: the principle that elections confer mandates that unelected chambers must not thwart. The convention coordinates voter expectations (mandates will be enacted) with institutional capacity (Lords will not block them). From the perspective of democratic legitimacy, the constraint solves a coordination problem without extraction — it establishes the rule that elected mandates bind the system. Theater is moderate (0.55) because the convention relies on unstated norms rather than law, but the norms have held for 70+ years, indicating genuine coordination rather than pure performance.
constraint_indexing:constraint_classification(constitutional_conventions__salisbury_convention, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM-MINDED LEGISLATORS (TANGLED ROPE) — Some legislators (both Commons and Lords) view the convention as coordination (legitimate electoral deference) AND extraction (unnecessary suppression of the revising chamber). These actors benefit from the convention's stabilization of manifesto passage but experience costs from the loss of detailed scrutiny and improvement potential. Exit is constrained by the political economy of reform: changing the convention requires either breaking it (risking Lords abolition) or formal amendment (requiring Commons supermajority). Moderate experience of extraction with genuine, if constrained, alternatives.
constraint_indexing:constraint_classification(constitutional_conventions__salisbury_convention, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONVENTION AS RITUAL (PITON) — Over 70 years, the Salisbury Convention has become partially theatricalized. Its original function — preventing unelected obstruction of clear electoral mandates — is genuine, but the convention now operates through unstated norms and taboo against breaking it rather than explicit negotiation. The Lords avoid even *discussing* manifesto bills at second reading, treating the convention as a ritual prohibition rather than a legitimate constraint. Theater_ratio here reflects that institutional actors go through the motions of deference without examining whether deference is structurally justified in each case. The convention persists through inertia and the Lords' internalized survival logic rather than active institutional functionality.
constraint_indexing:constraint_classification(constitutional_conventions__salisbury_convention, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational horizon, the Salisbury Convention appears as an immutable feature of Westminster democracy: unelected chambers must defer to electoral mandates, or else they forfeit legitimacy. This perspective sees the suppression of the Lords' veto as a natural law of democratic systems — elected institutions must dominate unelected ones. However, this classification is a false summit: the convention is not law of nature but stabilized institutional practice with identifiable beneficiaries (the Commons, manifesto democracy) and victims (the Lords' legislative capacity). The 'natural law' framing naturalizes what is a contingent political settlement.
constraint_indexing:constraint_classification(constitutional_conventions__salisbury_convention, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_conventions__salisbury_convention_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_conventions__salisbury_convention, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_conventions__salisbury_convention, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_conventions__salisbury_convention, TR),
    TR >= 0.70.

:- end_tests(constitutional_conventions__salisbury_convention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts the Lords' veto over manifesto legislation, a significant power loss. But extractiveness is not high because the Lords retain meaningful legislative function (revision, scrutiny, delay of non-manifesto bills) and because the constraint is paired with a genuine if precarious survival guarantee. The Commons' threat (abolition) is latent rather than active, and the convention is largely self-enforcing through Lords' internalized understanding of institutional limits. Measurement trajectory shows rising extractiveness (0.28 → 0.38) as the convention has shifted from explicit negotiation to ritualized silence, indicating that the extractive character has been increasingly naturalized and the coordination function has faded. Suppression (0.62): Moderate-high. The suppression operates through institutional taboo — the Lords are expected not to discuss manifesto bills at second reading, treating non-blockage as automatic. Alternatives to deference are suppressed by the credible threat of abolition or radical reform (Parliament Acts precedent). But suppression is not total — Lords retain procedural mechanisms (amendments, delay, third-reading debate) within the suppressed authority structure, and the convention has never been formally codified in law. Theater ratio (0.55): Moderate, rising. Initially (1950), deference was negotiated explicitly and the convention had genuine coordination content — the Lords could assess whether a bill truly constituted a manifesto commitment. By 2026, deference has become ritualized silence; the Lords avoid even discussing manifesto bills, and the convention operates through taboo rather than reasoned agreement. Theater has risen as functionality has degraded into institutional ritual.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a fundamental gap between the beneficiary's experience (Rope — legitimate coordination) and the victim's experience (Snare — pure extraction). The Commons sees the convention as solving a problem: ensuring unelected obstruction does not thwart electoral mandates. The Lords see the same constraint as forfeiting their veto as a survival condition. Both perspectives are structurally accurate — the convention genuinely coordinates electoral implementation AND genuinely extracts the Lords' legislative authority. The Tangled Rope classification from the second-chamber institutional perspective captures this hybrid: the Lords retain significant coordination function (revision, scrutiny) within an authority structure explicitly subordinated to the Commons. The Piton perspective (institutional degradation) reveals that the convention has shifted from explicit negotiation to ritualized silence, with functionality degrading into institutional theater. The false-summit perspective (mountain/natural law) shows how the convention naturalizes what is a contingent political settlement: unelected chambers 'must' defer because it is 'democratic principle,' not because it is law or because the costs of breaking the convention are actually insuperable.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness is computed from the agent's structural position (power, exit options, beneficiary/victim status) and the constraint's base extractiveness. The Commons (institutional/arbitrage) derive d from beneficiary status with arbitrage exit (threat of Lords abolition) — low d, low chi, rope experience. The Lords (powerless/trapped) derive high d from victim status with trapped exit (appointed chamber, cannot resign, survival depends on deference) — high d, high chi, snare experience. Reform-minded legislators (moderate/constrained) occupy an intermediate position (both benefit and bear costs) with constrained exit (reform threatens current institutional order) — moderate d, moderate chi, tangled rope. The analytical observer's false summit reveals that the d-computation obscures the extractive character when viewed from the system level: the convention naturalizes the Commons' asymmetric power by framing it as democratic necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   The Salisbury Convention resolves the mandatrophy by explicitly declaring which institutional actor (the Commons) holds the mandate and which (the Lords) must concede it. The convention creates an asymmetric classification: the Commons experience Rope (legitimate coordination), while the Lords experience Snare (pure extraction). The mandate itself — the electoral authorization to pass specific legislation — is not mandatrophic; it is well-defined by election results and manifesto commitments. The extraction arises from how the mandate is enforced: through suppression of the Lords' veto as a condition of not abolishing the second chamber. The tangled_rope classification from the Lords' institutional perspective captures the residual coordination function (revision, scrutiny) within an asymmetrically extracted authority structure. The piton classification reveals that the constraint has degraded into ritual as theater has risen. The false-summit perspective shows the critical diagnostic challenge: the convention naturalizes contingent political settlement as immutable democratic law, obscuring its extractive character and the contingency of its continued acceptance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manifesto_boundary_ambiguity,
    'What constitutes a ''manifesto bill'' that triggers the Lords'' deference obligation? How clearly can electoral mandates be distinguished from contested interpretation?',
    'Historical analysis of contested cases: which bills have been treated as manifesto bills? Which have sparked debate about manifesto status? Comparison of government assertions vs. Commons and Lords consensus on manifesto character.',
    'If manifesto boundary is clear: convention is well-defined coordination mechanism (Rope from beneficiary perspective). If boundary is contested: convention becomes selective extraction (Snare for Lords) because the Commons can unilaterally define what counts as manifesto and the Lords lose the ability to contest the classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(manifesto_boundary_ambiguity, empirical, 'Whether manifesto boundaries are clear or contested').

omega_variable(
    survival_threat_credibility,
    'How credible is the Lords'' belief that breaking the convention would result in abolition or radical reform? Is the threat of institutional death a genuine constraint on Lords behavior or a rationalization of deference?',
    'Counterfactual analysis: what would Commons and public do if Lords systematically broke the convention? Historical precedent (Parliament Acts 1911, 1949 stripped Lords powers over money bills; would similar escalation follow convention violation?). Survey and interview evidence of Lords'' institutional threat perception.',
    'If threat is credible: the Lords are genuinely constrained (trapped exit option confirmed, classification stands). If threat is low-credibility rationalization: the Lords are choosing deference for other reasons (party alignment, elite consensus, loss of confidence in institutional role), and the classification shifts toward tangled_rope or even rope for the Lords themselves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_threat_credibility, empirical, 'Credibility of institutional death threat constraining Lords behavior').

omega_variable(
    electoral_mandate_executability,
    'To what extent do manifesto promises constitute binding mandates on the Lords, and what is the extractive cost of enforcing manifesto passage over the Lords'' substantive objections?',
    'Analysis of Commons behavior when manifesto bills face substantive Lords amendments: does Commons accept compromise on manifesto legislation or use Parliament Acts to override? Comparison of manifesto bill passage timing (with/without Lords delays) and amendment rates.',
    'If Commons is willing to negotiate substantive changes to manifesto bills: extractiveness is lower and the constraint is coordination. If Commons uses Parliament Acts or refuses amendments: extractiveness is higher and the constraint is pure extraction of Lords veto capacity masked as democratic principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(electoral_mandate_executability, empirical, 'Whether manifesto enforcement involves substantive compromise or pure veto suppression').

omega_variable(
    convention_vs_law_distinction,
    'Is the Salisbury Convention a binding norm grounded in institutional survival incentives, or has it evolved into an unwritten constitutional rule with independent force?',
    'Textual analysis of how Lords justify deference: do they cite convention as norm or as law? Historical analysis of instances where convention was nearly broken and outcomes. Examination of legal opinions on whether convention is judicially enforceable.',
    'If convention is purely norm-based: it is contingent and can be abandoned if incentive structure changes. If convention has become unwritten constitutional law: it has independent binding force beyond institutional survival calculus. Distinction affects whether the constraint is better understood as coordination (rope) or extraction (snare/tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convention_vs_law_distinction, conceptual, 'Whether convention is norm or unwritten constitutional law').

omega_variable(
    kernel_reading_contest,
    'This reading of constitutional conventions (Salisbury Convention) competes with sibling readings (collective responsibility, ministerial responsibility, royal assent convention) for how Westminster''s legitimacy is grounded. Which kernel reading is authoritative?',
    'Historical and doctrinal analysis: which conventions are most relied upon in contemporary Westminster practice? Which have survived challenges? Which structure the others? Judicial and scholarly consensus on the hierarchy of conventions.',
    'If Salisbury Convention is foundational: it structures the others (collective responsibility operates within manifesto constraints; ministerial responsibility presumes manifesto government). If it is subordinate: other conventions (collective responsibility, royal assent) are more fundamental to legitimacy. Affects whether this constraint''s classification as extracted or coordinate is sustainable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which constitutional conventions are foundational to Westminster legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_conventions__salisbury_convention, 1950, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(salisbury_tr_t0, constitutional_conventions__salisbury_convention, theater_ratio, 0, 0.38).
narrative_ontology:measurement(salisbury_tr_t25, constitutional_conventions__salisbury_convention, theater_ratio, 25, 0.48).
narrative_ontology:measurement(salisbury_tr_t50, constitutional_conventions__salisbury_convention, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(salisbury_be_t0, constitutional_conventions__salisbury_convention, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(salisbury_be_t25, constitutional_conventions__salisbury_convention, base_extractiveness, 25, 0.33).
narrative_ontology:measurement(salisbury_be_t50, constitutional_conventions__salisbury_convention, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(salisbury_su_t0, constitutional_conventions__salisbury_convention, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(salisbury_su_t25, constitutional_conventions__salisbury_convention, suppression_requirement, 25, 0.57).
narrative_ontology:measurement(salisbury_su_t50, constitutional_conventions__salisbury_convention, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_conventions__salisbury_convention, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_conventions__salisbury_convention, constitutional_conventions__collective_responsibility).
narrative_ontology:affects_constraint(constitutional_conventions__salisbury_convention, constitutional_conventions__ministerial_responsibility).
narrative_ontology:affects_constraint(constitutional_conventions__salisbury_convention, constitutional_conventions__royal_assent_convention).

% DUAL FORMULATION NOTE:
% The Salisbury Convention is one reading of contested Westminster conventions. Sibling readings (collective responsibility, ministerial responsibility, royal assent) are separate constraint stories with different ε values and different structural logics. The Salisbury reading emphasizes electoral mandate's asymmetric claim over unelected chambers; sibling readings emphasize executive unity, bureaucratic accountability, and parliamentary sovereignty respectively. All four are simultaneously claimed as binding in contemporary practice, creating a presheaf of competing legitimacy grounds over the Westminster kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_conventions__salisbury_convention, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
