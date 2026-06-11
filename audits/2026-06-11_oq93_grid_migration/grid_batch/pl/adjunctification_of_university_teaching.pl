% ============================================================================
% CONSTRAINT STORY: adjunctification_of_university_teaching
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_adjunctification_of_university_teaching, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: adjunctification_of_university_teaching
 *   human_readable: Adjunctification of University Teaching Labor
 *   domain: labor/education/institutional_organization
 *
 * SUMMARY:
 *   Over the past two decades, U.S. universities have systematically
 *   converted tenured and tenure-track faculty lines into contingent adjunct
 *   positions. This constraint exhibits the full Deferential Realism
 *   spectrum: a snare for adjuncts (structural extraction with suppression of
 *   exit), tangled rope for tenure-track faculty (mixed benefit and
 *   complicity), rope for administration (pure coordination benefit), piton
 *   for shared governance (performative without function), and false-summit
 *   mountain for economists (naturalizing policy choice as inevitable). The
 *   conversion accelerates as endowments rise, demonstrating that it is not a
 *   necessity imposed by scarcity but a choice to concentrate wealth while
 *   externalizing teaching labor. Suppression operates at multiple levels:
 *   (1) individual — per-course payment with no benefits,
 *   semester-by-semester contracts, geographic isolation of adjuncts across
 *   multiple campuses, barriers to unionization through classification as
 *   independent contractors. (2) organizational — adjunct committees without
 *   decision-making power, exclusion from faculty senate votes on hiring,
 *   retaliation against organizing. (3) class — diffusion across many
 *   underpaid workers prevents collective action; the precariat has no shared
 *   workplace. (4) structural — the appearance of economic necessity
 *   (naturalization as mountain) suppresses alternative policy conversations.
 *   The grid reveals a level-stratified coercion profile: individual-level
 *   suppression (0.62→0.81) and accessibility collapse (0.65→0.82) intensify
 *   as the constraint deepens, while structural-level coercion remains stable
 *   and modest, protected by the false-necessity narrative. This is the
 *   signature of an institutionally maintained extraction: the apparatus
 *   (administration, budgets, hiring processes) is stable, but the individual
 *   burden escalates.
 *
 * KEY AGENTS:
 *   - Adjunct Faculty: Primary victim (powerless/trapped) — bears full extraction. Per-course pay, no benefits, no job security, no path to stable employment at credential level. Career advancement blocked within the constraint.
 *   - University Administration: Primary beneficiary (institutional/arbitrage) — converts fixed faculty costs to variable labor costs, frees endowment for other institutional uses (facilities, administration, donor amenities). Can defect to alternative models if challenged.
 *   - Tenure-Track Faculty Remnant: Secondary beneficiary/witness (moderate/constrained) — benefits from adjunct labor availability and reduced competition for line conversions, but constrained by complicity and witnessing cost of precarity.
 *   - Shared Governance Bodies: Institutional performance (institutional/arbitrage) — faculty senate and collegial machinery persist without real authority over hiring decisions; the apparatus is maintained while its function has atrophied.
 *   - Student Body: Victim by educational quality (powerless/constrained) — experiences adjunctification through larger classes, less faculty availability, teaching by instructors with no stability to invest in innovation. Absent from governance voice.
 *   - Donor Class: Secondary beneficiary (powerful/arbitrage) — endowment wealth is protected from teaching-labor costs; donations increase institutional prestige and power without funding instruction.
 *   - Peer Institutions: Structural pressure (institutional/constrained) — adjunctification spreads through competitive isomorphism; institutions that resist face budget pressure from peers offering lower tuition through labor cost externalization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(adjunctification_of_university_teaching, 0.68).
domain_priors:suppression_score(adjunctification_of_university_teaching, 0.72).
domain_priors:theater_ratio(adjunctification_of_university_teaching, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(adjunctification_of_university_teaching, extractiveness, 0.68).
narrative_ontology:constraint_metric(adjunctification_of_university_teaching, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(adjunctification_of_university_teaching, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(adjunctification_of_university_teaching, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(adjunctification_of_university_teaching, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(adjunctification_of_university_teaching, snare).
narrative_ontology:human_readable(adjunctification_of_university_teaching, "Adjunctification of University Teaching Labor").
narrative_ontology:topic_domain(adjunctification_of_university_teaching, "labor/education/institutional_organization").

domain_priors:requires_active_enforcement(adjunctification_of_university_teaching).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(adjunctification_of_university_teaching, university_administration).
narrative_ontology:constraint_beneficiary(adjunctification_of_university_teaching, donor_class).
narrative_ontology:constraint_victim(adjunctification_of_university_teaching, adjunct_faculty).
narrative_ontology:constraint_victim(adjunctification_of_university_teaching, student_education_quality).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(adjunctification_of_university_teaching, tenure_track_faculty_remnant).
narrative_ontology:constraint_victim(adjunctification_of_university_teaching, tenure_track_faculty_remnant).
narrative_ontology:constraint_victim(adjunctification_of_university_teaching, student_body).
narrative_ontology:constraint_victim(adjunctification_of_university_teaching, peer_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teaches multiple courses per semester at per-course rates ($2,500-$4,500 per course), no health insurance, no retirement matching, semester-by-semester contracts with no guarantee of renewal. Graduate debt averages $120k-$200k. Cannot afford to leave the academy because credential has no market outside universities; cannot afford to stay because per-course income ($30k-$45k/year) covers only basic needs. Geographically dispersed across multiple institutions to cobble together full-time income. Faces retaliation (non-renewal) for organizing or public dissent.
narrative_ontology:constraint_stakeholder(adjunctification_of_university_teaching, adjunct_faculty, payer,
    powerless, biographical, trapped, national).

% Sets hiring policy and budget priorities. Converts fixed tenure-track lines into variable contingent labor. Reallocates teaching-labor costs to adjunct precarity, freeing endowment for administrative expansion, facilities, and donor amenities. Can shift to alternative models (online delivery, for-profit outsourcing) if challenged. Faces no personal or institutional penalty for adjunctification.
narrative_ontology:constraint_stakeholder(adjunctification_of_university_teaching, university_administration, agenda_setter,
    institutional, immediate, arbitrage, national).

% Benefits from adjunct labor availability (graduate students and precarious instructors available to teach service courses). Constrained from resisting adjunctification because their own tenure-track line exists only by contrast to the precarious labor that surrounds them; solidarity with adjuncts would undermine the justification for their privilege. Bears witnessing cost and complicity cost; restricted from speaking publicly in solidarity without endangering their own tenured position.
narrative_ontology:constraint_stakeholder(adjunctification_of_university_teaching, tenure_track_faculty_remnant, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(adjunctification_of_university_teaching, tenure_track_faculty_remnant, payer).

% Experiences larger class sizes taught by instructors with no institutional security or incentive to innovate. Adjuncts teach most undergraduate courses; tenured faculty teach seminars to majors. Teaching quality suffers from adjunct overwork and instability. Students pay the same tuition as peers at low-adjunct institutions but receive education from less-stable faculty. Absence from governance conversations about hiring and instruction.
narrative_ontology:constraint_stakeholder(adjunctification_of_university_teaching, student_body, payer,
    powerless, immediate, constrained, local).

% Faculty senate and collegial decision-making apparatus persist theatrically. Committees vote on hiring that administration has already decided. Adjunct representatives serve without authority. The machinery is maintained as a legitimacy performance while real labor control is delegated to HR and budget mechanisms outside faculty purview.
narrative_ontology:constraint_stakeholder(adjunctification_of_university_teaching, shared_governance_machinery, observer,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_non_agent(adjunctification_of_university_teaching, shared_governance_machinery).

% Endowment wealth is protected from teaching-labor costs. Adjunctification enables universities to maintain prestige and facilities while lowering faculty payroll, maximizing available funds for donor-named buildings, research initiatives, and administrative expansion. Donors benefit from institutional status and wealth concentration without directly bearing labor costs.
narrative_ontology:constraint_stakeholder(adjunctification_of_university_teaching, donor_class, beneficiary,
    powerful, generational, arbitrage, global).

% Face competitive isomorphism: institutions that resist adjunctification are undercut on tuition by peers offering lower costs through labor externalization. Constrained to follow the adjunctification trend or lose institutional competitiveness. Structural pressure spreads the constraint across the higher-education sector.
narrative_ontology:constraint_stakeholder(adjunctification_of_university_teaching, peer_institutions, payer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(adjunctification_of_university_teaching, university_administration).
narrative_ontology:fixing_cost_class(adjunctification_of_university_teaching, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: University budget constraints and the need for teaching-labor flexibility. Adjunctification appears to solve the problem of rising instructional costs and the need to scale teaching without permanent salary obligations. This is a genuine coordination problem — universities do face real budget pressure and legitimate need to match teaching capacity to enrollment fluctuations.
% TRANSFER_FUNCTION: Transfer of teaching labor from relatively stable, well-paid tenure-track faculty to precarious, poorly-paid contingent instructors. The constraint moves: (1) salary from adjuncts to administration and donors (freed endowment funds), (2) labor security from adjuncts to tenure-track faculty (by contrast, to justify their privilege), (3) educational quality from students to institutional budgets (larger classes, less faculty investment).
% ABSENT_VOICES: Student governance: students who experience education quality degradation are excluded from hiring and budget conversations. Adjuncts themselves: individual adjuncts are scattered across institutions and organized into committees with no authority; the collective adjunct voice is excluded from decision-making. Comparative analysis: no consideration of peer institutions that maintain higher tenure-track ratios or alternative funding models. Future faculty: prospective academics who will enter an adjunct market are not part of the conversation about whether adjunctification should continue.
% DISAPPEARANCE_RATIONALE: If adjunctification disappeared overnight, universities would be forced to either: (1) convert contingent lines back to tenure-track, requiring endowment reallocation or tuition increases; (2) reduce enrollment or class sizes; (3) automate instruction (online delivery). The arrangements of faculty labor, student education quality, institutional prestige (built on faculty credentials and stability), and endowment deployment would all rearrange substantially. The constraint is not natural fact — it is a policy choice that shapes institutional structure.
% FOUNDING_PROBLEM: Rising instructional costs in the 1980s-1990s. As universities expanded enrollment, tenure-track salaries rose with seniority, and administrative overhead increased, universities faced budget pressure. The founding problem was: how to scale instruction without proportionally scaling fixed personnel costs?
% FOUNDING_PROBLEM_CORROBORATION: Contemporary university endowments are at record highs. The founding budget crisis is no longer live — universities have solved the revenue problem through fundraising and endowment growth. State disinvestment in higher education is cited as ongoing justification, but it is contradicted by the fact that universities with rising endowments choose to adjunctify. Harvard's endowment ($53B) and Stanford's ($37B) are larger than many nations' GDPs; neither faces the budget crisis that supposedly justified adjunctification. Non-beneficiary corroboration: audits from faculty unions, adjunct advocacy organizations, and institutional researchers show that adjunctification persists despite the founding problem being resolved. It persists because it benefits the administration and donor class, not because budget pressure requires it.
narrative_ontology:disappearance_verdict(adjunctification_of_university_teaching, world_rearranges).
narrative_ontology:founding_problem_status(adjunctification_of_university_teaching, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTINGENT FACULTY (SNARE) — Trapped by credential-to-employment barrier and debt load. Extraction mechanism fully operative: per-course pay with no benefits, no job security, no collective bargaining power. Maximum perceived extraction. Career advancement blocked; exit to comparable employment impossible at credential level.
constraint_indexing:constraint_classification(adjunctification_of_university_teaching, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TENURE-TRACK FACULTY REMNANT (TANGLED ROPE) — Experiences mixed coordination and extraction. Legitimate coordination function: hiring adjuncts maintains teaching capacity within budget constraints. But also extracts from contingent workers, reducing faculty labor costs and enabling higher administration/donor payouts. Benefits from adjunct availability (labor supply) while bearing witnessing cost of precarity.
constraint_indexing:constraint_classification(adjunctification_of_university_teaching, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: UNIVERSITY ADMINISTRATION (ROPE) — Net beneficiary with arbitrage exit. Experiences the constraint as pure coordination: converting fixed costs to variable costs solves budget problem. Can defect to alternative models (for-profit universities, online delivery) if needed. Experiences constraint as beneficial coordination mechanism.
constraint_indexing:constraint_classification(adjunctification_of_university_teaching, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SHARED GOVERNANCE NORMS (PITON) — The machinery of faculty senate, collegial decision-making, and peer-based governance persists theatrically while adjunctification strips actual faculty power. Shared governance structures vote to approve hiring decisions they cannot control; the ritual of faculty voice is maintained while real authority over labor relations is delegated to HR and budget. Theater ratio high because the apparatus of consultation persists while its functional impact has atrophied.
constraint_indexing:constraint_classification(adjunctification_of_university_teaching, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a purely structural economics frame, the conversion appears inevitable: rising costs, declining state funding, market pressures drive all institutions toward precarious labor. This perspective risks naturalizing what is actually a policy choice. The appearance of inevitability is the cover story that enables extraction to persist without organized resistance. The engine's false summit detector will identify this as naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(adjunctification_of_university_teaching, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(adjunctification_of_university_teaching_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(adjunctification_of_university_teaching, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(adjunctification_of_university_teaching, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(adjunctification_of_university_teaching, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(adjunctification_of_university_teaching, TR),
    TR >= 0.70.

:- end_tests(adjunctification_of_university_teaching_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but not maximal. The constraint extracts from adjuncts (reduced to per-course pay), from students (reduced educational quality), and from the tenure-track faculty (emotional/witnessing cost). But it is not a pure zero-sum predation — legitimate coordination functions exist (budget constraints, flexibility in course scheduling). The value reflects that extraction is real and asymmetric but wrapped in the appearance of coordination. Suppression (0.72): High. Individual-level suppression dominates: semester contracts prevent long-term planning, geographic dispersion across multiple campuses isolates workers, classification as independent contractors prevents unionization, administrative retaliation against organizing. Institutional suppression is active: adjunct committees have no voting power, organizing attempts are met with non-renewal of contracts. Class-level suppression is embedded in the structure itself — thousands of isolated per-course instructors cannot build collective power. Structural suppression is lowest because the policy is not hidden — it is naturalized as economic necessity. Theater ratio (0.61): Moderate-high. The theater appears in: (1) shared governance structures that continue to function while losing real authority. (2) Faculty participation in hiring decisions that administration can override. (3) Rhetoric of 'flexibility' and 'career diversity' applied to what is actually forced precarity. (4) The appearance that adjunctification is inevitable market force rather than policy choice. Over the interval, theater has increased as the apparatus of consultation persists while actual labor control is delegated to HR. Accessibility collapse (0.78): High. For adjuncts, alternatives have nearly collapsed: the credential-to-employment bridge is now primarily contingent contracts, not tenure lines. Debt load from graduate education makes exit to non-academic fields economically difficult. Geographic dispersion prevents concentration at any single institution where collective power could form. Resistance (0.42): Moderate. Individual resistance is low (fear of non-renewal suppresses visible dissent), but organizational and class-level resistance is emerging: adjunct unions in some institutions, cross-institutional organizing, faculty union chapters that include contingent workers. Structural-level resistance is minimal because the false-necessity narrative suppresses the question of whether the arrangement should exist at all.
 *
 * PERSPECTIVAL GAP:
 *   The largest gap is between the snare perspective (adjunct faculty seeing structural extraction with suppression) and the rope perspective (administration seeing pure coordination benefit). From the adjunct's position (trapped, powerless, biographical horizon), the constraint is clearly extractive — they bear costs with no exit. From the administration's position (institutional, arbitrage options, immediate horizon), the constraint solves a real problem (budget pressure) with no downside for them. The middle perspectives reveal the mechanism: tenure-track faculty (tangled_rope) see both coordination and extraction, and this middle position is where resistance could organize, but they are constrained by the threat that their own tenure is contingent on accepting the adjunct reality. The piton perspective reveals that the machinery of shared governance persists theatrically while losing function — this is how legitimacy is maintained despite the extraction. The analytical observer risks naturalizing the arrangement as economic necessity (mountain), which is the cover story that enables the extraction to persist unchallenged. The engine's false-summit detector will identify this as naturalization of a contingent policy choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from the structural position of each agent. Adjuncts as victims with trapped exit sit at d~0.95 (nearly full target). Administration as beneficiaries with arbitrage options sits at d~0.05 (nearly full beneficiary). Tenure-track faculty as mixed agents (benefiting from adjunct availability, constrained by complicity, moderate exit cost) sit at d~0.55 (symmetric extraction/benefit). The students as victims with constrained exit sit at d~0.75. Effective extraction (χ) is then computed by the engine from d and the constraint's metrics: for adjuncts, high d combined with high suppression produces maximum χ; for administration, low d produces negative χ (subsidy, not extraction). The piton perspective, though institutional, experiences low χ because the apparatus is performative rather than functional — the theater_ratio gate reduces the extracted value of maintaining shared governance machinery.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of shared governance and collegial decision-making in universities has outlived its function within this constraint. Faculty senates vote to approve hiring decisions they cannot control; adjunct committees without authority perform oversight; collegial processes are maintained while actual labor control is delegated to HR and budget mechanisms. The mandatrophy is not resolved because the apparatus persists — it is maintained theatrically to legitimate decisions that have already been made. Resolving the mandatrophy would require either: (1) restoring actual faculty authority over hiring (converting the constraint from snare back to rope or scaffold), or (2) explicitly dissolving the shared governance machinery and acknowledging that university governance is top-down (converting the piton from performance to structure). Neither has happened. The constraint persists by maintaining the theatrical machinery while gutting its function — a classic piton mechanism operating within the larger snare structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    funding_model_necessity,
    'Is adjunctification structurally necessary to maintain university operations given current funding models, or is it a policy choice enabled by weak governance?',
    'Comparative institutional analysis: peer institutions maintaining higher tenure-track ratios with comparable or superior outcomes; faculty cost burden as percentage of institutional budget vs. administrative bloat metrics; counterfactual budget scenarios with tuition increases or endowment reallocation',
    'If necessary: constraint reclassifies toward rope (coordination problem). If choice: remains snare with false-summit natural-law cover story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(funding_model_necessity, empirical, 'Whether adjunctification is structural necessity or policy choice').

omega_variable(
    adjunct_organizing_suppression,
    'What proportion of measured suppression is structural (contract-by-contract hiring, geographic dispersion) vs. institutional (active discouragement of organizing, retaliation, isolation of organizers)?',
    'Case studies of adjunct unionization attempts; documentation of retaliation patterns; analysis of hiring practices before/after organizing activity; testimony from adjuncts and administrators about organizing deterrence',
    'If institutional suppression is primary: the constraint includes a specific enforcement mechanism (retaliation) beyond the structural precarity. Strengthens snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjunct_organizing_suppression, empirical, 'Structure vs. institutional mechanisms in suppression of adjunct organizing').

omega_variable(
    natural_law_false_summit,
    'Does the ''inevitable market pressure'' narrative legitimize what is actually a policy choice by naturalizing it as economic law?',
    'Historical analysis of adjunctification timing relative to state funding cuts and policy decisions; comparative examination of institutions that resisted the trend and their outcomes; identification of decision points where institutional leadership chose adjunctification over alternative cost-reduction strategies',
    'If naturalizing: analytical-observer mountain perspective is false summit; engine''s FSM detector triggers. Reveals that the constraint is maintained by legitimacy narrative rather than structural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_false_summit, conceptual, 'False summit risk: naturalizing policy choice as economic law').

omega_variable(
    educational_quality_victim_absence,
    'Why is student education quality listed as a victim but not as an active stakeholder voice in governance decisions?',
    'Analysis of student representation in university budget and hiring decisions; measurement of student awareness of adjunct precarity and its effect on their education; comparison of institutional outcomes at high-adjunct vs. low-adjunct institutions',
    'If students are genuinely powerless and absent: strengthens snare classification (absent victims). If students could mobilize: opens alternative organizing path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(educational_quality_victim_absence, empirical, 'Educational quality victim status and student organizing capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(adjunctification_of_university_teaching, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(adjunct_tr_t0, adjunctification_of_university_teaching, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(adjunct_tr_t0, observed).
narrative_ontology:measurement(adjunct_tr_t10, adjunctification_of_university_teaching, theater_ratio, 10, 0.5).
narrative_ontology:measurement_basis(adjunct_tr_t10, observed).
narrative_ontology:measurement(adjunct_tr_t20, adjunctification_of_university_teaching, theater_ratio, 20, 0.61).
narrative_ontology:measurement_basis(adjunct_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(adjunct_be_t0, adjunctification_of_university_teaching, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(adjunct_be_t0, observed).
narrative_ontology:measurement(adjunct_be_t10, adjunctification_of_university_teaching, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(adjunct_be_t10, observed).
narrative_ontology:measurement(adjunct_be_t20, adjunctification_of_university_teaching, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(adjunct_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(adjunct_su_t0, adjunctification_of_university_teaching, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(adjunct_su_t0, observed).
narrative_ontology:measurement(adjunct_su_t10, adjunctification_of_university_teaching, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(adjunct_su_t10, observed).
narrative_ontology:measurement(adjunct_su_t20, adjunctification_of_university_teaching, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(adjunct_su_t20, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=20
narrative_ontology:measurement(adjunct_grid_01, adjunctification_of_university_teaching, accessibility_collapse(class), 0, 0.72).
narrative_ontology:measurement(adjunct_grid_02, adjunctification_of_university_teaching, accessibility_collapse(class), 20, 0.85).
narrative_ontology:measurement(adjunct_grid_03, adjunctification_of_university_teaching, accessibility_collapse(individual), 0, 0.65).
narrative_ontology:measurement(adjunct_grid_04, adjunctification_of_university_teaching, accessibility_collapse(individual), 20, 0.82).
narrative_ontology:measurement(adjunct_grid_05, adjunctification_of_university_teaching, accessibility_collapse(organizational), 0, 0.58).
narrative_ontology:measurement(adjunct_grid_06, adjunctification_of_university_teaching, accessibility_collapse(organizational), 20, 0.71).
narrative_ontology:measurement(adjunct_grid_07, adjunctification_of_university_teaching, accessibility_collapse(structural), 0, 0.48).
narrative_ontology:measurement(adjunct_grid_08, adjunctification_of_university_teaching, accessibility_collapse(structural), 20, 0.52).
narrative_ontology:measurement(adjunct_grid_09, adjunctification_of_university_teaching, resistance(class), 0, 0.38).
narrative_ontology:measurement(adjunct_grid_10, adjunctification_of_university_teaching, resistance(class), 20, 0.48).
narrative_ontology:measurement(adjunct_grid_11, adjunctification_of_university_teaching, resistance(individual), 0, 0.28).
narrative_ontology:measurement(adjunct_grid_12, adjunctification_of_university_teaching, resistance(individual), 20, 0.35).
narrative_ontology:measurement(adjunct_grid_13, adjunctification_of_university_teaching, resistance(organizational), 0, 0.45).
narrative_ontology:measurement(adjunct_grid_14, adjunctification_of_university_teaching, resistance(organizational), 20, 0.52).
narrative_ontology:measurement(adjunct_grid_15, adjunctification_of_university_teaching, resistance(structural), 0, 0.18).
narrative_ontology:measurement(adjunct_grid_16, adjunctification_of_university_teaching, resistance(structural), 20, 0.22).
narrative_ontology:measurement(adjunct_grid_17, adjunctification_of_university_teaching, stakes_inflation(class), 0, 0.65).
narrative_ontology:measurement(adjunct_grid_18, adjunctification_of_university_teaching, stakes_inflation(class), 20, 0.82).
narrative_ontology:measurement(adjunct_grid_19, adjunctification_of_university_teaching, stakes_inflation(individual), 0, 0.58).
narrative_ontology:measurement(adjunct_grid_20, adjunctification_of_university_teaching, stakes_inflation(individual), 20, 0.79).
narrative_ontology:measurement(adjunct_grid_21, adjunctification_of_university_teaching, stakes_inflation(organizational), 0, 0.42).
narrative_ontology:measurement(adjunct_grid_22, adjunctification_of_university_teaching, stakes_inflation(organizational), 20, 0.55).
narrative_ontology:measurement(adjunct_grid_23, adjunctification_of_university_teaching, stakes_inflation(structural), 0, 0.35).
narrative_ontology:measurement(adjunct_grid_24, adjunctification_of_university_teaching, stakes_inflation(structural), 20, 0.44).
narrative_ontology:measurement(adjunct_grid_25, adjunctification_of_university_teaching, suppression(class), 0, 0.71).
narrative_ontology:measurement(adjunct_grid_26, adjunctification_of_university_teaching, suppression(class), 20, 0.88).
narrative_ontology:measurement(adjunct_grid_27, adjunctification_of_university_teaching, suppression(individual), 0, 0.62).
narrative_ontology:measurement(adjunct_grid_28, adjunctification_of_university_teaching, suppression(individual), 20, 0.81).
narrative_ontology:measurement(adjunct_grid_29, adjunctification_of_university_teaching, suppression(organizational), 0, 0.48).
narrative_ontology:measurement(adjunct_grid_30, adjunctification_of_university_teaching, suppression(organizational), 20, 0.64).
narrative_ontology:measurement(adjunct_grid_31, adjunctification_of_university_teaching, suppression(structural), 0, 0.38).
narrative_ontology:measurement(adjunct_grid_32, adjunctification_of_university_teaching, suppression(structural), 20, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(adjunctification_of_university_teaching, resource_allocation).
narrative_ontology:affects_constraint(adjunctification_of_university_teaching, tenure_erosion_in_higher_education).
narrative_ontology:affects_constraint(adjunctification_of_university_teaching, student_debt_burden_amplification).
narrative_ontology:affects_constraint(adjunctification_of_university_teaching, academic_labor_precarity_cascade).

% DUAL FORMULATION NOTE:
% Adjunctification decomposes into structurally distinct constraints: (1) individual-level precarity (snare), (2) class-level organizing suppression (snare), (3) institutional-level shared governance atrophy (piton). Each story has distinct ε values reflecting different mechanisms. The three are linked: institutional suppression enables class-level isolation, which deepens individual precarity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(adjunctification_of_university_teaching, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
