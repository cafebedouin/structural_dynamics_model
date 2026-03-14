% ============================================================================
% CONSTRAINT STORY: information_asymmetry_edtech
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_information_asymmetry_edtech, []).

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
 *   constraint_id: information_asymmetry_edtech
 *   human_readable: Information Asymmetry in Educational Technology Markets
 *   domain: education/technology/economics
 *
 * SUMMARY:
 *   Educational technology markets exhibit a structural information asymmetry
 *   where vendors possess superior knowledge of system capabilities,
 *   limitations, security risks, and pedagogical effectiveness, while
 *   institutional buyers and students must make adoption decisions under
 *   uncertainty. This asymmetry is enforced through data lock-in, user
 *   switching costs, and limited transparency into system performance
 *   metrics. The constraint operates across seven distinct observational
 *   positions: from the student's perspective, it is a snare (pure extraction
 *   with no exit); from educators, a mixed coordination-extraction hybrid;
 *   from institutional buyers with bargaining power, a coordination mechanism
 *   with some asymmetry; from legacy institutions, a degraded ritual
 *   maintained through inertia; from open-education advocates, a temporary
 *   problem with an emerging sunset mechanism; and from a universal
 *   analytical view, an immutable feature of complex technology markets. The
 *   information asymmetry has grown over the interval as edtech systems have
 *   become more complex, integrated with student identity data, and entangled
 *   with institutional operations. Theater ratio has increased as
 *   institutions maintain compliance theater around vendor assessments even
 *   as underlying effectiveness remains unmeasured. The measurement
 *   trajectory shows extractiveness accumulating from 0.32 (early market) to
 *   0.62 (mature lock-in), while theater rises from 0.35 to 0.68, suggesting
 *   that the coordination function (genuine problem-solving) is being
 *   displaced by theatrical adoption and compliance performance.
 *
 * KEY AGENTS:
 *   - Edtech Vendors: Primary beneficiaries (institutional/arbitrage) — capture switching revenue, lock-in rents, and behavioral data while information asymmetry persists
 *   - School and University Administrators: Secondary beneficiaries (institutional/arbitrage) — benefit from administrative overhead reduction but are themselves information-disadvantaged relative to vendors
 *   - Students: Primary victims (powerless/trapped) — locked into platform ecosystems chosen by others, cannot evaluate quality, cannot port data or records
 *   - Educators: Secondary victims (moderate/constrained) — forced to adopt and teach unfamiliar platforms, face retraining costs, lose pedagogical autonomy
 *   - Families: Tertiary victims (powerless/constrained) — bear privacy risks from student data collection, lack transparency into data flows, cannot opt out
 *   - Open Educational Technology Coalition: Organized agents (organized/constrained) — Moodle Foundation, Open edX, faculty senate organizations building transparent alternatives
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing market information gaps as inherent to technology rather than policy choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(information_asymmetry_edtech, 0.58).
domain_priors:suppression_score(information_asymmetry_edtech, 0.62).
domain_priors:theater_ratio(information_asymmetry_edtech, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(information_asymmetry_edtech, extractiveness, 0.58).
narrative_ontology:constraint_metric(information_asymmetry_edtech, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(information_asymmetry_edtech, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(information_asymmetry_edtech, tangled_rope).
narrative_ontology:human_readable(information_asymmetry_edtech, "Information Asymmetry in Educational Technology Markets").
narrative_ontology:topic_domain(information_asymmetry_edtech, "education/technology/economics").

domain_priors:requires_active_enforcement(information_asymmetry_edtech).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(information_asymmetry_edtech, edtech_vendors).
narrative_ontology:constraint_beneficiary(information_asymmetry_edtech, institutional_buyers).
narrative_ontology:constraint_victim(information_asymmetry_edtech, students).
narrative_ontology:constraint_victim(information_asymmetry_edtech, educators).
narrative_ontology:constraint_victim(information_asymmetry_edtech, families).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STUDENT (SNARE) — Locked into platform ecosystems chosen by institutions without informed consent. Students cannot access competing tools, cannot port data, cannot evaluate which system serves their learning. Maximum suppression: switching costs are institutional (cannot transfer credits/records), psychological (platform lock-in through habit), and economic (no alternative is free). Pure extraction: students bear all switching costs and data lock-in burdens while vendors capture all switching revenue and behavioral data.
constraint_indexing:constraint_classification(information_asymmetry_edtech, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EDUCATOR (TANGLED ROPE) — Constrained by institutional adoption decisions and professional skill investment. Educators also benefit from the platform's coordination function (learning management, assessment automation, data dashboards) — the system genuinely solves communication and administrative problems. But the constraint contains asymmetric extraction: vendor lock-in, data visibility restrictions, and training costs favor vendors. Educators could theoretically exit (move to open-source tools) but face organizational barriers and retraining costs. Mixed experience: genuine coordination plus real extraction.
constraint_indexing:constraint_classification(information_asymmetry_edtech, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL BUYER (ROPE) — Schools and universities experience edtech as coordination: platforms solve real problems (attendance tracking, grade management, plagiarism detection, accessibility compliance) at scale. Institutions can arbitrage between vendors, negotiate discounts, and exit to alternatives (though at switching cost). Net beneficiary during contract: reduced administrative overhead, vendor support, compliance tools. Experience is coordination with exit options available to actors with bargaining power.
constraint_indexing:constraint_classification(information_asymmetry_edtech, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HIGH-RESOURCE INSTITUTION (TANGLED ROPE) — Well-funded schools can build custom solutions, negotiate better terms, or migrate to alternatives. Still experiences asymmetric information (vendors know their system's true reliability/limitations better than buyers do), but has sufficient power to demand transparency and exit. Classification remains tangled rope because information asymmetry persists even at this power level — the coordination function is real, but so is the extractive information gap.
constraint_indexing:constraint_classification(information_asymmetry_edtech, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: LEGACY LMS INSTITUTION (PITON) — Older LMS platforms (Blackboard, early Canvas adoptions) persist through institutional inertia despite younger alternatives offering better functionality. The constraint here is behavioral lock-in maintained through sunk costs and switching cost psychology, not genuine utility. Theater ratio is high: institutions continue paying for outdated systems because changing is perceived as risky, not because the old system functions well. Academic departments maintain Blackboard shells alongside Canvas; training still happens despite poor UX. The extractive mechanism is theater — the system keeps extracting through institutional theater, not real coordination.
constraint_indexing:constraint_classification(information_asymmetry_edtech, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN EDTECH COALITION (SCAFFOLD) — Organized actors (Moodle Foundation, Open edX, Instructure open-source initiatives, faculty governance movements) are building alternative verification pathways and transparency mechanisms. These represent a sunset clause in the original proprietary edtech constraint: as open-source alternatives mature, data portability standards spread, and faculty leverage grows, the information asymmetry gradually declines. High suppression is tolerated only if it declines over time — which it is, as governance models shift toward faculty participation and interoperability standards (LTI, xAPI) emerge.
constraint_indexing:constraint_classification(information_asymmetry_edtech, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, all information asymmetries in markets are inherent to complex goods: vendors will always know their systems better than buyers do, and this is a structural feature of capitalism, not a contingent institutional flaw. This perspective risks naturalizing what is actually a policy-contingent distribution of transparency rights, vendor liability, and data portability — all of which can be regulated. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(information_asymmetry_edtech, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(information_asymmetry_edtech_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(information_asymmetry_edtech, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(information_asymmetry_edtech, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(information_asymmetry_edtech, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(information_asymmetry_edtech, TR),
    TR >= 0.70.

:- end_tests(information_asymmetry_edtech_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Vendors capture clear rents through data lock-in, switching costs, and information advantage. But the extraction is not maximal because: (1) institutional buyers with sufficient scale can negotiate better terms and access to effectiveness data; (2) open-source alternatives exist, reducing lock-in to below the snare threshold; (3) the constraint coordinates legitimate administrative problems. The extractiveness has risen from 0.32 to 0.58 over the interval as market consolidation has reduced vendor competition and institutions have become more locked in. Suppression (0.62): Moderate-high. Students have no exit options (trapped at institution). Educators face high retraining costs and organizational inertia (constrained). Institutional buyers can theoretically exit but face switching costs, data migration risks, and vendor negotiation fatigue (constrained). Families cannot opt out without removing students from the institution (trapped). The suppression is maintained through both structural factors (integrated data systems, institutional dependencies) and informational factors (vendors control transparency about their own systems). Theater ratio (0.68): High. Institutional compliance with vendor assessments is substantially theatrical. Schools conduct 'edtech evaluations' that follow vendor-provided evaluation rubrics; training courses are mandated despite poor adoption outcomes; vendor partnerships are justified through vague 'educational innovation' language; retention metrics are presented as pedagogical outcomes when they reflect only system availability. The theater has increased over the interval as the market has matured and institutions face pressure to justify edtech spending rather than to genuinely assess pedagogical impact.
 *
 * PERSPECTIVAL GAP:
 *   The most dramatic perspectival gap is between the vendor (rope) and the student (snare). Vendors see the constraint as coordination — they are solving the legitimate problem of managing educational operations at scale, and they believe students benefit from access to sophisticated tools. The information asymmetry, from this view, is just normal competitive advantage: vendors have better knowledge of their own systems, and that's how markets work. Students see the constraint as pure extraction — they are forced into a system they did not choose, cannot exit, cannot evaluate, and from which their behavioral data is harvested. They receive no voice in the system's design, no transparency about how it works, and no ability to take their data elsewhere. The gap is not just perspectival disagreement; it reflects genuine structural difference. Vendors have arbitrage options and information advantage; students have no exit options and information disadvantage. The classification should differ dramatically, and it does: rope vs. snare. The secondary gap is between the institutional administrator (rope) and the educator (tangled rope). Administrators benefit from overhead reduction and vendor support; they have some negotiating leverage and can theoretically switch. Educators are forced to adopt, must retrain, and have no leverage in vendor negotiations. This gap reflects that institutional aggregation creates power (administrators can negotiate) while disaggregation eliminates it (individual educators cannot). A third gap is between the open-education coalition (scaffold) and the institutional inertia perspective (piton). The coalition sees emerging exits through open-source alternatives and governance reform; the piton perspective sees institutional lock-in maintained through sunk costs and switching cost psychology even when better alternatives exist. This gap is temporal: the piton is correct about current inertia, but the scaffold is correct about the direction of structural change.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from the agent's structural position relative to information flows and lock-in mechanisms. Vendors with asymmetric information and low exit costs have d ≈ 0.10-0.25 (beneficiaries). Institutional buyers with some negotiating power have d ≈ 0.40-0.55 (mixed). Educators with enforced adoption and retraining costs have d ≈ 0.65-0.75 (moderate targets). Students with zero agency and zero exit options have d ≈ 0.95 (full targets). The directionality derives from: (1) who possesses information that others lack; (2) who can enforce adoption; (3) who bears switching costs; (4) who owns behavioral data that emerges from the system. In all four dimensions, directionality flows from vendors toward students. This drives the chi formula: students experience high d, high f(d), and thus high χ; vendors experience low d, low f(d), and negative or near-zero χ. The institutional buyer occupies a middle position: they benefit from coordination (negative contribution to χ from their perspective) but bear some information disadvantage and switching cost (positive contribution to χ). The educator similarly occupies a middle position: they coordinate classroom administration (negative contribution) but are constrained and retrained (positive contribution).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the information asymmetry genuinely contains both coordination and extraction functions. The coordination component (learning management, attendance tracking, assessment automation, accessibility compliance, administrative workflow) is real — these systems solve genuine institutional problems and would need to be solved somehow even in a perfectly transparent market. The extraction component (data lock-in, switching cost enforcement, behavioral data harvesting, information advantage about system reliability) is also real and independent of whether the system coordinates well. The classification as tangled rope is correct because both functions are present and asymmetric — the vendors benefit from the coordination through their implementation while students bear the extraction costs without benefiting from the coordination function. A pure rope would be a system where all parties benefited from coordination symmetrically (true open standards with data portability and transparent effectiveness metrics). A pure snare would be one with no coordination function — pure lock-in with no legitimate problem-solving. The tangled rope correctly identifies that edtech is simultaneously solving real problems and extracting through information asymmetry. The mandatrophy is resolved by recognizing that reform should aim to decouple the coordination function (which is valuable) from the extraction mechanism (which is not), while maintaining the coordination benefit. This maps to the scaffold perspective: open standards, data portability, student governance participation, and vendor transparency would preserve the coordination while reducing extraction to normal competitive information gaps.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_portability_feasibility,
    'Is true data portability between educational platforms technically feasible and economically rational, or does it require standardization beyond current market incentives?',
    'Assessment of xAPI/IMS LTI adoption rates; technical audits of migration success between vendor platforms; cost-benefit analysis of portability infrastructure',
    'If feasible: information asymmetry can be substantially reduced through regulation. If not: asymmetry is partially structural and cannot be eliminated without public infrastructure investment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_portability_feasibility, empirical, 'Technical and economic feasibility of cross-platform data portability').

omega_variable(
    vendor_reliability_disclosure,
    'Can educational effectiveness metrics (student outcomes, accessibility compliance, security breach history) be reliably disclosed and compared across vendors in standardized form?',
    'Evaluation of existing effectiveness disclosure standards (if any); audit of whether vendors report comparable metrics; analysis of whether disclosed metrics predict actual institutional satisfaction',
    'If standardized comparison is possible: information asymmetry can be reduced to normal competitive information gaps. If vendors systematically obscure or cherry-pick metrics: asymmetry is enforced and institutional buyers remain systematically disadvantaged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_reliability_disclosure, empirical, 'Whether vendor effectiveness metrics can be standardized and reliably compared').

omega_variable(
    student_agency_mechanism,
    'What mechanism could restore student voice in edtech selection without creating advisory overhead that exceeds educational value?',
    'Piloting of student representation in adoption committees; analysis of whether student input changes vendor selection; cost-benefit assessment of formalized student governance',
    'If viable mechanism exists: students transition from powerless/trapped to constrained/mobile. If no mechanism works: student powerlessness is structural and cannot be fixed by institutional process alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(student_agency_mechanism, empirical, 'Mechanism for student participation in edtech governance without process overhead').

omega_variable(
    open_source_adoption_barrier,
    'What is the actual switching cost (in labor, training, institutional friction) of moving from proprietary to open-source edtech, and does it exceed the information-asymmetry discount that institutions receive?',
    'Cost accounting of institutions that have migrated to open-source; comparison of total cost of ownership (TCO) for proprietary vs open-source; timeline analysis of migration difficulty',
    'If switching cost is low: open-source alternative should be dominant; vendors extract rent through coordination value, not lock-in. If switching cost is high: even ''free'' open-source cannot solve lock-in; information asymmetry persists because exit is expensive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(open_source_adoption_barrier, empirical, 'Real switching cost for institutional migration to open-source edtech').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(information_asymmetry_edtech, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infoasym_tr_t0, information_asymmetry_edtech, theater_ratio, 0, 0.35).
narrative_ontology:measurement(infoasym_tr_t5, information_asymmetry_edtech, theater_ratio, 5, 0.52).
narrative_ontology:measurement(infoasym_tr_t10, information_asymmetry_edtech, theater_ratio, 10, 0.68).
narrative_ontology:measurement(infoasym_tr_t15, information_asymmetry_edtech, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(infoasym_be_t0, information_asymmetry_edtech, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(infoasym_be_t5, information_asymmetry_edtech, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(infoasym_be_t10, information_asymmetry_edtech, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(infoasym_be_t15, information_asymmetry_edtech, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(information_asymmetry_edtech, resource_allocation).
narrative_ontology:affects_constraint(information_asymmetry_edtech, student_data_privacy_edtech).
narrative_ontology:affects_constraint(information_asymmetry_edtech, vendor_lock_in_learning_platforms).

% DUAL FORMULATION NOTE:
% Information asymmetry in edtech is upstream of both student privacy violations (vendors have superior knowledge of data flows) and vendor lock-in (information disadvantage makes exit decisions appear riskier than they are). The information asymmetry story has its own extractiveness value (0.58) reflecting the core information gap; the downstream stories model specific extraction mechanisms (data harvesting, switching cost enforcement) that depend on this asymmetry existing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(information_asymmetry_edtech, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
