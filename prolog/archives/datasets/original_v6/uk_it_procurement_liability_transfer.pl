% ============================================================================
% CONSTRAINT STORY: uk_it_procurement_liability_transfer
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_it_procurement_liability_transfer, []).

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
 *   constraint_id: uk_it_procurement_liability_transfer
 *   human_readable: UK IT Procurement Liability Transfer Mechanism
 *   domain: public_procurement/risk_allocation
 *
 * SUMMARY:
 *   The UK IT procurement liability transfer mechanism is a structural
 *   arrangement in which government agencies assume contractual and
 *   operational liability for contractor performance while lacking direct
 *   control over delivery. This constraint exhibits multiple classification
 *   types across different observer positions: civil servants experience a
 *   snare (trapped by statute and market structure), contractors experience
 *   coordination (clear liability boundaries enable efficient pricing), the
 *   Public Accounts Committee performs degraded oversight (piton), and reform
 *   advocates see a solvable temporary problem (scaffold). The mechanism
 *   combines genuine coordination function (clear risk allocation enables
 *   contracting) with asymmetric extraction (liability concentrated on
 *   government side without corresponding control). Extractiveness has risen
 *   from 0.42 to 0.58 over 15 years as IT complexity has grown and contractor
 *   consolidation has reduced competitive pressure, while theater_ratio has
 *   increased (performative accountability rituals substitute for actual risk
 *   restructuring). The constraint demonstrates how a policy choice (where to
 *   place liability in contracts) becomes naturalized as an inevitable
 *   feature of procurement.
 *
 * KEY AGENTS:
 *   - IT Contractors: Primary beneficiaries (institutional/arbitrage) — benefit from liability transfer clauses; can exit or repricing underperforming contracts
 *   - Civil Service Procurers: Primary victims (powerless/trapped) — statutorily responsible for competitive procurement; contractually bound to accept risk transfer
 *   - Government Agencies: Secondary victims (powerless/trapped) — bear both financial and reputational risk for contractor failure
 *   - Public Accounts Committee: Institutional actor (institutional/arbitrage) — performs oversight ritual without structural authority to change terms
 *   - Government Digital Service: Organized reformers (organized/constrained) — attempting to rationalize procurement through new standards
 *   - Procurement Reform Movement: Organized coalition (organized/constrained) — advocating shifted risk models with visible exit path
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing policy choice as immutable constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_it_procurement_liability_transfer, 0.58).
domain_priors:suppression_score(uk_it_procurement_liability_transfer, 0.65).
domain_priors:theater_ratio(uk_it_procurement_liability_transfer, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_it_procurement_liability_transfer, extractiveness, 0.58).
narrative_ontology:constraint_metric(uk_it_procurement_liability_transfer, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(uk_it_procurement_liability_transfer, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_it_procurement_liability_transfer, tangled_rope).
narrative_ontology:human_readable(uk_it_procurement_liability_transfer, "UK IT Procurement Liability Transfer Mechanism").
narrative_ontology:topic_domain(uk_it_procurement_liability_transfer, "public_procurement/risk_allocation").

domain_priors:requires_active_enforcement(uk_it_procurement_liability_transfer).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_it_procurement_liability_transfer, it_contractors).
narrative_ontology:constraint_beneficiary(uk_it_procurement_liability_transfer, cabinet_office_efficiency_targets).
narrative_ontology:constraint_victim(uk_it_procurement_liability_transfer, government_agencies).
narrative_ontology:constraint_victim(uk_it_procurement_liability_transfer, public_service_continuity).
narrative_ontology:constraint_victim(uk_it_procurement_liability_transfer, taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVIL SERVICE PROCURER (SNARE) — Trapped by statutory duty to competitively tender; contractually bound to accept risk transfer clauses as standard procurement terms. Cannot exit without violating public law. Bears full liability for contractor failure while lacking operational control. Career risk compounds the structural trap: failure to deliver IT projects damages advancement prospects.
constraint_indexing:constraint_classification(uk_it_procurement_liability_transfer, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GOVERNMENT AGENCY (SNARE) — Statutory responsibility for service delivery but contractual liability for contractor performance. Cannot refuse the IT system or transfer responsibility further without abandoning the service. Suppressed by legal framework (public law duties) and market structure (few qualified vendors). Maximum extraction: agency pays for the system AND bears liability when it fails.
constraint_indexing:constraint_classification(uk_it_procurement_liability_transfer, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: IT CONTRACTOR (ROPE) — Benefits from liability transfer clauses in standard form contracts. Experiences the constraint as a coordination mechanism: clear liability boundaries enable competitive pricing and risk-appropriate resource allocation. Can arbitrage between contracts; can exit underperforming accounts. Net beneficiary of the liability transfer structure.
constraint_indexing:constraint_classification(uk_it_procurement_liability_transfer, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: GOVERNMENT DIGITAL SERVICE (TANGLED ROPE) — Organized agents attempting to rationalize procurement and drive digital standards. Benefit from clear service specifications that enable contract standardization; bear extraction when bad contracts persist due to path dependence. Constrained by legacy infrastructure and political resistance to renegotiation. See the constraint as simultaneously enabling better practice (coordination) and locking in problematic allocations (extraction).
constraint_indexing:constraint_classification(uk_it_procurement_liability_transfer, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC ACCOUNTS COMMITTEE (PITON) — Performs oversight function through performative inquiry cycles: investigate failed projects, issue reports, demand improvements, contracts still drafted identically. Theater_ratio reflects the ritual nature of accountability — reports are published but procurement terms persist unchanged. Maintains appearance of control over risk allocation despite persistent project failures. Degraded institutional function maintained through inertia.
constraint_indexing:constraint_classification(uk_it_procurement_liability_transfer, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: PROCUREMENT REFORM MOVEMENT (SCAFFOLD) — Organized civil servants, academics, and reform advocates working to shift liability allocation toward shared risk models and outcome-based contracting. See the constraint as temporary and solvable through procedural change. Low effective extraction because this coalition has agency and sees an exit path: revised Standard Government Contract terms, outcome-based SLAs, and capability-building in client organizations. Sunset horizon: 5-10 years as new procurement models mature.
constraint_indexing:constraint_classification(uk_it_procurement_liability_transfer, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, risk transfer is an inherent property of complex procurement: someone must bear the risk of failure, and in market transactions, liability naturally flows to the party with operational control. This perspective sees the constraint as an inevitable feature of how IT contracting must work. However, the base properties reveal this is a false summit: risk allocation is a policy choice, not a law of nature. Comparative analysis (Australia's shared risk models, Norway's capability-building approach) demonstrates structural alternatives.
constraint_indexing:constraint_classification(uk_it_procurement_liability_transfer, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_it_procurement_liability_transfer_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_it_procurement_liability_transfer, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_it_procurement_liability_transfer, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_it_procurement_liability_transfer, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_it_procurement_liability_transfer, TR),
    TR >= 0.70.

:- end_tests(uk_it_procurement_liability_transfer_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Government agencies and civil servants bear contractual liability while lacking operational control over IT delivery. Contractors retain pricing power despite liability transfer — they bundle risk premiums into fixed-price contracts and shift actual risks through subcontracting and scope interpretation. The extraction is not at snare levels (0.66+) because some of the liability transfer reflects genuine pricing for real risk; however, government capacity to negotiate alternative terms has declined as contractor consolidation has reduced competitive pressure. Theater ratio (0.68): Moderate-high. Public Accounts Committee investigations follow a ritualized cycle: failed project identified, report issued, recommendations made, contracts drafted identically the next cycle. The performative nature reflects that accountability mechanisms operate on oversight side (reports, inquiries) while procurement terms remain controlled by contractors. Theater has increased because IT project failures persist despite accountability investigations — the investigation ritual substitutes for structural change. Suppression (0.65): High. Statutory duty to competitively tender constrains procurers' ability to reject contractor terms. Market structure (few large contractors, complex technical requirements) suppresses agency's ability to build internal IT capacity or negotiate alternatives. Career risk suppresses innovation: procurement teams who attempt novel risk allocation models face blame if the model fails.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between contractors (rope — clear liability boundaries enable efficient coordination) and civil servants (snare — liability concentration with no control). Contractors see the constraint as solving the coordination problem of IT risk allocation; civil servants see it as pure extraction justified by market scarcity narratives. The Public Accounts Committee gap reflects performative oversight: they conduct investigations and issue reports (theater) but lack authority to restructure procurement terms, creating an illusion of control. The reform movement gap shows a tension between scaffold (sunset is plausible if political will materializes) and piton (sunset is perpetually deferred). The analytical observer gap reveals false naturalization: the constraint is presented as inherent to market procurement, but comparative analysis shows it is a policy choice that other countries have avoided.
 *
 * DIRECTIONALITY LOGIC:
 *   Contractor directionality (d≈0.15): Beneficiary status + arbitrage exit produce low d — contractors experience the constraint as enabling (clear liability boundaries permit competitive bidding and arbitrage between contracts). Government directionality (d≈0.88): Victim status + trapped exit produce high d — government cannot exit competitive tendering obligation, cannot refuse the IT system, and cannot reallocate liability without creating budget gaps or project delays. The high f(d) for government actors means they experience the effective extraction (chi) acutely despite the base extractiveness (0.58) being moderate. Organized reform coalitions (d≈0.55): Mixed status — they are institutional actors with constrained (not trapped) exit and advocacy capacity, but the constraint is embedded in public law and cabinet office policy, limiting their leverage.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing genuine coordination function (liability transfer does enable clearer contracting) from asymmetric extraction (government bears disproportionate downside risk). The classification as tangled_rope is defensible: the mechanism coordinates pricing and risk allocation (genuine benefit to contractors and, in theory, to government through more efficient bids) while extracting through concentrated liability allocation (actual government outcomes are worse than available alternatives). The false summit mountain classification is a critical diagnostic: it reveals that the 'naturally inevitable' framing of liability transfer is a cover story for institutional path dependence. If the mountain were real, comparative procurement models would fail; instead, countries with different liability allocations (Australia, Norway, Singapore) achieve comparable or better outcomes, confirming the constraint is contingent institutional arrangement, not law of nature. The constraint is defensible as tangled_rope only if the genuine coordination benefits exceed the extraction costs — longitudinal data on project success rates across procurement models is required to sustain this classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liability_allocation_vs_pricing_efficiency,
    'Does shifting liability to government agencies genuinely improve contractor pricing efficiency, or does it permit cost-plus extraction without incentive for delivery?',
    'Comparative analysis of IT project costs and delivery performance across procurement models (fixed-price vs shared-risk vs cost-reimbursable); correlation between liability transfer intensity and project outcomes',
    'If efficiency gains are real: the tangled_rope classification holds — genuine coordination benefit justifies some extraction. If efficiency gains are illusory: reclassify as snare — pure extraction justified by false efficiency narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_allocation_vs_pricing_efficiency, empirical, 'Whether liability transfer produces genuine pricing efficiency or permits cost extraction').

omega_variable(
    contractor_skill_scarcity_dependency,
    'Is the government''s acceptance of adverse liability terms driven by genuine scarcity of IT contractor capacity, or by institutional path dependence and procurement inertia?',
    'Market capacity analysis: ratio of qualified vendors to active procurements; analysis of contractor switching costs; survey of government procurement teams on actual vs perceived vendor availability',
    'If capacity scarcity is real: some extraction is unavoidable (high f(d) for government). If scarcity is manufactured or outdated: extracted value represents pure rent — reclassify toward higher snare signature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contractor_skill_scarcity_dependency, empirical, 'Whether scarcity of IT contractor capacity justifies liability allocation').

omega_variable(
    reform_sunset_timeline_feasibility,
    'Is the 5-10 year sunset horizon realistic for procurement reform, or does institutional inertia extend timeline indefinitely?',
    'Tracking of Standard Government Contract adoption rates; longitudinal analysis of major agency compliance with new outcome-based SLA frameworks; interviews with procurement leadership on actual barriers to change',
    'If timeline is realistic: scaffold classification confirmed. If sunset is perpetually deferred: reclassify toward piton — the reform movement is performative theater, not a genuine exit path.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_sunset_timeline_feasibility, empirical, 'Whether procurement reform sunset timeline is credible').

omega_variable(
    false_summit_naturalization,
    'Is the perception that liability transfer is inherent to procurement a genuine structural insight or a naturalization of contingent institutional arrangements?',
    'Cross-national comparative study of procurement liability allocation models (UK fixed-price transfer vs Australian shared-risk vs Norwegian capability-building); analysis of which models produce better outcomes for public value',
    'If naturalization is detected: mountain classification is false — reveals how institutional actors use inevitability rhetoric to block policy alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether liability transfer is structural necessity or naturalized institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_it_procurement_liability_transfer, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ukit_tr_t0, uk_it_procurement_liability_transfer, theater_ratio, 0, 0.55).
narrative_ontology:measurement(ukit_tr_t7, uk_it_procurement_liability_transfer, theater_ratio, 7, 0.62).
narrative_ontology:measurement(ukit_tr_t15, uk_it_procurement_liability_transfer, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(ukit_be_t0, uk_it_procurement_liability_transfer, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ukit_be_t7, uk_it_procurement_liability_transfer, base_extractiveness, 7, 0.5).
narrative_ontology:measurement(ukit_be_t15, uk_it_procurement_liability_transfer, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_it_procurement_liability_transfer, enforcement_mechanism).
narrative_ontology:affects_constraint(uk_it_procurement_liability_transfer, uk_it_project_delivery_failure).
narrative_ontology:affects_constraint(uk_it_procurement_liability_transfer, contractor_consolidation_market_power).

% DUAL FORMULATION NOTE:
% Liability transfer is downstream of broader UK procurement policy (competitive tendering mandates); upstream constraints include statutory duty to competitively tender and market consolidation reducing contractor competition. Linked to IT project delivery failure constraint which is a consequence of liability misallocation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_it_procurement_liability_transfer, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
