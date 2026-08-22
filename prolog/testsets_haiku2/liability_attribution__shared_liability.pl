% ============================================================================
% CONSTRAINT STORY: liability_attribution__shared_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__shared_liability, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: liability_attribution__shared_liability
 *   human_readable: Joint Liability Attribution Along Value Chain
 *   domain: legal/technological/regulatory
 *
 * SUMMARY:
 *   Shared liability distributes responsibility for software failures across
 *   developers and deployers based on causal contribution and control. A
 *   developer who ships code with a vulnerability bears some liability; a
 *   deployer who misconfigures that code in production also bears liability.
 *   The rule is presented as fair allocation and incentive alignment; critics
 *   read it as obscuring true responsibility through complex causal and
 *   control assessments, transferring opacity costs to end users, and
 *   enriching insurance and legal institutions. This story instantiates the
 *   SHARED LIABILITY reading of the contested liability_attribution kernel,
 *   one of three contending readings (developer_liability and
 *   deployer_liability are the siblings). The shared-liability reading's core
 *   claim: responsibility should distribute along both the causal chain (what
 *   did each party contribute to the failure?) and the control chain (who had
 *   decision authority to prevent it?). This claim forecloses the
 *   pure-developer reading (which denies deployer causal contribution
 *   matters) but coexists with the pure-deployer reading (both can be live
 *   positions held by different parties and litigated separately). The
 *   constraint's metrics reflect rising extractiveness as shared-liability
 *   institutions (insurance, legal interpretation) consolidate market power,
 *   theater rising as indemnification products become routinized, and
 *   suppression modest (developers and deployers contest the rule actively)
 *   but rising as contractual standards harden around shared allocations.
 *
 * KEY AGENTS:
 *   - software_developers: moderate power, constrained exit — bear joint liability exposure, face indemnification demands, insurance costs
 *   - deployment_operators: powerful to moderate power, constrained exit — retain deployment-context liability, pass indemnification to developers, distribute costs to users
 *   - liability_enforcement_institutions (courts, regulators): institutional power, analytical exit — benefit from complex causal/control assessments, generate interpretive authority
 *   - insurance_and_indemnification_markets: institutional power, arbitrage exit — benefit from shared-liability complexity, create specialized risk products
 *   - end_users: powerless, trapped exit — bear opacity and price costs, excluded from liability-allocation design
 *   - regulatory_authorities: institutional power, analytical exit — set and adjudicate the shared-liability framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__shared_liability, 0.68).
domain_priors:suppression_score(liability_attribution__shared_liability, 0.52).
domain_priors:theater_ratio(liability_attribution__shared_liability, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, extractiveness, 0.68).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__shared_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__shared_liability, "Joint Liability Attribution Along Value Chain").
narrative_ontology:topic_domain(liability_attribution__shared_liability, "legal/technological/regulatory").

domain_priors:requires_active_enforcement(liability_attribution__shared_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__shared_liability, '11d0654b-48d1-4b05-82d7-883ead9a8c4a').
narrative_ontology:cs_kernel_codification('11d0654b-48d1-4b05-82d7-883ead9a8c4a', formalized).
narrative_ontology:cs_authority_grounding('11d0654b-48d1-4b05-82d7-883ead9a8c4a', lineage).
narrative_ontology:cs_interpretation_layer_present('11d0654b-48d1-4b05-82d7-883ead9a8c4a').
narrative_ontology:cs_reading_relation('11d0654b-48d1-4b05-82d7-883ead9a8c4a', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('11d0654b-48d1-4b05-82d7-883ead9a8c4a', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_axiom('11d0654b-48d1-4b05-82d7-883ead9a8c4a', foundational, causal_contribution_allocates_liability).
narrative_ontology:cs_axiom_status(causal_contribution_allocates_liability, holdable).
narrative_ontology:cs_axiom_grounding('11d0654b-48d1-4b05-82d7-883ead9a8c4a', causal_contribution_allocates_liability, deontological).
narrative_ontology:cs_axiom('11d0654b-48d1-4b05-82d7-883ead9a8c4a', foundational, control_authority_allocates_liability).
narrative_ontology:cs_axiom_status(control_authority_allocates_liability, holdable).
narrative_ontology:cs_axiom_grounding('11d0654b-48d1-4b05-82d7-883ead9a8c4a', control_authority_allocates_liability, deontological).
narrative_ontology:cs_reference_frame('11d0654b-48d1-4b05-82d7-883ead9a8c4a', proportional_responsibility_framework).
narrative_ontology:cs_drift_state('11d0654b-48d1-4b05-82d7-883ead9a8c4a', contemporary_insurance_commodification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('11d0654b-48d1-4b05-82d7-883ead9a8c4a', '').
narrative_ontology:cs_kernel_id(liability_attribution__shared_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, liability_enforcement_institutions).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, insurance_and_indemnification_markets).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, software_developers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, deployment_operators).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, end_users_bearing_opacity_costs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, software_developers).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, deployment_operators).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Create software capabilities and distribute them. Under shared liability, bear joint responsibility for harms caused by their code even after deployment to contexts they do not control and cannot fully anticipate. Face indemnification demands from deployers, insurance premiums reflecting shared-liability exposure, and litigation costs. Exit is constrained: refusal to accept shared liability means market exclusion; offshore relocation is possible but costly and operationally limited. Benefit from shared liability only insofar as it creates institutional predictability and insurance markets that could allocate risk more efficiently than unilateral developer liability would.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, software_developers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__shared_liability, software_developers, beneficiary).

% Deploy software into production contexts, make decisions about configuration, access control, and usage conditions. Under shared liability, retain responsibility for harms because they control deployment context and could have mitigated risks through configuration choices. Face joint liability exposure, indemnification demands to developers (contractual pass-throughs), and direct liability to end users. Powerful institutional actors (cloud platforms, financial services firms) have resources to absorb and allocate the liability burden; smaller operators are constrained by insurance costs and indemnification negotiations. Benefit from shared liability insofar as it creates insurance markets and clarifies responsibility boundaries — pure deployer liability would leave developers with unmanageable tail risk.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, deployment_operators, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__shared_liability, deployment_operators, beneficiary).

% Courts, regulatory bodies, and legal institutions that adjudicate liability. Benefit from shared liability as an allocation rule that generates interpretive work, precedent-setting opportunities, and institutional authority over responsibility assignment. The rule requires continuous legal interpretation (what counts as causal contribution? What does control entail?), which creates demand for litigation and regulatory clarification. Shared liability is more complex to adjudicate than unilateral rules, generating more institutional value.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, liability_enforcement_institutions, beneficiary,
    institutional, generational, analytical, national).

% Insurance carriers and indemnification product designers benefit from shared liability as a complex, risk-partitioned responsibility structure. Shared liability creates demand for specialized coverage (development liability, deployment context liability, joint-and-several exposure), premium differentiation based on causal-contribution assessments, and indemnification products that allocate risk between developers and deployers. The opacity and complexity of shared-liability assessment enables higher margins on specialized products.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, insurance_and_indemnification_markets, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the diffuse costs of shared liability without voice in its design: higher software prices as developers and deployers pass liability and insurance costs through to users, opacity about risk allocation (they cannot easily determine who is liable for a failure), and delayed remedies as liability disputes slow security fixes. Their exclusion from the liability-assignment conversation is structural — they are harmed by failures but have no seat at the table where causal contribution and control are assessed.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, end_users, payer,
    powerless, immediate, trapped, universal).
narrative_ontology:stakeholder_secondary_role(liability_attribution__shared_liability, end_users, excluded).

% Would argue for developer liability only — the reading that places full responsibility on code creators, not deployers. Are excluded from liability-allocation decisions by the shared-liability regime, which presumes deployer involvement in risk assessment. Identity-locked: their exit would require repudiating the entire liability-sharing framework, which would undermine their own credibility in regulatory forums.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, alternative_developers, excluded,
    moderate, biographical, identity_locked, global).

% Would argue for deployer liability only — the reading that places primary responsibility on operators with deployment context and control. Are excluded from liability-allocation decisions by the shared-liability regime. Constrained exit: they could litigate against shared liability, but the cost and uncertainty make this a weak exit; regulatory capture is a more realistic path (lobbying for deployer-liability readings in specific sectors).
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, alternative_deployers, excluded,
    powerful, generational, constrained, global).

% Set the legal framework defining shared liability, determine how causal contribution and control are assessed, and adjudicate disputes. Derive authority from statutory law and common-law precedent. Their decisions about what counts as sufficient causal contribution (should developers be liable for deployment misconfiguration?) and how control is measured (does control flow from technical capability or decision authority?) directly determine which stakeholders bear costs and whether the regime persists.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__shared_liability, insurance_and_indemnification_markets).
narrative_ontology:fixing_cost_class(liability_attribution__shared_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates responsibility for software failures across the value chain, creating incentives for both developers and deployers to invest in safety and risk mitigation. A shared scheme reduces the tail risk borne by any single party (if developers bore all liability, they would face uninsurable tail risk from deployer negligence; if deployers bore all, developers would have no incentive to ship secure code). Coordinates the distribution of due care across two interdependent actors.
% TRANSFER_FUNCTION: Moves liability exposure (and associated insurance costs) from one party to both, increasing total coordination costs in the system. Transfers decision-making authority about risk allocation from unilateral rules to contested causal-contribution and control assessments. Transfers wealth from developers and deployers to insurance markets and legal institutions through premiums, indemnification products, and litigation.
% ABSENT_VOICES: End users (who bear opacity and higher prices) are excluded structurally. Alternative developers advocating unilateral developer liability and alternative deployers advocating unilateral deployer liability are excluded by the shared-regime's framing. Open-source developers and small deployers who lack resources for liability negotiation and insurance are marginalized by the complexity of the rule.
% DISAPPEARANCE_RATIONALE: If shared liability vanished overnight, the legal and contractual landscape would reorganize around either unilateral developer liability (shifting all burden to code creators) or unilateral deployer liability (shifting all burden to operators). Insurance markets would collapse and reform around whichever unilateral rule emerged. Developers and deployers would renegotiate contracts without indemnification clauses structured around shared responsibility. Software prices and deployment infrastructure costs would shift as insurance and legal exposure changed.
% FOUNDING_PROBLEM: Early software liability law assigned responsibility unclear-ly: was a developer liable for code that worked as written but was misdeployed? Was a deployer liable for choosing insecure configurations? Unilateral rules created extreme tail risk for whoever bore full liability, deterring innovation and deployment. Shared liability emerged as a compromise allocating risk between two parties with different information and control.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and technologists outside the beneficiary set (insurance and enforcement institutions) attest that unilateral liability rules create misaligned incentives and extreme tail risk. The founding problem is contested: some argue shared liability solved it; others argue it created new problems (opacity, litigation explosion, risk migration to end users) without solving the underlying information and control asymmetries.
narrative_ontology:disappearance_verdict(liability_attribution__shared_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__shared_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__shared_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(liability_attribution__shared_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__shared_liability, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__shared_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__shared_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__shared_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.68 over the interval as shared-liability markets mature and indemnification products become standard, locking in the cost structure. The rise plateaus around year 25 (baseline extractiveness reaches stability) because shared liability becomes the institutional default and litigation patterns stabilize. Theater rises from 0.25 to 0.41 and plateaus because indemnification clauses and causal-contribution assessments become routinized and decoupled from actual risk mitigation — the shared-liability framing persists even as its original coordination function (allocating risk between two interdependent parties) becomes obscured by insurance commodification. Suppression rises from 0.38 to 0.52 as contractual standards harden (developers accept shared-liability clauses as standard; deployers routinize indemnification language) but plateaus because active resistance remains moderate — both developers and deployers have institutional resources and do not face complete suppression. Accessibility collapse is modest (0.48) because alternative liability regimes (pure developer or pure deployer) remain logically available and are actively advocated by excluded parties; they are not materially accessible but they remain thinkable. Resistance is high (0.71) because both developers and deployers actively contest shared-liability assessments through litigation, regulatory lobbying, and contract renegotiation.
 *
 * PERSPECTIVAL GAP:
 *   The enforcement-institution seat (courts, regulators) experiences shared liability as a coordination function creating interpretive work and institutional authority — they benefit from the complexity. The developer and deployer seats experience it as cost-shifting and opacity, constrained by contractual standards. End users experience it as pure cost (higher prices, opacity about responsibility) without voice. The engine should compute these different types from the structural data: enforcement institutions see a rope-like coordination of responsibility; developers and deployers see an extractive tangled rope; end users see a snare. The authored claim (tangled_rope) describes the payer seats' experience; the engine's per-seat computation will diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers and deployers are joint payers (d near 0.8–0.9): they bear both the direct liability exposure and the indirect costs of insurance and indemnification. Liability enforcement institutions and insurance markets are beneficiaries (d near 0.0–0.2): they collect institutional value and market rents without bearing operational liability. End users are indirect payers (d near 0.85–0.95): they bear opacity and price escalation without voice. The beneficiaries are not traditional agents extracting rents but rather institutional seats that benefit from complexity: courts gain interpretive authority, insurers gain specialized-product market share. This justifies the inclusion of institutional beneficiaries alongside the operational payers.
 *
 * MANDATROPHY ANALYSIS:
 *   Shared liability was founded to solve the problem of misaligned incentives and uninsurable tail risk under unilateral rules. The founding problem is LIVE but increasingly contested: developers and deployers argue that shared liability creates opacity and higher coordination costs without actually improving safety outcomes; enforcement institutions argue that causal-contribution assessment enables more precise incentive alignment. The mandatrophy question: Has shared liability become a zombie rule, persisting because enforcement institutions benefit from its complexity rather than because it solves the founding coordination problem? The theater ratio's rise (0.25 to 0.41) supports this concern — more activity is devoted to indemnification negotiation and causal assessment than to actual safety improvement. However, the constraint remains genuinely contested (both alternative readings remain advocated), so it has not yet crossed into pure piton territory (which requires administrative inertia plus zero beneficiary demand). The classification as tangled_rope is correct: shared liability is both coordination (allocating responsibility across the value chain) and extraction (concentrating benefits in enforcement institutions and insurance markets).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_contribution_assessment,
    'How much of the liability burden should be allocated based on causal contribution (what each party contributed to the failure) versus control (who had decision authority to prevent it)? Where is the boundary between these two axes?',
    'Legal interpretation through precedent: courts will establish through cases whether causal contribution or control is weighted more heavily in liability allocation. Regulatory guidance from sector-specific authorities (financial services, health tech) will clarify the boundary.',
    'If courts weight causal contribution heavily, developers bear higher liability (code defects are clear causal contributions); if control is weighted heavily, deployers bear higher liability (they control deployment context). The balance directly determines whether the shared-liability regime tilts toward developer or deployer burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_contribution_assessment, empirical, 'Weighting of causal contribution versus control in liability allocation.').

omega_variable(
    shared_liability_opacity_cost,
    'How much of the measured extractiveness (0.68) represents genuine coordination costs (due diligence, insurance markets, liability assessment infrastructure) versus opacity and rent-seeking in indemnification products and legal fees?',
    'Post-shift empirical comparison: if a jurisdiction moved to unilateral deployer or developer liability and measured the total system cost (including litigation, but without indemnification overhead), would the cost be lower? Regulatory transparency about insurance-product margins and legal-fee concentration would reveal rent-seeking.',
    'If shared-liability costs are mostly genuine coordination (due diligence across the value chain), the tangled_rope classification is defensible. If most are opacity and rent-seeking, the constraint tilts toward snare. The theater ratio''s rise (to 0.41) suggests increasing opaqueness, but this remains to be verified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(shared_liability_opacity_cost, empirical, 'Proportion of extractiveness that represents genuine coordination versus rent-seeking.').

omega_variable(
    alternative_reading_suppression,
    'Are alternative readings (pure developer or pure deployer liability) suppressed by shared liability, or do they remain genuinely available as policy choices?',
    'Regulatory and legislative tracking: are alternative readings advocated in regulatory forums, litigated in courts, or adopted in jurisdictions that opt out of shared liability? If alternative readings persist as live positions held by institutional actors, suppression is low; if they are foreclosed by precedent or regulatory entrenchment, suppression is high.',
    'High suppression would indicate that shared liability has become the institutional default with alternatives marginalized — this would support a higher suppression metric and a shift toward piton or snare. Low suppression would indicate that the liability question remains genuinely contested — supporting the tangled_rope classification and the measured suppression value of 0.52.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_suppression, empirical, 'Whether alternative liability readings remain institutionally viable or are suppressed by shared-liability entrenchment.').

omega_variable(
    kernel_reading_containment,
    'Is shared liability a reading that binds the entire kernel (applies globally across all software liability questions), or is it a localized reading specific to particular domains (cloud computing, open-source, enterprise software)?',
    'Jurisdiction and sector tracking: do U.S. federal courts, EU regulators, or other institutional bodies adopt shared liability as a general principle, or do they adopt it only in specific sectors? Multi-jurisdictional comparison will clarify the kernel''s scope.',
    'If shared liability is a general reading binding the entire kernel, extractiveness and institutional authority are high and stable across domains. If it is a localized reading, then other readings (developer or deployer liability) remain viable in other sectors, and the total system extractiveness is lower. This affects whether shared liability should be modeled as a single global constraint or as multiple domain-specific constraints linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_containment, conceptual, 'Scope of shared liability as a kernel reading: global or domain-localized?').

omega_variable(
    end_user_opacity_mechanism,
    'Is the measured suppression (0.52) capturing the structural exclusion of end users from liability-allocation decisions, or is suppression concentrated in developer/deployer contract negotiations?',
    'Stakeholder analysis: trace where suppression mechanisms operate. If suppression operates primarily in developer-deployer indemnification clauses (opacity about risk allocation between them), end users are not suppressed but are diffusely harmed. If suppression operates in hiding liability-attribution rules from users (opacity about who is responsible to them), then suppression is internalized at the end-user level.',
    'If suppression is contractual (between developers and deployers), the constraint is primarily a tangled_rope with coordination costs. If suppression is cognitive/structural (end users cannot understand liability allocation and thus cannot demand accountability), the constraint tilts toward snare for the end-user seat. This affects whether end-user identity-locking should be modeled as structural or internalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(end_user_opacity_mechanism, conceptual, 'Locus of suppression: between payers (developers/deployers) or between payers and end users?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__shared_liability, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__shared_liability, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(liab_tr_t0, observed).
narrative_ontology:measurement(liab_tr_t5, liability_attribution__shared_liability, theater_ratio, 5, 0.29).
narrative_ontology:measurement_basis(liab_tr_t5, observed).
narrative_ontology:measurement(liab_tr_t10, liability_attribution__shared_liability, theater_ratio, 10, 0.33).
narrative_ontology:measurement_basis(liab_tr_t10, observed).
narrative_ontology:measurement(liab_tr_t15, liability_attribution__shared_liability, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(liab_tr_t15, observed).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__shared_liability, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(liab_tr_t20, observed).
narrative_ontology:measurement(liab_tr_t25, liability_attribution__shared_liability, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(liab_tr_t25, observed).
narrative_ontology:measurement(liab_tr_t30, liability_attribution__shared_liability, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(liab_tr_t30, observed).
narrative_ontology:measurement(liab_tr_t35, liability_attribution__shared_liability, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(liab_tr_t35, observed).
narrative_ontology:measurement(liab_tr_t40, liability_attribution__shared_liability, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(liab_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__shared_liability, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(liab_be_t0, observed).
narrative_ontology:measurement(liab_be_t5, liability_attribution__shared_liability, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(liab_be_t5, observed).
narrative_ontology:measurement(liab_be_t10, liability_attribution__shared_liability, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(liab_be_t10, observed).
narrative_ontology:measurement(liab_be_t15, liability_attribution__shared_liability, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(liab_be_t15, observed).
narrative_ontology:measurement(liab_be_t20, liability_attribution__shared_liability, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(liab_be_t20, observed).
narrative_ontology:measurement(liab_be_t25, liability_attribution__shared_liability, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(liab_be_t25, observed).
narrative_ontology:measurement(liab_be_t30, liability_attribution__shared_liability, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(liab_be_t30, observed).
narrative_ontology:measurement(liab_be_t35, liability_attribution__shared_liability, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(liab_be_t35, observed).
narrative_ontology:measurement(liab_be_t40, liability_attribution__shared_liability, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(liab_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__shared_liability, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(liab_su_t0, observed).
narrative_ontology:measurement(liab_su_t5, liability_attribution__shared_liability, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(liab_su_t5, observed).
narrative_ontology:measurement(liab_su_t10, liability_attribution__shared_liability, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(liab_su_t10, observed).
narrative_ontology:measurement(liab_su_t15, liability_attribution__shared_liability, suppression_requirement, 15, 0.49).
narrative_ontology:measurement_basis(liab_su_t15, observed).
narrative_ontology:measurement(liab_su_t20, liability_attribution__shared_liability, suppression_requirement, 20, 0.51).
narrative_ontology:measurement_basis(liab_su_t20, observed).
narrative_ontology:measurement(liab_su_t25, liability_attribution__shared_liability, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(liab_su_t25, observed).
narrative_ontology:measurement(liab_su_t30, liability_attribution__shared_liability, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(liab_su_t30, observed).
narrative_ontology:measurement(liab_su_t35, liability_attribution__shared_liability, suppression_requirement, 35, 0.52).
narrative_ontology:measurement_basis(liab_su_t35, observed).
narrative_ontology:measurement(liab_su_t40, liability_attribution__shared_liability, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(liab_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__shared_liability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(liability_attribution__shared_liability, 0.18).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__deployer_liability).

% DUAL FORMULATION NOTE:
% Shared liability is one of three readings of the contested liability_attribution kernel. The sibling readings (developer_liability and deployer_liability) are separate constraint stories in the same family. Shared liability influences both siblings by creating institutional infrastructure (liability assessment standards, insurance markets, contractual precedents) that increases the cost of implementing unilateral rules. However, the readings coexist as live positions: different jurisdictions and sectors adopt different readings, and no single reading forecloses the others within a single unified framework. The family is best modeled as three linked constraints with read_relations capturing the influences between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liability_attribution__shared_liability, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
