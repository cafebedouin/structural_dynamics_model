% ============================================================================
% CONSTRAINT STORY: integrated_digital_governance_stack
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_integrated_digital_governance_stack, []).

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
 *   constraint_id: integrated_digital_governance_stack
 *   human_readable: The Integrated Digital Governance Stack
 *   domain: technological/political
 *
 * SUMMARY:
 *   The Integrated Digital Governance Stack represents the convergence of
 *   four previously distinct systems: AI surveillance (sensor), digital
 *   credentialing (authentication), social credit scoring (logic), and
 *   central bank digital currencies (execution). When combined, these four
 *   components form a unified mechanism for real-time behavioral control,
 *   financial surveillance, and exclusion from economic participation. The
 *   constraint's power comes from integration — each component alone is
 *   extractive but limited; together, they form a total system with no
 *   material exit. The stack represents the highest extractiveness (0.72) and
 *   suppression (0.78) in the corpus because it combines data vacuum
 *   (surveillance), identity gating (credentialing), behavioral metrics
 *   (social credit), and execution authority (programmable money) into a
 *   single closed loop. The theater ratio (0.65) reflects that the system is
 *   presented as necessary coordination infrastructure (fraud prevention, AML
 *   compliance, tax enforcement, financial stability) while functioning as a
 *   behavioral control mechanism that goes far beyond those stated
 *   justifications.
 *
 * KEY AGENTS:
 *   - Unbanked Individuals: Primary victims (powerless/trapped) — zero alternatives for economic participation; maximum surveillance exposure; complete behavioral control
 *   - Informal Economy: Primary victims (moderate/constrained) — excluded from integrated stack by design; face collapse of alternative economic pathways; high cost of exit
 *   - Central Monetary Authority: Primary beneficiary (powerful/mobile) — controls money supply, interest rates, and who can transact; has full surveillance; mobile exit options (but constrains others' exits)
 *   - State Surveillance Apparatus: Primary beneficiary (institutional/arbitrage) — gains real-time behavioral data and enforcement authority; builds capacity to restrict movement, spending, association
 *   - Credential Issuing Authority: Primary beneficiary (institutional/arbitrage) — controls identity gates; determines who can participate; gathers identity and behavioral data
 *   - Technology Provider: Secondary beneficiary (institutional/arbitrage) — contracts for stack implementation; gains data access and platform lock-in; has jurisdictional exit options
 *   - Parallel Currency Coalition: Organized resistance (organized/constrained) — building exit pathways (cryptocurrency, community currencies, mesh payment); has agency but faces suppression
 *   - Analytical Observer: Civilizational context (analytical/analytical) — sees the stack as either natural coordination or contingent political choice depending on framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(integrated_digital_governance_stack, 0.72).
domain_priors:suppression_score(integrated_digital_governance_stack, 0.78).
domain_priors:theater_ratio(integrated_digital_governance_stack, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(integrated_digital_governance_stack, extractiveness, 0.72).
narrative_ontology:constraint_metric(integrated_digital_governance_stack, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(integrated_digital_governance_stack, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(integrated_digital_governance_stack, snare).
narrative_ontology:human_readable(integrated_digital_governance_stack, "The Integrated Digital Governance Stack").
narrative_ontology:topic_domain(integrated_digital_governance_stack, "technological/political").

domain_priors:requires_active_enforcement(integrated_digital_governance_stack).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(integrated_digital_governance_stack, central_monetary_authority).
narrative_ontology:constraint_beneficiary(integrated_digital_governance_stack, state_surveillance_apparatus).
narrative_ontology:constraint_beneficiary(integrated_digital_governance_stack, credential_issuing_authority).
narrative_ontology:constraint_victim(integrated_digital_governance_stack, individual_economic_autonomy).
narrative_ontology:constraint_victim(integrated_digital_governance_stack, informal_economy).
narrative_ontology:constraint_victim(integrated_digital_governance_stack, financial_privacy).
narrative_ontology:constraint_victim(integrated_digital_governance_stack, exit_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNBANKED INDIVIDUAL (SNARE) — No alternative payment infrastructure; cannot participate in economy without digital credential; complete surveillance exposure; cannot opt out without forfeiting economic participation. Zero exit options. Maximum experienced extraction and suppression. This agent is fully trapped within the system's logic gates.
constraint_indexing:constraint_classification(integrated_digital_governance_stack, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INFORMAL ECONOMY PARTICIPANT (SNARE) — Cash-based informal work becomes economically impossible once digital payment mandates take effect. Can theoretically exit via non-participation but pays maximum cost: exclusion from formal employment, credit, services. Suppression is extremely high because alternative economic pathways are being systematically collapsed. Extraction combines financial data harvest plus behavioral control via spending restrictions.
constraint_indexing:constraint_classification(integrated_digital_governance_stack, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL AUTHORITY (TANGLED ROPE) — Benefits from complete financial surveillance, programmable money (negative interest rates, spending restrictions), and real-time behavioral enforcement. Has genuine coordination function: enables anti-money-laundering, fraud detection, tax compliance. But the coordination benefit is asymmetric extraction — coordination is weaponized against targets. Mobile exit options exist (jurisdictional arbitrage, alternative digital currencies) but are actively restricted. High effective extraction sustained by institutional enforcement.
constraint_indexing:constraint_classification(integrated_digital_governance_stack, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: TECHNOLOGY PROVIDER (ROPE) — Benefits from infrastructure contracts, data access, and platform lock-in. Experiences the stack as coordination mechanism for systems integration. Has arbitrage options: can pivot to alternative jurisdictions, can build parallel infrastructure. High exit capacity reduces experienced extraction. Classification as Rope reflects the provider's genuine optionality and institutional position.
constraint_indexing:constraint_classification(integrated_digital_governance_stack, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY REGULATION (PITON) — Formal financial regulation (KYC, AML, transaction reporting) was designed for analog-era compliance. Digital stack makes these rules fully automatable, but the regulatory framework persists through institutional inertia beyond functional necessity. Theater ratio is high: compliance theater transforms into behavioral control because the same data flows that enforce banking regulations now enable real-time spending restrictions and account freezes. The constraint replaces regulation with direct execution.
constraint_indexing:constraint_classification(integrated_digital_governance_stack, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: PARALLEL CURRENCY COALITION (SCAFFOLD) — Organized groups (cryptocurrency networks, community currencies, mesh-based payment systems) are building exit pathways with explicit sunset logic: as alternative payment infrastructure matures, dependence on the integrated stack declines. Low experienced extraction because these agents have both agency and a visible exit path. Suppression by authorities is high but predictable, creating a temporary coordination failure rather than permanent trap.
constraint_indexing:constraint_classification(integrated_digital_governance_stack, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: NATURALIZING EFFICIENCY (MOUNTAIN - FALSE SUMMIT) — The narrative that digital integration is inevitable because it solves coordination problems (fraud, money laundering, tax evasion) with technological efficiency. This perspective presents the stack as a natural law of economic organization in a digital age. However, the structural data (high suppression, victim populations, lack of beneficiary alternatives, active enforcement) contradicts the mountain classification. The engine will flag this as a false summit — naturalization of a contingent political choice.
constraint_indexing:constraint_classification(integrated_digital_governance_stack, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(integrated_digital_governance_stack_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(integrated_digital_governance_stack, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(integrated_digital_governance_stack, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(integrated_digital_governance_stack, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(integrated_digital_governance_stack, TR),
    TR >= 0.70.

:- end_tests(integrated_digital_governance_stack_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72): Very high. The measurement trajectory (0.35 → 0.55 → 0.72) reflects increasing architectural tightness: early implementations focused on payment coordination (lower extraction); current implementations add behavioral restriction, account-level control, and social credit integration (higher extraction). The system now extracts at three levels simultaneously: (1) financial data harvest via transaction surveillance, (2) behavioral compliance via spending restrictions and account freezes, (3) social control via exclusion from participation. Suppression (0.78): Extremely high. Alternative economic pathways are systematically destroyed: cash is being phased out or tracked, informal economy is being criminalized, alternative currencies face legal restrictions, jurisdictional exit is constrained by international enforcement. Agents cannot exit without forfeiting economic survival. Theater ratio (0.65): Moderately high. The stack is framed as necessary infrastructure for AML/KYC/fraud prevention (legitimate coordination goals) but the architecture enables and enables and enables behavioral control far beyond these stated functions. The same data flows and enforcement mechanisms that justify compliance theater now execute spending restrictions and account freezes that have nothing to do with fraud. As the system matures, theater increases because the performative compliance justification becomes more obviously inadequate to explain the behavioral control capacity actually deployed.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits extreme perspectival divergence. The beneficiary institutions (central authority, surveillance apparatus) can frame the stack as Rope or even coordination necessity because they experience it as enabling their coordination function and have exit options. The powerless agents trapped in the system see Snare because they have no alternatives and experience only extraction and behavioral control. The analytical observer risks seeing a false Mountain — naturalizing the stack as inevitable technological necessity for modern governance — when the structural data reveals it as contingent institutional choice. The organized resistance coalition sees Scaffold because they perceive explicit exit paths (alternative payment systems) with generational sunset logic. The perspectival gap is not about measurement disagreement but about genuine structural asymmetry in how different agents relate to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural relationships to the constraint. The unbanked and informal economy have zero exit options and bear all extraction costs, yielding d ≈ 0.95-1.0. The central authority and surveillance apparatus are full beneficiaries with arbitrage exit options, yielding d ≈ 0.05-0.15. Technology providers have institutional positions and genuine optionality, yielding d ≈ 0.25-0.35. The parallel currency coalition has organized agency and visible exit paths, yielding d ≈ 0.50-0.60. Each perspective's classification follows from applying the sigmoid f(d) to these d values: powerless agents at d=1.0 see Snare (f(d)≈1.42); institutional beneficiaries at d=0.1 see Rope (f(d)≈-0.12); organized resistance at d=0.55 see Scaffold or Tangled Rope. The directionality derivation is structural, not interpretive.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] Reviewed 2026-03-01. Override: false_natural_law.
 *   MANDATROPHY RESOLUTION: The constraint's extractiveness (0.72) requires mandatrophy resolution per the schema. The resolution shows that the Snare classification is not confused with pure coordination (Rope) because the structural data reveals asymmetric extraction: (1) beneficiaries have mobile exit options; (2) victims have zero exit options; (3) the stated coordination function (AML/KYC) requires a fraction of the actual surveillance and behavioral control capacity deployed; (4) theater is high because compliance justification grossly underestimates the behavioral control mechanism. The constraint is Snare from the victims' perspective by necessity — they genuinely have no exit and bear maximum extraction. The constraint is Tangled Rope from the beneficiary perspective because it does perform a genuine coordination function (fraud prevention, tax compliance) alongside asymmetric extraction. The constraint cannot be classified as pure Rope because the asymmetry is not incidental — it is the organizing principle. The mandatrophy is resolved by the perspectival presheaf: each agent's classification is structurally justified by their exit options and directionality, and the convergence of perspectives on Snare (from victims) + Tangled Rope (from beneficiaries) reveals the constraint's true nature: extraction disguised as coordination, with escape routes available only to the powerful.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_of_integration,
    'Is the architectural integration of surveillance, credentialing, social credit, and execution technically necessary, or is it a political choice that could be decomposed into separate systems with different governance?',
    'Comparative institutional analysis: examine jurisdictions that implement subsets of these components separately (e.g., payment systems without behavioral restriction capacity); technical feasibility studies of modular alternatives; cost-benefit analysis with integration-specific costs removed',
    'If necessary: constraint approaches Mountain. If contingent: constraint is Snare by political design, not technological imperative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_of_integration, conceptual, 'Technical necessity vs political choice of architectural integration').

omega_variable(
    coalitional_bypass_capacity,
    'Can organized groups (informal networks, alternative payment systems, jurisdictional arbitrage) sustain meaningful economic activity that bypasses the integrated stack, or does the state''s enforcement capacity close off all material exits?',
    'Longitudinal tracking of parallel currency adoption rates; measurement of transaction volume flowing through non-integrated systems; state capacity to block alternative payment methods; cost analysis of surveillance evasion',
    'If bypass is material: Scaffold perspective confirmed, constraint has structural exit path. If bypass is marginal: constraint is closer to pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalitional_bypass_capacity, empirical, 'Whether organized groups can sustain meaningful economic bypass').

omega_variable(
    behavioral_restriction_legitimacy,
    'What portion of the stack''s extraction comes from legitimate anti-fraud/AML coordination vs pure behavioral control (spending restrictions, account freezes, purchasing surveillance)?',
    'Data comparison: fraction of enforcement actions that target genuine fraud/money laundering vs political/behavioral targeting; analysis of account freezes by jurisdiction and political context; measurement of behavioral restriction scope vs technical necessity',
    'If mostly legitimate: classification shifts toward Tangled Rope. If mostly behavioral control: classification remains Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_restriction_legitimacy, empirical, 'Proportion of extraction devoted to legitimate coordination vs behavioral control').

omega_variable(
    inter_jurisdictional_enforcement_ceiling,
    'Can individuals or groups sustain exit through jurisdictional arbitrage (relocating to jurisdictions outside the integrated stack), or does state enforcement capacity extend beyond borders?',
    'Measurement of capital flight, migration, and informal economy growth in non-integrated jurisdictions; tracking of international payment settlement mechanisms and their exposure to integrated stacks; analysis of digital border enforcement',
    'If arbitrage is viable: effective suppression is reduced, constraint is more Tangled Rope than Snare. If borders are sealed: constraint is closer to pure Snare with global scope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(inter_jurisdictional_enforcement_ceiling, empirical, 'Viability of jurisdictional arbitrage as exit mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(integrated_digital_governance_stack, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(idgs_tr_t0, integrated_digital_governance_stack, theater_ratio, 0, 0.4).
narrative_ontology:measurement(idgs_tr_t5, integrated_digital_governance_stack, theater_ratio, 5, 0.52).
narrative_ontology:measurement(idgs_tr_t10, integrated_digital_governance_stack, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(idgs_be_t0, integrated_digital_governance_stack, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(idgs_be_t5, integrated_digital_governance_stack, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(idgs_be_t10, integrated_digital_governance_stack, base_extractiveness, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(integrated_digital_governance_stack, enforcement_mechanism).
narrative_ontology:affects_constraint(integrated_digital_governance_stack, ai_surveillance_sensor).
narrative_ontology:affects_constraint(integrated_digital_governance_stack, digital_credentialing_authentication).
narrative_ontology:affects_constraint(integrated_digital_governance_stack, social_credit_scoring_logic).
narrative_ontology:affects_constraint(integrated_digital_governance_stack, central_bank_digital_currency_execution).

% DUAL FORMULATION NOTE:
% The Integrated Digital Governance Stack is downstream of and integrates four component constraints. Each component (surveillance, credentialing, social credit, currency) has its own extractiveness and structural properties; the stack represents their architectural convergence. The stack's extractiveness (0.72) exceeds the sum of its components because integration eliminates exit options that would be available if components were separate. This is the core decomposition principle: a unified stack with epsilon=0.72 has fundamentally different structural properties than four separate systems (each with lower epsilon) that could be mixed, matched, or bypassed independently. The network family shows the composition logic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(integrated_digital_governance_stack, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
