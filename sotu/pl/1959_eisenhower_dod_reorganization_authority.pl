% ============================================================================
% CONSTRAINT STORY: 1959_eisenhower_dod_reorganization_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1959_eisenhower_dod_reorganization_authority, []).

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
 *   constraint_id: 1959_eisenhower_dod_reorganization_authority
 *   human_readable: Eisenhower Department of Defense Centralized Authority (1958 Reorganization)
 *   domain: governance/military/organizational_structure
 *
 * SUMMARY:
 *   The 1958 Department of Defense Reorganization Act centralizes command
 *   authority in the Secretary of Defense, consolidating what had previously
 *   been distributed across independent military service branches. This
 *   structural change creates a classic tangled-rope constraint: it solves
 *   the genuine coordination problem of interservice conflict (enabling
 *   unified strategy, faster response, reduced doctrinal competition) while
 *   extracting institutional autonomy from the service branches and reducing
 *   the bureaucratic redundancy that previously provided institutional
 *   resilience. The constraint operates through legal-statutory mechanism
 *   (the reorganization authority), not through market pricing or individual
 *   choice. Service branches transition from quasi-independent institutional
 *   actors to subordinate components of a unified hierarchy. The mechanism
 *   embeds high suppression because the authority is statutory and enforced
 *   through career advancement incentives (compliance with central directives
 *   is necessary for promotion). The theater ratio (0.48) reflects that much
 *   competitive activity persists as ritual rather than structural reality —
 *   services continue to advocate for branch-specific interests, but the
 *   centralized authority has already determined outcomes. The extraction
 *   accumulates over the interval as the initial efficiency gains mature and
 *   are replaced by institutional inertia and potential service-branch
 *   capture of unified strategy.
 *
 * KEY AGENTS:
 *   - Service Branch Commanders: Primary victims (powerless/trapped) — lose institutional autonomy, embedded in statutory hierarchy with no exit options
 *   - Military Service Branches: Secondary victims (organized/constrained) — lose competitive budget leverage and operational independence but gain coordination benefits; can lobby Congress but cannot exit the structure
 *   - Secretary of Defense: Primary beneficiary (institutional/arbitrage) — gains centralized authority to implement unified strategy; low cost to this agent because power flows toward it
 *   - Executive Branch / President: Beneficiary (institutional/arbitrage) — gains capacity to direct unified military force without interservice negotiation; Cabinet-level efficiency benefit
 *   - Congress: Mixed (powerful/mobile) — benefits from standardized military authority for budgeting and oversight; bears extraction through loss of service-branch leverage and distributed veto points
 *   - Interservice Competition Mechanism: Institutional inertia (institutional/arbitrage) — persists as ritual despite structural override; maintains theater through separate academies, doctrine development, recruitment
 *   - Institutional Redundancy / Service Specialization: Potential victim (powerless/trapped) — loss of backup capacity if primary service fails; reduced institutional resilience through integration efficiency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1959_eisenhower_dod_reorganization_authority, 0.52).
domain_priors:suppression_score(1959_eisenhower_dod_reorganization_authority, 0.68).
domain_priors:theater_ratio(1959_eisenhower_dod_reorganization_authority, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1959_eisenhower_dod_reorganization_authority, extractiveness, 0.52).
narrative_ontology:constraint_metric(1959_eisenhower_dod_reorganization_authority, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(1959_eisenhower_dod_reorganization_authority, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1959_eisenhower_dod_reorganization_authority, tangled_rope).
narrative_ontology:human_readable(1959_eisenhower_dod_reorganization_authority, "Eisenhower Department of Defense Centralized Authority (1958 Reorganization)").
narrative_ontology:topic_domain(1959_eisenhower_dod_reorganization_authority, "governance/military/organizational_structure").

domain_priors:requires_active_enforcement(1959_eisenhower_dod_reorganization_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1959_eisenhower_dod_reorganization_authority, executive_branch_unified_command).
narrative_ontology:constraint_beneficiary(1959_eisenhower_dod_reorganization_authority, strategic_coordination_capacity).
narrative_ontology:constraint_victim(1959_eisenhower_dod_reorganization_authority, military_service_branch_autonomy).
narrative_ontology:constraint_victim(1959_eisenhower_dod_reorganization_authority, institutional_redundancy_buffer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SERVICE BRANCH COMMANDER (SNARE) — Constrained by statutory authority transferred to Secretary of Defense. Field commander experiences extraction of decision-making autonomy with minimal coordination benefit; cannot exit the hierarchy. The legislative structure removes alternatives. High suppression: career advancement requires compliance with centralized directives. No escape path — the constraint is embedded in the command structure itself.
constraint_indexing:constraint_classification(1959_eisenhower_dod_reorganization_authority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MILITARY INSTITUTIONAL INTEREST (TANGLED ROPE) — Services benefit from coordinated resource allocation and unified threat assessment across branches (coordination function) while bearing extraction through loss of competitive redundancy and institutional autonomy (asymmetric cost). Services are organized (can lobby Congress, maintain budget coalitions) but constrained by statutory limits on their independent authority. The structure genuinely solves the coordination problem of interservice conflict while extracting unified compliance.
constraint_indexing:constraint_classification(1959_eisenhower_dod_reorganization_authority, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE BRANCH / SECRETARY OF DEFENSE (ROPE) — Benefits from centralized authority to implement unified defense strategy without interservice negotiation friction. Experiences the constraint as pure coordination: the mechanism solves the cabinet-level problem of coordinating military operations under presidential direction. Low extraction cost to this agent because institutional power flows toward it. The SecDef benefits from the authority transfer; sees the constraint as enabling efficient command.
constraint_indexing:constraint_classification(1959_eisenhower_dod_reorganization_authority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONGRESS (OVERSIGHT AUTHORITY) (TANGLED ROPE) — Congress passed the Reorganization Act (coordination function: standardized military authority structure enables legislative budgeting and oversight). Congress also bears extraction through reduced ability to leverage service-branch advocates and constituency-based military contracts (loss of decentralized veto points). Congress is powerful and mobile (can amend legislation) but bound by constitutional constraints and political inertia. Experiences mixed extraction and coordination.
constraint_indexing:constraint_classification(1959_eisenhower_dod_reorganization_authority, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERSERVICE COMPETITION RITUAL (PITON) — The formal mechanisms for service-branch budget advocacy, strategic doctrine development, and operational planning persist despite centralized authority. Services maintain separate academies, recruitment pipelines, and institutional cultures. The ritual of service differentiation persists through inertia even as the structural mechanism for independent action has atrophied. Theater ratio (0.48) reflects that much interservice competitive activity is now performative — the centralized authority constraint has already determined the outcome before the ritual of service advocacy occurs. The structure is maintained because alternatives (full integration, full decentralization) each have political costs.
constraint_indexing:constraint_classification(1959_eisenhower_dod_reorganization_authority, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NECESSITY VIEW (MOUNTAIN) — From a civilizational perspective, centralized command authority appears as an immutable requirement of military effectiveness: any military establishment requires unified hierarchical command to respond to threats. The constraint appears as a natural law of organizational structure — the only way to coordinate lethal force without producing interservice conflict. However, historical data contradicts this: the pre-1958 military operated with distributed service autonomy for decades; the constraint is a legislative choice, not a structural necessity. The analytical observer risks naturalizing what is actually a contingent organizational design.
constraint_indexing:constraint_classification(1959_eisenhower_dod_reorganization_authority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1959_eisenhower_dod_reorganization_authority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1959_eisenhower_dod_reorganization_authority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1959_eisenhower_dod_reorganization_authority, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(1959_eisenhower_dod_reorganization_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(1959_eisenhower_dod_reorganization_authority, TR),
    TR >= 0.70.

:- end_tests(1959_eisenhower_dod_reorganization_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The statute transfers decision-making authority from distributed service branches to centralized executive hierarchy. The beneficiary (SecDef/Executive) captures the authority gain at low cost; the victims (service branches) lose autonomy and institutional independence. The extraction is not maximal (0.72+) because genuine coordination benefits exist — unified strategy and emergency response are real gains, not pure rent capture. The initial extractiveness (0.15) reflects the transition cost; as the structure matures and efficiency gains become routine (rather than novel), the institutional loss becomes salient. The extraction accumulates because services cannot reinvent themselves as independent centers — once authority is transferred, return requires legislative reversal. Suppression (0.68): High. Service branches cannot exit the hierarchy; statutory law enforces centralized authority. Career advancement requires compliance with centralized directives. Institutional cultures and doctrines are now subject to SecDef override. However, suppression is not total (0.90+) because the hierarchy is rule-based, not personalistic — subordinates can challenge decisions through proper channels and Congress retains oversight. Theater ratio (0.48): Moderate. The formal processes of interservice planning, doctrine development, and budget advocacy continue, but their structural function has been attenuated by centralized authority. Services maintain separate identities and institutional programs (academies, recruitment, operational doctrine) even though the final strategic direction is centralized. The theater is neither performative nor functional — it is partially functional (genuine expertise development) and partially performative (ritual interservice competition for resources already allocated by SecDef).
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer risks naturalizing a contingent institutional choice as immutable law. The constraint appears as 'unified command is a natural requirement of military effectiveness' when examined from sufficiently large temporal/spatial scale. However, detailed examination reveals: (1) the pre-1958 military operated effectively with distributed service autonomy for 150+ years; (2) alternative organizational models exist (federated authority, rotating command, consensus mechanisms) with theoretical viability; (3) the choice to centralize is traceable to specific Eisenhower administration decisions and Congressional action, not to immutable structural necessity. The false summit arises from confusing 'organizational principle widely adopted across modern militaries' with 'immutable law of nature.' The former is a contingent institutional arrangement; the latter would resist legislative override.
 *
 * DIRECTIONALITY LOGIC:
 *   The Secretary of Defense has directionality d ≈ 0.10-0.15 (beneficiary with arbitrage options): statutory authority flows toward this agent, options are mobile (can influence interpretation of authority, can recommend further reorganization), low f(d) → negative or minimal χ. Service branch commanders have directionality d ≈ 0.90-0.95 (victims with trapped options): authority flows away, no exit from statutory hierarchy, high f(d) → high χ for this group. Congress has directionality d ≈ 0.45-0.55 (both beneficiary and victim, mobile options): benefits from standardized structure for oversight, bears loss of service-branch leverage points, can amend legislation (mobile), moderate f(d) → moderate χ. The derived directionality produces the perspectival gap: beneficiary sees rope or coordination; victims see snare or extraction. The mix of coordination benefit and extraction loss classifies the constraint as tangled rope — it genuinely solves the problem it claims to solve (unified strategy) while imposing asymmetric costs on service branches (loss of autonomy and institutional independence).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: This constraint avoids mandatrophy by clearly identifying the coordination function (unified strategic planning, faster response, reduced interservice conflict) separate from the extraction mechanism (transfer of decision-making authority from services to SecDef). The coordination is real and substantial — before centralization, interservice budget competition and doctrinal disputes delayed strategic response. The extraction is also real and substantial — service branches lose institutional autonomy and competitive leverage. The constraint is not mislabeled as pure coordination (Rope) because extraction is present; it is not mislabeled as pure extraction (Snare) because coordination is present. The tangled rope classification is appropriate. The mandatrophy is resolved by the perspectival analysis: from the beneficiary's (SecDef's) perspective, it is Rope (pure coordination benefit); from the victim's (service branch) perspective, it is Snare (pure extraction cost); from a balanced institutional perspective, it is Tangled Rope (both functions present, asymmetric distribution). The executive branch and Congress both benefit from the coordination function; the service branches bear the extraction cost. This is not a classification error — it is the structural reality of the constraint. The piton perspective (viewing interservice competition as ritual inertia) captures that the organizational structure now overrides the competitive mechanism that previously provided doctrinal resilience and institutional creativity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unified_command_necessity,
    'Is centralized SecDef authority a structural necessity for military effectiveness or a contingent organizational choice?',
    'Comparative analysis of military effectiveness metrics (response time, operational coordination, strategic outcomes) pre-1958 vs post-1958; examination of alternative organizational models (federated authority, rotating command, consensus mechanisms) and their theoretical vs practical viability',
    'If necessity: mountain classification is correct, and extraction is minimal (inherent coordination cost). If contingent: mountain is false summit, extraction is institutional, and alternatives exist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unified_command_necessity, empirical, 'Whether centralized authority is structural necessity or contingent choice').

omega_variable(
    redundancy_as_buffer,
    'Does the loss of service-branch institutional redundancy increase strategic vulnerability or reduce wasteful duplication?',
    'Historical analysis of decision failures, strategic surprises, and institutional blind spots pre-1958 vs post-1958; examination of alternative explanations (doctrine change, technological shift, threat environment) separate from organizational structure',
    'If redundancy loss increases vulnerability: extraction is severe (institutional resilience cost). If duplication elimination is net benefit: extraction is modest (efficiency gain outweighs autonomy loss).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redundancy_as_buffer, empirical, 'Whether service redundancy loss increases or decreases strategic vulnerability').

omega_variable(
    service_branch_capture,
    'Has centralized authority created conditions for Executive capture by a single service branch''s strategic doctrine?',
    'Analysis of strategic doctrine dominance over time; identification of periods when one service''s doctrine shaped unified strategy; comparison with pre-1958 interservice contestation over doctrine',
    'If capture occurs: structure converts from tangled rope (mixed coordination/extraction) to snare (pure extraction benefiting dominant service). If doctrine diversity persists: tangled rope classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(service_branch_capture, empirical, 'Whether centralized authority enables strategic capture by service doctrine').

omega_variable(
    constitutional_limits_ambiguity,
    'What are the constitutional limits on the President''s and Congress''s authority to structure military command? Can they constitutionally assign centralized authority, or is some service autonomy constitutionally protected?',
    'Legal analysis of War Clause, Commander-in-Chief powers, Declare War power; examination of whether organizational structure is constitutional question (answerable by courts) or political question (answerable by Congress); historical precedent regarding military organization.',
    'If centralized authority is constitutionally required: mountain. If it is constitutionally optional but politically entrenched: false summit. If Congress can constitutionally mandate decentralization but chooses not to: reveals extraction beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_limits_ambiguity, conceptual, 'Constitutional character of military organizational authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1959_eisenhower_dod_reorganization_authority, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dod_cent_tr_t0, 1959_eisenhower_dod_reorganization_authority, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dod_cent_tr_t3, 1959_eisenhower_dod_reorganization_authority, theater_ratio, 3, 0.42).
narrative_ontology:measurement(dod_cent_tr_t6, 1959_eisenhower_dod_reorganization_authority, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(dod_cent_be_t0, 1959_eisenhower_dod_reorganization_authority, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(dod_cent_be_t3, 1959_eisenhower_dod_reorganization_authority, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(dod_cent_be_t6, 1959_eisenhower_dod_reorganization_authority, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1959_eisenhower_dod_reorganization_authority, enforcement_mechanism).
narrative_ontology:affects_constraint(1959_eisenhower_dod_reorganization_authority, interservice_budget_competition).
narrative_ontology:affects_constraint(1959_eisenhower_dod_reorganization_authority, military_doctrine_consolidation).
narrative_ontology:affects_constraint(1959_eisenhower_dod_reorganization_authority, joint_operations_coordination).

% DUAL FORMULATION NOTE:
% This constraint is upstream of service-specific strategic constraints (Army doctrine, Navy doctrine, Air Force doctrine). The centralized authority structure affects how these service doctrines are coordinated into unified strategy. The decomposition separates the organizational authority constraint from the specific doctrinal and operational constraints it structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(1959_eisenhower_dod_reorganization_authority, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
