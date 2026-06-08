% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency_flat_control, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_proxy_sufficiency_flat_control
 *   human_readable: Catastrophe-Avoidance Competence Maintenance Requirement
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   The catastrophe-avoidance competence maintenance requirement emerged from
 *   legitimate safety concerns following major industrial accidents (Bhopal,
 *   Chernobyl, Deepwater Horizon) where competence decay was identified as a
 *   contributing factor. The constraint mandates that organizations in
 *   high-consequence domains (nuclear power, chemical processing, aviation,
 *   healthcare) actively maintain catastrophe-avoidance skills through
 *   regular drills, simulations, and certification processes. Over time, the
 *   requirement has evolved from substantive practice (realistic scenario
 *   training, cognitive skill development) toward compliance theater
 *   (standardized drills, documentation overhead, box-checking exercises).
 *   The theater_ratio has increased from 0.35 to 0.68 over the measurement
 *   interval as organizations learned to optimize for auditable proxies
 *   rather than actual competence. The constraint exhibits all six DR types
 *   from different perspectives: operational staff experience it as pure
 *   extraction (Snare), safety managers experience mixed coordination and
 *   extraction (Tangled Rope), consulting firms experience pure coordination
 *   benefit (Rope), the HRO research community sees it as temporary
 *   scaffolding toward better methods (Scaffold), the compliance bureaucracy
 *   maintains it as degraded ritual (Piton), and the analytical observer sees
 *   irreducible coordination-extraction hybridity (Tangled Rope). The core
 *   contestation is not whether competence maintenance is necessary (all
 *   parties agree it is) but whether the current proxy-based implementation
 *   actually maintains competence or merely creates auditable compliance
 *   artifacts.
 *
 * KEY AGENTS:
 *   - Operational Staff: Primary victim (powerless/trapped) — bears time cost and cognitive load of mandatory drills and documentation; cannot exit the requirement
 *   - Safety Manager: Mixed position (moderate/constrained) — coordinates genuine competence maintenance but also administers compliance theater; constrained by regulatory mandates
 *   - Safety Consulting Industry: Primary beneficiary (institutional/arbitrage) — captures recurring revenue from drill design, simulation services, and compliance auditing
 *   - Regulatory Authority: Mixed position (institutional/constrained) — solves genuine coordination problem but captured by measurability constraint; mandates auditable proxies over actual competence
 *   - Resource-Constrained Organizations: Secondary victim (moderate/constrained) — compliance overhead crowds out substantive training; face higher relative burden than well-resourced organizations
 *   - HRO Research Community: Organized agents (organized/mobile) — developing better assessment methods; see current regime as temporary scaffolding
 *   - Compliance Bureaucracy: Institutional actor (institutional/arbitrage) — maintains degraded ritual through inertia and liability risk aversion
 *   - Actual Safety Margin: Abstract victim (powerless/trapped) — the collective good of genuine catastrophe-avoidance competence; contaminated by theater substitution effect
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency_flat_control, 0.48).
domain_priors:suppression_score(catastrophe_proxy_sufficiency_flat_control, 0.62).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency_flat_control, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency_flat_control, extractiveness, 0.48).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency_flat_control, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency_flat_control, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency_flat_control, tangled_rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency_flat_control, "Catastrophe-Avoidance Competence Maintenance Requirement").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency_flat_control, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(catastrophe_proxy_sufficiency_flat_control, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency_flat_control, safety_consulting_industry).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency_flat_control, compliance_bureaucracy).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency_flat_control, simulation_vendors).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency_flat_control, operational_staff).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency_flat_control, resource_constrained_organizations).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency_flat_control, actual_safety_margin).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency_flat_control, safety_managers).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency_flat_control, competence_decay_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency_flat_control, active_maintenance_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Front-line workers in high-consequence domains (nuclear operators, chemical plant technicians, flight crews, surgical teams) who must complete mandatory competence maintenance exercises. They bear the direct time cost of drills and documentation, experience the cognitive load of compliance theater, and cannot opt out without losing their positions. From their seat, the exercises often feel disconnected from actual operational challenges — standardized scenarios that don't match the complexity of real incidents, box-checking documentation that consumes time without improving skill.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency_flat_control, operational_staff, payer,
    powerless, biographical, trapped, local).

% Mid-level organizational actors responsible for designing and administering competence maintenance programs. They coordinate genuine safety functions (scheduling realistic scenario training, tracking skill development, maintaining shared mental models across teams) but also administer compliance theater (ensuring drill documentation meets regulatory standards, managing certification paperwork, preparing for audits). Constrained by regulatory mandates and organizational liability concerns — they cannot eliminate the theater without exposing the organization to legal risk, but they also see the theater crowding out substantive training.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency_flat_control, safety_managers, agenda_setter,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency_flat_control, safety_managers, payer).

% Firms that design drill scenarios, provide simulation services, conduct compliance audits, and sell certification programs. They capture recurring revenue from the maintenance requirement — organizations must purchase external expertise to design exercises that meet regulatory standards. From their seat, the requirement is pure coordination: organizations need help maintaining competence, and the consulting industry provides that service. They can exit any specific engagement and shift to other clients, giving them structural mobility.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency_flat_control, safety_consulting_industry, beneficiary,
    institutional, immediate, arbitrage, national).

% Government agencies responsible for preventing catastrophic incidents in high-consequence domains. They mandate competence maintenance requirements, set drill frequency standards, and audit compliance. Constrained by political pressure to prevent disasters and by limited capacity to verify actual competence — they can only mandate what they can audit, which means proxy measures (drill completion rates, certification status) rather than actual adaptive expertise. From their seat, the requirement solves a genuine coordination problem (ensuring organizations don't let skills atrophy) but they are captured by the measurability constraint.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency_flat_control, regulatory_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Smaller organizations or those in less profitable sectors (rural hospitals, municipal utilities, regional chemical processors) that face the same compliance requirements as large well-resourced organizations but with tighter budgets and smaller staff. The compliance overhead is a higher relative burden for them — the same drill frequency and documentation requirements consume a larger fraction of their operational capacity. From their seat, the theater substitution effect is most severe: time spent on compliance documentation is time not spent on patient care, infrastructure maintenance, or process improvement.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency_flat_control, resource_constrained_organizations, payer,
    moderate, biographical, constrained, regional).

% Academic researchers and practitioners studying high-reliability organizations, developing better competence assessment methods (scenario-based evaluation, cognitive task analysis, naturalistic decision-making studies, real-time performance monitoring). They see the current proxy-based maintenance regime as temporary scaffolding — the requirement coordinates attention on competence decay as a real problem, but the specific implementation (standardized drills, compliance documentation) is a placeholder until better methods mature. They can shift research focus if this domain becomes unproductive, giving them exit mobility.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency_flat_control, hro_research_community, observer,
    organized, generational, mobile, global).

% The institutional apparatus that administers and audits competence maintenance requirements — regulatory staff, internal compliance departments, third-party auditors, certification bodies. From their seat, the original function (ensuring catastrophe-avoidance competence) has atrophied into ritual. Everyone knows the quarterly drills are theater, but the theater continues because it is auditable and legally defensible. They maintain the requirement through institutional inertia and liability risk aversion — no one can prove a negative (that competence would persist without the drills) and experimentation is too risky.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency_flat_control, compliance_bureaucracy, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency_flat_control, compliance_bureaucracy, beneficiary).

% The abstract collective good of genuine catastrophe-avoidance competence — the actual ability of high-consequence systems to handle novel failure modes and prevent disasters. This is not an agent but a non-agent entity kept for narrative completeness. It is excluded from the conversation because it cannot advocate for itself and because the theater substitution effect actively harms it (compliance theater crowds out substantive training, reducing actual competence while increasing auditable compliance). From an analytical seat, this is the primary victim of the constraint's degradation.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency_flat_control, actual_safety_margin, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_proxy_sufficiency_flat_control, actual_safety_margin).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement coordinates competence maintenance across high-consequence organizations by creating external pressure to practice catastrophe-avoidance skills. Without the requirement, organizations would under-invest in readiness for rare events (the competence decay problem is real — skills atrophy without practice, and catastrophic scenarios are too infrequent to maintain readiness through operational experience alone).
% TRANSFER_FUNCTION: The arrangement moves time and money from operational staff and resource-constrained organizations (who bear the compliance burden) to the safety consulting industry and compliance bureaucracy (who capture revenue and organizational resources from administering the requirement). It also moves cognitive attention from substantive skill development to compliance theater (the theater substitution effect).
% ABSENT_VOICES: Front-line operators who see the theater clearly but cannot speak without career risk; small organizations that bear disproportionate compliance burden but lack political voice; the actual safety margin (the collective good of genuine competence) which has no advocate. The unanimity around 'competence maintenance is necessary' obscures the contestation over 'what constitutes sufficient maintenance' — dissenting voices on implementation are structurally excluded from the regulatory process.
% DISAPPEARANCE_RATIONALE: If the requirement disappeared overnight, organizations would reduce competence maintenance investment (the coordination problem is real), consulting revenue would collapse, compliance departments would shrink, and regulatory enforcement would shift to other proxies. The world rearranges because multiple parties' arrangements depend on the requirement — it is not a natural fact but a constructed institutional mandate.
% FOUNDING_PROBLEM: The requirement was built to solve the competence decay problem identified in major accident investigations: organizations in high-consequence domains were allowing catastrophe-avoidance skills to atrophy between rare events, contributing to disasters when novel failure modes emerged. The founding problem was genuine — Bhopal, Chernobyl, and Deepwater Horizon all showed competence decay as a contributing factor.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (competence decay) is corroborated by accident investigation boards, HRO researchers, and safety managers — all attest that skills do atrophy without practice. However, the STATUS is contested: HRO researchers argue the problem is still live but the current solution (proxy-based compliance) is increasingly ineffective; compliance bureaucracy argues the problem is solved by the current regime; operational staff argue the problem is live but the current solution actively harms it through theater substitution. The contestation is not over whether competence decays (all parties agree it does) but over whether the current maintenance regime actually maintains competence or merely creates auditable artifacts.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency_flat_control, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPERATIONAL STAFF (SNARE) — Trapped in mandatory drill cycles and compliance documentation that consume operational time without clear connection to actual safety improvement. Cannot exit the requirement; bears the time cost and cognitive load of performative exercises. Experiences maximum extraction — the maintenance theater displaces actual skill development and situational awareness training.
constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SAFETY MANAGER (TANGLED ROPE) — Constrained by regulatory requirements and organizational liability concerns, but also genuinely coordinating competence maintenance across teams. Mixed experience: the requirement creates real coordination value (standardized training, shared mental models) but also extracts through compliance theater (box-checking drills, documentation overhead that crowds out substantive practice). Moderate extraction — some agency to shape implementation but cannot escape the underlying mandate.
constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SAFETY CONSULTING FIRM (ROPE) — Benefits from the maintenance requirement through recurring revenue from drill design, simulation services, and compliance auditing. Experiences the constraint as pure coordination: organizations need expertise to design effective exercises, and the consulting industry provides that service. Net beneficiary — the requirement creates and sustains the market for their services.
constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (TANGLED ROPE) — Constrained by political pressure to prevent catastrophes and by limited capacity to verify actual competence. The maintenance requirement solves a genuine coordination problem (ensuring organizations don't let skills atrophy) but also extracts through mandating proxy measures (drill frequency, documentation standards) that are easier to audit than actual competence. Mixed extraction — the authority both coordinates and is captured by the measurability constraint.
constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HRO RESEARCH COMMUNITY (SCAFFOLD) — Organized researchers developing better competence assessment methods (scenario-based evaluation, cognitive task analysis, naturalistic decision-making studies) see the current proxy-based maintenance regime as temporary. The requirement coordinates attention on competence decay as a real problem, but the specific implementation (standardized drills, compliance documentation) is a placeholder until better assessment methods mature. Sunset logic: as real-time competence monitoring and adaptive training systems develop, the crude proxy measures become obsolete.
constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COMPLIANCE BUREAUCRACY (PITON) — The original function (ensuring catastrophe-avoidance competence) has atrophied into ritual. Quarterly drills and annual certifications persist through institutional inertia, maintained because they are auditable and legally defensible, not because they demonstrably maintain competence. The bureaucracy sees its own process as degraded — everyone knows the drills are theater, but the theater continues because no one can prove a negative (that competence would persist without the drills) and liability risk prevents experimentation.
constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the constraint solves a genuine coordination problem (competence does decay without practice, and organizations do need external pressure to maintain costly readiness for rare events) but also embeds substantial extraction (the proxy measures are gameable, the compliance overhead crowds out substantive training, and the requirement benefits consultants and bureaucrats more than it improves safety). The analytical classification is tangled_rope because both the coordination function and the extraction mechanism are structurally real and irreducible.
constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_proxy_sufficiency_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_proxy_sufficiency_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_proxy_sufficiency_flat_control, TR),
    TR >= 0.70.

:- end_tests(catastrophe_proxy_sufficiency_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The requirement extracts through compliance overhead (time spent on documentation and standardized drills that could be spent on substantive training), through consulting fees (organizations must purchase external expertise to design compliant exercises), and through the theater substitution effect (box-checking crowds out real skill development). The extraction is not maximal because some genuine coordination value persists — the requirement does prevent complete competence atrophy and does create shared mental models across teams. The value has increased from 0.28 to 0.48 over the interval as organizations optimized for proxies and theater displaced substance. Suppression (0.62): Moderate-high. Significant barriers to exit include regulatory mandates (organizations cannot opt out without losing operating licenses), liability risk (failure to comply creates legal exposure even if actual competence is high), and institutional inertia (the compliance infrastructure is embedded in organizational structure). Suppression has increased from 0.45 to 0.62 as the regulatory framework matured and enforcement intensified. Theater ratio (0.68): High. The majority of maintenance activity is now performative: quarterly drills follow standardized scripts that operators can execute without genuine cognitive engagement; annual certifications test memorization of procedures rather than adaptive expertise; documentation requirements consume more time than the exercises themselves. The theater has increased from 0.35 to 0.68 as the Goodhart dynamic took hold — organizations learned that optimizing for auditable compliance is safer (legally) and cheaper (operationally) than maintaining actual competence.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single structural phenomenon — the requirement to actively maintain catastrophe-avoidance competence — produces radically different experiences depending on the observer's position. Operational staff see pure extraction (Snare) because they bear the cost without seeing the benefit (the counterfactual competence decay is invisible to them). Safety managers see mixed coordination and extraction (Tangled Rope) because they experience both the genuine value (standardized training, shared mental models) and the extractive overhead (compliance theater, documentation burden). The consulting industry sees pure coordination (Rope) because they are net beneficiaries — the requirement creates their market. The HRO research community sees temporary scaffolding (Scaffold) because they are developing better methods that will make the current proxies obsolete. The compliance bureaucracy sees degraded ritual (Piton) because the original function has atrophied into theater maintained through inertia. The analytical observer sees irreducible tangled_rope because both the coordination function (competence does decay without practice) and the extraction mechanism (proxy optimization, theater substitution, consulting capture) are structurally real. The perspectival gap is not a disagreement about facts but a consequence of different structural relationships to the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. Operational staff are victims with trapped exit options — they bear the time cost and cognitive load of mandatory exercises without ability to opt out, producing high d and high experienced extraction. Safety managers are mixed — they are both coordinators (benefiting from standardized training frameworks) and victims (constrained by compliance mandates), producing moderate d. The safety consulting industry are pure beneficiaries with arbitrage exit — they capture revenue from the requirement and can exit any specific engagement, producing low d and negative experienced extraction (subsidy). Regulatory authorities are constrained institutional actors — they coordinate genuine safety improvement but are captured by the measurability constraint (can only mandate what they can audit), producing moderate d. Resource-constrained organizations are secondary victims — the compliance overhead is a higher relative burden for them than for well-resourced organizations, producing moderate-high d. The HRO research community are organized agents with mobile exit — they can shift focus to other research questions if this domain becomes unproductive, producing low d. The compliance bureaucracy are institutional beneficiaries with arbitrage exit — they maintain the requirement through inertia and collect organizational resources to administer it, producing low d. The analytical observer sees the irreducible hybridity — genuine coordination function coexisting with substantial extraction — producing moderate d at the analytical context.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the coordination function (preventing competence decay) and the extraction mechanism (compliance theater, consulting capture, theater substitution) are not separable. The requirement genuinely coordinates competence maintenance — without external pressure, organizations would under-invest in readiness for rare catastrophic events. But the coordination necessarily operates through proxy measures (drill completion, certification status) because actual competence is difficult to observe and verify. The proxy measures are gameable, creating the extraction mechanism. The extraction is not a bug that can be fixed while preserving the coordination function — it is a structural consequence of the observability constraint. The analytical classification is tangled_rope because both functions are irreducible. The mandate has not outlived its function (competence decay is still real), but the function cannot be achieved without the extractive overhead (proxy optimization is inevitable given the observability constraint). The scaffold perspective (HRO research community) offers a potential resolution: better competence assessment methods (real-time monitoring, adaptive training systems, naturalistic decision-making evaluation) could reduce the proxy gap and lower the extraction, but these methods are not yet mature enough to replace the current regime.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proxy_validity_threshold,
    'At what point does the gap between proxy measures (drill completion, certification status) and actual competence (ability to handle novel catastrophic scenarios) invalidate the maintenance requirement?',
    'Longitudinal study correlating proxy compliance rates with actual incident outcomes; comparison of organizations with high proxy compliance vs organizations with low proxy compliance but high actual competence (measured through surprise scenario testing)',
    'If proxy validity is high: the requirement is genuine coordination (Rope from more perspectives). If proxy validity is low: the requirement is extraction disguised as safety (Snare from more perspectives). Current evidence suggests validity is moderate and declining as organizations optimize for the proxy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_validity_threshold, empirical, 'Validity threshold for proxy measures of catastrophe-avoidance competence').

omega_variable(
    competence_decay_rate,
    'How quickly does catastrophe-avoidance competence actually decay without active maintenance, and does the decay rate justify the mandated maintenance frequency?',
    'Controlled studies measuring competence degradation over time in the absence of practice; comparison of decay rates across different skill types (procedural vs adaptive) and different operational contexts (high-consequence vs low-consequence domains)',
    'If decay is rapid (weeks to months): frequent maintenance is justified coordination. If decay is slow (years): the requirement is over-specified extraction. Current evidence is mixed and domain-dependent, with procedural skills decaying faster than adaptive expertise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_decay_rate, empirical, 'Actual rate of competence decay without maintenance').

omega_variable(
    theater_substitution_effect,
    'Does compliance theater (box-checking drills, documentation overhead) crowd out substantive competence maintenance (realistic scenario training, cognitive skill development)?',
    'Time-use studies in high-reliability organizations; comparison of competence outcomes in organizations with high theater ratios vs low theater ratios; qualitative analysis of how operators allocate limited training time when faced with competing compliance and skill-development demands',
    'If substitution effect is strong: the requirement actively harms safety by displacing real training with theater. If substitution effect is weak: theater is wasteful but not harmful. Preliminary evidence suggests strong substitution in resource-constrained organizations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_substitution_effect, empirical, 'Whether compliance theater crowds out substantive competence maintenance').

omega_variable(
    liability_shield_function,
    'Is the primary function of the maintenance requirement to improve safety or to provide legal liability protection for organizations and regulators?',
    'Analysis of legal outcomes in catastrophic incidents: do organizations with high proxy compliance receive more favorable treatment regardless of actual competence? Comparison of regulatory enforcement patterns: are violations of maintenance requirements punished more severely than actual competence failures?',
    'If liability shield is primary: the requirement is extraction (Snare) disguised as coordination. If safety improvement is primary: the requirement is genuine coordination (Rope) with some extractive overhead. Current legal patterns suggest liability shield function is substantial.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liability_shield_function, conceptual, 'Whether liability protection or safety improvement is the primary function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency_flat_control, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cat_proxy_theater_t0, catastrophe_proxy_sufficiency_flat_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cat_proxy_theater_t3, catastrophe_proxy_sufficiency_flat_control, theater_ratio, 3, 0.48).
narrative_ontology:measurement(cat_proxy_theater_t6, catastrophe_proxy_sufficiency_flat_control, theater_ratio, 6, 0.58).
narrative_ontology:measurement(cat_proxy_theater_t10, catastrophe_proxy_sufficiency_flat_control, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(cat_proxy_extract_t0, catastrophe_proxy_sufficiency_flat_control, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cat_proxy_extract_t3, catastrophe_proxy_sufficiency_flat_control, base_extractiveness, 3, 0.36).
narrative_ontology:measurement(cat_proxy_extract_t6, catastrophe_proxy_sufficiency_flat_control, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(cat_proxy_extract_t10, catastrophe_proxy_sufficiency_flat_control, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(cat_proxy_suppress_t0, catastrophe_proxy_sufficiency_flat_control, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cat_proxy_suppress_t3, catastrophe_proxy_sufficiency_flat_control, suppression_requirement, 3, 0.52).
narrative_ontology:measurement(cat_proxy_suppress_t6, catastrophe_proxy_sufficiency_flat_control, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(cat_proxy_suppress_t10, catastrophe_proxy_sufficiency_flat_control, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency_flat_control, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This is a flat construction of the catastrophe-avoidance competence maintenance substrate. The contestation over what constitutes sufficient maintenance exercise is captured through perspectival disagreement (operational staff vs safety managers vs consultants vs researchers) and through omega variables (proxy validity, decay rate, theater substitution, liability shield function). No decomposition into readings is performed in this control construction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
