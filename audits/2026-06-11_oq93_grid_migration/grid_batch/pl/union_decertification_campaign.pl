% ============================================================================
% CONSTRAINT STORY: union_decertification_campaign
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_union_decertification_campaign, []).

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
 *   constraint_id: union_decertification_campaign
 *   human_readable: Coordinated Union Decertification Campaign
 *   domain: labor_relations/organizational_power
 *
 * SUMMARY:
 *   A coordinated union decertification campaign exemplifies how extraction
 *   operates across multiple social levels simultaneously, with
 *   differentiated pressure mechanisms at each level producing a tangled
 *   coordination-extraction hybrid. At the individual level, workers face
 *   intensifying workplace pressure (surveillance, shift reassignments,
 *   selective incentive offers) alongside delegitimizing messaging that
 *   disconnects them from collective identity. At the organizational level,
 *   the union leadership must coordinate defensive resistance while the
 *   decertification infrastructure (management-backed petition drives,
 *   corporate communications, legal proceedings) applies sustained
 *   enforcement pressure. At the class level, a broad delegitimation campaign
 *   ('Union bosses enrich themselves,' 'Unions restrict worker choice')
 *   systematically suppresses the frame in which collective bargaining is
 *   understood as beneficial coordination. At the structural level,
 *   regulatory theater (NLRA procedures, secret ballot voting, disclosure
 *   rules) provides the appearance of worker free choice while systemic
 *   imbalances (manager access to workplace communication channels, unequal
 *   information disclosure, absence of union counter-power during petition
 *   window) structure the actual choice set. The constraint exhibits high
 *   theater ratio because the formal procedures exist to protect worker
 *   voice, yet the actual mechanisms — individual pressure, organizational
 *   warfare, class delegitimation, and structural imbalance — systematically
 *   work around these protections. The coercion grid captures the critical
 *   differentiation: individual-level suppression (0.78 at t18) is
 *   dramatically higher than structural-level suppression (0.72), revealing
 *   that the coercion is mechanistically concentrated on atomized workers
 *   while the system maintains formal legitimacy. Likewise, individual
 *   resistance (0.28 at t18) has collapsed from initial mobilization (0.62),
 *   while organizational and class resistance (0.55, 0.38) persist at
 *   moderate levels, showing that the campaign successfully isolates
 *   individuals from their collective power base. The interval (18 months)
 *   spans typical decertification campaign duration from initial petition
 *   drive through final election and post-decertification contract
 *   negotiation.
 *
 * KEY AGENTS:
 *   - Unionized workers: Primary victims (powerless/trapped) — face individualized pressure, alternative career paths blocked by relocation costs and seniority loss, structurally separated from collective organizing capacity
 *   - Union leadership and local union officers: Organized primary victims (organized/constrained) — coordinate resistance while under institutional attack, defending collective voice while managing defensive posture against management coalition
 *   - Management coalition and decertification campaign operators: Primary beneficiaries (institutional/arbitrage) — capture wage suppression, workplace flexibility, reduced regulatory friction; orchestrate the campaign with high exit optionality
 *   - Class-level delegitimators (management communications, funded 'worker choice' groups): Enforcement agents — systematically suppress the frame in which union representation is understood as beneficial, isolate workers from class consciousness
 *   - Regulatory system and labor boards: Theater operators (institutional/arbitrage) — maintain formal worker protection procedures while systemic imbalances structure actual outcomes
 *   - Labor movement and pro-union constituencies: Organized secondary actors (organized/constrained) — perceive campaign as temporary, mobilize legal/organizing counter-pressure, project sunset toward renewed unionization
 *   - Individual union activists and identity-locked workers: Secondary victims with resistance (powerless to organized/identity_locked) — maintain core resistance despite pressure, carry union identity through campaign
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(union_decertification_campaign, 0.68).
domain_priors:suppression_score(union_decertification_campaign, 0.72).
domain_priors:theater_ratio(union_decertification_campaign, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(union_decertification_campaign, extractiveness, 0.68).
narrative_ontology:constraint_metric(union_decertification_campaign, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(union_decertification_campaign, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(union_decertification_campaign, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(union_decertification_campaign, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(union_decertification_campaign, tangled_rope).
narrative_ontology:human_readable(union_decertification_campaign, "Coordinated Union Decertification Campaign").
narrative_ontology:topic_domain(union_decertification_campaign, "labor_relations/organizational_power").

domain_priors:requires_active_enforcement(union_decertification_campaign).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(union_decertification_campaign, management_coalition).
narrative_ontology:constraint_beneficiary(union_decertification_campaign, decertification_organizers).
narrative_ontology:constraint_victim(union_decertification_campaign, unionized_workforce).
narrative_ontology:constraint_victim(union_decertification_campaign, collective_bargaining_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(union_decertification_campaign, unionized_workers).
narrative_ontology:constraint_victim(union_decertification_campaign, union_leadership).
narrative_ontology:constraint_victim(union_decertification_campaign, union_activists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers employed in union-represented workplace face workplace pressure (shift reassignments following union organizing activity, surveillance of union meetings, timing of incentive offers to union skeptics), career risk (seniority/pension vesting tied to continued employment at current site), and family financial dependence on current wages and health insurance. Exit means relocation (family disruption), career setback (loss of seniority, skill specialization loss), and financial penalty (pension nonvesting, health insurance gap). They bear the cost of decertification through loss of collective voice in wage/benefit negotiation. Individual pressure (shift changes, promotional hints for 'undecided' workers, retention bonuses offered selectively) isolates them from peers also facing pressure, making collective resistance difficult.
narrative_ontology:constraint_stakeholder(union_decertification_campaign, unionized_workers, payer,
    powerless, biographical, trapped, national).

% Union leadership coordinates defensive resistance to decertification campaign (member communication, legal strategy, workplace organizing), but operates under institutional attack. Union officers face delegitimating campaigns ('union bosses enrich themselves,' 'union restricts worker choice'), member attrition as individuals are pressured or convinced to support decertification, and resource depletion (funding relies on membership dues, declining as members leave). Exit from union leadership means acknowledging campaign loss, but staying means mounting increasingly costly defense with dwindling member base. Union represents genuine worker coordination function (collective wage/benefit negotiation, workplace grievance procedures, safety enforcement) but this coordination is under systemic attack.
narrative_ontology:constraint_stakeholder(union_decertification_campaign, union_leadership, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(union_decertification_campaign, union_leadership, payer).

% Management orchestrates coordinated decertification campaign, managing petition drives, workplace communications, incentive structures for workers, and coordination with decertification 'consultants' and legal advisors. Management benefits from successful decertification through potential wage suppression (nonunion workplaces typically pay 15-25% less for comparable work), increased flexibility in scheduling and task assignment, reduced regulatory compliance burden, and weakened worker voice in discipline/termination decisions. Management has high exit optionality: if decertification campaign fails, management can close the facility, relocate production, or negotiate with union. Success in decertification is a positive outcome but not existential.
narrative_ontology:constraint_stakeholder(union_decertification_campaign, management_coalition, agenda_setter,
    institutional, immediate, arbitrage, national).

% Decertification consultants, management-funded 'worker choice' groups, and internal management communications staff operate the campaign infrastructure. They design and execute the messaging strategy (delegitimizing union leadership, emphasizing worker choice framing, associating decertification with benefits), orchestrate the petition drive, manage timing of incentive offers, coordinate workplace communications in way that reaches workers but union has limited access to, and conduct internal polling to identify persuadable workers. Benefit from successful campaign through fees, contract renewal, and reputation-building in the anti-union consulting industry.
narrative_ontology:constraint_stakeholder(union_decertification_campaign, decertification_campaign_operators, agenda_setter,
    institutional, immediate, arbitrage, national).

% The National Labor Relations Board and related labor law framework enforce the formal procedures of decertification (petition filing rules, waiting periods, secret ballot voting, disclosure requirements, unfair labor practice investigation). The regulatory system operates with the mandate to protect worker free choice in union representation, yet the actual operating environment—in which management has vastly superior communication access, concentrated workplace authority, and ability to make credible threats or offers—structures the outcome toward decertification regardless of formal protection. Regulatory theater persists (boards conduct elections, rules are followed) but protection mechanisms are systematically undermined by structural imbalances.
narrative_ontology:constraint_stakeholder(union_decertification_campaign, regulatory_system, observer,
    institutional, civilizational, arbitrage, national).

% Union activists and workers who strongly identify with union membership experience decertification campaign as assault on identity ('I am a union worker,' 'this is my solidarity with my class'). They face intense individual pressure (targeted for shift reassignments, social isolation from non-activist peers, implicit threats), delegitimation campaigns that attack their identity ('union bosses think they're better than you'), and the structural loss of their organizational power base as colleagues are pressured to decertify. For identity-locked activists, exit means not just leaving the workplace but abandoning union identity itself. Many remain engaged in resistance despite severe pressure because identity fission (ceasing to identify as union) is psychologically unavailable.
narrative_ontology:constraint_stakeholder(union_decertification_campaign, union_activists, payer,
    powerless, biographical, identity_locked, national).

% Broader labor movement (union federations, labor advocacy organizations, pro-union legal groups) observes decertification campaigns as a temporary structural challenge being countered through labor law reform organizing and younger-cohort pro-union sentiment. They provide legal support to defending unions, advocacy for first-contract arbitration and card-check legislation, and narrative counter-messaging emphasizing that decertification is not worker choice but management-orchestrated pressure. The labor movement has constrained exit (defending unions and workers is their mandate) but perceives a sunset: legal reforms and demographic shifts toward pro-union sentiment are building structural counter-pressure.
narrative_ontology:constraint_stakeholder(union_decertification_campaign, labor_movement, observer,
    organized, generational, constrained, global).

% Nonunion workers in comparable industries and occupations are structurally excluded from this specific decertification campaign (they are not in the voting unit) but would likely benefit from the policy outcome: successful decertification in unionized workplaces places downward pressure on nonunion wages (as management can point to 'decertified workers got no wage premium'). However, nonunion workers have no voice in the decertification decision itself, and their interests are not represented in campaign discourse.
narrative_ontology:constraint_stakeholder(union_decertification_campaign, nonunion_workers, excluded,
    powerless, biographical, trapped, national).

% Collective bargaining capacity—the abstract good of workers' ability to negotiate collectively over wages, benefits, and conditions—is the victim extracted by successful decertification. This is not an agent but a non-agent entity (a proposition: 'workers can collectively set their terms'). Decertification eliminates this capacity at the site, shifting wage/benefit determination from multi-party negotiation to unilateral management decision-making.
narrative_ontology:constraint_stakeholder(union_decertification_campaign, collective_bargaining_capacity, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(union_decertification_campaign, collective_bargaining_capacity).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(union_decertification_campaign, management_coalition).
narrative_ontology:fixing_cost_class(union_decertification_campaign, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The decertification campaign solves a one-directional coordination problem: how to transition from multi-party (union-management) negotiation of wages/conditions to unilateral management determination. From management's perspective, this is coordinating worker compliance through a combination of atomized incentives (bonuses for non-decertification support, promotions for 'cooperative' workers) and organizational enforcement (surveillance, shift reassignment, delegitimization). From union and worker perspective, this is dissolving a coordination mechanism (collective wage/benefit negotiation) rather than solving a coordination problem.
% TRANSFER_FUNCTION: The campaign transfers collective bargaining power from unionized workers to management. Monetary transfers: potential wage suppression (15-25% in comparable nonunion plants). Time transfers: unilateral schedule control shifts from negotiated schedules to management-controlled scheduling. Status transfers: loss of grievance procedures and workplace justice infrastructure (union arbitration replaced by at-will employment). Voice transfers: individual complaint channels replaced by management-only communication paths.
% ABSENT_VOICES: Nonunion workers in the same industry (who would prefer not to face competitive wage pressure from decertified plants) are excluded from the campaign but affected by its outcome. Future workers who would have accessed union representation are excluded but their interests are affected. Retired workers whose pension security depends on union contracts remaining in place are partially excluded from current decision-making but severely affected if decertification undermines pension funding.
% DISAPPEARANCE_RATIONALE: If the decertification campaign disappeared (union retained certification), the workplace would rearrange: wage negotiation would remain multi-party rather than unilateral, workplace discipline would operate through union grievance procedures, scheduling would require union negotiation, and workers would retain collective voice. The constraint is not incidental to the arrangement; it is constitutive of the transition from union to nonunion operations.
% FOUNDING_PROBLEM: Management sought to solve the constraint of unionized operations: the union raised wage and benefit costs, limited management flexibility in work rules and scheduling, required negotiation over workplace discipline, and reduced management unilateral authority. Decertification was built as the solution to this management problem.
% FOUNDING_PROBLEM_CORROBORATION: Management and decertification consultants explicitly attest that the founding problem (union constraint on management flexibility and costs) remains live and pressing. Union leadership attests that workers continue to need collective voice against monopsony wage pressure (confirming that the coordination problem the union solves remains live). Labor economists document that nonunion workplaces exercise unilateral control over wages and conditions more extensively than unionized workplaces. However, workers themselves are divided: those who support decertification attest that they prefer individual negotiation and 'worker choice,' while union-identified workers attest that collective voice remains necessary. Absent corroboration from outside the beneficiary set: no credible independent voice attests that decertification improves worker outcomes (economic data shows wage losses for decertified workers; worker satisfaction data is self-selected). The founding problem corroboration is asymmetric: management's claim is corroborated by their own actions and economist data on union wage effects; worker welfare claims are uncorroborated and contradicted by wage trajectory data.
narrative_ontology:disappearance_verdict(union_decertification_campaign, world_rearranges).
narrative_ontology:founding_problem_status(union_decertification_campaign, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL WORKER (SNARE) — Faces intensifying organizational pressure (workplace surveillance, threat of plant closure, shift reassignments), individualized incentives (retention bonuses, promotional promises), and social isolation from union solidarity. Exit costs are prohibitive: changing employers means losing seniority, pension vesting, and health insurance coverage. The worker perceives the decertification campaign as coercive extraction masked as individual choice. Maximum extraction at this level.
constraint_indexing:constraint_classification(union_decertification_campaign, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNION LEADERSHIP / CLASS RESISTANCE (TANGLED ROPE) — Organized resistance meets institutional enforcement machinery. The union coordinates worker protection and collective voice, solving a genuine coordination problem (workers need collective power to negotiate wages and conditions). Simultaneously, the constraint imposes asymmetric extraction: the union's legitimacy is under attack through delegitimizing campaigns ('Union bosses enrich themselves,' 'Union restricts worker choice'), and the organization bears the cost of defending itself while individual workers are peeled away through carrots and targeted pressure. Active enforcement includes corporate communications campaigns, decertification petition drives funded by management, and workplace interventions that separate union activists from the general workforce.
constraint_indexing:constraint_classification(union_decertification_campaign, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MANAGEMENT COALITION (ROPE) — Experiences the decertification campaign as pure coordination: organizing decertification, managing the petition process, communicating with workers, and orchestrating workplace transitions. From management's seat, this is a coordination problem solved smoothly — how to transition from union to non-union operations. The constraint functions as coordination from this vantage. Management has high exit optionality (can close the facility, relocate production, or accept unionization elsewhere) and captures the primary benefit: wage suppression, flexibility in work rules, and reduced regulatory friction. This perspective sees the constraint as unambiguously beneficial coordination.
constraint_indexing:constraint_classification(union_decertification_campaign, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR MOVEMENT (SCAFFOLD) — Organized pro-union constituencies (labor federations, worker advocacy NGOs, sympathetic legal organizations) perceive the decertification campaign as a temporary structural challenge with a declared sunset: labor law reform (card check, stronger first-contract arbitration, higher penalties for unfair labor practices) and labor movement resurgence (younger cohorts showing higher unionization interest) are building counter-pressure toward re-legalization of worker organizing. The sunset is contested but real — recent polling shows increased pro-union sentiment among younger workers. This perspective sees decertification as a transitional phenomenon that organizing and legal reform will reverse, rather than an endpoint.
constraint_indexing:constraint_classification(union_decertification_campaign, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY SYSTEM (PITON) — The National Labor Relations Act (NLRA) and its decertification procedures represent a degraded institutional framework. The formal procedures exist to ensure worker free choice (secret ballot, waiting periods, disclosure rules), yet the constraint's actual operation systematizes coercion at the individual level, delegitimation at the class level, and orchestrated pressure at the organizational level. The regulatory theater persists — labor boards conduct elections, enforce posting rules, investigate unfair labor practice charges — but the actual protection mechanisms fail structurally: workers lack genuine freedom when facing individualized pressure, the 'choice' is constrained by asymmetric information, and the union lacks power to defend collective interests during the vulnerable petition-to-election window. The theater is maintained because it legitimizes outcomes, not because it functions. Theater ratio reflects the gap between formal procedure and actual coercion.
constraint_indexing:constraint_classification(union_decertification_campaign, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — A civilizational view might naturalize decertification as an immutable feature of labor markets: workers have heterogeneous preferences, some prefer individual negotiation to collective representation, and decertification reflects this underlying diversity. This framing treats worker choice as natural and deregulation as the path to authentic preference revelation. However, the structural data reveals a false summit: the constraint systematically suppresses alternatives, applies differentiated pressure (carrots for compliers, sticks for organizers), and controls information flows. The mountain classification is a cover story for contingent institutional arrangements, not a recognition of natural law.
constraint_indexing:constraint_classification(union_decertification_campaign, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(union_decertification_campaign_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(union_decertification_campaign, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(union_decertification_campaign, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(union_decertification_campaign, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(union_decertification_campaign, TR),
    TR >= 0.70.

:- end_tests(union_decertification_campaign_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High-moderate. The campaign extracts significant benefits for management (wage suppression potential, flexibility, reduced unionization risk) while imposing substantial costs on workers (loss of collective voice, wage/benefit trajectory risk, identity disruption for union-identified workers). However, extractiveness is not as high as pure snare (0.85+) because a genuine coordination problem exists that the union solves (workers do need collective power) and because individual workers retain some agency through incentive structures and information access (imperfect though it is). The increasing trajectory in measurements (0.45 to 0.72) reflects mounting campaign intensity and tightening individual pressure as the decertification window narrows. Suppression (0.72): Moderate-high. Multiple suppression mechanisms operate: external barriers (career costs of exit, seniority/pension loss, health insurance loss), organizational pressure (shift reassignments, surveillance, social isolation), and internalized mechanisms (delegitimation narrative acceptance, identity dissonance for workers cross-pressured between union identity and management appeals). Suppression increases over the interval as campaign pressure intensifies and union resources for counter-messaging deplete. Theater ratio (0.58): Moderate-high. NLRA procedures (petition filing, waiting periods, secret ballot voting, posting rules) provide the surface of worker free choice, yet actual worker choice is structured by massive information asymmetry (management controls workplace communication, union has limited access), organizational pressure (implicit/explicit threats), and temporal pressure (compressed decision window during campaign). The theater increases over time as the campaign invests in legitimizing the vote as 'real choice' while suppression mechanisms tighten. Accessibility collapse (0.65 base): Individual-level alternatives collapse most severely (0.78 at t18): staying in unionized workplace means accepting loss of status/identity, decertifying means losing collective voice, leaving means seniority/pension loss and relocation costs. Organizational alternatives are more accessible to union (0.62 at t18) because re-mobilization and legal contestation remain possible. Class-level alternatives collapse moderately (0.68) as delegitimation narrative preempts the 'unionization is worker protection' frame. Structural alternatives (0.70) are somewhat accessible because labor law reform and broader labor movement resurgence represent counter-pressure. The spread across levels is the diagnostic signature of this constraint: individual workers are systematically more trapped than the organizational and class levels.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximum perspectival gap across all six types. Management sees pure coordination (Rope): solving the legitimate problem of workplace communication and work-rule clarity through decertification. The open-science scaffold would be naïve here, but the labor movement does see a real (contested but real) sunset toward legal labor law reform and demographic shifts favoring unionization. Union leadership sees tangled hybrid (Tangled Rope): genuine coordination problem solved (collective worker voice) alongside asymmetric extraction (union organizational capacity under attack, individuals peeled away). Individual workers see snare (Snare): coercion masked as choice, alternatives foreclosed, resistance delegitimized, exit costs prohibitive. The regulatory system sees piton (Piton): formal procedures that once meant something (secret ballot protection, fair-play requirements) now mostly perform legitimacy while systemic imbalances structure actual outcomes. The civilizational analytical observer risks seeing mountain (Mountain): worker heterogeneity in unionization preferences, individual choice over collective assignment, natural market evolution toward at-will employment. This is a false summit — naturalizing what is actually a constructed asymmetry (management investment in decertification infrastructure, class-level delegitimation campaign, individual-level pressure orchestration). The gap between snare (worker experience) and rope (management experience) is the diagnostic signal that this is extraction, not coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) determines how much of the measured extractiveness each agent experiences as 'chi' — effective extraction. Management and decertification organizers are full beneficiaries: d ≈ 0.05 (institutional power, arbitrage exit options, explicitly benefiting from union loss). The derivation chain produces low d → negative f(d) → subsidy rather than extraction: they experience the constraint as beneficial coordination. Union leadership: d ≈ 0.65 (organized power but constrained exit, mixed beneficiary/victim status — union is attacked but also defends worker interests). This mixed position produces moderate d → moderate f(d) → moderate experienced extraction. Individual workers: d ≈ 0.82 (powerless, trapped exit, victims of extraction). High d → high f(d) → high experienced extraction. The gap in d values (0.05 for management vs 0.82 for workers) is the structural source of the perspectival gap in classifications. Overrides are not needed here — the beneficiary/victim declarations automatically drive the correct directionality differentiation.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question is: was decertification built to solve a genuine worker-beneficial coordination problem (collective voice provision), or is it extractive rent-seeking dressed in coordination language? The data cuts both ways. Genuine coordination: workers do need collective bargaining power to negotiate against employer monopsony power. Without union, individual workers have weak voice. Union solves this. However, the campaign's structure — individual pressure, class delegitimation, organizational attack, regulatory theater — reveals that what is being extracted is not a coordination problem but a power imbalance. The union's real function (collective worker voice) is eliminated, but no alternative coordination mechanism is offered. Non-union workplaces typically feature high asymmetry in workplace discipline, wage-setting, and schedule control. The decertification campaign does not solve a coordination problem; it eliminates a coordination mechanism. Therefore, the tangled_rope classification is justified: there IS a genuine coordination function being attacked (union power provision), but the campaign's structure is pure extraction (asymmetric pressure, delegitimation, atomization). The rope perspective (management's view) misses that the 'coordination' they see is one-directional (management coordinating workers into submission) rather than mutual coordination. The snare perspective (worker's view) captures that the primary outcome is extraction, not coordination. Mandatrophy is NOT resolved here — the constraint could be challenged as misnamed 'decertification' (free choice) when it is actually 'decapitation' (elimination of worker power). This misframing is the core extract-disguised-as-coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_mechanism_perception_gap,
    'Do workers genuinely experience the decertification campaign as coercive extraction, or do they experience it as legitimate individual choice?',
    'Post-decertification surveys of workers who voted to decertify, comparing stated reasons (independent choice vs. response to pressure) with contemporaneous workplace observation data (timing of incentive offers, correlation between pressure intensity and vote likelihood, persistence of stated preferences in exit interviews or later regret).',
    'If coercive: classification as snare/tangled_rope confirmed, suppression metric validated. If perceived as choice: either suppression metric is overstated or the perception gap itself is the mechanism (workers internalize management framing). If mixed: identifies the identity_locked subset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_mechanism_perception_gap, empirical, 'Whether workers perceive coercion or perceive themselves as choosing').

omega_variable(
    individual_pressure_mechanisms,
    'What specific pressure mechanisms are applied to individual workers, and how widely are they deployed?',
    'Anonymous worker surveys about: shift reassignments after union activity, surveillance of union organizing, differential incentive offers (bonuses, promotions, benefits offered to ''undecided'' workers but not vocal union supporters), informal threats or hints about plant closure, and social isolation tactics (separating union activists from general workforce). Correlation analysis: workers exposed to multiple pressure mechanisms vs. single exposure vs. none.',
    'If pressure is sparse and inconsistent: suppression metric overstated, constraint is better classified as rope (coordination with side payments). If pressure is dense and systematic: suppression is validated, snare classification for individual level confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individual_pressure_mechanisms, empirical, 'Scope and intensity of individual-level pressure mechanisms').

omega_variable(
    collective_bargaining_capacity_counterfactual,
    'What is the counterfactual outcome if the union retains certification: do workers retain substantive collective voice in wage and condition setting, or is collective bargaining theater with management setting de facto terms?',
    'Comparison of certified vs. decertified plants: wage trajectories 3-5 years post-decertification vs. continued certification, controlling for industry/market conditions. Collective bargaining outcome analysis: average wage gains in first post-decertification contract vs. union contracts in comparable plants. Worker grievance redressal: formal process differences and resolution rates.',
    'If decertification results in measurable wage/benefit losses: union provided substantive coordination and workers lost it, validating the tangled_rope beneficiary claim (union solved coordination problem). If no measurable difference: union was purely extractive or wage suppression is structural, reframing the constraint as snare from both union and worker perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_bargaining_capacity_counterfactual, empirical, 'Actual collective bargaining value vs. threat-point alternatives').

omega_variable(
    delegitimation_campaign_effectiveness,
    'How effective is the class-level delegitimation campaign (''union bosses enrich themselves,'' ''union restricts worker choice'') in shifting worker opinions about union legitimacy?',
    'Time-series worker opinion surveys pre-campaign and during campaign phases, measuring: union favorability, perceived union corruption/self-enrichment, perceived worker choice restriction. Correlation with campaign messaging intensity and media exposure.',
    'If delegitimation is highly effective: class-level suppression is severe, workers internalize the narrative, identity_locked subset emerges. If delegitimation has weak effect: class-level suppression is lower than authored (0.72), resistance metric might be understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegitimation_campaign_effectiveness, empirical, 'Effectiveness of union delegitimation messaging on worker opinion').

omega_variable(
    identity_locked_mechanism,
    'For workers who identify strongly as union members or labor advocates, does decertification result in identity-constituted resistance or identity-based exit (joining management-aligned ''worker groups'')?',
    'Qualitative interviews with union activists and vocal union members post-decertification, identifying: whether they experience decertification as assault on identity (''I am a union worker'') vs. external coercion, and whether they remain engaged in resistance or withdraw. Measurement of reengagement in new union organizing campaigns.',
    'If identity-locked: a substantial subset of workers cannot exit despite pressure because their identity is fused with unionism. This subset shows persistent resistance regardless of material incentives. If identity is fungible: workers shift identity affiliation toward management (internalize ''we are a team without union division''), delegitimation campaign works by reframing identity, not by coercive pressure alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_mechanism, empirical, 'Identity fusion and identity-locked resistance in union decertification').

omega_variable(
    suppression_internalization_vs_structural,
    'Is the suppression measured here structural (external barriers to exit, organizational pressure mechanisms) or internalized (workers believe they deserve the pressure, blame themselves, accept delegitimation narrative)?',
    'Post-decertification worker interviews measuring locus of causality (external coercion vs. internal fault) and persistence of suppression after exit (if a worker leaves the workplace, do they continue to feel suppressed, or does suppression lift?). Comparison of workers who initially resisted decertification vs. those who complied.',
    'If mainly structural: suppression should decline after workers exit the workplace, persistence would indicate internalization. If mainly internalized: suppression persists as the worker carries delegitimation narrative with them. Mixed: reveals the two-layer suppression mechanism (external pressure + internal capture).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_vs_structural, empirical, 'Structural vs. internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(union_decertification_campaign, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udec_tr_t0, union_decertification_campaign, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(udec_tr_t0, observed).
narrative_ontology:measurement(udec_tr_t6, union_decertification_campaign, theater_ratio, 6, 0.48).
narrative_ontology:measurement_basis(udec_tr_t6, observed).
narrative_ontology:measurement(udec_tr_t12, union_decertification_campaign, theater_ratio, 12, 0.58).
narrative_ontology:measurement_basis(udec_tr_t12, observed).
narrative_ontology:measurement(udec_tr_t18, union_decertification_campaign, theater_ratio, 18, 0.62).
narrative_ontology:measurement_basis(udec_tr_t18, observed).

% Extraction over time
narrative_ontology:measurement(udec_be_t0, union_decertification_campaign, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(udec_be_t0, observed).
narrative_ontology:measurement(udec_be_t6, union_decertification_campaign, base_extractiveness, 6, 0.58).
narrative_ontology:measurement_basis(udec_be_t6, observed).
narrative_ontology:measurement(udec_be_t12, union_decertification_campaign, base_extractiveness, 12, 0.68).
narrative_ontology:measurement_basis(udec_be_t12, observed).
narrative_ontology:measurement(udec_be_t18, union_decertification_campaign, base_extractiveness, 18, 0.72).
narrative_ontology:measurement_basis(udec_be_t18, observed).

% Suppression requirement over time
narrative_ontology:measurement(udec_su_t0, union_decertification_campaign, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(udec_su_t0, observed).
narrative_ontology:measurement(udec_su_t6, union_decertification_campaign, suppression_requirement, 6, 0.65).
narrative_ontology:measurement_basis(udec_su_t6, observed).
narrative_ontology:measurement(udec_su_t12, union_decertification_campaign, suppression_requirement, 12, 0.72).
narrative_ontology:measurement_basis(udec_su_t12, observed).
narrative_ontology:measurement(udec_su_t18, union_decertification_campaign, suppression_requirement, 18, 0.75).
narrative_ontology:measurement_basis(udec_su_t18, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=18
narrative_ontology:measurement(udec_grid_01, union_decertification_campaign, accessibility_collapse(class), 0, 0.5).
narrative_ontology:measurement(udec_grid_02, union_decertification_campaign, accessibility_collapse(class), 18, 0.68).
narrative_ontology:measurement(udec_grid_03, union_decertification_campaign, accessibility_collapse(individual), 0, 0.55).
narrative_ontology:measurement(udec_grid_04, union_decertification_campaign, accessibility_collapse(individual), 18, 0.78).
narrative_ontology:measurement(udec_grid_05, union_decertification_campaign, accessibility_collapse(organizational), 0, 0.45).
narrative_ontology:measurement(udec_grid_06, union_decertification_campaign, accessibility_collapse(organizational), 18, 0.62).
narrative_ontology:measurement(udec_grid_07, union_decertification_campaign, accessibility_collapse(structural), 0, 0.6).
narrative_ontology:measurement(udec_grid_08, union_decertification_campaign, accessibility_collapse(structural), 18, 0.7).
narrative_ontology:measurement(udec_grid_09, union_decertification_campaign, resistance(class), 0, 0.68).
narrative_ontology:measurement(udec_grid_10, union_decertification_campaign, resistance(class), 18, 0.38).
narrative_ontology:measurement(udec_grid_11, union_decertification_campaign, resistance(individual), 0, 0.62).
narrative_ontology:measurement(udec_grid_12, union_decertification_campaign, resistance(individual), 18, 0.28).
narrative_ontology:measurement(udec_grid_13, union_decertification_campaign, resistance(organizational), 0, 0.72).
narrative_ontology:measurement(udec_grid_14, union_decertification_campaign, resistance(organizational), 18, 0.55).
narrative_ontology:measurement(udec_grid_15, union_decertification_campaign, resistance(structural), 0, 0.42).
narrative_ontology:measurement(udec_grid_16, union_decertification_campaign, resistance(structural), 18, 0.35).
narrative_ontology:measurement(udec_grid_17, union_decertification_campaign, stakes_inflation(class), 0, 0.48).
narrative_ontology:measurement(udec_grid_18, union_decertification_campaign, stakes_inflation(class), 18, 0.64).
narrative_ontology:measurement(udec_grid_19, union_decertification_campaign, stakes_inflation(individual), 0, 0.4).
narrative_ontology:measurement(udec_grid_20, union_decertification_campaign, stakes_inflation(individual), 18, 0.82).
narrative_ontology:measurement(udec_grid_21, union_decertification_campaign, stakes_inflation(organizational), 0, 0.55).
narrative_ontology:measurement(udec_grid_22, union_decertification_campaign, stakes_inflation(organizational), 18, 0.76).
narrative_ontology:measurement(udec_grid_23, union_decertification_campaign, stakes_inflation(structural), 0, 0.62).
narrative_ontology:measurement(udec_grid_24, union_decertification_campaign, stakes_inflation(structural), 18, 0.75).
narrative_ontology:measurement(udec_grid_25, union_decertification_campaign, suppression(class), 0, 0.45).
narrative_ontology:measurement(udec_grid_26, union_decertification_campaign, suppression(class), 18, 0.68).
narrative_ontology:measurement(udec_grid_27, union_decertification_campaign, suppression(individual), 0, 0.38).
narrative_ontology:measurement(udec_grid_28, union_decertification_campaign, suppression(individual), 18, 0.78).
narrative_ontology:measurement(udec_grid_29, union_decertification_campaign, suppression(organizational), 0, 0.52).
narrative_ontology:measurement(udec_grid_30, union_decertification_campaign, suppression(organizational), 18, 0.72).
narrative_ontology:measurement(udec_grid_31, union_decertification_campaign, suppression(structural), 0, 0.58).
narrative_ontology:measurement(udec_grid_32, union_decertification_campaign, suppression(structural), 18, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(union_decertification_campaign, enforcement_mechanism).
narrative_ontology:affects_constraint(union_decertification_campaign, workplace_wage_suppression).
narrative_ontology:affects_constraint(union_decertification_campaign, labor_regulatory_capture).
narrative_ontology:affects_constraint(union_decertification_campaign, workplace_organizing_repression).

% DUAL FORMULATION NOTE:
% The decertification campaign is downstream of broader labor regulatory capture (management influence over NLRA interpretation and enforcement) and upstream of workplace wage suppression (wage trajectories post-decertification). Each story in the family has its own extractiveness: labor regulatory capture is the structural condition enabling coordinated decertification campaigns; decertification campaigns are the tactical mechanism; wage suppression is the outcome. All three are linked by affects_constraints: regulatory capture enables campaigns, campaigns enable suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(union_decertification_campaign, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
