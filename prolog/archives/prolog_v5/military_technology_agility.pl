% ============================================================================
% CONSTRAINT STORY: military_technology_agility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_military_technology_agility, []).

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
 *   constraint_id: military_technology_agility
 *   human_readable: Military Technology Agility Constraint
 *   domain: defense/strategic_capability/institutional_structure
 *
 * SUMMARY:
 *   The military technology agility constraint describes the structural
 *   friction between the speed at which operational environments change and
 *   the speed at which institutional procurement systems can field new
 *   capabilities. Modern peer adversaries (China, Russia) operate with
 *   shorter innovation-to-deployment cycles than traditional U.S. military
 *   acquisition allows, creating asymmetric technological vulnerability. The
 *   constraint manifests as a bureaucratic extraction mechanism embedded in
 *   institutional risk management, procurement oversight, security
 *   certification, and budget planning cycles. It exhibits all six DR types
 *   depending on observer position: field commanders experience it as a snare
 *   (no exit, operational obsolescence); defense contractors experience
 *   coordination and extraction (long cycles protect from competition but
 *   impose carrying costs); the procurement bureaucracy benefits (budget
 *   stability, institutional authority); emerging startups see a snare
 *   (barred by regulatory friction); the strategic planning system is a piton
 *   (multi-year FYDP planning is theater); and organized reform coalitions
 *   (DARPA, DIU) see a scaffold with sunset mechanisms (rapid acquisition
 *   authorities and streamlined OTA processes). Extractiveness has increased
 *   over the 15-year measurement interval (0.35 → 0.62) as the pace of
 *   technological change has accelerated beyond procurement system response
 *   time, while theater_ratio has remained moderate (0.38 → 0.52), indicating
 *   the constraint retains genuine coordination functions alongside
 *   extraction.
 *
 * KEY AGENTS:
 *   - Field Commanders: Primary victims (powerless/trapped) — operational units in theater face multi-year timelines; no control over acquisition decisions; bear obsolescence cost
 *   - Procurement Bureaucracy: Primary beneficiary (institutional/arbitrage) — controls approval processes; benefits from budget predictability and institutional authority; sees constraint as legitimate risk management
 *   - Incumbent Defense Contractors: Secondary beneficiary (powerful/mobile) — protected from competition by high barriers to entry; benefit from long contract timelines; also bear carrying costs of idle capacity
 *   - Emerging Defense Startups: Secondary victim (moderate/constrained) — superior technology barred by security clearance delays and compliance requirements; high cost of market participation; trapped by regulatory friction
 *   - Strategic Planning System: Institutional actor (institutional/constrained) — maintains FYDP theater; sees own process as degraded but institutionally persistent
 *   - Defense Innovation Coalition: Organized actors (organized/mobile) — DARPA, DIU, SpaceForce rapid acquisition; building alternative pathways with shortened timelines
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(military_technology_agility, 0.58).
domain_priors:suppression_score(military_technology_agility, 0.62).
domain_priors:theater_ratio(military_technology_agility, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(military_technology_agility, extractiveness, 0.58).
narrative_ontology:constraint_metric(military_technology_agility, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(military_technology_agility, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(military_technology_agility, tangled_rope).
narrative_ontology:human_readable(military_technology_agility, "Military Technology Agility Constraint").
narrative_ontology:topic_domain(military_technology_agility, "defense/strategic_capability/institutional_structure").

domain_priors:requires_active_enforcement(military_technology_agility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(military_technology_agility, procurement_bureaucracy).
narrative_ontology:constraint_beneficiary(military_technology_agility, incumbent_defense_contractors).
narrative_ontology:constraint_beneficiary(military_technology_agility, military_leadership_hierarchy).
narrative_ontology:constraint_victim(military_technology_agility, operational_effectiveness).
narrative_ontology:constraint_victim(military_technology_agility, field_commanders).
narrative_ontology:constraint_victim(military_technology_agility, rapid_innovation_teams).
narrative_ontology:constraint_victim(military_technology_agility, emerging_defense_startups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIELD COMMANDER (SNARE) — Operational units in theater face multi-year procurement cycles while adversaries iterate rapidly. Trapped by acquisition timelines and authorization chains; cannot adapt doctrine or equipment faster than bureaucratic approval permits. Bears full cost of technological obsolescence with no exit option.
constraint_indexing:constraint_classification(military_technology_agility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INCUMBENT DEFENSE CONTRACTOR (TANGLED ROPE) — Benefits from long procurement cycles (protection from competition) and established supply chain advantages (coordination value). Simultaneously bears extraction: must maintain specialized workforce capacity for years between contract wins, invest in lengthy compliance certification, and absorb R&D costs upfront with uncertain return. Mixed extraction and genuine coordination function.
constraint_indexing:constraint_classification(military_technology_agility, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: PROCUREMENT BUREAUCRACY (ROPE) — Benefits from lengthy approval cycles (budget stability, predictable staffing, institutional continuity). Experiences the constraint as coordination: manages risk by enforcing oversight, documentation, and testing phases. Net beneficiary — the constraint allocates resources and authority toward this institutional actor.
constraint_indexing:constraint_classification(military_technology_agility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EMERGING DEFENSE STARTUP (SNARE) — Barred from rapid deployment despite superior capability by security clearance delays, compliance requirements, and established contractor preference in RFP processes. High cost to participate in military market; trapped by regulatory complexity even if technology is superior. Suppression via certification and preferential access rather than overt prohibition.
constraint_indexing:constraint_classification(military_technology_agility, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: STRATEGIC PLANNING SYSTEM (PITON) — The multi-year acquisition planning cycle (FYDP, Future Years Defense Program) was designed for force structure stability but now functions largely as theater: planning horizons are outdated before they execute, yet the ritual persists through inertia. High performance of planning document production (theater_ratio ~0.65) with degraded actual strategic foresight. Maintained because alternatives haven't fully replaced it.
constraint_indexing:constraint_classification(military_technology_agility, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DEFENSE INNOVATION REFORM COALITION (SCAFFOLD) — Organized actors (DARPA, Defense Innovation Unit, SpaceForce rapid acquisition authorities) see the bottleneck as a temporary institutional failure with sunset mechanisms: rapid prototyping cycles (OTA authority), streamlined security clearances, and agile software development practices are building parallel acquisition pathways that bypass traditional FYDP timelines. Low effective extraction because organized agents see agency and exit path toward compressed timelines.
constraint_indexing:constraint_classification(military_technology_agility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the constraint exhibits both genuine coordination (risk management, interoperability standards, supply chain coherence) and extractive dynamics (protecting incumbent market position, suppressing disruptive entrants, enforcing approval hierarchies that prioritize bureaucratic stability over operational need). The tension is structural: some procurement friction is legitimate; the degree embedded in current practice is excessive.
constraint_indexing:constraint_classification(military_technology_agility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(military_technology_agility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(military_technology_agility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(military_technology_agility, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(military_technology_agility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(military_technology_agility, TR),
    TR >= 0.70.

:- end_tests(military_technology_agility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extracts through two mechanisms: (1) market protection for incumbents (high barriers suppress competition), and (2) operational obsolescence (field units cannot adapt faster than bureaucracy permits). Initial extractiveness (0.35) reflects that early-2010s geopolitical context had less aggressive peer competition; rising to 0.62 reflects acceleration of adversary innovation cycles outpacing U.S. institutional response. The trend is asymmetric: U.S. adversaries have not improved their agility; U.S. institutions have failed to keep pace. Suppression (0.62): High. Barriers to rapid innovation deployment include: security clearance timelines (6-24 months), IP/classification restrictions limiting startup access, compliance certification requirements (18-36 months), acquisition regulation complexity, and congressional budget oversight cycles. These are real but not absolute — DARPA/DIU bypass some via OTA authority. Theater ratio (0.48): Moderate. The FYDP planning process is substantially performative — multi-year plans are obsolete before execution — but planning documents do inform resource allocation and interoperability coordination. Less theater than traditional review processes (0.48 vs 0.70+) because some actual capability decisions flow from the planning cycle. Measurement trajectory shows rising extractiveness and theater as the pace of change has accelerated, suggesting the constraint is increasingly extractive relative to its coordination function.
 *
 * PERSPECTIVAL GAP:
 *   The field commander and emerging startup see snares (no viable exit). The procurement bureaucracy sees ropes (legitimate coordination and authority allocation). The defense contractor sees tangled rope (mixed protection and carrying cost). The strategic planning system is a piton (theater without function). The reform coalition sees a scaffold (temporary, solvable via rapid acquisition authorities). The analytical observer sees tangled rope (both genuine coordination and extractive inefficiency coexist). The perspectival gap is wide because the constraint has genuinely moved over time: in 2010 when peer competition was less acute, the same procurement friction was more defensible as risk management; in 2025 with peer agility accelerating, the same friction is increasingly extractive. The false summit would be naturalizing this as an immutable law of military organizations — the constraint is contingent on institutional choices about acceptable risk, not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position: (1) Field commanders are victims with no exit (trapped) → high d → high f(d) → they experience high χ. (2) Procurement bureaucracy are beneficiaries with discretionary authority (arbitrage) → low d → negative f(d) → they experience negative χ (net benefit). (3) Defense contractors are beneficiaries with some exit (mobile) → low d → moderate f(d) → they experience low-to-moderate χ (net benefit despite carrying costs). (4) Emerging startups are victims with constrained exit (high barriers but technically feasible) → moderate-high d → moderate f(d) → they experience moderate χ. (5) The analytical observer computes d from victim/beneficiary composition across all agents, weighted by scope (national scope σ=1.0) → derives d ≈ 0.60 (mixed, leaning toward victim) → moderate-high f(d) → moderate χ. The tangled rope classification depends on the presence of both coordination (genuine interoperability, risk management) and asymmetric extraction (market protection, operational constraint). Both are present in the structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by exhibiting genuine temporal dynamics: the constraint's type changed over the measurement interval. In 2010 (t=0, ε=0.35), the constraint was closer to Rope — the extraction existed but was justified by lower-pace technological change and less aggressive peer competition. By 2025 (t=15, ε=0.62), the constraint has become Tangled Rope because the extraction is now unjustified by the coordination benefit it provides. The same institutional structures (FYDP, security clearances, compliance requirements) were defensible when adversary innovation cycles were similarly slow; they become extractive when adversaries accelerate. This is not about mislabeling coordination as extraction (the classic mandatrophy risk), but about recognizing that the ratio of coordination to extraction shifted over time. The defense innovation reform coalition's scaffold perspective shows that the constraint is not immutable — alternative acquisition authorities have already proven faster cycles are possible and compatible with security/interoperability requirements. Therefore, the current FYDP constraint cannot hide behind 'this is necessary for safe military technology management' — the existence of DARPA/DIU fast-cycle programs proves it is not necessary, only institutionally entrenched.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    risk_mitigation_threshold,
    'What level of procurement friction is legitimate risk management versus extractive bureaucratic self-protection?',
    'Comparative analysis of acquisition timelines across allied militaries; correlation between approval cycle length and operational outcomes (success rate of fielded systems); audit of failed or delayed programs to identify risk-mitigation value versus institutional gatekeeping',
    'If most friction is legitimate risk management: constraint reclassifies toward Rope; extraction is coordination cost. If significant fraction is self-protective: constraint remains Tangled Rope; extraction is institutional rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(risk_mitigation_threshold, empirical, 'Threshold between legitimate risk management and extractive bureaucratic friction').

omega_variable(
    adversary_iteration_parity,
    'Do peer adversaries operate under comparable technological agility constraints, or does asymmetric agility create strategic vulnerability for the constrained side?',
    'Comparative intelligence assessment of adversary acquisition timelines; analysis of operational technology refresh cycles in peer militaries; historical case studies of technological surprise or obsolescence',
    'If adversaries face similar friction: constraint is symmetric (may be extractive but not strategically dangerous). If adversaries iterate faster: constraint creates asymmetric vulnerability, elevating extraction severity and justifying reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adversary_iteration_parity, empirical, 'Whether adversaries face comparable agility constraints').

omega_variable(
    reform_sustainability,
    'Do rapid acquisition authorities (DARPA OTA, DIU, SpaceForce rapid prototyping) represent a sustainable sunset to traditional FYDP timelines, or are they niche exemptions that preserve the primary constraint?',
    'Longitudinal tracking of budget share in rapid-cycle programs versus traditional acquisition; analysis of whether rapid programs graduate to mainstream or remain exception; assessment of whether traditional procurement actors are actively constraining rapid program success',
    'If rapid programs scale sustainably: scaffold sunset is real; constraint is genuinely temporary. If rapid programs remain niche: scaffold is aspirational; constraint persists as primary institutional structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_sustainability, empirical, 'Whether rapid acquisition authorities provide sustainable alternative to traditional timelines').

omega_variable(
    interoperability_necessity,
    'How much of the procurement cycle friction is genuinely required for interoperability and compatibility across service branches and allied forces?',
    'Analysis of interoperability failures and their causes; assessment of whether failed interoperability stems from insufficient testing or from other sources (doctrine, training, command structure); case studies of rapid deployments and their interoperability outcomes',
    'If interoperability is primary driver: much friction is legitimate coordination (Rope classification increases). If other factors dominate: friction is extractive overhead (Tangled Rope classification holds).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_necessity, empirical, 'Proportion of procurement cycle required for interoperability coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(military_technology_agility, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mta_tr_t0, military_technology_agility, theater_ratio, 0, 0.38).
narrative_ontology:measurement(mta_tr_t5, military_technology_agility, theater_ratio, 5, 0.44).
narrative_ontology:measurement(mta_tr_t10, military_technology_agility, theater_ratio, 10, 0.48).
narrative_ontology:measurement(mta_tr_t15, military_technology_agility, theater_ratio, 15, 0.52).

% Extraction over time
narrative_ontology:measurement(mta_be_t0, military_technology_agility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mta_be_t5, military_technology_agility, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(mta_be_t10, military_technology_agility, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(mta_be_t15, military_technology_agility, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(military_technology_agility, enforcement_mechanism).
narrative_ontology:affects_constraint(military_technology_agility, adversary_technology_parity).
narrative_ontology:affects_constraint(military_technology_agility, military_workforce_modernization).
narrative_ontology:affects_constraint(military_technology_agility, defense_industrial_base_consolidation).

% DUAL FORMULATION NOTE:
% Military technology agility is upstream of specific capability constraints (adversary tech parity, workforce modernization) and downstream of defense industrial structure (consolidation, supply chain lock-in). The agility constraint is distinct from these because it focuses on institutional timelines rather than specific technological claims, but it causally affects whether particular capability gaps are resolved.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(military_technology_agility, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
