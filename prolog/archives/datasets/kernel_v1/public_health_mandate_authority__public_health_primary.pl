% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__public_health_primary, []).

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
 *   constraint_id: public_health_mandate_authority__public_health_primary
 *   human_readable: Public Health Mandate Authority (Public Health Primary Reading)
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   Public health mandate authority represents a structural obligation on
 *   populations to coordinate protective action for vulnerable subgroups
 *   (immunocompromised, healthcare infrastructure) via collective
 *   participation in epidemiological countermeasures (vaccination, masking,
 *   isolation when infected). This reading (public_health_primary)
 *   instantiates one side of a deep constitutional contest: whether
 *   collective obligation to protect the vulnerable commons takes precedence
 *   over individual bodily autonomy as a foundational right. Under this
 *   reading, mandate authority is justified because the vulnerable cannot
 *   protect themselves through individual action — coordinated population
 *   response is the only mechanism that prevents transmission to the
 *   immunocompromised. The constraint exhibits tangled_rope structure because
 *   it combines genuine coordination function (solving a collective action
 *   problem: individual incentive to free-ride vs. population incentive to
 *   prevent transmission) with asymmetric extraction (costs borne by
 *   mandate-resistant individuals via employment loss, service denial,
 *   identity rupture, while benefits accrue to vulnerable populations and
 *   public health institutions). The measurement trajectory shows
 *   extractiveness rising from 0.35 (early pandemic, high voluntary
 *   compliance, low enforcement) to 0.58 (peak mandate enforcement, maximum
 *   employment/service-access penalties) and declining to 0.54 (herd immunity
 *   progress reducing enforcement intensity). Suppression (coercive force
 *   available to enforce compliance) peaks at 0.68 mid-interval as
 *   employment-based mandates and institutional penalties are implemented at
 *   scale. Theater (performative vs. functional mandate activity) rises
 *   modestly from 0.42 to 0.52, indicating that as herd immunity climbs and
 *   epidemiological urgency declines, mandate enforcement increasingly relies
 *   on institutional ritual (continued enforcement to sustain budget and
 *   authority justification) rather than epidemiological necessity.
 *
 * KEY AGENTS:
 *   - Public Health Authority: Beneficiary & coordinator (institutional/arbitrage) — experiences mandate as solving coordination problem; derives authority from epidemic management; benefits from institutional persistence
 *   - Immunocompromised Populations: Primary beneficiary & victim (moderate/constrained) — protected by mandate coordination but dependent on population compliance; bears cost of knowing they require coercion to survive safely
 *   - Vulnerable Commons (healthcare infrastructure, elderly): Beneficiary (collective/implicit) — mandate protects infrastructure capacity and reduces mortality in vulnerable age groups
 *   - Mandate-Resistant Workers (Precarious Employment): Primary victim (powerless/trapped) — faces employment loss, service denial, livelihood pressure; minimal exit options; no coordination benefit experienced
 *   - Threshold Compliers (Sufficient Resources): Secondary victim (moderate/constrained) — can absorb mandate costs but faces identity strain, relational rupture, behavioral control; experiences mixed coordination and extraction
 *   - Identity-Locked Mandate Resisters: Deep victim (powerless/identity_locked) — vaccine refusal core to religious/political/health identity; faces community sanction and coercive enforcement; binding mechanism is identity maintenance, not material constraint alone
 *   - Public Health Institutional Infrastructure: Beneficiary (institutional/arbitrage) — budget justification, professional authority, institutional persistence depend on epidemic threat perception and mandate-authority legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, 0.58).
domain_priors:suppression_score(public_health_mandate_authority__public_health_primary, 0.65).
domain_priors:theater_ratio(public_health_mandate_authority__public_health_primary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__public_health_primary, "Public Health Mandate Authority (Public Health Primary Reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__public_health_primary, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__public_health_primary, 'b2ca09ca-1c7b-437c-84e2-740aea4e259a').
narrative_ontology:cs_kernel_codification('b2ca09ca-1c7b-437c-84e2-740aea4e259a', formalized).
narrative_ontology:cs_authority_grounding('b2ca09ca-1c7b-437c-84e2-740aea4e259a', extraction).
narrative_ontology:cs_interpretation_layer_present('b2ca09ca-1c7b-437c-84e2-740aea4e259a').
narrative_ontology:cs_reading_relation('b2ca09ca-1c7b-437c-84e2-740aea4e259a', public_health_mandate_authority__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('b2ca09ca-1c7b-437c-84e2-740aea4e259a', public_health_mandate_authority__proportionality_reading, influences).
narrative_ontology:cs_axiom('b2ca09ca-1c7b-437c-84e2-740aea4e259a', foundational, collective_obligation_to_protect_vulnerable).
narrative_ontology:cs_axiom_status(collective_obligation_to_protect_vulnerable, holdable).
narrative_ontology:cs_axiom_grounding('b2ca09ca-1c7b-437c-84e2-740aea4e259a', collective_obligation_to_protect_vulnerable, deontological).
narrative_ontology:cs_axiom('b2ca09ca-1c7b-437c-84e2-740aea4e259a', foundational, vulnerable_commons_dependency).
narrative_ontology:cs_axiom_status(vulnerable_commons_dependency, holdable).
narrative_ontology:cs_axiom_grounding('b2ca09ca-1c7b-437c-84e2-740aea4e259a', vulnerable_commons_dependency, empirically_contingent).
narrative_ontology:cs_reference_frame('b2ca09ca-1c7b-437c-84e2-740aea4e259a', collective_epidemiological_obligation).
narrative_ontology:cs_drift_state('b2ca09ca-1c7b-437c-84e2-740aea4e259a', contemporary_endemic_phase, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b2ca09ca-1c7b-437c-84e2-740aea4e259a', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__public_health_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, healthcare_infrastructure_capacity).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, vulnerable_commons).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, mandate_resistant_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, precarious_workers).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, uninsured_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MANDATE-RESISTANT WORKER (SNARE) — Faces employment loss, service denial, or institutional exclusion for non-compliance. No exit: cannot afford to quit; healthcare dependency may force compliance despite identity or belief commitment. High suppression (coercive enforcement via livelihood threat) and high experienced extractiveness. Minimal coordination benefit from the mandate itself — the extraction mechanism (forced participation) dominates.
constraint_indexing:constraint_classification(public_health_mandate_authority__public_health_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THRESHOLD COMPLIER (TANGLED ROPE) — Has resources to absorb mandate costs (time for vaccination, income to sustain employment if compliance needed, access to care if adverse effects occur) but faces genuine constraint: loss of occupational choice, relational rupture if family/community opposes mandate, identity strain if compliance contradicts health beliefs. Experiences both coordination benefit (protection of vulnerable networks they care about) and extraction (behavioral control via mandate enforcement). Moderate power and constrained exit create perspectival gap: sees mixed mechanism.
constraint_indexing:constraint_classification(public_health_mandate_authority__public_health_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PUBLIC HEALTH AUTHORITY (ROPE) — Experiences the mandate as a coordination mechanism solving a genuine collective action problem: individual incentive to free-ride (avoid vaccination risk) vs. population incentive (prevent transmission). Authority has epistemic and enforcement capacity to implement the solution. Experiences low effective extraction because the mechanism genuinely coordinates — the authority benefits from compliance, but the benefit derives from solving a real coordination problem, not from coercive extraction. Arbitrage exit (can delegate enforcement, can revise thresholds) keeps chi low.
constraint_indexing:constraint_classification(public_health_mandate_authority__public_health_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: IMMUNOCOMPROMISED POPULATIONS (TANGLED ROPE) — Primary beneficiary of mandate (coordination function protects their survival), but also bears extraction: dependency on others' compliance, relocation pressures as anti-mandate sentiment rises, emotional cost of knowing they require population-level coercion to survive safely. Generational horizon captures that this vulnerability is structurally persistent. Constrained exit: cannot choose communities where mandate is less stringent without losing access to concentrated medical infrastructure. Moderate power (organized advocacy, data-driven framing) but structural dependency creates asymmetry.
constraint_indexing:constraint_classification(public_health_mandate_authority__public_health_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MANDATE-RESISTANT IDENTITY COMMUNITIES (SNARE) — Identity-locked exit: for some agents, vaccine refusal is core to religious identity (bodily integrity as sacred), political identity (state authority as illegitimate), or health philosophy (natural immunity as superior). Structural mobility may exist (could accept mandate without material catastrophe) but identity frame makes exit literally unthinkable — compliance would require becoming a different person. The identity lock is enforced by community sanction (family rupture, excommunication, political exile from peer group). Snare classification from this perspective reflects that the identity-locking mechanism is the binding force, not material barriers alone. The extraction is total: full behavioral control via identity maintenance.
constraint_indexing:constraint_classification(public_health_mandate_authority__public_health_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 6: PUBLIC HEALTH TRANSITION INFRASTRUCTURE (SCAFFOLD) — Organized agents (epidemiologists, public health institutions, vaccine manufacturers, healthcare systems) implementing temporary coordination mechanisms with sunset logic: vaccine uptake sufficient to reach herd immunity threshold → mandate relaxation. Theater is moderate (0.48) because institutional implementation tracks genuine epidemiological conditions, not arbitrary enforcement ritual. Sunset clause is structural: as herd immunity climbs, mandate extractiveness declines and coordination becomes achievable without coercion. Constrained exit reflects that the infrastructure is locked into vaccination-promotion logic and cannot easily pivot if epidemiology changes or if alternative protective mechanisms emerge.
constraint_indexing:constraint_classification(public_health_mandate_authority__public_health_primary, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: LEGACY INFECTIOUS DISEASE CONTROL AUTHORITY (PITON) — From a civilizational view, public health mandate authority is a persistent institutional infrastructure originally designed for communicable disease control (smallpox eradication, tuberculosis management, water sanitation). Theater is high (0.65+) at this scale: much of contemporary mandate enforcement persists through institutional inertia (agency budgets depend on disease threat perception, professional authority depends on crisis framing) rather than current epidemiological need. The authority has partially atrophied (public trust eroded, enforcement capacity degraded by litigation and political capture) but persists because no alternative institutional framework has fully replaced it. Arbitrage exit means the authority can shift domain or relabel activities, so chi remains constrained.
constraint_indexing:constraint_classification(public_health_mandate_authority__public_health_primary, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some mandate authority is inherent to collective epidemiological survival: populations with dense transmission and vulnerable subgroups cannot achieve herd immunity through individual choice alone — coordination failure is a structural feature of epidemiology. This perspective sees mandate authority as an immutable property of how groups manage infectious disease, no different from quarantine or vaccination history tracking. However, the base properties (beneficiaries declared, active enforcement required, moderate theater) contradict the mountain classification. The engine will compute this as a false summit, revealing that epidemiological necessity is being naturalized to justify what is actually a contingent institutional arrangement with specific cost allocation.
constraint_indexing:constraint_classification(public_health_mandate_authority__public_health_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__public_health_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(public_health_mandate_authority__public_health_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(public_health_mandate_authority__public_health_primary, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(public_health_mandate_authority__public_health_primary, TR),
    TR >= 0.70.

:- end_tests(public_health_mandate_authority__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The public health primary reading justifies mandate authority as coordination mechanism solving genuine collective action problem (free-rider incentive to avoid vaccination risk vs. population incentive to prevent transmission). Under this framing, extractiveness should be moderate (necessary coordination cost). However, empirical extractiveness is higher because the mandate imposes costs on mandate-resistant individuals far beyond what coordination logic requires: employment loss (not necessary for immunity coordination), service denial (not necessary for transmission prevention), identity rupture (not necessary for vaccination compliance). The 0.58 value reflects that the actual implementation extracts surplus beyond what the coordination function requires. Suppression (0.65): Moderate-high. Coercive enforcement mechanisms include employment-based mandates (threat of livelihood), institutional service denial (healthcare, education, travel), and legal penalties in some jurisdictions. Suppression is not total (some exit routes exist: remote work, medical exemptions, jurisdictional arbitrage) but very significant for powerless agents. Theater (0.48): Moderate. Mandate enforcement has genuine epidemiological function (vaccination does reduce transmission), but theater rises over time as herd immunity increases and enforcement becomes increasingly decoupled from epidemiological justification. Early pandemic mandates (t=0, theater=0.42) have clear functional basis. Late-pandemic mandates (t=12, theater=0.52) increasingly rely on institutional ritual (continued enforcement to sustain authority despite declining transmission risk).
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces dramatic perspectival divergence across the six types. Public health authority sees rope (coordination mechanism). Immunocompromised populations see tangled_rope (genuine protection + dependency-based extraction). Mandate-resistant workers see snare (employment-based coercion with no exit). Identity-locked resisters see snare via identity-lock mechanism (internal enforcement via community sanction). Threshold compliers see tangled_rope (mixed benefit and cost). Public health institutional infrastructure sees piton (ritual authority persisting beyond epidemiological urgency). Analytical observer risks seeing mountain (immutable epidemiological necessity) but this is a false summit — the constraint declares beneficiaries and structural asymmetry indicating constructed institutional arrangement, not natural law. The perspectival gaps reflect that mandate authority operates through structurally different mechanisms for different agents: coordination function for authority, protection for vulnerable, coercion for resistant, identity enforcement for identity-locked.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) maps agent structural relationships to effective extractiveness chi via the sigmoid f(d). Public health authority (beneficiary + arbitrage exit) derives low d (0.15), producing negative/low f(d), keeping chi constrained despite moderate epsilon. Immunocompromised populations (beneficiary + constrained exit) derive moderate d (0.35), producing low f(d), but experience extraction despite beneficiary status because they depend on population compliance. Mandate-resistant workers (victim + trapped exit) derive high d (0.92), producing high f(d) (1.35+), making chi very high despite moderate epsilon — powerless agents bear maximum experienced extraction. Identity-locked resisters derive similar high d but through identity-fusion binding rather than material constraint, producing slightly different extraction pathway. Threshold compliers (moderate benefit and cost, constrained exit) derive moderate d (0.55), producing moderate f(d), capturing their mixed experience. Public health institutional infrastructure (beneficiary + arbitrage) derives low d, keeping chi constrained despite institutional power. The analytical observer (analytical exit option) derives canonical d (0.73), producing moderate f(d), enabling detection of false summit structure.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE AUTHORITY AND MANDATROPHY INTERACTION: This constraint exemplifies how mandatrophy (the gap between mandate-grounded policy and ground truth) emerges in public health systems. Mandatrophy triggers when: (1) the mandate classification (authority to enforce) exceeds empirical justification (herd immunity already achieved or alternative mechanisms available), (2) institutional theater substitutes for functional necessity (enforcement continues to sustain authority despite declining transmission risk), (3) the beneficiary group's actual protective needs diverge from mandate scope (elderly protected via care-home restrictions while younger vulnerable populations receive less targeted support). In the measurement trajectory, mandatrophy accumulates from t=6 to t=12: extractiveness declines (from 0.58 to 0.54) as herd immunity rises, but theater rises (0.48 to 0.52) as enforcement becomes increasingly decoupled from epidemiological function. This is the diagnostic signature of mandatrophy: theater rising while extractiveness falls indicates institutional ritual replacing genuine function. RESOLUTION: Mandate sunset logic (scaffold perspective) is the structural mechanism to prevent mandatrophy accumulation — explicit withdrawal of mandate authority when herd immunity threshold is achieved. Without sunset, the constraint risks reclassifying to piton (degraded institutional ritual) or snare (coercion maintained for institutional benefit rather than epidemiological necessity). The analysis recognizes mandatrophy not as a failure of the mandate itself but as a predictable failure of open-ended institutional authority absent explicit sunset mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does public health mandate authority derive from collective obligation to protect the vulnerable (public health primary) or from individual bodily autonomy as foundational right (bodily autonomy primary)?',
    'This is the irreducible kernel contest itself. The omega documents that this constraint IS one reading of the kernel, not the kernel itself. Resolution would require adjudicating which foundational claim takes priority — a political/normative decision, not an empirical discovery.',
    'If public health primary: immunocompromised populations enter victim set for mandate-resistant agents; mandate extractiveness is justified by coordination logic. If bodily autonomy primary: public health authority enters victim set (its coercive power is seen as rights violation); mandate extractiveness is an injustice regardless of epidemiological benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Core reading contest: public health obligation vs. bodily autonomy primacy').

omega_variable(
    threshold_ambiguity,
    'At what vaccination uptake rate does mandate extractiveness transition from justified coordination cost to unjustified coercion?',
    'Herd immunity threshold varies by pathogen (measles ~95%, COVID-19 ~70-90% depending on variant). But the normative question (when does mandatory enforcement become disproportionate?) is not determined by epidemiology alone. Requires assessment of: (1) whether voluntary compliance is trending toward threshold, (2) whether alternative protective mechanisms (treatment, reinfection immunity, environmental controls) reduce mandate necessity, (3) whether enforcement costs (employment loss, social rupture, identity violation) exceed avoided transmission risk.',
    'If threshold is strict (mandate ends at 70% uptake): scaffold sunset logic is credible, theater_ratio should decrease over time. If threshold is loose (mandate persists indefinitely): scaffold becomes piton, theater_ratio remains high, constraint reclassifies toward snare as enforcement tightens without epidemiological justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_ambiguity, empirical, 'Epidemiological and normative threshold for mandate sunset').

omega_variable(
    proportionality_hidden_cost,
    'What portion of mandate extractiveness from powerless agents (employment loss, service denial, identity rupture) is necessary to achieve public health coordination vs. extractive surplus that could be eliminated through alternative mechanisms (incentive design, voluntary risk-pooling, targeted protection)?',
    'Comparative institutional analysis: mandate coercion vs. voluntary high-incentive programs vs. targeted protection (isolation/shielding) for vulnerable populations. Cost accounting of employment loss, medical injury litigation, mental health impacts, trust erosion in public institutions. If alternative mechanisms achieve equivalent epidemiological outcomes with lower total extractiveness, the mandate''s surplus is disproportionate.',
    'If surplus is large: mandate reclassifies as snare (pure extraction) or worse from powerless perspective. If surplus is small: tangled_rope classification holds, indicating genuine coordination asymmetry. If no alternative achieves equivalent outcome: mountain (natural law) classification gains legitimacy, but only with explicit acknowledgment that the extraction is necessary, not optional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_hidden_cost, empirical, 'Extractive surplus vs. necessary coordination cost in mandate design').

omega_variable(
    identity_lock_vs_constrained_exit,
    'For mandate-resistant individuals, is the binding mechanism identity fusion (unthinkable to become pro-vaccine) or material constraint (employment/service access is genuinely unavailable without compliance)?',
    'Post-mandate-end observation: Do agents whose identity-lock framing was the primary resistance return to pre-mandate baseline if mandate ends and enforcement ceases? Or do they persist in resistance despite zero material penalty? If identity persists despite zero penalty: identity_locked classification is correct. If resistance was tactical/economically motivated and drops when enforcement ends: constrained classification is more accurate, indicating the binding mechanism was material coercion, not identity.',
    'If identity_locked dominates: the constraint operates via cognitive capture, not pure coercion. Mandate extractiveness is enforced by internal identity maintenance, not only external enforcement. This suggests the mandate''s theater is lower than measured (identity enforcement is real work), and suggests exit-pathway interventions should target identity reframing rather than policy change alone. If constrained dominates: the mechanism is straightforward material coercion, theater_ratio is accurate, and policy change (mandate removal or redesign) directly addresses the extractive mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_exit, empirical, 'Identity-lock vs. material constraint as binding mechanism in mandate resistance').

omega_variable(
    false_summit_natural_law,
    'Is public health mandate authority a natural law (immutable feature of epidemiology and population dynamics) or a constructed institutional arrangement that benefits specific actors and could be replaced?',
    'This is the FSM trigger. The constraint declares beneficiaries (public health authority, vulnerable populations) and victims (mandate-resistant individuals, precarious workers), indicating structural asymmetry. If the ''natural law'' framing naturalizes what is actually a beneficiary-designed system, FSM reclassification applies. Resolved by tracing which actors benefit from mandate-authority institutionalization and whether they have incentive to maintain the natural-law framing.',
    'If natural law is correct: mandate authority is justified by epidemiological necessity; extractiveness is unfortunate but unavoidable cost of group survival. If false summit confirmed: mandate authority is a constructed constraint that benefits public health institutions and vulnerable populations at the cost of mandate-resistant populations; reclassifies from mountain to tangled_rope or snare depending on alternative institutional designs available.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Is epidemic management mandate authority a natural law or constructed arrangement?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__public_health_primary, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(phm_theater_t0, public_health_mandate_authority__public_health_primary, theater_ratio, 0, 0.42).
narrative_ontology:measurement(phm_theater_t6, public_health_mandate_authority__public_health_primary, theater_ratio, 6, 0.48).
narrative_ontology:measurement(phm_theater_t12, public_health_mandate_authority__public_health_primary, theater_ratio, 12, 0.52).

% Extraction over time
narrative_ontology:measurement(phm_extractiveness_t0, public_health_mandate_authority__public_health_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(phm_extractiveness_t6, public_health_mandate_authority__public_health_primary, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(phm_extractiveness_t12, public_health_mandate_authority__public_health_primary, base_extractiveness, 12, 0.54).

% Suppression requirement over time
narrative_ontology:measurement(phm_suppression_t0, public_health_mandate_authority__public_health_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(phm_suppression_t6, public_health_mandate_authority__public_health_primary, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(phm_suppression_t12, public_health_mandate_authority__public_health_primary, suppression_requirement, 12, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__proportionality_reading).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, vaccine_injury_compensation_asymmetry).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, infection_risk_allocation_markets).

% DUAL FORMULATION NOTE:
% The public health mandate authority kernel contains three structurally distinct constraints, each producing different victim/beneficiary sets and extractiveness values. The public_health_primary reading prioritizes collective obligation to protect vulnerable; the bodily_autonomy_primary reading prioritizes individual right to bodily integrity; the proportionality_reading bridges through balancing test. Each is a complete constraint story with its own epsilon, perspectives, and measurements. They are linked via network.affects_constraints because the choice of reading determines which downstream constraints (vaccine injury compensation, infection risk allocation) activate and how they classify.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_mandate_authority__public_health_primary, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
