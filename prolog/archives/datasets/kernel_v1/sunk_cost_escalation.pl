% ============================================================================
% CONSTRAINT STORY: sunk_cost_escalation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sunk_cost_escalation, []).

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
 *   constraint_id: sunk_cost_escalation
 *   human_readable: Sunk Cost Escalation (Commitment Trap)
 *   domain: behavioral_economics/decision_theory
 *
 * SUMMARY:
 *   Sunk cost escalation is a commitment trap where agents continue investing
 *   in projects, relationships, or decisions to justify prior investments,
 *   even when rational cost-benefit analysis would recommend exit. The
 *   constraint operates through four distinct but interlocking mechanisms:
 *   (1) Identity fusion — the agent's self-concept becomes constituted
 *   through the commitment, making exit tantamount to ego death; (2) Social
 *   reputation pressure — peers, investors, and community members benefit
 *   from stability and penalize reversals, creating collective enforcement of
 *   escalation; (3) Organizational theater — escalation review processes
 *   (go/no-go meetings, sunk cost audits, portfolio reviews) are largely
 *   performative, legitimizing further resource outflow while performing the
 *   appearance of rational constraint; (4) Suppression of exit through
 *   structural barriers — the costs of exit (job loss, reputation damage,
 *   relationship dissolution, identity crisis) exceed the costs of continued
 *   escalation in the short term, creating a ratchet dynamic where each
 *   escalation round raises the exit threshold. The constraint exhibits all
 *   six DR types from different structural positions: pure extraction (snare)
 *   for the identity-locked agent who cannot exit; mixed
 *   coordination-extraction (tangled rope) for peer networks that benefit
 *   from stability; pure coordination (rope) for institutional beneficiaries
 *   with arbitrage capacity; temporary coordination with sunset (scaffold)
 *   for decision science reformers building alternative protocols; degraded
 *   ritual (piton) for the organizational commitment review process itself;
 *   and false-summit naturalization (mountain) when escalation is framed as
 *   inherent to bounded rationality. The escalating measurements of
 *   base_extractiveness (0.32 → 0.58), theater_ratio (0.52 → 0.68), and
 *   suppression_requirement (0.48 → 0.65) show a constraint that hardens over
 *   time: as the agent commits more resources, their identity fuses deeper,
 *   peer pressure intensifies, organizational theater becomes more elaborate,
 *   and exit barriers rise. The trap is self-tightening.
 *
 * KEY AGENTS:
 *   - Trapped Agent: Primary victim (powerless/identity_locked) — identity fused with commitment; bears escalating resource costs and psychological strain; cannot perceive exit despite structural mobility
 *   - Escalation Theater Producers: Primary beneficiaries (institutional/arbitrage) — managers, institutional actors, organizational hierarchy; profit from continued escalation while maintaining plausible deniability of extraction through performative review rituals
 *   - Peer Social Pressure Network: Secondary beneficiaries (moderate/constrained) — colleagues, investors, community; benefit from commitment stability while exerting reputational penalty on exit attempts
 *   - Decision Science Reform Movement: Organized agents (organized/constrained) — behavioral economists, policy reformers, organizational consultants; see escalation as temporary coordination failure with sunset, building decision-hygiene protocols
 *   - Organizational Commitment Ritual: Institutional actor (institutional/arbitrage) — the explicit escalation review process; performs constraint while legitimizing escalation; theater-heavy, function-light
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable cognitive limits (false summit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sunk_cost_escalation, 0.58).
domain_priors:suppression_score(sunk_cost_escalation, 0.65).
domain_priors:theater_ratio(sunk_cost_escalation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sunk_cost_escalation, extractiveness, 0.58).
narrative_ontology:constraint_metric(sunk_cost_escalation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sunk_cost_escalation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sunk_cost_escalation, tangled_rope).
narrative_ontology:human_readable(sunk_cost_escalation, "Sunk Cost Escalation (Commitment Trap)").
narrative_ontology:topic_domain(sunk_cost_escalation, "behavioral_economics/decision_theory").

domain_priors:requires_active_enforcement(sunk_cost_escalation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sunk_cost_escalation, escalation_enforcers).
narrative_ontology:constraint_beneficiary(sunk_cost_escalation, commitment_theater_producers).
narrative_ontology:constraint_victim(sunk_cost_escalation, trapped_agent).
narrative_ontology:constraint_victim(sunk_cost_escalation, resource_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED AGENT (SNARE) — The agent's identity becomes fused with the project or relationship. Exit would require abandoning a professional identity, relationship role, or self-concept constituted through the commitment. Structurally mobile (could leave) but identity-locked (cannot imagine leaving without becoming a different person). Experiences maximum extraction — must continue pouring resources to maintain identity coherence. Cannot perceive the exit option even when it becomes rational.
constraint_indexing:constraint_classification(sunk_cost_escalation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: PEER SOCIAL PRESSURE NETWORK (TANGLED ROPE) — Colleagues, investors, or community members experience genuine coordination benefits from stable commitment (predictability, follow-through) alongside extraction: they benefit from the trapped agent's continued investment while bearing none of the escalating cost. Mixed structure: the constraint coordinates reputation and stability while extracting resources. Exit costs are high (social penalty, reputational damage) but surmountable for organized peer groups.
constraint_indexing:constraint_classification(sunk_cost_escalation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ESCALATION THEATER PRODUCERS (ROPE) — Organizations, managers, or institutional actors who benefit from the commitment trap structure experience it as pure coordination: they are managing risk through escalating commitment review, maintaining team stability, and justifying further resource allocation. They have arbitrage capacity — can exit the escalation at any moment by declaring the project dead. Experience the constraint as beneficial coordination with minimal extraction cost.
constraint_indexing:constraint_classification(sunk_cost_escalation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECISION SCIENCE REFORM MOVEMENT (SCAFFOLD) — Behavioral economists, policy reformers, and organizational consultants see sunk cost escalation as a temporary coordination failure with a sunset: systematic decision review protocols, sunk cost accounting systems, and organizational structures that separate past decisions from future allocations are creating exit pathways. The constraint decays as institutions adopt decision-hygiene practices. Organized resistance with clear sunset mechanism.
constraint_indexing:constraint_classification(sunk_cost_escalation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ORGANIZATIONAL COMMITMENT RITUAL (PITON) — The explicit escalation review process (project portfolio meetings, go/no-go decisions, sunk cost audits) is largely performative theater: committees meet, past investments are reviewed, future viability is assessed — and the overwhelming default is continued escalation. The ritual maintains institutional identity (organizations are 'decisive' and 'committed') while legitimizing further resource outflow. Theater ratio 0.68 reflects that most escalation reviews produce escalation decisions, not exits. The function (constraining waste) has decayed; the ritual persists through institutional inertia.
constraint_indexing:constraint_classification(sunk_cost_escalation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, sunk cost escalation appears as an unchangeable property of bounded rationality: human decision-makers cannot fully disregard past investments (the sunk cost fallacy is rooted in cognitive limits), and commitment to coherent identity is inherent to human psychology. This perspective sees escalation as immutable law of human behavior. However, the presence of identifiable beneficiaries (escalation theater producers, institutions that profit from continued commitment) and victims (trapped agents bearing accelerating costs) indicates this is a false summit: the 'inevitable' psychological constraint masks contingent institutional arrangements.
constraint_indexing:constraint_classification(sunk_cost_escalation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sunk_cost_escalation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sunk_cost_escalation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sunk_cost_escalation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sunk_cost_escalation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sunk_cost_escalation, TR),
    TR >= 0.70.

:- end_tests(sunk_cost_escalation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significant resources from trapped agents through escalating commitment cycles, but the extraction is partially obscured by the coordination functions (peer stability, organizational predictability) and psychological mechanisms (identity maintenance). The measurement trajectory (0.32 → 0.58) reflects that extractiveness increases as the commitment deepens and identity fusion intensifies. Suppression (0.65): High. Multiple suppression mechanisms operate simultaneously: (1) Identity-psychological suppression — the agent's identity frame prevents exit perception; (2) Social suppression — reputational penalty for reversing commitment; (3) Structural suppression — job loss, relationship dissolution, financial consequences of exit; (4) Organizational suppression — escalation theater legitimizes continued investment while appearing to constrain it. The combination produces high suppression despite agents' nominal structural mobility. Theater ratio (0.68): High-moderate. The organizational escalation review process is substantially performative: committees meet, past sunk costs are reviewed, viability is assessed, and the default outcome is continued escalation. The theater has increased over the measurement interval (0.52 → 0.68) as the review process becomes more elaborate and formalized without changing escalation outcomes. The ritual maintains institutional identity while legitimizing extraction.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces a sharp perspectival divide between the trapped agent's snare experience and the institutional beneficiary's rope experience. The trapped agent perceives no exit (snare, identity_locked) — they are caught between escalation costs and ego-death costs. The institutional beneficiary perceives coordination and opportunity (rope, arbitrage) — they manage risk through escalation review and maintain team stability. The peer network sees mixed coordination-extraction (tangled rope) — they benefit from stability while exerting extraction pressure. The decision science reformers see a temporary problem (scaffold) — they believe protocol changes and decision-hygiene training will create sunset pathways. The organizational ritual itself is degraded (piton) — the review process appears functional but primarily legitimizes escalation. The civilizational analytical observer risks seeing an immutable cognitive law (mountain) — sunk cost fallacy is inherent to bounded rationality — but the structural data reveals this as false-summit naturalization: beneficiaries exist (escalation enforcers), victims exist (trapped agents), and extraction is real, suggesting the constraint is institutional rather than cognitive.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's structural relationship to the escalation flow. The trapped agent (powerless, identity_locked, victim) experiences maximum directionality toward extraction (d ≈ 0.89) — they are the extraction target and cannot exit. The institutional beneficiary (institutional, arbitrage, beneficiary) experiences minimum directionality (d ≈ 0.05) — they are the extraction beneficiary with full exit capacity (can declare projects dead at any moment). The peer network (moderate, constrained, mixed beneficiary-victim) experiences moderate-high directionality (d ≈ 0.55) — they benefit but also constrain their own exit through reputational enforcement. The decision science reformers (organized, constrained) experience moderate directionality (d ≈ 0.45) — they are trying to reduce extraction through protocol intervention. The organizational ritual (institutional, arbitrage) experiences low directionality (d ≈ 0.12) — the review process benefits the institution while appearing neutral. The analytical observer (analytical, analytical) experiences community-standard directionality (d ≈ 0.72) reflecting their structural distance from the constraint. The sigmoid f(d) function maps these directionality values to experienced chi: the trapped agent's high d produces high chi (maximum experienced extraction); the beneficiary's low d produces negative chi (extraction flows toward them); the peer network's moderate d produces moderate chi (mixed experience).
 *
 * MANDATROPHY ANALYSIS:
 *   Sunk cost escalation resolves mandatrophy by showing that the constraint is genuinely tangled-rope-like (mixed coordination-extraction) at the base structural level, but appears as snare, rope, scaffold, piton, or false-summit mountain depending on the observer's position. The mandatrophy question — 'Is this coordination or extraction?' — has a polyvalent answer: it is both, simultaneously, from different positions. The trapped agent experiences pure extraction (snare) because identity fusion has removed their exit option. The institutional beneficiary experiences pure coordination (rope) because they have arbitrage capacity and the constraint stabilizes their domain. The reform movement experiences temporary coordination (scaffold) because they see a sunset pathway. The organizational ritual experiences performative coordination (piton) because the function has decayed but the form persists. The analytical observer risks seeing immutable law (mountain) because the psychological mechanisms appear to be law-like features of cognition. The resolution is not to choose one type but to acknowledge that the constraint's type is indexical: it depends on who is measuring and from what structural position they observe. The false-summit detection on the mountain perspective is diagnostic — it reveals that when an apparently immutable cognitive law has identifiable beneficiaries and victims, the 'law' is likely an institutional arrangement wearing the mask of natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_depth,
    'What proportion of escalation is driven by identity fusion (internal lock) versus external material barriers (constrained exit costs)?',
    'Post-exit trajectory analysis: agents who successfully exit sunk-cost escalation — do they quickly re-establish identity and project engagement (identity_locked → mobile), or do they experience persistent constraint and disorientation (trapped/constrained)?',
    'If primarily identity-locked: the constraint is perceptual; interventions should target identity frame breaking (reframing exercises, identity diversification). If primarily constrained exit costs: interventions should target barrier reduction (organizational restructuring, legal fee reduction). If both: requires dual intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_depth, empirical, 'Proportion of escalation driven by identity fusion vs external barriers').

omega_variable(
    theater_gate_calibration,
    'At what ratio of escalation-decision rates to total project reviews does the organizational commitment ritual cross from performative theater into genuine functional gate?',
    'Organizational audit data: what percentage of projects escalate through review processes vs are terminated or restructured? Benchmark comparison: what is the base rate of escalation decisions in organizations without explicit sunk-cost accounting?',
    'If escalation rate > 80%: pure theater, piton classification confirmed. If escalation rate 40-60%: mixed functional/performative (tangled rope floor). If escalation rate < 30%: the review process is genuinely constraining escalation (rope or scaffold range).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_gate_calibration, empirical, 'Ratio of escalation decisions to total project reviews').

omega_variable(
    beneficiary_surplus_extraction,
    'What is the total resource surplus extracted by escalation enforcers (managers, investors, organizational hierarchy) relative to the trapped agent''s opportunity cost?',
    'Comparative cost accounting: resource dollars committed to escalated projects vs dollars that would have been deployed in optimal alternative allocation. Beneficiary gains tracking: do institutional actors show measurable career advancement, budget growth, or resource concentration correlated with escalation decisions they produce?',
    'If beneficiary surplus is large and uncorrelated with project success: pure extraction mechanism (snare floor). If beneficiary surplus tracks with genuine coordination value (team stability, reputation protection): tangled rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_surplus_extraction, empirical, 'Surplus extraction by escalation enforcers vs opportunity cost').

omega_variable(
    decision_science_reform_effectiveness,
    'Do formal sunk-cost elimination protocols (decision-hygiene training, accounting system changes, portfolio-level review) actually reduce escalation rates, or do they merely formalize escalation theater?',
    'Randomized controlled trials or quasi-experimental comparison: organizations adopting formal sunk-cost protocols vs control organizations. Measurement: escalation rates, project termination rates, resource reallocation speed, and long-term project ROI.',
    'If protocols are effective (escalation drops 20%+): scaffold sunset is real, reform movement is structurally powerful. If protocols are ineffective or increase theater ratio: the reform produces formalized escalation cover story (scaffold collapses to piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decision_science_reform_effectiveness, empirical, 'Effectiveness of decision-science reform protocols in reducing escalation').

omega_variable(
    false_summit_beneficiary_structure,
    'Is sunk cost escalation truly an immutable feature of bounded rationality, or a contingent institutional arrangement that benefits identifiable actors?',
    'Structural comparison: societies/organizations with high escalation rates vs those with low rates. Analysis of beneficiary groups: who profits from escalation acceptance? Are escalation enforcers distributed or concentrated? Do escalation patterns correlate with institutional structure (hierarchical vs flat, transparent vs opaque) independent of cognitive limits?',
    'If escalation is truly cognitive: mountain classification confirmed, no beneficiaries needed. If escalation is institutional: false summit (mountain reclassifies to tangled rope or snare based on coordination vs extraction ratio). This resolves whether the constraint is law-like or contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_beneficiary_structure, conceptual, 'Whether escalation is immutable cognitive limit or contingent institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sunk_cost_escalation, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sce_tr_t0, sunk_cost_escalation, theater_ratio, 0, 0.52).
narrative_ontology:measurement(sce_tr_t3, sunk_cost_escalation, theater_ratio, 3, 0.62).
narrative_ontology:measurement(sce_tr_t6, sunk_cost_escalation, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(sce_be_t0, sunk_cost_escalation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(sce_be_t3, sunk_cost_escalation, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(sce_be_t6, sunk_cost_escalation, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sce_su_t0, sunk_cost_escalation, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(sce_su_t3, sunk_cost_escalation, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(sce_su_t6, sunk_cost_escalation, suppression_requirement, 6, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sunk_cost_escalation, attachment_coordination).
narrative_ontology:affects_constraint(sunk_cost_escalation, organizational_sunk_cost_accounting).
narrative_ontology:affects_constraint(sunk_cost_escalation, relationship_exit_cost_asymmetry).
narrative_ontology:affects_constraint(sunk_cost_escalation, professional_identity_lock).

% DUAL FORMULATION NOTE:
% Sunk cost escalation is a constraint family spanning three structurally distinct claims: (1) organizational escalation of commitment (ε=0.58, tangled rope) — resource allocation decisions in institutional contexts; (2) relationship exit cost asymmetry (ε=0.72, snare) — personal relationships and partnerships with high exit barriers; (3) professional identity lock (ε=0.65, snare) — career commitment with identity fusion. Each has different beneficiaries, different suppression mechanisms, and different reform pathways, but all three operate through the same core mechanism: identity fusion + social pressure + organizational theater + structural exit barriers. They are linked causally: institutional escalation creates career identity lock, which creates relationship lock-in.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sunk_cost_escalation, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
