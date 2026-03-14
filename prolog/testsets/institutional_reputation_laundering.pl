% ============================================================================
% CONSTRAINT STORY: institutional_reputation_laundering
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_reputation_laundering, []).

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
 *   constraint_id: institutional_reputation_laundering
 *   human_readable: Institutional Reputation Laundering Through Third-Party Legitimacy
 *   domain: institutional_governance/organizational_ethics
 *
 * SUMMARY:
 *   Institutional reputation laundering is the structural practice of
 *   acquiring external legitimacy through third-party endorsement to obscure
 *   internal failures, ethical violations, or misaligned practices. A
 *   corporation funds environmental NGOs to certify its 'sustainability'; a
 *   government contracts external auditors to validate human rights
 *   compliance while blocking actual site access; a university hires
 *   consulting firms to attest to its 'diversity' while maintaining
 *   discriminatory hiring practices. The constraint creates a coordination
 *   problem (stakeholders need reliable reputation signals) that becomes an
 *   extraction mechanism (institutions capture the signal to cover extraction
 *   elsewhere). This constraint demonstrates all six DR types from different
 *   perspectives, revealing how institutional legitimacy itself becomes a
 *   commodity that can be separated from the actual conduct it is supposed to
 *   represent. The theater_ratio rises over the interval (0.35 → 0.68)
 *   because validation processes become increasingly performative —
 *   institutions learn to optimize for audit appearance rather than
 *   substantive change, and intermediaries become skilled at certifying
 *   predetermined conclusions. The extractiveness rises (0.32 → 0.58) because
 *   the capacity to launder reputation enables further internal extraction:
 *   the institution can exploit workers, customers, or communities with
 *   reduced external pressure because external stakeholders are distracted by
 *   manufactured reputation signals.
 *
 * KEY AGENTS:
 *   - Reputation-Seeking Institution: Primary beneficiary (institutional/arbitrage) — captures external legitimacy without internal change; benefits from decoupling appearance from reality
 *   - Reputation Commons: Primary victim (powerless/trapped) — abstract collective good of reliable reputation signals; bears extraction through signal degradation as laundering accumulates
 *   - Actual Stakeholders (employees, customers, communities): Secondary victims (powerless/trapped or moderate/constrained) — suffer extraction (labor exploitation, unfair terms, environmental burden) enabled by reputation laundering that suppresses external scrutiny
 *   - Legitimacy Intermediary (NGO, auditor, certification body): Secondary beneficiary (institutional/arbitrage) — experiences constraint as low-friction coordination; captures funding and partnerships through endorsement; minimal exit cost
 *   - Skeptical Auditor: Tertiary actor (moderate/constrained) — faces extraction through pressure and intimidation from institutions seeking favorable audits; also benefits from institutional ecosystem that creates audit demand
 *   - Regulatory Coalition: Organized agents (organized/constrained) — advocates for disclosure, verification mandates, and whistleblower protections; sees laundering as solvable through institutional design change
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing information asymmetry as immutable rather than recognizing it as design choice vulnerable to structural reform
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_reputation_laundering, 0.58).
domain_priors:suppression_score(institutional_reputation_laundering, 0.65).
domain_priors:theater_ratio(institutional_reputation_laundering, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_reputation_laundering, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_reputation_laundering, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_reputation_laundering, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_reputation_laundering, tangled_rope).
narrative_ontology:human_readable(institutional_reputation_laundering, "Institutional Reputation Laundering Through Third-Party Legitimacy").
narrative_ontology:topic_domain(institutional_reputation_laundering, "institutional_governance/organizational_ethics").

domain_priors:requires_active_enforcement(institutional_reputation_laundering).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_reputation_laundering, reputation_seeking_institution).
narrative_ontology:constraint_beneficiary(institutional_reputation_laundering, legitimacy_providing_intermediary).
narrative_ontology:constraint_victim(institutional_reputation_laundering, reputation_commons).
narrative_ontology:constraint_victim(institutional_reputation_laundering, actual_stakeholders_misled).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BETRAYED STAKEHOLDER (SNARE) — Cannot exit reliance on institutional reputation signals; trapped by asymmetric information. The stakeholder (employee, customer, donor, community) bears full extraction cost through misplaced trust. No alternatives available that don't require equal due diligence burden. Maximum extraction without coordination benefit.
constraint_indexing:constraint_classification(institutional_reputation_laundering, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SKEPTICAL AUDITOR (TANGLED ROPE) — Constrained by professional liability and regulatory compliance requirements, but also coordinating genuine accountability functions. Faces extraction through intimidation and resource pressure, but also benefits from institutional ecosystem that creates audit demand. Mixed experience: some coordination, significant coercion.
constraint_indexing:constraint_classification(institutional_reputation_laundering, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LEGITIMACY INTERMEDIARY (ROPE) — NGO, certification body, or media outlet positioned to validate institutional claims. Experiences constraint as low-friction coordination: providing endorsement enables partnerships and funding. Net beneficiary with easy exit — can withhold endorsement without cost. Pure coordination from this structural position.
constraint_indexing:constraint_classification(institutional_reputation_laundering, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (SCAFFOLD) — Organized agents (transparency advocates, enforcement agencies, whistleblower protections) are building disclosure requirements and reputational consequence systems that degrade the laundering mechanism. Structured sunset: mandatory reporting, third-party verification mandates, and regulatory escalation create declining extraction trajectories. Moderate extraction because the coalition has agency and sees the exit path.
constraint_indexing:constraint_classification(institutional_reputation_laundering, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REPUTATION RITUAL (PITON) — Third-party validation processes (corporate social responsibility reports, ESG certifications, diversity statements) have become performative theater: institutions fund the appearance of accountability without structural change. The ritual persists through normative pressure and stakeholder expectation, not because it functions as verification. Theater ratio 0.68 reflects this degradation — the validation process is substantially about managing perception rather than genuine assurance.
constraint_indexing:constraint_classification(institutional_reputation_laundering, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalizing frame, information asymmetry between organizations and external stakeholders is an immutable feature of complex institutions: no external auditor can fully verify internal realities. This perspective risks naturalizing what is actually a contingent institutional design choice — the choice to concentrate verification authority in third parties rather than distributing it through participatory structures. The engine's false summit detector will flag this as naturalization of a solvable coordination problem.
constraint_indexing:constraint_classification(institutional_reputation_laundering, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_reputation_laundering_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_reputation_laundering, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_reputation_laundering, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_reputation_laundering, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_reputation_laundering, TR),
    TR >= 0.70.

:- end_tests(institutional_reputation_laundering_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint enables extraction by decoupling reputation signals from actual conduct. The primary extraction occurs at two removal levels: (1) direct extraction by the institution from actual stakeholders, enabled by reputation laundering that suppresses scrutiny; (2) secondary extraction from the reputation commons through signal degradation. The 0.58 value reflects that while the mechanism is powerful, it requires active institutional effort to maintain (institutions must continuously fund intermediaries and manage disclosure), and regulatory pressure is increasing. Suppression (0.65): High. Suppression operates through multiple channels: information asymmetry (stakeholders cannot directly verify institutional conduct), resource barriers (independent verification is costly), reputational pressure (whistleblowers and skeptics face institutional retaliation), and institutional legitimacy (institutions have formal authority to define the terms of their own evaluation). Suppression is not total because organized groups (regulatory coalitions, investigative journalists, affected communities) can mobilize to challenge laundering, but the barriers are substantial. Theater ratio (0.68): High. Validation processes (CSR reports, ESG certifications, diversity statements, third-party audits) have become substantially performative. Institutions optimize for audit appearance rather than substantive change — they hire consultants to craft narratives, fund third parties to produce favorable reports, and use disclosure processes to preempt criticism rather than enable accountability. The theater has increased over the interval as laundering techniques have become more sophisticated and as institutional sophistication in managing external perception has grown.
 *
 * PERSPECTIVAL GAP:
 *   The original constraint hypothesis claims 'tangled_rope,' and this classification holds from the perspective of moderately powerful, constrained actors (skeptical auditors, partially-captured intermediaries). But the perspectival range reveals the full structure: (1) Institutional beneficiary sees pure Rope — they experience the constraint as unambiguous coordination without cost; (2) Trapped stakeholder sees pure Snare — they experience maximum extraction without coordination benefit; (3) Legitimacy intermediary sees pure Rope (if independent) or may be extracted (if captured, shifting toward Tangled Rope); (4) Organized regulatory coalition sees Scaffold — they perceive the constraint as solvable through structural reform with a sunset timeline; (5) Institution's internal reputation management sees Piton — the validation rituals are recognized as performative theater, maintained through institutional inertia rather than function; (6) Civilizational observer risks seeing Mountain — information asymmetry as natural law — but the structural data reveals this as false summit. The constraint is tangled because it simultaneously coordinates (provides stakeholders with reputation signals they need) and extracts (enables institutions to exploit while suppressing external scrutiny). The coordination is genuine but parasitized: the mechanism itself is sound, but the institutional incentive structure drives its capture and degradation.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value derives from the agent's structural position relative to the constraint. Trapped stakeholders bearing extraction without alternatives experience maximum d (≈0.95 → high f(d) ≈1.42). Institutional beneficiaries with arbitrage options experience minimum d (≈0.05 → low f(d) ≈-0.12). Intermediaries positioned to endorse without liability experience moderate-low d (≈0.20 → f(d) ≈0.02). Organized agents with exit pathways experience moderate d (≈0.40-0.55 → f(d) ≈0.40-0.75). The perspectival gap emerges because the same constraint produces opposite experiences: the institution experiences pure coordination (Rope) because validation enables partnerships without cost; the stakeholder experiences pure extraction (Snare) because reputation laundering enables institutional exploitation without external constraint. The intermediary occupies the bridge position — they could be extracted (constrained by reputational pressure to provide favorable certifications) but typically experience it as low-friction coordination because most intermediaries are financially dependent on institutional funding and can exit by simply accepting whatever is funded.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the 'coordination vs extraction' binary is false at the institutional level. The constraint is BOTH: it coordinates (provides a functional service — reputation signaling) AND extracts (enables institutional deception and exploitation). The mandatrophy emerges from trying to assign a single type when the structure is fundamentally hybrid. The Tangled Rope classification holds because: (1) Coordination function is real and valuable — stakeholders genuinely need reliable institutional reputation signals; (2) Asymmetric extraction is real and severe — the mechanism is actively captured to enable institutional exploitation; (3) Active enforcement is required — institutions invest significant resources in maintaining, funding, and managing third-party legitimacy processes; (4) The constraint cannot exist without both components: remove the coordination function and institutions would have no reason to maintain the intermediaries; remove the extraction opportunity and institutions would have no incentive to capture them. The perspectival multiplicity (all six types appearing from different positions) confirms that the constraint's structure is genuinely complex rather than admitting a single correct classification. The engine should NOT try to resolve this to a single type — the presheaf of perspectives is the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intermediary_capture_mechanism,
    'At what point does a legitimacy intermediary transition from genuine endorsement to capture by the reputation-seeking institution?',
    'Historical analysis of intermediary independence: correlation between funding sources and endorsement patterns; tracking of intermediary position reversals when relationships terminate',
    'If capture is rapid and systematic: intermediary is complicit rather than independent, classification shifts toward snare (higher extraction, lower coordination). If rare: rope classification holds, intermediary is genuinely separate agent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intermediary_capture_mechanism, empirical, 'Mechanism and timeline of intermediary capture').

omega_variable(
    stakeholder_verification_cost,
    'How much verification effort would enable a stakeholder to detect reputational laundering independently?',
    'Comparative study of stakeholder due diligence costs across sectors; measurement of information asymmetry reduction as verification depth increases',
    'If cost is low (< 5% of relationship value): stakeholders are trapped through inattention, not inherent inability — higher-extraction snare. If cost is prohibitive: trapped classification is structural, deepens snare diagnosis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stakeholder_verification_cost, empirical, 'Cost-benefit ratio of stakeholder independent verification').

omega_variable(
    reputation_commons_degradation_rate,
    'How quickly does accumulated reputational laundering degrade the integrity of reputation signals across sectors?',
    'Time-series analysis of reputation metric reliability; correlation between laundering frequency and external trust decline in institutional sectors',
    'If degradation is rapid and cascading: reputation commons experiences accelerating extraction (snare classification strengthens). If slow and recoverable: commons bears moderate extraction (tangled rope holds).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reputation_commons_degradation_rate, empirical, 'Rate of reputation commons degradation from accumulated laundering').

omega_variable(
    disclosure_mandate_effectiveness,
    'Do mandatory disclosure and third-party verification requirements actually reduce reputational laundering or merely formalize its appearance?',
    'Pre/post analysis of regulatory jurisdictions with disclosure mandates; measurement of stakeholder trust recovery and actual institutional behavior change post-regulation',
    'If effective: scaffold sunset is real, regulatory coalition is genuinely building alternative pathways. If merely formalizing appearance: sunset is aspirational, constraint persists despite nominal reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclosure_mandate_effectiveness, empirical, 'Whether disclosure mandates reduce laundering or merely ritualize it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_reputation_laundering, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(repl_tr_t0, institutional_reputation_laundering, theater_ratio, 0, 0.35).
narrative_ontology:measurement(repl_tr_t5, institutional_reputation_laundering, theater_ratio, 5, 0.52).
narrative_ontology:measurement(repl_tr_t10, institutional_reputation_laundering, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(repl_be_t0, institutional_reputation_laundering, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(repl_be_t5, institutional_reputation_laundering, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(repl_be_t10, institutional_reputation_laundering, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_reputation_laundering, identity_coordination).
narrative_ontology:affects_constraint(institutional_reputation_laundering, stakeholder_information_asymmetry).
narrative_ontology:affects_constraint(institutional_reputation_laundering, institutional_accountability_deficit).

% DUAL FORMULATION NOTE:
% Institutional reputation laundering decomposes into two structurally distinct constraints: (1) reputation_signal_intermediation (ε≈0.25, Rope) — the genuine coordination problem of providing stakeholders with reliable institutional reputation signals; (2) reputation_laundering_extraction (ε≈0.58, Tangled Rope) — the capture and degradation of reputation signals to enable institutional extraction. These constraints are linked via network.affects_constraints: laundering undermines signal integrity, which creates demand for increasingly sophisticated intermediation, which in turn creates greater capture opportunities. The reputation commons itself (ε≈0.72, Snare) experiences these as sequential degradation stages.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_reputation_laundering, institutional, 0.08).
constraint_indexing:directionality_override(institutional_reputation_laundering, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
