% ============================================================================
% CONSTRAINT STORY: cia_fbi_legal_wall
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% Status: [ACTIVE (HISTORICAL CONTEXT PRE-2001)]
% ============================================================================

:- module(constraint_cia_fbi_legal_wall, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cia_fbi_legal_wall
 *   human_readable: The CIA/FBI Intelligence-Criminal "Wall" (pre-PATRIOT Act)
 *   domain: political/legal
 *
 * SUMMARY:
 *   The 'Wall' refers to the legal and procedural barriers, primarily rooted
 *   in the 1978 FISA act and 1995 DOJ guidelines, that separated foreign
 *   intelligence gathering from domestic criminal investigations in the
 *   United States. Its stated purpose was to protect the civil liberties of
 *   US persons from the broad, lower-evidence standard powers of the
 *   intelligence community. However, as detailed in the 9/11 Commission
 *   Report, these barriers created critical intelligence gaps by preventing
 *   information sharing, which hindered counter-terrorism efforts. The
 *   constraint represents a stark trade-off between civil liberties and
 *   national security, with different actors experiencing it as either a
 *   vital safeguard or a deadly bureaucratic snare.
 *
 * KEY AGENTS:
 *   - Counter-Terrorism Agents: Primary victims (moderate/trapped) — their operational effectiveness was directly extracted by the information barriers.
 *   - Civil Liberties Advocates: Primary beneficiaries (organized/constrained) — saw the Wall as a necessary coordination mechanism to enforce constitutional protections.
 *   - Intelligence Agency Leadership: Institutional actors (institutional/constrained) — experienced a dual role, benefiting from source protection while suffering from operational friction.
 *   - Foreign Adversaries: Secondary beneficiaries (organized/arbitrage) — exploited the intelligence seams created by the Wall.
 *   - Analytical Observers: Post-hoc analysts (analytical/analytical) — view the system's dual nature and catastrophic failure mode.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cia_fbi_legal_wall, 0.55).
domain_priors:suppression_score(cia_fbi_legal_wall, 0.8).
domain_priors:theater_ratio(cia_fbi_legal_wall, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cia_fbi_legal_wall, extractiveness, 0.55).
narrative_ontology:constraint_metric(cia_fbi_legal_wall, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(cia_fbi_legal_wall, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cia_fbi_legal_wall, tangled_rope).
narrative_ontology:human_readable(cia_fbi_legal_wall, "The CIA/FBI Intelligence-Criminal \"Wall\" (pre-PATRIOT Act)").
narrative_ontology:topic_domain(cia_fbi_legal_wall, "political/legal").

domain_priors:requires_active_enforcement(cia_fbi_legal_wall).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cia_fbi_legal_wall, civil_liberties_advocates).
narrative_ontology:constraint_beneficiary(cia_fbi_legal_wall, us_persons_under_investigation).
narrative_ontology:constraint_beneficiary(cia_fbi_legal_wall, intelligence_community_sources_and_methods).
narrative_ontology:constraint_victim(cia_fbi_legal_wall, counter_terrorism_agents).
narrative_ontology:constraint_victim(cia_fbi_legal_wall, national_security_operations).
narrative_ontology:constraint_victim(cia_fbi_legal_wall, general_public_safety).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE AGENT (SNARE) — From the perspective of an FBI or CIA agent trying to connect dots, the Wall was a bureaucratic trap. It actively prevented the sharing of critical information, extracting operational effectiveness and creating immense friction. They are trapped by the legal and procedural rules, bearing the full cost of intelligence gaps. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.78.
constraint_indexing:constraint_classification(cia_fbi_legal_wall, snare,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE ADVOCATE (ROPE) — For groups like the ACLU, the Wall was a pure coordination mechanism. It solved the problem of preventing intelligence agencies (with lower evidence standards) from infringing on the constitutional rights of US persons during criminal investigations. The costs are seen as necessary safeguards. As a beneficiary, d is low. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.005. Negative extraction indicates a net subsidy to their goals.
constraint_indexing:constraint_classification(cia_fbi_legal_wall, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AGENCY LEADERSHIP (TANGLED ROPE) — Agency heads were constrained by the law but also saw a coordination benefit in protecting sources and methods from disclosure in criminal trials. They experienced both the benefit (protecting assets) and the cost (operational friction), making it a hybrid system. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.43.
constraint_indexing:constraint_classification(cia_fbi_legal_wall, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYST (TANGLED ROPE) — The analytical view recognizes the dual function. The Wall had a legitimate, if imperfect, coordination purpose (civil liberties protection) but also imposed a severe, asymmetric extraction of security capability. This is the canonical Tangled Rope classification. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(cia_fbi_legal_wall, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: THE FOREIGN ADVERSARY (ROPE) — From the perspective of a foreign terrorist group, the Wall was a beneficial feature of the operational environment. It acted as a coordination mechanism for US government inaction, creating exploitable seams. As a full beneficiary with arbitrage exit, their effective extraction is negative. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.08.
constraint_indexing:constraint_classification(cia_fbi_legal_wall, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cia_fbi_legal_wall_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cia_fbi_legal_wall, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cia_fbi_legal_wall_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.55): Represents the significant operational friction and loss of effectiveness imposed on national security operations. This is not a financial extraction, but an extraction of capability. Suppression (0.80): High. The Wall was legally mandated and procedurally enforced; sharing information across the divide was not an option for agents. Theater Ratio (0.30): Low. The Wall was not primarily performative; it was a set of legally binding rules with real, tangible consequences, rooted in genuine post-Watergate concerns about intelligence overreach.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For civil liberties advocates, the Wall was a Rope, a successful coordination device to protect rights. For a counter-terrorism agent, it was a Snare, a deadly trap that prevented them from stopping attacks. For agency leadership and later analysts, it was a Tangled Rope, a system with a valid purpose (coordination) but also a severe, asymmetric cost (extraction). This demonstrates how a single legal structure can be perceived as fundamentally different types of constraints depending on the observer's structural relationship to it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Civil Liberties Advocates) are structurally subsidized by the constraint, leading to a Rope classification. Victims (Counter-Terrorism Agents) are structurally targeted for extraction of their capabilities, leading to a Snare classification. The mixed position of Agency Leadership, who are both beneficiaries (source protection) and victims (friction), places them in the middle, resulting in a Tangled Rope. The analytical perspective, which weighs both functions, also arrives at Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a powerful resolution of the mandatrophy. The claim that the Wall was a 'good' constraint (Rope) to protect liberty and a 'bad' constraint (Snare) that enabled terrorism are both correct from their respective indexical positions. The mandatrophy arises from attempting to assign a single, non-indexical classification. The Deferential Realism framework shows that the system was simultaneously a Rope for one group and a Snare for another, with the analytical classification of Tangled Rope capturing this inherent structural conflict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_legal_barrier,
    'To what extent were intelligence failures caused by the legal ''Wall'' versus pre-existing institutional rivalry and mistrust between the CIA and FBI?',
    'Analysis of declassified internal communications and after-action reports from pre-2001 investigations; interviews with former officers from both agencies.',
    'If culture was the primary driver, the legal ''Wall'' was a theatrical justification (Piton) for non-cooperation. If the law was the primary barrier, its classification as a Snare/Tangled Rope is confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_vs_legal_barrier, empirical, 'Distinguishing the effect of the legal rule from institutional culture.').

omega_variable(
    preventability_of_attacks,
    'How many specific terror plots, including 9/11, could have been verifiably thwarted if the ''Wall'' did not exist?',
    'Counter-factual analysis based on declassified intelligence available to different agencies at the time (e.g., the 9/11 Commission Report''s findings on tracking al-Mihdhar and al-Hazmi).',
    'A high number of preventable attacks confirms the Snare perspective from a public safety viewpoint. A low number would support the Rope perspective that the civil liberties protection was worth the low security cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preventability_of_attacks, empirical, 'Counter-factual analysis of attack prevention absent the Wall.').

omega_variable(
    alternative_safeguard_viability,
    'Could an alternative legal framework have protected civil liberties with less operational friction?',
    'Comparative legal analysis of intelligence-law enforcement frameworks in other democracies (e.g., UK, Canada, Australia) during the same period.',
    'If viable alternatives existed, the ''Wall'' appears less like a necessary Rope and more like a poorly designed, high-extraction Tangled Rope. If not, its Rope-like characteristics are strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_safeguard_viability, conceptual, 'Viability of less extractive legal alternatives for civil liberty protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cia_fbi_legal_wall, 1978, 2001).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cia__tr_t0, cia_fbi_legal_wall, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cia__tr_t17, cia_fbi_legal_wall, theater_ratio, 17, 0.25).
narrative_ontology:measurement(cia__tr_t23, cia_fbi_legal_wall, theater_ratio, 23, 0.3).

% Extraction over time
narrative_ontology:measurement(cia__be_t0, cia_fbi_legal_wall, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cia__be_t17, cia_fbi_legal_wall, base_extractiveness, 17, 0.5).
narrative_ontology:measurement(cia__be_t23, cia_fbi_legal_wall, base_extractiveness, 23, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cia_fbi_legal_wall, enforcement_mechanism).
narrative_ontology:affects_constraint(cia_fbi_legal_wall, usa_patriot_act_surveillance_provisions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
