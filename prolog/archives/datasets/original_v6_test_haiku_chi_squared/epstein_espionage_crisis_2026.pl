% ============================================================================
% CONSTRAINT STORY: epstein_espionage_crisis_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epstein_espionage_crisis_2026, []).

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
 *   constraint_id: epstein_espionage_crisis_2026
 *   human_readable: The Epstein-Starmer Sovereignty Crisis
 *   domain: political/espionage
 *
 * SUMMARY:
 *   The disclosure of millions of DOJ pages alleging Jeffrey Epstein was an
 *   Israeli intelligence asset creates a multi-layered sovereignty crisis for
 *   the UK. The primary constraint is not the Epstein case itself, but the
 *   structural mechanism by which a more powerful state (US) unilaterally
 *   releases counterintelligence material about a smaller state's operations
 *   and prior decisions without consultation or consent. The UK government
 *   faces a trilemma: acknowledge prior knowledge of Epstein-intelligence
 *   material and face accusations of complicity or negligence; deny knowledge
 *   and appear incompetent in counterintelligence; or accept responsibility
 *   for a crisis created by US disclosure decisions. The constraint's
 *   extractiveness has increased over the interval as the full scope of
 *   disclosure has become apparent and secondary political consequences
 *   (parliamentary inquiries, press scrutiny, diplomatic tensions) have
 *   cascaded. The constraint operates across multiple institutional layers
 *   simultaneously: bilateral intelligence partnership trust, UK government
 *   credibility, UK counterintelligence reputation, and public epistemic
 *   integrity. The theater ratio (0.65) reflects that much of the UK
 *   government's response is necessarily performative — damage control,
 *   reassurance rhetoric, and restored-confidence statements that cannot
 *   alter the fundamental fact of uncontrolled disclosure.
 *
 * KEY AGENTS:
 *   - UK Counterintelligence Apparatus: Primary victim (powerless/trapped) — suffers irreversible reputational damage and compartmentalization breach
 *   - UK Government (Starmer Administration): Primary target (powerful/trapped) — must manage diplomatic, domestic, and institutional consequences of US disclosure decision
 *   - Public Epistemic Commons: Secondary victim (powerless/trapped) — exposed to unverified intelligence claims without context or verification mechanisms
 *   - US Department of Justice: Institutional beneficiary (institutional/arbitrage) — fulfills transparency obligations; experiences constraint as rule-of-law alignment
 *   - UK Parliament and Press: Organized mediator (organized/constrained) — drive accountability narratives but constrained by diplomatic and legal considerations
 *   - Intelligence Community Institutional Inertia: Piton agent (institutional/arbitrage) — partnership persists functionally degraded through institutional momentum
 *   - Analytical Observer: Structural sovereignty analyst — sees disclosure as unilateral information extraction violating smaller-state sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epstein_espionage_crisis_2026, 0.68).
domain_priors:suppression_score(epstein_espionage_crisis_2026, 0.78).
domain_priors:theater_ratio(epstein_espionage_crisis_2026, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epstein_espionage_crisis_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(epstein_espionage_crisis_2026, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(epstein_espionage_crisis_2026, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epstein_espionage_crisis_2026, snare).
narrative_ontology:human_readable(epstein_espionage_crisis_2026, "The Epstein-Starmer Sovereignty Crisis").
narrative_ontology:topic_domain(epstein_espionage_crisis_2026, "political/espionage").

% --- Structural relationships ---
narrative_ontology:constraint_victim(epstein_espionage_crisis_2026, uk_institutional_sovereignty).
narrative_ontology:constraint_victim(epstein_espionage_crisis_2026, uk_counterintelligence_credibility).
narrative_ontology:constraint_victim(epstein_espionage_crisis_2026, us_uk_intelligence_cooperation).
narrative_ontology:constraint_victim(epstein_espionage_crisis_2026, public_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UK COUNTERINTELLIGENCE (SNARE) — Trapped by cascading disclosure and loss of compartmentalization. Cannot retroactively unlearn what DOJ has released; cannot control narrative framing of counterintelligence decisions made under previous administrations. No exit from the epistemic damage. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.75.
constraint_indexing:constraint_classification(epstein_espionage_crisis_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: UK GOVERNMENT / STARMER ADMINISTRATION (SNARE) — Trapped by US disclosure decisions made without consultation. Must either acknowledge prior knowledge and face domestic legitimacy crisis, deny knowledge and appear incompetent, or accept responsibility for a crisis created by foreign agency action. All paths are extractive. d≈0.88, f(d)≈1.35, σ=1.0 → χ≈0.68.
constraint_indexing:constraint_classification(epstein_espionage_crisis_2026, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: US DEPARTMENT OF JUSTICE (ROPE) — Benefits from transparency mandate and public accountability requirements that necessitated disclosure. Experiences the constraint as coordination: fulfilling FOIA obligations and rule-of-law commitments, even when the information creates diplomatic friction. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.08. Negative effective extraction = alignment with institutional rules.
constraint_indexing:constraint_classification(epstein_espionage_crisis_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PUBLIC EPISTEMIC COMMONS (SNARE) — Trapped by unverified, uncontextualized, and legally ambiguous claims in mass disclosure. Cannot evaluate competing narratives; cannot distinguish between confirmed intelligence, speculation, and prosecutorial theory. Lacks exit mechanism; bears cost of epistemic pollution. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.78.
constraint_indexing:constraint_classification(epstein_espionage_crisis_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: UK PARLIAMENT AND PRESS (TANGLED ROPE) — Organized actors with investigative capacity and platform. Benefit from access to raw disclosure and ability to drive accountability narratives. Constrained by diplomatic considerations and legal exposure. Classification reflects both coordination (enabling scrutiny) and extraction (pressure on government). d≈0.62, f(d)≈0.85, σ=1.0 → χ≈0.46.
constraint_indexing:constraint_classification(epstein_espionage_crisis_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTELLIGENCE COMMUNITY INSTITUTIONAL INERTIA (PITON) — The UK-US intelligence partnership persists as a structural relationship despite acute trust damage. Compartmentalization doctrine, liaison protocols, and Five Eyes framework remain nominally intact but functionally degraded. Theater_ratio reflects that partnership rituals (meetings, exchanges, consultations) continue performatively while substantive trust is eroded. d≈0.15, f(d)≈0.01, σ=1.1 → χ≈0.02.
constraint_indexing:constraint_classification(epstein_espionage_crisis_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational standpoint, the disclosure creates a structural extraction mechanism: a more powerful state (US) unilaterally determines what information about a smaller state's (UK's) counterintelligence operations is made public, without consent or coordination. This is a sovereignty violation at the informational level, independent of the Epstein case itself. The template is extractive.
constraint_indexing:constraint_classification(epstein_espionage_crisis_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epstein_espionage_crisis_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epstein_espionage_crisis_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epstein_espionage_crisis_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epstein_espionage_crisis_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(epstein_espionage_crisis_2026, TR),
    TR >= 0.70.

:- end_tests(epstein_espionage_crisis_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts from UK at multiple levels: loss of compartmentalization, forced disclosure of prior decisions without consent, damage to UK institutional credibility, and vulnerability to secondary exploitation (narrative weaponization, intelligence asymmetry widening). The mechanism is unilateral US action (FOIA disclosure) that UK cannot prevent or control. The extraction increases over time as secondary consequences cascade. Suppression (0.78): Very high. UK counterintelligence cannot suppress the information (already public and distributed globally); cannot prevent secondary speculation; cannot control narrative framing; faces legal and diplomatic constraints on public response; cannot exit the partnership (Five Eyes is structurally sticky). The suppression operates through inability to act, not through overt coercion. Theater ratio (0.65): Moderate-high. UK government response includes necessary performative elements: reassurance statements, partnership-continuity rhetoric, diplomatic consultations that reaffirm partnership commitment while the underlying damage is irreversible. The theater has increased from 0.40 to 0.65 as the government's public management has become more prominent relative to actual remediation (which is impossible).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits dramatic perspectival divergence. The US DOJ sees alignment with rule-of-law and democratic accountability (Rope) — they are fulfilling legal obligations. The UK government sees trap (Snare) — all response paths are politically damaging. The public sees unverified claims entering the epistemic commons without verification (Snare). The intelligence partnership's institutional persistence (Piton) masks the actual erosion of trust (Snare). The analytical observer identifies a structural sovereignty violation — unilateral information extraction by a more powerful state (Snare across all perspectives). The gap reveals a conflict between two legitimate institutional logics: US transparency accountability (FOIA) vs. UK intelligence sovereignty and Five Eyes compartmentalization doctrine. The constraint cannot be resolved at the perspective level; it must be managed at the structural level through new agreements on disclosure of allied intelligence material.
 *
 * DIRECTIONALITY LOGIC:
 *   UK Counterintelligence: Victim + trapped → d≈0.92, f(d)≈1.40. Irreversible extraction — compartmentalization breach cannot be repaired. UK Government: Target + trapped → d≈0.88, f(d)≈1.35. All response paths are damaging; no exit. Public: Victim + trapped → d≈0.95, f(d)≈1.42. Exposed to epistemic pollution with no verification mechanism. US DOJ: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Alignment with institutional rules, not adversarial extraction. UK Parliament: Organized + constrained → d≈0.62, f(d)≈0.85. Both coordinating scrutiny and constrained by diplomatic considerations. Intelligence Community Inertia: Institutional + arbitrage → d≈0.15, f(d)≈0.01. Piton classification from theater gate, not directionality. Analytical Observer: Sees structural sovereignty extraction, independent of participants' intent.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy confusion by distinguishing between the surface-level political crisis (which looks like coordination/partnership management) and the underlying structural extraction (which is unilateral information release by more powerful actor). The Snare classification is correct because: (1) high base extractiveness (0.68) reflects unilateral control and irreversible damage; (2) suppression (0.78) reflects inability to prevent or manage the extraction; (3) multiple victim groups (UK institutions, public epistemic commons) cannot exit or organize effective response; (4) the extraction mechanism persists regardless of all parties' preferences. The US DOJ's Rope perspective (alignment with transparency rules) is not contradicted — both can be true: the US action is aligned with US institutional rules AND extractive to UK sovereignty. The constraint's existence depends on structural asymmetry (unilateral power to disclose), not on whether disclosure was well-intentioned or legally authorized. Mandatrophy is resolved by recognizing that rule-of-law compliance and intelligence partnership extraction are not mutually exclusive — the constraint is the collision between two legitimate institutional logics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epstein_agent_status_verification,
    'What was Epstein''s actual operational relationship to Israeli intelligence? Foreign asset, controlled source, peripheral contact, or prosecutorial speculation?',
    'Declassified intelligence assessments; credible third-party verification (UK, Israeli, or allied service confirmation); court documents distinguishing allegations from intelligence findings',
    'If confirmed asset: UK faces legitimacy crisis for prior knowledge/non-action. If prosecutorial speculation: US faces credibility damage for including unverified claims in public disclosure. If peripheral: threat level assessment was dramatically overstated, undermining both narrative frames.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epstein_agent_status_verification, empirical, 'Verification of Epstein''s actual intelligence relationship').

omega_variable(
    uk_prior_knowledge_scope,
    'How much of the disclosed information was known to UK counterintelligence at the time of Epstein''s death, and what decisions were made on that basis?',
    'Parliamentary inquiry; declassification review; internal UK government disclosure (under confidentiality if necessary) to establish institutional knowledge timeline',
    'If UK knew most details: government faces accountability for non-disclosure to public and to US partners. If UK was largely unaware: government faces credibility damage for appearing incompetent in counterintelligence. Either way, extraction occurs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uk_prior_knowledge_scope, empirical, 'UK counterintelligence prior knowledge of Epstein material').

omega_variable(
    compartmentalization_doctrine_viability,
    'Can compartmentalization (need-to-know) survive bulk disclosure? Has the doctrine become obsolete in the age of digital mass releases?',
    'Historical analysis of compartmentalization failures post-disclosure (WikiLeaks, Snowden, Pentagon Papers); assessment of whether bulk-release-proof security is possible in democratic societies with FOIA obligations',
    'If compartmentalization can survive: intelligence services can adapt, trust damage is recoverable, partnerships remain viable. If doctrine is obsolete: Five Eyes and UK-US intelligence cooperation faces structural pressure; fundamental rethinking of democratic disclosure vs. national security required.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compartmentalization_doctrine_viability, conceptual, 'Viability of compartmentalization doctrine post-bulk-disclosure').

omega_variable(
    us_disclosure_authorization_legitimacy,
    'Did the US DOJ have proper authorization to disclose UK counterintelligence material without UK consent? Did legal review distinguish between US-sourced intelligence and shared intelligence from allies?',
    'FOIA review documents; DOJ legal office memoranda; Five Eyes agreements examination; assessment of whether disclosure complied with intelligence-sharing treaty obligations',
    'If authorization was improper: US liability and duty to inform UK before release. If authorization was proper: UK must accept that democratic transparency rules override intelligence-sharing courtesy; partnership is legally asymmetric. Both outcomes are extractive to UK.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_disclosure_authorization_legitimacy, empirical, 'Legitimacy of US disclosure authorization process').

omega_variable(
    narrative_weaponization_risk,
    'Is the disclosure being weaponized by state or non-state actors to drive UK-US wedge, delegitimize both governments, or reshape intelligence partnerships for advantage?',
    'Intelligence assessment of narrative amplification; identification of coordinated disinformation campaigns; tracking of who benefits from partnership degradation',
    'If weaponization is detected: coordination becomes a secondary constraint (managing information warfare) overlaid on the primary sovereignty crisis. Snare classification remains, but with added complexity layer. If no evidence of weaponization: crisis is ''authentic'' — driven by disclosure alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_weaponization_risk, empirical, 'State or non-state weaponization of disclosure narrative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epstein_espionage_crisis_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epstein_tr_t0, epstein_espionage_crisis_2026, theater_ratio, 0, 0.4).
narrative_ontology:measurement(epstein_tr_t3, epstein_espionage_crisis_2026, theater_ratio, 3, 0.53).
narrative_ontology:measurement(epstein_tr_t6, epstein_espionage_crisis_2026, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(epstein_be_t0, epstein_espionage_crisis_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(epstein_be_t3, epstein_espionage_crisis_2026, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(epstein_be_t6, epstein_espionage_crisis_2026, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epstein_espionage_crisis_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(epstein_espionage_crisis_2026, five_eyes_compartmentalization_doctrine).
narrative_ontology:affects_constraint(epstein_espionage_crisis_2026, uk_us_intelligence_partnership_trust).
narrative_ontology:affects_constraint(epstein_espionage_crisis_2026, democratic_transparency_vs_espionage_secrecy).

% DUAL FORMULATION NOTE:
% This constraint is downstream of both the Epstein espionage case itself (ε variable depending on verification of agent status) and the broader tension between US FOIA obligations and intelligence-sharing partner protections. The 0.68 extractiveness reflects the disclosure mechanism, not the underlying Epstein intelligence status. If Epstein's operational relationship were fully verified as high-level asset, extraction would increase (0.75+); if prosecutorial speculation, extraction dynamics change but remain high (0.65+) due to damage already inflicted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(epstein_espionage_crisis_2026, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
