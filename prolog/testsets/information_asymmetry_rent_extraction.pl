% ============================================================================
% CONSTRAINT STORY: information_asymmetry_rent_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_information_asymmetry_rent_extraction, []).

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
 *   constraint_id: information_asymmetry_rent_extraction
 *   human_readable: Information Asymmetry Rent Extraction
 *   domain: economic/information/structural
 *
 * SUMMARY:
 *   Information asymmetry rent extraction occurs when one party possesses
 *   material information that another party lacks, and uses that information
 *   advantage to capture surplus value in transactions, negotiations, or
 *   market interactions. This is a structural constraint present in all
 *   economic systems with distributed knowledge, but the degree and
 *   enforceability of asymmetry varies dramatically across institutional
 *   contexts. In some domains (financial markets with strong transparency
 *   mandates, consumer markets with warranty requirements, labor markets with
 *   hiring disclosure rules), the asymmetry is actively compressed. In others
 *   (used car sales, professional services pricing, insider trading in weak
 *   regulatory regimes), the asymmetry is actively preserved or exploited.
 *   The constraint exhibits all six classification types from different
 *   perspectives, revealing that the boundary between 'natural information
 *   scarcity' and 'policy-enforced extraction' is contestable. The
 *   extractiveness trajectory (0.42 → 0.58) reflects that absent regulatory
 *   intervention, informed agents progressively extract more rents as they
 *   develop more sophisticated information production and concealment
 *   techniques. Theater ratio increases (0.35 → 0.48) as disclosure
 *   compliance grows without corresponding information transparency — the
 *   performative layer (compliance documents, disclaimer statements,
 *   regulatory signoffs) expands while actual information access stagnates.
 *
 * KEY AGENTS:
 *   - Informed Agents: Primary beneficiary (institutional/arbitrage) — possess actionable information asymmetry and profit from selective use and concealment; face no structural barriers to exit from exploitation
 *   - Uninformed Agents: Primary victim (powerless/trapped) — lack material information; face transaction costs, search costs, and bounded rationality that prevent full closure of asymmetry; cannot exit without bearing costs
 *   - Market Participants with Partial Information: Secondary agent (moderate/constrained) — aware of some information gaps but not their full scope; face costs to acquire complete information or to exit markets
 *   - Regulatory Coalition: Organized actors (organized/constrained) — transparency mandates, disclosure requirements, consumer protection rules; aim to compress asymmetry through sunset-clause institutional design
 *   - Professional Gatekeepers: Institutional actors (institutional/arbitrage) — licensing, credentialing, professional standards that ostensibly reduce asymmetry but often preserve information scarcity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent policy choices (what asymmetry is enforced/allowed) as inherent limits of knowledge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(information_asymmetry_rent_extraction, 0.58).
domain_priors:suppression_score(information_asymmetry_rent_extraction, 0.65).
domain_priors:theater_ratio(information_asymmetry_rent_extraction, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(information_asymmetry_rent_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(information_asymmetry_rent_extraction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(information_asymmetry_rent_extraction, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(information_asymmetry_rent_extraction, snare).
narrative_ontology:human_readable(information_asymmetry_rent_extraction, "Information Asymmetry Rent Extraction").
narrative_ontology:topic_domain(information_asymmetry_rent_extraction, "economic/information/structural").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(information_asymmetry_rent_extraction, informed_agents).
narrative_ontology:constraint_victim(information_asymmetry_rent_extraction, uninformed_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINFORMED AGENT (SNARE) — Structurally trapped in a market, transaction, or negotiation where counterparty possesses material information they will not disclose. Cannot exit without bearing transaction costs or foregone opportunity. Suppression is structural: information barriers prevent even awareness of their own disadvantage. High effective extraction as the informed party extracts rents through selective disclosure.
constraint_indexing:constraint_classification(information_asymmetry_rent_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INFORMED AGENT (ROPE) — Possesses actionable information advantage and exercises it for material benefit. Experiences the constraint as coordination: using private information to select advantageous transactions is their legitimate function. Exit is costless (they can trade on the information or withhold it). From this perspective, the information asymmetry solves a selection problem — their knowledge enables efficient matching.
constraint_indexing:constraint_classification(information_asymmetry_rent_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: MARKET PARTICIPANT WITH PARTIAL INFORMATION (TANGLED ROPE) — Aware of some information asymmetry but not its full extent. Faces high costs to acquire complete information or to exit (transaction costs, search costs, relocation costs). The constraint both coordinates market clearing and extracts from them through information opacity. Neither pure coordination nor pure extraction — mixed extraction with some genuine coordinating function.
constraint_indexing:constraint_classification(information_asymmetry_rent_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY COALITION (SCAFFOLD) — Organized agents (disclosure mandates, transparency requirements, financial regulations, consumer protection rules) aim to temporarily compress information asymmetry through enforcement and sunset-clause logic: as transparency norms and market infrastructure mature, the asymmetry should self-liquidate. Enforcement is active but decreases as voluntary disclosure standards mature. Sees the constraint as a temporary coordination failure being solved.
constraint_indexing:constraint_classification(information_asymmetry_rent_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL INERTIA (PITON) — Information asymmetry is often perpetuated through vestigial institutional arrangements: licensing requirements that restrict information access, professional gatekeeping that creates artificial scarcity of expert knowledge, regulatory compliance theater that obscures rather than clarifies. The constraint persists through institutional momentum rather than functional necessity. Theater ratio reflects that much 'information disclosure' is performative — compliance documents that satisfy legal requirements but don't convey actionable intelligence.
constraint_indexing:constraint_classification(information_asymmetry_rent_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some information asymmetry is inherent to any economic system: no agent can know everything about counterparties, future states, or hidden attributes. Information gaps are an irreducible feature of bounded rationality and distributed knowledge. This perspective risks naturalizing what is actually a policy-contingent degree of asymmetry. The engine will flag this as a false summit if structural data shows the asymmetry is compressible through institutional design.
constraint_indexing:constraint_classification(information_asymmetry_rent_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(information_asymmetry_rent_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(information_asymmetry_rent_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(information_asymmetry_rent_extraction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(information_asymmetry_rent_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(information_asymmetry_rent_extraction, TR),
    TR >= 0.70.

:- end_tests(information_asymmetry_rent_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. Information asymmetry enables rents because uninformed agents make value-destroying decisions they would reverse with full information. The magnitude of extraction is substantial — studies of used car markets, insurance adverse selection, and financial products show informed-uninformed price discrepancies of 15-50%. Suppression is structural (information barriers prevent awareness) but not total (market signals provide partial information). Extractiveness is not at snare maxima (0.70+) because the asymmetry is compressible through institutional design — regulatory intervention demonstrably reduces it. Theater ratio is moderate (0.48) because some information disclosure is genuine (price transparency in financial markets) while much is performative (disclaimer statements). The trajectory shows theater rising faster than true extractiveness, indicating Goodhart drift: as more information is disclosed, uninformed agents may believe the asymmetry is closing while their actual information gap widens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural mechanism (information advantage) classifies as snare, rope, tangled rope, scaffold, piton, and mountain depending on the observer. The uninformed agent sees pure extraction (snare) — they cannot escape the disadvantage and bear its full cost. The informed agent sees coordination (rope) — they are solving a selection problem by using private information efficiently. The moderate agent sees mixed extraction and coordination (tangled rope) — they have some information and some options but face barriers that create extraction. The regulatory coalition sees a temporary problem with a sunset (scaffold) — transparency mandates are meant to compress asymmetry over time. The institutional gatekeepers see a degraded ritual (piton) — disclosure requirements persist through inertia while actual information access stagnates. The civilizational observer risks seeing an immutable feature of bounded rationality (mountain) — but the institutional evidence shows asymmetry is policy-contingent, not natural. The perspectival gap is diagnostic: it reveals that the degree of information asymmetry is not determined by technology (what information is knowable) but by institutional choice (what information is compressible, transmitted, or protected).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is determined by each agent's structural position relative to the information advantage. Informed agents with arbitrage-level exit (can use or withhold information at will) derive low d (~0.15). Uninformed agents with trapped exit (cannot access information regardless of effort/cost) derive high d (~0.95). Moderate agents with constrained exit (can acquire some information but at high cost) derive mid-range d (~0.65). The sigmoid f(d) maps these to experienced extractiveness multipliers. The pipeline computes chi = ε × f(d) × σ(S). For the uninformed agent at (powerless, trapped, global scope): d ≈ 0.95, f(d) ≈ 1.42, σ(global) = 1.2, yielding χ ≈ 0.58 × 1.42 × 1.2 ≈ 0.99 — essentially pure extraction. For the informed agent at (institutional, arbitrage): d ≈ 0.15, f(d) ≈ -0.01, σ(global) = 1.2, yielding χ ≈ 0.58 × (-0.01) × 1.2 ≈ -0.01 — negative effective extraction (the constraint benefits this agent). The perspectival gap reveals that one agent's snare is another's rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing how information asymmetry is neither pure coordination nor pure extraction, but a hybrid constrained by institutional design. The snare classification from the uninformed perspective is correct — they experience pure extraction. The rope classification from the informed perspective is also correct — they experience coordination benefit. The mandatrophy dissolves when we recognize that BOTH perspectives are measuring the same structural fact: whether an institutional actor (the informed agent) is locked into extraction or can costlessly refrain. If regulatory pressure or reputational incentives constrain informed agents to disclose information, snare becomes tangled rope or scaffold. If informed agents face no barriers to exploiting asymmetry, snare is the correct classification for uninformed victims. The false mountain (from the civilizational observer) is particularly important to reject — treating information asymmetry as a natural law prevents recognition that institutional design choices (transparency mandates, disclosure requirements, professional licensing reforms) directly compress it. The constraint is genuinely policy-contingent rather than natural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    asymmetry_irreducibility_threshold,
    'What portion of observed information asymmetry is inherent to bounded rationality vs. policy-enforced scarcity?',
    'Comparative analysis across regulatory regimes: countries with strong transparency mandates vs. minimal disclosure requirements; correlation between regulation stringency and measured asymmetry compression',
    'If policy-enforced scarcity dominates: mountain classification fails (reveals snare). If inherent asymmetry is dominant: mountain classification confirmed (natural structural feature). Distribution of the gap determines whether the constraint is contingent or natural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asymmetry_irreducibility_threshold, empirical, 'Degree to which information asymmetry is policy-enforced vs inherent').

omega_variable(
    disclosure_quality_vs_compliance,
    'Does mandatory disclosure mandate actually reduce information asymmetry or merely create performative compliance without changing actual information access?',
    'Pre/post regulatory analysis: information asymmetry measures (bid-ask spreads, pricing discrepancies, transaction costs) before and after major transparency rules; analysis of whether disclosed information correlates with price discovery',
    'If effective: regulatory constraints are rope (coordination). If performative: disclosure theater is piton (degraded institution). Determines whether scaffold sunset logic is real or aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclosure_quality_vs_compliance, empirical, 'Whether disclosure mandates reduce actual asymmetry or create theater').

omega_variable(
    informed_agent_exit_costlessness,
    'Can informed agents costlessly exit from exploiting information asymmetry or are they locked into extraction by institutional structure?',
    'Market data: frequency of informed agent voluntary full disclosure; correlation between information advantage and participation in market; analysis of whether informed agents face consequences for non-exploitation',
    'If exit is costless: rope classification valid (arbitrage-level exit). If exit is constrained by institutional pressure to exploit: extraction is more coercive than rope (snare component strengthens). Determines whether beneficiary actually experiences rope or is partially trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_agent_exit_costlessness, empirical, 'Whether informed agents can costlessly refrain from exploiting asymmetry').

omega_variable(
    suppression_internalization,
    'Do uninformed agents accept the asymmetry as natural/inevitable or do they experience it as illegitimate extraction they are unable to resist?',
    'Qualitative analysis: survey data on perceived fairness/inevitability; correlation between belief in asymmetry inevitability and willingness to engage in information disclosure transparency; analysis of social movements demanding transparency',
    'If internalized as natural: suppression includes cognitive component (identity_locked exit for some uninformed agents). If perceived as illegitimate: suppression is purely structural (trapped/constrained exit). Affects whether snare is experienced as coercive or as ''just how markets work''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether uninformed agents internalize asymmetry as natural').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(information_asymmetry_rent_extraction, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iare_tr_t0, information_asymmetry_rent_extraction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(iare_tr_t3, information_asymmetry_rent_extraction, theater_ratio, 3, 0.42).
narrative_ontology:measurement(iare_tr_t6, information_asymmetry_rent_extraction, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(iare_be_t0, information_asymmetry_rent_extraction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(iare_be_t3, information_asymmetry_rent_extraction, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(iare_be_t6, information_asymmetry_rent_extraction, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(information_asymmetry_rent_extraction, information_standard).
narrative_ontology:affects_constraint(information_asymmetry_rent_extraction, adverse_selection).
narrative_ontology:affects_constraint(information_asymmetry_rent_extraction, moral_hazard).
narrative_ontology:affects_constraint(information_asymmetry_rent_extraction, market_failure_information_goods).
narrative_ontology:affects_constraint(information_asymmetry_rent_extraction, regulatory_arbitrage).

% DUAL FORMULATION NOTE:
% Information asymmetry rent extraction is upstream of multiple specific extractive mechanisms: adverse selection in insurance/credit markets, moral hazard in principal-agent relationships, market failures in information goods pricing. Each downstream constraint has its own ε reflecting domain-specific institutional arrangements. The upstream constraint is the general structural mechanism; downstream constraints are particular instantiations with different policy levers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
