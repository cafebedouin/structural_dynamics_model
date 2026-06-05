% ============================================================================
% CONSTRAINT STORY: trump_epa_greenhouse_gas_reversal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trump_epa_greenhouse_gas_reversal, []).

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
 *   constraint_id: trump_epa_greenhouse_gas_reversal
 *   human_readable: Trump EPA Reversal of Greenhouse Gas Endangerment Finding
 *   domain: political/regulatory_capture
 *
 * SUMMARY:
 *   The Trump EPA's 2020 reversal of the 2009 endangerment finding that
 *   greenhouse gases threaten public health represents a regulatory
 *   extraction mechanism disguised as scientific reconsideration. The 2009
 *   finding created the foundation for emissions regulations under the Clean
 *   Air Act; the reversal removes that foundation without addressing the
 *   underlying atmospheric physics. The constraint exhibits snare
 *   characteristics: high extraction (ε=0.68), extreme suppression
 *   (suppression=0.72 through defunding research, intimidating career
 *   scientists, prioritizing industry testimony, and blocking publication of
 *   government research), and increasing theater (theater_ratio rising from
 *   0.32 to 0.58 as the reversal is dressed in pseudoscientific language
 *   while resting on political decision-making). The primary beneficiaries
 *   are fossil fuel industries and high-emission manufacturers, who gain
 *   deregulatory arbitrage and delay of compliance costs. The primary victims
 *   are future generations, climate-vulnerable populations with no exit
 *   option, and the atmospheric commons itself — all trapped actors with no
 *   alternative jurisdiction. The suppression is severe: career scientists at
 *   the EPA face intimidation and censorship; research contradicting the
 *   reversal is deprioritized; peer-review gatekeeping is weaponized to
 *   exclude inconvenient findings; and the agency's own scientific advisory
 *   boards are reshuffled to remove climate expertise. Theater has increased
 *   because the reversal requires constant performative justification — press
 *   releases, 'science summits' with invited skeptics, and manufactured
 *   controversy to sustain the appearance that the endangerment finding was
 *   scientifically disputed, when the reversal is fundamentally a political
 *   choice made by an administration aligned with extractive industry
 *   interests.
 *
 * KEY AGENTS:
 *   - Fossil Fuel Industry and Aligned Manufacturers: Primary beneficiary (institutional/arbitrage) — captures deregulatory gains, reduced compliance costs, extended profitable operation of carbon-intensive infrastructure
 *   - Future Generations and Climate-Vulnerable Populations: Primary victim (powerless/trapped) — bear full cost of accelerated atmospheric accumulation; cannot exit; no alternative atmosphere
 *   - Career EPA Scientists and Environmental Researchers: Secondary victim (moderate/constrained) — face intimidation, censorship, defunding, career risk if research contradicts administration priorities
 *   - Environmental and Public Health Coalition: Organized victim (organized/constrained) — face lobbying barriers, regulatory defunding, judicial reversals, and media disinformation campaigns despite organizational capacity
 *   - EPA Institutional Apparatus: Degraded machinery (institutional/constrained) — reveals that regulatory legitimacy is contingent on executive alignment; the 2009 finding was scaffolding resting on political will rather than structural entrenchment
 *   - State-Level Governments: Mixed experience (powerful/mobile) — face coordination burden maintaining higher state-level standards while federal baseline is removed; experience both coordination and extraction
 *   - Analytical Observer: Risks naturalizing contingency as physical law (analytical/analytical) — the atmospheric constraint is real, but regulatory reversal is political, not scientific
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trump_epa_greenhouse_gas_reversal, 0.68).
domain_priors:suppression_score(trump_epa_greenhouse_gas_reversal, 0.72).
domain_priors:theater_ratio(trump_epa_greenhouse_gas_reversal, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trump_epa_greenhouse_gas_reversal, extractiveness, 0.68).
narrative_ontology:constraint_metric(trump_epa_greenhouse_gas_reversal, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(trump_epa_greenhouse_gas_reversal, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trump_epa_greenhouse_gas_reversal, snare).
narrative_ontology:human_readable(trump_epa_greenhouse_gas_reversal, "Trump EPA Reversal of Greenhouse Gas Endangerment Finding").
narrative_ontology:topic_domain(trump_epa_greenhouse_gas_reversal, "political/regulatory_capture").

domain_priors:requires_active_enforcement(trump_epa_greenhouse_gas_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trump_epa_greenhouse_gas_reversal, fossil_fuel_industry).
narrative_ontology:constraint_beneficiary(trump_epa_greenhouse_gas_reversal, high_emission_manufacturers).
narrative_ontology:constraint_victim(trump_epa_greenhouse_gas_reversal, atmospheric_commons).
narrative_ontology:constraint_victim(trump_epa_greenhouse_gas_reversal, future_generations).
narrative_ontology:constraint_victim(trump_epa_greenhouse_gas_reversal, climate_vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE VICTIMS (SNARE) — Cannot exit the atmospheric commons or the regulatory capture mechanism. Bears full cost of delayed climate action and policy reversal. No exit option, no alternative jurisdiction. d≈0.96, f(d)≈1.41, σ=1.2 → χ≈0.78.
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: WORKERS IN FOSSIL FUEL SECTORS (SNARE) — Trapped by employment and geographic specificity. Benefits from short-term job protection but constrained in long-term exit to alternative sectors due to skill specificity and regional economic dependencies. d≈0.78, f(d)≈1.09, σ=1.0 → χ≈0.74.
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FOSSIL FUEL BENEFICIARIES (ROPE) — Experiences the reversal as coordination: the EPA finding reversal removes regulatory constraint and coordinates industry expectation. High exit options (regulatory arbitrage between states, or exit to less regulated jurisdictions). d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.08. Net beneficiary experiencing constraint relief.
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ENVIRONMENTAL & PUBLIC HEALTH ADVOCATES (SNARE) — Organized actors facing systematic suppression: lobbying barriers, defunding of regulatory enforcement, media disinformation campaigns, and repeated judicial reversals of environmental rules. High suppression despite organized power. d≈0.65, f(d)≈0.98, σ=1.2 → χ≈0.61.
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: EPA INSTITUTIONAL APPARATUS (PITON) — The EPA itself operates as degraded constraint machinery: the 2009 endangerment finding created regulatory scaffolding, but the 2020 reversal revealed that the finding rested on political will rather than structural entrenchment. The agency's authority is contingent and performative — regulatory legitimacy depends on executive alignment. Theater ratio increased as the reversal was dressed in scientific language while resting on political logic. d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.20.
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: STATE GOVERNMENTS & DECENTRALIZED REGULATORS (TANGLED ROPE) — Powerful but mobile actors that experience both coordination and extraction. Federal reversal forces states to choose between complying with federal policy or maintaining higher state-level standards (CA emission rules, etc.). Experience is mixed: coordination burden (state-level implementation) combined with extraction (loss of federal baseline protection and regulatory coordination). d≈0.52, f(d)≈0.68, σ=1.0 → χ≈0.45.
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a purely physical/chemical perspective, anthropogenic greenhouse gas accumulation creates atmospheric radiative forcing independent of regulatory status. The constraint (atmospheric CO2→radiative imbalance) appears as a physical law that reversal cannot eliminate. However, the structural data (ε=0.68, suppression=0.72, theater=0.58, requires_active_enforcement=true) contradicts mountain classification — this is a false summit. The analytical observer risks naturalizing a contingent regulatory and political choice as a law of nature.
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trump_epa_greenhouse_gas_reversal_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trump_epa_greenhouse_gas_reversal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trump_epa_greenhouse_gas_reversal, TR),
    TR >= 0.70.

:- end_tests(trump_epa_greenhouse_gas_reversal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): Very high. The reversal extracts deregulatory rents from fossil fuel producers by removing the administrative foundation for emissions regulations. The 2009 endangerment finding required emissions rules under Clean Air Act §202; removing it eliminates that legal trigger without changing atmospheric physics. The extraction is not total (0.68 rather than 0.85) because state-level regulations remain and international treaty pressure creates some offsetting constraint. The measurement trajectory (0.35→0.52→0.68) reflects escalating extraction as the reversal was consolidated: initial partial rollback, then accelerated deregulation, then hardening into policy framework. Suppression (0.72): Extreme. The reversal mechanism required systematic suppression: career scientist intimidation, budget cuts to climate research programs, removal of climate scientists from advisory boards, censorship of government research findings, creation of 'red team' theater to manufacture scientific doubt, and prioritization of industry testimony over peer-reviewed literature. Suppression is not absolute (0.72 rather than 0.90) because external scientific consensus remains intact, state-level enforcement persists, and some career scientists escaped via outside institutions. Theater ratio (0.58, rising from 0.32): Moderate-high and increasing. The reversal required constant performative justification: scientific-sounding restatements of a political choice, manufactured controversy through red teams, creation of appearance that endangerment finding was disputed when it was never scientifically contested (97%+ consensus), and theatrical 'regulatory streamlining' framed as efficiency rather than extraction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. The fossil fuel beneficiary sees rope (constraint relief and coordination of industry expectations). The EPA institutional apparatus sees piton (degraded machinery where legitimacy is contingent). State governments see tangled rope (mixed coordination burden and extraction of federal baseline). Climate victims and future generations see pure snare (trapped, costly, unremediable through their own action). Environmental advocates see snare with organized power trying to resist (high d, high f(d), despite organizational capacity). The analytical observer risks seeing mountain (atmospheric physics is law) but the structural data (requires_active_enforcement=true, beneficiaries clearly identified, suppression explicit) reveal this as false summit — it is a contingent political choice, not a physical law.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil fuel industry: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. Climate victims: Victim + trapped → d≈0.96, f(d)≈1.41. Maximum extraction — no exit, global scope. EPA workers: Victim + constrained → d≈0.78, f(d)≈1.09. High extraction but not maximal — some can leave for other agencies. Environmental coalition: Victim + organized/constrained → d≈0.65, f(d)≈0.98. Significant extraction despite organizational capacity because suppression is severe and exit options are limited (political system itself is the constraint mechanism). State governments: Both benefit (some competitive advantage vs federal baseline) and bear costs (coordination burden) → d≈0.52, f(d)≈0.68. Mixed experience, moderate extraction. EPA machinery: Institutional/constrained → d≈0.35, f(d)≈0.30. Low effective extraction from the EPA's perspective because the reversal reveals the agency's degraded status — it executes politics, not law.
 *
 * MANDATROPHY ANALYSIS:
 *   The reversal resolves apparent mandatrophy by revealing the snare classification as primary. Initial temptation: Is this a rope (regulatory coordination) or a snare (extractive capture)? The 2009 finding appeared coordinating (establishing baseline emissions rules for all actors). The reversal appears as extraction (removing that baseline to benefit one sector). The mandatrophy dissolves when recognizing that the reversal is not coordinating any legitimate collective action problem — it is pure removal of a constraint on profitable extraction. The rope (2009 finding) was legitimate coordination: establishing science-based emissions standards. The snare (reversal) is extractive: removing those standards without addressing atmospheric physics, purely to shift rents to fossil fuel producers. This is not a single constraint viewed from two angles; it is two different constraints: the original-finding-as-coordination (rope) and the reversal-as-extraction (snare). The reversal story is snare at χ≈0.74 (effective extraction after accounting for state-level regulation and international coupling). Mandatrophy resolved: the classification is unambiguous once the structural data are declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scientific_consensus_enforcement,
    'Can regulatory reversal actually suppress the underlying scientific consensus on climate causation, or does suppression apply only to enforcement mechanisms?',
    'Track citation patterns in climate science, government agency research funding, international scientific body statements, and peer-review gatekeeping before/after reversal',
    'If suppression extends to scientific consensus: snare classification is stable across multiple knowledge communities. If suppression is enforcement-only: the scientific consensus remains independent, constraining future policy reversals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scientific_consensus_enforcement, empirical, 'Whether regulatory reversal suppresses scientific consensus or only enforcement').

omega_variable(
    regulatory_arbitrage_stability,
    'Does the federal reversal create a stable arbitrage opportunity for industry, or is the decentralized state-level regulatory landscape (CA, NY, etc.) sufficiently fragmented to limit extractive gains?',
    'Economic analysis of industry relocation patterns, compliance cost differentials across state-level regimes, and capital flight to unregulated or less-regulated jurisdictions',
    'If stable arbitrage: snare extraction is high and durable (χ≈0.68 is justified). If fragmented/unstable: arbitrage gains are partial, and effective extraction is lower than base ε=0.68.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_stability, empirical, 'Stability of industry regulatory arbitrage post-reversal').

omega_variable(
    reversal_permanence,
    'Is the reversal structurally permanent (requires new congressional legislation to reverse again) or is it contingent on executive continuation (reversible by next administration)?',
    'Analysis of Administrative Procedure Act (APA) pathway dependencies; tracking of legal challenges and injunction history; assessment of precedent stickiness in appellate courts',
    'If permanent: victims face indefinite suppression (snare confirmed at high confidence). If contingent: suppression is temporary and reversal-risk reduces effective extraction (scaffold or piton classification becomes more plausible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversal_permanence, conceptual, 'Whether reversal is structurally permanent or executive-contingent').

omega_variable(
    international_treaty_coupling,
    'How tightly does the EPA reversal couple to international climate agreements (Paris, UNFCCC), and do international treaty obligations create countervailing constraints?',
    'Treaty compliance audits, trade negotiation analysis, and assessment of whether climate commitments are priced into supply-chain negotiations',
    'If tightly coupled: international pressure creates an offsetting constraint (tangled rope becomes more accurate than snare). If loosely coupled: EPA reversal achieves near-total extraction decoupling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_treaty_coupling, empirical, 'International treaty coupling to EPA reversal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trump_epa_greenhouse_gas_reversal, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epa_ghg_tr_t0, trump_epa_greenhouse_gas_reversal, theater_ratio, 0, 0.32).
narrative_ontology:measurement(epa_ghg_tr_t5, trump_epa_greenhouse_gas_reversal, theater_ratio, 5, 0.45).
narrative_ontology:measurement(epa_ghg_tr_t10, trump_epa_greenhouse_gas_reversal, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(epa_ghg_be_t0, trump_epa_greenhouse_gas_reversal, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(epa_ghg_be_t5, trump_epa_greenhouse_gas_reversal, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(epa_ghg_be_t10, trump_epa_greenhouse_gas_reversal, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trump_epa_greenhouse_gas_reversal, enforcement_mechanism).
narrative_ontology:affects_constraint(trump_epa_greenhouse_gas_reversal, paris_agreement_commitment).
narrative_ontology:affects_constraint(trump_epa_greenhouse_gas_reversal, state_level_emissions_regulation).
narrative_ontology:affects_constraint(trump_epa_greenhouse_gas_reversal, renewable_energy_investment_incentives).

% DUAL FORMULATION NOTE:
% The EPA reversal is downstream of the 2009 endangerment finding (which constituted a rope constraint establishing science-based coordination). The reversal represents a snare constraint that extracts value from the victims of the reversal by removing the protective framework. These are structurally distinct: the finding (ε≈0.05, rope) established coordination; the reversal (ε≈0.68, snare) extracts through removal. The network link documents that the reversal's extraction mechanism depends on undoing the prior coordination constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trump_epa_greenhouse_gas_reversal, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
