% ============================================================================
% CONSTRAINT STORY: the_wall_procedural_barrier
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_the_wall_procedural_barrier, []).

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
 *   constraint_id: the_wall_procedural_barrier
 *   human_readable: The Intelligence/Law Enforcement Information Sharing Barrier ("The Wall")
 *   domain: legal/institutional
 *
 * SUMMARY:
 *   The Wall refers to a set of administrative procedures and attorney
 *   general guidelines restricting information sharing between intelligence
 *   agencies (CIA, NSA, FBI intelligence division) and law enforcement
 *   prosecutors (DOJ, FBI criminal division) that were formalized in the 1995
 *   DOJ memorandum and remained in effect through September 2001. The
 *   constraint was designed to prevent prosecutorial weaponization of
 *   classified intelligence and protect intelligence sources and methods from
 *   criminal discovery obligations. However, the wall created information
 *   silos that prevented law enforcement from detecting coordinated terrorism
 *   plots. The constraint embodies a genuine civil liberties protection
 *   (preventing intelligence weaponization against protected speech and
 *   defendants' rights) simultaneously with a security extraction (preventing
 *   detection of terrorism coordination). This makes it a canonical Tangled
 *   Rope: both the coordination function and the asymmetric extraction are
 *   structurally real, not an error in framing. The 9/11 Commission found
 *   that information held separately by the FBI's intelligence division and
 *   criminal division — combined with CIA intelligence on the same
 *   individuals — could have revealed the plot, but the wall procedures
 *   prevented those databases from being queried together. This suggests the
 *   wall's extraction cost on public safety was substantial. Simultaneously,
 *   the civil liberties protection was real: post-wall, concerns about
 *   prosecutorial abuse of intelligence were partially vindicated in cases
 *   like the COINTELPRO-adjacent tracking of domestic protest groups.
 *
 * KEY AGENTS:
 *   - Civil Liberties Advocates: Primary beneficiary (organized/constrained) — protected from intelligence weaponization in prosecution; also constrained by inability to access intelligence summaries in their own litigation
 *   - Intelligence Community: Primary beneficiary (institutional/arbitrage) — protected source identities and operational methods from prosecutorial discovery
 *   - Criminal Investigators: Primary victim (moderate/constrained) — unable to access relevant intelligence data; case development hindered; terrorism detection capacity reduced
 *   - Law Enforcement Prosecutors: Secondary victim (institutional/constrained) — forbidden from accessing intelligence summaries; discovery obligations limited by wall rules
 *   - Potential Terrorism Victims: Secondary victim (powerless/trapped) — unaware of threat interconnections; lack information-enabled security. Cannot exit or access protective information.
 *   - DOJ Bureaucracy: Institutional actor (institutional/arbitrage) — maintains wall procedures through administrative inertia; wall is largely theatrical after 9/11 policy reversals
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees wall as real civil-security tradeoff, not as natural law or unambiguous error
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(the_wall_procedural_barrier, 0.52).
domain_priors:suppression_score(the_wall_procedural_barrier, 0.68).
domain_priors:theater_ratio(the_wall_procedural_barrier, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(the_wall_procedural_barrier, extractiveness, 0.52).
narrative_ontology:constraint_metric(the_wall_procedural_barrier, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(the_wall_procedural_barrier, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(the_wall_procedural_barrier, tangled_rope).
narrative_ontology:human_readable(the_wall_procedural_barrier, "The Intelligence/Law Enforcement Information Sharing Barrier (\"The Wall\")").
narrative_ontology:topic_domain(the_wall_procedural_barrier, "legal/institutional").

domain_priors:requires_active_enforcement(the_wall_procedural_barrier).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(the_wall_procedural_barrier, civil_liberties_advocates).
narrative_ontology:constraint_beneficiary(the_wall_procedural_barrier, intelligence_source_protection).
narrative_ontology:constraint_victim(the_wall_procedural_barrier, criminal_investigators).
narrative_ontology:constraint_victim(the_wall_procedural_barrier, counterterrorism_operations).
narrative_ontology:constraint_victim(the_wall_procedural_barrier, terrorism_prevention_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POTENTIAL TERRORISM VICTIM (SNARE) — Cannot exit the information silo that prevents law enforcement from detecting coordinated plots. Lacks awareness of threat interconnections. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73. The constraint extracts from the general public through constrained security.
constraint_indexing:constraint_classification(the_wall_procedural_barrier, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CRIMINAL INVESTIGATOR (SNARE) — Structurally trapped by wall procedures. Cannot request intelligence agency data even when investigating terrorism-related financial crimes or conspiracies. Career advancement blocked; cases weakened. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.60. High suppression: institutional rules prohibit information requests; no alternative career pathway avoids the constraint.
constraint_indexing:constraint_classification(the_wall_procedural_barrier, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CIVIL LIBERTIES COALITION (TANGLED ROPE) — Benefits from wall as protection against intelligence agency over-reach into criminal prosecution; also constrained by it (cannot access intelligence summaries even for constitutional challenges). Coordination function: prevents chilling effect on protected political speech by separating intelligence surveillance from prosecutorial use. Asymmetric extraction via suppression: must maintain visibility restrictions even when those restrictions harm their own litigation. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.34.
constraint_indexing:constraint_classification(the_wall_procedural_barrier, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INTELLIGENCE COMMUNITY (ROPE) — Beneficiary through source protection and operational security. Wall separates classified intelligence from prosecutorial discovery rules. Experiences constraint as pure coordination: protecting source identities and methods from criminal discovery. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Negative effective extraction = institutional beneficiary.
constraint_indexing:constraint_classification(the_wall_procedural_barrier, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DOJ BUREAUCRATIC IMPLEMENTATION (PITON) — The wall procedures (Attorney General Guidelines, administrative walls, CIPA procedures) are largely theatrical: designed to appear to enforce separation while permitting functional intelligence-prosecution coordination through back-channels (national security grounds, foreign intelligence exception, attorney general waiver). theater_ratio=0.58 reflects substantial procedural theater masking substantive access. The official wall is maintained through institutional inertia despite acknowledged functional inadequacy post-9/11. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(the_wall_procedural_barrier, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CIVIL-SECURITY TRADEOFF VIEW (TANGLED ROPE) — From a civilizational perspective, the wall embodies a genuine coordination problem: how to enable counterterrorism while preventing prosecutorial weaponization of intelligence. Both functions are legitimate; neither can be fully optimized without harming the other. Analyzed as a tradeoff constraint rather than a natural law. Benefits civil liberties communities; extracts from counterterrorism capacity. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.39. The constraint is enduring but not immutable — policy choices about what constitutes appropriate oversight reshape it.
constraint_indexing:constraint_classification(the_wall_procedural_barrier, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(the_wall_procedural_barrier_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(the_wall_procedural_barrier, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(the_wall_procedural_barrier, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(the_wall_procedural_barrier, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(the_wall_procedural_barrier, TR),
    TR >= 0.70.

:- end_tests(the_wall_procedural_barrier_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The wall's security extraction is substantial — the 9/11 Commission documented specific intelligence-criminal investigative data silos. However, extractiveness is not maximal (not 0.70+) because post-wall analysis shows that information sharing alone would not have prevented 9/11 (other intelligence sharing, policy, and operational failures were critical). The extraction cost is real but not total. Suppression (0.68): Moderately high. Institutional rules actively prohibit information requests across the wall. Alternative pathways (national security letters, attorney general waivers) exist but are administratively difficult and require high-level authorization. Career incentives do not punish investigators who don't try to access intelligence, so suppression is not maximum (not 0.85+). Theater ratio (0.58): Moderate. The wall is substantial procedure but includes functionally-operative back-channels (national security grounds exception, attorney general waivers, certain CIPA procedures). The official wall is maintained for civil liberties signaling while intelligence-prosecution coordination occurs through legitimate exceptions. Theater has risen over the interval as acknowledged inadequacy of the wall post-9/11 becomes manifest, yet the wall is not immediately dismantled — bureaucratic and civil liberties inertia maintain it despite functional erosion.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The intelligence community sees the wall as a legitimate source protection mechanism (Rope). Criminal investigators see pure extraction blocking necessary information (Snare). Civil liberties advocates see a real but insufficient protection against intelligence weaponization (Tangled Rope — both protected and constrained). The DOJ bureaucracy sees its own procedures as largely performative theater (Piton). Potential terrorism victims see invisible threat that could have been detected (Snare — trapped, with extraction cost in lives). The analytical observer recognizes a genuine civil-security tradeoff (Tangled Rope) rather than a natural law or unambiguous institutional failure. This perspectival divergence is not due to observer bias but to genuine structural asymmetry: the wall truly does protect civil liberties (benefiting organized agents), truly does extract security capacity (harming powerless potential victims), and truly is maintained through bureaucratic inertia (piton features).
 *
 * DIRECTIONALITY LOGIC:
 *   Civil liberties advocates: Beneficiary + constrained → d≈0.45, f(d)≈0.55. Protected from intelligence weaponization but cannot access intelligence for their own litigation. Moderate beneficiary, some extraction. Intelligence community: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary through source protection. Criminal investigators: Victim + constrained → d≈0.85, f(d)≈1.15. High extraction; cannot access needed data but must work around wall. Potential terrorism victims: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction — unaware of threats, no exit option. DOJ bureaucracy: Institutional + arbitrage → d≈0.12, f(d)≈-0.02. Piton classification from theater ratio gate, not from high chi. Analytical observer: analytical → d≈0.55, f(d)≈0.75. Tradeoff classification reflects genuine dual function.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves the mandatrophy by showing that the wall is structurally a Tangled Rope: it genuinely provides civil liberties coordination (prevents prosecutorial weaponization of intelligence) while simultaneously extracting from security capacity (prevents threat detection). Both functions are real. The mandatrophy error would be to classify it as pure Snare (mislabeling the real civil liberties protection as theater) or as pure Rope (ignoring the real security extraction). Post-9/11, policy discourse attempted to resolve the mandatrophy by arguing the wall was a pure Snare (bureaucratic obstruction), justifying its removal. But the civil liberties protection was real — the post-wall period (2001-2008) saw expansion of intelligence-prosecution cooperation and documented concerns about prosecutorial overreach in terrorism cases. The correct framing is: the wall was a legitimate Tangled Rope protecting civil liberties at the cost of security capacity. Removing it required accepting higher risk of prosecutorial abuse, not merely removing bureaucratic obstacle. This fidelity to the dual structure prevents the false choice between 'pure security enhancement' vs 'bureaucratic obstruction.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    source_protection_necessity,
    'How much information sharing would compromise intelligence source protection and operational methods?',
    'Declassified post-action reviews of specific cases; comparison of actual source compromises in post-wall vs wall period; analysis of how prosecutorial discovery rules affect classified information',
    'If minimal compromise: wall may be overbuilt extraction mechanism (Snare classification confirmed). If significant: wall is legitimate coordination protection (Rope classification confirmed). Current empirical status contested post-9/11.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(source_protection_necessity, empirical, 'How much information sharing would compromise intelligence sources').

omega_variable(
    prosecutorial_weaponization_risk,
    'Would prosecutors routinely weaponize intelligence summaries to justify criminal charges, creating prosecutorial overreach?',
    'Post-wall case analysis (2001-2008): frequency of charges filed based on questionable intelligence foundation; comparison with pre-wall prosecutorial patterns; civil rights litigation outcomes',
    'If weaponization frequent: wall is legitimate civil liberties protection (Tangled Rope with real asymmetric extraction). If rare: wall over-constrains legitimate counterterrorism coordination (pure Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prosecutorial_weaponization_risk, empirical, 'Whether prosecutors would abuse intelligence information').

omega_variable(
    information_silos_threat_detection,
    'How much of the 9/11 plot coordination could have been detected with wall removal?',
    '9/11 Commission Report analysis of specific intelligence vs criminal investigative data held separately; reconstruction of detection pathways under pre-wall vs wall procedures; comparison with post-wall terrorism prevention effectiveness',
    'If substantial coordination detectable: extraction cost on public safety is high (Snare classification confirmed). If minimal: wall''s security cost is overestimated (Rope or Piton classification suggested).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_silos_threat_detection, empirical, 'Whether wall procedures blocked viable threat detection').

omega_variable(
    alternative_coordination_mechanisms,
    'Do modified back-channel procedures (national security letters, attorney general waivers, CIPA procedures) provide sufficient information flow without full wall removal?',
    'Post-9/11 usage data on alternative information sharing methods; effectiveness comparison; civil liberties impact of alternative channels',
    'If alternatives sufficient: the wall itself becomes optional (Piton classification — maintained through inertia). If alternatives inadequate: wall removal is necessary (Snare classification confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_mechanisms, empirical, 'Whether alternative mechanisms provide adequate information flow').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(the_wall_procedural_barrier, 1995, 2001).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wall_tr_t0, the_wall_procedural_barrier, theater_ratio, 0, 0.42).
narrative_ontology:measurement(wall_tr_t3, the_wall_procedural_barrier, theater_ratio, 3, 0.5).
narrative_ontology:measurement(wall_tr_t6, the_wall_procedural_barrier, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(wall_be_t0, the_wall_procedural_barrier, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(wall_be_t3, the_wall_procedural_barrier, base_extractiveness, 3, 0.43).
narrative_ontology:measurement(wall_be_t6, the_wall_procedural_barrier, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(the_wall_procedural_barrier, enforcement_mechanism).
narrative_ontology:affects_constraint(the_wall_procedural_barrier, fisa_surveillance_expansion).
narrative_ontology:affects_constraint(the_wall_procedural_barrier, prosecutorial_discovery_rules_terrorism).

% DUAL FORMULATION NOTE:
% The wall decomposed into two structurally distinct constraints: (1) the civil liberties protection against prosecutorial weaponization of intelligence (ε≈0.20, pure coordination, Rope), and (2) the information silo preventing threat detection (ε≈0.60, pure extraction, Snare). The wall as a single institutional structure embodied both — removal of the wall resolved one at the cost of the other. The post-wall environment trades Rope protection for reduced Snare extraction, but increases prosecutorial overreach risks (captured in separate constraint on prosecutorial discretion in terrorism cases).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(the_wall_procedural_barrier, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
