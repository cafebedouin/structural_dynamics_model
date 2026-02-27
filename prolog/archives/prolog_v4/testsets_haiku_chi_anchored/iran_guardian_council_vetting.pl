% ============================================================================
% CONSTRAINT STORY: iran_guardian_council_vetting
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iran_guardian_council_vetting, []).

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
 *   constraint_id: iran_guardian_council_vetting
 *   human_readable: Iranian Guardian Council's Candidate Vetting System
 *   domain: political/governance
 *
 * SUMMARY:
 *   Iran's Guardian Council (Shura-ye Negahban-e Qanun-e Asasi-ye Jomhuri-ye
 *   Eslami-ye Iran) has authority to vet candidates for the Islamic
 *   Consultative Assembly (Majlis), the Presidency, and the Assembly of
 *   Experts. The vetting system operates through opaque criteria applied by a
 *   12-member council (6 clerics appointed by the Supreme Leader, 6 jurists
 *   elected by parliament). Disqualification is final and non-appealable;
 *   official reasons are often withheld or generic. Approximately 50-90% of
 *   candidates are disqualified in parliamentary elections; nearly all
 *   reformist presidential candidates have been barred since 2005. This
 *   constraint combines features of institutional coordination (preventing
 *   factional instability, ensuring ideological alignment with Islamic
 *   Republic principles) with severe extraction (removing candidates from
 *   ballot despite public support, creating a performative electoral process
 *   where outcome is predetermined). The constraint's extractiveness has
 *   increased over Iran's political history: from 0.35 in 1979 (when multiple
 *   political tendencies competed within the revolutionary framework) to 0.68
 *   by 2009 (when vetting was weaponized to eliminate reformist competition
 *   after disputed 2009 election). Theater ratio has similarly increased as
 *   the formal electoral ritual (campaigns, debates, voting ceremonies) has
 *   become increasingly decoupled from meaningful candidate selection. This
 *   is a diagnostic case for how extraction accumulates over institutional
 *   lifespans through rent-seeking layering onto originally legitimate
 *   coordination functions.
 *
 * KEY AGENTS:
 *   - Supreme Leader: Primary beneficiary (institutional/arbitrage) — uses vetting to maintain ultimate control; experiences system as pure coordination
 *   - Guardian Council Leadership: Institutional beneficiary (institutional/arbitrage) — derives power from gatekeeping role; sees vetting as legitimate ideological filtering
 *   - Hardline Conservative Faction: Secondary beneficiary (organized/constrained) — benefits from exclusion of liberal/secular competitors, but also subject to disqualification if threatening regime stability
 *   - Reform Movement Candidates: Primary victim (powerless/trapped) — barred from ballot despite electoral support; cannot appeal or contest decision
 *   - Reform Movement Base: Secondary victim (moderate/constrained) — locked into voting for pre-vetted slate; systematic exclusion of preferred candidates
 *   - Urban Middle-Class Voters: Tertiary victim (moderate/constrained) — effectively disfranchised through pre-filtering of choice set
 *   - Formal Electoral Process: Institutional system (institutional/constrained) — maintains democratic theater while serving as mechanism of factional control
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional gatekeeping as necessity of theocratic governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iran_guardian_council_vetting, 0.68).
domain_priors:suppression_score(iran_guardian_council_vetting, 0.78).
domain_priors:theater_ratio(iran_guardian_council_vetting, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iran_guardian_council_vetting, extractiveness, 0.68).
narrative_ontology:constraint_metric(iran_guardian_council_vetting, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(iran_guardian_council_vetting, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iran_guardian_council_vetting, snare).
narrative_ontology:human_readable(iran_guardian_council_vetting, "Iranian Guardian Council's Candidate Vetting System").
narrative_ontology:topic_domain(iran_guardian_council_vetting, "political/governance").

domain_priors:requires_active_enforcement(iran_guardian_council_vetting).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iran_guardian_council_vetting, supreme_leader_faction).
narrative_ontology:constraint_beneficiary(iran_guardian_council_vetting, guardian_council_institutional_power).
narrative_ontology:constraint_victim(iran_guardian_council_vetting, reform_movement_candidates).
narrative_ontology:constraint_victim(iran_guardian_council_vetting, popular_sovereignty).
narrative_ontology:constraint_victim(iran_guardian_council_vetting, political_competition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISQUALIFIED REFORMIST CANDIDATE (SNARE) — Cannot exit the electoral system; vetting decision is opaque, non-appealable, and carries severe career consequences. Extraction is maximal: removal from ballot despite public support. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.96.
constraint_indexing:constraint_classification(iran_guardian_council_vetting, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM MOVEMENT BASE (SNARE) — Constrained by residence and national identity; cannot migrate to alternative political system. Faces systematic exclusion of preferred candidates. Suppression includes post-vetting detention, harassment of supporters, media blackout. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.78.
constraint_indexing:constraint_classification(iran_guardian_council_vetting, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: URBAN MIDDLE-CLASS VOTER (SNARE) — Constrained to the Iranian political arena; technically can vote but choice set is pre-filtered. Theater ratio reflects performative 'choice' between pre-vetted candidates. d≈0.80, f(d)≈1.12, σ=1.0 → χ≈0.76.
constraint_indexing:constraint_classification(iran_guardian_council_vetting, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: GUARDIAN COUNCIL LEADERSHIP (ROPE) — Beneficiary of vetting system. Experiences constraint as coordination mechanism: maintaining institutional power through candidate filtration prevents destabilizing factional competition. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.08. Negative effective extraction = pure beneficiary.
constraint_indexing:constraint_classification(iran_guardian_council_vetting, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SUPREME LEADER'S FACTION (ROPE) — Primary institutional beneficiary. Vetting system enables control of electoral outcome while maintaining democratic theater. No coercion perceived — sees system as legitimate gatekeeping of ideological compatibility. d≈0.00, f(d)≈-0.20, σ=1.0 → χ≈-0.14. Net beneficiary; experiences constraint as pure coordination.
constraint_indexing:constraint_classification(iran_guardian_council_vetting, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: FORMAL ELECTORAL PROCESS (PITON) — The vetting system's institutional logic has degraded: originally intended as ideological filtering for Islamic Republic principles, now functions as factional power consolidation. theater_ratio=0.65 indicates significant performative content (public voting ceremonies, candidate debates, election night broadcasts maintain democratic theater despite pre-determined outcomes). Vetting enforcement is maintained through institutional inertia and because no consensus exists on alternatives. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.42.
constraint_indexing:constraint_classification(iran_guardian_council_vetting, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: HARDLINE CONSERVATIVE FACTION (TANGLED ROPE) — Organized institutional actor that benefits from vetting system (prevents liberal/secular candidates) but is also partially subject to it (high-profile hardliners also face disqualification if perceived as threatening regime stability). Mixed position: both beneficiary and constrained victim. d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.33.
constraint_indexing:constraint_classification(iran_guardian_council_vetting, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / INSTITUTIONAL NECESSITY VIEW (MOUNTAIN) — Claims that candidate vetting by guardian bodies is inherent to theocratic systems; vetting reflects a logical necessity of regime survival rather than extractive choice. However, the structural data (ε=0.68, suppression=0.78, theater=0.65) contradicts the mountain classification — the engine will compute this as a false summit, revealing that regime survival arguments naturalize what is contingent institutional design.
constraint_indexing:constraint_classification(iran_guardian_council_vetting, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iran_guardian_council_vetting_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iran_guardian_council_vetting, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iran_guardian_council_vetting, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iran_guardian_council_vetting, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(iran_guardian_council_vetting, TR),
    TR >= 0.70.

:- end_tests(iran_guardian_council_vetting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The vetting system captures substantial political rent through three mechanisms: (1) removal of competitive threats to entrenched factions, (2) creation of obligation among pre-vetted candidates to the actors who secured their candidacy, (3) preservation of the Guardian Council's institutional power in perpetuity. The extraction is not maximal (0.68 rather than 0.85+) because the system partially functions as coordination — preventing factional collapse is a real function, even if the primary driver is power concentration. Suppression (0.78): High. Barriers include: (a) opacity of vetting criteria, (b) non-appealability of decisions, (c) post-disqualification harassment and detention of prominent reformists, (d) media blackout on disqualified candidates, (e) state control of broadcasting prevents alternative candidates from reaching voters. Theater ratio (0.65): Moderate-high. Electoral ceremonies (public voting, candidate debates broadcast on state TV, election night celebrations) maintain the appearance of meaningful choice despite predetermined outcomes. The ratio has increased from 0.40 in 1979 because early post-revolutionary elections involved genuine factional competition, while later elections are increasingly performative. The trajectory shows not degradation from mountain to piton, but accumulation of rent-extraction onto an originally functional coordination mechanism — the Goodhart drift where performance metrics (elections held, voting participation) become decoupled from function (candidates with genuine public support gaining office).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The disqualified candidate sees pure extraction (Snare) — removal from ballot despite public support, with no coordination benefit. The reform movement base sees systematic suppression and fake choice (Snare). The urban voter sees performative democracy (Snare trending toward Piton). The Guardian Council leadership sees pure coordination (Rope) — maintaining institutional integrity through ideological vetting. The Supreme Leader's faction sees necessary regime preservation (Rope). The hardline conservatives see beneficial exclusion of liberal competitors but also recognize they are constrained by the same system (Tangled Rope). The formal electoral process, seen longitudinally, is a Piton — the original coordination function (preventing destabilizing factional fission) has been overlaid with extraction (guaranteeing particular factional victory), and the system is maintained through theatrical performance rather than genuine function. The analytical observer risks a false summit (Mountain) by claiming vetting is inherent to theocracy, but the structural data contradicts this: theocratic systems exist without Guardian Council equivalents, and the specific vetting architecture is a contingent design choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Disqualified candidates: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction — candidates have no exit option and bear full cost. Reform movement base: Victim + constrained → d≈0.85, f(d)≈1.15. High extraction; cannot migrate, cannot shift to alternative political system, forced to choose from pre-vetted slate. Urban middle-class voter: Victim + constrained → d≈0.80, f(d)≈1.12. Extraction via pre-filtered choice despite nominal democratic participation. Guardian Council: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiary; can exit (interpret criteria differently) but chooses not to; experiences constraint as pure power coordination. Supreme Leader: Beneficiary + arbitrage → d≈0.00, f(d)≈-0.20. Maximum beneficiary position; vetting system directly serves supreme leader's ultimate authority preservation. Hardline conservatives: Mixed + constrained → d≈0.45, f(d)≈0.48. Beneficiaries in practice (competitors excluded) but also constrained by same system (potential disqualification if viewed as threatening); partial victims of the mechanism they benefit from.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the vetting system is NOT a pure coordination mechanism despite institutional claims that it prevents factional instability. The coordination function (preventing regime-destabilizing fission) is real but secondary to the extraction function (ensuring particular factional outcomes). The system classifies as SNARE from the perspective of any actor excluded or constrained by vetting, which is the vast majority of political participants. The ROPE perspectives (Guardian Council leadership, Supreme Leader) mischaracterize the system as pure coordination because they are maximal beneficiaries and cannot perceive extraction that benefits them. The TANGLED_ROPE perspective (hardline conservatives) is the only institutional actor with genuine mixed exposure — they benefit from competitor exclusion but face potential disqualification themselves. The PITON perspective (electoral theater) correctly identifies that the original post-revolutionary coordination function (managing competition among revolutionary factions) has been supplanted by extraction (guaranteeing factional dominance) while maintaining institutional theater. The analytical observer's MOUNTAIN perspective (vetting is inherent to theocracy) is false — it naturalizes what is a contingent institutional design. The proper classification IS SNARE: the system extracts political participation rights from a substantial population (disqualified candidates and their supporters), suppression is high, and the coordination function is secondary to factional power consolidation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vetting_transparency_threshold,
    'Would making vetting criteria explicit and appealable transform this from Snare to Tangled Rope, or is opacity a core feature?',
    'Comparative analysis of other guardian systems (Tunisia''s Islamic Bloc, Egypt''s Islamic Legitimacy Commission); examination of whether transparency + appeals would reduce extraction or merely make it visible',
    'If transparency would preserve control: vetting system is defensible hybrid (Tangled Rope). If transparency would require substantive reform: opacity is intentional extraction, confirming Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vetting_transparency_threshold, empirical, 'Whether vetting transparency would change classification').

omega_variable(
    alternative_regime_legitimacy,
    'Is the vetting system necessary for the Islamic Republic''s institutional survival, or is it a contingent rent-extraction mechanism layered onto a coherent political system?',
    'Historical counterfactual analysis: what would happen to regime stability under different vetting rules? Comparison with other theocratic systems that use different gatekeeping mechanisms.',
    'If necessary for survival: vetting approaches Mountain (immutable constraint of theocratic logic). If contingent: vetting is pure Snare (extractive mechanism with no coordination function).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_regime_legitimacy, conceptual, 'Whether vetting is necessary for regime survival').

omega_variable(
    factional_veto_power,
    'Does the vetting system primarily serve supreme leader control, or do hardline conservative factions exercise genuine veto power over reformist candidates?',
    'Analysis of disqualified candidate patterns: are hardliners protected from vetting? Do hardline candidates experience disqualification? Examination of council voting records and factional composition over time.',
    'If supreme leader has unilateral control: Snare from all victim perspectives. If hardliners have genuine veto: Tangled Rope for hardline faction (mixed beneficiary-victim), Snare for reformists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(factional_veto_power, empirical, 'Whether vetting distributes power or concentrates it').

omega_variable(
    electoral_coalescence_function,
    'Does vetting prevent the regime''s actual nightmare scenario (fragmentation into uncontrollable factions), or is this rationalization of power concentration?',
    'Comparison of electoral volatility in vetted vs non-vetted Iranian elections (pre-1979); analysis of factional coherence under different vetting intensities; counterfactual modeling of unvetted competition.',
    'If vetting genuinely prevents regime collapse: defensive Tangled Rope (coordination function is real, even if asymmetric). If power concentration is the primary function: pure Snare with false legitimation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_coalescence_function, empirical, 'Whether vetting serves essential coordination function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iran_guardian_council_vetting, 1979, 2009).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iran_gc_tr_t1979, iran_guardian_council_vetting, theater_ratio, 1979, 0.4).
narrative_ontology:measurement(iran_gc_tr_t1999, iran_guardian_council_vetting, theater_ratio, 1999, 0.55).
narrative_ontology:measurement(iran_gc_tr_t2009, iran_guardian_council_vetting, theater_ratio, 2009, 0.65).

% Extraction over time
narrative_ontology:measurement(iran_gc_be_t1979, iran_guardian_council_vetting, base_extractiveness, 1979, 0.35).
narrative_ontology:measurement(iran_gc_be_t1999, iran_guardian_council_vetting, base_extractiveness, 1999, 0.52).
narrative_ontology:measurement(iran_gc_be_t2009, iran_guardian_council_vetting, base_extractiveness, 2009, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iran_guardian_council_vetting, enforcement_mechanism).
narrative_ontology:affects_constraint(iran_guardian_council_vetting, iranian_assembly_of_experts_vetting).
narrative_ontology:affects_constraint(iran_guardian_council_vetting, iranian_presidential_candidacy).
narrative_ontology:affects_constraint(iran_guardian_council_vetting, factional_factional_elite_competition).

% DUAL FORMULATION NOTE:
% The Guardian Council vetting system is downstream of Iran's theocratic governance structure but represents a distinct constraint with its own evolution. Upstream constraints (constitutional role of Supreme Leader, Islamic Republic founding principles) define the institutional context; the vetting system is a specific mechanism within that context. This story's ε=0.68 reflects the actual extraction measured through candidate disqualification rates and electoral competitiveness, not abstract theocratic requirements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(iran_guardian_council_vetting, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
