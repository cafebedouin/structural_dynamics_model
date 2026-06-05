% ============================================================================
% CONSTRAINT STORY: victorian_class_stratification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_victorian_class_stratification, []).

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
 *   constraint_id: victorian_class_stratification
 *   human_readable: Victorian Class Stratification and Social Mobility Control
 *   domain: social/economic/political
 *
 * SUMMARY:
 *   Victorian class stratification represents one of history's most
 *   comprehensive constraint systems, embedding extraction across legal,
 *   economic, social, and ideological domains. The constraint operates
 *   through multiple reinforcing mechanisms: legal restrictions on property
 *   ownership, political representation, and movement; economic barriers
 *   (education costs, wage suppression, lack of capital access); social
 *   enforcement (marriage markets, club membership, honor systems); and
 *   ideological naturalization (claims that class difference is genetic,
 *   moral, or inevitable). This constraint exhibits all six DR types from
 *   different structural positions, making it a diagnostic exemplar for how
 *   stratification systems achieve legitimacy while maintaining severe
 *   extraction. The theater ratio (0.65) reflects the elaborate ceremonial
 *   apparatus (debutante balls, honors system, titled nobility) that
 *   increasingly performs class distinction in late Victorian period even as
 *   actual power distribution shifts toward industrial capital and
 *   professional bureaucracy. The extractiveness (0.58) is moderate rather
 *   than maximal because the system does provide genuine coordination
 *   functions (property rights, administrative hierarchy, professional
 *   credentialing) alongside extraction, though the proportion of
 *   coordination to extraction is contested (omega variable).
 *
 * KEY AGENTS:
 *   - Working Class and Agricultural Laborers: Primary victims (powerless/trapped) — face economic dependency, legal restrictions, education barriers, and systematic suppression. No meaningful exit options within biographical time horizon.
 *   - Urban Poor: Primary victims (powerless/trapped) — lack property rights, face criminalization of poverty, dependent on parish relief, subjected to poorhouse system.
 *   - Aspiring Middle Class: Secondary victims (moderate/constrained) — face gatekeeping through education access and social club membership; some genuine advancement possible at generational time scale.
 *   - Aristocracy and Landed Gentry: Primary beneficiaries (institutional/arbitrage) — extract wealth through agricultural rent, land value, inherited privilege. Coordination function: maintain estates and property systems.
 *   - Industrial Capitalists: Secondary beneficiaries (powerful/constrained) — benefit from suppressed wages and stable labor supply; constrained by mercantilist regulations and social gatekeeping from direct political power.
 *   - Professional Middle Class (Law, Medicine, Civil Service): Secondary beneficiaries (institutional/constrained) — benefit from credentialing gatekeeping and social distinction; constrained by aristocratic priority in highest offices.
 *   - Imperial Administration System: Institutional actor (institutional/arbitrage) — maintains the formal apparatus of class hierarchy; increasingly performative in late Victorian period.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent human nature or biological inevitability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(victorian_class_stratification, 0.58).
domain_priors:suppression_score(victorian_class_stratification, 0.72).
domain_priors:theater_ratio(victorian_class_stratification, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(victorian_class_stratification, extractiveness, 0.58).
narrative_ontology:constraint_metric(victorian_class_stratification, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(victorian_class_stratification, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(victorian_class_stratification, tangled_rope).
narrative_ontology:human_readable(victorian_class_stratification, "Victorian Class Stratification and Social Mobility Control").
narrative_ontology:topic_domain(victorian_class_stratification, "social/economic/political").

domain_priors:requires_active_enforcement(victorian_class_stratification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(victorian_class_stratification, aristocracy_and_landed_gentry).
narrative_ontology:constraint_beneficiary(victorian_class_stratification, industrial_capitalists).
narrative_ontology:constraint_beneficiary(victorian_class_stratification, professional_middle_class).
narrative_ontology:constraint_victim(victorian_class_stratification, working_class).
narrative_ontology:constraint_victim(victorian_class_stratification, agricultural_laborers).
narrative_ontology:constraint_victim(victorian_class_stratification, urban_poor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WORKING CLASS LABORER (SNARE) — Trapped by economic dependency, legal restrictions on movement, lack of education access, and social enforcement mechanisms. Exit from class position is nearly impossible within a biographical horizon. Maximum extraction: forced labor at subsistence wages, no representation in governance, no meaningful access to justice or property rights.
constraint_indexing:constraint_classification(victorian_class_stratification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ASPIRING MIDDLE CLASS (TANGLED ROPE) — Faces significant barriers to upward mobility (education costs, social gatekeeping through clubs and credentials), yet the system does provide coordination benefits through standardized professions (law, medicine, commerce) and some genuine social advancement through demonstrated capability. Extraction exists (artificial scarcity of professional positions, social policing), but coordination function is real (credentialing systems do select for competence).
constraint_indexing:constraint_classification(victorian_class_stratification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ARISTOCRATIC BENEFICIARY (ROPE) — Experiences the constraint as pure coordination: the class system organizes society, enables property transfer, coordinates military and administrative hierarchies, and secures wealth accumulation. Extraction runs entirely toward this agent. The system solves their coordination problem (maintaining estates, securing heirs, managing the social order) with minimal coercive overhead from their perspective — the subordinate classes bear suppression; the beneficiary experiences coordination.
constraint_indexing:constraint_classification(victorian_class_stratification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INDUSTRIAL CAPITALIST (TANGLED ROPE) — Benefits from class stratification (stable labor supply, suppressed wages, no union power). Also constrained by the system: must maintain social propriety, marry within acceptable circles, follow mercantilist regulations, navigate aristocratic gatekeeping of political power. High extractiveness relative to workers, but also trapped in a set of social expectations and legal constraints. Genuine coordination function: the class system enables factory labor organization and property rights enforcement.
constraint_indexing:constraint_classification(victorian_class_stratification, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: IMPERIAL ADMINISTRATION SYSTEM (PITON) — By late Victoria, the formal class system is increasingly performative. Actual power has shifted to industrial capital and bureaucratic institutions, but the theater of class hierarchy (titled nobility, debutante balls, honor systems) persists through institutional inertia. The system maintains itself through ceremonial validation rather than structural necessity. Theater ratio reflects the growing gap between formal class distinctions and actual wealth/power distribution.
constraint_indexing:constraint_classification(victorian_class_stratification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — Risk of misclassifying the constraint as a natural law ('human societies naturally stratify,' 'hierarchy is inevitable'). The analytical observer from a distance may see class stratification as an inherent feature of human social organization rather than a contingent institutional mechanism that could be reformed or dismantled. The engine will flag this as a false summit, revealing that naturalization of the constraint serves the beneficiaries' interests.
constraint_indexing:constraint_classification(victorian_class_stratification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(victorian_class_stratification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(victorian_class_stratification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(victorian_class_stratification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(victorian_class_stratification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(victorian_class_stratification, TR),
    TR >= 0.70.

:- end_tests(victorian_class_stratification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value from working classes through wage suppression, property restriction, education denial, and legal disadvantage. However, the measurement reflects declining extractiveness from 0.62 to 0.58 over the Victorian period, suggesting that rising challenges (labor organization, suffrage demands, alternative ideologies) are eroding the system's extraction efficiency. This is consistent with a constraint in transition. If extractiveness had remained stable or increased, a Snare classification would be more appropriate. The decline suggests the system is beginning to lose structural grip. Suppression (0.72): High. Multiple reinforcing suppression mechanisms: economic (wages insufficient for capital accumulation), legal (vagrancy laws, property restrictions, no political voice), social (marriage market gatekeeping, club exclusion, reputational damage for upward mobility), and ideological (education system teaches class inferiority, religious doctrines naturalize hierarchy). Suppression does not decline materially over the interval — if anything, it intensifies as challenges grow (police expansion, workhouse reform that increases confinement, educational segregation). Theater ratio (0.65): Moderate-high and increasing. Early Victorian period sees more functional class system (genuine need for rural hierarchy, factory labor organization, administrative structure). Late Victorian period shows increasing ceremonial elaboration (debutante balls become more elaborate, honors system expands, titled nobility becomes less economically powerful but more socially conspicuous) even as underlying power distribution shifts to industrial capital. The theater represents defensive maintenance of legitimacy against rising challenges.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates near-maximal perspectival divergence. This reveals the core structural truth of stratification: the same mechanism produces radically different experienced constraints depending on structural position. The working class cannot see the 'coordination' function that the aristocracy experiences — they see pure suppression. The aristocracy cannot see their own extraction mechanism because they experience only the coordination benefits. The analytical observer risks collapsing this gap by naturalizing the system as inevitable. The Tangled Rope classification (claimed type) is justified because the constraint genuinely has both a coordination function (organizing labor, securing property rights, managing succession) AND asymmetric extraction (suppressing wages, restricting access, concentrating wealth). But which dominates depends entirely on where the observer sits.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim status and exit options. Working class (victim + trapped) derives d ≈ 0.95 → high f(d) → high χ. Aristocracy (beneficiary + arbitrage) derives d ≈ 0.05 → low f(d) → negative χ. Middle class aspiring upward (both + constrained) derives d ≈ 0.50 → moderate f(d). Industrial capitalist (beneficiary + constrained) derives d ≈ 0.40 → low-moderate f(d). The directionality values reflect actual power flow in the system: extraction runs from powerless-trapped toward institutional-arbitrage. Scope modifier σ(national) = 1.0, so no additional scaling beyond the core f(d) calculation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the six classifications are not competing theories but perspectival readings of a stratified system. The mandatrophy is not 'which type is correct?' but 'which position are you measuring from?' The analytical observer's mountain (class hierarchy is natural law) is a false summit — the empirical data show it is maintained through active enforcement (requires_active_enforcement: true). The aristocracy's rope is their genuine experience — they solve coordination problems with the system. The working class's snare is their structural reality — they are trapped with extraction and no exit. The tangled rope perspectives (middle class, industrial capitalist) capture the hybrid experience of those positioned ambiguously. The piton reflects the system's degradation in late Victoria — theater increasing even as functional necessity declines. The mandatrophy is resolved by recognizing that all six readings coexist in the same historical moment, revealing different aspects of a complex constraint. Mislabeling the system as purely 'natural law' (mountain) would naturalize what is actually enforced extraction. Mislabeling it as pure coordination (rope) would erase the suppression and victimization. The Tangled Rope classification forces recognition of both functions simultaneously, preventing ideological collapse into either pure naturalization or pure victimhood narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genetic_vs_institutional_hierarchy,
    'To what extent does Victorian class stratification reflect genuine differences in ability/merit versus institutional barriers that create the appearance of merit-based hierarchy?',
    'Empirical comparison of mobility rates within same socioeconomic cohort; analysis of educational access and achievement gaps; cross-generational outcome tracking controlling for family advantage',
    'If institutional barriers dominate: constraint is pure Snare (extraction without coordination). If genuine ability sorting exists: constraint shifts toward Tangled Rope (extraction + coordination). Current assumption: heavily institutional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genetic_vs_institutional_hierarchy, empirical, 'Genetic vs institutional determinants of hierarchy').

omega_variable(
    coordination_function_necessity,
    'Do the coordination functions attributed to class stratification (labor organization, property rights, administrative hierarchy) actually require class suppression, or could they be achieved through alternative mechanisms?',
    'Comparative analysis of non-stratified or lower-stratification societies achieving similar coordination outcomes; examination of alternative coordination mechanisms (cooperative labor, collective property, meritocratic administration)',
    'If alternative coordination exists: current Tangled Rope classifications shift toward Snare (coordination function was spurious). If stratification is necessary: Tangled Rope classifications confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_necessity, conceptual, 'Whether class hierarchy is necessary for coordination').

omega_variable(
    suppression_internalization_mechanism,
    'Is the measured suppression (0.72) primarily structural (legal/economic barriers to exit) or internalized (working class accepts class ideology as natural/deserved)?',
    'Historical analysis of working class consciousness; examination of strike/rebellion patterns; analysis of ideological narratives in labor vs elite media; post-constraint suppression trajectory (do barriers persist if ideology is challenged?)',
    'If primarily internalized: effective suppression is higher than structural measure suggests — constraint persists through cognitive capture. If structural: removing legal barriers would enable exit. Current assumption: substantial internalization (identity_locked dynamics at generational time).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Structural vs internalized suppression mechanisms').

omega_variable(
    theater_ratio_acceleration,
    'Why does theater ratio increase from 0.55 (early Victorian) to 0.65 (late Victorian)? Does this reflect degradation of class function or intensification of performative gatekeeping?',
    'Analysis of ceremonial elaboration (growth of debutante balls, honors system, social clubs); comparison of formal class barriers vs actual wealth/power distribution over the period; examination of who benefits from performative maintenance',
    'If degradation: constraint is shifting toward Piton (inertial maintenance). If intensification: constraint is evolving defensive theater to maintain control against rising challenges (labor movement, suffrage demands). Affects piton classification confidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_acceleration, empirical, 'Driver of increasing theater ratio in late Victorian period').

omega_variable(
    identity_lock_escape_mechanisms,
    'What percentage of working-class individuals who achieve educational/professional advancement experience liberation from class identity versus internalized internalization that persists as ''imposter syndrome''?',
    'Autobiographical analysis of upward mobility narratives; comparison of psychological integration versus persistent identity bifurcation; examination of whether mobility is followed by ideological shift or maintained working-class consciousness',
    'If most experience persistent identity lock: exit is psychological barrier not structural. If identity shifts freely: exit barriers are primarily structural. Affects classification of middle-class perspectives at longer time horizons.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_escape_mechanisms, empirical, 'Identity lock persistence through social mobility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(victorian_class_stratification, 0, 64).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vict_tr_t0, victorian_class_stratification, theater_ratio, 0, 0.55).
narrative_ontology:measurement(vict_tr_t32, victorian_class_stratification, theater_ratio, 32, 0.6).
narrative_ontology:measurement(vict_tr_t64, victorian_class_stratification, theater_ratio, 64, 0.65).

% Extraction over time
narrative_ontology:measurement(vict_be_t0, victorian_class_stratification, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(vict_be_t32, victorian_class_stratification, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(vict_be_t64, victorian_class_stratification, base_extractiveness, 64, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(victorian_class_stratification, resource_allocation).
narrative_ontology:affects_constraint(victorian_class_stratification, victorian_marriage_market_gatekeeping).
narrative_ontology:affects_constraint(victorian_class_stratification, industrial_labor_extraction).
narrative_ontology:affects_constraint(victorian_class_stratification, imperial_governance_hierarchy).

% DUAL FORMULATION NOTE:
% Victorian class stratification decomposes into multiple structurally distinct constraints with different ε values: agricultural rent extraction (ε≈0.65, Snare), industrial wage suppression (ε≈0.58, Tangled Rope), educational gatekeeping (ε≈0.50, Tangled Rope), marriage market control (ε≈0.55, Tangled Rope), and imperial administration hierarchy (ε≈0.40, Rope). These are linked via network.affects_constraints because they share the same ideological legitimation (naturalization of hierarchy) and reinforce each other's suppression mechanisms. The base story models the unified system; the network decomposition tracks specific extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(victorian_class_stratification, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
