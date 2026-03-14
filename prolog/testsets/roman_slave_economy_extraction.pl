% ============================================================================
% CONSTRAINT STORY: roman_slave_economy_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_slave_economy_extraction, []).

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
 *   constraint_id: roman_slave_economy_extraction
 *   human_readable: Roman Slave Economy Extraction System
 *   domain: economic/political/social
 *
 * SUMMARY:
 *   The Roman slave economy extraction system represents one of history's
 *   most durable and extractive institutional arrangements, operating across
 *   8+ centuries and multiple imperial phases. The constraint exhibits
 *   extreme extractiveness (0.88) and suppression (0.92), driven by legal
 *   frameworks that denied enslaved persons personhood, property rights, and
 *   mobility. The system extracted value from enslaved labor at near-totality
 *   rates — the enslaved person's productive output belonged to the owner,
 *   with subsistence provision returned at minimal levels. Suppression was
 *   extraordinarily comprehensive: legal prohibition of resistance, brutal
 *   enforcement via torture/execution/branding, denial of family bonds, and
 *   control of reproduction. Theater ratio remains low (0.35) because the
 *   extraction mechanism was structurally transparent and unambiguous —
 *   slavery did not require performative justification in the early-to-middle
 *   imperial periods; it was openly acknowledged as an economic mechanism.
 *   The constraint's perspectival structure reveals why it was so durable:
 *   the beneficiary class (aristocracy, military administration, merchant
 *   class) experienced it as pure coordination (Rope) while the victimized
 *   class experienced it as pure extraction (Snare). This perspectival gap
 *   meant that reform pressure came only from internal ethical/philosophical
 *   developments (late Stoicism, Christianity) rather than from structural
 *   incentive misalignment.
 *
 * KEY AGENTS:
 *   - Enslaved Persons: Primary victim (powerless/trapped) — maximum structural extraction, zero legal recourse, complete suppression
 *   - Free Lower Classes: Secondary victim (powerless/constrained) — wage depression, labor monopoly by slaves, limited exit options
 *   - Landholding Aristocracy: Primary beneficiary (institutional/arbitrage) — capital accumulation via enslaved labor, arbitrage capacity unused due to profitability
 *   - Military Administration: Secondary beneficiary (institutional/arbitrage) — resource mobilization via slave taxation and labor, expansion of slave supply via conquest
 *   - Conquered Populations: Tertiary victim (moderate/constrained) — vulnerable to enslavement through military defeat
 *   - Organized Resistance (Slave Revolts): Oppressed agent achieving coalition power (organized/constrained) — Spartacus and other organized revolts demonstrate power shift when enslaved persons coordinate
 *   - Reformer Class (Late Imperial): Philosophical opposition (powerful/mobile) — Stoic and Christian critique created alternative framing enabling transition to feudalism
 *   - Analytical Observer: Risk of false natural law framing (analytical/analytical) — danger of naturalizing contingent extraction as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_slave_economy_extraction, 0.88).
domain_priors:suppression_score(roman_slave_economy_extraction, 0.92).
domain_priors:theater_ratio(roman_slave_economy_extraction, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_slave_economy_extraction, extractiveness, 0.88).
narrative_ontology:constraint_metric(roman_slave_economy_extraction, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(roman_slave_economy_extraction, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_slave_economy_extraction, snare).
narrative_ontology:human_readable(roman_slave_economy_extraction, "Roman Slave Economy Extraction System").
narrative_ontology:topic_domain(roman_slave_economy_extraction, "economic/political/social").

domain_priors:requires_active_enforcement(roman_slave_economy_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_slave_economy_extraction, landholding_aristocracy).
narrative_ontology:constraint_beneficiary(roman_slave_economy_extraction, merchant_class).
narrative_ontology:constraint_beneficiary(roman_slave_economy_extraction, military_administration).
narrative_ontology:constraint_victim(roman_slave_economy_extraction, enslaved_persons).
narrative_ontology:constraint_victim(roman_slave_economy_extraction, free_lower_classes).
narrative_ontology:constraint_victim(roman_slave_economy_extraction, conquered_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENSLAVED PERSON (SNARE) — Complete structural entrapment. No legal personhood, no property rights, no mobility. Exit via escape is severely suppressed by pursuit, branding, torture, and execution. Extraction is maximal: labor value is wholly captured by owner, with minimal survival subsistence returned. The constraint's existence depends entirely on suppression of alternatives — slavery persists only through force.
constraint_indexing:constraint_classification(roman_slave_economy_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: FREE LOWER CLASSES (TANGLED ROPE) — Technically free but structurally suppressed. Enslaved labor undercuts wages and eliminates alternative employment. Exit is costly: moving to different occupation/region requires resources; slavery monopolizes agricultural and industrial labor. Some coordination function exists (urban grain distribution requires controlled labor supply), but extraction is asymmetric — free poor subsidize the system through wage depression. Moderate extraction with genuine coordination overhead.
constraint_indexing:constraint_classification(roman_slave_economy_extraction, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: LANDHOLDING ARISTOCRACY (ROPE) — Primary beneficiary (institutional/arbitrage). Experiences the constraint as pure coordination: organizing enslaved labor solves the problem of large-scale agricultural production and capital accumulation. Can exit via manumission or transition to wage labor (arbitrage capacity), but does not because the system is profitable. Net beneficiary — extraction flows toward this agent. Suppression is externalized (enforced by state apparatus), not borne by this agent.
constraint_indexing:constraint_classification(roman_slave_economy_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: MILITARY ADMINISTRATION (ROPE) — Secondary beneficiary (institutional/arbitrage). Enslaved persons provide: military logistics labor, infrastructure construction (roads, aqueducts, fortifications), and revenue via slave taxation. Conquests generate slave supply, creating feedback loop: military expansion justifies enslaved labor policy, which funds military expansion. Experiences constraint as coordination mechanism for imperial resource mobilization. High beneficiary status.
constraint_indexing:constraint_classification(roman_slave_economy_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: CONQUERED POPULATIONS (SNARE) — Structurally vulnerable to enslavement via conquest. Exit options (resistance, migration, treaty negotiation) are constrained by military power asymmetry. High suppression through military occupation and legal prohibition. Extraction occurs at both individual level (those enslaved) and collective level (populations transferred as slave supplies via conquest). Generations-long entrapment.
constraint_indexing:constraint_classification(roman_slave_economy_extraction, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ORGANIZED RESISTANCE (SNARE WITH COALITION POWER) — Slave revolts (Spartacus, Egyptian slave uprisings) demonstrate that organizing transforms the structural dynamic. Organized agents see the constraint as a snare but with coalition power that can inflict costs on beneficiaries. Extraction remains maximal from individual slave perspective, but organized groups shift from powerless to organized power through collective action. High suppression still in place (brutal suppression of revolts), but the classified power level reflects actual structural change when enslaved persons coordinate.
constraint_indexing:constraint_classification(roman_slave_economy_extraction, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: REFORMER CLASS / LATE IMPERIAL (SCAFFOLD) — Philosophical opposition (Stoics, early Christian thought) and economic transitions in later empire created counter-pressure to slavery. Some elites transitioned to feudal labor arrangements (scaffolding toward alternative coordination mechanisms). Theater ratio low — the scaffolding toward alternative systems was explicit, not performative. Sunset clause: slavery gradually replaced by serfdom/feudal obligations over centuries. High exit mobility for agents pioneering alternatives.
constraint_indexing:constraint_classification(roman_slave_economy_extraction, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN?) — Risk of false summit: natural law frame ('slavery is ancient practice inherent to all civilizations, an unchangeable economic law') naturalizes what is a contingent institutional arrangement. Engine's false summit detector should flag this — the base properties show extreme extractiveness (0.88) and suppression (0.92), which contradict mountain's claim of low suppression. The apparent 'naturalness' of Roman slavery is an artifact of historiographical naturalization, not structural immutability.
constraint_indexing:constraint_classification(roman_slave_economy_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_slave_economy_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roman_slave_economy_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_slave_economy_extraction, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(roman_slave_economy_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(roman_slave_economy_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.88): Extreme. The enslaved person's entire productive capacity is extracted, minus minimal subsistence. Calculation: enslaved person generates estimated 5-8 units of economic value daily (agricultural labor, construction, service); receives back ~0.5-1.0 units in food/shelter. Extraction rate = 85-90%. The measurement trajectory shows slight increase over time (0.78 → 0.88) reflecting intensification as slave trade consolidated and productivity demands increased. Suppression (0.92): Extreme. Legal barriers: enslaved persons have no standing in courts, cannot own property, cannot marry legally, cannot accumulate capital. Enforcement barriers: capture/return laws, mutilation (branding), execution for resistance, denial of movement. Psychological barriers: cultural dehumanization, denial of kinship bonds, reproductive control. Only organized coalition reduces suppression slightly, but baseline suppression remains overwhelming. Theater (0.35): Low. Roman slavery did not require performative justification in prime imperial period — extraction was transparent mechanism. Theater increases slightly (0.28 → 0.38) during late imperial period as philosophical opposition emerged and defensive justifications became necessary. The low theater indicates high structural clarity: constraints need theater when beneficiaries fear challenge; slavery's durability meant little theatrical necessity.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival gap between beneficiary and victim. Aristocratic landowner experiences Rope (pure coordination of agricultural production). Enslaved person experiences Snare (pure extraction with maximum suppression). These are not different readings of the same event — they are structurally opposed experiences of the same institutional arrangement. The beneficiary's 'coordination problem' (organizing large-scale production) is precisely the apparatus that creates the victim's 'extraction trap' (complete labor confiscation). This opposition is the hallmark of Snare classification with high chi: the beneficiary experiences genuine coordination value while the victim experiences zero coordination benefit and maximum extraction cost. The perspectival gap also reveals why the constraint persisted: beneficiaries had no incentive to perceive the Snare structure; the Rope experience was complete enough. Reform had to come from outside the direct beneficiary-victim dyad (philosophical opposition from reformer class, threat from organized resistance).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: Enslaved persons face legal prohibition + military enforcement + economic dependency = trapped exit with d ≈ 0.95 (victim + trapped → maximum f(d)). Free lower classes face wage competition + movement barriers + skill requirements = constrained exit with d ≈ 0.70 (partial victim + constrained). Aristocracy faces no legal barriers + capital flexibility + arbitrage capacity = arbitrage exit with d ≈ 0.08 (beneficiary + arbitrage → f(d) near -0.12). Military administration faces institutional integration + state apparatus control = arbitrage exit with d ≈ 0.10. Conquered populations face military power asymmetry + legal disability = constrained exit with d ≈ 0.75. Organized resistance gains coalition power, shifting from d ≈ 0.95 to d ≈ 0.55 when coordination achieved (threat credibility via Spartacus demonstrates d drop). Analytical observer faces risk of naturalizing contingent arrangement, false d ≈ 0.70.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint demonstrates why mandatrophy matters. At extremeness (0.88), the constraint risks classification as 'natural law of civilization' (false Mountain) or 'pure inevitable condition' (false narrative closure). The mandatrophy is resolved by clarifying: (1) slavery was not universal (many contemporaneous non-slave economies existed), (2) slavery required active enforcement (not spontaneous), (3) slavery was deliberately chosen by beneficiary class (not inevitable), (4) slavery was vulnerable to organized resistance (Spartacus, Egyptian revolts demonstrate conditionality), (5) slavery was abolished and replaced by alternatives (feudal labor, wage systems), therefore (6) the extreme extractiveness reflects contingent institutional choice, not natural law. The constraint is Snare precisely because it could have been otherwise — and eventually was. The false natural law frame would claim 'slavery is inherent to ancient civilization' or 'extraction is unchangeable law of nature'; the mandatrophy resolution reveals this as a rationalization serving beneficiary interests. The actual constraint is: contingent extraction maintained by organized suppression, which persists as long as beneficiaries maintain enforcement capacity and resist philosophical/organizing pressure for alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_internalization,
    'What proportion of slave suppression was structural (legal/military) versus internalized (psychological domination, identity capture)?',
    'Historical analysis of resistance patterns, manumission behavior, psychological autonomy in diaries/accounts, suicide rates, self-harm documentation',
    'If primarily structural: suppression persists only while external force applied. If significantly internalized: even after legal abolition, affected populations carry internalized suppression with psychological/social consequences extending beyond constraint''s formal end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Proportion of slave suppression that was structural vs internalized').

omega_variable(
    economic_necessity_vs_rent_seeking,
    'What proportion of Roman slavery''s extractiveness was necessary coordination cost (agricultural production required scale) versus pure rent-seeking (extraction beyond production function)?',
    'Economic modeling comparing slave productivity to free labor productivity; analysis of labor-to-capital substitution rates; comparison with non-slave economies achieving similar agricultural output',
    'If primarily necessary cost: classify as Tangled Rope (coordination + extraction). If primarily rent-seeking: confirm Snare classification. If mixed: distribution determines which classification is primary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_necessity_vs_rent_seeking, empirical, 'Economic necessity vs rent-seeking in Roman slavery').

omega_variable(
    alternative_institutional_paths,
    'Did Roman economic trajectories require slavery, or were alternatives (free wage labor, tenant farming, cooperative production) structurally viable?',
    'Comparative institutional analysis with contemporaneous non-slave economies; counterfactual historical analysis of technological/demographic alternatives; analysis of post-Roman feudal/free labor transitions showing viable alternatives existed',
    'If alternatives were viable: slavery reveals as chosen extraction mechanism rather than necessary coordination. Strengthens Snare classification. If alternatives were structurally impossible: some coordination necessity exists, supporting mixed Tangled Rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_institutional_paths, empirical, 'Viability of alternative institutional arrangements to slavery').

omega_variable(
    slave_manumission_as_systemic_release,
    'Does the manumission system function as a release valve (controlled suppression relief enabling system persistence) or as genuine exit path (reducing extraction over time)?',
    'Historical analysis of manumission rates over time; correlation between manumission rates and slave unrest/rebellion; examination of manumission conditions (debt slavery, term contracts, ritual formality); tracking of freed persons'' actual economic autonomy versus continued dependency',
    'If release valve: manumission enables continued extraction by reducing rebellion risk; functionally increases system durability. If genuine exit: manumission represents scaffold-like transition path; should increase exit options from ''trapped'' to ''mobile'', changing classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(slave_manumission_as_systemic_release, empirical, 'Manumission as release valve versus genuine exit path').

omega_variable(
    identity_lock_in_enslaved_populations,
    'To what extent were enslaved persons'' identities constituted through the constraint itself (identity-locked) versus structurally trapped by external barriers?',
    'Analysis of enslaved persons'' self-concept from available texts (inscriptions, legal documents, accounts); study of post-emancipation identity formation; psychological continuity studies in freed person populations',
    'If significantly identity-locked: suppression persists beyond formal abolition through internalized self-concept; exit requires identity reconstruction. If primarily structurally trapped: suppression is removed with legal abolition and external barrier removal. Distribution determines whether exit_options should be ''trapped'' or ''identity_locked''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_in_enslaved_populations, empirical, 'Identity lock versus structural trapping in enslaved persons').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_slave_economy_extraction, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rse_tr_t0, roman_slave_economy_extraction, theater_ratio, 0, 0.28).
narrative_ontology:measurement(rse_tr_t2, roman_slave_economy_extraction, theater_ratio, 2, 0.31).
narrative_ontology:measurement(rse_tr_t4, roman_slave_economy_extraction, theater_ratio, 4, 0.35).
narrative_ontology:measurement(rse_tr_t6, roman_slave_economy_extraction, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(rse_be_t0, roman_slave_economy_extraction, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(rse_be_t2, roman_slave_economy_extraction, base_extractiveness, 2, 0.82).
narrative_ontology:measurement(rse_be_t4, roman_slave_economy_extraction, base_extractiveness, 4, 0.88).
narrative_ontology:measurement(rse_be_t6, roman_slave_economy_extraction, base_extractiveness, 6, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_slave_economy_extraction, resource_allocation).
narrative_ontology:affects_constraint(roman_slave_economy_extraction, ancient_empire_stability).
narrative_ontology:affects_constraint(roman_slave_economy_extraction, tributary_state_extraction).
narrative_ontology:affects_constraint(roman_slave_economy_extraction, feudal_serfdom_transition).

% DUAL FORMULATION NOTE:
% Roman slavery is upstream of feudal serfdom — the constraint family decomposes into: (1) roman_slave_economy_extraction (ε=0.88, Snare), (2) feudal_serfdom_labor_obligation (ε=0.55, Tangled Rope), (3) free_wage_labor_transition (ε=0.12, Rope/Scaffold). Each story has different extractiveness reflecting institutional evolution. The shift from slavery to feudalism to wage labor represents progressively lower extraction as exit options improve and beneficiary-victim asymmetry reduces through technological/philosophical change.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(roman_slave_economy_extraction, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
