% ============================================================================
% CONSTRAINT STORY: post_conflict_reconciliation_mechanisms
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_post_conflict_reconciliation_mechanisms, []).

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
 *   constraint_id: post_conflict_reconciliation_mechanisms
 *   human_readable: Post-Conflict Reconciliation Mechanisms
 *   domain: political/social/transitional_justice
 *
 * SUMMARY:
 *   Post-conflict reconciliation mechanisms—truth commissions, amnesty
 *   processes, public apologies, healing dialogues—are institutionalized
 *   frameworks designed to manage transitions from violence to coexistence.
 *   They function simultaneously as coordination mechanisms (enabling
 *   survivors and perpetrators to share space, creating historical record,
 *   reintegrating combatants) and extractive mechanisms (distributing the
 *   costs of peace asymmetrically, prioritizing stability over survivor
 *   healing, shielding elites from prosecution). The constraint exhibits the
 *   full spectrum of DR classification depending on structural position:
 *   political elites see a coordination mechanism protecting their rule
 *   (Rope); survivors see a mechanism that extracts emotional labor and
 *   forgiveness without delivering justice (Snare); international
 *   institutions see a legitimate coordination tool generating institutional
 *   authority and funding flows (Rope); the reconciliation ritual apparatus
 *   itself is increasingly theatrical as substantive accountability declines
 *   (Piton). The theater ratio (0.68) reflects that reconciliation mechanisms
 *   are substantially performative: public hearings create the appearance of
 *   accountability while amnesty provisions prevent actual prosecution;
 *   survivor participation creates legitimacy for political transitions
 *   without empowering survivors to shape outcomes; perpetrator testimony
 *   creates historical record while protecting perpetrators from proportional
 *   consequences.
 *
 * KEY AGENTS:
 *   - Survivors and Victim Groups: Primary victims (powerless/trapped) — compelled to participate, extract emotional labor and forgiveness without proportional healing or justice
 *   - Survivor Advocacy Organizations: Secondary victims (moderate/constrained) — dependent on international funding, constrained by pressure to legitimize reconciliation frameworks designed for political stability
 *   - International Transitional Justice Institutions: Primary beneficiaries (institutional/arbitrage) — extract institutional authority, funding, and precedent-setting capacity while framing as humanitarian
 *   - Post-Conflict Political Elites: Primary beneficiaries (institutional/arbitrage) — achieve stability and international recognition while avoiding accountability for systemic causes of conflict
 *   - Perpetrator Communities and Mid-Level Combatants: Mixed (powerful/constrained to powerful/mobile) — constrained by international pressure and domestic accountability movements; elites extract protection while lower ranks face limited accountability
 *   - Reconciliation Ritual Apparatus: Institutional actor (institutional/arbitrage) — maintains performative functions (legitimacy, historical record) through institutional inertia despite declining substantive accountability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(post_conflict_reconciliation_mechanisms, 0.58).
domain_priors:suppression_score(post_conflict_reconciliation_mechanisms, 0.62).
domain_priors:theater_ratio(post_conflict_reconciliation_mechanisms, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(post_conflict_reconciliation_mechanisms, extractiveness, 0.58).
narrative_ontology:constraint_metric(post_conflict_reconciliation_mechanisms, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(post_conflict_reconciliation_mechanisms, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(post_conflict_reconciliation_mechanisms, tangled_rope).
narrative_ontology:human_readable(post_conflict_reconciliation_mechanisms, "Post-Conflict Reconciliation Mechanisms").
narrative_ontology:topic_domain(post_conflict_reconciliation_mechanisms, "political/social/transitional_justice").

domain_priors:requires_active_enforcement(post_conflict_reconciliation_mechanisms).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(post_conflict_reconciliation_mechanisms, political_elites).
narrative_ontology:constraint_beneficiary(post_conflict_reconciliation_mechanisms, international_institutions).
narrative_ontology:constraint_beneficiary(post_conflict_reconciliation_mechanisms, perpetrator_communities).
narrative_ontology:constraint_victim(post_conflict_reconciliation_mechanisms, survivors_and_victims).
narrative_ontology:constraint_victim(post_conflict_reconciliation_mechanisms, transitional_justice_process).
narrative_ontology:constraint_victim(post_conflict_reconciliation_mechanisms, truth_recovery).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SURVIVOR/VICTIM (SNARE) — Trapped within the reconciliation framework with no exit option. Compelled to participate in truth commissions, forgive perpetrators, or witness amnesty proceedings to achieve any acknowledgment. High suppression: victims cannot reject the process without losing voice entirely. Maximum extraction: the mechanism extracts emotional labor (retelling trauma) and forgiveness (abandoning justice claims) without delivering proportional healing or accountability.
constraint_indexing:constraint_classification(post_conflict_reconciliation_mechanisms, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SURVIVOR ADVOCACY ORGANIZATIONS (TANGLED ROPE) — Constrained by funding dependencies, international pressure for reconciliation frameworks, and the need to maintain legitimacy with both survivors and state actors. Some coordination function exists: organizing collective testimony, documenting truth, building historical record. But asymmetric extraction: organizations are compelled to legitimize processes designed primarily for political stability rather than survivor justice. High suppression of alternative approaches (prosecutions, reparations-first models).
constraint_indexing:constraint_classification(post_conflict_reconciliation_mechanisms, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERNATIONAL INSTITUTIONS (ROPE) — UN, International Criminal Court, transitional justice NGOs experience reconciliation as a coordination mechanism: enabling states to manage post-conflict transitions while maintaining international legitimacy. Arbitrage exit: these institutions can invest in multiple countries, switching resources to states with better outcomes. Net beneficiaries: extracting value (institutional authority, funding flows, precedent-setting) while framing as humanitarian service.
constraint_indexing:constraint_classification(post_conflict_reconciliation_mechanisms, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PERPETRATOR COMMUNITIES (TANGLED ROPE) — Constrained by international pressure and domestic accountability movements. Reconciliation mechanisms provide both coordination (reintegrating combatants into civil society) and asymmetric extraction: elites avoid prosecution while lower-ranking perpetrators face limited accountability. Some cooperation is enforced; some is voluntary. Theater: amnesty proceedings frame accountability as 'truth-for-forgiveness' when reality is avoidance of prosecution.
constraint_indexing:constraint_classification(post_conflict_reconciliation_mechanisms, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: POST-CONFLICT POLITICAL LEADERSHIP (ROPE) — Net beneficiaries of reconciliation mechanisms. Achieve political stability and international recognition without addressing root causes or systemic inequality. Arbitrage exit: can distance themselves from the process while claiming commitment to peace. Low experienced extraction because the mechanism legitimizes their rule and deflects accountability demands.
constraint_indexing:constraint_classification(post_conflict_reconciliation_mechanisms, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: RECONCILIATION RITUAL APPARATUS (PITON) — Truth commissions, amnesty hearings, and public apologies have become performative rituals maintained through institutional inertia. Theater ratio (0.68) reflects that most post-conflict reconciliation theater—the public hearing, the amnesty certificate, the 'healing dialogue'—performs legitimate institutional functions (legitimizing stability, creating historical record) but shows minimal correlation with actual survivor healing, perpetrator behavior change, or structural justice. The apparatus persists because alternatives (prosecution, reparations, systemic reform) are politically costly for elites, not because it functions optimally.
constraint_indexing:constraint_classification(post_conflict_reconciliation_mechanisms, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / IMMUTABILITY VIEW (MOUNTAIN) — From a civilizational perspective, some form of post-conflict accommodation is structurally required: groups that fought cannot immediately coexist without a transition mechanism. This perspective risks naturalizing what is actually a contingent institutional arrangement. The engine's false summit detector will flag this: the 'inherent need for reconciliation' is real, but the specific mechanisms (amnesty-heavy, survivor-light, elite-protective) are political choices, not natural laws.
constraint_indexing:constraint_classification(post_conflict_reconciliation_mechanisms, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(post_conflict_reconciliation_mechanisms_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(post_conflict_reconciliation_mechanisms, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(post_conflict_reconciliation_mechanisms, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(post_conflict_reconciliation_mechanisms, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(post_conflict_reconciliation_mechanisms, TR),
    TR >= 0.70.

:- end_tests(post_conflict_reconciliation_mechanisms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The mechanism extracts from survivors (emotional labor, forgiveness, narrative testimony) and from transitional justice outcomes (selective amnesty prioritizes stability over prosecution, elite perpetrators escape accountability while lower-rank combatants face limited consequences). Extractiveness is not as high as pure prosecution avoidance (0.75+) because some genuine coordination occurs (shared space for survivors and perpetrators, documented truth, reintegration support) and some survivor input shapes processes. Suppression (0.62): Moderate-high. Significant barriers to rejecting reconciliation frameworks: survivors face loss of voice if they refuse participation; prosecution-focused advocates are pressured toward reconciliation; perpetrator communities face international enforcement. But suppression is not total—survivor organizations can and do push back, some states pursue hybrid models (truth + prosecution), international actors do not uniformly enforce reconciliation. Theater ratio (0.68): High and rising. Truth commissions perform legitimacy-building functions (creating historical record, demonstrating state responsiveness) but show weak correlation with survivor healing outcomes or perpetrator behavioral change. Public apologies and amnesty hearings are theatrical—creating appearances of accountability while preventing actual prosecution. The ratio has risen over the interval (0.40 → 0.68) as reconciliation frameworks have become institutionalized and disconnected from concrete justice delivery.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary perspectives (Rope, low extraction experienced) and victim perspectives (Snare/Tangled Rope, high extraction experienced) reveals that the mechanism is structurally extractive despite framing as coordination. International institutions' Rope classification depends on their ability to exit (arbitrage) and maintain external legitimacy; survivors' Snare classification depends on being trapped with no exit and no alternative voice channels. Survivor advocacy organizations occupy an intermediary position (Tangled Rope) — they have some agency and benefit from documentation/voice platforms, but their agency is constrained by funding dependencies and pressure to legitimize processes designed primarily for elite stability, not survivor justice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect each agent's structural position relative to the extraction flow. Survivors with no exit (trapped) experience maximum d ≈ 0.95 → high f(d) → high χ. International institutions with arbitrage options experience low d ≈ 0.10 → low/negative f(d) → low χ or negative (they are beneficiaries). Post-conflict elites with arbitrage options experience d ≈ 0.15 → minimal χ. Survivor organizations with constrained exits (funding dependency, legitimacy pressure) experience higher d ≈ 0.60 → moderate χ. The beneficiary/victim declarations are the primary inputs: survivors are declared as victims (high d), elites and international institutions as beneficiaries (low d). Organized survivor coalitions with some agency experience moderate d ≈ 0.50-0.60 rather than powerless agents' maximum, reflecting partial agency within constrained space.
 *
 * MANDATROPHY ANALYSIS:
 *   Reconciliation mechanisms resolve the mandatrophy by revealing that the classification depends entirely on structural position. The mechanism is simultaneously coordination (for elites seeking stability) and extraction (for survivors seeking justice). The false summit is the analytical observer's mountain — the 'inherent need for post-conflict accommodation' is real, but the specific mechanisms (amnesty-heavy, survivor-voice-limited, elite-protective) are not natural laws of post-conflict transitions. They are contingent institutional arrangements chosen by political actors. The Tangled Rope classification is confirmed by the presence of genuine coordination functions (enabling survivor-perpetrator coexistence, creating historical record, reintegrating combatants) alongside asymmetric extraction (suppressing prosecution, prioritizing elite stability, extracting emotional labor from survivors without proportional healing). The Piton classification of the ritual apparatus reflects that the mechanisms have become increasingly performative as substantive accountability has declined — the public hearing, the amnesty certificate, the 'healing dialogue' persist through institutional inertia rather than functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    healing_versus_legitimation_function,
    'Are reconciliation mechanisms primarily healing mechanisms for survivors or legitimation mechanisms for post-conflict political order?',
    'Longitudinal outcome tracking: psychological recovery rates, perpetrator behavior change, survivor satisfaction, vs. international recognition, domestic stability, elite retention metrics',
    'If primarily healing: extractiveness should be lower (0.35-0.45); classification shifts toward Scaffold. If primarily legitimation: extractiveness remains high (0.58+); Tangled Rope confirmed. If bifurcated: decompose into two separate constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(healing_versus_legitimation_function, empirical, 'Whether mechanisms serve survivor healing or regime legitimation').

omega_variable(
    structural_versus_interpersonal_reconciliation,
    'Can interpersonal reconciliation (perpetrator-survivor dialogue) substitute for structural reform (institutional accountability, power redistribution, reparations)?',
    'Comparative analysis across post-conflict societies: reconciliation-light transitions vs. those pairing reconciliation with structural reform; measurement of re-conflict risk, inequality trends, institutional capture',
    'If substitutable: Theater ratio rises (interpersonal reconciliation becomes pure ritual). If complementary: Tangled Rope confirmed. If theater masks avoidance of structural change: Snare classification applies to survivor experience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_versus_interpersonal_reconciliation, empirical, 'Whether interpersonal reconciliation can substitute for structural reform').

omega_variable(
    perpetrator_versus_bystander_complicity,
    'Do reconciliation mechanisms differentiate between active perpetrators and passive/coerced bystanders, or do they collapse the distinction into uniform amnesty?',
    'Review of truth commission findings and amnesty patterns; analysis of whether differentiated accountability or restorative justice options exist; comparison of survivor satisfaction across perpetrator vs. bystander cases',
    'If differentiated: suppression and extraction values lower; Tangled Rope confirmed. If collapsed: suppression higher (survivors forced to forgive all); extractiveness rises toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perpetrator_versus_bystander_complicity, empirical, 'Whether mechanisms differentiate active perpetrators from coerced participants').

omega_variable(
    survivor_versus_perpetrator_community_identity_lock,
    'To what extent is survivor or perpetrator participation in reconciliation mechanisms driven by identity fusion (self-concept constituted through the process) vs. constrained choice (material barriers to exit)?',
    'Post-process ethnographic analysis; tracking of agents who reject reconciliation frameworks; measurement of identity reconstruction patterns vs. material incentive structures',
    'If identity-locked: exit_options shift from ''trapped'' to ''identity_locked''; classification remains Snare but reveals cognitive rather than structural binding. If material constraint: current assessment correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(survivor_versus_perpetrator_community_identity_lock, conceptual, 'Whether participation is identity-locked or materially constrained').

omega_variable(
    international_versus_domestic_reconciliation_logic,
    'Are reconciliation mechanisms designed to satisfy international standards (ICC, UN precedent, donor expectations) or to serve domestic survivor/perpetrator needs?',
    'Comparative analysis: processes designed with international input vs. survivor-led processes; measurement of international legitimacy gain vs. domestic satisfaction; tracking of donor influence on amnesty/prosecution decisions',
    'If internationally driven: International Institutions perspective (Rope) becomes the dominant design logic; suppression rises (domestic preferences overridden); extractiveness confirmed. If domestically driven: elites still extract but mechanism is responsive to survivor input.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_versus_domestic_reconciliation_logic, empirical, 'Whether mechanisms serve international standards or domestic needs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(post_conflict_reconciliation_mechanisms, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(post_tr_t0, post_conflict_reconciliation_mechanisms, theater_ratio, 0, 0.4).
narrative_ontology:measurement(post_tr_t3, post_conflict_reconciliation_mechanisms, theater_ratio, 3, 0.55).
narrative_ontology:measurement(post_tr_t6, post_conflict_reconciliation_mechanisms, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(post_be_t0, post_conflict_reconciliation_mechanisms, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(post_be_t3, post_conflict_reconciliation_mechanisms, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(post_be_t6, post_conflict_reconciliation_mechanisms, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(post_conflict_reconciliation_mechanisms, enforcement_mechanism).
narrative_ontology:affects_constraint(post_conflict_reconciliation_mechanisms, transitional_justice_prosecution_avoidance).
narrative_ontology:affects_constraint(post_conflict_reconciliation_mechanisms, survivor_community_trauma_cycles).
narrative_ontology:affects_constraint(post_conflict_reconciliation_mechanisms, perpetrator_reintegration_resistance).

% DUAL FORMULATION NOTE:
% Post-conflict reconciliation mechanisms decompose into three structurally distinct constraints: (1) the coordination problem (enabling coexistence) with lower ε, (2) the elite protection mechanism (avoiding prosecution) with higher ε, and (3) the survivor voice/healing mechanism (documentation and emotional processing) with medium ε. This story captures the hybrid mechanism; downstream constraints model specific functional dimensions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(post_conflict_reconciliation_mechanisms, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
