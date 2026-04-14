% ============================================================================
% CONSTRAINT STORY: roman_imperial_succession_instability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_imperial_succession_instability, []).

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
 *   constraint_id: roman_imperial_succession_instability
 *   human_readable: Roman Imperial Succession Instability
 *   domain: political_history/institutional_governance
 *
 * SUMMARY:
 *   The Roman Imperial succession system, absent fixed constitutional
 *   mechanisms for determining the next emperor, created structural
 *   instability that deepened over centuries from the Julio-Claudian period
 *   through the 3rd century crisis. No formal electoral procedure existed;
 *   emperors could adopt heirs, the military could acclaim claimants, the
 *   Senate could legitimize choices, or war could settle succession disputes.
 *   This absence of clear mechanism created extractive dynamics: claimants
 *   competed for military loyalty through promises of donative payments
 *   (extracted from the treasury and provinces); provincial governors faced
 *   extraction through threats to their power bases; civilians were
 *   conscripted or taxed to support rival campaigns; and institutional
 *   authorities (Senate, provincial structures) lost autonomy as emperors
 *   consolidated power. The constraint exhibits a Snare classification: high
 *   extraction (0.68), high suppression (0.75 — subjects have no peaceful
 *   exit from the succession system), and multiple victim groups with no
 *   coordination benefit. The theater ratio (0.65) reflects that imperial
 *   legitimation rituals (acclamation ceremonies, purple birth, Senate
 *   recognition) persist ceremonially but fail functionally to prevent
 *   succession crises, especially visible in the 3rd century when multiple
 *   emperors were declared and deposed in rapid succession. By the 3rd
 *   century crisis, the legitimation apparatus had become substantially
 *   performative — the ritual forms remained while institutional control
 *   fragmented.
 *
 * KEY AGENTS:
 *   - Provincial Populations: Primary victims (powerless/trapped) — conscripted into armies, taxed heavily, lose property in territorial disputes between claimants
 *   - Military Hierarchy: Organized victims (moderate/constrained) — forced to choose loyalty to claimants; execution risk if choice loses; career dependency traps them
 *   - Provincial Elite and Governors: Powerful but vulnerable (powerful/mobile) — threatened by different claimants; must pledge allegiance that constrains autonomy; can switch allegiances but at political cost
 *   - Senate and Republican Institutions: Organized actors (organized/constrained) — provide ceremonial legitimation but lose real authority over time; constrained by precedent and military pressure
 *   - Imperial Court and Court Factions: Primary beneficiaries (institutional/arbitrage) — succession crises elevate courtier influence; factions gain leverage during transitions; can arbitrage between claimants
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks seeing succession instability as inherent structural law rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_imperial_succession_instability, 0.68).
domain_priors:suppression_score(roman_imperial_succession_instability, 0.75).
domain_priors:theater_ratio(roman_imperial_succession_instability, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_imperial_succession_instability, extractiveness, 0.68).
narrative_ontology:constraint_metric(roman_imperial_succession_instability, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(roman_imperial_succession_instability, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_imperial_succession_instability, snare).
narrative_ontology:human_readable(roman_imperial_succession_instability, "Roman Imperial Succession Instability").
narrative_ontology:topic_domain(roman_imperial_succession_instability, "political_history/institutional_governance").

domain_priors:requires_active_enforcement(roman_imperial_succession_instability).

% --- Structural relationships ---
narrative_ontology:constraint_victim(roman_imperial_succession_instability, military_hierarchy).
narrative_ontology:constraint_victim(roman_imperial_succession_instability, senate_institutional_continuity).
narrative_ontology:constraint_victim(roman_imperial_succession_instability, provincial_governance).
narrative_ontology:constraint_victim(roman_imperial_succession_instability, civilian_population).
narrative_ontology:constraint_victim(roman_imperial_succession_instability, imperial_treasury).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROVINCIAL POPULATION (SNARE) — Powerless, trapped populations in provinces face extractive succession instability with no exit: conscription into rival legions, tax surges during civil wars, loss of property in territorial disputes between claimants. High suppression with no alternatives. Maximum experienced extraction.
constraint_indexing:constraint_classification(roman_imperial_succession_instability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: MILITARY HIERARCHY (SNARE) — Officers and legionaries face forced choices: commit to a succession claimant (risking execution if their choice loses) or refuse (risking mutiny charges). High extraction with constrained exit: relocation within empire is possible but abandoning military career carries massive status loss.
constraint_indexing:constraint_classification(roman_imperial_succession_instability, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: PROVINCIAL ELITE (SNARE WITH MOBILITY) — Provincial governors and elites have some mobility and power but still experience severe extraction: their capital and provincial bases are threatened by different claimants, requiring allegiance pledges that constrain future autonomy. Powerful but not sovereign; extraction remains high despite some exit options through alliance-switching.
constraint_indexing:constraint_classification(roman_imperial_succession_instability, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: SENATE (TANGLED ROPE) — The Senate experiences succession instability as both coordination mechanism and extraction: succession ceremonies and legitimation through Senate acclamation coordinate leadership transitions, yet the Senate itself is increasingly extracted from as emperors bypass it, reducing its institutional power over time. Organized resistance but constrained by institutional precedent.
constraint_indexing:constraint_classification(roman_imperial_succession_instability, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: IMPERIAL COURT (TANGLED ROPE) — The imperial court benefits from succession instability: court factions gain leverage during transitions, and succession crises elevate courtier influence. Active enforcement of loyalty mechanisms and adoption of heirs coordinates succession, but also enables extraction of concessions from other power centers. Net beneficiary with arbitrage options.
constraint_indexing:constraint_classification(roman_imperial_succession_instability, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: IMPERIAL LEGITIMATION RITUAL (PITON) — The theater of imperial succession (purple birth, Senate acclamation, military acclamation, deification of predecessors) persists through institutional inertia but increasingly fails to prevent succession crises. By the 3rd century, the ritual is substantially performative — acclamation ceremonies happen, yet civil wars follow regardless. Theater ratio high because the legitimation apparatus is maintained ceremonially while functionally degraded.
constraint_indexing:constraint_classification(roman_imperial_succession_instability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — From a civilizational perspective, succession instability in a non-hereditary elective system with no constitutional mechanism appears as a natural law of political structure: without fixed heredity or clear electoral procedure, power concentration creates incentive structures that make succession crises inevitable. However, this naturalizes what is actually a contingent institutional choice — the constraints were different under the Julio-Claudian and Antonine dynasties.
constraint_indexing:constraint_classification(roman_imperial_succession_instability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_imperial_succession_instability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roman_imperial_succession_instability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_imperial_succession_instability, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(roman_imperial_succession_instability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(roman_imperial_succession_instability, TR),
    TR >= 0.70.

:- end_tests(roman_imperial_succession_instability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. Early period shows lower extractiveness (0.35) because Julio-Claudian and early imperial succession operated under relatively stable dynasties with clearer expectations. Extractiveness increases to 0.52 by the Antonine period as adoption-based succession creates ambiguity and civil wars become more frequent. By 235 CE (time_point 100), extractiveness reaches 0.68 as the 3rd century crisis produces rapid succession changes, constant military claims, provincial fragmentation, and maximum economic extraction through conscription and taxation. The trajectory shows accumulating institutional degradation and extraction intensification over 300 years. Suppression (0.75): Very high and stable. Imperial subjects have no legitimate exit from the succession system — they cannot opt out of military service demands, cannot refuse tax obligations, cannot protect their property from claimants' armies, and cannot change the emperor through peaceful political participation. The only exits are escape to barbarian territories (rarely feasible), private withdrawal (rare and punished), or violent resistance (suicidal). Suppression remains near-constant because the structural barriers (military monopoly on force, centralized fiscal extraction, territorial control) persist across the period. Theater ratio (0.65): Moderate-high and rising. The imperial legitimation apparatus (acclamation ceremonies, Senate recognition, purple birth, deification of predecessors) persists throughout the period as theater. In the early period, this theater has some coordination function — it helps legitimize succession choices and provides ceremony that binds the empire together. By the 3rd century, the theater is substantially disconnected from function: acclamation ceremonies are performed while civil wars rage; Senate recognition is ceremonial while military strongmen impose claimants; the ritual forms are maintained but their power to prevent succession crises has collapsed. Theater rises from 0.45 to 0.65 as institutional forms ossify while institutional function degrades.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the court (rope/tangled_rope experience) and the province (snare experience) reveals the constraint's core asymmetry. The court genuinely benefits from succession competition — it coordinates elite politics through succession mechanism. But this coordination is built on top of extraction from populations without political voice. The constraint is not pure coordination (rope) because it produces measurable harm (conscription, taxation, property loss); it is not pure extraction (mountain) because it genuinely coordinates something (elite succession). It is a snare disguised as coordination when viewed from the institutional perspective. The piton perspective (legitimation ritual) shows institutional degradation — the theater persists while function fails. The mountain perspective (structural inevitability) naturalizes what is actually a choice: the Julio-Claudian dynasty and Antonine dynasty operated with much lower instability through different institutional arrangements (heredity, explicit adoption). The naturalizing move is a false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from the beneficiary-victim structure. The imperial court and closely-related institutional actors are beneficiaries of succession instability — succession crises elevate their political leverage, create opportunities for faction advancement, and enable extraction of concessions from other power centers. Their directionality (d) is low because they benefit from and actively participate in the extraction flow. All other positions are victims: military hierarchy, provincial populations, provincial elite, Senate, and the imperial treasury all bear costs without equivalent benefits. Their directionality (d) is high, indicating they experience the constraint as extractive. The magnitude of effective extraction (χ) is particularly high for powerless and trapped agents (provincial population) whose f(d) modifier is high due to their complete lack of exit options. For constrained or mobile agents (military, provincial elite), f(d) is somewhat lower, but still substantial, because the constraint's suppression mechanisms are strong despite some exit capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing the snare classification across victim perspectives while acknowledging partial coordination at the institutional elite level. The constraint is not 'coordination masquerading as extraction' but rather 'coordination that requires extraction as its implementation mechanism.' The imperial court needs succession crises to maintain leverage; military strongmen need ambiguity to justify their political role; provincial populations must bear costs to enable elite succession competition. This is not a false positive (a snare wrongly labeled as rope) but a genuine snare where coordination benefits accrue asymmetrically to institutional elites while extraction costs are distributed across powerless agents. The measurement trajectory (extractiveness rising from 0.35 to 0.68, theater rising from 0.45 to 0.65) confirms degradation: early imperial period showed lower extractiveness because institutional arrangements (dynastic precedent, clearer succession expectations) created some stability that benefited everyone. As those arrangements degraded, extraction intensified. By the 3rd century, the constraint had become a pure snare with minimal coordination benefit. Mandatrophy is resolved: the constraint is correctly classified as snare because extraction dominates coordination across the full time horizon, and the beneficiary group (imperial court factions) is institutional/arbitrage (not powerless), making their experience of coordination a secondary effect rather than the primary coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    heredity_versus_merit_tradeoff,
    'Was the instability inherent to meritocratic succession selection, or was it a failure of institutional mechanism for legitimating chosen heirs?',
    'Comparative analysis of succession outcomes under explicit heredity (Julio-Claudian, Constantinian dynasties) vs explicit adoption (Antonine dynasty) vs chaotic selection (3rd century crisis); measurement of civil war frequency and duration under each regime',
    'If inherent: succession instability is a structural limit of non-hereditary systems (mountain classification justified). If institutional failure: the constraint is a tangled_rope or snare reflecting poor legitimation design (false summit detection).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(heredity_versus_merit_tradeoff, empirical, 'Whether succession instability was inherent to meritocratic selection or institutional design failure').

omega_variable(
    legitimation_versus_power_concentration,
    'Did succession crises drive power concentration (military strongmen replacing Senate authority), or did prior power concentration make succession crises inevitable?',
    'Chronological correlation between institutional changes (Senate authority reduction, military autonomy increase, provincial fiscal independence) and succession crisis frequency; causal analysis of which preceded which',
    'If concentration caused instability: snare classification is correct — the mechanism extracts from all but those holding concentrated power. If instability caused concentration: the constraint evolved over time from tangled_rope to snare, and measurement trajectory shows this transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimation_versus_power_concentration, empirical, 'Causal direction between power concentration and succession instability').

omega_variable(
    military_versus_civilian_extraction_mechanism,
    'Was the primary extraction mechanism the military''s ability to impose claimants (extraction from civilians and Senate), or the instability itself (civil war destruction affecting all)?',
    'Analysis of extraction flows: military conscription rates during succession crises vs stability; tax increases on provinces; destruction of civilian property; institutional authority changes affecting different classes',
    'If military extraction: snare with organized agents as primary extractors (military factions). If instability-based harm: more generalized snare affecting all powerless agents equally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_versus_civilian_extraction_mechanism, empirical, 'Whether military organization or general instability was primary extraction mechanism').

omega_variable(
    third_century_structural_break,
    'Does the 3rd century crisis represent a continuation of earlier succession instability or a categorical structural change requiring separate constraint story?',
    'Comparative metrics: succession interval variance, civil war frequency, provincial autonomy increase, institutional authority distribution before and after 235 CE; analysis of whether 3rd century mechanisms are continuous with earlier dynamics',
    'If continuous: single constraint story valid across period. If categorical break: separate stories for early/mid empire (tangled_rope/snare hybrid) and 3rd century crisis (pure snare) with network links.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_century_structural_break, empirical, 'Whether 3rd century crisis is structural continuity or categorical change').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_imperial_succession_instability, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ris_tr_t0, roman_imperial_succession_instability, theater_ratio, 0, 0.45).
narrative_ontology:measurement(ris_tr_t50, roman_imperial_succession_instability, theater_ratio, 50, 0.55).
narrative_ontology:measurement(ris_tr_t100, roman_imperial_succession_instability, theater_ratio, 100, 0.65).

% Extraction over time
narrative_ontology:measurement(ris_be_t0, roman_imperial_succession_instability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ris_be_t50, roman_imperial_succession_instability, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(ris_be_t100, roman_imperial_succession_instability, base_extractiveness, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_imperial_succession_instability, enforcement_mechanism).
narrative_ontology:affects_constraint(roman_imperial_succession_instability, roman_military_loyalty_extraction).
narrative_ontology:affects_constraint(roman_imperial_succession_instability, provincial_fiscal_pressure).
narrative_ontology:affects_constraint(roman_imperial_succession_instability, senate_institutional_authority_loss).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
