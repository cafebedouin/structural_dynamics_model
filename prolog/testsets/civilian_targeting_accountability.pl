% ============================================================================
% CONSTRAINT STORY: civilian_targeting_accountability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_civilian_targeting_accountability, []).

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
 *   constraint_id: civilian_targeting_accountability
 *   human_readable: Civilian Targeting Accountability in Armed Conflict
 *   domain: military/legal/humanitarian
 *
 * SUMMARY:
 *   Civilian targeting accountability operates as a structural constraint in
 *   armed conflict that nominally protects non-combatants through legal
 *   norms, international law, and investigation mechanisms, but
 *   systematically fails to constrain powerful military actors. The
 *   constraint exhibits the key signature of a snare: civilians (powerless,
 *   trapped) experience the extraction of being exposed to targeting without
 *   effective protection; military command structures (powerful, mobile)
 *   benefit from operating under norms they can violate with minimal
 *   consequence; and the accountability machinery (investigation bodies, IHL
 *   institutions) is locked into constrained positions that prevent
 *   enforcement against state actors. The theater ratio (0.68) reflects that
 *   targeting procedures, proportionality assessments, and compliance
 *   documentation are substantially performative—procedures exist and are
 *   followed, but their real-time effect on targeting decisions is minimal.
 *   Post-hoc accountability narratives (after-action investigations, war
 *   crimes prosecutions) reconstruct events to fit existing norms rather than
 *   preventing violations ex ante.
 *
 * KEY AGENTS:
 *   - Civilian populations in conflict zones: Primary victims (powerless/trapped) — experience targeting exposure and suppression (communication blackouts, displacement threats, siege tactics) without exit or recourse
 *   - Military command structures: Primary beneficiaries (powerful/mobile) — capture operational efficiency benefits and maintain capacity to violate norms; bear costs of restraint but can choose non-compliance
 *   - State security apparatus: Secondary beneficiary (institutional/arbitrage) — maintains international legitimacy through procedural compliance while preserving operational freedom
 *   - International Humanitarian Law institutions: Institutional actor (institutional/constrained) — appear to coordinate legitimate norms but lack enforcement power against state actors
 *   - War crimes investigation mechanisms (ICC, ad hoc tribunals): Institutional actor (organized/constrained) — institutionally dependent on state cooperation; cannot initiate prosecution against powerful actors
 *   - Humanitarian advocacy organizations: Secondary victim (organized/constrained) — document violations but lack enforcement mechanisms; face suppression (access denial, political pressure)
 *   - Analytical observer: Universal view (analytical/analytical) — risks naturalizing operational constraints (fog of war, asymmetric information) as unchangeable features rather than resource allocation choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(civilian_targeting_accountability, 0.68).
domain_priors:suppression_score(civilian_targeting_accountability, 0.72).
domain_priors:theater_ratio(civilian_targeting_accountability, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(civilian_targeting_accountability, extractiveness, 0.68).
narrative_ontology:constraint_metric(civilian_targeting_accountability, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(civilian_targeting_accountability, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(civilian_targeting_accountability, snare).
narrative_ontology:human_readable(civilian_targeting_accountability, "Civilian Targeting Accountability in Armed Conflict").
narrative_ontology:topic_domain(civilian_targeting_accountability, "military/legal/humanitarian").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(civilian_targeting_accountability, military_command_structures).
narrative_ontology:constraint_beneficiary(civilian_targeting_accountability, state_security_apparatus).
narrative_ontology:constraint_victim(civilian_targeting_accountability, civilian_populations).
narrative_ontology:constraint_victim(civilian_targeting_accountability, humanitarian_principle_enforcement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Civilians in conflict zones experience pure extraction: exposure to targeting without recourse, legal protection that is unenforced, and suppression mechanisms (siege tactics, communication blackouts, displacement threats) that prevent collective action or escape. No meaningful exit exists; protection mechanisms are theater.
constraint_indexing:constraint_classification(civilian_targeting_accountability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% IHL mechanisms (Geneva Conventions, ICRC protocols, targeting rules) appear to international lawyers and humanitarian institutions as a coordination system: establishing shared norms for distinguishing combatants from civilians, proportionality standards, and medical neutrality. These are genuine coordination functions that serve all parties' long-term interests by reducing chaos and enabling mutual restraint. The framework benefits from institutionalization and has arbitrage options (use vs. non-use of protocols).
constraint_indexing:constraint_classification(civilian_targeting_accountability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% Military organizations experience the accountability constraint as pure extraction with high effective power: they bear the costs of restraint (operational complexity, intelligence overhead, slower target acquisition) while maintaining the capacity to violate norms with low accountability. Their exit options (mobile) mean they can choose compliance or violation; their power means they face minimal enforcement. The constraint exists primarily to extract legitimacy and international standing while maintaining operational freedom.
constraint_indexing:constraint_classification(civilian_targeting_accountability, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% ICC, ad hoc tribunals, and investigation bodies see genuine coordination (establishing facts, deterring worst excesses) alongside severe constraints on their actual enforcement power. They benefit from the accountability framework's existence (mandate, legitimacy, donor support) but are systematically prevented from prosecuting powerful actors. Constrained exit reflects institutional dependence on state compliance and political will.
constraint_indexing:constraint_classification(civilian_targeting_accountability, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Military doctrines and targeting procedures (proportionality assessments, civilian harm estimates, rules of engagement) are substantially performative: compliance assessments are rarely independent, documentation serves post-hoc narrative construction rather than real-time accountability, and procedural compliance does not prevent civilian harm. Theater ratio reflects that the machinery of accountability persists through institutional inertia despite low functional constraint on actual targeting decisions.
constraint_indexing:constraint_classification(civilian_targeting_accountability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational/universal perspective, some casualty gap between combatant and civilian targeting is inherent to warfare: asymmetric information, fog of war, and technical limitations create irreducible verification problems. This perspective risks naturalizing what is actually a structural choice about resource allocation (do we invest in precision targeting verification or accept higher civilian harm as acceptable cost?). The mountain classification is a false summit diagnostic.
constraint_indexing:constraint_classification(civilian_targeting_accountability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(civilian_targeting_accountability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(civilian_targeting_accountability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(civilian_targeting_accountability, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(civilian_targeting_accountability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(civilian_targeting_accountability, TR),
    TR >= 0.70.

:- end_tests(civilian_targeting_accountability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Military organizations systematically extract operational freedom (faster targeting, higher-confidence elimination, lower collateral-harm documentation burden) from the accountability framework while maintaining public compliance. The extraction is not total because some genuine restraint occurs (proportionality reviews, medical neutral protection), but the gap between procedural compliance and actual civilian protection is substantial. The measurement trajectory (0.52→0.68) reflects increasing extractiveness as conflicts intensify and accountability mechanisms are strained. Suppression (0.72): High. Mechanisms preventing civilian exit and countermeasures include: siege tactics and blockades (geographic suppression), communication blackouts (information suppression), displacement threats and forced evacuation (mobility suppression), infiltration of civil defense organizations (collective action suppression), and legal immunity for military personnel in sponsor states (institutional suppression). Theater ratio (0.68): High and rising. Targeting procedures, proportionality assessments, rules of engagement reviews, and post-action investigations all have substantive procedural content but disconnect from real-time targeting decisions. The theater has increased (0.45→0.68) as conflicts have intensified and documentation requirements have grown without corresponding enforcement power.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a maximal perspectival gap. Civilians see a snare (pure extraction with no escape). Military organizations see a rope (coordination of legitimate shared interests in preventing chaos). Humanitarian institutions see a tangled rope (genuine coordination function mixed with systematic inability to enforce). Investigation bodies see a piton (degraded ritual persisting through institutional inertia despite low function). The analytical observer at civilizational scale risks seeing a mountain (inherent fog of war) when the gap between norms and enforcement reveals a structurally chosen extraction mechanism. The perspectival spread (snare→rope→tangled_rope→piton→mountain) is diagnostic of how powerful actors maintain legitimacy: they publicly accept the rope framing (coordination of shared interests) while operating according to the snare framing (extraction of operational freedom). Weaker parties (civilians, humanitarian organizations) are locked into snare and tangled_rope perspectives without exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are determined by agents' structural positions relative to the targeting accountability flow. Civilians experience d≈0.95 (near-total victims): they bear targeting exposure costs, experience maximum suppression, and have trapped exit options. Military organizations experience d≈0.25-0.35 (beneficiaries with mobile exit): they extract operational freedom, face nominal rather than real enforcement, and can choose compliance levels. Humanitarian institutions experience d≈0.60-0.75 (partial victims): they depend on state cooperation for enforcement, face political suppression, but benefit institutionally from accountability frameworks. The directionality asymmetry is the mechanism of extraction: those with the most power to violate norms (d low) experience minimal effective constraint, while those most exposed to harm (d high) experience maximum suppression. No directionality override is needed; the structural data produces the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY (extractiveness 0.72): This constraint resolves the mandatrophy by showing that what appears as 'coordination of legitimate restraint norms' (rope perspective) from military and institutional actors' views is actually sustained extraction of operational freedom (snare perspective) from civilians' views. The mandatrophy resolution is not to choose between types but to recognize that the types describe different structural relationships to the same constraint. Military organizations genuinely experience a coordination mechanism (they coordinate targeting standards with allies, establish mutual restraint norms). Civilians genuinely experience pure extraction (they have no reciprocal protection, no exit, and no ability to shape the norms). The same institutional machinery (IHL, investigation bodies, targeting rules) functions as both rope and snare depending on whether you benefit from or bear the costs of the regime. The mandatrophy dissolves when we recognize that coordination at one scale (military-to-military norms) can simultaneously be extraction at another scale (military-to-civilian harm). The constraint is snare classified because the predominant structural relationship—civilian exposure without effective protection—is what the constraint mechanically enforces.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accountability_enforcement_gap,
    'Why are accountability mechanisms nominally strong in law but systematically weak in enforcement against powerful actors?',
    'Comparative analysis of prosecution rates: cases brought vs. plausible violations; political will measurement via ICC state cooperation patterns; institutional autonomy analysis of investigation bodies',
    'If gap is institutional (state veto power): snare classification confirmed. If gap is evidentiary (genuine difficulty proving violations): classification shifts toward tangled_rope. If gap is normative (states reserve sovereignty): constraint is scaffolding with indefinite sunset.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accountability_enforcement_gap, empirical, 'Why accountability enforcement fails against powerful military actors').

omega_variable(
    civilian_definition_instability,
    'Is the civilian/combatant distinction verifiable and stable, or does it collapse under operational pressure and definitional gaming?',
    'Analysis of targeting review processes; documentation of re-classification patterns (combatant claims for civilians); comparison of post-action assessments vs. real-time targeting decisions',
    'If distinction holds: accountability gap is enforcement failure. If distinction collapses: verification bottleneck is insurmountable, and targeting ''accountability'' is necessarily theater regardless of institutional commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_definition_instability, empirical, 'Stability of civilian/combatant distinction under operational conditions').

omega_variable(
    deterrence_efficacy_null,
    'Does actual enforcement (or threat of enforcement) of targeting rules measurably deter civilian harm, or is deterrence effect null and accountability is purely ritualistic?',
    'Controlled analysis of compliance patterns: do states/units increase restraint when enforcement risk rises? Do investigation announcements change targeting behavior? Comparison of civilian casualty rates in high-accountability vs. low-accountability contexts controlling for conflict intensity',
    'If deterrence is real: snare classification softens (extraction is constrained by fear). If deterrence is null: snare is confirmed, and accountability is pure legitimation theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy_null, empirical, 'Whether targeting rule enforcement has deterrent effect on civilian harm').

omega_variable(
    precision_targeting_cost_asymmetry,
    'Is the restraint burden (precision intelligence, procedural overhead, operational delay) genuinely higher than the benefit (international legitimacy, reduced counter-insurgency escalation), or is it lower but deliberately exaggerated to justify lower civilian protection?',
    'Cost accounting for precision targeting infrastructure vs. benefit quantification; comparison of operational effectiveness with and without targeting restraint; analysis of targeting choice patterns when precision is available but high-casualty option is faster',
    'If cost > benefit: snare reflects genuine operational extraction from military. If cost < benefit: snare reveals that targeting choices are driven by preferences for efficiency over protection, not by constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(precision_targeting_cost_asymmetry, empirical, 'Actual vs. claimed costs of precision targeting restraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(civilian_targeting_accountability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cta_tr_t0, civilian_targeting_accountability, theater_ratio, 0, 0.45).
narrative_ontology:measurement(cta_tr_t3, civilian_targeting_accountability, theater_ratio, 3, 0.58).
narrative_ontology:measurement(cta_tr_t6, civilian_targeting_accountability, theater_ratio, 6, 0.68).
narrative_ontology:measurement(cta_tr_t10, civilian_targeting_accountability, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(cta_be_t0, civilian_targeting_accountability, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(cta_be_t3, civilian_targeting_accountability, base_extractiveness, 3, 0.61).
narrative_ontology:measurement(cta_be_t6, civilian_targeting_accountability, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(cta_be_t10, civilian_targeting_accountability, base_extractiveness, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(civilian_targeting_accountability, enforcement_mechanism).
narrative_ontology:affects_constraint(civilian_targeting_accountability, proportionality_doctrine_verification).
narrative_ontology:affects_constraint(civilian_targeting_accountability, medical_neutrality_enforcement).
narrative_ontology:affects_constraint(civilian_targeting_accountability, distinction_principle_collapse).

% DUAL FORMULATION NOTE:
% Civilian targeting accountability decomposes into structurally distinct constraints: proportionality doctrine (verification bottleneck, ε≈0.42), medical neutrality (institutional capture, ε≈0.55), distinction principle (epistemological collapse, ε≈0.65). Each has different ε values and different enforcement mechanisms. The parent constraint aggregates these into a single snare structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(civilian_targeting_accountability, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
