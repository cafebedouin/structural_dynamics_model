% ============================================================================
% CONSTRAINT STORY: roman_republican_constitution__magistracies_and_collegiality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_republican_constitution__magistracies_and_collegiality, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: roman_republican_constitution__magistracies_and_collegiality
 *   human_readable: Roman Republican Constitution: Magistracies and Collegiality
 *   domain: political/historical/constitutional
 *
 * SUMMARY:
 *   The Roman Republic's constitution is often read as a system of
 *   magistracies: annually elected, collegially held offices that parcel the
 *   king's power into pieces no one could reassemble. This reading emphasizes
 *   the structural innovation of distributed executive power — no magistrate
 *   holds office for more than one year, and most senior offices require two
 *   holders who can veto each other's decisions. The mechanism is designed to
 *   prevent any individual from reconstituting monarchical authority.
 *   However, this reading is one of five competing interpretations of the
 *   Republic's constitutional foundation, each grounded in different primary
 *   evidence and different aspects of republican practice. The magistracies
 *   reading coexists with (and is partially explained by) the Senate's silent
 *   authority (permanent senators redirect magistrates' decisions), the
 *   popular assemblies reading (tribunes and assemblies constrain magistrates
 *   from below), the legal codification reading (the Twelve Tables set
 *   written limits on magisterial power), and the crisis machinery reading
 *   (dictatorship temporarily overrides the constraint to meet emergencies).
 *   This story instantiates only the magistracies_and_collegiality reading.
 *
 * KEY AGENTS:
 *   - Patrician Class Collective: Primary beneficiary (institutional/arbitrage) — exclusive access to magistracies; rotation through office preserves class dominance across generations
 *   - Would-Be Permanent Holders: Primary victim (powerful/mobile) — individuals who seek extended authority are suppressed by colleague veto and annual term limits
 *   - Plebeian Excluded Class: Secondary victim (powerless/trapped) — structurally locked out of magistracies by birth; no exit despite formal claims of republican equality
 *   - Serving Magistrate: Mixed agent (powerful/constrained) — enjoys annual power but faces colleague veto and term-limit loss; benefits from prestige and access
 *   - Plebeian Tribune Coalition: Organized counter-agent (organized/constrained) — increasingly mobile through democratic pressure; gains veto power over magistrates by later Republic
 *   - Senate Permanent Body: Silent beneficiary (institutional/arbitrage) — directs magistrates through auctoritas; hidden reassembly of power despite magistracy distribution
 *   - Analytical Observer: Sees both coordination (anti-monarchy function) and extraction (class privilege, plebeian exclusion)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_republican_constitution__magistracies_and_collegiality, 0.38).
domain_priors:suppression_score(roman_republican_constitution__magistracies_and_collegiality, 0.45).
domain_priors:theater_ratio(roman_republican_constitution__magistracies_and_collegiality, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_republican_constitution__magistracies_and_collegiality, extractiveness, 0.38).
narrative_ontology:constraint_metric(roman_republican_constitution__magistracies_and_collegiality, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(roman_republican_constitution__magistracies_and_collegiality, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_republican_constitution__magistracies_and_collegiality, tangled_rope).
narrative_ontology:human_readable(roman_republican_constitution__magistracies_and_collegiality, "Roman Republican Constitution: Magistracies and Collegiality").
narrative_ontology:topic_domain(roman_republican_constitution__magistracies_and_collegiality, "political/historical/constitutional").

domain_priors:requires_active_enforcement(roman_republican_constitution__magistracies_and_collegiality).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(roman_republican_constitution__magistracies_and_collegiality, '61a1520e-ac37-4adc-bce1-aed1b8839a1a').
narrative_ontology:cs_kernel_codification('61a1520e-ac37-4adc-bce1-aed1b8839a1a', distributed).
narrative_ontology:cs_authority_grounding('61a1520e-ac37-4adc-bce1-aed1b8839a1a', lineage).
narrative_ontology:cs_interpretation_layer_present('61a1520e-ac37-4adc-bce1-aed1b8839a1a').
narrative_ontology:cs_reading_relation('61a1520e-ac37-4adc-bce1-aed1b8839a1a', roman_republican_constitution__crisis_machinery, coexists_with).
narrative_ontology:cs_reading_relation('61a1520e-ac37-4adc-bce1-aed1b8839a1a', roman_republican_constitution__legal_codification_twelve_tables, coexists_with).
narrative_ontology:cs_reading_relation('61a1520e-ac37-4adc-bce1-aed1b8839a1a', roman_republican_constitution__popular_assemblies_and_tribunate, coexists_with).
narrative_ontology:cs_reading_relation('61a1520e-ac37-4adc-bce1-aed1b8839a1a', roman_republican_constitution__senate_authority, influences).
narrative_ontology:cs_axiom('61a1520e-ac37-4adc-bce1-aed1b8839a1a', foundational, executive_power_must_be_distributed).
narrative_ontology:cs_axiom_status(executive_power_must_be_distributed, holdable).
narrative_ontology:cs_axiom_grounding('61a1520e-ac37-4adc-bce1-aed1b8839a1a', executive_power_must_be_distributed, deontological).
narrative_ontology:cs_axiom('61a1520e-ac37-4adc-bce1-aed1b8839a1a', foundational, monarchy_reassembly_prevented_by_term_limits).
narrative_ontology:cs_axiom_status(monarchy_reassembly_prevented_by_term_limits, overridden).
narrative_ontology:cs_axiom_grounding('61a1520e-ac37-4adc-bce1-aed1b8839a1a', monarchy_reassembly_prevented_by_term_limits, empirically_contingent).
narrative_ontology:cs_reference_frame('61a1520e-ac37-4adc-bce1-aed1b8839a1a', distributed_executive_power_prevents_monarchy).
narrative_ontology:cs_drift_state('61a1520e-ac37-4adc-bce1-aed1b8839a1a', late_republic_triumvirate_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('61a1520e-ac37-4adc-bce1-aed1b8839a1a', '').
narrative_ontology:cs_kernel_id(roman_republican_constitution__magistracies_and_collegiality, roman_republican_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_republican_constitution__magistracies_and_collegiality, office_holding_patrician_class).
narrative_ontology:constraint_beneficiary(roman_republican_constitution__magistracies_and_collegiality, rotating_magistrate_cohorts).
narrative_ontology:constraint_victim(roman_republican_constitution__magistracies_and_collegiality, would_be_permanent_holders).
narrative_ontology:constraint_victim(roman_republican_constitution__magistracies_and_collegiality, plebeian_access_to_magistracy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED PLEBEIAN (SNARE) — Locked out of magistracies by birth status despite formal collegial equality. The veto suppresses alternatives (plebeian candidates, direct power access). Trapped without exit. Experiences maximum extraction: office-holding remains patrician privilege despite the constitution's claim of shared authority.
constraint_indexing:constraint_classification(roman_republican_constitution__magistracies_and_collegiality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SERVING MAGISTRATE (TANGLED ROPE) — Rotates through annual office with real power during tenure, but colleague's veto constrains unilateral action and term limit strips power at year's end. Mixed: benefits from access and prestige; constrained by structural checks. Genuine coordination function (distributed power prevents reassembly of kingly authority); asymmetric extraction (temporary privilege, then return to peer status).
constraint_indexing:constraint_classification(roman_republican_constitution__magistracies_and_collegiality, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PATRICIAN CLASS COLLECTIVE (ROPE) — Class-level beneficiary. All magistrates drawn from patriciate; collegiality ensures that peers rotate through high office. No one can monopolize power; all major families gain access. Benefits from the system's coordination function (prevents civil war over power concentration). Extraction is distributed within the class — a Rope classification from institutional perspective.
constraint_indexing:constraint_classification(roman_republican_constitution__magistracies_and_collegiality, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANTI-MONARCHY CONSTRAINT (MOUNTAIN) — From a civilizational view, annual terms and colleague veto are presented as immutable structural necessities: the Republic cannot tolerate kingly power; any magistrate with extended tenure or unchecked authority threatens the collective. This perspective naturalizes the institutional arrangements as inherent to republican stability. However, the engine detects this as a false summit: the structural data shows contingent institutional benefits (class privilege) and declared victims (excluded plebeians), contradicting the mountain metric gates.
constraint_indexing:constraint_classification(roman_republican_constitution__magistracies_and_collegiality, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 5: PLEBEIAN TRIBUNE COALITION (TANGLED ROPE) — Organized counter-force (tribunes, assemblies) that gains veto power over magistrates by the later Republic. Constrained by patrician institutional dominance but increasingly mobile through democratic pressure. Sees the magistracy constraint as extraction that their own veto machinery can partially counter. Mixed classification: coordinating with patrician office-holders on some issues while extracting concessions (tribunes' sacrosanctity, access to assemblies) through sustained organization.
constraint_indexing:constraint_classification(roman_republican_constitution__magistracies_and_collegiality, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SENATE'S SILENT AUTHORITY (PITON) — The Senate steers finance, war, and religion through auctoritas rather than legal command. The magistracy system appears functional and checked, but the Senate's permanent membership (senators for life, reconstituted from magistrates) functions as a hidden reassembly of power — the veto of colleagues is overridden by senatorial consensus. Theater ratio high: the annual magistracies appear to parcelize power, but Senate authority concentrates it. Piton classification reflects the degraded function: the check (colleague veto) persists, but the Senate's auctoritas has rendered it largely theatrical by the late Republic.
constraint_indexing:constraint_classification(roman_republican_constitution__magistracies_and_collegiality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Identifies both genuine coordination (distributed power prevents kingly reassembly) and asymmetric extraction (class privilege, plebeian exclusion, time-boxed authority creates status asymmetry). The constraint serves the function of preventing monarchy while preserving patrician collective dominance. Neither pure coordination nor pure extraction — the analytical view confirms the tangled_rope classification.
constraint_indexing:constraint_classification(roman_republican_constitution__magistracies_and_collegiality, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_republican_constitution__magistracies_and_collegiality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roman_republican_constitution__magistracies_and_collegiality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_republican_constitution__magistracies_and_collegiality, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(roman_republican_constitution__magistracies_and_collegiality, TR),
    TR >= 0.70.

:- end_tests(roman_republican_constitution__magistracies_and_collegiality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint delivers genuine anti-monarchy coordination — no magistrate can hold extended office or act without colleague approval. This is a real coordination benefit, not pure extraction. However, the extraction is real too: the patrician class monopolizes magistracies, and individual members experience time-boxed power as extraction when their authority vanishes at year's end. The extractiveness increases over the interval (0.22 → 0.52) as the Senate's hidden reassembly accumulates power, making the magistracy distribution increasingly theatrical. Suppression (0.45): Moderate-high. Annual terms and colleague veto suppress would-be permanent holders and plebeian candidates. But suppression is not maximal — plebeians gain tribunes' veto, and magistrates retain considerable annual power. Suppression increases over the interval as senatorial dominance requires more active enforcement to maintain the appearance of magistracy-based distribution. Theater ratio (0.35 → 0.62): The magistracy system functions with low theater initially — the checks are real and the annual cycle distributes power genuinely. But as the Senate's auctoritas outgrows magistrate legal authority, the magistracies become increasingly theatrical — they appear to govern while the Senate actually directs. The measurement trajectory reflects the piton transition: early effective coordination, late degraded ritual.
 *
 * PERSPECTIVAL GAP:
 *   The magistracies reading produces radically different classifications from different perspectives. From the powerless plebeian perspective, it is a snare — locked out by birth. From the serving magistrate's perspective, it is tangled_rope — mixing authority with constraint. From the patrician class collective's perspective, it is rope — genuine coordination that preserves class dominance. From the Senate's perspective (piton), it is increasingly theatrical. The civilizational analytical view risks seeing it as a mountain (immutable anti-monarchy structure) but the structural data reveals false summit: the beneficiary class and excluded victims contradict natural law. The plebeian coalition's organized resistance (tribunes, assemblies) transforms the constraint from snare (for individuals) to tangled_rope (for the coalition) by gaining organized exit options. This perspectival variation shows that the constraint is not a single type but a presheaf of types over the index space.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value is derived from its power level, exit options, and structural relationship to the extraction flow. Powerless plebeians locked out by birth status have maximum d (trapped to maximum extraction). Serving magistrates with annual power and colleague veto have moderate d (constrained exit moderates chi). The patrician class collective has low d (arbitrage options, distributed benefits within the class). The Senate's hidden authority has near-zero d (benefits from appearing constrained while actually directing). The plebeian coalition shifts from high d (organized but initially locked out) to lower d (constrained but gaining exit options through tribunes). The analytical observer at universal scope sees the full structure with d around 0.70 — measuring from outside all factions.
 *
 * MANDATROPHY ANALYSIS:
 *   The magistracies reading resolves mandatrophy by showing that the constraint is genuinely a tangled_rope: it coordinates power distribution (preventing monarchy) while extracting class privilege (patrician monopoly, plebeian exclusion). The reading is not snare-misclassified-as-rope, nor rope-hiding-snare. It is authentically both. The perspectival variation (snare from below, rope from class level, piton from Senate perspective, mountain from naturalizing view) confirms the mixed classification. The analytics sees this as tangled_rope because both the coordination and extraction mechanisms are real and active. The mandatrophy dissolves: the constraint is correctly identified as a hybrid, and the perspectival variation is a feature, not a bug.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collegial_veto_effectiveness,
    'Does colleague veto actually prevent power concentration, or do informal coalitions and senatorial consensus reconstruct unified authority despite the formal check?',
    'Historical analysis of magistrate overrides, senatorial intervention, and the late-Republic concentration of power (Pompeii, Caesar, Augustus). Comparison of early Republic (veto effective) vs late Republic (veto theatrical).',
    'If veto effective: colleague-veto reading sustains tangled_rope classification. If veto overcome by coalition and Senate: constraint becomes piton (theatrical check). Drift from effective to theatrical is measurable in measurement data.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collegial_veto_effectiveness, empirical, 'Whether colleague veto prevents or merely delays power concentration').

omega_variable(
    reading_monopoly_prevention_vs_class_extraction,
    'Is this reading''s core claim — that magistracies prevent monarchy — compatible with the structural data showing patrician class benefits and plebeian exclusion? Or does class extraction contradict the anti-monarchy narrative?',
    'Theoretical framework choice: (a) Monopoly prevention is the primary function; class extraction is a side effect acceptable to the system''s logic. (b) Class extraction is the primary function; anti-monarchy rhetoric is the legitimating cover. (c) Both are equally primary — tangled_rope correctly captures both.',
    'Framework (a): reading emphasizes anti-monarchy, downplays class extraction. Framework (b): reading becomes snare-adjacent. Framework (c): tangled_rope is confirmed as the correct classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_monopoly_prevention_vs_class_extraction, conceptual, 'Tension between anti-monarchy prevention and patrician class extraction as primary functions').

omega_variable(
    sibling_reading_empirical_status,
    'Which of the five sibling readings has the strongest historical support? Can all five readings coexist, or do some foreclose others?',
    'Historiographic analysis of each reading''s grounding in primary sources (Polybius, Livy, Cicero). Assess whether each reading describes a real structural component or a rhetorical cover for one of the others.',
    'If all coexist: all five are reading_relations=coexists_with. If crisis_machinery empirically forecloses the anti-monarchy reading: shift to reading_relations=forecloses. If Senate_authority empirically explains magistracy collegiality: reading_relations become influences rather than coexists_with.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_empirical_status, empirical, 'Whether all five kernel readings coexist or some foreclose others').

omega_variable(
    annual_term_extraction_window,
    'What is the optimal annual term length for preventing reassembly without creating excessive extractive pressure through rapid power cycling?',
    'Institutional analysis of magistrate behavior across term lengths; measurement of policy instability, corruption incentives, and power-seeking behavior as term length varies.',
    'Shorter terms reduce extraction opportunity but may increase instability and short-termism. Longer terms enable better governance but increase reassembly risk. This omega grounds the measurement trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(annual_term_extraction_window, empirical, 'Optimal annual term length for anti-reassembly function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_republican_constitution__magistracies_and_collegiality, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rom_mag_tr_t0, roman_republican_constitution__magistracies_and_collegiality, theater_ratio, 0, 0.15).
narrative_ontology:measurement(rom_mag_tr_t150, roman_republican_constitution__magistracies_and_collegiality, theater_ratio, 150, 0.35).
narrative_ontology:measurement(rom_mag_tr_t300, roman_republican_constitution__magistracies_and_collegiality, theater_ratio, 300, 0.62).

% Extraction over time
narrative_ontology:measurement(rom_mag_be_t0, roman_republican_constitution__magistracies_and_collegiality, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(rom_mag_be_t150, roman_republican_constitution__magistracies_and_collegiality, base_extractiveness, 150, 0.38).
narrative_ontology:measurement(rom_mag_be_t300, roman_republican_constitution__magistracies_and_collegiality, base_extractiveness, 300, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(rom_mag_su_t0, roman_republican_constitution__magistracies_and_collegiality, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(rom_mag_su_t150, roman_republican_constitution__magistracies_and_collegiality, suppression_requirement, 150, 0.45).
narrative_ontology:measurement(rom_mag_su_t300, roman_republican_constitution__magistracies_and_collegiality, suppression_requirement, 300, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_republican_constitution__magistracies_and_collegiality, enforcement_mechanism).
narrative_ontology:affects_constraint(roman_republican_constitution__magistracies_and_collegiality, roman_republican_constitution__crisis_machinery).
narrative_ontology:affects_constraint(roman_republican_constitution__magistracies_and_collegiality, roman_republican_constitution__senate_authority).
narrative_ontology:affects_constraint(roman_republican_constitution__magistracies_and_collegiality, roman_republican_constitution__popular_assemblies_and_tribunate).

% DUAL FORMULATION NOTE:
% The magistracies_and_collegiality reading is one component of a contested kernel (roman_republican_constitution). All five readings share the same historical period and primary sources but emphasize different structural elements: magistracies emphasize executive separation-of-powers; crisis_machinery emphasizes suspension mechanisms; legal_codification emphasizes written law; popular_assemblies emphasize democratic sovereignty; senate_authority emphasizes permanent bureaucratic direction. Each reading has its own constraint story with its own extractiveness value. They are linked through network.affects_constraints to show family relationships, not to collapse them into a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(roman_republican_constitution__magistracies_and_collegiality, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
