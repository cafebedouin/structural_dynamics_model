% ============================================================================
% CONSTRAINT STORY: salic_prohibition__immutable_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__immutable_mandate_reading, []).

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
 *   constraint_id: salic_prohibition__immutable_mandate_reading
 *   human_readable: Salic Law as Immutable Divine Mandate in Dynastic Succession
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   Salic Law in Western European dynastic constitutions is presented by this
 *   reading as an immutable divine or natural mandate excluding women from
 *   succession. The constraint instantiates agnatic primogeniture as both a
 *   coordination mechanism (solving succession disputes) and an extraction
 *   mechanism (concentrating inheritance rights in male heirs while
 *   suppressing female claimants and alternative legitimacy framings). This
 *   reading is one of three structurally distinct interpretations of the
 *   Salic kernel: the immutable-mandate reading (this constraint) holds that
 *   Salic Law cannot be overridden or revised; the sovereign-override reading
 *   holds that the sovereign retains authority to modify succession rules;
 *   the cognatic-reversion reading holds that female succession is the
 *   natural or original state and Salic Law is a contingent overlay. Each
 *   reading produces a different constraint with different ε values and
 *   different structural implications. The immutable-mandate reading
 *   generates high extractiveness (0.68) and high suppression (0.82) because
 *   it forecloses alternatives and justifies military enforcement. The
 *   constraint exhibits all six DR types depending on perspective — but the
 *   immutable-mandate reading's mountain classification (from the analytical
 *   observer) is a false summit: identifiable beneficiaries (agnatic male
 *   heirs, patriarchal nobility) exist, triggering FSM evaluation and
 *   revealing the doctrine as a naturalized contingent arrangement rather
 *   than genuine natural law.
 *
 * KEY AGENTS:
 *   - Female Claimants/Heirs: Primary victims (powerless/trapped) — excluded from succession by law they cannot escape, bearing the cost of dynastic consolidation
 *   - Agnatic Male Heirs: Primary beneficiaries (institutional/arbitrage) — gain exclusive succession rights, experience constraint as coordination mechanism
 *   - Patriarchal Nobility Class: Secondary beneficiary (organized/constrained) — benefit from agnatic transmission rules, bear enforcement costs of suppressing female claims
 *   - Cognatic Succession Advocates: Secondary victims (moderate/constrained) — face suppression of alternative legitimacy framings, constrained by doctrinal authority and military imbalance
 *   - Church/Theological Authority: Doctrinal maintainer (institutional/arbitrage) — maintains immutability framing; experiences constraint as performative ritual increasingly separated from secular political practice
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent institutional arrangement as immutable law; the false summit detector reveals the structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, 0.68).
domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, 0.82).
domain_priors:theater_ratio(salic_prohibition__immutable_mandate_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__immutable_mandate_reading, snare).
narrative_ontology:human_readable(salic_prohibition__immutable_mandate_reading, "Salic Law as Immutable Divine Mandate in Dynastic Succession").
narrative_ontology:topic_domain(salic_prohibition__immutable_mandate_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__immutable_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__immutable_mandate_reading, 'f8c85762-d8b5-421c-bb19-0c55fff0f928').
narrative_ontology:cs_kernel_codification('f8c85762-d8b5-421c-bb19-0c55fff0f928', formalized).
narrative_ontology:cs_authority_grounding('f8c85762-d8b5-421c-bb19-0c55fff0f928', extraction).
narrative_ontology:cs_interpretation_layer_present('f8c85762-d8b5-421c-bb19-0c55fff0f928').
narrative_ontology:cs_reading_relation('f8c85762-d8b5-421c-bb19-0c55fff0f928', salic_prohibition__sovereign_override_reading, forecloses).
narrative_ontology:cs_reading_relation('f8c85762-d8b5-421c-bb19-0c55fff0f928', salic_prohibition__cognatic_reversion_reading, forecloses).
narrative_ontology:cs_axiom('f8c85762-d8b5-421c-bb19-0c55fff0f928', foundational, female_succession_divinely_prohibited).
narrative_ontology:cs_axiom_status(female_succession_divinely_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('f8c85762-d8b5-421c-bb19-0c55fff0f928', female_succession_divinely_prohibited, theological).
narrative_ontology:cs_axiom('f8c85762-d8b5-421c-bb19-0c55fff0f928', foundational, agnatic_exclusion_immutable_law).
narrative_ontology:cs_axiom_status(agnatic_exclusion_immutable_law, holdable).
narrative_ontology:cs_axiom_grounding('f8c85762-d8b5-421c-bb19-0c55fff0f928', agnatic_exclusion_immutable_law, deontological).
narrative_ontology:cs_reference_frame('f8c85762-d8b5-421c-bb19-0c55fff0f928', immutable_agnatic_succession_mandate).
narrative_ontology:cs_drift_state('f8c85762-d8b5-421c-bb19-0c55fff0f928', reformation_enlightenment_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f8c85762-d8b5-421c-bb19-0c55fff0f928', '2026-02-26T14:23:00Z').
narrative_ontology:cs_kernel_id(salic_prohibition__immutable_mandate_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnatic_male_heirs).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, patriarchal_nobility).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, female_claimants).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, cognatic_succession_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED FEMALE HEIR (SNARE) — Structurally trapped. Birth into the dynasty offers no succession right; dynastic law interprets her as constitutionally incapable of legitimate rule. No exit option: cannot renounce dynasty (identity is constituted through it) nor claim throne (law forbids it). Suppression is enforced through military capability — challenges to female succession justify preventive war. Maximum extraction: loses inheritance rights while bearing the symbolic weight of dynastic legitimacy through marriage alliances.
constraint_indexing:constraint_classification(salic_prohibition__immutable_mandate_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COGNATIC SUCCESSION ADVOCATES (SNARE) — Constrained by military imbalance and doctrinal authority. Cognatic succession (female heirs included) is theoretically defensible but labeled as violation of divine law. Advocates face military enforcement, excommunication threats, and delegitimation. Exit exists at high cost: accept agnatic exclusion or risk destructive succession war. The constraint suppresses alternative legitimacy framings.
constraint_indexing:constraint_classification(salic_prohibition__immutable_mandate_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AGNATIC MALE HEIRS (ROPE) — Institutional beneficiary with full arbitrage. Salic Law solves a genuine coordination problem: unambiguous succession line eliminates dispute over legitimacy and prevents dynastic fragmentation. The beneficiary experiences the constraint as coordination — it allocates succession rights cleanly and efficiently. Zero suppression experienced — the law aligns with the beneficiary's interests. The constraint is not experienced as coercive from this position.
constraint_indexing:constraint_classification(salic_prohibition__immutable_mandate_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PATRIARCHAL NOBILITY CLASS (TANGLED ROPE) — Organized collective that benefits from agnatic rules but also faces constraints. Salic Law preserves male property transmission and prevents partition through female heiresses. Genuine coordination: predictable inheritance enables long-term estate planning. But enforcement requires military capacity to suppress female claimants and cognatic advocates — suppression feeds extraction. Secondary beneficiary status: benefits from succession clarity but also bears enforcement costs.
constraint_indexing:constraint_classification(salic_prohibition__immutable_mandate_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CHURCH/DOCTRINAL AUTHORITY (PITON) — Maintains the immutable-mandate framing through theological authority. Theater ratio (0.55) reflects that doctrinal grounding persists despite contradictions (female saints, prophetesses, Joan of Arc). The church functions as the performance authority — it declares Salic Law divine and immutable even when faced with counterexamples. The doctrine persists through institutional inertia rather than compelling logical force. As secular constitutional theory develops, church authority degrades without the church explicitly renouncing the doctrine.
constraint_indexing:constraint_classification(salic_prohibition__immutable_mandate_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW FRAMING (MOUNTAIN — FALSE SUMMIT) — The immutable-mandate reading claims Salic Law reflects natural law or divine immutability. From this perspective, agnatic succession is inevitable and unchangeable — it flows from the structure of dynasties themselves. However, the beneficiary analysis reveals identifiable actors who benefit from this 'natural' law (agnatic heirs, patriarchal nobility). The classification signals a false summit: the engine detects beneficiaries on a mountain, triggering FSM evaluation and revealing that the 'immutable' framing naturalizes a contingent institutional arrangement.
constraint_indexing:constraint_classification(salic_prohibition__immutable_mandate_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__immutable_mandate_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(salic_prohibition__immutable_mandate_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(salic_prohibition__immutable_mandate_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(salic_prohibition__immutable_mandate_reading, TR),
    TR >= 0.70.

:- end_tests(salic_prohibition__immutable_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint concentrates inheritance rights in agnatic male heirs while excluding female heirs entirely from succession consideration. Female claimants lose dynastic inheritance despite birth into the dynasty. The extraction is not total (agnatic males do experience genuine coordination benefits) but is substantial enough to classify as snare from the victim perspective. Over the interval (0 to 4 time units), extractiveness rises from 0.58 to 0.68 as enforcement machinery hardens and challenges accumulate, forcing stronger doctrinal defense. Suppression (0.82): Very high. The constraint suppresses female succession claims through military enforcement, theological prohibition, and legal incapacity. Challengers to the law risk excommunication, military defeat, and delegitimation. The measurement shows rising suppression from 0.75 to 0.82 as enforcement infrastructure develops and doctrine hardens against challenges. Theater ratio (0.55): Moderate-high. The doctrine's immutability is partly performative — theological texts invoke divine will, but practical succession disputes reveal flexibility and negotiation. The doctrine functions as a legitimacy narrative that justifies enforcement rather than as a self-executing natural law. Theater rises slightly from 0.48 to 0.55 as the immutability claim is invoked more frequently to defend against challenges, indicating increasing performative content relative to functional operation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival disagreement. Agnatic male heirs see Rope — a coordination mechanism that solves succession disputes. Female claimants see Snare — pure extraction with suppression. The patriarchal nobility sees Tangled Rope — coordination benefits mixed with enforcement costs. Cognatic advocates see Snare — suppression of an alternative coordination mechanism. The church sees Piton — maintaining a doctrine that no longer compels but persists through institutional inertia. The analytical observer risks seeing Mountain — treating immutable mandate as natural law — but the beneficiary analysis reveals this as a false summit. The core gap: beneficiaries experience coordination; victims experience extraction; the doctrine frames both as natural/immutable.
 *
 * DIRECTIONALITY LOGIC:
 *   The immutable-mandate reading's directionality structure differs from sibling readings in one critical way: it treats female succession as logically impossible, not merely as a different policy choice. This affects d for female claimants: in the immutable-mandate reading, d ~ 0.95 (full victim of an immutable constraint); in the sovereign-override reading, d ~ 0.75 (victim of a policy choice the sovereign could reverse); in the cognatic-reversion reading, d ~ 0.50 (symmetric position in a legitimacy dispute). The shift in d drives different χ values and classifications. The immutable-mandate framing also justifies military enforcement as defense of immutable law; the sovereign-override framing treats succession disputes as administrative matters; the cognatic-reversion framing treats enforcement as unjust suppression of the true rule.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The immutable-mandate reading resolves mandatrophy by explicitly coupling the coordination function (succession clarity) to the extraction mechanism (agnatic exclusion). The beneficiary's rope classification reflects the genuine coordination service: unambiguous succession prevents dynastic fragmentation. But the constraint's snare classification from the victim perspective reveals that the coordination service is structurally coupled to asymmetric extraction — you cannot get the coordination without getting the exclusion. The immutable doctrine naturalizes this coupling, making it appear immutable when it is actually contingent. The mandatrophy resolves when the reading acknowledges that the constraint does BOTH coordination and extraction, and that the immutability framing prevents questioning whether the coupling is necessary. If cognatic succession could provide equal coordination with less extraction, the mandate's necessity falls away. The omega variables address this: replication of cognatic systems' coordination outcomes, enforcement-dependency analysis, and assessment of whether alternatives exist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_vs_strategic_fiction,
    'Is the claim that Salic Law reflects divine immutable will a genuine theological commitment, or a strategic fiction deployed to prevent challenge?',
    'Historical analysis of theological argumentation before and after female succession challenges; examination of whether the doctrine shifts when challenged vs. held constant; comparison to theological treatment of other gender-based rules (e.g., female clergy, female witness testimony) to assess consistency',
    'If genuine commitment: the constraint is more robustly entrenched, and overturning it requires theological refutation. If strategic fiction: the constraint''s immutability depends entirely on enforcement capacity; erosion of enforcement reveals the constructed nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_mandate_vs_strategic_fiction, empirical, 'Whether Salic Law immutability is genuine theological commitment or strategic doctrine').

omega_variable(
    cognatic_viability_as_coordination,
    'Could cognatic succession (female heirs included) provide equally stable succession coordination as Salic agnation?',
    'Historical comparative analysis: cognatic succession systems (e.g., Portuguese, Spanish, later French practice with female heirs in collateral lines) vs. strict agnatic systems; measurement of succession disputes, dynastic fragmentation, and civil war frequency under each rule',
    'If cognatic equally viable: Salic Law is revealed as one coordination mechanism among several, not the sole solution. The constraint is contingent, not immutable. If cognatic is empirically worse: the immutable-mandate reading gains support — the law solves a problem that alternatives cannot.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cognatic_viability_as_coordination, empirical, 'Whether cognatic succession provides comparable coordination stability').

omega_variable(
    enforcement_capacity_as_constraint_substrate,
    'Does the immutability of Salic Law depend on military capacity to suppress female claimants, such that constraint durability is structural (enforcement-dependent) rather than doctrinal?',
    'Analysis of successful female succession challenges and their correlation with military imbalance or breakdown in enforcement capacity; examination of cases where female claimants prevailed or forced succession revision when enforcement machinery weakened',
    'If enforcement-dependent: the constraint is not immutable — it is sustained Snare. Erosion of enforcement capacity (rise of nation-states with stronger militaries than dynastic houses, development of secular legal alternatives) undermines the constraint regardless of doctrinal commitment. If doctrinal: military capacity is secondary to theological authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_as_constraint_substrate, empirical, 'Degree to which Salic Law constraint depends on enforcement capacity vs. doctrinal authority').

omega_variable(
    kernel_reading_identity_ambiguity,
    'What distinguishes the immutable-mandate reading from the sovereign-override reading? Does the immutable-mandate reading claim the sovereign CANNOT override Salic Law, or merely that they SHOULD NOT?',
    'Historical and doctrinal analysis: cases where sovereigns claimed override authority; responses from authorities invoking Salic Law; doctrinal texts claiming absolute vs. presumptive immutability; examination of whether immutability is framed as logical/divine constraint vs. normative obligation',
    'If logical/divine constraint: immutable-mandate forecloses sovereign-override (logically incompatible). If normative obligation: immutable-mandate coexists with sovereign-override (different parties hold different readings). This determines reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity_ambiguity, conceptual, 'Whether immutable-mandate reading claims logical or normative immutability').

omega_variable(
    female_agency_suppression_mechanism,
    'Is suppression of female succession claims enforced through external barriers (military, legal prohibition, excommunication) or through internalized identity constraints (women accept doctrine as reflecting their nature)?',
    'Analysis of female claimants'' narratives, framing, and resistance strategies; examination of whether female heirs challenged succession exclusion when external enforcement weakened; assessment of female advocacy for or against Salic Law',
    'If external barriers: suppression is structural; erosion of enforcement undermines suppression. If internalized: suppression persists even after external barriers fall — female heirs may internalize the doctrine and accept exclusion. This affects the measurement trajectory of suppression over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(female_agency_suppression_mechanism, empirical, 'Whether suppression is externally enforced or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__immutable_mandate_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(salic_tr_t0, salic_prohibition__immutable_mandate_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(salic_tr_t2, salic_prohibition__immutable_mandate_reading, theater_ratio, 2, 0.51).
narrative_ontology:measurement(salic_tr_t4, salic_prohibition__immutable_mandate_reading, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(salic_be_t0, salic_prohibition__immutable_mandate_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(salic_be_t2, salic_prohibition__immutable_mandate_reading, base_extractiveness, 2, 0.63).
narrative_ontology:measurement(salic_be_t4, salic_prohibition__immutable_mandate_reading, base_extractiveness, 4, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(salic_su_t0, salic_prohibition__immutable_mandate_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(salic_su_t2, salic_prohibition__immutable_mandate_reading, suppression_requirement, 2, 0.8).
narrative_ontology:measurement(salic_su_t4, salic_prohibition__immutable_mandate_reading, suppression_requirement, 4, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__immutable_mandate_reading, resource_allocation).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__sovereign_override_reading).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__cognatic_reversion_reading).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, dynastic_legitimacy_framework).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, female_political_agency_suppression).

% DUAL FORMULATION NOTE:
% Salic Law exists in three constraint stories corresponding to three readings of the kernel. The immutable-mandate reading (this constraint, ε=0.68, snare) treats Salic Law as unchangeable divine law. The sovereign-override reading treats it as mutable policy subject to sovereign authority. The cognatic-reversion reading treats female succession as the natural original state. These are not three measurements of the same constraint — they are three structurally distinct constraints instantiating different claims about the same kernel. Each story has its own ε, classification, and implications. Network links connect siblings: each reading is downstream of the salic_prohibition kernel and influences the others through doctrinal and institutional pressure. The immutable-mandate reading forecloses the sovereign-override reading within any single framework that accepts divine immutability — but different parties hold different readings simultaneously, so they coexist institutionally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
