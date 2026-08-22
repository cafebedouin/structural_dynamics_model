% ============================================================================
% CONSTRAINT STORY: salic_prohibition__immutable_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: salic_prohibition__immutable_mandate_reading
 *   human_readable: Salic Law as Irrevocable Natural/Divine Mandate Barring Female Succession
 *   domain: constitutional/dynastic/political
 *
 * SUMMARY:
 *   This constraint models the reading of Salic Law that treats the
 *   agnatic-only succession bar as an irrevocable natural or divine law woven
 *   into the fundamental constitution of the dynasty — not a policy choice
 *   but a discovered feature of legitimate order, immune to sovereign
 *   revision or territorial variation. Under this reading, any female claim
 *   to the throne is not merely disfavored but structurally void from
 *   inception, which makes armed resistance to a female or cognatic
 *   succession not merely permissible but a defense of constitutional order
 *   itself. The theater rises over time as the doctrine's 'immemorial'
 *   character is progressively elaborated by jurists long after its actual
 *   (and much narrower) medieval origin, papering over the fact that its
 *   application to royal succession was a retroactive construction serving a
 *   specific 14th-century claimant.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, 0.68).
domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, 0.79).
domain_priors:theater_ratio(salic_prohibition__immutable_mandate_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__immutable_mandate_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__immutable_mandate_reading, "Salic Law as Irrevocable Natural/Divine Mandate Barring Female Succession").
narrative_ontology:topic_domain(salic_prohibition__immutable_mandate_reading, "constitutional/dynastic/political").

domain_priors:requires_active_enforcement(salic_prohibition__immutable_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__immutable_mandate_reading, '144036b1-b427-4a93-8295-dbb2d9949d8a').
narrative_ontology:cs_kernel_codification('144036b1-b427-4a93-8295-dbb2d9949d8a', formalized).
narrative_ontology:cs_authority_grounding('144036b1-b427-4a93-8295-dbb2d9949d8a', lineage).
narrative_ontology:cs_interpretation_layer_present('144036b1-b427-4a93-8295-dbb2d9949d8a').
narrative_ontology:cs_reading_relation('144036b1-b427-4a93-8295-dbb2d9949d8a', salic_prohibition__sovereign_override_reading, forecloses).
narrative_ontology:cs_reading_relation('144036b1-b427-4a93-8295-dbb2d9949d8a', salic_prohibition__cognatic_reversion_reading, forecloses).
narrative_ontology:cs_axiom('144036b1-b427-4a93-8295-dbb2d9949d8a', foundational, agnatic_succession_as_constitutive_natural_order).
narrative_ontology:cs_axiom_status(agnatic_succession_as_constitutive_natural_order, holdable).
narrative_ontology:cs_axiom_grounding('144036b1-b427-4a93-8295-dbb2d9949d8a', agnatic_succession_as_constitutive_natural_order, deontological).
narrative_ontology:cs_axiom('144036b1-b427-4a93-8295-dbb2d9949d8a', secondary, preventive_war_as_constitutional_defense).
narrative_ontology:cs_axiom_status(preventive_war_as_constitutional_defense, holdable).
narrative_ontology:cs_axiom_grounding('144036b1-b427-4a93-8295-dbb2d9949d8a', preventive_war_as_constitutional_defense, instrumental).
narrative_ontology:cs_reference_frame('144036b1-b427-4a93-8295-dbb2d9949d8a', agnatic_natural_order_framework).
narrative_ontology:cs_drift_state('144036b1-b427-4a93-8295-dbb2d9949d8a', post_enlightenment_constitutionalism, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('144036b1-b427-4a93-8295-dbb2d9949d8a', '').
narrative_ontology:cs_kernel_id(salic_prohibition__immutable_mandate_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnatic_male_claimants).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, collateral_male_dynastic_branches).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, clergy_ratifying_agnatic_theology).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, female_heirs_and_their_lineages).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, cognatic_claimant_territories).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, populations_subjected_to_succession_wars).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, agnatic_priority_as_natural_order).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, fundamental_law_of_the_realm_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold or stand to inherit the throne solely because female lines and their issue are categorically barred. They commission jurists and theologians to declare the exclusion a fundamental, unamendable law of the realm rather than a policy choice, and they mobilize armies to enforce it against cognatic claims.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, agnatic_male_claimants, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__immutable_mandate_reading, agnatic_male_claimants, agenda_setter).

% Possess seniority of blood, sometimes explicit designation by a predecessor, yet are removed from the succession by declared fundamental law regardless of personal merit or prior promise. Their only recourse is dynastic marriage alliance, litigation before bodies controlled by the beneficiary faction, or war — all of which the doctrine's 'irrevocable' framing is designed to foreclose.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, female_heirs_and_their_lineages, payer,
    powerful, biographical, trapped, national).

% Neighboring or annexed territories whose own inheritance customs permit female or cognatic succession find their rulers' claims delegitimized the moment those claims touch the excluding realm's throne. They bear the costs of wars fought to enforce agnatic priority against their sovereigns.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, cognatic_claimant_territories, payer,
    powerful, generational, constrained, continental).

% Peasants, townspeople, and soldiers who pay in taxation, conscription, and devastation when agnatic priority is defended by force against a cognatic or female claim. They have no voice in the succession dispute and no ability to exit the territory being fought over.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, populations_subjected_to_succession_wars, payer,
    powerless, biographical, trapped, regional).

% Ecclesiastical authorities who supply the theological and natural-law vocabulary declaring the exclusion divinely or naturally ordained, in exchange for royal patronage, land grants, and doctrinal deference from the dynasty they legitimize.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, clergy_ratifying_agnatic_theology, beneficiary,
    institutional, generational, arbitrage, national).

% Distant male cousins who would have no plausible claim under cognatic or elective systems become viable successors purely because the female-line seniors ahead of them are excluded by the doctrine. They have every incentive to defend the rule's 'immutability' even when personally distant from the current crisis.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, collateral_male_dynastic_branches, beneficiary,
    powerful, generational, mobile, continental).

% Legal historians and comparative constitutionalists who examine whether the rule's claimed antiquity and divine sanction withstand documentary scrutiny, and who trace how 'fundamental law' rhetoric was retrofitted onto a much narrower and later medieval custom.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, constitutional_jurists, observer,
    analytical, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, non-negotiable succession rule that forecloses ambiguity and (in principle) prevents contested claims from multiple lines destabilizing the dynasty at the moment of a ruler's death.
% TRANSFER_FUNCTION: Moves the crown, its territories, and its revenues away from senior female heirs and cognatic lines toward junior or collateral male claimants; moves the cost of enforcing that transfer onto contested territories and the populations conscripted or taxed to fight succession wars.
% ABSENT_VOICES: The excluded female heirs themselves are rarely present in the councils, church synods, or juristic tribunals that declare the exclusion 'fundamental' and 'immemorial' — the doctrine is authored and ratified entirely by parties who benefit from it. Populations who bear the war costs have no seat in the succession dispute at all.
% DISAPPEARANCE_RATIONALE: If the immutable-mandate framing collapsed, the throne would pass to the senior heir regardless of sex or line, collateral male branches would lose their inherited expectancy, clergy would lose a doctrinal service they currently sell to the crown, and the entire apparatus of preventive war used to enforce agnatic priority would lose its legal justification overnight.
% FOUNDING_PROBLEM: Originally a narrow Frankish inheritance custom (concerning land, not necessarily the crown) meant to prevent fragmentation of family holdings among multiple heirs and in-laws; later retrofitted as a claimed constitutional bar to female succession specifically to resolve a contested royal succession crisis in favor of the male claimant already in possession.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the beneficiary dynasties (comparative constitutional scholars examining the 14th-century jurists' citations) have documented that the 'immemorial fundamental law' claim was constructed and popularized only after, and specifically to justify, a particular male claimant's seizure of the throne over a female-line rival — the doctrine's own foundational problem (preventing land fragmentation) had nothing to do with royal succession and was already obsolete by the time the crown-succession claim was made. No corroboration exists from any source belonging to the beneficiary factions themselves that predates the political crisis it was invoked to resolve.
narrative_ontology:disappearance_verdict(salic_prohibition__immutable_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__immutable_mandate_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__immutable_mandate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(salic_prohibition__immutable_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__immutable_mandate_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__immutable_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__immutable_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.42 to 0.68 as the doctrine hardens from ad hoc justification into settled constitutional dogma — the later the point in the interval, the more thoroughly female claimants and cognatic territories are foreclosed by an apparatus that started as a contingent legal argument. Suppression rises sharply and then plateaus (0.50 to 0.79) reflecting the transition from persuasion (juristic argument, church endorsement) to coercion (standing armies committed to preventive war whenever a female or cognatic claim arises). Theater ratio rises steadily as more scholarly and theological apparatus is built to assert antiquity and naturalness the doctrine did not originally possess — this is Goodhart drift: the proxy (claimed immemorial pedigree) increasingly substitutes for the real function (orderly succession), since orderly succession could equally be achieved by seniority-blind or cognatic rules.
 *
 * DIRECTIONALITY LOGIC:
 *   Agnatic male claimants, collateral branches, and the ratifying clergy sit at the beneficiary end: the doctrine manufactures their claim, expectancy, or doctrinal utility out of what would otherwise be a weaker or nonexistent position. Female heirs are the paradigm target — trapped by identity (their sex is the excluded category itself, not a contingent circumstance they could alter), which is why exit_options is 'trapped' rather than 'constrained': there is no maneuver available to a female heir that escapes the categorical bar. Populations subjected to succession wars are powerless payers with no stake in the doctrinal dispute at all, bearing costs generated entirely by others' claims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/status mismatch (dead + world_rearranges) is the diagnostic core of this story: the doctrine's original function — preventing fragmentation of Frankish family land — was already irrelevant to royal succession by the time it was invoked to bar a female claimant, yet the arrangement not only persisted but hardened into a claimed 'fundamental law of the realm.' This is a textbook capture/zombie signature: a coordination story (orderly, undisputed succession) draped over what is structurally continued extraction (excluding a class of legitimate claimants) sustained by force. Classifying this as tangled_rope rather than pure snare preserves the genuine coordination function the doctrine also serves — reducing the field of claimants does lower the objective risk of multi-claimant civil war — while the enforcement requirement and clearly identified victim class keep the extraction visible rather than laundering it entirely into 'natural order.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is the agnatic-only succession bar a genuine feature of natural/divine order predating and independent of the succession dispute it is invoked to resolve, or was it constructed and retrofitted specifically to legitimize a particular male claimant''s seizure of the throne?',
    'Documentary and juristic-historical analysis: trace the citation chain of the ''fundamental law'' claim backward to determine whether sources asserting its immemorial and natural character predate or postdate the specific succession crisis in which it was first invoked to bar a female claimant.',
    'If the doctrine demonstrably postdates and was constructed for the crisis it purports to predate, the ''mountain'' framing this reading depends on is false, and the arrangement is better classified as a tangled_rope or outright snare wearing constitutional-law language; if genuinely antecedent, the immutable-mandate reading has stronger structural grounding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, empirical, 'Whether the claimed antiquity of agnatic exclusion is genuine or retroactively manufactured.').

omega_variable(
    kernel_reading_indeterminacy,
    'Which of the three competing readings of the Salic prohibition kernel (immutable mandate, sovereign-revisable positive law, or non-binding Frankish anachronism) actually governed contemporary political practice at any given moment, given that all three were argued simultaneously by different factions?',
    'Comparative analysis of which reading prevailed in specific succession crises versus which reading was merely argued by the losing faction — actual outcomes reveal which reading had operative force, as opposed to which reading was merely rhetorically available.',
    'If the sovereign_override_reading in fact prevailed in practice (e.g., sovereigns successfully legislated exceptions), this reading''s claim to irrevocability is descriptively false even though it was the dominant rhetorical framing — this would not change this story''s own ε (which is authored to this reading''s own lights) but would clarify why sibling readings coexist rather than one foreclosing the others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Which kernel reading actually governed succession outcomes versus which was merely argued.').

omega_variable(
    coalition_potential_of_excluded_lines,
    'Could female heirs and cognatic claimant territories have formed an effective coalition to jointly resist the agnatic exclusion, given their shared structural interest, despite differences in power level (powerful vs. powerless populations)?',
    'Historical case analysis of instances where female claimants allied with foreign cognatic powers against agnatic incumbents, assessing whether such coalitions achieved durable success or were consistently defeated by the collateral branches'' military and doctrinal advantages.',
    'If such coalitions were structurally viable but rarely realized, that supports treating the suppression as partly a coordination-failure problem among victims rather than pure enforcement strength on the beneficiary side.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_potential_of_excluded_lines, empirical, 'Whether excluded female heirs and cognatic territories had viable coalition options against agnatic enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__immutable_mandate_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__immutable_mandate_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sali_tr_t100, salic_prohibition__immutable_mandate_reading, theater_ratio, 100, 0.28).
narrative_ontology:measurement(sali_tr_t200, salic_prohibition__immutable_mandate_reading, theater_ratio, 200, 0.36).
narrative_ontology:measurement(sali_tr_t300, salic_prohibition__immutable_mandate_reading, theater_ratio, 300, 0.4).
narrative_ontology:measurement(sali_tr_t400, salic_prohibition__immutable_mandate_reading, theater_ratio, 400, 0.41).
narrative_ontology:measurement(sali_tr_t500, salic_prohibition__immutable_mandate_reading, theater_ratio, 500, 0.42).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__immutable_mandate_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sali_be_t100, salic_prohibition__immutable_mandate_reading, base_extractiveness, 100, 0.55).
narrative_ontology:measurement(sali_be_t200, salic_prohibition__immutable_mandate_reading, base_extractiveness, 200, 0.63).
narrative_ontology:measurement(sali_be_t300, salic_prohibition__immutable_mandate_reading, base_extractiveness, 300, 0.66).
narrative_ontology:measurement(sali_be_t400, salic_prohibition__immutable_mandate_reading, base_extractiveness, 400, 0.68).
narrative_ontology:measurement(sali_be_t500, salic_prohibition__immutable_mandate_reading, base_extractiveness, 500, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__immutable_mandate_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(sali_su_t100, salic_prohibition__immutable_mandate_reading, suppression_requirement, 100, 0.68).
narrative_ontology:measurement(sali_su_t200, salic_prohibition__immutable_mandate_reading, suppression_requirement, 200, 0.75).
narrative_ontology:measurement(sali_su_t300, salic_prohibition__immutable_mandate_reading, suppression_requirement, 300, 0.78).
narrative_ontology:measurement(sali_su_t400, salic_prohibition__immutable_mandate_reading, suppression_requirement, 400, 0.79).
narrative_ontology:measurement(sali_su_t500, salic_prohibition__immutable_mandate_reading, suppression_requirement, 500, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__immutable_mandate_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(salic_prohibition__immutable_mandate_reading, 0.1).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, sovereign_override_reading).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the salic_prohibition kernel. immutable_mandate_reading treats the exclusion as constitutive and unamendable (ε=0.68, tangled_rope, enforcement mandatory); sovereign_override_reading treats the identical textual/customary kernel as ordinary revisable positive law (expected lower suppression, since a sovereign remedy exists in principle); cognatic_reversion_reading treats it as a Frankish parochial custom never properly extended to non-Frankish territories (expected much lower ε for those territories, since the doctrine's writ is denied entirely). The three do not average into one ε — each is a structurally distinct constraint sharing a textual kernel but diverging in enforcement modality, victim scope, and remedy availability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
