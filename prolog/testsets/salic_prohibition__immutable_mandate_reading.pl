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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Salic Law as Immutable Natural/Divine Mandate
 *   domain: constitutional/dynastic
 *
 * SUMMARY:
 *   Salic Law, a Frankish code traditionally forbidding female succession,
 *   became crystallized in medieval constitutional theory as an immutable
 *   natural and divine law — not a contingent policy choice but an
 *   unchangeable feature of the human and cosmic order. Under this reading,
 *   female exclusion from succession is not an enforcement choice but a
 *   recognition of natural necessity. The claim/metric gap is deliberate: the
 *   constraint is CLAIMED as mountain (natural law) while metrics show
 *   substantial extractiveness and active suppression — a false summit
 *   candidate. The measured extraction (0.68) reflects the benefits accruing
 *   to agnatic males from exclusionary rules; the measured suppression (0.76)
 *   reflects the institutional machinery deployed to prevent female
 *   succession; the measured theater (0.42) reflects the performative
 *   constitutional rhetoric that casts enforcement as mere acknowledgment of
 *   what nature demands. The engine will detect this divergence.
 *
 * KEY AGENTS:
 *   - agnatic_male_heirs: Beneficiaries of the constraint; their succession rights are treated as grounded in nature itself, shielding them from accountability for exclusion.
 *   - female_claimants: Systematically excluded; challenging their exclusion is framed as transgression against nature/divinity.
 *   - cognatic_succession_advocates: Organized opposition to the agnatic-only rule; delegitimized by the immutable-law framing.
 *   - territorial_rulers: Enforce the constraint while claiming no choice — mere executors of natural law, not architects of exclusion.
 *   - ecclesiastical_authority: Benefits from the natural/divine framing by certifying succession without bearing responsibility for exclusion.
 *   - historical_record_keepers: Analytical observers who document the constraint's operation and contestation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, 0.68).
domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, 0.76).
domain_priors:theater_ratio(salic_prohibition__immutable_mandate_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__immutable_mandate_reading, mountain).
narrative_ontology:human_readable(salic_prohibition__immutable_mandate_reading, "Salic Law as Immutable Natural/Divine Mandate").
narrative_ontology:topic_domain(salic_prohibition__immutable_mandate_reading, "constitutional/dynastic").

domain_priors:emerges_naturally(salic_prohibition__immutable_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__immutable_mandate_reading, 'f4e19322-af10-4742-8ff2-ad13bde9b079').
narrative_ontology:cs_kernel_codification('f4e19322-af10-4742-8ff2-ad13bde9b079', fixed_text).
narrative_ontology:cs_authority_grounding('f4e19322-af10-4742-8ff2-ad13bde9b079', extraction).
narrative_ontology:cs_interpretation_layer_present('f4e19322-af10-4742-8ff2-ad13bde9b079').
narrative_ontology:cs_reading_relation('f4e19322-af10-4742-8ff2-ad13bde9b079', salic_prohibition__cognatic_reversion_reading, coexists_with).
narrative_ontology:cs_reading_relation('f4e19322-af10-4742-8ff2-ad13bde9b079', salic_prohibition__sovereign_override_reading, forecloses).
narrative_ontology:cs_axiom('f4e19322-af10-4742-8ff2-ad13bde9b079', foundational, agnatic_kinship_as_immutable_natural_order).
narrative_ontology:cs_axiom_status(agnatic_kinship_as_immutable_natural_order, overridden).
narrative_ontology:cs_axiom_grounding('f4e19322-af10-4742-8ff2-ad13bde9b079', agnatic_kinship_as_immutable_natural_order, empirically_contingent).
narrative_ontology:cs_axiom('f4e19322-af10-4742-8ff2-ad13bde9b079', foundational, female_unsuitability_for_rule_divinely_ordained).
narrative_ontology:cs_axiom_status(female_unsuitability_for_rule_divinely_ordained, overridden).
narrative_ontology:cs_axiom_grounding('f4e19322-af10-4742-8ff2-ad13bde9b079', female_unsuitability_for_rule_divinely_ordained, theological).
narrative_ontology:cs_reference_frame('f4e19322-af10-4742-8ff2-ad13bde9b079', immutable_natural_law_of_succession).
narrative_ontology:cs_drift_state('f4e19322-af10-4742-8ff2-ad13bde9b079', contemporary_democratic_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('f4e19322-af10-4742-8ff2-ad13bde9b079', '').
narrative_ontology:cs_kernel_id(salic_prohibition__immutable_mandate_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnatic_male_heirs).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, female_claimants).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, cognatic_succession_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, ecclesiastical_authority).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, natural_law_supremacy).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, divine_ordering_of_succession).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, agnatic_kinship_as_immutable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Guaranteed succession rights and the political authority that flows from unchallenged dynastic legitimacy. Under this reading, their claims are grounded in natural law itself, not subject to revision or negotiation. They defend the interpretation as the only coherent reading of the law; challengers to their position are read as either ignorant of or rebellious against natural order.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, agnatic_male_heirs, beneficiary,
    powerful, generational, arbitrage, continental).

% Systematically excluded from dynastic succession regardless of capability, legitimacy of birth, or political circumstances. Their exclusion is justified not as policy choice but as adherence to immutable natural/divine law. Formal challenge to this exclusion is read as transgression against the order of nature itself, subject to violent suppression. Their options are to accept exclusion, marry into other lines (losing independent dynastic claim), or attempt revolution — none are viable exits from the constraint itself.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, female_claimants, payer,
    powerful, generational, trapped, continental).

% Argue for inclusive succession rules recognizing female heirs. Under the immutable-mandate reading, their advocacy is not a legitimate policy dispute but a challenge to the natural order itself. They bear the cost of being systematically delegitimized; their arguments are treated as sophistry or heresy rather than as coherent alternative framings. Some face legal or military suppression; others are marginalized from court authority.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, cognatic_succession_advocates, payer,
    organized, generational, constrained, continental).

% Implement and enforce the Salic prohibition across their domains. They legitimate this enforcement by invoking the natural/divine law framing: the law is not their choice, but an immutable constraint they are duty-bound to uphold. This rhetorical move shields them from accountability for the exclusion itself — they are merely executing what nature/divinity demands.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, territorial_rulers, agenda_setter,
    institutional, generational, analytical, continental).

% Non-agnatic competitors whose claims are delegitimized by the Salic reading. This includes female heirs of prior kings, cognatic descendants, and lateral branches. They would argue for more inclusive succession rules; their exclusion from the negotiation is structural — the immutable-law framing makes their participation heretical rather than legitimate political discussion.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, rival_claimants_to_throne, excluded,
    powerful, biographical, trapped, continental).

% Vindicates the divine-law reading by pronouncing succession disputes settled by natural/divine order rather than by ecclesiastical dispensation or negotiation. This reserves to itself the authority to certify whether a succession is lawful (divine confirmation) while disclaiming responsibility for the exclusionary outcome. Ecclesiastical authority benefits from the transfer of legitimacy burden to 'natural law' rather than to papal decree.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, ecclesiastical_authority, beneficiary,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__immutable_mandate_reading, ecclesiastical_authority, agenda_setter).

% Chronicle succession disputes and the invocations of Salic Law. Their records preserve the pattern of the constraint's operation: repeated female exclusions, recurring arguments that the law is immutable, occasional challenges framed as transgression or heresy, and the institutional machinery deployed to enforce the reading.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, historical_record_keepers, observer,
    moderate, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__immutable_mandate_reading, agnatic_male_heirs).
narrative_ontology:fixing_cost_class(salic_prohibition__immutable_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, recognizable rule for dynastic succession independent of circumstantial dispute or military strength: the kingdom passes to the nearest agnatic male heir. This solves the coordination problem of civil war over contested succession by providing an ex-ante decision rule.
% TRANSFER_FUNCTION: Transfers dynastic legitimacy, power, and the material fruits of rule from female claimants and cognatic heirs to agnatic male heirs. The transfer is justified as arising from natural order, not from choice, shifting the legitimacy burden from those who enforce it to those who ostensibly merely execute immutable law.
% ABSENT_VOICES: Female rulers from neighboring domains who might challenge the universality of the agnatic rule; philosophers and jurists from traditions (Roman, Islamic, Jewish) with different succession norms; the female claimants themselves, whose exclusion from succession often entails exclusion from the council where succession disputes are discussed. A female claimant cannot argue for her own succession within the immutable-law framework without being read as transgressive.
% DISAPPEARANCE_RATIONALE: If this constraint vanished — if female succession became admissible and the agnatic bar fell away — the identities of dynasty-holders would shift (female lines would produce ruling queens), the composition of courts would change (female authority-holders would advise on succession), and the material distribution of power would reallocate across gender lines. Wars fought to enforce agnatic-only succession would become unnecessary. The constraint's disappearance would reshape the political landscape.
% FOUNDING_PROBLEM: The Frankish succession crisis of the 5th–6th centuries: after the collapse of the Western Roman successor states, agnatic-only succession provided a stable rule for inheritance of landed property and political authority, reducing disputes over succession that turned on competing maternal or lateral claims.
% FOUNDING_PROBLEM_CORROBORATION: Medieval chroniclers sympathetic to dynastic order (Gregory of Tours, later Frankish court historians) attest the founding problem — succession chaos — and the solution agnatic priority provided. Cognatic succession advocates and female claimants attest the founding problem is solved: cognatic succession operates stably in neighboring territories (Anglo-Saxon, Iberia) without equivalent chaos; by 1500 the institutional apparatus for handling succession contests exists and functions regardless of agnatic restriction. Modern historians (Ennen, Karras, Stafford on female rulership) document that the founding problem of succession instability was addressed by institutional development and precedent, not by agnatic restriction. No corroborating authority outside the benefiting parties (agnatic rulers, ecclesiastical authority) attests that agnatic-only succession is still necessary to prevent chaos.
narrative_ontology:disappearance_verdict(salic_prohibition__immutable_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__immutable_mandate_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__immutable_mandate_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(salic_prohibition__immutable_mandate_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__immutable_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, ExtMetricName, E),
    domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(salic_prohibition__immutable_mandate_reading),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(salic_prohibition__immutable_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness remains high across the interval (0.55–0.68) because the constraint consistently benefits agnatic males and burdens female claimants. It does not dissipate over time; if anything, it stabilizes as the rhetorical naturalization deepens. Suppression rises across the interval (0.52–0.76) because institutional machinery for preventing female succession becomes more elaborate and more energetically deployed as challenges mount. Theater rises (0.25–0.42) because the purely performative defense of the constraint strengthens: by the late medieval period, the justification is almost entirely rhetorical — the constraint persists not because it solves a coordination problem but because it serves agnatic interests and is defended by appeals to nature/divinity. Accessibility collapse rises (individual 0.72–0.81, class 0.65–0.84) because the options available to female claimants narrow over time: early Frankish period saw occasional female succession and cognatic variants; by 1500 those paths are closed off by the crystallized doctrine. Suppression is highest at the class level (0.81) and structural level (0.76) by 1900, showing that the constraint operates through both institutional enforcement and systematic class exclusion.
 *
 * PERSPECTIVAL GAP:
 *   From the agnatic-male perspective, this is a natural law protecting legitimate order; from the female-claimant perspective, it is an extractive mechanism preventing them from accessing power they have legitimate claim to. From the ecclesiastical perspective, it is divine order confirmed by nature; from the cognatic-succession perspective, it is anachronistic policy masquerading as immutable law. The engine will compute these seats as experiencing fundamentally different types: the beneficiary seat (agnatic) will see coordination; the victim seats (female, cognatic) will see extraction. The claim of naturalness (mountain) conflicts with the measured extraction and suppression, which is the false-summit detection case.
 *
 * DIRECTIONALITY LOGIC:
 *   Agnatic males sit at d ≈ 0.1–0.15 (structured beneficiary: no cost, all benefit, full arbitrage on succession rules that favor them). Female claimants sit at d ≈ 0.85–0.95 (structured target: systematic exclusion, trapped exit, power to challenge but no power to exit the constraint). Cognatic advocates sit at d ≈ 0.70–0.80 (payer: bear suppression for advocating alternatives, constrained rather than trapped). Ecclesiastical authority sits at d ≈ 0.25–0.35 (moderate beneficiary: benefits from transferring legitimacy burden to nature, but also bears some accountability for the certification). The directive flow is asymmetric: agnatic rules and female bears the cost of those rules.
 *
 * MANDATROPHY ANALYSIS:
 *   A founding-problem analysis surfaces the false summit: the constraint was built to solve succession chaos in the Frankish 5th–6th centuries (genuine coordination problem). By the 12th century, the problem is demonstrably solved — cognatic succession operates stably in neighboring domains, Salic law is invoked against female claimants even in territories where agnatic succession had not been established. The constraint persists not to solve the founding problem but to exclude female heirs from power and to maintain agnatic authority. The natural-law framing is the mechanism by which this extractive persistence is hidden: if the constraint is immutable, then its persistence requires no justification. A ruler cannot be accused of extracting rents if they are merely executing what nature demands. The theater_ratio rise (0.25–0.42) models this: from functional coordination to defensive rhetoric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed,
    'Is Salic Law a genuine natural law (immutable feature of human order and succession) or a constructed constraint that benefits agnatic males and is defended rhetorically as natural to conceal its extractive operation?',
    'Comparative analysis: if female succession operates stably in domains that reject Salic Law (cognatic succession in Iberia, Anglo-Saxon kingdoms, Islamic succession norms), the constraint is shown to be contingent, not natural. If female-led succession produces equivalent or superior coordination and political outcomes, naturalness is falsified.',
    'If resolved as constructed: reclassifies from mountain to tangled_rope (coordination cover for agnatic extraction) or snare (pure exclusion). If resolved as natural: sustains the mountain classification and the immutable-mandate reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed, empirical, 'Whether the constraint is a feature of nature or a contingent institutional choice.').

omega_variable(
    divine_ordination_vs_ecclesiastical_authority_claim,
    'Does the divine-law framing reflect genuine ecclesiastical doctrine (what the Church authoritatively claims) or is it an appropriation by rulers of religious language to legitimize agnatic exclusion?',
    'Ecclesiastical sources from outside the realm: papal bulls, council records, theologians not dependent on the ruling dynasty. Comparison with how other laws are justified — if only Salic law receives ''divine order'' framing while others receive purely positive-law framing, the divine language is selective deployment, not doctrine.',
    'If selective deployment: the ''divine'' framing is rhetorical theater, raising theater_ratio and suggesting the constraint is better modeled as a snare disguised as coordination than as a natural law. If genuine doctrine: sustains the natural-law reading but requires explaining why cognatic succession in other Christian domains lacks comparable divine opposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_ordination_vs_ecclesiastical_authority_claim, conceptual, 'Whether the divine-law claim reflects genuine ecclesiastical doctrine or rhetorical appropriation by rulers.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.76) structural (legal barriers, military enforcement, institutional exclusion) or internalized (female claimants have internalized the belief that they are unfit to rule, do not deserve succession, or that challenging the law is transgressive)?',
    'Historical record of female claimants'' stated positions: do they challenge Salic law itself, or do they accept it as legitimate and merely request exceptions? Post-constraint moments (when Salic law falls away or is openly challenged): do female claimants emerge with already-formed competence and claim, or do they require time to overcome internalized exclusion?',
    'If structural only: the constraint''s suppression power dissipates when enforcement machinery is removed. If internalized: female claimants carry the suppression with them; overcoming it requires identity-recovery and re-learning, raising the effective suppression beyond the structural metric.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of female succession is maintained by external barriers or by internalized belief in unfitness.').

omega_variable(
    kernel_reading_stability_across_domains,
    'Is the immutable_mandate reading stably held across all medieval European domains, or do cognatic and sovereign_override readings emerge in specific territories and historical moments?',
    'Survey of succession law and practice across Christian Europe 500–1900: which territories invoke Salic immutability, which accept female succession or sovereign override, how do readings shift over time?',
    'If the reading is universal and stable: it is plausibly a canonical commitment system reading. If it is local and contested: it is one reading among live alternatives held by different factions, better modeled as coexists_with or influences relations to siblings rather than as a mountain natural law. This would strengthen the false-summit diagnosis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_stability_across_domains, empirical, 'Whether the immutable-mandate reading is universal across medieval Europe or local/contested across territories.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__immutable_mandate_reading, 400, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t400, salic_prohibition__immutable_mandate_reading, theater_ratio, 400, 0.25).
narrative_ontology:measurement_basis(sali_tr_t400, projected).
narrative_ontology:measurement(sali_tr_t700, salic_prohibition__immutable_mandate_reading, theater_ratio, 700, 0.31).
narrative_ontology:measurement_basis(sali_tr_t700, observed).
narrative_ontology:measurement(sali_tr_t1100, salic_prohibition__immutable_mandate_reading, theater_ratio, 1100, 0.38).
narrative_ontology:measurement_basis(sali_tr_t1100, observed).
narrative_ontology:measurement(sali_tr_t1500, salic_prohibition__immutable_mandate_reading, theater_ratio, 1500, 0.42).
narrative_ontology:measurement_basis(sali_tr_t1500, observed).
narrative_ontology:measurement(sali_tr_t1700, salic_prohibition__immutable_mandate_reading, theater_ratio, 1700, 0.41).
narrative_ontology:measurement_basis(sali_tr_t1700, observed).
narrative_ontology:measurement(sali_tr_t1900, salic_prohibition__immutable_mandate_reading, theater_ratio, 1900, 0.42).
narrative_ontology:measurement_basis(sali_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(sali_be_t400, salic_prohibition__immutable_mandate_reading, base_extractiveness, 400, 0.55).
narrative_ontology:measurement_basis(sali_be_t400, projected).
narrative_ontology:measurement(sali_be_t700, salic_prohibition__immutable_mandate_reading, base_extractiveness, 700, 0.61).
narrative_ontology:measurement_basis(sali_be_t700, observed).
narrative_ontology:measurement(sali_be_t1100, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1100, 0.66).
narrative_ontology:measurement_basis(sali_be_t1100, observed).
narrative_ontology:measurement(sali_be_t1500, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1500, 0.68).
narrative_ontology:measurement_basis(sali_be_t1500, observed).
narrative_ontology:measurement(sali_be_t1700, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1700, 0.67).
narrative_ontology:measurement_basis(sali_be_t1700, observed).
narrative_ontology:measurement(sali_be_t1900, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1900, 0.68).
narrative_ontology:measurement_basis(sali_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t400, salic_prohibition__immutable_mandate_reading, suppression_requirement, 400, 0.52).
narrative_ontology:measurement_basis(sali_su_t400, projected).
narrative_ontology:measurement(sali_su_t700, salic_prohibition__immutable_mandate_reading, suppression_requirement, 700, 0.65).
narrative_ontology:measurement_basis(sali_su_t700, observed).
narrative_ontology:measurement(sali_su_t1100, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1100, 0.72).
narrative_ontology:measurement_basis(sali_su_t1100, observed).
narrative_ontology:measurement(sali_su_t1500, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1500, 0.76).
narrative_ontology:measurement_basis(sali_su_t1500, observed).
narrative_ontology:measurement(sali_su_t1700, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1700, 0.75).
narrative_ontology:measurement_basis(sali_su_t1700, observed).
narrative_ontology:measurement(sali_su_t1900, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1900, 0.76).
narrative_ontology:measurement_basis(sali_su_t1900, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=400, tn=1900
narrative_ontology:measurement(sali_grid_01, salic_prohibition__immutable_mandate_reading, accessibility_collapse(class), 400, 0.65).
narrative_ontology:measurement(sali_grid_02, salic_prohibition__immutable_mandate_reading, accessibility_collapse(class), 1900, 0.84).
narrative_ontology:measurement(sali_grid_03, salic_prohibition__immutable_mandate_reading, accessibility_collapse(individual), 400, 0.72).
narrative_ontology:measurement(sali_grid_04, salic_prohibition__immutable_mandate_reading, accessibility_collapse(individual), 1900, 0.81).
narrative_ontology:measurement(sali_grid_05, salic_prohibition__immutable_mandate_reading, accessibility_collapse(organizational), 400, 0.68).
narrative_ontology:measurement(sali_grid_06, salic_prohibition__immutable_mandate_reading, accessibility_collapse(organizational), 1900, 0.79).
narrative_ontology:measurement(sali_grid_07, salic_prohibition__immutable_mandate_reading, accessibility_collapse(structural), 400, 0.58).
narrative_ontology:measurement(sali_grid_08, salic_prohibition__immutable_mandate_reading, accessibility_collapse(structural), 1900, 0.82).
narrative_ontology:measurement(sali_grid_09, salic_prohibition__immutable_mandate_reading, resistance(class), 400, 0.35).
narrative_ontology:measurement(sali_grid_10, salic_prohibition__immutable_mandate_reading, resistance(class), 1900, 0.58).
narrative_ontology:measurement(sali_grid_11, salic_prohibition__immutable_mandate_reading, resistance(individual), 400, 0.38).
narrative_ontology:measurement(sali_grid_12, salic_prohibition__immutable_mandate_reading, resistance(individual), 1900, 0.52).
narrative_ontology:measurement(sali_grid_13, salic_prohibition__immutable_mandate_reading, resistance(organizational), 400, 0.42).
narrative_ontology:measurement(sali_grid_14, salic_prohibition__immutable_mandate_reading, resistance(organizational), 1900, 0.61).
narrative_ontology:measurement(sali_grid_15, salic_prohibition__immutable_mandate_reading, resistance(structural), 400, 0.32).
narrative_ontology:measurement(sali_grid_16, salic_prohibition__immutable_mandate_reading, resistance(structural), 1900, 0.59).
narrative_ontology:measurement(sali_grid_17, salic_prohibition__immutable_mandate_reading, stakes_inflation(class), 400, 0.45).
narrative_ontology:measurement(sali_grid_18, salic_prohibition__immutable_mandate_reading, stakes_inflation(class), 1900, 0.68).
narrative_ontology:measurement(sali_grid_19, salic_prohibition__immutable_mandate_reading, stakes_inflation(individual), 400, 0.48).
narrative_ontology:measurement(sali_grid_20, salic_prohibition__immutable_mandate_reading, stakes_inflation(individual), 1900, 0.71).
narrative_ontology:measurement(sali_grid_21, salic_prohibition__immutable_mandate_reading, stakes_inflation(organizational), 400, 0.52).
narrative_ontology:measurement(sali_grid_22, salic_prohibition__immutable_mandate_reading, stakes_inflation(organizational), 1900, 0.74).
narrative_ontology:measurement(sali_grid_23, salic_prohibition__immutable_mandate_reading, stakes_inflation(structural), 400, 0.38).
narrative_ontology:measurement(sali_grid_24, salic_prohibition__immutable_mandate_reading, stakes_inflation(structural), 1900, 0.62).
narrative_ontology:measurement(sali_grid_25, salic_prohibition__immutable_mandate_reading, suppression(class), 400, 0.48).
narrative_ontology:measurement(sali_grid_26, salic_prohibition__immutable_mandate_reading, suppression(class), 1900, 0.81).
narrative_ontology:measurement(sali_grid_27, salic_prohibition__immutable_mandate_reading, suppression(individual), 400, 0.44).
narrative_ontology:measurement(sali_grid_28, salic_prohibition__immutable_mandate_reading, suppression(individual), 1900, 0.68).
narrative_ontology:measurement(sali_grid_29, salic_prohibition__immutable_mandate_reading, suppression(organizational), 400, 0.51).
narrative_ontology:measurement(sali_grid_30, salic_prohibition__immutable_mandate_reading, suppression(organizational), 1900, 0.73).
narrative_ontology:measurement(sali_grid_31, salic_prohibition__immutable_mandate_reading, suppression(structural), 400, 0.42).
narrative_ontology:measurement(sali_grid_32, salic_prohibition__immutable_mandate_reading, suppression(structural), 1900, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__immutable_mandate_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(salic_prohibition__immutable_mandate_reading, 0.12).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__cognatic_reversion_reading).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__sovereign_override_reading).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, female_succession_battles_medieval_europe).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, agnatic_kinship_property_transmission).

% DUAL FORMULATION NOTE:
% Salic Law is a contested kernel with three readings: immutable_mandate_reading (this story), cognatic_reversion_reading (Salic Law as Frankish anachronism never binding on non-Frankish territories), and sovereign_override_reading (Salic Law as revocable positive law). Each reading has different ε, different beneficiaries/victims, different type classifications. The three stories are linked by network.affects_constraints and by the kernel_id/reading_id structure in cs_structure. Do not attempt to force all three readings into one story; each reading is a separate constraint with its own structural data and its own engine classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(salic_prohibition__immutable_mandate_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
