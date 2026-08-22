% ============================================================================
% CONSTRAINT STORY: salic_prohibition__cognatic_reversion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__cognatic_reversion_reading, []).

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
 *   constraint_id: salic_prohibition__cognatic_reversion_reading
 *   human_readable: Salic Prohibition as Anachronistic Territorial Constraint (Cognatic Reversion Reading)
 *   domain: constitutional_law/dynastic_succession
 *
 * SUMMARY:
 *   Under this reading, Salic Law prohibiting female succession is understood
 *   as a Frankish tribal custom that was never properly binding on
 *   non-Frankish territories conquered or incorporated into the realm. The
 *   constraint persists by enforcing agnatic male primogeniture on peripheral
 *   kingdoms that had their own cognatic succession traditions. The founding
 *   problem (chaotic male succession in early Frankish period) is understood
 *   as dead by the time the rule becomes formalized as 'natural law,' making
 *   the persistence of the constraint primarily extractive: it concentrates
 *   dynastic authority in agnatic male lines and suppresses the alternative
 *   of cognatic succession that would have opened succession to the most
 *   capable heir regardless of sex. This reading privileges territorial
 *   integrity and legitimate local succession custom over agnatic purity as
 *   the governing principle, framing Salic Law as an anachronistic imposition
 *   on non-Frankish law.
 *
 * KEY AGENTS:
 *   - Agnatic male lineage holders: benefit from narrowed succession pool; control succession narrative through clerical and noble advocacy
 *   - Frankish feudal establishment: administers Salic rule through councils and charters; defines territorial boundaries of Salic applicability; enforces through military power and ecclesiastical alliance
 *   - Female potential successors: bear extraction cost of dynastic authority transfer; trapped in subordinate positions (regency, marriage, influence)
 *   - Non-Frankish peripheral territories: lose right to apply their own succession custom; forced to adopt agnatic exclusion under military and ecclesiastical pressure
 *   - Ecclesiastical authority: lends theological legitimation to Salic rule; derives authority from role in succession adjudication; benefits from concentration of royal power in male lines dependent on ecclesiastical validation
 *   - Secular legal scholars: observe and challenge the naturality claim; produce historical analysis undermining the constraint's foundational myth
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, 0.62).
domain_priors:suppression_score(salic_prohibition__cognatic_reversion_reading, 0.71).
domain_priors:theater_ratio(salic_prohibition__cognatic_reversion_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__cognatic_reversion_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__cognatic_reversion_reading, "Salic Prohibition as Anachronistic Territorial Constraint (Cognatic Reversion Reading)").
narrative_ontology:topic_domain(salic_prohibition__cognatic_reversion_reading, "constitutional_law/dynastic_succession").

domain_priors:requires_active_enforcement(salic_prohibition__cognatic_reversion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__cognatic_reversion_reading, 'bde891dd-b790-44e2-831c-543a83e3f528').
narrative_ontology:cs_kernel_codification('bde891dd-b790-44e2-831c-543a83e3f528', fixed_text).
narrative_ontology:cs_authority_grounding('bde891dd-b790-44e2-831c-543a83e3f528', extraction).
narrative_ontology:cs_interpretation_layer_present('bde891dd-b790-44e2-831c-543a83e3f528').
narrative_ontology:cs_reading_relation('bde891dd-b790-44e2-831c-543a83e3f528', salic_prohibition__immutable_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('bde891dd-b790-44e2-831c-543a83e3f528', salic_prohibition__sovereign_override_reading, influences).
narrative_ontology:cs_axiom('bde891dd-b790-44e2-831c-543a83e3f528', foundational, salic_law_is_frankish_custom_not_natural_law).
narrative_ontology:cs_axiom_status(salic_law_is_frankish_custom_not_natural_law, holdable).
narrative_ontology:cs_axiom_grounding('bde891dd-b790-44e2-831c-543a83e3f528', salic_law_is_frankish_custom_not_natural_law, empirically_contingent).
narrative_ontology:cs_axiom('bde891dd-b790-44e2-831c-543a83e3f528', foundational, territorial_integrity_principle_over_agnatic_purity).
narrative_ontology:cs_axiom_status(territorial_integrity_principle_over_agnatic_purity, holdable).
narrative_ontology:cs_axiom_grounding('bde891dd-b790-44e2-831c-543a83e3f528', territorial_integrity_principle_over_agnatic_purity, conventional).
narrative_ontology:cs_axiom('bde891dd-b790-44e2-831c-543a83e3f528', secondary, female_cognatic_succession_is_legitimate_outside_frankish_heartland).
narrative_ontology:cs_axiom_status(female_cognatic_succession_is_legitimate_outside_frankish_heartland, holdable).
narrative_ontology:cs_axiom_grounding('bde891dd-b790-44e2-831c-543a83e3f528', female_cognatic_succession_is_legitimate_outside_frankish_heartland, empirically_contingent).
narrative_ontology:cs_reference_frame('bde891dd-b790-44e2-831c-543a83e3f528', cognatic_succession_right).
narrative_ontology:cs_drift_state('bde891dd-b790-44e2-831c-543a83e3f528', late_medieval_formalization, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('bde891dd-b790-44e2-831c-543a83e3f528', '').
narrative_ontology:cs_kernel_id(salic_prohibition__cognatic_reversion_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, agnatic_male_lineage_holders).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, frankish_feudal_establishment).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, female_potential_successors).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, non_frankish_peripheral_territories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, ecclesiastical_authority).
narrative_ontology:constraint_vindicates(salic_prohibition__cognatic_reversion_reading, territorial_integrity_doctrine).
narrative_ontology:constraint_vindicates(salic_prohibition__cognatic_reversion_reading, cognatic_primogeniture_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Male members of the royal house benefit from the Salic exclusion of female succession by narrowing the pool of legitimate heirs and concentrating inheritance within agnatic branches. They invoke the law to exclude sisters and daughters when succession disputes arise, and maintain control over the succession narrative through clerical and noble advocacy.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, agnatic_male_lineage_holders, beneficiary,
    institutional, generational, arbitrage, national).

% Frankish feudal nobility and ecclesiastical authorities enforce the Salic rule through councils, charters, and succession adjudications. They frame it as natural law or divine ordinance to justify exclusion and to maintain male-lineage power concentration. They administer the succession process and decide which territories are 'truly Frankish' and thus bound by Salic restriction.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, frankish_feudal_establishment, agenda_setter,
    institutional, generational, mobile, national).

% Queens, princesses, and daughters of kings are systematically excluded from direct succession despite possessing legitimate claim by birth and ability. They bear the cost of being stripped of inheritance rights and dynastic authority. Their only recourse is to marry and transfer their legitimacy to their husband's line, or to exercise power indirectly through regency or influence—both subordinate positions enforced by the Salic restriction.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, female_potential_successors, payer,
    powerful, biographical, trapped, national).

% Peripheral kingdoms and territories (Aquitaine, Burgundy, Neustria) are subjected to the Salic rule by conquest or feudal imposition, even though they have their own succession traditions and legal systems that often permitted female inheritance. They lose the ability to apply their own cognatic succession rules and are forced to adopt agnatic exclusion as the price of remaining within the Frankish realm. Resistance is suppressed through military enforcement and ecclesiastical authority.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, non_frankish_peripheral_territories, payer,
    organized, biographical, constrained, regional).

% The Church frames Salic Law as reflecting divine will and natural order, lending theological authority to agnatic exclusion. Bishops and abbots administer succession adjudications and derive authority and influence from their role in affirming the Salic rule. They benefit from the concentration of royal power in agnatic lines that remain dependent on ecclesiastical legitimation.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, ecclesiastical_authority, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__cognatic_reversion_reading, ecclesiastical_authority, beneficiary).

% Daughters and sisters of deceased kings who might inherit under cognatic systems are completely barred from succession voice and from challenging the Salic rule through formal channels. They are structurally absent from succession councils and have no standing to contest the rule's application. Some resist through regional insurrection or by supporting rival male claimants against the agnatic-preferred heir, but this indirect resistance is itself suppressed by the enforcement machinery.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, potential_alternative_successors, excluded,
    moderate, biographical, trapped, national).

% Later medieval and early modern jurists and historians examine whether Salic Law is truly natural law, divine mandate, or an anachronistic Frankish custom misapplied to non-Frankish territories. They produce historical and textual analysis that undermines the naturality claim, but their conclusions remain confined to scholarly debate and do not immediately alter succession practice.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, secular_legal_scholars, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__cognatic_reversion_reading, agnatic_male_lineage_holders).
narrative_ontology:fixing_cost_class(salic_prohibition__cognatic_reversion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, predictable succession rule that prevents the realm from fragmenting into rival female-headed branches competing for dynastic authority. In the Frankish heartland, agnatic exclusion concentrates inheritance and reduces succession disputes between multiple competing heirs. The rule coordinates expectations among the nobility about who is a legitimate successor.
% TRANSFER_FUNCTION: Transfers succession rights away from female potential heirs toward agnatic male heirs, narrowing the distribution of dynastic authority. In peripheral territories, the rule transfers the right to apply local succession custom (including female inheritance) to the imperative to follow Frankish agnatic practice, consolidating central control. The constraint moves legitimacy and territorial authority from the dispossessed (women, non-Frankish legal traditions) to the agnatic beneficiaries (male lineage holders and their supporters).
% ABSENT_VOICES: Female potential successors and inhabitants of non-Frankish territories are structurally excluded from succession councils and legislative bodies that affirm the Salic rule. They would argue for cognatic succession and the right to apply their own legal traditions, but are silenced by the enforcement machinery (military force in territories, social prohibition in the dynastic center). Their objections are systematically prevented from entering the succession discourse.
% DISAPPEARANCE_RATIONALE: If the Salic prohibition vanished overnight, succession would open to female heirs (eldest child regardless of sex, or the most capable potential successor). Peripheral territories would likely reclaim their own succession customs, fragmenting the unified Frankish succession rule into regional variation. The dynastic authority structure would reorganize around different legitimacy criteria, and rival claimants (female and male alike) would contest for succession under new rules, precipitating a major reorganization of the realm's political structure.
% FOUNDING_PROBLEM: Early Frankish succession was chaotic and disputed: ambiguity about who qualified as heir led to civil war between rival male claimants, weakening the kingdom against external enemies and internal fragmentation. Establishing a clear, male-only succession rule was intended to eliminate the ambiguity and reduce contested claims by narrowing the pool of legitimate heirs.
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical historians from outside the Frankish establishment (peripheral kingdom chroniclers, later medieval scholars) attest that by the later medieval period, the founding problem of succession chaos had been substantially resolved and female succession in neighboring regions (Castile, Portugal, Poland) did not result in realm fragmentation. Non-Frankish territorial rulers (Aquitainian chroniclers, Burgundian sources) attested that their own cognatic succession traditions maintained stable succession without the Salic restriction. Modern historical scholarship confirms that the founding problem's urgency had declined by the time the Salic rule became formalized as 'irrevocable law.'
narrative_ontology:disappearance_verdict(salic_prohibition__cognatic_reversion_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__cognatic_reversion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__cognatic_reversion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(salic_prohibition__cognatic_reversion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__cognatic_reversion_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__cognatic_reversion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__cognatic_reversion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.62 (interval end) because the constraint transfers succession rights away from female and non-Frankish alternatives toward agnatic male heirs; this transfer is the constitutive extraction. Suppression is higher (0.71) because maintaining the constraint requires active enforcement: military suppression of peripheral resistance to Salic imposition, social prohibition against female succession discourse, and ecclesiastical suppression of alternative legal interpretations. Theater ratio is moderate-high (0.48) and rising over the interval because the constraint's original coordination function (resolving early Frankish succession chaos) has atrophied, yet the enforcement machinery persists through appeals to naturalized law and divine mandate—the performative justification increasingly exceeds the functional necessity. The measurement trajectory shows extractiveness rising then stabilizing (peak at t=40, slight decline at t=50 as reform pressures begin), while theater ratio plateaus at high level, indicating the constraint has shifted from coordination to inert extraction maintained by theatrical naturalness claims. The suppression trajectory is monotonically rising, reflecting intensifying enforcement to maintain the rule against growing intellectual and peripheral resistance.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (female successors, peripheral territories) and the agenda-setter seats (Frankish feudal establishment, ecclesiastical authority) should compute radically differently. From the agenda-setter perspective, Salic Law is a settled, natural principle protecting realm stability; from the payer perspective, it is an anachronistic imposition sustained by force and theological fiction. This reading foregrounds the gap: the constraint's persistence depends entirely on the suppression of the payer-seat analysis. The engine will compute a high-extraction tangled_rope from these structural inputs; the claimed type (tangled_rope) aligns with this because this reading acknowledges the extractive asymmetry at the constraint's core.
 *
 * DIRECTIONALITY LOGIC:
 *   Agnatic male beneficiaries experience directionality near 0.0 (full beneficiary: they collect the succession rights and authority the constraint protects). The Frankish feudal establishment experiences near-symmetric directionality (d ≈ 0.5) because they bear the cost of enforcement while also collecting some authority-legitimation benefit. Female potential successors experience high directionality (d ≈ 0.85) because they are the primary targets of extraction—the constraint's entire force is directed at suppressing their succession claim. Non-Frankish peripheral territories experience slightly lower but still high directionality (d ≈ 0.75) because the extraction is twofold: loss of succession rights AND loss of the right to apply their own legal tradition. Ecclesiastical authority sits near-symmetric (d ≈ 0.52) because they invest enforcement effort while also deriving legitimacy and influence from the constraint's persistence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (early succession chaos) is dead by the time Salic Law becomes formalized. The constraint persists not because the problem it was built to solve remains live, but because the beneficiary seats have invested in theological and legal naturalness claims that obscure the problem's death. The theater ratio trajectory (rising from 0.22 to 0.48 over the interval) captures this drift: the enforcement machinery increasingly consists of performative appeals to natural law rather than functional succession stabilization. A mandatrophy verdict is warranted: the constraint's original mandate is obsolete, its persistence is sustained by inertia and theatrical naturalization, and fixing it would require unseating the beneficiary seats' authority to define what counts as natural law. The constraint meets the canonical Piton criteria (atrophied function, inertial persistence, theater exceeding function) even though it is claimed as Tangled Rope—the divergence highlights that this reading sees the constraint as Piton-trajectory (extractive function exhausted, only performance remains), while the immutable_mandate_reading would claim it as unchanged Mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_custom_ambiguity,
    'Is Salic Law a genuine natural law reflecting immutable principles of dynastic legitimacy, or an anachronistic custom elevated to natural law status by beneficiary interpretation?',
    'Comparative historical analysis: if female succession in non-Frankish territories (Castile, Poland, Portugal) produces equivalent or superior stability outcomes, the naturality claim is undermined; if peripheral territories adopting Salic rule show no stability improvement over their prior cognatic systems, the custom-vs-natural distinction clarifies in favor of custom.',
    'If custom, the constraint reclassifies from immutable-mandate framing to revocable positive law, opening direct challenge to the Frankish establishment''s authority to impose it. The extractive component becomes explicit and remediable rather than accepted as natural fact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_custom_ambiguity, empirical, 'Whether Salic Law''s legitimacy rests on natural-law status or on custom-elevation by beneficiary interpretation.').

omega_variable(
    territorial_binding_scope_ambiguity,
    'On which territories was Salic Law originally binding, and on which was it imposed by conquest or feudal subordination after the original Frankish kingdoms?',
    'Archival analysis of charters, succession adjudications, and resistance records from peripheral territories: does the historical record show Salic prohibition as accepted local practice or as enforced imposition against local cognatic traditions?',
    'If Salic Law was imposed on non-cognatic territories, the constraint''s persistence in those territories is pure extraction with no coordination function—the suppression metric rises, the beneficiary set narrows to Frankish beneficiaries alone, and the constraint reclassifies toward snare from payer-seat perspective in peripheral regions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(territorial_binding_scope_ambiguity, empirical, 'Territorial scope of original Salic binding vs. later imposed scope.').

omega_variable(
    female_succession_stability_empirical,
    'Does permitting female succession (eldest child regardless of sex) produce greater or lesser realm stability than agnatic-male-only succession?',
    'Comparative analysis of succession disputes and civil wars in realms with and without female succession restrictions, controlling for realm size, external threats, and noble faction density.',
    'If female succession produces equivalent or superior stability, the founding problem''s resolution claim (that male-only succession prevents chaos) is undermined and the constraint is purely extractive. If female succession produces worse stability outcomes, the coordination function of Salic Law is partially vindicated and the constraint reclassifies toward rope from beneficiary-seat perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(female_succession_stability_empirical, empirical, 'Empirical stability comparison between female-inclusive and agnatic-only succession regimes.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression of female succession structural (legal prohibition, military enforcement) or internalized (female heirs believe they lack legitimacy, accept subordinate roles voluntarily)?',
    'Post-reform tracking: if female succession becomes legally permissible and females still rarely contest or claim succession, internalization is high; if female succession immediately becomes claimed when legally available, suppression was primarily structural.',
    'If internalized, the constraint''s effective suppression persists beyond legal reform and must be addressed through cultural re-legitimation. If structural, legal change is sufficient to dissolve the constraint''s force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism in Salic prohibition enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__cognatic_reversion_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__cognatic_reversion_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(sali_tr_t0, observed).
narrative_ontology:measurement(sali_tr_t10, salic_prohibition__cognatic_reversion_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(sali_tr_t10, observed).
narrative_ontology:measurement(sali_tr_t20, salic_prohibition__cognatic_reversion_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(sali_tr_t20, observed).
narrative_ontology:measurement(sali_tr_t30, salic_prohibition__cognatic_reversion_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement_basis(sali_tr_t30, observed).
narrative_ontology:measurement(sali_tr_t40, salic_prohibition__cognatic_reversion_reading, theater_ratio, 40, 0.49).
narrative_ontology:measurement_basis(sali_tr_t40, observed).
narrative_ontology:measurement(sali_tr_t50, salic_prohibition__cognatic_reversion_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement_basis(sali_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(sali_be_t0, observed).
narrative_ontology:measurement(sali_be_t10, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(sali_be_t10, observed).
narrative_ontology:measurement(sali_be_t20, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(sali_be_t20, observed).
narrative_ontology:measurement(sali_be_t30, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(sali_be_t30, observed).
narrative_ontology:measurement(sali_be_t40, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement_basis(sali_be_t40, observed).
narrative_ontology:measurement(sali_be_t50, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(sali_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(sali_su_t0, observed).
narrative_ontology:measurement(sali_su_t10, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(sali_su_t10, observed).
narrative_ontology:measurement(sali_su_t20, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(sali_su_t20, observed).
narrative_ontology:measurement(sali_su_t30, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(sali_su_t30, observed).
narrative_ontology:measurement(sali_su_t40, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(sali_su_t40, observed).
narrative_ontology:measurement(sali_su_t50, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(sali_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__cognatic_reversion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(salic_prohibition__cognatic_reversion_reading, 0.12).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__sovereign_override_reading).

% DUAL FORMULATION NOTE:
% The salic_prohibition kernel has three distinct readings instantiated as separate constraint stories: (1) cognatic_reversion_reading—this story—treats Salic Law as Frankish anachronism inapplicable outside original Frankish jurisdiction, privileging cognatic succession and territorial integrity; (2) immutable_mandate_reading treats Salic Law as irrevocable natural/divine law, extractiveness lower, mountain-or-rope classification; (3) sovereign_override_reading treats Salic Law as revocable positive law subject to sovereign legislative authority, creating a third classification distinct from both. The three readings coexist in public dispute (different parties hold them simultaneously), with this reading influencing the sovereign_override reading and coexisting with the immutable_mandate reading. Decomposition follows the ε-invariance principle: same referent (the Salic prohibition's standing application), three distinct ε values (this reading: 0.62; immutable_mandate: ~0.15; sovereign_override: ~0.48) because each reading instantiates different core premises about legitimacy and thus different extraction assessments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(salic_prohibition__cognatic_reversion_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
