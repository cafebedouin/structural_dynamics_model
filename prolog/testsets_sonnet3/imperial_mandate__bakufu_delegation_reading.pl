% ============================================================================
% CONSTRAINT STORY: imperial_mandate__bakufu_delegation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__bakufu_delegation_reading, []).

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
 *   constraint_id: imperial_mandate__bakufu_delegation_reading
 *   human_readable: Bakufu Delegation Reading of the Imperial Mandate: Emperor as Ritual Legitimator, Shogun as Governing Authority
 *   domain: political_philosophy/comparative_constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   This story authors the bakufu delegation reading of the imperial mandate
 *   kernel: the claim that divine/dynastic legitimacy and practical
 *   governance are separable functions, such that the emperor can retain sole
 *   legitimating authority while the shogun and samurai class exercise actual
 *   rule under a delegated commission. This reading underwrote roughly seven
 *   centuries of shogunal governance (Kamakura through Tokugawa) and is
 *   structurally distinct from the loyalist restoration reading, which holds
 *   legitimacy and governance to be inseparable. The two readings are not
 *   competing interpretations of one constraint but two constraints sharing a
 *   kernel — each has its own beneficiary/victim structure, its own
 *   extraction profile, and its own historical trajectory. This file authors
 *   only the delegation reading; the restoration reading is a sibling story.
 *
 * KEY AGENTS:
 *   - shogunal_house: primary beneficiary and agenda-setter (institutional/arbitrage) — holds administrative authority under delegated commission
 *   - samurai_governing_stratum: beneficiary class (organized/constrained) — governing legitimacy depends on the delegation reading holding
 *   - imperial_court_nobility: primary payer (moderate/trapped) — retains ritual precedence, stripped of political power
 *   - loyalist_scholars_and_clergy: excluded voice (powerless/trapped) — hold the rival reading, marginalized until politically useful
 *   - peasantry_under_bakufu_taxation: diffuse payer (powerless/trapped) — bears material cost regardless of which reading prevails
 *   - confucian_legal_scholars: analytical observer (moderate/analytical) — supplies doctrinal apparatus for the separability claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, 0.58).
domain_priors:suppression_score(imperial_mandate__bakufu_delegation_reading, 0.71).
domain_priors:theater_ratio(imperial_mandate__bakufu_delegation_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__bakufu_delegation_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__bakufu_delegation_reading, "Bakufu Delegation Reading of the Imperial Mandate: Emperor as Ritual Legitimator, Shogun as Governing Authority").
narrative_ontology:topic_domain(imperial_mandate__bakufu_delegation_reading, "political_philosophy/comparative_constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__bakufu_delegation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__bakufu_delegation_reading, '36dbf703-9a23-4953-888a-423fe2be1ab2').
narrative_ontology:cs_kernel_codification('36dbf703-9a23-4953-888a-423fe2be1ab2', distributed).
narrative_ontology:cs_authority_grounding('36dbf703-9a23-4953-888a-423fe2be1ab2', lineage).
narrative_ontology:cs_interpretation_layer_present('36dbf703-9a23-4953-888a-423fe2be1ab2').
narrative_ontology:cs_reading_relation('36dbf703-9a23-4953-888a-423fe2be1ab2', imperial_mandate__loyalist_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('36dbf703-9a23-4953-888a-423fe2be1ab2', foundational, legitimacy_and_governance_are_separable_functions).
narrative_ontology:cs_axiom_status(legitimacy_and_governance_are_separable_functions, holdable).
narrative_ontology:cs_axiom_grounding('36dbf703-9a23-4953-888a-423fe2be1ab2', legitimacy_and_governance_are_separable_functions, conventional).
narrative_ontology:cs_axiom('36dbf703-9a23-4953-888a-423fe2be1ab2', secondary, institutional_continuity_through_delegation_preserves_mandate).
narrative_ontology:cs_axiom_status(institutional_continuity_through_delegation_preserves_mandate, holdable).
narrative_ontology:cs_axiom_grounding('36dbf703-9a23-4953-888a-423fe2be1ab2', institutional_continuity_through_delegation_preserves_mandate, instrumental).
narrative_ontology:cs_reference_frame('36dbf703-9a23-4953-888a-423fe2be1ab2', heavenly_grandson_descent_doctrine).
narrative_ontology:cs_drift_state('36dbf703-9a23-4953-888a-423fe2be1ab2', late_tokugawa_bakumatsu_period, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('36dbf703-9a23-4953-888a-423fe2be1ab2', '').
narrative_ontology:cs_kernel_id(imperial_mandate__bakufu_delegation_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, shogunal_house).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, samurai_governing_stratum).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, bakufu_administrative_officials).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, imperial_court_nobility).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, loyalist_scholars_and_clergy).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, peasantry_under_bakufu_taxation).
narrative_ontology:constraint_vindicates(imperial_mandate__bakufu_delegation_reading, separability_of_legitimacy_and_governance).
narrative_ontology:constraint_vindicates(imperial_mandate__bakufu_delegation_reading, institutional_continuity_through_delegation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds and exercises administrative, military, and judicial authority over the realm under a commission (sei-i taishogun) formally issued by the emperor. Structures court ritual, controls the imperial household's finances and marriages, and determines how much political latitude the throne retains. Can revise the terms of delegation at will since it controls the enforcement apparatus that makes the delegation meaningful.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, shogunal_house, agenda_setter,
    institutional, generational, arbitrage, national).

% Occupies the administrative, military, and landholding positions that constitute actual governance under bakufu authority. Their status as a legitimate ruling class depends on the mandate being read as delegable rather than requiring direct imperial exercise; exit from this arrangement would mean forfeiting the entire basis of their social and political position.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, samurai_governing_stratum, beneficiary,
    organized, generational, constrained, national).

% Staff the actual machinery of governance — magistrates, deputies, provincial administrators — whose offices exist only because the delegation reading treats administrative function as separable from and superior in practice to ritual sovereignty. Their careers and incomes are structured entirely around the bakufu's continuing claim to legitimate delegated authority.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, bakufu_administrative_officials, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__bakufu_delegation_reading, bakufu_administrative_officials, agenda_setter).

% Retains ceremonial precedence and ritual function but is stripped of independent political and economic power; income, marriages, and household size are controlled by the shogunate. Cannot leave the arrangement — their entire social identity and material survival is bound to a court whose political relevance has been deliberately hollowed out by the very structure that claims to honor it.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, imperial_court_nobility, payer,
    moderate, generational, trapped, national).

% Advance the position that the mandate cannot be delegated without corruption of its sacred character, and that active imperial rule is the only legitimate form. Marginalized from official doctrine and often surveilled or suppressed when their arguments gain political traction, particularly in later periods when their reading becomes a vehicle for anti-bakufu mobilization.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, loyalist_scholars_and_clergy, excluded,
    powerless, generational, trapped, national).

% Bear the material weight of the governing arrangement through land taxes and corvée obligations levied by domain and bakufu administration. Have no voice in whether the mandate is read as delegable; the delegation reading's legitimacy is invisible to their daily experience of taxation and administrative control, which would persist under either reading.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, peasantry_under_bakufu_taxation, payer,
    powerless, biographical, trapped, regional).

% Develop and articulate the doctrinal apparatus — drawing on continental theories of delegated mandate and hierarchical role-differentiation — that provides the intellectual scaffolding for treating ritual and administrative sovereignty as legitimately separable. Their scholarship is cited by the bakufu to justify the arrangement but exists somewhat independently of it.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, confucian_legal_scholars, observer,
    moderate, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Separates the sacral/legitimating function of sovereignty (which requires ritual continuity, genealogical purity, and insulation from the compromises of daily administration) from the practical function of governance (which requires military capacity, administrative infrastructure, and rapid decision-making) — allowing each function to be performed by the institution best suited to it without requiring one person or office to do both.
% TRANSFER_FUNCTION: Moves effective political authority, tax revenue control, and coercive capacity from the imperial household to the shogunal house and samurai administration, in exchange for the shogunate's maintenance of the imperial court's ritual precedence, ceremonial income, and formal fiction of ultimate sovereignty.
% ABSENT_VOICES: Loyalist scholars and clergy who hold that the mandate is not delegable are structurally excluded from doctrinal authority during bakufu ascendancy; their arguments are treated as fringe or seditious until political conditions (foreign pressure, bakufu fiscal collapse) make restoration ideology suddenly useful to different factions.
% DISAPPEARANCE_RATIONALE: If the delegation reading collapsed — if it became broadly accepted that legitimacy cannot be separated from active imperial governance — the entire structure of samurai administrative authority would lose its claim to legitimacy overnight, exactly as occurred historically in the Meiji Restoration: the bakufu's officials, the domain lord system, and the samurai class's governing role were dismantled once the rival reading achieved political dominance.
% FOUNDING_PROBLEM: A recurring problem in periods of court weakness and military crisis (retired-emperor politics, the Genpei War, provincial disorder) where the imperial court lacked the military and administrative capacity to govern directly, but abolishing the imperial institution entirely would have destroyed the primary source of legitimating authority that any governing power needed to draw on.
% FOUNDING_PROBLEM_CORROBORATION: Bakufu-era Confucian scholars and shogunal court historians attest the delegation is a stable, legitimate constitutional settlement solving a genuine governance-capacity problem. Meiji-era loyalist historians and, independently, foreign observers of the mid-19th century bakufu (whose accounts note the shogunate's declining administrative competence relative to its accumulated privileges) corroborate that by the Bakumatsu period the founding capacity problem had been resolved or reversed — the bakufu no longer clearly outperformed a plausible directly-imperial alternative — while the delegation's material privileges for the samurai administrative class persisted regardless.
narrative_ontology:disappearance_verdict(imperial_mandate__bakufu_delegation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__bakufu_delegation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__bakufu_delegation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imperial_mandate__bakufu_delegation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__bakufu_delegation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__bakufu_delegation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__bakufu_delegation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-to-substantial (0.58 by interval end) because the delegation reading transfers real governing authority and tax capacity to the samurai administrative class while the imperial court retains only ceremonial income — a genuine asymmetry, though softened by the court's continued material support. Suppression is higher (0.71) because maintaining the separability claim requires actively preventing the imperial household from exercising independent political action (marriage control, succession control, restricted movement) and periodically suppressing loyalist doctrine. Theater ratio rises over the period (0.40 to 0.62) reflecting the accumulation of ritual formalism — elaborate court ceremony, symbolic bakufu deference to the throne — that increasingly substitutes for any functional interaction between the two seats of authority, particularly as bakufu administrative competence itself declined in the late Tokugawa period.
 *
 * DIRECTIONALITY LOGIC:
 *   The shogunal house and samurai stratum sit near the beneficiary end: they receive delegated governing authority, tax revenue, and social status whose legitimacy depends entirely on the delegation reading holding. The imperial court nobility sits near the target end: real political and economic power is removed from them under cover of ceremonial elevation — a classic tangled-rope signature where the payer is given symbolic compensation for structural loss. Loyalist scholars are excluded rather than coordinated; their exclusion is what active suppression maintains. Peasantry experience extraction that is largely invariant to which reading of the mandate prevails, since taxation flows from administrative capacity rather than doctrinal legitimacy — their d is high but for reasons orthogonal to this constraint's specific claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (court incapacity to govern directly during periods of military crisis) was genuinely live during the Kamakura and early Muromachi periods, making early delegation closer to a rope: samurai military capacity solved a real coordination problem the court could not solve alone. By the late Tokugawa period the founding problem had substantially inverted — the bakufu's own administrative and military competence had atrophied relative to domains and foreign powers, while the accumulated privileges of the samurai administrative class persisted and even hardened through the alternate-attendance system and hereditary office-holding. This is the mandatrophy signature: the coordination function that justified the delegation eroded while the extraction it enabled did not, and the constraint's classification should track that drift toward tangled_rope-with-declining-coordination-share rather than freeze at either the founding-era rope reading or a purely extractive snare reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    delegation_as_genuine_doctrine_vs_post_hoc_rationalization,
    'Was the separability of legitimacy and governance a coherent doctrinal position developed prior to and independent of shogunal power, or was it constructed retroactively by bakufu-aligned scholars to legitimate an arrangement that arose from raw military necessity?',
    'Examine the chronology of doctrinal articulation (e.g., Kitabatake Chikafusa''s Jinno Shotoki, Neo-Confucian commentaries under the Tokugawa) relative to the actual establishment of shogunal institutions — does formal doctrine precede or trail institutional practice at each major transition (Kamakura founding, Muromachi consolidation, Tokugawa settlement)?',
    'If doctrine consistently trails and rationalizes existing power arrangements, the delegation reading is better modeled as an extraction-grounded authority structure (cs_structure authority_grounding: extraction) rather than a genuinely independent lineage-based doctrine; this would raise the confidence that theater_ratio understates rather than overstates the constraint''s performative character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_as_genuine_doctrine_vs_post_hoc_rationalization, conceptual, 'Whether delegation doctrine is prior justification or retroactive legitimation of samurai rule.').

omega_variable(
    kernel_reading_bifurcation_stability,
    'Is the imperial mandate kernel genuinely bifurcatable into a stable delegation reading and a stable restoration reading, or does the delegation reading contain the seeds of its own reversal — such that sufficient bakufu weakness always eventually triggers reversion to the restoration reading?',
    'Comparative analysis across all major bakufu transition points (Kamakura collapse, Ashikaga weakness during the Sengoku period, and the actual Meiji Restoration) — does restoration doctrine only succeed when bakufu administrative/military capacity has visibly collapsed, suggesting the readings are not independently stable but exist in a capacity-contingent equilibrium?',
    'If the delegation reading is only stable conditional on demonstrated administrative competence, this constraint''s classification should be understood as inherently time-limited/scaffold-like even absent an explicit sunset clause — the coordination justification decays automatically as competence decays, independent of any deliberate transition plan.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_bifurcation_stability, empirical, 'Whether the delegation reading is conditionally stable on demonstrated bakufu competence, or an independently persistent doctrine.').

omega_variable(
    peasant_indifference_to_reading,
    'Does the peasantry''s material experience of taxation and administrative control actually vary at all between the delegation reading and the restoration reading, or is this constraint''s extraction profile for that stakeholder group entirely orthogonal to which reading of the mandate prevails?',
    'Compare tax burden and administrative treatment of the peasantry immediately before and after the Meiji Restoration (the actual reading-switch event) controlling for the modernization reforms that accompanied it.',
    'If peasant material conditions are invariant to the reading, the peasantry''s inclusion as a ''victim'' of this specific constraint (rather than of governance-as-such) should be weighted lightly in directionality — their extraction is real but not specifically attributable to the delegation doctrine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(peasant_indifference_to_reading, empirical, 'Whether peasant extraction tracks the specific mandate reading or governance capacity generally.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__bakufu_delegation_reading, 0, 700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t0, imperial_mandate__bakufu_delegation_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement_basis(impe_tr_t0, observed).
narrative_ontology:measurement(impe_tr_t120, imperial_mandate__bakufu_delegation_reading, theater_ratio, 120, 0.45).
narrative_ontology:measurement_basis(impe_tr_t120, observed).
narrative_ontology:measurement(impe_tr_t250, imperial_mandate__bakufu_delegation_reading, theater_ratio, 250, 0.5).
narrative_ontology:measurement_basis(impe_tr_t250, observed).
narrative_ontology:measurement(impe_tr_t400, imperial_mandate__bakufu_delegation_reading, theater_ratio, 400, 0.55).
narrative_ontology:measurement_basis(impe_tr_t400, observed).
narrative_ontology:measurement(impe_tr_t550, imperial_mandate__bakufu_delegation_reading, theater_ratio, 550, 0.6).
narrative_ontology:measurement_basis(impe_tr_t550, observed).
narrative_ontology:measurement(impe_tr_t700, imperial_mandate__bakufu_delegation_reading, theater_ratio, 700, 0.62).
narrative_ontology:measurement_basis(impe_tr_t700, observed).

% Extraction over time
narrative_ontology:measurement(impe_be_t0, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(impe_be_t0, observed).
narrative_ontology:measurement(impe_be_t120, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 120, 0.42).
narrative_ontology:measurement_basis(impe_be_t120, observed).
narrative_ontology:measurement(impe_be_t250, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 250, 0.48).
narrative_ontology:measurement_basis(impe_be_t250, observed).
narrative_ontology:measurement(impe_be_t400, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 400, 0.52).
narrative_ontology:measurement_basis(impe_be_t400, observed).
narrative_ontology:measurement(impe_be_t550, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 550, 0.57).
narrative_ontology:measurement_basis(impe_be_t550, observed).
narrative_ontology:measurement(impe_be_t700, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 700, 0.58).
narrative_ontology:measurement_basis(impe_be_t700, observed).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t0, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(impe_su_t0, observed).
narrative_ontology:measurement(impe_su_t120, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 120, 0.55).
narrative_ontology:measurement_basis(impe_su_t120, observed).
narrative_ontology:measurement(impe_su_t250, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 250, 0.6).
narrative_ontology:measurement_basis(impe_su_t250, observed).
narrative_ontology:measurement(impe_su_t400, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 400, 0.65).
narrative_ontology:measurement_basis(impe_su_t400, observed).
narrative_ontology:measurement(impe_su_t550, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 550, 0.7).
narrative_ontology:measurement_basis(impe_su_t550, observed).
narrative_ontology:measurement(impe_su_t700, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 700, 0.71).
narrative_ontology:measurement_basis(impe_su_t700, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__bakufu_delegation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imperial_mandate__bakufu_delegation_reading, 0.12).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, loyalist_restoration_reading).

% DUAL FORMULATION NOTE:
% This constraint and loyalist_restoration_reading are the two readings of the imperial_mandate kernel. bakufu_delegation_reading authors bifurcated sovereignty with samurai administrative legitimacy and suppressed imperial political agency (tangled_rope, epsilon=0.58, moderate-high suppression). loyalist_restoration_reading authors unmediated imperial sovereignty as the sole legitimate form, with the delegation arrangement itself as an illegitimate usurpation to be reversed. The two share no beneficiary overlap: the delegation reading's beneficiaries (shogunal house, samurai stratum) are the restoration reading's targets, and vice versa. Historically the readings did not coexist as stable parallel positions but contested for doctrinal dominance, with the restoration reading achieving political victory at the 1868 Meiji Restoration — this is documented as a live historical case of one kernel reading structurally foreclosing the material conditions (samurai stipends, domain governance, shogunal office) that sustained its sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
