% ============================================================================
% CONSTRAINT STORY: salic_prohibition__cognatic_reversion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Salic Exclusion as Misapplied Frankish Custom (Cognatic Reversion Reading)
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This story instantiates the cognatic reversion reading of the contested
 *   Salic prohibition kernel: the claim that Salic Law was a specific
 *   Frankish rule of allodial land inheritance among the Salian Franks, never
 *   enacted as constitutional law outside that original jurisdiction, and
 *   therefore not properly binding on the many later dynastic territories
 *   that invoked it to bar female succession. Under this reading, the correct
 *   default in a non-Frankish territory is cognatic primogeniture — the
 *   eldest child inherits regardless of sex — and territorial integrity
 *   (keeping the realm as its native law constituted it) is privileged over
 *   an imported standard of agnatic purity. This is a distinct constraint
 *   from the immutable_mandate_reading (which treats the rule as embedded
 *   natural/divine dynastic constitution) and the sovereign_override_reading
 *   (which treats it as ordinary positive law a sovereign may revoke by
 *   legislative act) — the three readings share a kernel text and historical
 *   episode but disagree on what the kernel actually established and where
 *   its writ runs, and each authors its own epsilon rather than averaging
 *   across the dispute.
 *
 * KEY AGENTS:
 *   - collateral_agnatic_claimants: beneficiary (powerful/arbitrage) — inherit by displacing the nearer female claimant
 *   - male_succession_dependent_courts: agenda_setter (institutional/constrained) — enforce the imported rule as settled law
 *   - eldest_daughters_of_reigning_houses: payer (powerless/trapped) — displaced from a succession they hold under native custom
 *   - cognatic_line_territories: payer (moderate/constrained) — lose territorial integrity to an agnatic standard foreign to their own law
 *   - cognatic_claimant_and_her_issue: excluded (powerless/trapped) — has no forum to press the claim
 *   - legal_historians_of_succession: observer (analytical) — documents the rule's original narrow scope and later misapplication
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, 0.62).
domain_priors:suppression_score(salic_prohibition__cognatic_reversion_reading, 0.58).
domain_priors:theater_ratio(salic_prohibition__cognatic_reversion_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__cognatic_reversion_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__cognatic_reversion_reading, "Salic Exclusion as Misapplied Frankish Custom (Cognatic Reversion Reading)").
narrative_ontology:topic_domain(salic_prohibition__cognatic_reversion_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__cognatic_reversion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__cognatic_reversion_reading, '94551445-f9b9-43d9-b100-9d034609e5e1').
narrative_ontology:cs_kernel_codification('94551445-f9b9-43d9-b100-9d034609e5e1', distributed).
narrative_ontology:cs_authority_grounding('94551445-f9b9-43d9-b100-9d034609e5e1', distributed).
narrative_ontology:cs_reading_relation('94551445-f9b9-43d9-b100-9d034609e5e1', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('94551445-f9b9-43d9-b100-9d034609e5e1', salic_prohibition__sovereign_override_reading, coexists_with).
narrative_ontology:cs_axiom('94551445-f9b9-43d9-b100-9d034609e5e1', foundational, jurisdictional_non_extension_of_frankish_custom).
narrative_ontology:cs_axiom_status(jurisdictional_non_extension_of_frankish_custom, holdable).
narrative_ontology:cs_axiom_grounding('94551445-f9b9-43d9-b100-9d034609e5e1', jurisdictional_non_extension_of_frankish_custom, empirically_contingent).
narrative_ontology:cs_axiom('94551445-f9b9-43d9-b100-9d034609e5e1', foundational, territorial_integrity_over_agnatic_purity).
narrative_ontology:cs_axiom_status(territorial_integrity_over_agnatic_purity, holdable).
narrative_ontology:cs_axiom_grounding('94551445-f9b9-43d9-b100-9d034609e5e1', territorial_integrity_over_agnatic_purity, conventional).
narrative_ontology:cs_reference_frame('94551445-f9b9-43d9-b100-9d034609e5e1', frankish_allodial_land_custom).
narrative_ontology:cs_drift_state('94551445-f9b9-43d9-b100-9d034609e5e1', post_medieval_dynastic_union_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('94551445-f9b9-43d9-b100-9d034609e5e1', '').
narrative_ontology:cs_kernel_id(salic_prohibition__cognatic_reversion_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, collateral_agnatic_claimants).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, male_succession_dependent_courts).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, eldest_daughters_of_reigning_houses).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, cognatic_line_territories).
narrative_ontology:constraint_vindicates(salic_prohibition__cognatic_reversion_reading, territorial_integrity_doctrine).
narrative_ontology:constraint_vindicates(salic_prohibition__cognatic_reversion_reading, cognatic_primogeniture_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Male relatives in the collateral line who would inherit only if the eldest daughter or her line is excluded. They invoke Salic Law imported from Frankish custom to displace a nearer female or cognatic heir, even where the territory in question was never Frankish and never adopted the rule by its own constitutional process.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, collateral_agnatic_claimants, beneficiary,
    powerful, generational, arbitrage, national).

% Royal councils, jurists, and heraldic authorities who administer and enforce the exclusion rule, treating it as settled dynastic constitution. They cite precedent from Frankish succession disputes without establishing that the territory's own founding law ever incorporated the rule, and they benefit institutionally from a bright-line rule that forecloses litigation over female claims.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, male_succession_dependent_courts, agenda_setter,
    institutional, generational, constrained, national).

% First-born daughters of monarchs who, under this reading, should inherit by cognatic primogeniture but are displaced in favor of a more distant male relative. They have no exit from their birth position and no forum that will hear a challenge to the rule's applicability, since the same courts that would adjudicate it are the ones enforcing it.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, eldest_daughters_of_reigning_houses, payer,
    powerless, biographical, trapped, national).

% Provinces and client territories whose own inheritance customs recognized female or cognatic succession before absorption into a dynastic union. Under the imported rule their territorial integrity is subordinated to an agnatic purity standard foreign to their own legal tradition, producing partition or foreign rule when the direct female line is passed over.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, cognatic_line_territories, payer,
    moderate, civilizational, constrained, national).

% The excluded daughter's own descendants, who would have held a valid succession right in the territory's native custom. They have standing to object in principle but no institutional forum recognizes their claim, since the enforcing courts are staffed and legitimated by the agnatic line they would displace.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, cognatic_claimant_and_her_issue, excluded,
    powerless, biographical, trapped, national).

% Scholars who trace the actual textual and jurisdictional history of Salic Law, documenting that the Pactus Legis Salicae governed inheritance of allodial land among Franks and was never enacted as constitutional law in territories that later invoked it against female heirs.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, legal_historians_of_succession, observer,
    analytical, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, litigation-resistant rule for resolving succession disputes among multiple possible heirs, reducing the risk of contested successions and civil war over ambiguous claims.
% TRANSFER_FUNCTION: Moves the crown, its territories, and the resources attached to them from the eldest daughter's line to a more distant male relative's line, and moves interpretive authority over succession from the excluded territory's native legal tradition to the importing dynasty's agnatic jurists.
% ABSENT_VOICES: The excluded daughters and their issue, and the legal traditions of the cognatic-line territories themselves, are never seated in the councils that decide whether Salic Law applies to them at all — the question of applicability is decided entirely by parties who benefit from an affirmative answer.
% DISAPPEARANCE_RATIONALE: If the imported exclusion rule were withdrawn, the eldest daughter or her line would succeed under the territory's own cognatic custom, entire succession crises and wars of succession fought over invoking or denying Salic applicability would not have occurred, and several historical partitions and dynastic unions would not have taken the shape they did.
% FOUNDING_PROBLEM: Frankish inheritance law was built to keep allodial land within the male kin group among the Salian Franks — a specific tribal property-succession problem, not a general theory of dynastic monarchy.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the dynastic courts (working from the Pactus Legis Salicae text and comparative succession law) attest the rule's original problem was Frankish allodial land tenure, already obsolete by the time it was invoked in later royal successions; no source outside the agnatic claimants and the courts that seated them attests the rule was ever properly enacted as constitutional law in the excluded territories.
narrative_ontology:disappearance_verdict(salic_prohibition__cognatic_reversion_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__cognatic_reversion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__cognatic_reversion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(salic_prohibition__cognatic_reversion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__cognatic_reversion_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.62: substantial but not maximal, because the rule does perform a real coordination function (a bright-line succession rule reduces contested-succession violence) even under this reading — the extraction is in WHO the bright line favors and WHERE it is applied, not in the existence of a bright line as such. Suppression sits at 0.58: the excluded daughters and cognatic territories face real institutional foreclosure (no forum will hear the applicability challenge) but not the near-total suppression of a constraint with no coordination cover story at all. Theater ratio rises across the interval (0.20 to 0.45) as the rule increasingly functions as ex post justification for outcomes already decided by the collateral line's political power, rather than as a genuinely contested legal question — later invocations lean more on citing precedent than on litigating the actual jurisdictional scope of the original Frankish text. Accessibility collapse is moderate (0.40): unlike a genuine mountain, the alternative (native cognatic custom) is well documented and was live within the excluded territories' own tradition, it is simply foreclosed by the enforcing institution's composition, not by the absence of a coherent alternative. Resistance is high (0.70) because excluded claimants, their partisans, and territories that stood to inherit cognatically did contest applicability repeatedly across succession crises.
 *
 * PERSPECTIVAL GAP:
 *   From the agnatic claimant and enforcing-court seats, the rule reads as a stabilizing dynastic constitution correctly extended to a new territory upon union or inheritance. From the excluded daughter and cognatic-territory seats, the same structure reads as an anachronistic import applied without the territory's own constitutional consent, extracting a right that existed under prior native law. The engine should compute these as different per-seat classifications from the same structural data — the coordination story is real for the enforcing seat and the extraction is real for the payer seat, which is exactly the tangled-rope signature this reading claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Collateral agnatic claimants and the courts that seat them are the structural beneficiaries: the rule transfers the crown and its resources to them and gives the enforcing jurists a bright line that forecloses costly succession litigation, so directionality sits near the full-beneficiary end for both. Eldest daughters and cognatic-line territories are the structural targets: the same rule extracts a succession right and a territorial legal identity from them, so directionality sits near the full-target end, amplified by their trapped/constrained exit options — a first-born daughter cannot change her birth position, and a territory absorbed into a dynastic union cannot easily reassert its native succession custom without war or partition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (keeping allodial land within the male Salian Frankish kin group) is dead outside its original jurisdiction — later invocations solve a different problem (resolving succession disputes among competing dynastic claimants) using a rule whose original justification does not transfer. Declaring tangled_rope rather than snare preserves the genuine coordination function (a bright-line succession rule does reduce civil war risk) while still naming the asymmetric extraction (the bright line's content was chosen to favor the agnatic line, not derived from the receiving territory's own law) and the active enforcement the rule requires to hold against native cognatic counter-claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_choice,
    'Is Salic Law, as applied historically outside Francia, best understood as a Frankish-specific inheritance custom wrongly extended (this reading), an irrevocable natural/divine dynastic mandate (immutable_mandate_reading), or ordinary positive law subject to sovereign revocation (sovereign_override_reading)?',
    'Comparative constitutional history: examine whether the receiving territory''s own founding law or estates ever formally enacted the Salic exclusion, versus whether it was imposed by the arriving dynasty''s jurists without local constitutional process.',
    'If the receiving territory''s own law shows no formal enactment, this reading''s jurisdictional-anachronism claim strengthens and the tangled_rope classification (real coordination function, asymmetric imported extraction) holds; if formal local enactment is found, the constraint collapses toward the sovereign_override_reading instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_choice, conceptual, 'Which of the three kernel readings best fits the historical jurisdictional record for a given succession dispute.').

omega_variable(
    coordination_versus_pure_pretext,
    'Does the bright-line succession function genuinely reduce civil war risk (a real coordination benefit under this reading), or is the coordination story pure cover for collateral-line capture in every documented instance?',
    'Compare succession outcomes in cases where cognatic claims were honored versus barred: if honored-cognatic successions show comparable or lower rates of civil war than barred ones, the coordination justification weakens toward pretext.',
    'If coordination benefit is negligible across cases, the classification should move from tangled_rope toward snare (extraction with no genuine coordination function); if coordination benefit is real and substantial, tangled_rope is the correct call.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_versus_pure_pretext, empirical, 'Whether the succession-stability coordination function is real or a pretext for agnatic capture.').

omega_variable(
    territorial_law_survival,
    'Did the excluded territories'' native cognatic succession custom persist as living law (in local courts, estates, or charters) after the imposition of the agnatic rule, or had it already lapsed independently?',
    'Archival review of the territory''s own charters, estate records, and local court rulings on inheritance in the period immediately preceding the disputed succession.',
    'If native cognatic custom was still live and enforced locally, the imposition is a clearer case of extraction overriding a functioning alternative; if it had already lapsed, the exclusion rule''s marginal extraction is smaller than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_law_survival, empirical, 'Whether the cognatic alternative this reading privileges was living law or already dormant at the time of exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__cognatic_reversion_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__cognatic_reversion_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sali_tr_t8, salic_prohibition__cognatic_reversion_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(sali_tr_t16, salic_prohibition__cognatic_reversion_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(sali_tr_t24, salic_prohibition__cognatic_reversion_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(sali_tr_t32, salic_prohibition__cognatic_reversion_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(sali_tr_t40, salic_prohibition__cognatic_reversion_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sali_be_t8, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(sali_be_t16, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(sali_be_t24, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(sali_be_t32, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(sali_be_t40, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sali_su_t8, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(sali_su_t16, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(sali_su_t24, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(sali_su_t32, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(sali_su_t40, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__sovereign_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the salic_prohibition kernel. The immutable_mandate_reading authors a near-zero epsilon (correct application of settled dynastic constitution, no illegitimate extraction). The sovereign_override_reading authors epsilon concentrated on whichever direction a specific sovereign's revocation decision cuts, centered on legislative authority rather than territorial jurisdiction. This reading (cognatic_reversion) authors epsilon around 0.62, reflecting a genuine but asymmetrically-captured coordination function. The three do not average; each is a separate constraint instantiated from a shared contested kernel and linked here for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
