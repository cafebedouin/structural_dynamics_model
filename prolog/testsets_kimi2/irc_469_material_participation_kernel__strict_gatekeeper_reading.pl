% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strict_gatekeeper_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__strict_gatekeeper_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: irc_469_material_participation_kernel__strict_gatekeeper_reading
 *   human_readable: IRC Â§469 Strict Material Participation Gatekeeper
 *   domain: tax law / real estate investment / regulatory interpretation
 *
 * SUMMARY:
 *   This constraint story models the strict gatekeeper reading of IRC Â§469's
 *   material participation requirement, under which taxpayers must produce
 *   verifiable evidence of substantial personal labor to deduct passive real
 *   estate losses against ordinary income. The reading treats the statutory
 *   threshold as a high-documentation compliance barrier that narrows the
 *   qualifying population and suspends most passive losses. It is
 *   instantiated as one reading of the contested
 *   irc_469_material_participation_kernel, structurally opposed to the
 *   strategic_shelter_reading that would permit aggressive hour-counting and
 *   grouping elections. The strict reading is authored as a tangled rope: it
 *   coordinates a genuine anti-shelter function while asymmetrically
 *   extracting compliance costs and loss deductions from passive investors.
 *
 * KEY AGENTS:
 *   - internal_revenue_service: Agenda-setter (institutional/analytical) â interprets and enforces the material participation test through regulations and audit programs.
 *   - us_treasury: Beneficiary (institutional/analytical) â collects revenue from disallowed passive loss deductions.
 *   - materially_participating_investors: Beneficiary (moderate/mobile) â retain loss deduction access by meeting the labor threshold, differentiated from passive capital.
 *   - passive_real_estate_investors: Primary target (powerful/constrained) â bear suspended losses and high compliance friction.
 *   - tax_shelter_promoters: Excluded (powerful/constrained) â advocate for permissive readings foreclosed by strict interpretation.
 *   - federal_tax_judiciary: Observer (institutional/analytical) â reviews disputes and substantiation adequacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.72).
domain_priors:suppression_score(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.68).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strict_gatekeeper_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strict_gatekeeper_reading, "IRC Â§469 Strict Material Participation Gatekeeper").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strict_gatekeeper_reading, "tax law / real estate investment / regulatory interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strict_gatekeeper_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strict_gatekeeper_reading, '01f94b1e-4559-41b2-99c8-2b6d39322525').
narrative_ontology:cs_kernel_codification('01f94b1e-4559-41b2-99c8-2b6d39322525', formalized).
narrative_ontology:cs_authority_grounding('01f94b1e-4559-41b2-99c8-2b6d39322525', lineage).
narrative_ontology:cs_interpretation_layer_present('01f94b1e-4559-41b2-99c8-2b6d39322525').
narrative_ontology:cs_reading_relation('01f94b1e-4559-41b2-99c8-2b6d39322525', irc_469_material_participation_kernel__strategic_shelter_reading, influences).
narrative_ontology:cs_axiom('01f94b1e-4559-41b2-99c8-2b6d39322525', foundational, personal_labor_verifiability_prerequisite).
narrative_ontology:cs_axiom_status(personal_labor_verifiability_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('01f94b1e-4559-41b2-99c8-2b6d39322525', personal_labor_verifiability_prerequisite, conventional).
narrative_ontology:cs_axiom('01f94b1e-4559-41b2-99c8-2b6d39322525', secondary, high_documentation_bar_justified).
narrative_ontology:cs_axiom_status(high_documentation_bar_justified, holdable).
narrative_ontology:cs_axiom_grounding('01f94b1e-4559-41b2-99c8-2b6d39322525', high_documentation_bar_justified, instrumental).
narrative_ontology:cs_reference_frame('01f94b1e-4559-41b2-99c8-2b6d39322525', statutory_anti_shelter_framework).
narrative_ontology:cs_drift_state('01f94b1e-4559-41b2-99c8-2b6d39322525', contemporary_strict_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('01f94b1e-4559-41b2-99c8-2b6d39322525', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, us_treasury).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, materially_participating_investors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, passive_real_estate_investors).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strict_gatekeeper_reading, passive_loss_ring_fence_doctrine).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strict_gatekeeper_reading, anti_shelter_statutory_integrity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces IRC Â§469 material participation standards through Treasury regulations, revenue procedures, and audit programs. Defines what counts as verifiable, substantial personal labor and sets the documentation bar that taxpayers must clear. Can alter compliance friction through guidance and audit strategy but remains bound by the statutory framework enacted in 1986.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, internal_revenue_service, agenda_setter,
    institutional, generational, analytical, national).

% Collects additional tax revenue when passive real estate losses are suspended or disallowed under strict material participation interpretations. Benefits from a broader, more stable tax base that is protected from erosion by passive loss shelters.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, us_treasury, beneficiary,
    institutional, generational, analytical, national).

% Real estate professionals and active operators who regularly perform substantial labor in their activities and can produce contemporaneous records of hours and tasks. They retain access to full loss deductions and other tax advantages that passive investors lose under strict gatekeeping, preserving their competitive position against purely passive capital.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, materially_participating_investors, beneficiary,
    moderate, biographical, mobile, national).

% Invest in real estate primarily for capital appreciation and portfolio diversification but do not personally perform substantial labor. Under the strict reading, they cannot deduct passive losses against ordinary income and face a high documentation barrier if they attempt to qualify. Restructuring into active participation is costly, often impractical, and may not survive audit.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, passive_real_estate_investors, payer,
    powerful, biographical, constrained, national).

% Advisors and syndicators who design aggressive grouping elections, hour-counting strategies, and interpretive arguments to enable passive investors to claim material participation. Their preferred strategies are directly foreclosed by the strict reading's documentation and substantiation requirements, and they are not parties to IRS interpretive guidance processes.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_shelter_promoters, excluded,
    powerful, biographical, constrained, national).

% Tax Court and federal appellate courts review taxpayer challenges to IRS material participation determinations. They evaluate whether taxpayers' documentation meets the strict reading's substantiation bar, serving as arbiters of interpretive disputes between the IRS and taxpayers without directly administering the rule.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, federal_tax_judiciary, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents high-income taxpayers from using passive real estate investments as tax shelters against ordinary salary and business income, preserving the integrity of the progressive tax base and preventing distortion of investment decisions by tax avoidance rather than economic merit.
% TRANSFER_FUNCTION: Moves suspended tax losses and deferred tax liabilities from passive real estate investors to the federal fisc via disallowed current deductions, while preserving immediate deduction access for those who perform verifiable substantial labor. Also transfers compliance documentation burden from the IRS to taxpayers.
% ABSENT_VOICES: Tax shelter promoters and passive investment syndicators who would advocate for permissive hour-counting and loose grouping standards are structurally excluded from the interpretive process that produces the strict reading; their strategies are classified as non-compliant before reaching the deliberative table.
% DISAPPEARANCE_RATIONALE: If the strict material participation requirement vanished, passive real estate investors would immediately deduct suspended losses against ordinary income, collapsing the passive activity loss regime's revenue fence. The Treasury would face significant revenue loss, and investment capital would likely shift toward tax-favored passive real estate structures, rearranging both fiscal flows and market composition.
% FOUNDING_PROBLEM: Congress enacted IRC Â§469 in 1986 to halt widespread abuse of tax shelters by high-income individuals who used paper losses from passive real estate partnerships to offset wages and portfolio income, eroding the tax base.
% FOUNDING_PROBLEM_CORROBORATION: The Joint Committee on Taxation and 1986 legislative history attest to the original shelter problem. However, independent tax economists and real estate industry groups contest whether the current strict reading's documentation bar remains proportionate to that problem, or whether it now captures non-abusive investment activity Congress did not intend to penalize. No external party outside the revenue-collection interest corroborates that the current strict gatekeeping level is necessary.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strict_gatekeeper_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strict_gatekeeper_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) reflects the high documentation bar and narrow qualifying population that effectively deny most passive investors the deduction. Suppression (0.68) captures the active exclusion of alternative shelter strategies through audit and penalty. Theater ratio (0.25) acknowledges real anti-abuse function while recognizing that some compliance activity is ritualistic documentation production. Accessibility collapse (0.75) is high because once the strict reading is applied, taxpayers' alternatives reduce to either restructuring into active participation (costly) or accepting loss suspension. Resistance (0.45) reflects persistent litigation and aggressive planning. The metrics and claimed type are authored independently: the strict reading operates as a tangled rope because the anti-shelter coordination is structurally inseparable from the asymmetric extraction it imposes on non-qualifying investors.
 *
 * PERSPECTIVAL GAP:
 *   The IRS and Treasury experience the constraint as necessary statutory enforcement preserving tax base integrity. Passive investors experience the same rules as extraction of tax benefits they would otherwise claim. Materially participating investors sit in between: they benefit from the gatekeeping that prevents passive capital from arbitraging their tax-advantaged status. The engine computes these divergent seat classifications from the structural data rather than from any authored type override.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (us_treasury, materially_participating_investors) derive low directionality from their structural position: the constraint subsidizes or protects their position. Victims (passive_real_estate_investors) derive high directionality because the constraint extracts deductions and imposes compliance costs. The federal_tax_judiciary sits near symmetric as an analytical observer. No override is needed: the derivation chain produces accurate directionalities from the declared roles and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â widespread tax shelter abuse in the 1980s â is contested in status. If the problem is dead but the strict arrangement persists, the constraint risks piton or snare drift. However, because the anti-shelter coordination function remains live (evidenced by ongoing shelter promotion), the constraint retains genuine coordination content despite its extraction. The R5 genealogy (dead founding problem + world rearranges if removed) triggers mandatrophy monitoring: the mismatch between status and verdict flags the need for temporal tracking. The authored measurements show slowly rising extraction and theater, consistent with gradual drift but not yet mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the strict gatekeeper reading the only structurally coherent interpretation of IRC Â§469 material participation, or does the strategic shelter reading represent an equally valid textual construction?',
    'Judicial consensus or legislative amendment clarifying the documentation and hour standards required to establish material participation.',
    'If the strategic reading is textually valid, the strict reading''s high extraction is an interpretive choice rather than statutory necessity, shifting classification toward snare. If the strict reading is the only coherent construction, extraction is mandated by law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the strict reading is statutorily compelled or interpretively optional').

omega_variable(
    substantial_labor_ambiguity,
    'Does substantial personal labor have an objective measurable threshold, or is the documentation bar an arbitrary enforcement lever?',
    'Empirical study of audit outcomes and Tax Court rulings measuring variance in participation findings across similarly situated taxpayers.',
    'If findings are inconsistent, the strict reading extracts through discretionary enforcement rather than clear rules, raising effective suppression beyond the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substantial_labor_ambiguity, empirical, 'Whether the material participation standard is applied consistently').

omega_variable(
    founding_problem_obsolescence,
    'Has the original 1986 tax shelter abuse problem subsided to the point that strict material participation gatekeeping now captures non-abusive investment?',
    'Comparative revenue estimates and industry participation studies measuring the share of disallowed losses attributable to genuine shelters versus ordinary passive investment.',
    'If the shelter problem is dead, the constraint''s coordination function is attenuated and its classification drifts toward piton or snare; if still live, tangled rope remains accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the anti-shelter founding problem remains live').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(irc__tr_t6, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(irc__tr_t12, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(irc__tr_t18, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 18, 0.18).
narrative_ontology:measurement(irc__tr_t24, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(irc__tr_t30, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(irc__be_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(irc__be_t6, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(irc__be_t12, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(irc__be_t18, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(irc__be_t24, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(irc__be_t30, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(irc__su_t6, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(irc__su_t12, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(irc__su_t18, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 18, 0.64).
narrative_ontology:measurement(irc__su_t24, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(irc__su_t30, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strict_gatekeeper_reading, resource_allocation).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, strategic_shelter_reading).

% DUAL FORMULATION NOTE:
% This story and strategic_shelter_reading are decomposed readings of the irc_469_material_participation_kernel. The same statutory text and regulatory history ground both, but they instantiate structurally distinct constraints with different epsilon values, beneficiary/victim structures, and coordination/extraction balances. The strict reading is linked to the strategic reading as its sibling in the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
