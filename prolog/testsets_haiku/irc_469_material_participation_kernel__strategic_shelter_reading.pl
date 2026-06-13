% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strategic_shelter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__strategic_shelter_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: irc_469_material_participation_kernel__strategic_shelter_reading
 *   human_readable: IRC §469 Material Participation—Strategic Shelter Reading
 *   domain: tax_law/regulatory_interpretation
 *
 * SUMMARY:
 *   Under the strategic-shelter reading of IRC §469 material participation,
 *   the statutory requirement that a taxpayer materially participate in a
 *   rental real estate activity to claim passive loss deductions is satisfied
 *   through aggressive hour-counting, grouping elections under Treas. Reg.
 *   §1.469-4(f), and favorable interpretation of 'relevant participation
 *   factors' (Treas. Reg. §1.469-5T). This reading permits high-income
 *   investors to engineer material participation status through documentation
 *   and aggregation strategies, enabling them to claim losses that shelter
 *   ordinary income—the very outcome the 1986 statute intended to prevent.
 *   The constraint is a tangled rope: it coordinates the tax system's
 *   allocation function (determining who bears the burden of taxation) while
 *   simultaneously extracting wealth from those unable to access the
 *   permissive interpretation. This is one reading of a contested kernel: the
 *   IRC §469 material participation requirement itself. The sibling
 *   strict-gatekeeper reading interprets material participation as a genuine
 *   participation bar requiring substantial, verifiable personal labor and
 *   tight documentation.
 *
 * KEY AGENTS:
 *   - high_income_real_estate_investors: Primary beneficiary (d ≈ 0.15, arbitrage exit) — can engineer participation claims and offset passive income
 *   - passive_loss_shelter_users: Secondary beneficiary (d ≈ 0.20, mobile exit) — use the permissive standard to structure deductions
 *   - wage_earners_with_passive_income: Primary payer (d ≈ 0.75, trapped exit) — cannot credibly claim equivalent participation despite comparable effort
 *   - small_landlords_unable_to_qualify: Secondary payer (d ≈ 0.70, constrained exit) — lack resources for sophisticated structuring
 *   - tax_preparation_industry: Beneficiary-agenda-setter (d ≈ 0.25, mobile exit) — profits from interpretive ambiguity and resists clarification
 *   - internal_revenue_service: Institutional agenda-setter (d ≈ analytical, analytical exit) — operationalizes the permissive reading through regulations and audit practice
 *   - congress: Excluded observer (d ≈ analytical, analytical exit) — legislative intent is displaced by regulatory/audit domain practice
 *   - compliance_courts: Observer (d ≈ analytical, analytical exit) — rulings establish precedent for permissive standard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, 0.68).
domain_priors:suppression_score(irc_469_material_participation_kernel__strategic_shelter_reading, 0.52).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strategic_shelter_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strategic_shelter_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strategic_shelter_reading, "IRC §469 Material Participation—Strategic Shelter Reading").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strategic_shelter_reading, "tax_law/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strategic_shelter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strategic_shelter_reading, 'da1028e7-8095-4f48-bcc5-e0d6fe17ef05').
narrative_ontology:cs_kernel_codification('da1028e7-8095-4f48-bcc5-e0d6fe17ef05', fixed_text).
narrative_ontology:cs_authority_grounding('da1028e7-8095-4f48-bcc5-e0d6fe17ef05', lineage).
narrative_ontology:cs_interpretation_layer_present('da1028e7-8095-4f48-bcc5-e0d6fe17ef05').
narrative_ontology:cs_reading_relation('da1028e7-8095-4f48-bcc5-e0d6fe17ef05', irc_469_material_participation_kernel__strict_gatekeeper_reading, coexists_with).
narrative_ontology:cs_axiom('da1028e7-8095-4f48-bcc5-e0d6fe17ef05', foundational, material_participation_permissive_qualification).
narrative_ontology:cs_axiom_status(material_participation_permissive_qualification, holdable).
narrative_ontology:cs_axiom_grounding('da1028e7-8095-4f48-bcc5-e0d6fe17ef05', material_participation_permissive_qualification, conventional).
narrative_ontology:cs_axiom('da1028e7-8095-4f48-bcc5-e0d6fe17ef05', secondary, documentation_as_participation_proxy).
narrative_ontology:cs_axiom_status(documentation_as_participation_proxy, holdable).
narrative_ontology:cs_axiom_grounding('da1028e7-8095-4f48-bcc5-e0d6fe17ef05', documentation_as_participation_proxy, conventional).
narrative_ontology:cs_reference_frame('da1028e7-8095-4f48-bcc5-e0d6fe17ef05', flexible_investor_participation_standard).
narrative_ontology:cs_drift_state('da1028e7-8095-4f48-bcc5-e0d6fe17ef05', contemporary_post_2010_stabilization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('da1028e7-8095-4f48-bcc5-e0d6fe17ef05', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_real_estate_investors).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, passive_loss_shelter_users).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, wage_earners_with_passive_income).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, small_landlords_unable_to_qualify).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strategic_shelter_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the permissive standard systematically transfers tax incidence from high-income investors to others by enabling deductions disconnected from genuine economic participation. The transfer is substantial and sustained. Suppression is moderate (0.52) because high-income beneficiaries do not need coercive suppression—the regulatory regime favors them, and compliance friction is low for professionally-represented taxpayers. Wage earners and small landlords face compliance costs and audit risk that suppress their ability to compete, but this is asymmetric enforcement rather than overt coercion. Theater is moderate-high (0.41) because the material participation requirement itself is performed (hours are counted, documents are prepared) but increasingly as a formality—the outcome is predetermined by the taxpayer's ability to afford sophisticated structuring. The measurement series shows gradual extraction accumulation from 1986 to 2026 as sophisticated planning strategies proliferated and audit capacity declined, with theater rising as documentation became ritualized. The plateau from year 30 to 40 reflects stabilization of the interpretive practice: the permissive reading is now entrenched.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (high-income investors and the tax profession), the constraint is genuine coordination: the material participation standard prevents arbitrary loss sheltering, and the permissive interpretation is a reasonable read of statutory text allowing qualified investors to claim legitimate deductions. From the payer seats (wage earners and small landlords), the same constraint operates as a discrimination mechanism: identical participation generates different tax treatment based on the taxpayer's ability to afford professional representation and documentation sophistication. The IRS's perspective sits between: it administers the constraint as written but acknowledges in internal guidance that the permissive reading creates inconsistency across audit populations. The engine will compute different effective extractions for each seat based on exit options and power asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   High-income investors derive d ≈ 0.15 (near beneficiary end) from their beneficiary role, arbitrage exit (they can exit by divesting, but choose to stay because the returns are favorable), powerful position (they can afford professional structuring), and high time horizon (they plan long-term real estate portfolios). Wage earners derive d ≈ 0.75 (near target end) from their payer role, trapped exit (they cannot exit without incurring capital loss or complex restructuring), moderate power (they lack resources for equivalent structuring), and biographical time horizon. The tax profession derives d ≈ 0.25 because it benefits (secondary beneficiary through service revenue) but is not the direct capturer (the investor captures the tax savings) and has mobile exit (they could shift service focus if the standard tightened). The IRS sits at d ≈ analytical (institutional observer) because it administers the constraint but does not personally benefit or pay—it is the enforcement machine, not an interested party.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint presents a classical mandatrophy scenario: the founding problem (preventing shelter abuse) remains nominally live (the statute is still on the books, the IRS still administers material participation tests) but the mechanism has atrophied into something closer to a tax planning tool than a gatekeeper. The founding problem status is 'contested' precisely because both readings coexist: the strategic-shelter reading attests participation qualification has become routine for qualified taxpayers; the strict-gatekeeper reading attests the founding bar has been circumvented. The key to mandatrophy detection is the disappearance verdict: if the constraint vanished, the world would rearrange (investors would restructure, revenue would shift) — so the constraint is not yet pure piton. But the theater_ratio of 0.41 and the moderate suppression (0.52) suggest the real enforcement mechanism is not the material participation requirement itself but the asymmetric ability to document and structure around it. A piton candidate would show much higher theater and lower suppression; a mature snare would show lower theater and higher suppression. This constraint sits in tangled-rope territory with mandatrophy-watching relevance: the founding mandate is displaced, but not yet inert.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    congressional_intent_vs_regulatory_practice,
    'Does the permissive material participation standard operationalized by IRS regulations and audit practice align with the 1986 Congressional intent to create a genuine participation bar, or has regulatory drift displaced the original mandate?',
    'Historical audit data showing acceptance rates for material participation claims by taxpayer income level and professional representation; econometric analysis comparing passive loss deductions claimed under §469 to Treasury revenue-loss estimates from 1986; comparative study of audit outcomes in circuits with divergent case law.',
    'If regulatory drift is confirmed, the constraint would be reclassified from tangled-rope (coordinate + extract) to snare (pure extraction with coordination cover), or to piton (atrophied mandate maintained theatrically). If alignment is confirmed, the strategic reading is correct and the constraint functions as intended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_intent_vs_regulatory_practice, empirical, 'Whether regulatory practice reflects or contradicts Congressional statutory intent.').

omega_variable(
    documentation_vs_participation_equivalence,
    'Is aggressive hour-counting and grouping-election structuring a legitimate proxy for genuine economic participation, or does it systematically misidentify passive arrangement-shopping as material participation?',
    'Comparative analysis of actual labor inputs and capital-at-risk correlations for material participation claimants vs. non-claimants; audit examination of contemporaneous time records and their correlation with actual business decisions; case studies of properties claimed as material participation where passive loss deductions were followed by inactive management or rapid dispositions.',
    'If documentation proves a reliable proxy, the permissive standard is justified; if documentation is decoupled from actual participation, the standard is a documentation gatekeeper rather than a participation gatekeeper—restructuring the constraint as extraction masked by bureaucratic formality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(documentation_vs_participation_equivalence, empirical, 'Whether hour-counting documents correlate with genuine economic participation.').

omega_variable(
    kernel_reading_legitimacy,
    'Is the permissive reading of material participation a coherent interpretation of statutory text and regulatory authority, or does it stretch Treasury authority beyond reasonable construction?',
    'Textualist statutory analysis of §469(c)-(h) and ''material participation'' definition in §469(h); review of Treasury''s express delegated authority under §469(j); tax law scholarship evaluating whether Treas. Reg. §1.469-5T and §1.469-4(f) grouping elections exceed the agency''s interpretive scope.',
    'If the reading is within Treasury''s delegated authority, it is legitimate interpretive power; if it exceeds authority, the constraint would be vulnerable to judicial reversal or Congressional override. This affects whether the permissive standard is sustainable or susceptible to foreclosure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_legitimacy, conceptual, 'Whether the permissive reading is a valid statutory interpretation or regulatory overreach.').

omega_variable(
    audit_selection_structural_bias,
    'Do audit selection and settlement patterns favor high-income investors and professional representation in material participation disputes, or are they neutral across taxpayer populations?',
    'IRS audit data by taxpayer income, audit rate by claimed participation status, settlement rate by representation type, correlation between professional tax representation and successful material participation defenses.',
    'Evidence of bias would indicate suppression asymmetry: the constraint operates as a low-friction benefit for high-income beneficiaries but high-friction risk for low-income payers. This would strengthen the tangled-rope classification and confirm extraction asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_selection_structural_bias, empirical, 'Whether audit enforcement is asymmetric across taxpayer wealth and representation.').

omega_variable(
    alternative_readings_foreclosure,
    'Does this strategic-shelter reading logically foreclose the strict-gatekeeper reading, or do they coexist as legitimate alternative interpretations of the same statute?',
    'Textual analysis of whether permissive hour-counting aggregation and strict participation requirements are mutually exclusive constructions of §469 text, or whether both are defensible readings of ambiguous statutory language.',
    'If foreclosure occurs, one reading must yield and the other becomes the settled interpretation. If coexistence holds, both readings persist and the constraint remains contested—audit practice and legislative action become the real dispute-resolution mechanism rather than textual clarity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_readings_foreclosure, conceptual, 'Whether the strategic and strict readings are logically compatible or mutually exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strategic_shelter_reading, 1986, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t0, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(irc__tr_t5, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(irc__tr_t10, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(irc__tr_t15, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(irc__tr_t20, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(irc__tr_t25, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(irc__tr_t30, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(irc__tr_t40, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(irc__be_t0, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(irc__be_t5, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(irc__be_t10, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(irc__be_t15, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(irc__be_t20, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(irc__be_t25, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(irc__be_t30, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(irc__be_t40, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t0, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(irc__su_t5, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 5, 0.47).
narrative_ontology:measurement(irc__su_t10, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 10, 0.49).
narrative_ontology:measurement(irc__su_t15, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(irc__su_t20, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(irc__su_t25, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement(irc__su_t30, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(irc__su_t40, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strategic_shelter_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(irc_469_material_participation_kernel__strategic_shelter_reading, 0.18).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel__strict_gatekeeper_reading).

% DUAL FORMULATION NOTE:
% The IRC §469 material participation requirement is a contested kernel decomposed into two constraint stories: the strategic-shelter reading (this constraint) and the strict-gatekeeper reading (sibling constraint). These are not the same constraint viewed from different seats—they instantiate different ε values, different beneficiary/victim structures, and different classifications because they rest on fundamentally different interpretations of whether §469 material participation is a permissive or restrictive gate. The strategic reading treats the requirement as a coordination mechanism with reasonable interpretive flexibility; the strict reading treats it as a gatekeeper with heightened documentation and participation burdens. Their ε values diverge because extractiveness hinges on whether the same hour-counting and grouping election practices are legitimate coordination optimization (low ε) or disguised rent-seeking (high ε). The stories are linked because policy reform targeting one reading would cascade to the other—legislative clarification of material participation intent, audit guidance tightening documentation standards, or judicial precedent constraining grouping elections would force both readings toward convergence or eliminate one. The network edge represents this structural coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
