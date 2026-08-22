% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__regulatory_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__regulatory_takings_reading, []).

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
 *   constraint_id: takings_clause_boundary__regulatory_takings_reading
 *   human_readable: Regulatory Takings Doctrine (Penn Central / Diminution-in-Value Reading)
 *   domain: constitutional_law/property_rights
 *
 * SUMMARY:
 *   This constraint instantiates the regulatory-takings reading of the Fifth
 *   Amendment Takings Clause boundary: the position, traceable to
 *   Pennsylvania Coal v. Mahon and doctrinally structured by Penn Central
 *   Transportation Co. v. New York City (1978), that a regulation which goes
 *   'too far' in diminishing a property's economic value can itself
 *   constitute a taking requiring just compensation, even absent physical
 *   occupation or seizure. This reading expands the victim set relative to
 *   the physical-appropriation reading (which would deny recovery here
 *   entirely) by recognizing severe value diminution as compensable, but it
 *   does so through an admittedly ad hoc, multi-factor balancing test
 *   (economic impact, interference with investment-backed expectations,
 *   character of government action) that the Court has repeatedly declined to
 *   reduce to a bright-line rule. The result is a genuine coordination
 *   function (preventing regulation from achieving de facto confiscation
 *   without payment) bundled with asymmetric extraction: the test's
 *   indeterminacy systematically favors owners with litigation capital and
 *   creates a standing structural cost — regulatory chilling and
 *   litigation-defense burden — borne by regulating governments and, more
 *   diffusely, by the publics those regulations were meant to protect.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, 0.58).
domain_priors:suppression_score(takings_clause_boundary__regulatory_takings_reading, 0.42).
domain_priors:theater_ratio(takings_clause_boundary__regulatory_takings_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__regulatory_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__regulatory_takings_reading, "Regulatory Takings Doctrine (Penn Central / Diminution-in-Value Reading)").
narrative_ontology:topic_domain(takings_clause_boundary__regulatory_takings_reading, "constitutional_law/property_rights").

domain_priors:requires_active_enforcement(takings_clause_boundary__regulatory_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__regulatory_takings_reading, '17d0bc3a-e606-4297-8c68-3a4e7286d1e4').
narrative_ontology:cs_kernel_codification('17d0bc3a-e606-4297-8c68-3a4e7286d1e4', distributed).
narrative_ontology:cs_authority_grounding('17d0bc3a-e606-4297-8c68-3a4e7286d1e4', lineage).
narrative_ontology:cs_interpretation_layer_present('17d0bc3a-e606-4297-8c68-3a4e7286d1e4').
narrative_ontology:cs_reading_relation('17d0bc3a-e606-4297-8c68-3a4e7286d1e4', takings_clause_boundary__physical_appropriation_reading, forecloses).
narrative_ontology:cs_reading_relation('17d0bc3a-e606-4297-8c68-3a4e7286d1e4', takings_clause_boundary__categorical_takings_reading, influences).
narrative_ontology:cs_axiom('17d0bc3a-e606-4297-8c68-3a4e7286d1e4', foundational, severe_value_diminution_without_possession_is_compensable).
narrative_ontology:cs_axiom_status(severe_value_diminution_without_possession_is_compensable, holdable).
narrative_ontology:cs_axiom_grounding('17d0bc3a-e606-4297-8c68-3a4e7286d1e4', severe_value_diminution_without_possession_is_compensable, conventional).
narrative_ontology:cs_axiom('17d0bc3a-e606-4297-8c68-3a4e7286d1e4', secondary, case_specific_balancing_superior_to_bright_line_categorization).
narrative_ontology:cs_axiom_status(case_specific_balancing_superior_to_bright_line_categorization, holdable).
narrative_ontology:cs_axiom_grounding('17d0bc3a-e606-4297-8c68-3a4e7286d1e4', case_specific_balancing_superior_to_bright_line_categorization, instrumental).
narrative_ontology:cs_reference_frame('17d0bc3a-e606-4297-8c68-3a4e7286d1e4', penn_central_ad_hoc_balancing_framework).
narrative_ontology:cs_drift_state('17d0bc3a-e606-4297-8c68-3a4e7286d1e4', contemporary_post_lingle_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('17d0bc3a-e606-4297-8c68-3a4e7286d1e4', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, property_owners_facing_severe_regulation).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, takings_litigation_bar).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, municipalities_and_regulatory_agencies).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, environmental_and_land_use_beneficiary_publics).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, under_resourced_property_owners_who_cannot_litigate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own land or investment-backed property interests subjected to a zoning, environmental, or land-use rule that eliminates most or all economic use. Can sue for compensation under the ad hoc Penn Central balancing test, but the test's indeterminacy means outcomes hinge heavily on litigation resources, expert valuation testimony, and which circuit hears the claim.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, property_owners_facing_severe_regulation, beneficiary,
    moderate, biographical, constrained, national).

% Specialist property-rights litigators and advocacy organizations (e.g. public-interest law firms) whose practice exists because the balancing test is open-ended and contestable. Each new regulation is a potential case; the doctrine's ambiguity is their raw material and their livelihood does not depend on resolving that ambiguity.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, takings_litigation_bar, beneficiary,
    organized, generational, arbitrage, national).

% Draft and enforce zoning, environmental, historic-preservation, and land-use regulations in the public interest. Under this reading, any regulation that goes 'too far' in diminishing value exposes the government to compensation liability or invalidation, so agencies must budget for litigation risk, hire valuation experts, and sometimes water down regulations pre-emptively rather than risk a takings judgment. They cannot exit the jurisdiction whose land they must regulate.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, municipalities_and_regulatory_agencies, payer,
    institutional, generational, trapped, regional).

% Residents, downstream water users, and future generations who benefit from the regulations that get chilled, weakened, or reversed under takings-liability pressure. They are not party to the litigation and often unaware that a wetlands protection or coastal setback rule was narrowed specifically to avoid a takings claim. Their interest is diffuse, unorganized, and structurally excluded from the compensation calculus.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, environmental_and_land_use_beneficiary_publics, payer,
    powerless, generational, trapped, regional).

% Small landowners who suffer comparable or greater proportional value diminution but lack the capital to fund the multi-year litigation and expert appraisal battle the ad hoc test requires. The doctrine formally protects them but functionally the protection is priced out of reach; they bear the same regulatory burden as large owners without a realistic remedy.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, under_resourced_property_owners_who_cannot_litigate, payer,
    powerless, biographical, trapped, local).

% Administer the ad hoc Penn Central balancing test (economic impact, interference with distinct investment-backed expectations, character of the government action) case by case. Could clarify or bright-line the standard but have repeatedly declined to, preserving discretionary case-by-case adjudication as the operative mode.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, supreme_court_and_lower_federal_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Study and critique the doctrine's indeterminacy, documenting how outcomes vary by circuit, judge, and litigant resources. Their scholarship shapes doctrinal development but does not itself resolve the ambiguity.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, legal_scholars_and_practitioners, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism, however indeterminate, for property owners to seek compensation when regulation crosses from generally-applicable public-welfare limitation into effective confiscation of economic value — preventing government from achieving physical-taking-equivalent outcomes through regulatory means alone, without paying for them.
% TRANSFER_FUNCTION: When successful, moves compensation from public treasuries to owners whose property value was severely diminished by regulation. Structurally, it also moves regulatory caution and chilling effect toward agencies (who must discount future regulations against litigation risk) and moves a large share of the protection's practical benefit toward owners who can afford the ad hoc test's litigation costs, away from those who cannot.
% ABSENT_VOICES: Diffuse publics who benefit from the regulations that get chilled or weakened (downstream water users, future residents, ecosystem beneficiaries) have no seat in a takings claim, which is adjudicated solely between the individual owner and the regulating government. Under-resourced owners who cannot afford the balancing-test litigation are nominally covered by the doctrine but practically absent from its benefits.
% DISAPPEARANCE_RATIONALE: If the regulatory-takings reading disappeared and only physical appropriation triggered compensation, municipalities and agencies would regulate land use far more aggressively without compensation exposure, some owners facing severe value diminution would have no federal remedy at all, and the specialist takings litigation practice built around Penn Central claims would substantially contract or redirect to categorical-taking arguments.
% FOUNDING_PROBLEM: Governments could, in principle, achieve the practical effect of confiscating property value through regulation alone — zoning a parcel to zero use, for instance — without ever physically occupying it, and without triggering the compensation the Takings Clause plainly requires for outright seizure. Pennsylvania Coal v. Mahon (1922) and later Penn Central (1978) were built to close that loophole: some regulations, if severe enough, function as takings in substance even without physical dispossession.
% FOUNDING_PROBLEM_CORROBORATION: Property-rights advocates and the takings bar attest the founding problem remains fully live — regulatory overreach diminishing value without compensation continues today. Municipal law associations, environmental law scholars, and several sitting federal judges (in concurrences and dissents criticizing Penn Central's indeterminacy) attest that the doctrine has drifted from remedying genuine confiscation into a generalized, unpredictable liability exposure for ordinary land-use regulation, chilling regulation the doctrine was never meant to reach. Independent doctrinal scholarship (e.g. critiques by administrative and environmental law academics outside the property-rights advocacy bar) corroborates the drift reading.
narrative_ontology:disappearance_verdict(takings_clause_boundary__regulatory_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__regulatory_takings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__regulatory_takings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(takings_clause_boundary__regulatory_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__regulatory_takings_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__regulatory_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__regulatory_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (moderate-high, rising over the interval) because the doctrine's practical operation increasingly functions as a discretionary liability exposure for ordinary land-use and environmental regulation, not merely a backstop against confiscatory regulation — the drift from Mahon's narrow diminution principle to Penn Central's open multi-factor balancing to later applications (e.g. Palazzolo, Lingle) has widened what counts as 'too far.' Suppression (0.42) reflects the doctrine's real but partial coercive force: it does not suppress alternatives outright, but its unpredictability functions as a standing threat that shapes regulatory drafting ex ante. Theater ratio (0.28) captures a moderate but growing performative element: courts recite the three Penn Central factors as if applying a determinate test while acknowledging in the same opinions that the standard is inherently ad hoc, and some jurisdictions cite the factors without meaningfully weighing them. Accessibility collapse is authored low-moderate (0.35): categorical and physical-appropriation alternatives remain fully available and often preferred by litigants and courts alike, so this reading has not foreclosed its siblings. Resistance is high (0.68): the doctrine faces continuous resistance both from property-rights advocates who find it too narrow (they want per se rules) and from municipalities and environmental scholars who find it too broad and unpredictable — resistance from both directions simultaneously.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a well-resourced property owner or the takings bar, this doctrine looks like a functioning, if imperfect, coordination mechanism protecting a genuine constitutional right. From the seat of a municipal planning department or an under-resourced small owner, the same structure computes as an unpredictable extraction risk or an inaccessible formal right respectively. The engine should register this divergence directly from the differing power/exit profiles of these stakeholder seats rather than from any single authored verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners facing severe regulatory diminution are the doctrine's structural beneficiaries when they can litigate — the balancing test exists to give them a remedy. The takings litigation bar benefits structurally and durably regardless of case outcomes, because indeterminacy itself is billable. Municipalities and regulatory agencies are targets: they bear compensation liability, litigation defense costs, and a chilling effect on regulatory ambition that constrains their institutional mission. Diffuse publics who benefit from the regulations at risk of being chilled are targets with no seat at the table — their loss is real but never priced into any individual takings claim. Under-resourced owners are nominal beneficiaries who function structurally as payers: they bear the same regulatory burden as capital-rich owners but cannot access the remedy the doctrine formally provides them, so the protection's benefit is unevenly distributed by litigation capacity rather than by severity of harm.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (regulation achieving confiscation-in-substance without physical seizure) remains genuinely live in the abstract — governments can and do impose regulations that eliminate essentially all economic use of specific parcels. But the mechanism built to solve that narrow problem has drifted into a generalized liability-exposure doctrine that chills ordinary land-use and environmental regulation far beyond the confiscation-equivalent cases it was designed for. This is not classified as pure extraction (a snare) because the core coordination function is real and independently corroborated by scholars outside the property-rights advocacy community; it is classified as tangled_rope because the same balancing-test structure that occasionally delivers the intended remedy also imposes a standing, asymmetric cost on regulating governments and diffuse publics who have no voice in individual takings litigation. The requires_active_enforcement flag reflects that courts must continuously administer and re-litigate the ad hoc factors case by case — the ambiguity is not incidental but the operative mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ad_hoc_test_vs_bright_line_tradeoff,
    'Does the indeterminacy of the Penn Central balancing test serve a genuine adjudicative function (allowing courts to distinguish confiscatory regulation from ordinary public-welfare limitation on a case-specific basis that a bright-line rule could not capture), or is the indeterminacy itself the extractive mechanism, generating litigation demand and unpredictable liability exposure independent of any adjudicative benefit?',
    'Comparative empirical study of outcome consistency and predictability across circuits and case types, alongside a comparison to jurisdictions or historical periods operating under more determinate takings standards, to see whether case-specific accuracy improves enough to justify the unpredictability cost.',
    'If the ad hoc structure produces materially better-calibrated outcomes than a bright-line alternative would, the extraction is closer to a genuine coordination cost; if outcome variance tracks litigant resources more than case merits, the indeterminacy is substantially extractive rent-generation dressed as doctrinal nuance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ad_hoc_test_vs_bright_line_tradeoff, empirical, 'Whether Penn Central''s indeterminacy is adjudicative necessity or extraction mechanism.').

omega_variable(
    chilling_effect_magnitude,
    'How much regulatory ambition is actually chilled by takings-liability exposure under this reading, versus regulations that would have been narrower or absent for independent political-economy reasons?',
    'Natural-experiment comparison of regulatory stringency in jurisdictions and time periods with differing takings-liability exposure (e.g. before and after major regulatory-takings decisions, or across state constitutional takings standards of varying strictness).',
    'A large demonstrated chilling effect would strengthen the victim classification for diffuse regulatory beneficiary publics; a small effect would suggest the doctrine''s transfer function is narrower than the compensation-liability numbers alone suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilling_effect_magnitude, empirical, 'Magnitude of the doctrine''s chilling effect on regulation.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that all three sibling readings of the takings_clause_boundary kernel are simultaneously cited by different courts and litigants depending on case posture, is the ''regulatory takings'' framing itself a stable doctrinal commitment, or is it one rhetorical register among several that courts deploy opportunistically depending on which framing favors the desired outcome in a given case?',
    'Content analysis of takings opinions to determine whether courts'' choice among the categorical, physical-appropriation, and ad hoc regulatory-takings framings correlates with case facts (suggesting genuine doctrinal selection) or with outcome-orientation (suggesting the framing is chosen to justify a predetermined result).',
    'If framing choice correlates with predetermined outcomes rather than case facts, this reading''s claimed coordination function is partly illusory and the tangled_rope classification should weight more heavily toward extraction; if framing tracks facts, the coordination function is more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether courts select among kernel readings principledly or outcome-strategically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__regulatory_takings_reading, 1922, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1922, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1922, 0.1).
narrative_ontology:measurement(taki_tr_t1978, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1978, 0.15).
narrative_ontology:measurement(taki_tr_t1992, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1992, 0.2).
narrative_ontology:measurement(taki_tr_t2005, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 2005, 0.24).
narrative_ontology:measurement(taki_tr_t2015, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 2015, 0.26).
narrative_ontology:measurement(taki_tr_t2024, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(taki_be_t1922, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1922, 0.32).
narrative_ontology:measurement(taki_be_t1978, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1978, 0.42).
narrative_ontology:measurement(taki_be_t1992, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1992, 0.5).
narrative_ontology:measurement(taki_be_t2005, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2005, 0.54).
narrative_ontology:measurement(taki_be_t2015, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2015, 0.56).
narrative_ontology:measurement(taki_be_t2024, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1922, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1922, 0.2).
narrative_ontology:measurement(taki_su_t1978, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1978, 0.28).
narrative_ontology:measurement(taki_su_t1992, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1992, 0.34).
narrative_ontology:measurement(taki_su_t2005, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement(taki_su_t2015, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(taki_su_t2024, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__regulatory_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, categorical_takings_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the takings_clause_boundary kernel. physical_appropriation_reading (narrowest: only physical seizure/occupation compensable) and categorical_takings_reading (intermediate: per se rules for total elimination/permanent occupation, Penn Central factors for everything else) are the sibling files. Each carries its own ε, victim set, and classification; this file's expanded victim set (owners suffering severe non-physical value diminution) and higher suppression/extraction reflect the ad hoc balancing test's broader but less determinate reach relative to the categorical reading, and its far broader reach relative to the physical-appropriation reading which would recognize none of this file's named victims as victims at all.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
