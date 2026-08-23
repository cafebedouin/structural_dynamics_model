% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__regulatory_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: takings_clause_boundary__regulatory_takings_reading
 *   human_readable: Regulatory Takings Doctrine ('Goes Too Far') Compensation Requirement
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   Since 1922, American constitutional law has treated regulation that goes
 *   'too far' in diminishing property value as the equivalent of a taking
 *   requiring just compensation. The rule is administered case by case:
 *   courts weigh the economic impact on the owner, the owner's
 *   investment-backed expectations, and the character of the government
 *   action, and order public payment when the balance tips. The arrangement
 *   solves a real problem — it stops governments from accomplishing
 *   confiscation through regulation and concentrates the costs of public
 *   programs on unlucky individual owners — while simultaneously generating
 *   large litigation volumes from its indeterminacy, skewing access toward
 *   well-capitalized claimants, and deterring public regulatory programs
 *   through liability anticipation. The claimed type (tangled_rope) is stated
 *   from the authoring seat as what is structurally true; the metrics are
 *   authored independently as what is descriptively true of the doctrine's
 *   operation, including its rising extraction and theatricality over the
 *   interval.
 *
 * KEY AGENTS:
 *   - - resourced_landowners_developers: Primary beneficiary (powerful/constrained) — receives compensation, settlements, and regulatory concessions; land immobile but capital mobile
 *   - - takings_litigation_bar: Secondary beneficiary (organized/arbitrage) — collects fees from the volume and unpredictability the case-by-case inquiry produces
 *   - - small_parcel_owners: Nominal protectee, practical absentee (moderate/trapped) — covered on paper, priced out of the litigation the doctrine demands
 *   - - municipal_and_state_governments: Payer and co-agenda-setter (institutional/constrained) — enacts the regulated rules and pays the resulting judgments
 *   - - general_taxpayers: Diffuse payer (moderate/mobile) — funds compensation invisibly through public budgets
 *   - - public_regulatory_programs: Payer via deterrence (institutional/generational) — narrows mandates to avoid liability
 *   - - federal_judiciary: Agenda-setter (institutional/analytical) — defines the boundary, collects nothing, pays nothing
 *   - - legal_academia: Analytical observer (analytical/analytical) — maps the doctrine's incoherence without deciding anything
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, 0.38).
domain_priors:suppression_score(takings_clause_boundary__regulatory_takings_reading, 0.4).
domain_priors:theater_ratio(takings_clause_boundary__regulatory_takings_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__regulatory_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__regulatory_takings_reading, "Regulatory Takings Doctrine ('Goes Too Far') Compensation Requirement").
narrative_ontology:topic_domain(takings_clause_boundary__regulatory_takings_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__regulatory_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__regulatory_takings_reading, 'ca7f4ec4-7453-4edd-a549-aa465bfa4437').
narrative_ontology:cs_kernel_codification('ca7f4ec4-7453-4edd-a549-aa465bfa4437', fixed_text).
narrative_ontology:cs_authority_grounding('ca7f4ec4-7453-4edd-a549-aa465bfa4437', lineage).
narrative_ontology:cs_interpretation_layer_present('ca7f4ec4-7453-4edd-a549-aa465bfa4437').
narrative_ontology:cs_reading_relation('ca7f4ec4-7453-4edd-a549-aa465bfa4437', takings_clause_boundary__physical_appropriation_reading, forecloses).
narrative_ontology:cs_reading_relation('ca7f4ec4-7453-4edd-a549-aa465bfa4437', takings_clause_boundary__categorical_takings_reading, influences).
narrative_ontology:cs_axiom('ca7f4ec4-7453-4edd-a549-aa465bfa4437', foundational, severe_economic_diminution_is_appropriation).
narrative_ontology:cs_axiom_status(severe_economic_diminution_is_appropriation, holdable).
narrative_ontology:cs_axiom_grounding('ca7f4ec4-7453-4edd-a549-aa465bfa4437', severe_economic_diminution_is_appropriation, deontological).
narrative_ontology:cs_axiom('ca7f4ec4-7453-4edd-a549-aa465bfa4437', foundational, ad_hoc_factor_balancing_required).
narrative_ontology:cs_axiom_status(ad_hoc_factor_balancing_required, holdable).
narrative_ontology:cs_axiom_grounding('ca7f4ec4-7453-4edd-a549-aa465bfa4437', ad_hoc_factor_balancing_required, empirically_contingent).
narrative_ontology:cs_reference_frame('ca7f4ec4-7453-4edd-a549-aa465bfa4437', mahon_penn_central_balancing_framework).
narrative_ontology:cs_drift_state('ca7f4ec4-7453-4edd-a549-aa465bfa4437', contemporary_post_lucas_lingle_murr_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ca7f4ec4-7453-4edd-a549-aa465bfa4437', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, resourced_landowners_developers).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, takings_litigation_bar).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, municipal_and_state_governments).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, general_taxpayers).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, public_regulatory_programs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own or option large parcels whose highest-value uses are blocked by zoning, environmental, or preservation limits. When a regulation threatens a development plan, they can fund the surveys, appraisals, and multi-year appeals the case-by-case inquiry requires, and can credibly threaten litigation that extracts settlements, permit concessions, or compensation from public treasuries. Their land cannot move, but their capital and project pipelines can be redirected toward friendlier jurisdictions, which shapes how hard they press each claim and where.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, resourced_landowners_developers, beneficiary,
    powerful, biographical, constrained, national).

% Hold homes or small lots subject to the same regulatory limits. On paper the compensation rule covers them identically; in practice the appraisal-and-appeal path costs more than the disputed difference in value, so they absorb the loss or sell. Their equity is tied to property they cannot relocate, and they lack the resources to make the doctrine's promise real in their own cases.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, small_parcel_owners, excluded,
    moderate, biographical, trapped, local).

% Specialist firms and property-rights practices whose revenue tracks the volume and unpredictability of compensation disputes. Each doctrinal refinement generates re-litigation of old theories; they select clients, shop among circuits, and can move to whichever jurisdiction offers the richest claim supply, bearing little downside from the instability they monetize.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, takings_litigation_bar, beneficiary,
    organized, immediate, arbitrage, national).

% Enact the zoning codes, environmental rules, and preservation ordinances whose burdens the doctrine prices. They draft around compensation risk, settle weak-but-expensive claims, and pay judgments from budgets committed elsewhere. They cannot exit the constitutional rule and can reshape it only slowly, through litigation positions and amicus strategy across decades.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, municipal_and_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__regulatory_takings_reading, municipal_and_state_governments, agenda_setter).

% Fund compensation awards and settlements through taxes, usually without knowing any specific claim exists. The cost per household is small and diffuse; the benefit of any particular regulation preserved or lost is equally diffuse. Households can relocate between jurisdictions, which disciplines local fiscal choices but leaves the federal constitutional floor untouched.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, general_taxpayers, payer,
    moderate, generational, mobile, national).

% Environmental, conservation, and historic-preservation agencies whose rules carry compensation exposure. Anticipating liability, they narrow or abandon measures their statutes direct them to pursue — wetland buffers, view corridors, density caps — trading program effectiveness for budget safety. Their mandates are fixed by legislation; their usable toolset shrinks with each adverse precedent.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, public_regulatory_programs, payer,
    institutional, generational, constrained, national).

% Supreme Court and circuit judges who define what counts as going too far, which inquiries are categorical, and which factors control. They collect no fees and pay no judgments; their stake is doctrinal coherence and institutional authority. Each term's takings docket lets them adjust the boundary without electoral accountability, and the balancing method is their profession's own century-long construction.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Property-law and constitutional scholars who map the doctrine's incoherence, propose replacement frameworks, and supply the critiques that both defenders and opponents cite. They shape the long-run framing of the dispute without deciding any case or bearing any of its costs.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, legal_academia, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__regulatory_takings_reading, resourced_landowners_developers).
narrative_ontology:fixing_cost_class(takings_clause_boundary__regulatory_takings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines a judicially administered boundary between permissible regulation and compensable expropriation, allowing governments to pursue public aims through police-power regulation while guaranteeing owners that the costs of public programs will not be silently concentrated on them; channels owner-government disputes into a common adjudicative forum instead of open political conflict over land use.
% TRANSFER_FUNCTION: Moves compensation dollars from public treasuries (taxpayers at large) to owners who prove severe value diminution; moves settlement leverage and delay rights to well-resourced landholders; moves regulatory decision-making discretion from elected bodies to courts; moves fee income to the specialist litigation bar.
% ABSENT_VOICES: Renters, neighboring communities, and future residents affected by land-use decisions have no standing anywhere in the process — compensation claims belong to owners, so the public interest appears only defensively, through government counsel. Small owners are nominally represented by the doctrine but effectively absent from it: the multi-year, expert-heavy litigation the balancing inquiry demands prices them out, so the claimant voice actually heard in court belongs almost exclusively to well-capitalized parties.
% DISAPPEARANCE_RATIONALE: If the rule vanished overnight, governments could regulate without compensation liability; land values would reprice immediately as development-limiting rules became costless to impose; pending claims and the litigation market built on them would collapse; lenders' underwriting models, which price regulatory risk against recovery prospects, would be rewritten; and agencies would rediscover regulatory tools currently shelved for liability reasons. The arrangement's removal reorganizes property markets, public budgets, and regulatory practice simultaneously.
% FOUNDING_PROBLEM: Governments had discovered that regulation could accomplish what outright confiscation legally could not: impose public-benefit costs on specific owners without payment. The founding problem was preventing appropriation-by-regulation — ensuring that when public action destroys private value, the burden falls on the public that benefits rather than on the unlucky individual owner (the equitable-apportionment problem later given its canonical statement in the Armstrong case).
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested from outside the current claimant class: state constitutions with parallel compensation clauses are applied to the same problem by their own courts; legal historians document the confiscatory-taxation and nuisance-escape practices that motivated early recognition; and the Supreme Court's own articulation of the apportionment rationale predates the modern claimant industry and serves no present beneficiary's interest. Corroboration is broad and does not depend on the parties who now collect under the rule.
narrative_ontology:disappearance_verdict(takings_clause_boundary__regulatory_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__regulatory_takings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__regulatory_takings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(takings_clause_boundary__regulatory_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__regulatory_takings_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__regulatory_takings_reading_tests).
:- end_tests(takings_clause_boundary__regulatory_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.38 from this reading's own lights: the doctrine genuinely shields owners from non-physical confiscation (which lowers extraction from the protected class), but its operation transfers real value — public funds to concentrated claimants, settlement leverage to deep pockets, fee income to specialists, and regulatory capacity from agencies to courts. Suppression is 0.40 and is authored as a raw structural property, unscaled by power or scope: the constitutional floor cannot be legislated around, though competing interpretive positions survive openly in scholarship and state courts, which keeps suppression below the levels seen in arrangements that must silence exits. Theater_ratio reaches 0.50 because the governing three-factor test is widely applied in conclusory fashion — opinions frequently announce factor-weighing while arriving at outcome-driven results — so roughly half the doctrine's visible activity is performative rather than decision-producing. Accessibility_collapse is 0.45: governments retain partial exits (drafting regulations around liability, using transferable development rights, paying legislatively scheduled compensation), and owners retain non-judicial channels, so understanding the constraint does not close the option space. Resistance is 0.60: the doctrine has faced a century of sustained criticism from legal academia, periodic legislative hostility, intra-court disagreement, and repeated proposals to replace balancing with categorical rules. The temporal series run on one shared grid (seven points, 1922-2026) and show extraction accumulation and rising theatricality concentrated after 1978, when the balancing framework was institutionalized and the claim-supply industry matured.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute fundamentally different constraints from the same doctrine. From the resourced-claimant seat, the arrangement is indispensable property security — the only barrier between investment-backed expectations and majoritarian confiscation. From the government and taxpayer seats, the same structure operates as an open-ended liability that transfers budget authority to courts and rewards whoever can litigate longest. From the judiciary's seat, it is an imperfect but administrable method that preserves flexibility no categorical rule could match. The small-parcel seat experiences a fourth thing: a promise that is real in doctrine and unreachable in practice, since the multi-year, expert-intensive inquiry costs more than most small claims are worth. The judiciary additionally carries a mild institutional identity fusion — the balancing method is the profession's own creation, and abandoning it would read as repudiating a century of craft, which raises the internal cost of categorical reform independent of any external pressure. Taxpayer resistance is structurally weak despite aggregate stakes because the per-household cost is invisible and the class is diffuse; no coalition forms around a cost nobody can see.
 *
 * DIRECTIONALITY LOGIC:
 *   Resourced landowners and developers are declared beneficiaries and sit near the beneficiary end of directionality: the doctrine subsidizes them with compensation and leverage, and their constrained-but-real capital mobility moderates how fully they are bound to any single jurisdiction's rules. The litigation bar is a declared beneficiary with arbitrage-grade exit — it selects forums and clients and bears no downside from doctrinal churn, since churn is its revenue. Municipal and state governments are declared victims with high directionality toward the constraint: they pay judgments, absorb drafting costs, and cannot exit the constitutional floor; their secondary agenda-setting role (they write the regulations that trigger the doctrine) makes them dual-positioned rather than purely targeted. General taxpayers are victims with diffuse, mobile incidence — individually negligible costs, collectively substantial, with relocation as the only discipline valve. Public regulatory programs are victims bearing extraction in the currency of foregone mandate rather than money. The federal judiciary administers the constraint without collecting from it, placing it near the middle-low range; its stake is doctrinal coherence, not rent.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Reading the doctrine as pure coordination (as its defenders do) erases the measurable extraction: the regressive access skew, the indeterminacy rents, and the regulatory chill that transfers program capacity from publics to courts. Reading it as pure extraction (as its critics on the governance side do) erases the genuine coordination function: burden-spreading across the tax base instead of onto individual owners, investment security that underwrites credit markets, and a common adjudicative forum replacing political retaliation cycles. The founding problem — stopping regulation from becoming confiscation-by-other-means — remains live: severe regulation continues and the question of who bears its costs is unresolved, so the arrangement has not outlived its mandate and no zombie flag is expected; the founding-problem status (live) paired with the disappearance verdict (world_rearranges) is internally consistent rather than mismatched.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (regulatory_takings_reading) of the takings_clause_boundary kernel; what structurally changes if a sibling reading is adopted instead?',
    'Adoption of the physical-appropriation sibling would shrink the compensated-victim set to owners of physically occupied property and dissolve the balancing apparatus entirely; adoption of the categorical sibling would retain the balancing framework but confine per se treatment to total economic elimination, shrinking the effective victim set to near-total-loss claimants.',
    'Victim-set size, litigation volume, and regulatory-chill magnitude all swing with the boundary placement; the same constitutional text yields constraints with materially different epsilon and different payer populations depending on which reading governs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: which reading of the takings boundary governs determines the constraint''s victim set and enforcement surface.').

omega_variable(
    too_far_threshold_location,
    'Where does ''too far'' sit — what magnitude of economic diminution converts a lawful regulation into a compensable taking?',
    'Doctrinal evolution (a workable judicial threshold or legislatively enacted compensation schedule) or systematic study of decided claims mapping diminution percentages against outcomes.',
    'A low threshold expands the victim set dramatically and raises extraction from public treasuries; a high threshold collapses the doctrine toward the categorical sibling''s total-elimination line, leaving intermediate losses uncompensated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(too_far_threshold_location, conceptual, 'The doctrine''s central quantity is undefined except at the extremes.').

omega_variable(
    regulatory_chill_magnitude,
    'How much otherwise-valid regulation is deterred, narrowed, or abandoned because agencies anticipate compensation liability?',
    'Comparative study of regulatory output across jurisdictions with strong versus weak compensation exposure, controlling for political variables; agency self-reports of rules modified or dropped after takings review.',
    'If chill is large, the doctrine''s principal extraction falls on public regulatory capacity and the payer-side reading dominates; if small, the burden-spreading coordination function dominates and the arrangement sits closer to pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_chill_magnitude, empirical, 'Size of the off-books transfer of regulatory capacity extracted by liability anticipation.').

omega_variable(
    distributive_access_skew,
    'Does the doctrine''s protection in practice flow disproportionately to well-resourced claimants, leaving small owners formally covered but practically unable to invoke it?',
    'Claimant demographics and outcome data across reported decisions; comparison of median claim cost against median disputed value for small parcels.',
    'If access is strongly skewed, the extraction asymmetry deepens — public funds compensate a narrow class while the nominally protected majority bears uncompensated diminution — increasing pressure toward extraction-dominated classification at the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributive_access_skew, empirical, 'Whether the compensation promise is realizable for the full class it nominally covers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__regulatory_takings_reading, 1922, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1922, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1922, 0.08).
narrative_ontology:measurement(taki_tr_t1958, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1958, 0.12).
narrative_ontology:measurement(taki_tr_t1978, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1978, 0.35).
narrative_ontology:measurement(taki_tr_t1992, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1992, 0.45).
narrative_ontology:measurement(taki_tr_t2005, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 2005, 0.5).
narrative_ontology:measurement(taki_tr_t2017, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 2017, 0.54).
narrative_ontology:measurement(taki_tr_t2026, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 2026, 0.55).

% Extraction over time
narrative_ontology:measurement(taki_be_t1922, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1922, 0.12).
narrative_ontology:measurement(taki_be_t1958, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1958, 0.18).
narrative_ontology:measurement(taki_be_t1978, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1978, 0.42).
narrative_ontology:measurement(taki_be_t1992, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1992, 0.55).
narrative_ontology:measurement(taki_be_t2005, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(taki_be_t2017, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2017, 0.6).
narrative_ontology:measurement(taki_be_t2026, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1922, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1922, 0.06).
narrative_ontology:measurement(taki_su_t1958, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1958, 0.1).
narrative_ontology:measurement(taki_su_t1978, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1978, 0.3).
narrative_ontology:measurement(taki_su_t1992, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1992, 0.4).
narrative_ontology:measurement(taki_su_t2005, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2005, 0.44).
narrative_ontology:measurement(taki_su_t2017, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2017, 0.47).
narrative_ontology:measurement(taki_su_t2026, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2026, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__regulatory_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary__categorical_takings_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'scope of the Takings Clause' decomposes into three structurally distinct constraints sharing one kernel text. The physical-appropriation reading (smallest victim set, no balancing apparatus), this regulatory reading (expanded victim set including severe value diminution without possession, ad hoc balancing), and the categorical reading (hybrid: per se rules at the extremes, balancing between) have different epsilon values, different payer populations, and different failure modes, and are therefore modeled as separate stories linked by network edges rather than one story with a measurement parameter. This story links to both siblings; the upstream-downstream causal structure runs from this reading's institutionalized balancing framework to the categorical sibling's emergence as a corrective response.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
