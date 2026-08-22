% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__substantial_effects_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__substantial_effects_limited_reading, []).

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
 *   constraint_id: commerce_clause_text__substantial_effects_limited_reading
 *   human_readable: Substantial Effects Doctrine (Economic/Non-Economic Category Boundary)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint is the substantial-effects-with-nexus reading of the
 *   Commerce Clause kernel: Congress may regulate intrastate activity that
 *   substantially affects interstate commerce, but only where the regulated
 *   conduct is genuinely economic and the statute shows a jurisdictional
 *   nexus (rather than using the Commerce Clause as a vehicle for what is
 *   actually police-power regulation). This is the Lopez/Morrison line as it
 *   operates against Wickard-era aggregation: neither unlimited federal reach
 *   nor a return to a narrow trade-crossing-borders test, but a
 *   category-policing exercise that draws and redraws an
 *   economic/non-economic boundary case by case. The doctrine functions as
 *   tangled rope: it genuinely coordinates a national economy that requires
 *   uniform regulation (rope function) while its category-boundary mechanism
 *   systematically denies remedy to non-economic-conduct victims regardless
 *   of the severity of the underlying harm (extraction function), and both
 *   functions ride the same jurisprudential machinery — courts must actively
 *   police the boundary in every contested case, which is the active
 *   enforcement this reading requires.
 *
 * KEY AGENTS:
 *   - federal_regulatory_agencies: institutional agenda_setter/beneficiary — drafts and enforces statutes reaching intrastate economic activity
 *   - national_market_participants: organized beneficiary — relies on uniform federal floor across state economic activity
 *   - gun_free_school_zone_defendants: powerless payer — bears federal criminal exposure when courts miscategorize non-economic conduct as economic
 *   - gender_violence_civil_plaintiffs: powerless payer — denied federal civil remedy because the underlying conduct is classified non-economic
 *   - local_noneconomic_conduct_regulators: moderate payer/beneficiary — states whose police-power domain is preserved in principle but destabilized by recategorization
 *   - federal_courts: institutional agenda_setter — the actual mechanism enforcing (or eroding) the nexus requirement case by case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, 0.42).
domain_priors:suppression_score(commerce_clause_text__substantial_effects_limited_reading, 0.38).
domain_priors:theater_ratio(commerce_clause_text__substantial_effects_limited_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__substantial_effects_limited_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__substantial_effects_limited_reading, "Substantial Effects Doctrine (Economic/Non-Economic Category Boundary)").
narrative_ontology:topic_domain(commerce_clause_text__substantial_effects_limited_reading, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__substantial_effects_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__substantial_effects_limited_reading, '292cba7e-9d04-4fa7-a3c5-6cce7e1f1837').
narrative_ontology:cs_kernel_codification('292cba7e-9d04-4fa7-a3c5-6cce7e1f1837', fixed_text).
narrative_ontology:cs_authority_grounding('292cba7e-9d04-4fa7-a3c5-6cce7e1f1837', lineage).
narrative_ontology:cs_interpretation_layer_present('292cba7e-9d04-4fa7-a3c5-6cce7e1f1837').
narrative_ontology:cs_reading_relation('292cba7e-9d04-4fa7-a3c5-6cce7e1f1837', commerce_clause_text__expansive_federal_reading, forecloses).
narrative_ontology:cs_reading_relation('292cba7e-9d04-4fa7-a3c5-6cce7e1f1837', commerce_clause_text__originalist_narrow_reading, forecloses).
narrative_ontology:cs_axiom('292cba7e-9d04-4fa7-a3c5-6cce7e1f1837', foundational, economic_character_required_for_federal_nexus).
narrative_ontology:cs_axiom_status(economic_character_required_for_federal_nexus, holdable).
narrative_ontology:cs_axiom_grounding('292cba7e-9d04-4fa7-a3c5-6cce7e1f1837', economic_character_required_for_federal_nexus, conventional).
narrative_ontology:cs_axiom('292cba7e-9d04-4fa7-a3c5-6cce7e1f1837', foundational, aggregation_permitted_only_for_genuinely_economic_activity).
narrative_ontology:cs_axiom_status(aggregation_permitted_only_for_genuinely_economic_activity, holdable).
narrative_ontology:cs_axiom_grounding('292cba7e-9d04-4fa7-a3c5-6cce7e1f1837', aggregation_permitted_only_for_genuinely_economic_activity, conventional).
narrative_ontology:cs_axiom('292cba7e-9d04-4fa7-a3c5-6cce7e1f1837', secondary, federalism_structure_imposes_judicially_enforceable_outer_limit).
narrative_ontology:cs_axiom_status(federalism_structure_imposes_judicially_enforceable_outer_limit, holdable).
narrative_ontology:cs_axiom_grounding('292cba7e-9d04-4fa7-a3c5-6cce7e1f1837', federalism_structure_imposes_judicially_enforceable_outer_limit, deontological).
narrative_ontology:cs_reference_frame('292cba7e-9d04-4fa7-a3c5-6cce7e1f1837', post_lopez_morrison_equilibrium).
narrative_ontology:cs_drift_state('292cba7e-9d04-4fa7-a3c5-6cce7e1f1837', contemporary_administrative_state_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('292cba7e-9d04-4fa7-a3c5-6cce7e1f1837', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, federal_regulatory_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, national_market_participants).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_categorization_lawyers).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, gun_free_school_zone_defendants).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, gender_violence_civil_plaintiffs).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, local_noneconomic_conduct_regulators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, local_noneconomic_conduct_regulators).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, economic_noneconomic_distinction_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, aggregation_principle_for_economic_activity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce regulations reaching intrastate economic activity by characterizing it as substantially affecting interstate commerce and aggregating it with similar conduct nationwide. Litigate to defend the economic characterization whenever a regulated party challenges jurisdiction, and shape statutory findings language to survive the economic/non-economic line.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_regulatory_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__substantial_effects_limited_reading, federal_regulatory_agencies, beneficiary).

% Operate across state lines and benefit from a uniform federal regulatory floor that prevents a race-to-the-bottom among states on issues like wages, agriculture, and lending. Their compliance costs are the same regardless of the doctrinal line's location, so the substantial-effects test's stability serves their planning.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, national_market_participants, beneficiary,
    organized, biographical, mobile, national).

% Prosecuted under federal statutes that attempt to reach conduct (gun possession near schools) with no genuine economic nexus, relying on strained aggregation theories. Under this reading they should be shielded because the conduct is non-economic, but categorization is litigated case by case and a wrong court characterization exposes them to federal liability they had no way to predict.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, gun_free_school_zone_defendants, payer,
    powerless, immediate, trapped, local).

% Sought a federal civil remedy for gender-motivated violence on the theory that such violence has aggregate economic effects (deterring women from commerce and employment). Under this reading their claim is barred because the underlying conduct is non-economic, leaving them dependent entirely on state tort and criminal remedies regardless of enforcement quality in their state.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, gender_violence_civil_plaintiffs, payer,
    powerless, biographical, trapped, national).

% States and municipalities that regulate family law, local crime, and land use expect this reading to preserve their police-power domain from federal displacement. They bear the cost of constant relitigation over which side of the economic line their regulatory subject falls on, and lose predictability whenever Congress or an agency recharacterizes a subject as economic to reach it.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, local_noneconomic_conduct_regulators, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__substantial_effects_limited_reading, local_noneconomic_conduct_regulators, beneficiary).

% Litigators and scholars whose practice depends on the economic/non-economic distinction remaining contestable and fact-intensive; each new case generates billable litigation and academic output over where a given statute falls on the line.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_categorization_lawyers, beneficiary,
    organized, biographical, mobile, national).

% Adjudicate the jurisdictional nexus requirement, deciding case by case whether a statute's findings and structure show genuine economic regulation or pretextual reach into police-power territory. Their categorization decisions are the actual mechanism by which the reading is enforced or eroded.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__substantial_effects_limited_reading, diffuse).
narrative_ontology:fixing_cost_class(commerce_clause_text__substantial_effects_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a functioning federal system by giving Congress reliable authority over genuinely national economic problems (wages, agricultural markets, lending, environmental spillovers) while reserving traditionally local police-power subjects (family law, most violent crime, education) to the states, preventing both federal paralysis and unchecked federal displacement of state governance.
% TRANSFER_FUNCTION: Moves regulatory jurisdiction itself: economic activity with a demonstrable interstate nexus is allocated to federal control (and its beneficiaries — uniform-market participants, federal agencies); non-economic local conduct is allocated to state control, denying federal remedy to plaintiffs harmed by conduct a court characterizes as non-economic regardless of its actual aggregate social cost.
% ABSENT_VOICES: Plaintiffs seeking federal civil remedies for harms embedded in non-economic conduct (gender-motivated violence, some civil rights harms) are effectively unrepresented in the doctrinal line-drawing — courts weigh statutory text and formal economic character, not the lived consequence of remedy denial, and no party formally represents 'harm severity independent of economic characterization' in the test itself.
% DISAPPEARANCE_RATIONALE: If the substantial-effects-with-nexus-requirement reading vanished, federal jurisdiction would either collapse toward the narrow originalist reading (stripping federal agencies of authority over much of modern economic regulation: labor, environment, lending) or expand toward the unlimited aggregate-effects reading (eliminating meaningful limits on federal reach into traditionally local subjects). Either shift would immediately reallocate which government retains authority over enormous swaths of American economic and social life.
% FOUNDING_PROBLEM: The doctrine was built to solve the New Deal-era crisis of a national economy that individual states could not regulate effectively (interstate rail rates, agricultural overproduction, labor conditions in goods shipped nationally) while later reasserting an outer limit after decades of near-limitless deference threatened to erase any distinction between federal and state authority (United States v. Lopez, United States v. Morrison).
% FOUNDING_PROBLEM_CORROBORATION: Federal agencies and national market participants attest the underlying coordination problem (fragmented state regulation of a national economy) remains live. State governments, several sitting and former appellate judges writing outside pending litigation, and constitutional scholars unaffiliated with either federal agencies or plaintiff advocacy groups attest that the nexus/pretext limitation is honored inconsistently — the category line moves depending on the regulatory target rather than tracking a stable jurisprudential principle, suggesting the 'genuine limit' function is partly aspirational rather than operative.
narrative_ontology:disappearance_verdict(commerce_clause_text__substantial_effects_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__substantial_effects_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__substantial_effects_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_text__substantial_effects_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__substantial_effects_limited_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__substantial_effects_limited_reading_tests).
:- end_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) sits at a moderate level reflecting the doctrine's dual function: it is not a pure extraction mechanism (it does solve a genuine coordination problem for a national economy) but the economic/non-economic line reliably produces losers who cannot access federal remedy regardless of harm severity, which is a real transfer of legal protection away from certain victim classes. Suppression (0.38) reflects that the doctrine does not physically coerce compliance so much as foreclose an entire category of remedy through categorization — the suppression is doctrinal rather than coercive in the ordinary sense, but it is real: a plaintiff whose harm is characterized as non-economic has no federal avenue regardless of how compelling the case for federal interest. Theater ratio (0.44) is elevated because much of the litigation activity around the economic/non-economic line functions as boundary-policing performance — statutory findings clauses are drafted specifically to survive judicial review of the 'genuinely economic' requirement, which is itself a form of doctrinal theater responding to the test rather than to underlying policy need. Accessibility collapse (0.5) and resistance (0.6) reflect that alternatives (state remedy, statutory redrafting, constitutional amendment) remain theoretically available but are costly and slow, and the doctrine meets sustained resistance from both directions — those who think it under-includes (Morrison plaintiffs) and those who think it over-includes (school zone defendants).
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (federal agencies, federal courts) this reading looks like principled line-drawing that legitimately coordinates federal-state authority. From the powerless payer seats, the same mechanism looks like an arbitrary sorting device whose outcome depends on how well a statute's findings clause was drafted rather than on the substance of the harm — the engine's per-seat computation is expected to diverge sharply here because the structural inputs (trapped exit, powerless power atom, no gain capture) point toward extraction from that seat even though the doctrine's stated purpose is coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulatory agencies and national market participants sit near the beneficiary end: they collect the predictability and reach the doctrine provides without bearing the categorization risk directly. The powerless payer classes (school-zone defendants, gender-violence plaintiffs) sit near the full-target end: they are trapped by the outcome of a categorization exercise they do not control and cannot exit — the harm has already occurred by the time the jurisdictional question is litigated. Local regulators occupy a hybrid position: they benefit from the doctrine's preservation of police power in principle but pay the cost of doctrinal instability whenever a subject gets recharacterized, which is why they carry both roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a fragmented national economy that individual states could not regulate) remains partly live for market-participant beneficiaries, which prevents this reading from being labeled a pure zombie mandate. But the category-boundary mechanism itself has arguably outlived any function beyond litigation-generation: the same statutory text can be recharacterized as economic or non-economic depending on drafting technique, which is a strong signal that the boundary is being administered for its own continuation (feeding categorization-lawyer practice and doctrinal theater) rather than tracking a stable underlying principle. The founding_problem_status is authored as contested precisely to surface this: the coordination function is real for one beneficiary class while the limiting function is increasingly performative for the victim classes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_noneconomic_line_stability,
    'Is the economic/non-economic distinction a stable, principled jurisprudential category, or is it a post-hoc rationalization that tracks the outcome courts want to reach for other reasons (federalism sympathies, subject-matter salience)?',
    'Systematic coding of appellate Commerce Clause decisions against statutory findings language, judicial ideology measures, and case outcome to test whether the economic/non-economic classification predicts outcome independent of political and doctrinal priors.',
    'If the line is unstable and outcome-driven, this reading functions more as snare (categorization theater masking a discretionary judicial power grab) than tangled rope (genuine hybrid coordination/extraction); if stable, the coordination function is more robust than the extraction critique suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_noneconomic_line_stability, empirical, 'Whether the economic/non-economic boundary tracks a genuine principle or outcome-driven discretion.').

omega_variable(
    committer_kernel_disagreement_location,
    'This constraint is one reading of the commerce_clause_text kernel; the disagreement with expansive_federal_reading and originalist_narrow_reading is located specifically in where the outer limit of ''substantial effects'' sits and whether a jurisdictional-nexus/economic-character requirement is textually or structurally compelled at all.',
    'Comparative doctrinal history across the three readings, tracking how each reading''s adherents would resolve identical fact patterns (e.g., a federal statute regulating intrastate firearm possession, or intrastate medical marijuana cultivation) to isolate exactly where the readings diverge in application rather than rhetoric.',
    'Clarifies that this reading is not a compromise or average of the sibling readings but a structurally distinct test with its own beneficiary/victim allocation; a sibling reading adopted by a future court majority would reallocate jurisdiction wholesale rather than merely shift a metric.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_disagreement_location, conceptual, 'Locates the structural point of disagreement between this reading and its two sibling readings within the shared kernel.').

omega_variable(
    pretext_detection_reliability,
    'Can courts reliably distinguish pretextual invocation of the Commerce power (police-power regulation dressed as economic regulation) from genuine economic regulation with incidental non-economic effects?',
    'Track reversal rates and inter-circuit splits on nexus/pretext findings over time; a high and non-declining split rate would indicate the pretext test is not administrable with the precision the reading''s own legitimacy claims.',
    'Low reliability would suggest the reading''s central limiting mechanism is aspirational rather than operative, supporting reclassification toward snare for the powerless payer seats; high reliability would support the tangled_rope claim as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pretext_detection_reliability, empirical, 'Whether the pretext/nexus test is administrable in practice or merely rhetorically stable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__substantial_effects_limited_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1937, 0.2).
narrative_ontology:measurement(comm_tr_t1964, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1964, 0.25).
narrative_ontology:measurement(comm_tr_t1985, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1985, 0.32).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1995, 0.4).
narrative_ontology:measurement(comm_tr_t2000, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(comm_tr_t2012, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2012, 0.44).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2024, 0.44).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1937, 0.25).
narrative_ontology:measurement(comm_be_t1964, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1964, 0.3).
narrative_ontology:measurement(comm_be_t1985, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1985, 0.38).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1995, 0.4).
narrative_ontology:measurement(comm_be_t2000, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement(comm_be_t2012, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2012, 0.43).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1937, 0.2).
narrative_ontology:measurement(comm_su_t1964, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1964, 0.28).
narrative_ontology:measurement(comm_su_t1985, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1985, 0.3).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement(comm_su_t2000, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2000, 0.39).
narrative_ontology:measurement(comm_su_t2012, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2012, 0.38).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__substantial_effects_limited_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__substantial_effects_limited_reading, 0.12).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__originalist_narrow_reading).

% DUAL FORMULATION NOTE:
% This constraint is the middle reading of the commerce_clause_text kernel, positioned between commerce_clause_text__expansive_federal_reading (no meaningful outer limit on aggregate economic effects) and commerce_clause_text__originalist_narrow_reading (interstate commerce limited to actual cross-border trade and instrumentalities). Each reading is authored as a separate constraint with its own ε: this reading's ε (0.42, tangled_rope) is deliberately non-averaged relative to the siblings — it reflects this reading's own hybrid coordination/extraction structure, not an interpolation between the other two readings' extraction values. A future doctrinal shift toward either sibling reading would reallocate the entire beneficiary/victim structure authored here, not merely adjust a metric.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
