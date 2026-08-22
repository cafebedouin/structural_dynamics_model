% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__narrow_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__narrow_originalist, []).

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
 *   constraint_id: commerce_clause_scope__narrow_originalist
 *   human_readable: Commerce Clause — Narrow Originalist Reading (Trade-Crossing-Borders / Facilitation-Only)
 *   domain: constitutional/political/economic
 *
 * SUMMARY:
 *   This story instantiates the narrow-originalist reading of the Commerce
 *   Clause kernel: 'commerce among the several states' means trade that
 *   itself crosses a state line, and 'to regulate' means to make regular — to
 *   facilitate and standardize existing interstate trade, not to prohibit or
 *   comprehensively control economic activity. Under this reading, federal
 *   power is confined to removing state-erected barriers to interstate trade
 *   and establishing uniform commercial rules for transactions that cross
 *   borders; it does not reach production, labor conditions, environmental
 *   externalities, or civil rights violations that occur wholly within one
 *   state, even when those in-state activities have significant downstream
 *   effects on interstate markets. This is a single, ε-invariant reading
 *   among three siblings in the commerce_clause_scope kernel
 *   (broad_effects_test and intermediate_channels are separate constraint
 *   stories, not alternative measurements of this one). The referent for
 *   extractiveness here is the standing arrangement under contest as this
 *   reading's own adherents see it: a constitutional text they read as
 *   narrowly limiting federal power, currently overridden in practice by
 *   nearly a century of broader doctrine — so ε is authored low (this reading
 *   claims minimal extraction from state sovereignty) while the reading's OWN
 *   victims (civil rights claimants, workers, cross-border pollution victims)
 *   are authored honestly as victims of the reading's adoption, not of some
 *   rival reading's endorsed alternative.
 *
 * KEY AGENTS:
 *   - state_governments: primary beneficiary and co-agenda-setter (institutional/arbitrage) — retain autonomy over matters this reading removes from federal reach
 *   - civil_rights_claimants_in_recalcitrant_states: primary target (powerless/trapped) — lose federal commerce-power backing for protections against local discrimination
 *   - workers_seeking_federal_labor_protection: primary target (powerless/constrained) — lose federal labor floors for intrastate employers
 *   - federal_judiciary: administers the boundary (institutional/analytical) — decides case by case which statutes survive
 *   - constitutional_law_scholars_and_historians: analytical observers — assess whether the reading recovers original meaning or launders policy preference as text
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__narrow_originalist, 0.28).
domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, 0.42).
domain_priors:theater_ratio(commerce_clause_scope__narrow_originalist, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, extractiveness, 0.28).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__narrow_originalist, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__narrow_originalist, "Commerce Clause — Narrow Originalist Reading (Trade-Crossing-Borders / Facilitation-Only)").
narrative_ontology:topic_domain(commerce_clause_scope__narrow_originalist, "constitutional/political/economic").

domain_priors:requires_active_enforcement(commerce_clause_scope__narrow_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__narrow_originalist, 'f1e1ba00-f7fa-4012-8b5c-aebf3d0d7106').
narrative_ontology:cs_kernel_codification('f1e1ba00-f7fa-4012-8b5c-aebf3d0d7106', fixed_text).
narrative_ontology:cs_authority_grounding('f1e1ba00-f7fa-4012-8b5c-aebf3d0d7106', lineage).
narrative_ontology:cs_interpretation_layer_present('f1e1ba00-f7fa-4012-8b5c-aebf3d0d7106').
narrative_ontology:cs_reading_relation('f1e1ba00-f7fa-4012-8b5c-aebf3d0d7106', commerce_clause_scope__broad_effects_test, forecloses).
narrative_ontology:cs_reading_relation('f1e1ba00-f7fa-4012-8b5c-aebf3d0d7106', commerce_clause_scope__intermediate_channels, influences).
narrative_ontology:cs_axiom('f1e1ba00-f7fa-4012-8b5c-aebf3d0d7106', foundational, commerce_means_trade_crossing_borders_only).
narrative_ontology:cs_axiom_status(commerce_means_trade_crossing_borders_only, holdable).
narrative_ontology:cs_axiom_grounding('f1e1ba00-f7fa-4012-8b5c-aebf3d0d7106', commerce_means_trade_crossing_borders_only, conventional).
narrative_ontology:cs_axiom('f1e1ba00-f7fa-4012-8b5c-aebf3d0d7106', foundational, regulate_means_facilitate_not_prohibit).
narrative_ontology:cs_axiom_status(regulate_means_facilitate_not_prohibit, holdable).
narrative_ontology:cs_axiom_grounding('f1e1ba00-f7fa-4012-8b5c-aebf3d0d7106', regulate_means_facilitate_not_prohibit, conventional).
narrative_ontology:cs_axiom('f1e1ba00-f7fa-4012-8b5c-aebf3d0d7106', secondary, federal_power_confined_to_barrier_removal).
narrative_ontology:cs_axiom_status(federal_power_confined_to_barrier_removal, holdable).
narrative_ontology:cs_axiom_grounding('f1e1ba00-f7fa-4012-8b5c-aebf3d0d7106', federal_power_confined_to_barrier_removal, instrumental).
narrative_ontology:cs_reference_frame('f1e1ba00-f7fa-4012-8b5c-aebf3d0d7106', founding_era_enumerated_powers_bargain).
narrative_ontology:cs_drift_state('f1e1ba00-f7fa-4012-8b5c-aebf3d0d7106', post_1937_switch_in_time_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('f1e1ba00-f7fa-4012-8b5c-aebf3d0d7106', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__narrow_originalist, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, local_businesses_shielded_from_federal_regulation).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, decentralized_regulatory_experimenters).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, civil_rights_claimants_in_recalcitrant_states).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, workers_seeking_federal_labor_protection).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, environmental_pollution_victims_across_state_lines).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, national_market_participants_facing_regulatory_fragmentation).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, enumerated_powers_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, dual_sovereignty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain plenary police-power authority over labor conditions, environmental permitting, land use, and civil rights enforcement within their borders under this reading, since only trade physically crossing state lines is federally regulable. States that wish to regulate aggressively may do so; states that wish not to are shielded from federal preemption. They administer the resulting patchwork and benefit from insulation against federal mandates they did not choose.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, state_governments, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__narrow_originalist, state_governments, agenda_setter).

% Purely intrastate manufacturers, small employers, and local service providers escape federal wage, safety, and environmental rules under this reading because their activity does not itself cross a state line. They can also relocate operations to jurisdictions with the lightest local rules, since federal uniformity does not reach them.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, local_businesses_shielded_from_federal_regulation, beneficiary,
    moderate, biographical, mobile, regional).

% Under the narrow reading, federal civil rights statutes reaching local hotels, restaurants, or employers who do not themselves transact across state lines lose their commerce-power foundation. Claimants in states unwilling to enforce comparable protections have no federal commerce-power hook to fall back on; their remedy depends entirely on the goodwill of the state they cannot leave without abandoning home, job, and community ties.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, civil_rights_claimants_in_recalcitrant_states, payer,
    powerless, biographical, trapped, local).

% Employees of firms whose production is consumed locally rather than shipped interstate lose access to federal minimum-wage, collective-bargaining, and safety floors under this reading, since their employer's activity is not itself commerce among the states. They depend on state legislatures to voluntarily replicate protections that this reading strips of federal backing; changing jobs or states is costly and often not a realistic option.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, workers_seeking_federal_labor_protection, payer,
    powerless, biographical, constrained, regional).

% Pollution from an intrastate factory that drifts downstream or downwind into a neighboring state harms people who had no vote in the polluting state's regulatory choices. Under this reading, federal environmental statutes reaching purely intrastate pollution sources lack commerce-power grounding, leaving cross-border externality victims without a federal forum unless the pollution itself is shown to be literally interstate trade.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, environmental_pollution_victims_across_state_lines, payer,
    powerless, generational, trapped, regional).

% Businesses operating across many states must comply with fifty different regulatory regimes on labor, environment, and consumer protection because federal uniformity cannot reach activity that is not itself interstate trade. They bear compliance-cost fragmentation that a uniform federal floor would have eliminated, and lobbying for federal preemption is foreclosed by the reading's own logic.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, national_market_participants_facing_regulatory_fragmentation, payer,
    organized, biographical, constrained, national).

% Advocacy groups, think tanks, and policy entrepreneurs who favor state-level policy variation as a laboratory for governance benefit structurally: this reading guarantees the persistence of fifty distinct regulatory environments to study, compete among, and lobby within, regardless of the human cost of fragmentation to any single state's residents.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, decentralized_regulatory_experimenters, beneficiary,
    organized, civilizational, mobile, national).

% Federal courts adopting this reading determine, case by case, which statutes retain a valid commerce-power foundation and which do not. They administer the boundary between interstate trade and local activity, and their rulings determine which victims of local harms have or lack a federal forum.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Study the founding-era usage of 'commerce' and 'regulate,' compare it against two centuries of doctrinal drift, and assess whether the narrow reading recovers original public meaning or merely re-labels a preferred policy outcome as textual compulsion.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, constitutional_law_scholars_and_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:fixing_cost_class(commerce_clause_scope__narrow_originalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the federal government from displacing state police powers over matters the Constitution's text plausibly leaves to the states — genuinely coordinates the vertical division of authority between two sovereigns by giving each a clear, textually-anchored domain, reducing perpetual boundary litigation over who may act.
% TRANSFER_FUNCTION: Moves regulatory authority away from the federal government and toward state legislatures and state courts on any matter not itself constituting trade crossing a state line; correspondingly moves the cost of state regulatory failure or inaction from a federal floor onto the residents of the affected state, who often cannot exit that state's jurisdiction.
% ABSENT_VOICES: Residents of states with weak enforcement capacity or hostile legislative majorities have no seat in the doctrinal debate over what 'commerce' meant in 1789 — the debate is conducted among judges, scholars, and litigants representing organized interests, not among the powerless people whose federal remedies disappear if the reading is adopted.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned overnight in favor of a broader test, federal statutes on labor standards, environmental protection, and civil rights currently vulnerable to narrow-commerce challenges would become secure; state governments would lose the insulation this reading provides and face federal floors on matters they currently regulate autonomously; national businesses would gain uniform compliance rules. The reading is not inert description — its adoption or rejection visibly reallocates who can be sued, regulated, or protected.
% FOUNDING_PROBLEM: The reading claims to solve the problem of federal overreach into matters the Constitution reserved to the states, restoring what its adherents describe as the original bargain of a national government of limited and enumerated powers rather than a general police power.
% FOUNDING_PROBLEM_CORROBORATION: Originalist legal scholars and some state attorneys general attest the problem (federal police-power overreach) is live and unaddressed by post-New-Deal doctrine. Civil rights historians, labor economists, and a substantial majority of constitutional scholars outside the originalist school attest that the 'problem' as framed is itself a post-hoc construction designed to delegitimize twentieth-century civil rights and labor statutes, and that the founding-era commercial-trade meaning of 'commerce' is itself contested among historians — corroboration from outside the reading's own adherents is genuinely split rather than absent, which is itself part of the contest.
narrative_ontology:disappearance_verdict(commerce_clause_scope__narrow_originalist, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__narrow_originalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__narrow_originalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_scope__narrow_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__narrow_originalist, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__narrow_originalist_tests).
:- end_tests(commerce_clause_scope__narrow_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28 at interval end) because, by this reading's own lights, the arrangement removes federal power rather than adding it — the reading is fundamentally about federal restraint, not federal extraction, so the primary 'cost' it imposes is the ABSENCE of a federal remedy rather than a positive transfer of resources to a rent-collecting party. But that absence is not costless: civil rights claimants, workers, and cross-border externality victims bear the real cost of the gap the reading opens, which is why victims are named honestly even though the mechanism is omission rather than active extraction. Suppression is moderate (0.42) and rising over the interval — the doctrine requires increasingly active judicial policing (case-by-case commerce-power litigation) to hold the boundary against nearly a century of contrary precedent and legislative practice, which is itself a form of enforcement cost. Theater is low (0.20): the reading's adherents are not performing coordination while doing something else; they are making a genuine (if contested) textual and structural claim.
 *
 * PERSPECTIVAL GAP:
 *   From the state-government seat, this reading is Rope: a clean division of sovereign labor that lets each level of government do what it is suited to do, with genuine coordination value in preventing perpetual boundary disputes. From the seat of a civil-rights claimant in a state unwilling to enforce comparable protections, the same textual rule operates as Tangled Rope at best (there IS a genuine coordination function — federalism does solve a real problem of overcentralization — but it also asymmetrically extracts protection from people who cannot exit) or arguably Snare in its effect (removing the only forum that could have vindicated their claim). The engine computes these divergent seat-classifications from the structural power/exit data; the claimed_type of tangled_rope reflects the authoring judgment that both a genuine coordination function AND asymmetric victim-bearing costs are simultaneously present and require active judicial enforcement to maintain against contrary doctrinal pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and local businesses sit near the beneficiary end: the reading directly enlarges their zone of unreviewable autonomy and its adoption transfers regulatory authority to them at no cost they bear. Civil rights claimants, low-wage workers, and cross-border pollution victims sit near the target end: they are structurally powerless, often trapped by economic or personal ties to the state whose choices harm them, and this reading removes the one lever (federal commerce power) that could override a hostile or indifferent state government. National market participants occupy an intermediate position — organized and resourced, but genuinely burdened by the fragmentation this reading entrenches, since their harm (compliance cost) is diffuse and non-identity-based rather than a matter of trapped exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — checking federal overreach into matters reserved to the states — is genuinely contested rather than dead or clearly live: originalist scholars maintain it remains an active constitutional commitment; most other scholars and civil rights historians argue the 'problem' as posed is itself a retrospective construction. Declaring founding_problem_status as contested (rather than live or dead) prevents this story from either dismissing the reading as pure pretext or crediting it as settled historical fact — the corroboration is genuinely split across a real scholarly and political divide, which the classification should reflect rather than resolve by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_meaning_of_commerce_contested,
    'Did ''commerce'' at the founding actually carry the narrow trade-crossing-borders meaning this reading asserts, or did founding-era usage already include production, agriculture, and manufacture affecting interstate markets?',
    'Historical linguistic corpus analysis of founding-era usage of ''commerce'' in ratification debates, Federalist Papers, state convention records, and contemporaneous dictionaries and legal treatises; comparison against the historical record of what problems the Commerce Clause was drafted to solve (interstate tariff wars, Articles of Confederation trade barriers).',
    'If founding-era usage was already broader than physical border-crossing, this reading''s textualist claim to originality collapses and it becomes a policy preference dressed in historical costume — which would push its classification toward snare (extraction of state autonomy at civil-rights-claimant expense, justified by a false naturalness claim) rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_meaning_of_commerce_contested, empirical, 'Whether the narrow reading''s claimed original meaning is historically accurate or a retrospective construction.').

omega_variable(
    reading_choice_as_kernel_framing,
    'Is the narrow_originalist reading a live, defensible interpretation of an irreducibly ambiguous constitutional text, or is the appearance of three co-equal readings itself a framing artifact that obscures a doctrinal history in which the broad reading has been settled law for eighty-plus years and the narrow reading is a minority reconstruction project?',
    'Track citation frequency, controlling-precedent status, and legislative reliance across all three readings over the post-1937 doctrinal period; assess whether courts treat narrow_originalist as a live alternative or as a dissenting/academic position.',
    'If the narrow reading is properly understood as a minority reconstruction project rather than a co-equal live reading, its own founding_problem_status framing (contested) may overstate its current doctrinal standing, and analysts using this story should weight the dissenting seats (civil rights claimants, national market participants) more heavily than adherent seats (state governments) in any aggregate assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_choice_as_kernel_framing, conceptual, 'Whether treating narrow_originalist as a co-equal kernel reading (rather than a minority position within the broad-reading regime) accurately represents current doctrinal reality.').

omega_variable(
    state_autonomy_beneficiary_or_natural_arrangement,
    'Is state regulatory autonomy under this reading a genuine constitutional beneficiary structure (states collect real governance rents from federal restraint), or is ''state sovereignty'' itself better understood as a vindicated proposition (a doctrine that legitimates the arrangement) rather than an actor that benefits?',
    'Distinguish cases where specific state governments materially benefit (retained taxing/regulatory authority, avoided compliance costs) from cases where ''federalism'' as an abstract value is invoked without any specific state government capturing a concrete gain.',
    'If state sovereignty is more doctrine than actor in many applications, some of what is coded here as beneficiary should instead be coded as vindicated_propositions, which would lower the measured beneficiary concentration and could shift the classification toward rope in contexts where no specific state actually gains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_autonomy_beneficiary_or_natural_arrangement, conceptual, 'Whether ''state sovereignty'' functions as a genuine beneficiary or as a legitimating doctrine layered over specific state-government beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__narrow_originalist, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_scope__narrow_originalist, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(comm_tr_t0, projected).
narrative_ontology:measurement(comm_tr_t8, commerce_clause_scope__narrow_originalist, theater_ratio, 8, 0.14).
narrative_ontology:measurement_basis(comm_tr_t8, projected).
narrative_ontology:measurement(comm_tr_t16, commerce_clause_scope__narrow_originalist, theater_ratio, 16, 0.16).
narrative_ontology:measurement_basis(comm_tr_t16, observed).
narrative_ontology:measurement(comm_tr_t24, commerce_clause_scope__narrow_originalist, theater_ratio, 24, 0.17).
narrative_ontology:measurement_basis(comm_tr_t24, observed).
narrative_ontology:measurement(comm_tr_t32, commerce_clause_scope__narrow_originalist, theater_ratio, 32, 0.19).
narrative_ontology:measurement_basis(comm_tr_t32, observed).
narrative_ontology:measurement(comm_tr_t40, commerce_clause_scope__narrow_originalist, theater_ratio, 40, 0.2).
narrative_ontology:measurement_basis(comm_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_scope__narrow_originalist, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(comm_be_t0, projected).
narrative_ontology:measurement(comm_be_t8, commerce_clause_scope__narrow_originalist, base_extractiveness, 8, 0.21).
narrative_ontology:measurement_basis(comm_be_t8, projected).
narrative_ontology:measurement(comm_be_t16, commerce_clause_scope__narrow_originalist, base_extractiveness, 16, 0.23).
narrative_ontology:measurement_basis(comm_be_t16, observed).
narrative_ontology:measurement(comm_be_t24, commerce_clause_scope__narrow_originalist, base_extractiveness, 24, 0.25).
narrative_ontology:measurement_basis(comm_be_t24, observed).
narrative_ontology:measurement(comm_be_t32, commerce_clause_scope__narrow_originalist, base_extractiveness, 32, 0.27).
narrative_ontology:measurement_basis(comm_be_t32, observed).
narrative_ontology:measurement(comm_be_t40, commerce_clause_scope__narrow_originalist, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(comm_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_scope__narrow_originalist, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(comm_su_t0, projected).
narrative_ontology:measurement(comm_su_t8, commerce_clause_scope__narrow_originalist, suppression_requirement, 8, 0.33).
narrative_ontology:measurement_basis(comm_su_t8, projected).
narrative_ontology:measurement(comm_su_t16, commerce_clause_scope__narrow_originalist, suppression_requirement, 16, 0.36).
narrative_ontology:measurement_basis(comm_su_t16, observed).
narrative_ontology:measurement(comm_su_t24, commerce_clause_scope__narrow_originalist, suppression_requirement, 24, 0.38).
narrative_ontology:measurement_basis(comm_su_t24, observed).
narrative_ontology:measurement(comm_su_t32, commerce_clause_scope__narrow_originalist, suppression_requirement, 32, 0.4).
narrative_ontology:measurement_basis(comm_su_t32, observed).
narrative_ontology:measurement(comm_su_t40, commerce_clause_scope__narrow_originalist, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(comm_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__narrow_originalist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__narrow_originalist, 0.12).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__intermediate_channels).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the commerce_clause_scope kernel, each authored as an independent ε-invariant constraint per the ε-invariance principle. narrow_originalist authors low extractiveness (0.28) reflecting its self-understanding as federal restraint rather than federal extraction, with victims borne through omission (absence of federal remedy) rather than active transfer. broad_effects_test would author substantially higher extractiveness from the state-autonomy perspective (aggregation doctrine reaching deep into intrastate activity) while authoring near-zero extraction from the civil-rights/labor-protection perspective. intermediate_channels sits structurally between the two, authoring moderate extraction bounded by explicit limiting principles. All three share the same underlying constitutional text and beneficiary/victim universe (state governments vs. civil rights claimants/workers/cross-border victims) but produce different structural classifications because they encode different readings of 'commerce' and 'regulate.' Network edges here are directional: adoption of narrow_originalist as controlling doctrine would foreclose or substantially narrow the operative reach of both siblings in any given case, which is the influences/forecloses structure captured in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_scope__narrow_originalist, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
