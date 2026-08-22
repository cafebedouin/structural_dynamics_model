% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__liberal_institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__liberal_institutional_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__liberal_institutional_reading
 *   human_readable: Rules-Based International Order as Universal Consent-Based Multilateral Coordination (Liberal-Institutional Reading)
 *   domain: international relations / international law / political economy
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the contested kernel
 *   rbio_practice_norm_complex: the liberal-institutional reading, on which
 *   the rules-based international order is a universal, consent-based,
 *   multilaterally revisable coordination achievement, and uneven enforcement
 *   reflects capacity limits rather than illegitimacy. On this reading,
 *   intervention is justified when the Security Council authorizes it or when
 *   grave atrocities trigger responsibility-to-protect reasoning; economic
 *   conditionality is contract terms freely ratified; the arrangement's costs
 *   are real but legitimate. The epsilon referent is the standing arrangement
 *   under contest - the actual operating practice of the order
 *   (Security-Centered security governance, sanctions regimes, conditionality
 *   lending, contractor-supported intervention) - assessed by this reading's
 *   own lights, which is why epsilon lands at a moderate 0.56 rather than the
 *   markedly higher values the sibling readings would author over the same
 *   referent. Claim and metrics are authored independently: the claimed_type
 *   (rope) records the reading's own legitimating claim, while the metrics
 *   record the arrangement's actual operation, including burden distributions
 *   this reading itself acknowledges. The sibling readings are separate
 *   constraint files linked through network.affects_constraints; per the
 *   epsilon-invariance principle they are not folded into this one.
 *
 * KEY AGENTS:
 *   - - intervening_coalition_states: Primary beneficiary (powerful/arbitrage) - lead authorized coalitions, channel intervention and reconstruction spending to home firms, face minimal adverse enforcement
 *   - - defense_reconstruction_contractors: Secondary beneficiary (organized/mobile) - revenue scales with enforcement volume; supply logistics, security, and rebuilding services
 *   - - p5_permanent_members: Agenda-setter with beneficiary position (institutional/arbitrage) - hold veto, draft mandates, shield selves and allies from enforcement
 *   - - international_financial_institutions: Agenda-setter with beneficiary position (institutional/constrained) - design and monitor conditionality; income rides on lending volumes
 *   - - small_rule_dependent_states: Incidental beneficiary (powerless/constrained) - genuine protection from maritime law, dispute settlement, and verification regimes; press hardest for universality
 *   - - sanctioned_targeted_states: Primary payer (moderate/trapped) - absorb asset freezes, embargoes, financial exclusion; exit from the financial commons is ruinous
 *   - - civilian_populations_of_sanctioned_states: Primary payer (powerless/trapped) - bear medicine shortages, food inflation, infrastructure decay, and intervention violence with no standing in design
 *   - - global_south_coalition_states: Excluded voice (organized/constrained) - Assembly majorities without veto; document asymmetry, propose reform, build parallel institutions
 *   - - international_law_community: Analytical observer (analytical/analytical) - tracks compliance patterns and selectivity across the full case population
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, 0.56).
domain_priors:suppression_score(rbio_practice_norm_complex__liberal_institutional_reading, 0.66).
domain_priors:theater_ratio(rbio_practice_norm_complex__liberal_institutional_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__liberal_institutional_reading, rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__liberal_institutional_reading, "Rules-Based International Order as Universal Consent-Based Multilateral Coordination (Liberal-Institutional Reading)").
narrative_ontology:topic_domain(rbio_practice_norm_complex__liberal_institutional_reading, "international relations / international law / political economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__liberal_institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__liberal_institutional_reading, 'c754f915-166f-4115-bfe9-e5f97c5b837a').
narrative_ontology:cs_kernel_codification('c754f915-166f-4115-bfe9-e5f97c5b837a', fixed_text).
narrative_ontology:cs_authority_grounding('c754f915-166f-4115-bfe9-e5f97c5b837a', lineage).
narrative_ontology:cs_interpretation_layer_present('c754f915-166f-4115-bfe9-e5f97c5b837a').
narrative_ontology:cs_reading_relation('c754f915-166f-4115-bfe9-e5f97c5b837a', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('c754f915-166f-4115-bfe9-e5f97c5b837a', rbio_practice_norm_complex__sovereignty_maximalist_reading, forecloses).
narrative_ontology:cs_axiom('c754f915-166f-4115-bfe9-e5f97c5b837a', foundational, multilateral_authorization_confers_legitimacy).
narrative_ontology:cs_axiom_status(multilateral_authorization_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('c754f915-166f-4115-bfe9-e5f97c5b837a', multilateral_authorization_confers_legitimacy, conventional).
narrative_ontology:cs_axiom('c754f915-166f-4115-bfe9-e5f97c5b837a', foundational, enforcement_selectivity_is_capacity_limited).
narrative_ontology:cs_axiom_status(enforcement_selectivity_is_capacity_limited, holdable).
narrative_ontology:cs_axiom_grounding('c754f915-166f-4115-bfe9-e5f97c5b837a', enforcement_selectivity_is_capacity_limited, empirically_contingent).
narrative_ontology:cs_axiom('c754f915-166f-4115-bfe9-e5f97c5b837a', secondary, ratified_consent_binds_non_withdrawing_states).
narrative_ontology:cs_axiom_status(ratified_consent_binds_non_withdrawing_states, holdable).
narrative_ontology:cs_axiom_grounding('c754f915-166f-4115-bfe9-e5f97c5b837a', ratified_consent_binds_non_withdrawing_states, conventional).
narrative_ontology:cs_reference_frame('c754f915-166f-4115-bfe9-e5f97c5b837a', universal_multilateral_consent_order).
narrative_ontology:cs_drift_state('c754f915-166f-4115-bfe9-e5f97c5b837a', contemporary_multipolar_rivalry, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c754f915-166f-4115-bfe9-e5f97c5b837a', '2026-08-05T14:22:00Z').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, intervening_coalition_states).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, defense_reconstruction_contractors).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, small_rule_dependent_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, sanctioned_targeted_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_of_sanctioned_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, p5_permanent_members).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, international_financial_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lead military and sanctions coalitions under Security Council authorization or grave-atrocity justification. Their logistics networks, firms, and allied institutions carry intervention and reconstruction spending, and their exposure to adverse enforcement is minimal because they hold agenda power over mandate drafting. When authorization is blocked they can act through ad hoc coalitions while continuing to invoke the rules they interpret.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, intervening_coalition_states, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, intervening_coalition_states, agenda_setter).

% Supply transport, private security, demining, logistics, and rebuilding services during interventions and post-sanctions recovery. Revenue scales with the volume of enforcement activity, and contracts are awarded by the intervening states and financial institutions. Their capital and personnel move fluidly between theaters and regulatory environments.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, defense_reconstruction_contractors, beneficiary,
    organized, biographical, mobile, global).

% Depend on maritime-law guarantees, dispute-settlement bodies, and non-proliferation verification for protection against larger neighbors. They ratify widely and comply closely, and their benefit depends on the rules applying to everyone - which is why they press hardest for universality and are the first harmed when enforcement turns selective.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, small_rule_dependent_states, beneficiary,
    powerless, generational, constrained, national).

% Hold veto power over Council action, draft sanctions resolutions and mandate language, and administer the arrangement's central security machinery. The veto shields themselves and their allies from enforcement directed at them, so they collect immunity from the very enforcement they administer. Reform proposals that would dilute this position originate elsewhere and stall here.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, p5_permanent_members, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, p5_permanent_members, beneficiary).

% Attach policy conditions to stabilization and development lending; their staff design, negotiate, and monitor adjustment programs. Interest, fee, and administrative income depends on continued lending volumes under the conditionality framework, giving the institutions a structural stake in the arrangement's continuation and in the terms remaining enforceable.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, international_financial_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, international_financial_institutions, beneficiary).

% Face asset freezes, trade embargoes, designation regimes, and exclusion from dollar clearing and trade finance. Leaving the financial commons would collapse their economies, so they absorb the measures, build workaround channels, and contest designations diplomatically without ever exiting the system the measures ride on.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, sanctioned_targeted_states, payer,
    moderate, biographical, trapped, regional).

% Bear medicine shortages, food-price inflation, infrastructure decay, and, during interventions, direct violence. They have no standing in sanctions-committee design, no vote in the institutions that impose the measures, and no realistic ability to relocate at scale. Humanitarian carve-outs modulate but do not eliminate their exposure.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_of_sanctioned_states, payer,
    powerless, immediate, trapped, national).

% Hold durable majorities in the General Assembly but no permanent seats or veto. They document enforcement asymmetries, sponsor Council-reform proposals, and construct parallel institutions and payment arrangements. Their objections register in debate and resolution text but not in agenda control over mandates or designations.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, global_south_coalition_states, excluded,
    organized, generational, constrained, continental).

% Scholars, judges, and practitioners who track compliance patterns across the full case population, publish selectivity studies, and adjudicate disputes. They see the whole structure of authorization, exemption, and burden distribution that individual participant seats experience only locally.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, international_law_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__liberal_institutional_reading, intervening_coalition_states).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__liberal_institutional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides common rules and standing forums that reduce transaction costs and make commitments credible among sovereign states: navigation and overflight guarantees, trade dispute settlement, treaty verification, diplomatic recognition, and collective-security procedure. Stated without evaluation: these are the coordination problems the arrangement addresses.
% TRANSFER_FUNCTION: Moves wealth and policy autonomy from targeted states and their populations toward the intervening-coalition side: sanctioned assets and forgone trade, conditionality-linked payments and policy concessions, and intervention-and-reconstruction contract flows to coalition-state firms. It also moves agenda authority from national capitals to multilateral bodies in which the coalition holds procedural power.
% ABSENT_VOICES: Targeted-state governments appear in the arrangement only as objects of enforcement, never as agenda-setters; their publics have no standing anywhere in sanctions design; the global-south majority holds debate time but no veto or agenda control. These are the voices that would object to selectivity and burden distribution if seated, and they correspond to the excluded stakeholder in this story.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would force wholesale reconstruction: maritime commerce would lose its guarantee structure, cross-border payment and trade finance would fragment, non-proliferation verification would lapse, and diplomatic protocol would revert to bilateral improvisation. Regional powers would move coercively into the vacuums, and the small states currently protected by universal rules would lose their principal shield.
% FOUNDING_PROBLEM: Two world wars and the interwar collapse: great-power war, aggressive revisionism, and beggar-thy-neighbor economic fragmentation. The arrangement was built to prevent great-power war through collective security and to stabilize economic relations through agreed rules.
% FOUNDING_PROBLEM_CORROBORATION: Diplomatic historians and international-relations scholars outside the intervening-coalition beneficiary set corroborate both the founding problem and its continuing liveness; the archival record of 1919 and 1945 negotiations is public and non-party-gated. Independent corroboration also comes from the rival powers' own behavior: their investment in parallel institutions presupposes the same coordination problem rather than disputing it. No seat attests the founding problem is dead.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__liberal_institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__liberal_institutional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__liberal_institutional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__liberal_institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.56 because even by this reading's own lights the arrangement moves substantial resources upward: comprehensive and targeted sanctions concentrate costs on targeted economies, conditionality transfers policy autonomy alongside funds, and intervention-and-reconstruction spending flows to coalition-state firms. The reading deems these flows legitimate, which caps rather than eliminates the measured extraction. Suppression is 0.66 as a raw structural property, unscaled by power or scope: sanctions, financial exclusion, and authorized force are coercive instruments by design, whatever their legality. Theater_ratio is 0.45 and rising across the series: summitry, compliance reporting, and rules-based-order rhetoric have grown faster than corresponding practice, though core functions (verification, dispute settlement, navigation guarantees) remain real. Accessibility_collapse is 0.50: alternatives (bilateralism, regional blocs, parallel institutions, autarky) remain comprehensible and partially usable, but full exit from the financial and legal commons is ruinous, so alternatives collapse substantially for trapped agents while remaining open for powerful ones. Resistance is 0.62: Council-reform campaigns, de-dollarization efforts, new development banks, and ICC non-cooperation are organized, sustained, and partially effective. All three tracked series share one nine-point time grid (1945-2025 at decade steps plus endpoints) so every metric is authored at every examined time point; the gentle mid-century dip in extractiveness tracks decolonization-driven universalization, and the post-1985 rise tracks the structural-adjustment and post-Cold-War enforcement waves.
 *
 * PERSPECTIVAL GAP:
 *   The same structure computes differently by seat. From the agenda-setter seats (permanent members, financial institutions) the arrangement is machinery they operate: authorization procedures, mandate drafting, program design - experienced as legitimate process. From the payer seats (targeted states, their civilians) the identical machinery arrives as externally imposed burden with no agenda input - experienced as imposition. From the small compliant states the arrangement delivers genuine protection against larger neighbors - the strongest evidence for the reading's coordination claim. The engine derives these divergent per-seat classifications from the authored power, exit, and role data; this story's rope claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive directionality toward the subsidy end: intervening_coalition_states combine beneficiary status with arbitrage-grade exit (they wrote the rules and can reinterpret or bypass them), placing them nearest d=0; defense_reconstruction_contractors sit similarly. small_rule_dependent_states are beneficiaries but with constrained exit, so their derived d sits somewhat higher than the interveners'. Victim declarations drive the payer seats toward the target end: civilian_populations_of_sanctioned_states combine victim status with trapped exit and powerless power, placing them nearest d=1; sanctioned_targeted_states are close behind. The dual-positioned seats (permanent members, financial institutions) derive beneficiary-side directionality from their declarations despite their administrative roles - their agenda power amplifies rather than offsets the collection position. The excluded seat contributes no directional arithmetic; it marks where objection would enter if admitted.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - preventing great-power war and interwar-style economic collapse - remains live, so the arrangement is not yet a mandatrophy corpse; the mismatch consumer should find status=live paired with verdict=world_rearranges, no zombie flag. The risk this story tracks is forward-looking: the theater_ratio series (0.20 to 0.45) traces a growing rhetorical layer atop selectively applied practice, which is the characteristic precursor of mandate substitution - 'rules-based order' becoming a performance invoked rather than a system operated. The classification discipline cuts both ways: keeping the rope claim on record preserves visibility of the genuine coordination function (navigation guarantees, dispute settlement, verification that demonstrably work), while the victim declarations preserve visibility of the burden asymmetry the reading's own framing tends to background - blocking both pure-coordination complacency and pure-extraction dismissal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (liberal_institutional_reading) of the kernel rbio_practice_norm_complex; what would the sibling readings (hegemonic_extraction_reading, sovereignty_maximalist_reading) change structurally if instantiated instead?',
    'Cross-reading comparison of the three files'' epsilon values, beneficiary/victim sets, and computed types over the shared referent (the standing RBIO arrangement); disagreement located specifically in the interpretation of enforcement selectivity and the legitimacy of authorized intervention.',
    'The hegemonic sibling would author much higher epsilon and reframe selectivity as intent-revealing; the sovereignty sibling would deny intervention legitimacy entirely and expand the victim set to every interfered state. Per-seat classifications and network contamination paths differ accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: this file authors one reading of a contested kernel, not the kernel itself.').

omega_variable(
    selectivity_capacity_vs_intent,
    'Is enforcement selectivity genuinely capacity-limited (this reading''s claim) or intent-revealing (the hegemonic sibling''s claim)?',
    'Compare enforcement initiation rates against similarly situated violators, controlling for target capability and testing whether alignment with coalition preferences predicts enforcement better than capability does.',
    'If alignment dominates capability as a predictor, the capacity explanation fails, the foundational axiom enforcement_selectivity_is_capacity_limited loses its empirical grounding, and epsilon rises sharply toward the hegemonic sibling''s valuation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_capacity_vs_intent, empirical, 'Whether the reading''s capacity framing of selectivity survives controlled comparison.').

omega_variable(
    accession_consent_quality,
    'Is ratification by weaker states meaningful consent, or structured acquiescence under threat of market exclusion and aid withdrawal?',
    'Examine accession negotiation records for coercion markers: linkage of ratification to financing, trade access, or security assistance; compare terms offered to strong versus weak applicants.',
    'If consent is systematically coerced, the contract-term framing of economic conditionality collapses, the consent-based axiom weakens, and the extraction component of the arrangement grows within this reading''s own ledger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accession_consent_quality, conceptual, 'Quality of the consent on which the reading''s legitimacy claim rests.').

omega_variable(
    civilian_harm_ledger_placement,
    'Do sanctions harms borne by civilian populations count as extraction costs in this reading''s ledger, or as tragic side effects that humanitarian carve-outs adequately address?',
    'A values question resolved by specifying the reading''s welfare accounting: whether foreseeable, recurrent, population-wide harm from a deliberate instrument enters the extraction column or is discounted as collateral.',
    'Placing civilian harm in the extraction column raises epsilon materially and pushes per-seat computation toward tangled_rope or snare for the enforcing seats; discounting it keeps epsilon moderate and preserves the rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_harm_ledger_placement, preference, 'Where the reading''s own value framework places the largest recurring cost of enforcement.').

omega_variable(
    amendment_pathway_usability,
    'Are the formal revision pathways (Charter amendment under Articles 108/109, treaty review conferences) genuinely usable, or practically blocked by permanent-member interest?',
    'Historical base rate: count proposed amendments and revisions against successful ones since 1945, coding each failure''s proximate cause (great-power objection versus diffuse inertia).',
    'If the pathways are practically blocked, the revisability half of the reading''s claim fails, the reading converges structurally toward the hegemonic sibling, and the arrangement''s coordination function degrades toward frozen-rule maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_pathway_usability, empirical, 'Whether the multilateral revision machinery the reading appeals to actually functions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__liberal_institutional_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(rbio_tr_t1955, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1955, 0.23).
narrative_ontology:measurement(rbio_tr_t1965, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1965, 0.26).
narrative_ontology:measurement(rbio_tr_t1975, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1975, 0.29).
narrative_ontology:measurement(rbio_tr_t1985, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1985, 0.31).
narrative_ontology:measurement(rbio_tr_t1995, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1995, 0.34).
narrative_ontology:measurement(rbio_tr_t2005, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2005, 0.39).
narrative_ontology:measurement(rbio_tr_t2015, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2015, 0.42).
narrative_ontology:measurement(rbio_tr_t2025, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1945, 0.42).
narrative_ontology:measurement(rbio_be_t1955, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1955, 0.38).
narrative_ontology:measurement(rbio_be_t1965, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1965, 0.37).
narrative_ontology:measurement(rbio_be_t1975, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1975, 0.41).
narrative_ontology:measurement(rbio_be_t1985, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1985, 0.47).
narrative_ontology:measurement(rbio_be_t1995, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1995, 0.51).
narrative_ontology:measurement(rbio_be_t2005, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2005, 0.53).
narrative_ontology:measurement(rbio_be_t2015, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(rbio_be_t2025, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2025, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(rbio_su_t1955, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1955, 0.41).
narrative_ontology:measurement(rbio_su_t1965, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1965, 0.38).
narrative_ontology:measurement(rbio_su_t1975, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1975, 0.43).
narrative_ontology:measurement(rbio_su_t1985, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1985, 0.49).
narrative_ontology:measurement(rbio_su_t1995, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(rbio_su_t2005, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2005, 0.62).
narrative_ontology:measurement(rbio_su_t2015, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2015, 0.64).
narrative_ontology:measurement(rbio_su_t2025, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2025, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__liberal_institutional_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__sovereignty_maximalist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'RBIO norms': the label covers at least three structurally distinct claims, each with its own stable epsilon, beneficiary/victim structure, and type. This file is the liberal-institutional member (legitimate, revisable coordination with capacity-limited enforcement). The hegemonic_extraction_reading member reads the same institutions as a frozen extractive project; the sovereignty_maximalist_reading member denies intervention legitimacy outright. Edges run FROM this reading TO both siblings because this reading supplies the legitimating vocabulary the hegemonic reading cites as cover and the sovereignty reading cites as pretext: degradation of this reading's credibility propagates contamination to both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
