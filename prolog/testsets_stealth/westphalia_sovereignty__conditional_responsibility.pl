% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__conditional_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__conditional_responsibility, []).

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
 *   constraint_id: westphalia_sovereignty__conditional_responsibility
 *   human_readable: Conditional Sovereignty: Protection Failure as Forfeiture of Territorial Inviolability
 *   domain: international law/political theory/state systems
 *
 * SUMMARY:
 *   Since the 2005 World Summit Outcome Document, the operative international
 *   settlement holds that territorial inviolability is not unconditional: a
 *   state that commits or fails to prevent mass atrocities may have its
 *   immunity from external interference set aside, case by case, through
 *   Security Council authorization. The arrangement solves a real
 *   collective-action problem (no state intervenes against atrocity alone at
 *   acceptable cost) while transferring adjudicative authority over every
 *   state's borders to a small council of veto holders, whose application of
 *   the trigger has tracked strategic interest as often as atrocity severity.
 *   This file authors ONE reading of the westphalia_sovereignty kernel, per
 *   the epsilon-invariance rule: the conditional_responsibility reading, with
 *   its own epsilon, beneficiary/victim structure, and classification. The
 *   sibling readings (absolute_non_intervention, graded_sovereignty) are
 *   separate constraint stories linked through network.affects_constraints;
 *   they are not folded into this one. The claim/metric independence rule
 *   applies: the claimed type is stated from structural analysis, the metrics
 *   from descriptive record, and the engine computes per-seat classifications
 *   from the structural data.
 *
 * KEY AGENTS:
 *   - un_security_council_p5: Agenda-setting seat (institutional/arbitrage) — administers the forfeiture determination case by case and collects the adjudicative authority the arrangement concentrates; simultaneously bears exposure, since its members' own conduct is judged by the standard they wield
 *   - humanitarian_intervention_coalitions: Primary beneficiary (powerful/constrained) — supplies the capability that turns authorization into protection and receives legitimation and burden-sharing unavailable to unilateral action
 *   - global_governance_institutions: Secondary beneficiary (institutional/identity_locked) — UN organs and regional bodies gain mandate, staffing, and budget as designated adjudicators and implementers
 *   - protected_civilian_populations: Intended beneficiary with no procedural seat (powerless/trapped) — the parties the standard exists to shield; receive protection only when authorization and coalition capacity align
 *   - intervention_target_states: Primary payer (moderate/trapped) — governments accused of atrocity failure lose the expectation that borders shield them
 *   - small_weak_states: Structural payer (powerless/constrained) — carry a standing discount on inviolability because application of the standard is likelier against them than against strong states
 *   - international_humanitarian_ngos: Advocacy beneficiary (organized/mobile) — gain agenda influence, funding, and access from the framing of atrocity response as international duty
 *   - international_legal_scholars: Analytical observer (analytical/analytical) — track doctrine-practice divergence and supply interpretive ammunition to every faction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, 0.62).
domain_priors:suppression_score(westphalia_sovereignty__conditional_responsibility, 0.5).
domain_priors:theater_ratio(westphalia_sovereignty__conditional_responsibility, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, extractiveness, 0.62).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__conditional_responsibility, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__conditional_responsibility, "Conditional Sovereignty: Protection Failure as Forfeiture of Territorial Inviolability").
narrative_ontology:topic_domain(westphalia_sovereignty__conditional_responsibility, "international law/political theory/state systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__conditional_responsibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__conditional_responsibility, '161d4c08-3526-4f5d-a19a-cee7640c22f8').
narrative_ontology:cs_kernel_codification('161d4c08-3526-4f5d-a19a-cee7640c22f8', formalized).
narrative_ontology:cs_authority_grounding('161d4c08-3526-4f5d-a19a-cee7640c22f8', lineage).
narrative_ontology:cs_interpretation_layer_present('161d4c08-3526-4f5d-a19a-cee7640c22f8').
narrative_ontology:cs_reading_relation('161d4c08-3526-4f5d-a19a-cee7640c22f8', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('161d4c08-3526-4f5d-a19a-cee7640c22f8', westphalia_sovereignty__graded_sovereignty, influences).
narrative_ontology:cs_axiom('161d4c08-3526-4f5d-a19a-cee7640c22f8', foundational, protection_failure_forfeits_inviolability).
narrative_ontology:cs_axiom_status(protection_failure_forfeits_inviolability, holdable).
narrative_ontology:cs_axiom_grounding('161d4c08-3526-4f5d-a19a-cee7640c22f8', protection_failure_forfeits_inviolability, deontological).
narrative_ontology:cs_axiom('161d4c08-3526-4f5d-a19a-cee7640c22f8', secondary, council_adjudicates_forfeiture).
narrative_ontology:cs_axiom_status(council_adjudicates_forfeiture, holdable).
narrative_ontology:cs_axiom_grounding('161d4c08-3526-4f5d-a19a-cee7640c22f8', council_adjudicates_forfeiture, conventional).
narrative_ontology:cs_reference_frame('161d4c08-3526-4f5d-a19a-cee7640c22f8', sovereignty_conditioned_on_protection).
narrative_ontology:cs_drift_state('161d4c08-3526-4f5d-a19a-cee7640c22f8', post_libya_backlash_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('161d4c08-3526-4f5d-a19a-cee7640c22f8', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, global_governance_institutions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, protected_civilian_populations).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, intervention_target_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, small_weak_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, international_humanitarian_ngos).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, un_security_council_p5).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five permanent members hold veto over any determination that a state's protection has failed and any consequent authorization. They decide case by case whether territorial inviolability yields, which makes their discretion the arrangement's principal yield. Their own domestic conduct stands equally exposed to the standard they administer, and they pay diplomatically when the standard is turned toward their allies or themselves; their arbitrage position lets them shield clients and deflect application from their own affairs.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, un_security_council_p5, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__conditional_responsibility, un_security_council_p5, payer).

% Ad hoc groupings of capable states supply the aircraft, troops, logistics, and funding that turn an authorization into deployed protection. They receive legitimation and burden-sharing that unilateral action cannot obtain, and their reputational investment in the framework raises the cost of abandoning it. Declining any particular operation remains open to them, but acting outside the authorization channel costs alliances, domestic support, and legal cover.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions, beneficiary,
    powerful, biographical, constrained, global).

% United Nations organs, regional organizations, and treaty bodies gain mandate, staffing, budget, and moral standing from serving as the designated adjudicators and implementers of atrocity response. Their constitutive charters define them partly through this protective purpose, so relinquishing the role would dissolve institutional self-conception; the identity is fused with the function.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, global_governance_institutions, beneficiary,
    institutional, generational, identity_locked, global).

% Civilians facing massacre, expulsion, or starvation inside failing or predatory states are the parties the standard exists to shield. What reaches them depends entirely on whether authorization passes and coalitions deploy; when either fails, they retain no recourse except flight. They hold no seat in the chambers where the determination about their protection is made, entering the record only through testimony gathered by others.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, protected_civilian_populations, beneficiary,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__conditional_responsibility, protected_civilian_populations, excluded).

% Governments accused of committing or permitting mass atrocities lose the working assumption that borders shield internal conduct. They face sanctions, judicial referral, or authorized force; once targeted, diplomatic isolation closes off most exits short of capitulation, negotiation from weakness, or state collapse. Their objections register in the authorizing chamber only as the position of the accused.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, intervention_target_states, payer,
    moderate, biographical, trapped, national).

% States without nuclear deterrents, large economies, or patron protectors carry a standing discount on their inviolability: the standard is more likely to be applied to them than to strong states with equivalent or worse records, and they cannot credibly threaten costs against those who would apply it. Their defense is procedural argument, bloc voting in the General Assembly, and cultivation of veto-holder patrons. They coordinate with similarly positioned states through non-aligned and Group of 77 structures, which is the main coalition lever available to them.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, small_weak_states, payer,
    powerless, generational, constrained, national).

% Advocacy and aid organizations gain agenda influence, donor funding, and operational access from a norm that frames atrocity response as an international duty. They document failures, lobby the authorizing chamber, and supply the evidentiary record on which determinations rest. Their attachment is professional rather than existential: if the framework loses traction they can redirect effort to other causes.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, international_humanitarian_ngos, beneficiary,
    organized, biographical, mobile, global).

% Academics and jurists track the doctrine's internal consistency, document divergence between declaration and practice, and supply the interpretive arguments that every faction deploys. They neither collect nor pay under the arrangement; their stake is the coherence of the legal order the arrangement claims to serve.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__conditional_responsibility, un_security_council_p5).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__conditional_responsibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared threshold and an authorizing procedure for collective response to mass atrocity: states that would never intervene alone at acceptable cost gain a legitimate, burden-shared channel, and threatened populations gain a focal point for appeal beyond their own government.
% TRANSFER_FUNCTION: Moves adjudicative authority over territorial inviolability from individual states to the Security Council and its authorized coalitions; moves intervention costs onto participating coalition members; moves a standing security discount onto every state's borders, weighted toward those least able to resist application; and moves protection, when authorization and capacity align, to threatened civilian populations.
% ABSENT_VOICES: Populations of targeted states have no formal seat in the chamber deciding intervention against their government. Small states without permanent representation shape outcomes only through General Assembly resolutions the Council may disregard. Target governments object loudly but enter the record solely as the accused. The strongest procedural dissenters (skeptical veto holders) are present but exercise their voice through obstruction rather than argument.
% DISAPPEARANCE_RATIONALE: Overnight removal would return atrocity response to pure coalition-of-the-willing politics or categorical abstention: the Council would lose its case-by-case gatekeeping role, humanitarian advocacy would lose its legal hook, weak states would reprice border security sharply upward, and every atrocity would be negotiated anew with no shared threshold, no pre-built coalition templates, and no legitimation channel for those inclined to act.
% FOUNDING_PROBLEM: The 1990s demonstrated that categorical non-intervention left Rwanda and Srebrenica unprotected while unilateral intervention lacked legitimacy. The arrangement was built to reconcile territorial inviolability with a duty to stop mass atrocity by making protection the condition of sovereign immunity from external interference.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the African Union's Constitutive Act (Article 4(h), adopted 2000) independently asserted a right of intervention in grave circumstances before the Western coalition embraced the framing, attesting the problem from a non-permanent-member seat. United Nations commissions of inquiry and human rights organizations document continuing atrocity in Sudan, Myanmar, Ethiopia, and Syria. Norm-skeptical Group of 77 statements concede the occurrence of mass atrocities while disputing the remedy, which attests the founding problem even among parties that reject the solution.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__conditional_responsibility, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__conditional_responsibility, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__conditional_responsibility, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalia_sovereignty__conditional_responsibility, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__conditional_responsibility, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__conditional_responsibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__conditional_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.62: the arrangement genuinely transfers something of value (border security, adjudicative autonomy) away from all states and concentrates decision authority in five capitals, with application weighted toward the weak; but it also delivers real protective coordination in some cases, so extraction is substantial rather than dominant. Suppression is 0.50 and is authored as a raw structural property, unscaled by power or scope: the arrangement's coercive machinery (sanctions, referral, authorized force) matured through the 2011 Libya operation and has since decayed as veto-holder consensus eroded, producing the inverted-U suppression_requirement series. Theater_ratio is 0.52 and rising: invocation without deployment (Myanmar, Ethiopia, Sudan debates) now rivals actual protective delivery, as rhetorical maintenance substitutes for enforcement. Accessibility_collapse is 0.35: the alternatives remain fully live — categorical non-intervention is still argued by powerful states, and unauthorized unilateral intervention recurs — so understanding the arrangement does not close off exits. Resistance is 0.60: vetoes, the Brazilian 'responsibility while protecting' initiative, and sustained Group of 77 objection constitute organized, ongoing pushback. All three tracked metrics run on one shared seven-point grid (t=0 maps to the 2001 ICISS report, t=24 to 2025) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. From the permanent-member seat the arrangement is discretionary authority: near-zero personal exposure, full control of when the condition binds, and the standing option to shield allies. From the small-state payer seat it is a standing hazard: a border guarantee that is weaker for them than for the strong, defended mainly through procedural argument and bloc voting. From the protected-population seat it is promissory protection whose delivery depends on other parties' politics in chambers they cannot enter. From the coalition seat it is legitimation plus burden-sharing. The engine derives these divergent per-seat classifications from the power, exit, and directional data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the coalition, governance, and population seats: the arrangement subsidizes them with authority, mandate, and (contingently) protection. Victim declarations drive high directionality for target and small states: they bear the transfer of border security and adjudicative autonomy, with trapped exit amplifying their position toward the full-target end. The permanent members occupy a genuinely dual position: they administer and collect the arrangement's principal yield (the discretion itself), yet their own domestic conduct stands exposed to the standard, which is why they carry a payer secondary role. Humanitarian NGOs sit as modest beneficiaries with mobile exit; scholarly observers sit at the analytical pole with no flow in either direction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: mass atrocities continue and each case reopens the question the arrangement was built to answer, so no mandatrophy resolution is declared. Two mislabeling risks are guarded against. First, mistaking this for pure extraction ignores the demonstrated coordination deliveries (diplomatic protection in Kenya 2008, authorized action in Cote d'Ivoire 2011 and Gambia 2017) — the coordination function is real, which is why the hybrid classification rather than a pure-extraction one. Second, mistaking the rising theater_ratio for terminal atrophy ignores the capture structure: unlike an inertial leftover that nobody profits from maintaining, a specific seat (the permanent members) demonstrably collects the arrangement's principal yield, which sustains active maintenance. The trajectory to watch is rhetorical maintenance outpacing functional delivery; if enforcement capacity continues decaying while invocation continues rising, the arrangement drifts toward performance-dominated operation with the captured seat still collecting.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    westphalia_kernel_reading_contest,
    'This constraint instantiates the conditional_responsibility reading of the westphalia_sovereignty kernel; what structural changes would the sibling readings (absolute_non_intervention, graded_sovereignty) produce?',
    'Comparative classification of the sibling stories against this one: identical referent (the standing sovereignty/intervention arrangement), but different victim sets, epsilon, and adjudicative structure per reading.',
    'Under absolute_non_intervention the forfeiture mechanism vanishes entirely: no victim set, negligible extraction, and the humanitarian coalition and governance seats lose the authority they currently collect. Under graded_sovereignty the trigger shifts from conduct to capacity, converting atrocity-exposed populations into one subclass of a broader capacity-deficient population and raising the standing burden on weak states across the board.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(westphalia_kernel_reading_contest, conceptual, 'Committer structure: this is one of three live readings of the Westphalian sovereignty kernel; sibling readings are separate constraint files.').

omega_variable(
    adjudication_neutrality,
    'Is the forfeiture determination applied by the declared criterion (failure to protect populations from mass atrocity) or by the permanent members'' strategic interests?',
    'Cross-case analysis of intervention decisions regressed on atrocity severity while controlling for permanent-member strategic exposure (alliance ties, resource interests, geographic proximity to veto holders).',
    'Interest-driven selection would establish the protective criterion as cover for a power-politics allocation and would invert several beneficiary designations; criterion-driven application would confirm the coordination function as dominant and stabilize the hybrid profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjudication_neutrality, empirical, 'Whether the conditionality trigger tracks atrocity facts or veto-holder interests.').

omega_variable(
    protection_net_effect,
    'Does invocation of the standard improve survival outcomes for threatened populations, net of intervention harms and of the deterrence lost when the standard is invoked but not enforced?',
    'Matched comparison of atrocity trajectories in cases with authorized protective action (Kenya 2008, Cote d''Ivoire 2011, Gambia 2017) against closely matched non-cases (Myanmar 2017, Ethiopia 2020-2022).',
    'Net-negative outcomes would flip protected_civilian_populations from beneficiary toward payer and raise measured extraction substantially; net-positive outcomes would stabilize the current hybrid coordination-plus-transfer profile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protection_net_effect, empirical, 'Whether the arrangement delivers net protection to the populations it names as its object.').

omega_variable(
    forfeiture_threshold_definition,
    'Where does ''failing to protect'' begin: manifest commission or permission of mass atrocity, or mere incapacity to prevent it?',
    'Doctrinal analysis of World Summit Outcome Document paragraphs 138-139 drafting history, plus subsequent state practice distinguishing unwilling from unable governments.',
    'A broad incapacity reading converts every fragile state into a standing candidate for forfeiture, drifting the arrangement toward the graded_sovereignty sibling; a narrow commission-only reading renders the condition nearly inert, drifting back toward absolute_non_intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forfeiture_threshold_definition, conceptual, 'Threshold ambiguity in the forfeiture condition determines which states are exposed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__conditional_responsibility, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__conditional_responsibility, theater_ratio, 0, 0.2).
narrative_ontology:measurement(west_tr_t4, westphalia_sovereignty__conditional_responsibility, theater_ratio, 4, 0.25).
narrative_ontology:measurement(west_tr_t8, westphalia_sovereignty__conditional_responsibility, theater_ratio, 8, 0.3).
narrative_ontology:measurement(west_tr_t12, westphalia_sovereignty__conditional_responsibility, theater_ratio, 12, 0.38).
narrative_ontology:measurement(west_tr_t16, westphalia_sovereignty__conditional_responsibility, theater_ratio, 16, 0.45).
narrative_ontology:measurement(west_tr_t20, westphalia_sovereignty__conditional_responsibility, theater_ratio, 20, 0.5).
narrative_ontology:measurement(west_tr_t24, westphalia_sovereignty__conditional_responsibility, theater_ratio, 24, 0.52).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(west_be_t4, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(west_be_t8, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(west_be_t12, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(west_be_t16, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(west_be_t20, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(west_be_t24, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(west_su_t4, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(west_su_t8, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(west_su_t12, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(west_su_t16, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(west_su_t20, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(west_su_t24, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 24, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__conditional_responsibility, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, graded_sovereignty).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the westphalia_sovereignty kernel per the epsilon-invariance principle. The colloquial label 'sovereignty' conflates three structurally distinct claims: categorical inviolability (no forfeiture mechanism, no victim set, negligible extraction), conduct-triggered conditionality (this file: forfeiture on protection failure, adjudicated centrally, hybrid coordination/extraction), and capacity-graded authority (scalar calibration, extraction weighted to weak states). The absolute_non_intervention reading is upstream in lineage terms (Charter text) and is cited by skeptics as evidence against this reading; the graded_sovereignty reading exerts practical pressure on this one because implementing a conduct trigger requires capacity judgments about who can protect. Each member of the family links to the others through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
