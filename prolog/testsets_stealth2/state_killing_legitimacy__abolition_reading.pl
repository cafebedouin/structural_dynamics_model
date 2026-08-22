% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__abolition_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: state_killing_legitimacy__abolition_reading
 *   human_readable: Abolition Reading: Categorical Prohibition of State Killing
 *   domain: criminal justice/political philosophy/legal theory
 *
 * SUMMARY:
 *   This story instantiates the abolition reading of the contested kernel
 *   state_killing_legitimacy: the claim that state killing categorically
 *   violates human dignity, admitting no exception for desert or utility. The
 *   standing arrangement under contest — the referent fixed for all three
 *   readings of this kernel — is the practice of state killing as authorized
 *   and carried out by retaining jurisdictions. This reading assesses that
 *   arrangement through an inalienable-life-right lens and finds it maximally
 *   violative: the extracted good is life itself, and the reading's
 *   categorical form refuses every offsetting consideration its siblings
 *   offer. The constraint this reading instantiates is the prohibition
 *   itself, enforced by constitutional courts and treaty bodies against
 *   recurring reinstatement pressure; its incidence falls on the state's
 *   killing power (stripped, and migrating into substitutes) while condemned
 *   persons and capital-risk defendants hold the secured life-right. KEY
 *   AGENTS (by structural relationship): - condemned_persons: Primary
 *   beneficiary (powerless/trapped) — holds the secured life-right the
 *   prohibition guarantees - persons_at_capital_risk: Preventive beneficiary
 *   (powerless/constrained) — protected before they know it -
 *   state_killing_power: Primary target (institutional/arbitrage) — the
 *   capacity the prohibition strips; migrates into substitutes -
 *   constitutional_abolition_courts: Agenda setter
 *   (institutional/constrained) — administers and defends the prohibition -
 *   human_rights_treaty_bodies: Agenda setter (institutional/constrained) —
 *   makes abolition a membership condition - retentionist_legislatures: Payer
 *   and excluded voice (powerful/arbitrage) — bear the stripped option; test
 *   its edges - capital_prosecution_offices: Payer (organized/constrained) —
 *   lost the capital lever, rebuilt around life terms -
 *   crime_victims_families: Excluded voice (organized/constrained) — remedy
 *   foreclosed by the categorical form - abolition_advocacy_movements: Agenda
 *   setter (organized/identity_locked) — the campaign is the institution -
 *   human_rights_philosophers: Analytical observer (analytical/analytical) —
 *   sees the full kernel structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, 0.95).
domain_priors:suppression_score(state_killing_legitimacy__abolition_reading, 0.58).
domain_priors:theater_ratio(state_killing_legitimacy__abolition_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__abolition_reading, rope).
narrative_ontology:human_readable(state_killing_legitimacy__abolition_reading, "Abolition Reading: Categorical Prohibition of State Killing").
narrative_ontology:topic_domain(state_killing_legitimacy__abolition_reading, "criminal justice/political philosophy/legal theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__abolition_reading, 'ce1ff6ed-3d11-4c32-9052-18af63cbcca6').
narrative_ontology:cs_kernel_codification('ce1ff6ed-3d11-4c32-9052-18af63cbcca6', formalized).
narrative_ontology:cs_authority_grounding('ce1ff6ed-3d11-4c32-9052-18af63cbcca6', lineage).
narrative_ontology:cs_interpretation_layer_present('ce1ff6ed-3d11-4c32-9052-18af63cbcca6').
narrative_ontology:cs_reading_relation('ce1ff6ed-3d11-4c32-9052-18af63cbcca6', state_killing_legitimacy__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('ce1ff6ed-3d11-4c32-9052-18af63cbcca6', state_killing_legitimacy__deterrence_reading, forecloses).
narrative_ontology:cs_axiom('ce1ff6ed-3d11-4c32-9052-18af63cbcca6', foundational, life_right_is_inalienable).
narrative_ontology:cs_axiom_status(life_right_is_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('ce1ff6ed-3d11-4c32-9052-18af63cbcca6', life_right_is_inalienable, deontological).
narrative_ontology:cs_axiom('ce1ff6ed-3d11-4c32-9052-18af63cbcca6', secondary, balancing_frameworks_erode_into_exceptions).
narrative_ontology:cs_axiom_status(balancing_frameworks_erode_into_exceptions, holdable).
narrative_ontology:cs_axiom_grounding('ce1ff6ed-3d11-4c32-9052-18af63cbcca6', balancing_frameworks_erode_into_exceptions, empirically_contingent).
narrative_ontology:cs_reference_frame('ce1ff6ed-3d11-4c32-9052-18af63cbcca6', categorical_dignity_inviolability).
narrative_ontology:cs_drift_state('ce1ff6ed-3d11-4c32-9052-18af63cbcca6', contemporary_retentionist_cores, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ce1ff6ed-3d11-4c32-9052-18af63cbcca6', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__abolition_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, condemned_persons).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, persons_at_capital_risk).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, state_killing_power).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, retentionist_legislatures).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, capital_prosecution_offices).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sentenced to death under a capital statute, or living under a death sentence commuted by abolition. What flows to them is the securing of their life-right: where the prohibition holds, no court, governor, or legislature can carry the sentence out. They hold no leverage of their own — the protection arrives entirely through courts, treaties, and constitutional text acting above them. Exit from custody is not available; what they receive is the guarantee that custody will not end in execution.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, condemned_persons, beneficiary,
    powerless, biographical, trapped, national).

% Defendants and suspects whose alleged conduct could, in a retaining jurisdiction, expose them to a capital charge. They benefit preventively: the worst case available to a prosecutor against them is imprisonment, not death. Most never learn the protection applied to them.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, persons_at_capital_risk, beneficiary,
    powerless, biographical, constrained, national).

% The sovereign capacity to execute convicted persons as punishment, wielded by legislatures that authorize it, prosecutors who seek it, and corrections agencies that carry it out. Where the prohibition holds, this capacity is struck from the penal toolkit entirely: no balancing test, no exception clause, no compensation. Its bearers respond by substituting — life without parole, transfer of prisoners to retaining jurisdictions, lethal force outside the criminal process. The capacity survives intact in retaining states and migrates rather than disappearing.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, state_killing_power, payer,
    institutional, civilizational, arbitrage, global).

% Constitutional and supreme courts that have struck down capital statutes or barred their restoration, and that must keep striking down reinstatement bills as they recur. They administer the prohibition case by case; their doctrine deepens with each defense. They could in principle reverse course, at the cost of overturning decades of settled precedent and their own accumulated commitments.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, constitutional_abolition_courts, agenda_setter,
    institutional, generational, constrained, national).

% Treaty monitoring bodies and regional institutions operating additional protocols that make abolition a condition of membership and accession. They review state reports, press retaining states through periodic review, and wield reputational and structural leverage: adverse findings, accession vetoes, exclusion from clubs.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, human_rights_treaty_bodies, agenda_setter,
    institutional, generational, constrained, continental).

% Legislative majorities that maintain or would restore capital statutes. Where the prohibition already binds them, they have lost a penal option their constituents periodically demand, and they test its edges with reinstatement bills, referendum proposals, and treaty-denunciation threats. Where it does not yet bind them, they are the expansion frontier the prohibition's advocates target — present in the dispute, absent from the settlements that bind others.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, retentionist_legislatures, payer,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__abolition_reading, retentionist_legislatures, excluded).

% Prosecutorial offices that formerly used the capital charge as their maximum sanction and their strongest plea-bargaining lever. Under the prohibition they lose that lever and rebuild around stacked life terms and extended minimum sentences; some redirect resources toward the aggravated circumstances that once fed capital cases.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, capital_prosecution_offices, payer,
    organized, generational, constrained, national).

% Families of homicide victims, and their advocacy organizations, a substantial share of whom demand execution as the proportionate response to killing. The categorical form of the prohibition refuses to weigh their claim — it holds regardless of desert — so their preferred remedy has no standing inside the settlement, and their objection registers only as outside political pressure.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, crime_victims_families, excluded,
    organized, biographical, constrained, national).

% International and domestic non-governmental organizations that ran the decades-long campaign: documenting executions, defending capital defendants, drafting protocol language, and conditioning memberships and aid on abolition. Their organizational identity fused with the cause generations ago; the campaign is what the institutions are.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, abolition_advocacy_movements, agenda_setter,
    organized, generational, identity_locked, global).

% Scholars of criminal law and political philosophy who hold the full structure in view: the kernel dispute among desert, deterrence, and dignity readings; the migration of killing power into substitutes; the counter-majoritarian character of judicial abolition. They publish the critiques each side deploys against the others.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, human_rights_philosophers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__abolition_reading, diffuse).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__abolition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the credible-commitment problem over state lethal power: citizens and defendants cannot verify case-by-case that the state will restrain its capacity to kill them, and balancing tests leave that discretion permanently open. The categorical rule removes the discretion outright and harmonizes jurisdictions on a single unambiguous standard — no race to the bottom in penal severity, no erosion through accumulated exceptions.
% TRANSFER_FUNCTION: Moves the option of lethal punishment out of the state's penal toolkit and converts it into a secured life-right for condemned persons and capital-risk defendants; incidentally it extinguishes, rather than redistributes, the remedy that crime-victims' families demanding execution would otherwise claim.
% ABSENT_VOICES: Crime-victims' families demanding execution as proportionate desert: the categorical form refuses their claim by design ('regardless of desert'), so they sit outside the settlement their loss animates. Also retentionist electorates in jurisdictions where courts abolished ahead of legislative or popular preference — bound by a settlement they did not consent to. Both voices register only as external political pressure that the enforcement structure is built to override.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would reopen capital statutes in most abolitionist jurisdictions within a legislative session — moratoria lapse, reinstatement bills pass where majorities favor them, and condemned persons under commuted sentences face renewed exposure where retroactivity permits. The treaty architecture (additional protocols, membership conditions) loses a load-bearing member, and state-citizen relations around lethal power revert to discretionary balancing.
% FOUNDING_PROBLEM: A state lethal power that will be misapplied: documented wrongful executions, class and racial disparity in application, use of execution as an instrument of political terror, and the recognition that no judicial procedure reliably separates the deserving from the undeserving killed. The prohibition was built to remove the power rather than regulate it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: death-row exoneration registries maintained by non-governmental projects document continuing releases of innocent condemned persons; governors and ministries in retaining jurisdictions have suspended executions citing integrity failures — admissions made by the arrangement's own operators; and criminological studies of application disparity are produced largely within retaining jurisdictions. No source outside the benefiting parties attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__abolition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__abolition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_legitimacy__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__abolition_reading, 0.95, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.95 with the referent fixed by the kernel-reading rule: the standing state-killing arrangement, assessed by this reading's own lights — never the prohibition the reading endorses. Because the reading holds the life-right inalienable, every execution the arrangement performs is an uncompensated taking of the maximal good, and the reading admits no desert or utility offset; the accumulating exoneration record (documented executions of the innocent) deepens rather than tempers the assessment across the interval, driving the mild upward series. Suppression (0.58) is the active enforcement the prohibition requires — courts striking recurring reinstatement bills, treaty bodies pressing retaining states — structural in mechanism, protective in aim. Theater (0.28) reflects a mostly functional operation (statutes repealed, sentences barred) with a growing ceremonial share: protocol signings without ratification, abolition commemorated alongside prisoner transfers to retaining jurisdictions. Accessibility collapse (0.80) is high because the categorical form collapses the alternative space wherever its premise is accepted — no safeguarded-retention middle position survives the premise — while stopping short of natural-law completeness because a large fraction of states demonstrably retain the practice. Resistance (0.65) is sustained: reinstatement campaigns, referendum proposals, sibling philosophical traditions, and majorities in several jurisdictions. The temporal series share one grid (1977–2025, eight-year spacing) so every tracked metric is authored at every examined point; the scalar base_properties values equal the 2025 endpoints.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by construction. From the condemned-person seat the prohibition is pure protection — the engine should find subsidy-directionality there. From the killing-power seat the same instrument reads as confiscation: a categorical stripping of a sovereign capacity with no compensation and no balancing, which is why that seat's computed type should diverge sharply from the condemned seat's. The courts and treaty bodies experience administration and doctrinal deepening rather than either protection or loss. Crime-victims'-families experience a foreclosed remedy — the one constituency whose objection the categorical form refuses by design. The authored claim (rope) sits with the protected and coordinating seats; the divergence across seats is the measurement the corpus exists to take, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real protection: condemned persons (d near 0 — the constraint exists to secure their life-right) and capital-risk defendants (preventive beneficiaries). The victim declaration maps the delta's inversion: the state killing power bears the constraint's incidence — it is what the prohibition removes. No directionality_overrides are authored, deliberately: the override surface is keyed by power atom, and this story's institutional class mixes the target (the killing power, true d approximately 0.9) with its own administrators (courts and treaty bodies, d far lower), so a blanket institutional override would corrupt the administrator seats. The victim declaration plus the killing power's arbitrage modulation approximates its position adequately; the residual imprecision is noted here rather than forced through a blunt override. On the receipt surface: gain_flow is affirmatively 'diffuse' after checking every named seat — the stripped killing capacity is destroyed rather than transferred to any capturer; condemned persons receive the protection the destruction produces but receive no captured share of the extracted capacity itself, and no seat converts the prohibition into private advantage. fixing_cost is 'prohibitive' for the binding core of the constraint: where the prohibition is entrenched (constitutional eternity clauses, protocol membership), removal requires treaty denunciation and constitutional amendment whose cost exceeds the benefit to its opponents; statutory-only abolition jurisdictions are cheaper to reverse, and the authored value reflects the entrenched layer where the constraint actually binds against political will.
 *
 * MANDATROPHY ANALYSIS:
 *   The prohibition's founding problem — a lethal power that will be misapplied because procedure cannot reliably separate the deserving from the undeserving killed — is more live now than at founding: the exoneration record hardened over the interval. Mandatrophy is therefore unresolved, and the classification guards against two mislabels. First, reading the prohibition as a snare because its heaviest computed extraction lands on the killing-power seat: the 'extraction' there is the removal of an illegitimate capacity, destroyed rather than captured — no seat receives it, which is why gain_flow is affirmatively diffuse rather than naming a capturing seat. Second, reading it as a piton because removal is prohibitively costly for its administrators: the cost-asymmetry test fails, because the courts maintaining the prohibition bear real counter-pressure and maintain it because its function is demanded by its beneficiaries, not from inertia — theater is low, the founding problem is corroborated from outside the beneficiary set, and the arrangement's disappearance would visibly rearrange the world.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the state_killing_legitimacy kernel (the abolition reading); what would the sibling readings — retributive_reading (life-right forfeited through proportional desert) and deterrence_reading (execution justified as a rational signal preventing future murders) — change structurally?',
    'Compare the three family stories'' victim sets and epsilon values over the shared referent (the standing state-killing arrangement); adopt a sibling''s axiom set and recompute the classification.',
    'Retributive adoption flips the condemned person from protected beneficiary to deservedly-targeted payer and drops epsilon sharply; deterrence adoption makes epsilon contingent on measured signal value. The disagreement is located in the alienability of the life-right: forfeitable by desert, weighable by utility, or inalienable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story instantiates one of three readings of a single contested kernel.').

omega_variable(
    categorical_form_necessity,
    'Is the categorical form (no exceptions regardless of desert or utility) load-bearing for the prohibition''s credible-commitment function, or would a near-absolute rule with narrowly carved-out exceptions preserve it?',
    'Comparative jurisprudence across jurisdictions with and without exception clauses; erosion trajectories of safeguarded-retention regimes that admitted exceptions.',
    'If narrow exceptions preserve the commitment, the constraint is a coordination device with carve-outs and the authored accessibility_collapse is overstated; if any exception licenses the next (the slippery-slope dynamic the categorical form was built to preclude), the categorical rigidity is the constraint''s core and the current authoring stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_form_necessity, empirical, 'Whether categorical rigidity is functional or overbroad.').

omega_variable(
    killing_power_migration,
    'Does the prohibition destroy state killing or migrate it into substitutes — life-without-parole as deferred death, extradition of prisoners to retaining jurisdictions, lethal force exercised outside the criminal process?',
    'Mortality cohort studies of life-without-parole populations against executed-counterfactual baselines; audits of prisoner-transfer flows and out-of-process lethal incidents in abolitionist jurisdictions.',
    'If the killing power migrates substantially, part of the prohibition''s achievement is displacement rather than elimination, the reading''s assessment of the standing arrangement partially rebounds onto its own substitutes, and the killing-power seat''s arbitrage mobility is confirmed as substantive evasion capacity rather than nominal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(killing_power_migration, empirical, 'Substitution versus elimination of the state''s killing capacity.').

omega_variable(
    counter_majoritarian_durability,
    'Where the prohibition binds against persistent majoritarian preference, does its durability depend on insulated enforcement (constitutional courts, treaty locks) that democratic backlash could unwind?',
    'Track reinstatement-attempt outcomes by entrenchment type — constitutional eternity clauses and protocol membership versus statutory repeal — across successive electoral cycles.',
    'If insulation fails where retentionist preference is strong, the prohibition''s classification in those jurisdictions drifts toward a transitional reading — holding until politics converges rather than as a settled commitment — and the suppression series would date an enforcement-decay phase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_majoritarian_durability, empirical, 'Durability of judicially-imposed abolition against electoral reversal.').

omega_variable(
    dignity_premise_status,
    'Is the dignity premise a discovered moral invariant (which would push the constraint toward natural-law presentation) or a constructed commitment maintained by institutions?',
    'Cross-cultural convergence analysis of dignity-based limits on state killing; observe whether the premise holds its shape where enforcement infrastructure is absent.',
    'A natural-law reading would pair the constraint''s beneficiary declarations with a false-summit evaluation; the constructed reading supports the rope claim authored here and locates the constraint''s persistence in courts, treaties, and advocacy rather than in self-enforcing moral fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_premise_status, conceptual, 'Naturality ambiguity of the dignity premise underlying the categorical prohibition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__abolition_reading, 1977, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(skila_tr_t1977, state_killing_legitimacy__abolition_reading, theater_ratio, 1977, 0.12).
narrative_ontology:measurement_basis(skila_tr_t1977, observed).
narrative_ontology:measurement(skila_tr_t1985, state_killing_legitimacy__abolition_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement_basis(skila_tr_t1985, observed).
narrative_ontology:measurement(skila_tr_t1993, state_killing_legitimacy__abolition_reading, theater_ratio, 1993, 0.18).
narrative_ontology:measurement_basis(skila_tr_t1993, observed).
narrative_ontology:measurement(skila_tr_t2001, state_killing_legitimacy__abolition_reading, theater_ratio, 2001, 0.21).
narrative_ontology:measurement_basis(skila_tr_t2001, observed).
narrative_ontology:measurement(skila_tr_t2009, state_killing_legitimacy__abolition_reading, theater_ratio, 2009, 0.24).
narrative_ontology:measurement_basis(skila_tr_t2009, observed).
narrative_ontology:measurement(skila_tr_t2017, state_killing_legitimacy__abolition_reading, theater_ratio, 2017, 0.26).
narrative_ontology:measurement_basis(skila_tr_t2017, observed).
narrative_ontology:measurement(skila_tr_t2025, state_killing_legitimacy__abolition_reading, theater_ratio, 2025, 0.28).
narrative_ontology:measurement_basis(skila_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(skila_be_t1977, state_killing_legitimacy__abolition_reading, base_extractiveness, 1977, 0.88).
narrative_ontology:measurement_basis(skila_be_t1977, observed).
narrative_ontology:measurement(skila_be_t1985, state_killing_legitimacy__abolition_reading, base_extractiveness, 1985, 0.89).
narrative_ontology:measurement_basis(skila_be_t1985, observed).
narrative_ontology:measurement(skila_be_t1993, state_killing_legitimacy__abolition_reading, base_extractiveness, 1993, 0.91).
narrative_ontology:measurement_basis(skila_be_t1993, observed).
narrative_ontology:measurement(skila_be_t2001, state_killing_legitimacy__abolition_reading, base_extractiveness, 2001, 0.92).
narrative_ontology:measurement_basis(skila_be_t2001, observed).
narrative_ontology:measurement(skila_be_t2009, state_killing_legitimacy__abolition_reading, base_extractiveness, 2009, 0.93).
narrative_ontology:measurement_basis(skila_be_t2009, observed).
narrative_ontology:measurement(skila_be_t2017, state_killing_legitimacy__abolition_reading, base_extractiveness, 2017, 0.94).
narrative_ontology:measurement_basis(skila_be_t2017, observed).
narrative_ontology:measurement(skila_be_t2025, state_killing_legitimacy__abolition_reading, base_extractiveness, 2025, 0.95).
narrative_ontology:measurement_basis(skila_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(skila_su_t1977, state_killing_legitimacy__abolition_reading, suppression_requirement, 1977, 0.35).
narrative_ontology:measurement_basis(skila_su_t1977, observed).
narrative_ontology:measurement(skila_su_t1985, state_killing_legitimacy__abolition_reading, suppression_requirement, 1985, 0.4).
narrative_ontology:measurement_basis(skila_su_t1985, observed).
narrative_ontology:measurement(skila_su_t1993, state_killing_legitimacy__abolition_reading, suppression_requirement, 1993, 0.46).
narrative_ontology:measurement_basis(skila_su_t1993, observed).
narrative_ontology:measurement(skila_su_t2001, state_killing_legitimacy__abolition_reading, suppression_requirement, 2001, 0.5).
narrative_ontology:measurement_basis(skila_su_t2001, observed).
narrative_ontology:measurement(skila_su_t2009, state_killing_legitimacy__abolition_reading, suppression_requirement, 2009, 0.53).
narrative_ontology:measurement_basis(skila_su_t2009, observed).
narrative_ontology:measurement(skila_su_t2017, state_killing_legitimacy__abolition_reading, suppression_requirement, 2017, 0.56).
narrative_ontology:measurement_basis(skila_su_t2017, observed).
narrative_ontology:measurement(skila_su_t2025, state_killing_legitimacy__abolition_reading, suppression_requirement, 2025, 0.58).
narrative_ontology:measurement_basis(skila_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__abolition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__deterrence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'capital punishment debate' decomposes into three structurally distinct constraints instantiating one kernel (state_killing_legitimacy). All three stories share a single epsilon referent — the standing state-killing arrangement — and differ in reading-indexed assessment: this abolition reading authors epsilon near the maximum because its categorical form admits no offsetting consideration; the retributive reading counts desert-satisfaction as compensation; the deterrence reading counts signal value. Upstream/downstream: the retributive reading is the historically upstream lineage (lex talionis), the deterrence reading the mid-century consequentialist layer, and this reading the post-war rights-layer that contests both; each sibling story links back here via its own network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
