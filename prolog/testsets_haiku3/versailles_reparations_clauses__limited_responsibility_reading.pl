% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__limited_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__limited_responsibility_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: versailles_reparations_clauses__limited_responsibility_reading
 *   human_readable: Versailles Reparations: Limited Responsibility Reading
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   The Versailles Treaty imposes reparations on Germany as the defeated
 *   power in World War I. The limited-responsibility reading interprets the
 *   treaty's Article 231 (Germany's 'responsibility' for the war) as a legal
 *   formality enabling debt collection, NOT as a moral judgment grounding
 *   unlimited liability. Under this reading, reparations must be calibrated
 *   to German economic capacity; payment schedules are revised downward from
 *   the original (crushing) Dawes Plan through negotiated deferrals and the
 *   Young Plan. German elites gain negotiating leverage; Allied creditors
 *   accept reduced recovery; working classes bear the fiscal burden. The
 *   constraint exhibits cyclical dynamics: 1920–1924 saw maximum suppression
 *   (occupation, inflation crisis, failed collection); the mid-1920s saw
 *   normalization under the Dawes Plan and German economic recovery;
 *   1928–1929 saw the constraint operating at minimal theater (routine
 *   payment); 1932 onward saw renewed suppression as the Great Depression
 *   triggered default threats and renegotiation pressure.
 *
 * KEY AGENTS:
 *   - German Weimar government: formal obligor; benefits from downward revision; constrained exit
 *   - German economic elites: beneficiaries of extended timelines and capacity-based moderation; political constituency for negotiated reparations
 *   - Allied creditors (Britain, France, Belgium): victims of the capacity principle; extract reduced compensation; powerful but constrained by threat of German default
 *   - League of Nations Reparations Commission: agenda-setter; administers payment schedules and capacity assessments
 *   - Working classes (all nations): bear fiscal extraction as taxes and inflation; powerless to exit
 *   - Punitive-liability advocates: excluded; their core premise (moral guilt → unlimited liability) is foreclosed by this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, 0.58).
domain_priors:suppression_score(versailles_reparations_clauses__limited_responsibility_reading, 0.42).
domain_priors:theater_ratio(versailles_reparations_clauses__limited_responsibility_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__limited_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__limited_responsibility_reading, "Versailles Reparations: Limited Responsibility Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__limited_responsibility_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__limited_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__limited_responsibility_reading, 'fd0c12ff-516a-4423-9618-8724ffcbd39d').
narrative_ontology:cs_kernel_codification('fd0c12ff-516a-4423-9618-8724ffcbd39d', formalized).
narrative_ontology:cs_authority_grounding('fd0c12ff-516a-4423-9618-8724ffcbd39d', extraction).
narrative_ontology:cs_interpretation_layer_present('fd0c12ff-516a-4423-9618-8724ffcbd39d').
narrative_ontology:cs_reading_relation('fd0c12ff-516a-4423-9618-8724ffcbd39d', versailles_reparations_clauses__punitive_liability_reading, forecloses).
narrative_ontology:cs_reading_relation('fd0c12ff-516a-4423-9618-8724ffcbd39d', versailles_reparations_clauses__repudiation_reading, coexists_with).
narrative_ontology:cs_axiom('fd0c12ff-516a-4423-9618-8724ffcbd39d', foundational, article_231_legal_formalism).
narrative_ontology:cs_axiom_status(article_231_legal_formalism, holdable).
narrative_ontology:cs_axiom_grounding('fd0c12ff-516a-4423-9618-8724ffcbd39d', article_231_legal_formalism, conventional).
narrative_ontology:cs_axiom('fd0c12ff-516a-4423-9618-8724ffcbd39d', foundational, capacity_bounded_extraction).
narrative_ontology:cs_axiom_status(capacity_bounded_extraction, holdable).
narrative_ontology:cs_axiom_grounding('fd0c12ff-516a-4423-9618-8724ffcbd39d', capacity_bounded_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('fd0c12ff-516a-4423-9618-8724ffcbd39d', capacity_moderated_reparations).
narrative_ontology:cs_drift_state('fd0c12ff-516a-4423-9618-8724ffcbd39d', great_depression_onset_1929, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fd0c12ff-516a-4423-9618-8724ffcbd39d', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_economic_viability_constituency).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, moderate_allied_creditors).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, allied_creditors_denied_full_recovery).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, occupied_territories_reduced_compensation).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, german_working_classes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_economic_elites).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, german_weimar_government).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_governments).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, occupied_territories_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legally obligated to pay reparations under the Treaty of Versailles. Under the limited-responsibility reading, the government argues that payments must be scaled to Germany's post-war productive capacity rather than to the full cost of Allied war effort. It negotiates downward revisions of payment schedules (Dawes Plan in 1924, Young Plan in 1929) to stretch obligations across decades. Exit options are constrained: formal treaty repudiation risks Allied military response and international isolation; unilateral default risks occupation or asset seizure. The government accepts the capacity-based framing as preferable to the punitive alternative.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_weimar_government, payer,
    moderate, biographical, constrained, national).

% Gain significant advantage from the capacity-based reading and downward payment revisions. Lower annual reparations payments preserve capital accumulation, enable industrial reconstruction, and support export recovery. They frame the constraint as mutually beneficial: a solvent Germany with growing tax base can pay reparations more reliably over time than an impoverished, unstable Germany. They benefit from negotiating leverage; they can credibly threaten economic collapse or political instability if payments are raised. Their exit options include capital flight and political repositioning; the limited-responsibility reading improves their material position and political standing.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_economic_elites, beneficiary,
    organized, generational, mobile, national).

% Become partial victims of the capacity-based reading: they recover less than they demanded under the original Treaty. They wanted full compensation for war costs but accept moderated payments as politically sustainable (total impoverishment of Germany would trigger revolution, communist takeover, or renewed military threat, all worse for Allied interests). They bear enforcement costs: maintaining occupation, supervising League oversight, negotiating deferrals. Their exit options are limited: they can write off losses (acknowledge non-recovery) or escalate military enforcement (occupy, extract directly), both costly and politically unpopular domestically.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_governments, payer,
    powerful, biographical, constrained, global).

% Belgium, France, Poland, and others with territorial damage are reduced creditors under the capacity principle. Their compensation for occupation costs and reconstruction needs is lower than their actual losses. They depend on the Allied great powers (Britain, USA) to enforce collection; they lack independent leverage. The capacity-based frame constrains their recovery relative to damage sustained.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, occupied_territories_governments, payer,
    moderate, biographical, constrained, regional).

% Bear the domestic cost of reparations through state taxes, inflation, wage suppression, and reduced social spending. They are framed as bearers of national responsibility (the constraint applies to Germany-as-a-unit; working classes pay through the fiscal system). Their exit is severely constrained: they cannot emigrate easily, and their identity as German citizens ties them to the state obligation. They experience the constraint primarily as suppression through fiscal extraction and monetary inflation that erodes their purchasing power.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_working_classes, payer,
    powerless, biographical, identity_locked, national).

% Administers the reparations regime and interprets the treaty. Under the limited-responsibility reading, the Commission oversees capacity assessments, approves payment schedules, negotiates deferrals, and certifies German economic viability. It sets the procedural framework for collection but lacks enforcement power on its own; it depends on Allied willingness to back it militarily if necessary. The Commission's authority rests on treaty interpretation (capacity principle) and technical expertise (economic assessment). Its role enables moderation by legitimating capacity-based downward revision.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, league_of_nations_reparations_commission, agenda_setter,
    institutional, generational, analytical, global).

% Allied hardliners (French nationalists, some British politicians, U.S. isolationists demanding full payment) are structurally excluded from the limited-responsibility reading's framework. They hold political power in some Allied governments but their core premise—that Article 231 grounds unlimited liability based on moral judgment—is foreclosed by this reading's axiom (Article 231 is legal formality, not moral foundation). Their only recourse is to repudiate the limited-responsibility frame entirely and argue for the punitive reading. They are trapped in opposition to the international consensus around the capacity principle.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, punitive_liability_advocates, excluded,
    powerful, biographical, trapped, global).

% German political forces arguing that the Treaty was imposed under duress and has no legitimacy are excluded from the limited-responsibility reading's framework. This includes radical nationalists, communists, and revanchists who would argue for complete repudiation of reparations, not negotiated reduction. Under the limited-responsibility reading, they are not at the negotiation table; they represent a structural alternative (repudiation reading) that this reading's legal-formalist framework does not accommodate.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_repudiationist_movement, excluded,
    moderate, biographical, constrained, national).

% Price German sovereign debt and allocate credit based on perceived payment capacity and sustainability. Their behavior validates or invalidates capacity claims through market signals: credit extended when reparations are revised downward (signal of sustainability); credit withdrawn when payment schedules are tightened (signal of default risk). They do not set policy but their pricing constrains what is politically feasible; they function as a distributed observer whose price signals feed back into negotiation dynamics.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, international_financial_markets, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_governments).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__limited_responsibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves a genuine coordination problem: how to extract war-debt repayment from a defeated, economically devastated nation in a way that (1) ensures actual payment rather than default or revolution, and (2) aligns incentives so the creditor powers support German economic recovery (which increases tax base for reparations extraction). The capacity-based reading coordinates around the proposition that a solvent Germany benefits all parties more than an impoverished one — a mutual-interest framing.
% TRANSFER_FUNCTION: Moves a portion of Germany's economic output to Allied creditors and occupied territories as war-debt repayment, scaled to German productive capacity rather than to the full cost of the Allied war effort. The transfer occurs through state-directed payments, fiscal extraction (taxation and monetary creation), and delayed schedule (repayment stretched across decades).
% ABSENT_VOICES: German repudiationists (who reject the treaty's legitimacy entirely) are excluded; their voice would argue the constraint has no binding force. Allied hardliners arguing for unlimited reparations based on moral guilt are excluded; their voice would argue for the punitive reading. Voices from occupied territories most harmed by German occupation are under-represented relative to their creditor claim; they lack the Allied great-power backing of France and Britain.
% DISAPPEARANCE_RATIONALE: If the limited-responsibility reading and its capacity-based payment regime vanished, either the punitive-liability reading would be adopted (unlimited reparations, tighter extraction) or the repudiation reading would prevail (Germany defaults, occupations resume or are formalized). In either case, the political economy of post-war Europe reorganizes: either Germany suffers impoverishment and instability, or Allied creditors accept large losses. The constraint structures the middle ground between these two poles.
% FOUNDING_PROBLEM: Post-war Europe faces the problem of war-debt: the Allied powers incurred enormous costs in defeating Germany; Germany is economically devastated and politically unstable. The founding problem is: how can war debts be collected from a defeated nation without destroying its economy (which would prevent repayment and trigger social upheaval) or bankrupting the creditor nations (which have no alternative funding source)? The limited-responsibility reading frames this as a problem of capacity and mutual interest, not of punishment or moral judgment.
% FOUNDING_PROBLEM_CORROBORATION: Economists and financial experts outside the Allied governments (John Maynard Keynes, Swedish economists, neutral Swiss analysts) attested that unlimited reparations would destroy German economy and trigger default or revolutionary upheaval. German government and business elites attest that capacity-based payments are the only sustainable path. Allied hardliners contest this, arguing that Germany should bear the full cost regardless of capacity. League of Nations officials attest that the capacity principle is necessary for enforceability.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__limited_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__limited_responsibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__limited_responsibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(versailles_reparations_clauses__limited_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__limited_responsibility_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.58 (interval endpoint) because the constraint does extract substantial value from Germany (state fiscal burden, working-class inflation tax, capital constraints), but the extraction is moderated by the capacity principle relative to the original Treaty intent. Suppression at 0.42 reflects that enforcement does not require constant coercion after the mid-1920s: once the Dawes Plan stabilizes German economy and payment schedule, suppression drops because the constraint becomes routine and German elites accept it as preferable to alternatives. Theater at 0.28 reflects that significant energy goes into legitimating capacity assessments (economic surveys, League reports) rather than purely functional payment; this theatrical component rises again after 1928 as renegotiation pressures mount. The measurement series track one shared grid (1920, 1924, 1928, 1932) across all three metrics, capturing the cyclical pattern: crisis suppression → normalization → stability → renewed pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the limited-responsibility reading's own seat, the constraint appears as sustainable coordination: capacity-based moderation enables long-term payment and preserves German economic recovery. From the punitive-liability reading's seat, the same constraint appears as inadequate extraction: Germany escapes the full moral and financial weight of responsibility. From the repudiation reading's seat, the constraint has no legitimacy at all. The engine computes per-seat types from this structural data; the authored claim (tangled_rope) represents the limited-responsibility reading's own classification of what it is doing (coordinating around mutual economic interest while asymmetrically extracting). A punitive-liability reader would classify the same facts as a snare (reduced extraction disguised as cooperation); a repudiationist would deny legitimacy entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   German government is a payer (high d, constrained exit, obligor status) but benefits from the moderation — a dual-positioned agent whose d sits near 0.65 (partial target, partial negotiator). German elites are pure beneficiaries (d ≈ 0.15, low extraction impact, high exit via capital mobility). Allied creditors are victims of the capacity principle (they wanted more) but also semi-beneficiaries of German economic recovery (which enables payment) — d ≈ 0.55 (symmetric, caught between extraction interests and economic interest). Working classes are pure targets (high d ≈ 0.85, identity-locked, bear tax burden). League of Nations as agenda-setter has low d (analytical seat, d ≈ 0.2) because it does not collect extraction but administers it. The capacity principle itself moderates effective extraction by constraining creditor claims downward.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to extract war debt from a devastated debtor without triggering default or revolution) remains live throughout the interval. The constraint prevents mandatrophy by maintaining the capacity principle: even under Great Depression pressure (1932 onward), the constraint persists because the alternative (unlimited reparations or formal default) is worse for all parties than continued moderated extraction. The constraint prevents pure snare classification by maintaining genuine coordination around mutual economic interest: German recovery enables payment; payment enables creditor recovery. But it prevents pure rope classification by maintaining asymmetric extraction: creditors never recover the full cost; German elites gain leverage; working classes bear burden. The tangled-rope classification holds: coordination function (mutual interest in German recovery) + asymmetric extraction (capacity principle reduces creditor recovery) + active enforcement (League oversight, occupation threat) = genuine entanglement of coordination and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_measurement_ambiguity,
    'How is German economic capacity actually measured and verified? Does the measurement shift to favor one reading or another depending on which economic indicators are chosen?',
    'Comparative analysis of League Reparations Commission capacity assessments (1920–1932) against independent economic historians'' reconstructions; examination of which indicators (GDP, tax revenue, export capacity, industrial production) were weighted and how weighting changed over time.',
    'If capacity measures were systematically biased downward to justify lower payments, the limited-responsibility reading becomes a cover story for German elite interests, not genuine coordination. If measures were reasonable, the reading stands. This affects whether the constraint is snare-adjacent or genuinely tangled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_measurement_ambiguity, empirical, 'Whether capacity assessments were structurally biased.').

omega_variable(
    article_231_interpretive_contest,
    'Is Article 231 genuinely open to interpretation as legal formality rather than moral judgment, or was the moral judgment interpretation the original drafting intent that the limited-responsibility reading strategically reframed?',
    'Examination of drafting history, contemporary legal commentary from 1919–1920, and the explicit language choices made by treaty framers. Comparison with how the clause has been used in subsequent international law (to determine if the limited-responsibility reading''s interpretation became canonical or remained contested).',
    'If Article 231 was drafted with moral-judgment intent and the capacity reading represents strategic reinterpretation, the limited-responsibility reading is a constructed framing favoring German negotiating position. If the legal-formality reading was plausible from the start, the reading reflects genuine interpretive ambiguity. This affects whether the constraint embodies false-summit dynamics (natural law vs. constructed extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_231_interpretive_contest, conceptual, 'Whether Article 231''s legal status was genuinely ambiguous or strategically reinterpreted.').

omega_variable(
    reading_foreclosure_empirical_test,
    'After 1933, when Germany repudiated reparations under Nazi leadership, did the Great Depression and global economic collapse make the punitive_liability_reading empirically impossible to execute (unlimited extraction from a currency-collapsed economy), thereby vindicating the limited-responsibility reading''s capacity principle?',
    'Counterfactual analysis: what would unlimited reparations extraction have looked like after 1929? Economic modeling of German capacity under depression conditions compared to historical outcome. Examination of whether punitive-reading advocates acknowledged the capacity constraint during the depression or continued to demand unlimited payments.',
    'If unlimited reparations became empirically impossible during depression, the limited-responsibility reading''s core premise (capacity must bound extraction) gains retrospective validation. This would strengthen the claim that the reading was genuine coordination logic, not just elite interest. Conversely, if punitive advocates persisted in demanding unlimited payments despite obvious economic impossibility, the distinction between the readings might be ideological rather than structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_empirical_test, empirical, 'Whether post-1929 economic collapse vindicated the capacity principle.').

omega_variable(
    committer_frame_kernel_vs_readings,
    'Does the Versailles Treaty represent a single contested kernel with multiple readings, or are punitive-liability and limited-responsibility different treaties (Versailles vs. Dawes/Young Plans) that happen to reference the same Article 231?',
    'Examination of whether the Dawes Plan and Young Plan are reinterpretations of the same Article 231 (readings of one kernel) or independent treaties that supersede Versailles. Analysis of legal citations and political discourse: do actors frame the plans as reinterpretations of Article 231 or as new agreements?',
    'If the plans are readings of Versailles, the limited-responsibility reading is a genuine hermeneutical position. If they are independent treaties, the limited-responsibility reading is a framing choice but the economic constraint structure remains novel (no kernel). This affects the committer-frame analysis and the engine''s kernel_recognition procedures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_kernel_vs_readings, conceptual, 'Whether reparations plans are readings of one kernel or independent constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__limited_responsibility_reading, 1920, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1920, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1920, 0.45).
narrative_ontology:measurement(vers_tr_t1924, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1924, 0.22).
narrative_ontology:measurement(vers_tr_t1928, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1928, 0.18).
narrative_ontology:measurement(vers_tr_t1932, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1932, 0.28).

% Extraction over time
narrative_ontology:measurement(vers_be_t1920, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1920, 0.72).
narrative_ontology:measurement(vers_be_t1924, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1924, 0.58).
narrative_ontology:measurement(vers_be_t1928, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1928, 0.48).
narrative_ontology:measurement(vers_be_t1932, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1932, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1920, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1920, 0.68).
narrative_ontology:measurement(vers_su_t1924, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1924, 0.45).
narrative_ontology:measurement(vers_su_t1928, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1928, 0.32).
narrative_ontology:measurement(vers_su_t1932, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1932, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__limited_responsibility_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(versailles_reparations_clauses__limited_responsibility_reading, 0.18).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__repudiation_reading).

% DUAL FORMULATION NOTE:
% The versailles_reparations_clauses kernel supports three distinct constraint stories representing three irreconcilable readings of the same treaty text. The limited_responsibility_reading (this story) interprets Article 231 as enabling capacity-bounded debt collection and modulates extraction downward through Dawes/Young plans. The punitive_liability_reading reads Article 231 as grounding unlimited liability and claims enforcement of maximal payments. The repudiation_reading denies the treaty's legitimacy entirely. These are not three perspectives on one constraint; they are three structurally distinct constraints instantiated by three different frameworks applied to the same kernel. They are linked here to enable cross-reading comparison and foreclosure analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(versailles_reparations_clauses__limited_responsibility_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
