% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__substantial_effects_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
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
 *   human_readable: Commerce Clause Substantial-Effects Reading with Jurisdictional Nexus and Non-Pretext Limits
 *   domain: constitutional_law/federalism/commerce_regulation
 *
 * SUMMARY:
 *   The substantial-effects reading of the Commerce Clause holds that
 *   Congress may regulate intrastate activity whose cumulative effect on
 *   interstate commerce is substantial, but only where a genuine
 *   jurisdictional nexus exists and the regulated object is genuinely
 *   economic rather than police-power business dressed in commerce language.
 *   It is the intermediate position on the federalism spectrum: broader than
 *   any cross-border-only conception of the power (home-consumed wheat is
 *   reachable), narrower than an aggregation-without-limits conception
 *   (school-zone gun possession and federally-created domestic-violence
 *   remedies are not). Its operative mechanism is category-boundary policing:
 *   the economic/non-economic line, the nexus test, and the anti-pretext
 *   requirement are the load-bearing walls, and their maintenance is judicial
 *   work. The arrangement coordinates national economic governance while
 *   conscripting private economic conduct into federal reach — a structure
 *   with both a real coordination function and identifiable parties who bear
 *   its costs without consenting to them. KEY AGENTS (by structural
 *   relationship): - congressional_legislative_coalitions: Agenda setter
 *   (institutional/arbitrage) — authors the statutes this reading empowers -
 *   federal_regulatory_agencies: Primary beneficiary and administrator
 *   (institutional/constrained) — converts statutes into enforcement,
 *   collects penalties and jurisdiction - intrastate_economic_actors: Primary
 *   target (powerless/trapped) — local economic conduct reached by
 *   aggregation - state_governments_displaced: Dual-positioned bearer and
 *   gainer (institutional/constrained) — loses economic fields, retains the
 *   non-economic zone - noneconomic_activity_holders: Shielded gainer
 *   (moderate/constrained) — protected only by continued judicial policing -
 *   interstate_commerce_operators: Secondary gainer (organized/mobile) —
 *   purchases uniformity - federal_courts: Analytical observer and doctrinal
 *   agenda setter (institutional/analytical)
 *
 * KEY AGENTS:
 *   - congressional_legislative_coalitions: Agenda setter (institutional power, arbitrage-grade exit) — authors statutes relying on the reading and reroutes around limitations
 *   - federal_regulatory_agencies: Primary beneficiary and administrator (institutional, constrained) — converts statutes into enforcement regimes, collects penalties and jurisdictional turf, bears the cost of proving nexus and economic character when challenged
 *   - intrastate_economic_actors: Primary target (powerless, trapped) — farmers, small producers, patient-cultivators whose local economic conduct aggregates into national reach
 *   - state_governments_displaced: Dual-positioned bearer and gainer (institutional, constrained) — loses preempted economic fields, retains the non-economic police-power zone
 *   - noneconomic_activity_holders: Shielded beneficiary (moderate, constrained) — protected from federal reach only so long as courts keep enforcing the economic-character requirement
 *   - interstate_commerce_operators: Secondary beneficiary (organized, mobile) — purchase uniform national rules displacing state patchworks
 *   - federal_courts: Analytical observer and doctrinal agenda setter (institutional, analytical) — adjudicate the line; collect no rent but author the operative doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, 0.6).
domain_priors:suppression_score(commerce_clause_text__substantial_effects_limited_reading, 0.46).
domain_priors:theater_ratio(commerce_clause_text__substantial_effects_limited_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0.46).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__substantial_effects_limited_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__substantial_effects_limited_reading, "Commerce Clause Substantial-Effects Reading with Jurisdictional Nexus and Non-Pretext Limits").
narrative_ontology:topic_domain(commerce_clause_text__substantial_effects_limited_reading, "constitutional_law/federalism/commerce_regulation").

domain_priors:requires_active_enforcement(commerce_clause_text__substantial_effects_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__substantial_effects_limited_reading, 'bc3e0972-7435-4de8-b81a-42ed998681f6').
narrative_ontology:cs_kernel_codification('bc3e0972-7435-4de8-b81a-42ed998681f6', fixed_text).
narrative_ontology:cs_authority_grounding('bc3e0972-7435-4de8-b81a-42ed998681f6', lineage).
narrative_ontology:cs_interpretation_layer_present('bc3e0972-7435-4de8-b81a-42ed998681f6').
narrative_ontology:cs_reading_relation('bc3e0972-7435-4de8-b81a-42ed998681f6', commerce_clause_text__expansive_federal_reading, forecloses).
narrative_ontology:cs_reading_relation('bc3e0972-7435-4de8-b81a-42ed998681f6', commerce_clause_text__originalist_narrow_reading, forecloses).
narrative_ontology:cs_axiom('bc3e0972-7435-4de8-b81a-42ed998681f6', foundational, categorical_noneconomic_exclusion_required).
narrative_ontology:cs_axiom_status(categorical_noneconomic_exclusion_required, holdable).
narrative_ontology:cs_axiom_grounding('bc3e0972-7435-4de8-b81a-42ed998681f6', categorical_noneconomic_exclusion_required, deontological).
narrative_ontology:cs_axiom('bc3e0972-7435-4de8-b81a-42ed998681f6', foundational, nexus_and_nonpretext_conditions_binding).
narrative_ontology:cs_axiom_status(nexus_and_nonpretext_conditions_binding, holdable).
narrative_ontology:cs_axiom_grounding('bc3e0972-7435-4de8-b81a-42ed998681f6', nexus_and_nonpretext_conditions_binding, conventional).
narrative_ontology:cs_axiom('bc3e0972-7435-4de8-b81a-42ed998681f6', secondary, aggregation_valid_for_economic_activity).
narrative_ontology:cs_axiom_status(aggregation_valid_for_economic_activity, holdable).
narrative_ontology:cs_axiom_grounding('bc3e0972-7435-4de8-b81a-42ed998681f6', aggregation_valid_for_economic_activity, empirically_contingent).
narrative_ontology:cs_reference_frame('bc3e0972-7435-4de8-b81a-42ed998681f6', substantial_effects_bounded_authority).
narrative_ontology:cs_drift_state('bc3e0972-7435-4de8-b81a-42ed998681f6', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('bc3e0972-7435-4de8-b81a-42ed998681f6', '2026-08-12T14:30:00Z').
narrative_ontology:cs_kernel_id(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, congressional_legislative_coalitions).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, federal_regulatory_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, interstate_commerce_operators).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, noneconomic_activity_holders).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, intrastate_economic_actors).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, state_governments_displaced).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, state_governments_displaced).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, substantial_effects_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, enumerated_powers_limitation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and passes statutes regulating national economic life, relying on this reading to reach conduct occurring wholly inside single states. When the courts police the boundary — striking a statute as non-economic or pretextual — the coalition's recourse is redrafting to fit the economic category or routing authority through alternate constitutional headings. Gains legislative jurisdiction; loses individual statutes occasionally.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, congressional_legislative_coalitions, agenda_setter,
    institutional, generational, arbitrage, national).

% Convert commerce-power statutes into operating enforcement regimes — crop acreage controls, controlled-substance scheduling, facility permitting — reaching production and possession that never crosses a state line. Collect civil penalties, fees, and jurisdictional ground with each upheld statute. When a challenge succeeds, must demonstrate a concrete jurisdictional nexus and the regulated activity's economic character, an administrative cost the boundary-policing imposes on them.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_regulatory_agencies, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__substantial_effects_limited_reading, federal_regulatory_agencies, agenda_setter).

% Grow, produce, or transact locally — wheat consumed on the farm, cannabis grown for personal medical use, goods sold within one state. None of it crosses state lines, but each act joins an aggregate national market effect, which is sufficient to bring federal regulation to the doorstep. Cannot exit federal jurisdiction by relocating inside the country, cannot scale up to a seat in the doctrine's authorship, and bears compliance and penalty exposure set far away.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, intrastate_economic_actors, payer,
    powerless, biographical, trapped, local).

% Regulate health, safety, morals, and non-economic conduct inside their borders — authority this reading expressly leaves intact — but watch economic fields with interstate effects migrate to the national government under preemption. Litigate the boundary line when it moves, experiment in the retained zone, and otherwise operate inside a federal scheme they cannot leave.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, state_governments_displaced, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__substantial_effects_limited_reading, state_governments_displaced, beneficiary).

% Persons whose conduct carries no economic character — possessing a firearm near a school, seeking a civil remedy for domestic violence, consuming what they grow. The categorical exclusion is their shield: as long as courts hold that non-economic activity stays beyond the commerce power regardless of aggregate effects, federal regulation cannot reach them. They hold no independent lever; the shield exists only at the courts' pleasure.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, noneconomic_activity_holders, beneficiary,
    moderate, biographical, constrained, local).

% Firms shipping, selling, or financing across state lines. Uniform national rules displace fifty-state patchworks and lower the cost of operating at scale, so they support the reading's maintenance where it preempts stricter state regimes. Compliance costs are real but routinely passed through prices; relocation and restructuring keep their exit open.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, interstate_commerce_operators, beneficiary,
    organized, biographical, mobile, continental).

% Adjudicate whether activity is economic, whether a nexus exists, and whether a statute's commerce framing is pretext. Author the operative doctrine case by case; the limits are real exactly as long as the judgments keep coming. Collect no material benefit from the arrangement and bear its costs only as institutional workload.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_courts, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__substantial_effects_limited_reading, federal_courts, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__substantial_effects_limited_reading, federal_regulatory_agencies).
narrative_ontology:fixing_cost_class(commerce_clause_text__substantial_effects_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of regulating national markets composed of intrastate activity: uniform federal rules prevent interstate externalities and races to the bottom among states competing for investment, while the categorical carve-out preserves state authority over non-economic life so federal economic governance does not collapse into general government.
% TRANSFER_FUNCTION: Moves regulatory authority and compliance burden from state governments and intrastate economic actors to federal legislative and administrative institutions for economic conduct; moves immunity-from-federal-reach to holders of non-economic activity; moves litigation costs to whoever contests the boundary.
% ABSENT_VOICES: Intrastate economic actors themselves — the home-consuming farmer, the patient cultivator — had no seat when the doctrine's scope was authored; state reserved-powers advocates and would-be state innovators in preempted economic fields enter the conversation only as litigants after the line has moved.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the federal-state authority allocation would reorganize around one of the rival allocations: under a cross-border-only allocation, whole federal regulatory fields — agricultural quotas, controlled substances, permitting of stationary facilities — would lose their constitutional foundation and revert to the states; under an aggregation-without-limits allocation, the remaining non-economic enclaves would fall to federal reach. Either way the map of who may regulate what rearranges.
% FOUNDING_PROBLEM: Whether a national government of enumerated powers could regulate intrastate economic conduct whose aggregate effect is national — the surplus-agriculture and integrated-industrial-market crisis of the 1930s — while preventing the same logic from converting the commerce power into an unlimited federal police power.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: successive court majorities writing in opposing directions — the 1942 expansion, the 1995 and 2000 limitations, the 2005 confirmation of the economic core — attest the problem's persistence from the adjudicating seat rather than the collecting one; state attorneys general filing boundary challenges and the academic federalism literature document that the economic/non-economic line remains unsettled. No attestation comes only from federal institutions that gain jurisdiction from the arrangement.
narrative_ontology:disappearance_verdict(commerce_clause_text__substantial_effects_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__substantial_effects_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__substantial_effects_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_text__substantial_effects_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__substantial_effects_limited_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is authored at 0.60: after the medical-cultivation holding, reach into intrastate economic life is broad — the nexus condition was satisfied for cannabis grown for personal use, and aggregation sweeps nearly any economic conduct into federal grasp — while the categorical exclusion of non-economic activity keeps a real domain off the bearing surface entirely. Suppression is authored at 0.46 as a raw structural property (unscaled by power or scope in the engine's arithmetic): federal supremacy closes the exit of states and individuals from the scheme, but judicial review keeps a live, occasionally winning alternative channel, so closure is partial. Theater_ratio 0.35 reflects the post-1995 condition: boundary-policing became real again after six dormant decades, but strikes remain rare enough that a performative residue persists. Accessibility_collapse 0.42: alternatives do not fully collapse — states regulate freely in the carved-out non-economic zone and constitutional challenges sometimes succeed. Resistance 0.55: states litigate, Congress re-expands after each limitation, and the scholarly and political contest over the line is continuous. All three temporal series are authored on one shared eight-point grid (1942-2026); the base_properties values are the interval-end measurements. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: the judicial machinery needed to hold the boundary was nearly idle at mid-century and has been ratcheting upward since 1995.
 *
 * PERSPECTIVAL GAP:
 *   From the congressional and agency seats the reading is the enabling charter of national economic governance — the same structure that reaches the home-consuming farmer also delivers the uniform market in which every national firm operates. From the trapped intrastate actor's seat the identical structure is conscription of private conduct into federal jurisdiction without consent and without exit. From the state seat it is a property line that has moved twice in living memory — outward for five decades, then partially back. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for congressional_legislative_coalitions, federal_regulatory_agencies, interstate_commerce_operators, and noneconomic_activity_holders — the first two author and administer the arrangement, the third purchases uniformity, the fourth is shielded by the categorical exclusion. intrastate_economic_actors carry the transfer with trapped exit, placing them near the full-target end where effective extraction is amplified. state_governments_displaced are genuinely dual-positioned — displaced in economic fields, sovereign in the carved-out zone — and should compute near-symmetric. federal_courts occupy the analytical seat: they collect no material rent, but their opinions are the mechanism that makes the limits real or lets them lapse, which is why the enforcement-intensity series tracks judicial behavior rather than agency behavior. No directionality overrides are authored: the override mechanism keys on power atoms, and an institutional-level override would flatten genuinely different institutional seats (author, administrator, displaced state, court) onto one value — the role declarations already differentiate them more precisely than a power-atom-wide correction could.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling national market regulation with enumerated-power limits — is live, so no resolved-mandatrophy flag is authored. The 1937-1995 dormancy episode is the instructive near-miss: for six decades the limit function atrophied into rhetoric (theater_ratio peaking at 0.70 in 1995) while the reach function persisted unopposed — the shape this arrangement takes if the revival fails. Tracking theater_ratio and enforcement intensity separately is what distinguishes a functioning hybrid from a limit in name only; if the categorical exclusion decays again while the reach persists, operation drifts toward pure conscription for whatever class the boundary next abandons. The classification also guards the reverse error: because the coordination function is real (uniform national market rules, preserved state police power over non-economic life), reading the arrangement as pure extraction would erase the genuine protection the carve-out delivers to non-economic actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_category_boundary_stability,
    'Is the economic/non-economic boundary a stable structural line or a discretionary instrument applied by result?',
    'Longitudinal coding of Commerce Clause and enumerated-powers cases: classify each regulated activity independently of outcome, then test whether economic-character findings predict results after controlling for statutory subject matter.',
    'If the line is discretionary, boundary-policing operates as selective enforcement — costs concentrate on politically disfavored classes and the arrangement drifts toward pure conscription for targeted groups despite its coordination core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_category_boundary_stability, conceptual, 'Stability of the economic/non-economic category line under adjudication.').

omega_variable(
    reading_drift_to_unbounded_operation,
    'Will the post-1995 revival of categorical limits hold, or will operation collapse back into aggregation-without-limits as it did between 1937 and 1995?',
    'Track strike-down frequency, aggregation holdings that absorb non-economic conduct, and congressional drafting behavior over coming decades; sustained non-economic exclusions indicate the revival holds.',
    'If collapse recurs, measured extraction converges toward the unbounded allocation''s level and the noneconomic_activity_holders'' protection becomes purely nominal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_drift_to_unbounded_operation, empirical, 'Persistence of the revived limit against expansive drift pressure.').

omega_variable(
    kernel_reading_contest_structure,
    'This constraint is one reading of the commerce_clause_text kernel — what would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Comparative classification of the sibling stories: the cross-border-only sibling removes intrastate_economic_actors from the bearing pool entirely; the aggregation-without-limits sibling dissolves noneconomic_activity_holders'' protection; the disagreement is located in whether categorical limits attach to the enumerated power.',
    'Seat classifications, bearing sets, and epsilon differ across siblings; cross-reading comparison is valid only story-by-story, never averaged inside one constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Committer structure: this story instantiates the bounded-aggregation reading of a three-way kernel contest.').

omega_variable(
    nexus_requirement_triviality,
    'Does the jurisdictional-nexus requirement actually exclude any economically characterized activity, or is it satisfiable whenever Congress declares the activity economic?',
    'Inventory of post-1942 federal statutes enjoined for nexus failure versus upheld despite remote nexus; count cases where the nexus condition alone decided the outcome.',
    'If nexus is trivially satisfiable, one of the reading''s two binding conditions is decorative — effective extraction rises toward the unbounded allocation''s level and the authored theater_ratio understates the decoration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nexus_requirement_triviality, empirical, 'Whether the nexus condition binds or merely decorates the aggregation mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__substantial_effects_limited_reading, 1942, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1942, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1942, 0.3).
narrative_ontology:measurement(comm_tr_t1955, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1955, 0.4).
narrative_ontology:measurement(comm_tr_t1968, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1968, 0.55).
narrative_ontology:measurement(comm_tr_t1980, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1980, 0.68).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1995, 0.7).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2005, 0.45).
narrative_ontology:measurement(comm_tr_t2015, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(comm_tr_t2026, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2026, 0.35).

% Extraction over time
narrative_ontology:measurement(comm_be_t1942, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1942, 0.5).
narrative_ontology:measurement(comm_be_t1955, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1955, 0.55).
narrative_ontology:measurement(comm_be_t1968, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1968, 0.61).
narrative_ontology:measurement(comm_be_t1980, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1995, 0.68).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2005, 0.63).
narrative_ontology:measurement(comm_be_t2015, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement(comm_be_t2026, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2026, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1942, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1942, 0.35).
narrative_ontology:measurement(comm_su_t1955, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1955, 0.32).
narrative_ontology:measurement(comm_su_t1968, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1968, 0.3).
narrative_ontology:measurement(comm_su_t1980, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1980, 0.28).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement(comm_su_t2005, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2005, 0.52).
narrative_ontology:measurement(comm_su_t2015, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(comm_su_t2026, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2026, 0.57).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__substantial_effects_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, originalist_narrow_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Commerce Clause' conflates three structurally distinct constraints corresponding to three readings of one constitutional text: an aggregation-without-limits reading, a cross-border-and-instrumentalities reading, and this bounded-aggregation reading. Each instantiates a different epsilon, a different beneficiary/victim structure, and a different operative mechanism, so each is authored as its own story and linked here per the epsilon-invariance principle. This reading sits downstream of both siblings: it accepts the aggregation mechanism the expansive consolidation built while reinstating the categorical-limit commitment the narrow reading preserves, which is why its extraction profile blends features of both parents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
