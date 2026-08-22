% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__territorial_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__territorial_sovereignty_reading, []).

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
 *   constraint_id: gdpr_article_3_scope__territorial_sovereignty_reading
 *   human_readable: GDPR Article 3 Scope: Territorial Sovereignty Reading
 *   domain: technology_governance/international_law/privacy_regulation
 *
 * SUMMARY:
 *   This constraint story instantiates the territorial sovereignty reading of
 *   GDPR Article 3's scope question: does the EU have legitimate regulatory
 *   authority to enforce data protection rules on entities and activities
 *   outside EU borders, when those activities affect EU residents? The
 *   territorial sovereignty reading answers: no—jurisdiction is bounded by
 *   territorial control, and extraterritorial application exceeds legitimate
 *   authority. This is one reading of a contested kernel (Article 3 itself,
 *   the text that all three readings interpret). The constraint is CLAIMED as
 *   tangled_rope because the reading asserts both a genuine coordination
 *   function (mutual recognition of jurisdictional boundaries among
 *   sovereigns) AND asymmetric extraction (global platforms and non-EU
 *   entities bear compliance costs to defend that boundary, while non-EU
 *   state regulators benefit from preserved autonomy). The reading is live in
 *   international law debates, state diplomatic positions, and WTO contests,
 *   but contested—other readings assign different beneficiaries, costs, and
 *   legitimacy to the same text.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, 0.68).
domain_priors:suppression_score(gdpr_article_3_scope__territorial_sovereignty_reading, 0.71).
domain_priors:theater_ratio(gdpr_article_3_scope__territorial_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__territorial_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__territorial_sovereignty_reading, "GDPR Article 3 Scope: Territorial Sovereignty Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__territorial_sovereignty_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__territorial_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__territorial_sovereignty_reading, '65fe8e66-c6bf-489d-b408-bf58be4ddbcc').
narrative_ontology:cs_kernel_codification('65fe8e66-c6bf-489d-b408-bf58be4ddbcc', fixed_text).
narrative_ontology:cs_authority_grounding('65fe8e66-c6bf-489d-b408-bf58be4ddbcc', lineage).
narrative_ontology:cs_interpretation_layer_present('65fe8e66-c6bf-489d-b408-bf58be4ddbcc').
narrative_ontology:cs_reading_relation('65fe8e66-c6bf-489d-b408-bf58be4ddbcc', gdpr_article_3_scope__effects_jurisdiction_reading, forecloses).
narrative_ontology:cs_reading_relation('65fe8e66-c6bf-489d-b408-bf58be4ddbcc', gdpr_article_3_scope__market_access_reading, influences).
narrative_ontology:cs_axiom('65fe8e66-c6bf-489d-b408-bf58be4ddbcc', foundational, jurisdiction_bounded_by_territory).
narrative_ontology:cs_axiom_status(jurisdiction_bounded_by_territory, holdable).
narrative_ontology:cs_axiom_grounding('65fe8e66-c6bf-489d-b408-bf58be4ddbcc', jurisdiction_bounded_by_territory, conventional).
narrative_ontology:cs_axiom('65fe8e66-c6bf-489d-b408-bf58be4ddbcc', foundational, regulatory_authority_cannot_exceed_sovereignty).
narrative_ontology:cs_axiom_status(regulatory_authority_cannot_exceed_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('65fe8e66-c6bf-489d-b408-bf58be4ddbcc', regulatory_authority_cannot_exceed_sovereignty, deontological).
narrative_ontology:cs_reference_frame('65fe8e66-c6bf-489d-b408-bf58be4ddbcc', westphalian_territorial_sovereignty).
narrative_ontology:cs_drift_state('65fe8e66-c6bf-489d-b408-bf58be4ddbcc', contemporary_globalized_data_flows, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('65fe8e66-c6bf-489d-b408-bf58be4ddbcc', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulators).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, global_data_processors).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_data_controllers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, eu_residents).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, competing_jurisdictions).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, us_technology_companies).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, competing_jurisdictions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Non-EU governments benefit from the territorial sovereignty reading insofar as it constrains EU regulatory authority beyond EU borders, preserving their own ability to set domestic data governance rules without EU interference. They advocate for jurisdictional limits as the legitimate boundary of regulatory power. Where they hold institutional power over data flows within their territory, they can implement countervailing data localization requirements.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulators, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulators, agenda_setter).

% EU data protection authorities enforce Article 3 as written: jurisdiction extends to processing activities targeting EU residents, regardless of where the controller or processor is located. They view this as a coherent reading of the regulation's protective intent. Under the territorial sovereignty reading, their enforcement is characterized as extraterritorial assertion exceeding legitimate authority.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_regulators, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the cost of GDPR compliance globally, even when processing data outside EU borders and for non-EU residents. Under the territorial sovereignty reading, they are subject to enforcement for activities the reading classifies as outside EU regulatory jurisdiction. The cost is substantial: dual compliance regimes, localization infrastructure, legal exposure in multiple jurisdictions with conflicting rules.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, global_data_processors, payer,
    powerful, biographical, constrained, global).

% Non-EU entities processing data domestically without targeting EU residents find themselves subject to GDPR compliance burdens when data flows cross borders or when they offer services accessible to EU users incidentally. Under the territorial sovereignty reading, this is enforcement overreach; they face compliance costs for activities occurring entirely outside EU territory.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_data_controllers, payer,
    moderate, biographical, constrained, global).

% Benefit from EU data protection wherever their data is processed globally, under the protective logic of Article 3's extraterritorial reach. Under the territorial sovereignty reading, this benefit is framed as the EU imposing its rules beyond its borders rather than extending protection to its residents. Their exit is identity-locked: they cannot become non-EU residents without changing citizenship, and even then their historical data may remain subject to GDPR.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_residents, beneficiary,
    powerless, biographical, identity_locked, global).

% Major US tech platforms process data globally and incidentally reach EU users; they face GDPR compliance burdens on all EU-resident data processing. Under the territorial sovereignty reading, this is illegitimate extraterritorial reach. Their exit is constrained by market size: abandoning EU market access is costly, but EU compliance globally requires substantial infrastructure changes.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, us_technology_companies, payer,
    powerful, biographical, constrained, global).

% WTO, USMCA, and bilateral trade authorities examine GDPR Article 3's scope as a potential trade barrier or regulatory protectionism. They take positions on whether the territorial sovereignty reading aligns with trade law or whether the effects jurisdiction reading constitutes legitimate market-access protection.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, international_trade_bodies, observer,
    institutional, generational, analytical, global).

% Other regions (US, China, India) simultaneously benefit when their own regulatory reach is recognized as bounded by territorial limits (beneficiary), while paying when their companies must comply with EU rules on non-territorial grounds (payer). The tension is structural: supporting territorial limits benefits their regulators, but their tech companies bear compliance costs.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, competing_jurisdictions, payer,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__territorial_sovereignty_reading, competing_jurisdictions, beneficiary).

% Civil society organizations focused on digital rights globally would argue for expanded extraterritorial protection (supporting the effects reading or market access reading) but are not seated in territorial sovereignty arguments. Their voice—that data protection should follow data flows, not territorial borders—is structurally absent from the sovereignty debate.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, excluded_digital_rights_advocates, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulators).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__territorial_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a principle that regulatory jurisdiction is bounded by territorial authority: a state can govern conduct and data flows within its borders but cannot legitimately extend its rules to activities occurring entirely outside its territory, even if those activities incidentally affect its residents. This solves a coordination problem among sovereigns: mutual recognition that each state's regulatory power stops at its borders, preventing regulatory collision and escalation.
% TRANSFER_FUNCTION: Moves compliance burden and legal exposure from non-EU entities (controllers, processors, platforms) to EU regulators (who must defend the legitimacy of their extraterritorial reach) and to EU residents (who lose extraterritorial data protection coverage beyond what territorial sovereignty permits). It redistributes regulatory authority: more to non-EU states over their domestic data governance, less to EU authorities over global data flows affecting their residents.
% ABSENT_VOICES: Digital rights advocates and EU residents in non-EU jurisdictions are structurally absent: they would argue that data protection should follow data and harm, not political borders, and that territorial limits leave people in non-EU countries with minimal protections. Tech companies' voices are present as payers, but voices arguing for human-centered protection across borders (rather than state-centered territorial rights) do not appear.
% DISAPPEARANCE_RATIONALE: If the territorial sovereignty reading disappeared and effects jurisdiction prevailed, global data governance would reorganize: EU regulatory reach would expand, non-EU states' domestic regulatory autonomy would compress, and global tech platforms would face integrated EU rules instead of dual compliance. If it instead prevailed absolutely (no extraterritorial reach at all), EU protection would shrink to EU borders, data flows would segregate, and compliance costs would shift from global platforms to data subjects in non-EU jurisdictions. Either shift is substantial enough that arrangements depend on which reading holds.
% FOUNDING_PROBLEM: Early internet governance lacked a coherent principle for jurisdictional reach when activities, actors, and harms crossed borders. The territorial sovereignty reading was founded to establish a limiting principle: states cannot make rules for conduct outside their borders, even indirectly. This prevents regulatory imperialism and establishes mutual respect among sovereigns.
% FOUNDING_PROBLEM_CORROBORATION: International law scholars and non-EU governments attest the founding problem remains live: without territorial limits, jurisdictional conflicts escalate and smaller states lose regulatory autonomy. EU regulators and digital rights organizations attest the founding problem has shifted: data flows are inherently global, and territorial limits leave individuals outside EU borders unprotected. Independent analysis (Hoeren, Pinello, Poullet) documents both positions as live jurisprudential contests; no external corroboration resolves the dispute.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__territorial_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__territorial_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gdpr_article_3_scope__territorial_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 because the territorial sovereignty reading asserts that global compliance burden is imposed without legitimate authority—the extraction is the cost of defending a boundary claim. Under this reading, EU regulators extract legitimacy benefit (their authority is recognized and expanded) and non-EU regulators extract autonomy benefit (their domestic regulatory space is preserved), while global platforms and non-EU data controllers bear costs for activities classified as outside EU jurisdiction. Suppression is higher (0.71) because the constraint requires active policing of the territorial boundary: EU regulators must continuously assert and enforce their extraterritorial reach (enforcement machinery), while non-EU states must resist (data localization requirements, trade complaints) to maintain their side of the boundary. Theater (0.42) is moderate: the protective narrative ('protecting EU residents') is real, but a growing share of enforcement defends the jurisdictional claim itself rather than data protection outcomes. Accessibility collapse (0.62) is moderate because alternatives (territorial localization, consent-based processing, market-based compliance) remain partly viable for data controllers, though expensive. Resistance (0.73) is high because non-EU states, tech platforms, and business associations actively resist the extraterritorial reach through diplomatic channels, trade suits, and data localization counter-regulations.
 *
 * PERSPECTIVAL GAP:
 *   The reading instantiates a sharp perspectival gap: from non-EU state seats, the territorial sovereignty boundary is legitimate coordination—mutual recognition of jurisdiction. From global platform and non-EU controller seats, the same boundary is a facade for extraterritorial imposition. From EU regulator seats, the reading mischaracterizes extraterritorial reach as exceeding authority when the reading's own text (Article 3(2)) permits it. The engine computes per-seat types from the structural data; the gap is encoded in the shared metrics combined with different stakeholder positions. A platform analyst would compute this as a snare (pure extraction, no coordination); a non-EU regulator would compute it as rope (genuine coordination of sovereignty norms). The reading declares tangled rope because it asserts both coordination (the boundary principle) and extraction (the compliance burden imposed in its name).
 *
 * DIRECTIONALITY LOGIC:
 *   The non-EU state regulators sit at d near the beneficiary end (0.15–0.25) because the territorial sovereignty reading benefits them by preserving their regulatory autonomy—they are coordinated (recognizing mutual jurisdictional limits) and they gain (domestic rule-setting power). EU regulators sit at d near the middle-to-target (0.40–0.60) because they claim a coordination function but the reading asserts they extract authority beyond legitimate bounds. Global data processors and non-EU controllers sit at d near the full-target end (0.75–0.90) because they are extracted from (compliance costs imposed for out-of-territory activity) without meaningful exit—their constraint is principally extractive. EU residents sit near symmetric (d ≈ 0.50) in the territorial sovereignty reading: they lose extraterritorial protection (cost) but keep territorial protection (benefit), and their exit is identity-locked (they cannot change residence easily to escape GDPR).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (establishing a limiting principle on jurisdictional reach) remains contested—international law scholarship has not converged, and diplomatic practice exhibits continued conflict. The territorial sovereignty reading claims the founding problem is live and Article 3 correctly solves it; the effects jurisdiction reading claims the problem has evolved and Article 3(2) correctly extends protection to global data flows. The mandatrophy risk is moderate: if the founding problem—preventing jurisdictional collision—has been solved by mutual acceptance of extraterritorial reach as legitimate (the effects reading wins), then enforcing territorial limits becomes nostalgia for a solved problem, and the constraint becomes piton-like (maintained by institutional inertia rather than real coordination need). Conversely, if territorial limits are indeed the correct principle and extraterritorial reach is escalating jurisdictional conflict (the territorial reading wins), then the constraint remains live coordination. The measurement series shows modest extraction growth (0.52 to 0.68 over the interval), consistent with either reading: it could track increasing EU enforcement (supporting the effects reading's mandatrophy claim—a dead limit persisting) or increasing global compliance pressure (supporting the territorial reading's claim—the limit is under siege). The contest is unresolved by the data alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_kernel_identity,
    'Is the territorial sovereignty reading''s core premise—that jurisdiction is legitimately bounded by territory—a fixed principle of international law, or a contestable doctrine competing with legitimate alternatives?',
    'Survey of contemporary international law scholarship, state practice in jurisdictional disputes (cases at ICJ, arbitral tribunals, bilateral negotiations), and diplomatic consensus on extraterritorial regulatory reach.',
    'If territorial limits are a fixed principle, the effects reading is foreclosed and this reading is correct. If extraterritorial reach is also legitimate under some conditions (effects, market access, human rights), then this and the effects reading coexist, and the classification shifts from tangled rope toward snare (extraction with weaker coordination justification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_identity, conceptual, 'Whether territorial boundaries are fixed law or contestable principle.').

omega_variable(
    sibling_reading_coexistence,
    'Can the territorial sovereignty reading and the effects jurisdiction reading both be true in the same international legal framework, or does one logically foreclose the other?',
    'Interpretation of Article 3 in light of EU Charter, CJEU jurisprudence (especially Schrems cases), and state practice. If CJEU jurisprudence converges on effects as legitimate, the readings coexist (different courts enforce different readings). If one court forecloses the other, they compete for dominance.',
    'Coexistence supports modeling both readings as live constraints in a constraint family; foreclosure (rare) would mean one reading has been displaced. Currently, the readings coexist—the EU enforces effects, the US and China resist with territorial arguments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, empirical, 'Whether sibling readings are logically compatible.').

omega_variable(
    extraction_vs_coordination_primacy,
    'Is the primary function of the territorial sovereignty boundary to coordinate mutual jurisdictional respect (tangled rope claim), or to extract regulatory authority from global data processors by claiming universal reach (snare claim)?',
    'Trace the evolution of Article 3 enforcement: if early enforcement focused on cross-border data transfers (coordinating data flow governance), the coordination claim is stronger; if enforcement has expanded to cover incidental exposure to EU residents and targeting tests broaden, the extraction claim strengthens. Compare enforcement actions against EU and non-EU entities; asymmetric enforcement suggests extraction.',
    'If coordination is primary, the constraint is tangled rope and mandatrophy risk is low—the coordination function is real. If extraction is primary, the constraint is snare and mandatrophy risk is high—the boundary principle is cover for expanding authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_primacy, empirical, 'Whether Article 3 enforcement prioritizes coordination or extraction.').

omega_variable(
    reading_as_false_summit,
    'Is the territorial sovereignty reading a natural principle of international law, or a construct benefiting non-EU state regulators and tech companies that adopt data localization as competitive advantage?',
    'Examine who benefits from and advocates for the territorial reading: non-EU governments, US tech firms, competitors seeking regulatory advantage. Compare with independent international law scholarship and civil society positions (which largely support effects jurisdiction for human rights protection). If the reading is primarily advocated by parties with material interest and opposed by rights-focused actors, it is a false-summit candidate.',
    'If false summit, the reading''s claimed type (rope) masks extraction benefiting specific institutional actors. FSM would reclassify to tangled rope or snare. If the reading is indeed principled international law doctrine independent of beneficiaries, the classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_as_false_summit, conceptual, 'Whether the territorial sovereignty reading is a natural principle or a constructed beneficiary doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__territorial_sovereignty_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(gdpr_tr_t4, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement(gdpr_tr_t8, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement(gdpr_tr_t12, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement(gdpr_tr_t16, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement(gdpr_tr_t20, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(gdpr_be_t4, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(gdpr_be_t8, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(gdpr_be_t12, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(gdpr_be_t16, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(gdpr_be_t20, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(gdpr_su_t4, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 4, 0.59).
narrative_ontology:measurement(gdpr_su_t8, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(gdpr_su_t12, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(gdpr_su_t16, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(gdpr_su_t20, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 20, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__territorial_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gdpr_article_3_scope__territorial_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__market_access_reading).

% DUAL FORMULATION NOTE:
% Article 3 of the GDPR is a contested kernel. Three structurally distinct constraints are authored as separate stories, each instantiating one reading. This story (territorial_sovereignty_reading) asserts jurisdiction is bounded by territory; effects_jurisdiction_reading asserts jurisdiction follows effects on protected persons; market_access_reading asserts GDPR is a conditional market-access requirement, not a jurisdictional claim. The three readings share the kernel text but instantiate different constraints because they assign different beneficiaries, extraction mechanisms, and regulatory legitimacy. Each has its own epsilon (extracted from, against whom), beneficiary set, and compliance cost distribution. The constraint family models the contest: all three readings are live in international law and policy; the engine measures which reading's structural claims hold empirically (per-seat computation reveals divergence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gdpr_article_3_scope__territorial_sovereignty_reading, powerful, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
