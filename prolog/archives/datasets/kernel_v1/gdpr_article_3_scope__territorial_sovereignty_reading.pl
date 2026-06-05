% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__territorial_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   GDPR Article 3 establishes the territorial scope of EU data protection
 *   law, extending it to processing of personal data of EU residents
 *   regardless of the controller's location or where the processing occurs.
 *   This constraint models ONE reading of a contested kernel: the territorial
 *   sovereignty reading, which frames Article 3's extraterritorial
 *   application as a violation or excess of legitimate regulatory authority
 *   grounded in territorial boundaries. From this reading, a non-EU state (or
 *   a non-EU actor) experiences Article 3 as unilateral rule-setting that
 *   exceeds the EU's legitimate jurisdictional reach under international law
 *   principles of territorial sovereignty. The alternative readings—effects
 *   jurisdiction (Article 3 is legitimate because EU residents are affected
 *   and the EU has sovereign authority to protect them) and market access
 *   (Article 3 is simply the price of market entry, not a jurisdiction
 *   claim)—construct the same legal text differently and produce different
 *   beneficiary/victim structures. This story instantiates only the
 *   territorial sovereignty reading: data protection becomes a compliance
 *   cost imposed on non-EU actors; the counter-resistance (data localization,
 *   mutual recognition rejection) becomes rational defensive regulation; the
 *   constraint exhibits both coordination function (standards enable trade)
 *   and extraction (unilateral scope determination). The extractiveness has
 *   risen from 0.35 (2018, immediately post-GDPR adoption, when compliance
 *   mechanisms were still informal) to 0.58 (2026, as enforcement actions
 *   multiply, Schrems II invalidates standard contractual clauses, and
 *   adequacy determinations become conditional). Suppression has risen from
 *   0.50 to 0.65 as non-EU states increase counter-regulatory pressure (data
 *   localization laws, restrictions on EU data flows, reciprocal enforcement
 *   threats). The theater ratio has remained relatively stable (0.42-0.48)
 *   because the constraint's mechanism is structural (market exclusion)
 *   rather than performative—the EU actually enforces Article 3 scope against
 *   major platforms and controllers, unlike many regulatory regimes.
 *
 * KEY AGENTS:
 *   - EU Data Protection Authorities (EDPB, NDPAs): Primary beneficiary (institutional/arbitrage) — enforce Article 3 scope unilaterally; gain regulatory authority that extends beyond EU territory
 *   - EU Data Subjects: Secondary beneficiary (institutional/mobile) — protected by Article 3's extraterritorial reach; gain privacy protections that follow them globally
 *   - Non-EU State Regulators (Russia, China, India, Brazil, US): Primary victims (organized/mobile) — experience Article 3 as unilateral scope claim; respond with counter-regulatory measures (data localization, mutual recognition rejection)
 *   - Non-EU Data Controllers (non-EU companies, global platforms): Secondary victims (moderate/constrained) — must comply with GDPR even when located outside EU; bear compliance costs imposed externally; some benefit from standardized rules
 *   - Non-EU Data Subjects: Tertiary victims (powerless/trapped) — no territorial authority enforces their protections; their data is regulated by EU law but they have no recourse to EU mechanisms
 *   - International Data Flow Infrastructure: Structural victim (abstract) — increasing friction, localization mandates, mutual recognition breakdowns degrade the global data commons
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, 0.58).
domain_priors:suppression_score(gdpr_article_3_scope__territorial_sovereignty_reading, 0.65).
domain_priors:theater_ratio(gdpr_article_3_scope__territorial_sovereignty_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__territorial_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__territorial_sovereignty_reading, "GDPR Article 3 Scope: Territorial Sovereignty Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__territorial_sovereignty_reading, "technology_governance/international_law/privacy_regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__territorial_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__territorial_sovereignty_reading, 'c6e30d41-424f-4347-8935-9fdc5831c8ae').
narrative_ontology:cs_kernel_codification('c6e30d41-424f-4347-8935-9fdc5831c8ae', fixed_text).
narrative_ontology:cs_authority_grounding('c6e30d41-424f-4347-8935-9fdc5831c8ae', extraction).
narrative_ontology:cs_interpretation_layer_present('c6e30d41-424f-4347-8935-9fdc5831c8ae').
narrative_ontology:cs_reading_relation('c6e30d41-424f-4347-8935-9fdc5831c8ae', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('c6e30d41-424f-4347-8935-9fdc5831c8ae', gdpr_article_3_scope__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('c6e30d41-424f-4347-8935-9fdc5831c8ae', foundational, territorial_authority_as_jurisdictional_limit).
narrative_ontology:cs_axiom_status(territorial_authority_as_jurisdictional_limit, holdable).
narrative_ontology:cs_axiom_grounding('c6e30d41-424f-4347-8935-9fdc5831c8ae', territorial_authority_as_jurisdictional_limit, deontological).
narrative_ontology:cs_axiom('c6e30d41-424f-4347-8935-9fdc5831c8ae', foundational, reciprocal_submission_requirement_for_legitimacy).
narrative_ontology:cs_axiom_status(reciprocal_submission_requirement_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('c6e30d41-424f-4347-8935-9fdc5831c8ae', reciprocal_submission_requirement_for_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('c6e30d41-424f-4347-8935-9fdc5831c8ae', international_law_territorial_sovereignty_principle).
narrative_ontology:cs_drift_state('c6e30d41-424f-4347-8935-9fdc5831c8ae', contemporary_unilateral_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c6e30d41-424f-4347-8935-9fdc5831c8ae', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_state_regulators).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_controllers).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, international_data_flows).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_data_subjects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-EU DATA SUBJECT (SNARE) — Trapped in jurisdiction where no territorial authority enforces GDPR protections, yet their data is regulated by EU law when processed by controllers subject to Article 3. No exit capacity; no local protection mechanism; extraction runs via regulatory asymmetry. Maximum suppression — the subject cannot opt out of EU scope determination.
constraint_indexing:constraint_classification(gdpr_article_3_scope__territorial_sovereignty_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-EU DATA CONTROLLER (TANGLED ROPE) — Constrained by Article 3's extraterritorial application (cannot exit GDPR scope), but also gains coordination benefit: clear regulatory standards reduce compliance uncertainty across markets. Extraction exists (compliance costs imposed externally) but coordination function is genuine (standardization enables trade).
constraint_indexing:constraint_classification(gdpr_article_3_scope__territorial_sovereignty_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EU DATA SUBJECT (ROPE) — Beneficiary through pure coordination. GDPR's territorial scope ensures their protections follow them globally when they engage with EU-based or EU-targeting controllers. No extraction experienced — protection is the coordinate benefit itself.
constraint_indexing:constraint_classification(gdpr_article_3_scope__territorial_sovereignty_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EU REGULATOR / DATA PROTECTION AUTHORITY (TANGLED ROPE) — Benefits from Article 3 scope expansion (regulatory authority and enforcement reach extend beyond territory), but constrained by legitimacy friction when enforcing against controllers in jurisdictions that reject EU legal authority. Coordination function: harmonizing standards globally. Extraction: unilateral rule-setting across borders without reciprocal submission.
constraint_indexing:constraint_classification(gdpr_article_3_scope__territorial_sovereignty_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: NON-EU STATE REGULATOR / ORGANIZED RESISTANCE (TANGLED ROPE) — Experiences Article 3 as extraction: sovereignty violation + capacity asymmetry (EU can enforce, non-EU state cannot reciprocally regulate EU controllers). But mobile exit exists: data localization mandates, national data residency laws, reciprocal enforcement against EU services. Organized agents (Russia, China, India, Brazil) are building counter-regulatory structures. Coordination function: mutual recognition frameworks (real, but subordinate to the extraction dynamic).
constraint_indexing:constraint_classification(gdpr_article_3_scope__territorial_sovereignty_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LEGAL REALISM VIEW (MOUNTAIN) — From a civilizational horizon, the constraint appears as an immutable feature of international law: the right of a sovereign jurisdiction to enforce its law within its territory, combined with the extension of that law to extraterritorial conduct meeting jurisdictional hooks, is a foundational principle (effects doctrine, passive personality, protective principle). The constraint looks like natural law of sovereignty. However, this is a FALSE SUMMIT: the sovereignty principle is itself contested (territorial vs. functional), and Article 3's specific formulation (targeting of EU residents, professional offering to EU market) is a contingent regulatory choice, not a deduction from sovereignty first principles.
constraint_indexing:constraint_classification(gdpr_article_3_scope__territorial_sovereignty_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: LEGACY INSTITUTIONAL DOCTRINE (PITON) — Traditional international law doctrine treating territorial sovereignty as the primary limitation on state regulatory authority has become partly performative. The actual practice (effects doctrine, extraterritorial enforcement, unilateral standard-setting by large economic blocs) has diverged from the doctrinal claim. Institutions cite territorial limits while systematically exceeding them. Theater ratio reflects the gap between doctrinal authority (invoking territorial limits) and actual practice (scope expansion).
constraint_indexing:constraint_classification(gdpr_article_3_scope__territorial_sovereignty_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gdpr_article_3_scope__territorial_sovereignty_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gdpr_article_3_scope__territorial_sovereignty_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gdpr_article_3_scope__territorial_sovereignty_reading, TR),
    TR >= 0.70.

:- end_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits a genuine coordination function (GDPR standardization enables compliance prediction across EU markets) but paired with significant extraction. Non-EU controllers bear compliance costs not negotiated with them; the EU unilaterally determines scope without reciprocal submission to external standards. The extraction is not maximal (0.7+) because some coordination benefit is real—controllers can predict requirements and build compliance infrastructure. But the ratio of extraction to coordination is asymmetric: the EU benefits from standard-setting authority; non-EU actors bear the cost. Suppression (0.65): Moderate-high. Non-EU controllers cannot exit Article 3 scope without abandoning EU market access or data collection from EU residents. Non-EU states cannot reciprocally regulate EU controllers with equivalent authority. But suppression is not total (0.85+) because alternatives exist: data localization allows non-EU states to exclude EU processors; controllers can refuse EU data collection; mutual recognition frameworks (though conditional) create exit paths. Suppression has risen as these alternatives have activated (Schrems II invalidating standard contractual clauses made localization more attractive). Theater ratio (0.48): Moderate-low. The constraint's mechanism is enforcement-based (actual GDPR fines, market exclusion) rather than performative. However, moderate theater exists in the legitimacy framing: Article 3 is presented as derivative from territorial authority to protect EU residents, but this framing obscures the unilateral scope determination that non-EU states reject. The theater increases when the EU invokes territorial sovereignty as the justification while simultaneously enforcing extraterritorially in ways that a non-territorial power (like a city-state) could never achieve.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The EU regulator sees Tangled Rope (coordination function + justified extraction). The EU data subject sees Rope (pure coordination benefit). The non-EU state regulator sees Tangled Rope with extraction dominance (the coordination is subordinate). The non-EU controller sees Tangled Rope with constrained mobility (trapped in EU market, but can pursue data localization). The non-EU data subject sees Snare (no exit, no protection). The analytical observer at civilizational scope risks seeing Mountain (territorial authority as natural law), but this is a false summit—the territorial principle is itself contested, and Article 3's specific jurisdictional hooks are contingent choices. The gap between regulatory perspectives reveals that Article 3's legitimacy rests on EU interpretations of international law that non-EU states do not accept. The engine's false summit detector should identify the mountain perspective as a naturalization of contested regulatory choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures the agent's structural position from victim (-like) d ≈ 1.0 to beneficiary (-like) d ≈ 0.0. Non-EU state regulators (organized/mobile) have d ≈ 0.70—they are organized enough to resist but cannot exit the constraint without enormous cost (economic isolation). Non-EU data controllers (moderate/constrained) have d ≈ 0.65—they bear costs but gain some coordination benefit. EU data subjects (institutional/arbitrage) have d ≈ 0.10—they are beneficiaries with arbitrage options (market access + protection). Non-EU data subjects (powerless/trapped) have d ≈ 0.95—they are full targets with no exit. The derived d values feed into the sigmoid f(d) to compute effective extraction (chi) from each perspective. EU regulators enjoy low or negative chi (they extract benefit), while non-EU actors experience high chi (they bear extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the kernel (GDPR Article 3 scope) admits multiple coherent readings, each producing a different constraint story. The territorial sovereignty reading (this story) constructs Article 3 as an extraction mechanism with coordination benefit. The effects jurisdiction reading would construct it as legitimate extraterritorial authority based on effects on EU residents. The market access reading would construct it as a simple pricing mechanism (you want EU market access, you accept EU standards). No single reading is 'correct' in the abstract—each is coherent within its own jurisdictional frame. The mandatrophy is resolved not by choosing one but by recognizing that the kernel is itself the site of irreducible disagreement. Political resolution comes through negotiation (adequacy determinations, mutual recognition frameworks) or power asymmetry (EU's market leverage enforces its reading on smaller economies). The constraint story models one reading as a clean ε-invariant constraint, and the sibling stories model the alternative readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_3_jurisdictional_hooks_ambiguity,
    'Are Article 3 jurisdictional hooks (establishment in EU, targeting of EU residents, offering goods/services to EU market) derivative from EU territorial authority or unilateral scope claims that exceed legitimate authority?',
    'Comparative analysis of other states'' extraterritorial claims and reciprocal enforceability. If China applied China''s data sovereignty standard to EU companies serving Chinese residents, would the EU accept this as legitimate? If not, the reciprocity test reveals that Article 3 rests on power asymmetry, not on first-principles jurisdiction.',
    'If derived from legitimate territorial authority: constraint classifies as Rope or Tangled Rope from EU perspective (coordinated scope expansion). If unilateral: constraint classifies as Snare from non-EU state perspective (regulatory imperialism). Reciprocity test often fails, indicating the latter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_3_jurisdictional_hooks_ambiguity, conceptual, 'Whether Article 3 hooks reflect territorial authority or power-asymmetric scope expansion').

omega_variable(
    data_localization_counter_extraction,
    'Do data localization mandates (counter-regulatory measures by non-EU states) represent legitimate local regulatory authority or retaliatory extraction?',
    'Structural analysis: do localization requirements protect identifiable local interests (e.g., reducing surveillance by foreign intelligence) or are they primarily shields against GDPR enforcement? Comparative analysis of localization regimes across states with different bargaining power.',
    'If legitimate: each state''s regulatory scope is symmetric, extractiveness declines (multiple Tangled Rope perspectives become Rope). If retaliatory: counter-extraction escalates, suppression rises across all non-EU perspectives, snare characteristics intensify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_localization_counter_extraction, empirical, 'Whether data localization regimes are legitimate or retaliatory extraction').

omega_variable(
    eu_market_power_asymmetry_as_enforcement_mechanism,
    'Does the EU''s economic market size enable unilateral enforcement of Article 3 scope in ways that a smaller jurisdiction could never achieve, making the constraint''s enforceability (not its legitimacy) the primary mechanism?',
    'Counterfactual: if Moldova applied the same Article 3 logic (effects-based scope over any data controller targeting Moldovan residents globally), what would be its actual enforcement capacity? Comparison of GDPR enforcement actions against non-EU controllers vs enforcement capacity of non-EU states against EU controllers.',
    'If market power is the primary mechanism: the constraint is less about legal principle (territorial authority) and more about economic coercion (ability to exclude from a large market). This shifts classification toward Snare from non-EU state perspective and increases theater_ratio (the legitimacy framing obscures the enforcement mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(eu_market_power_asymmetry_as_enforcement_mechanism, empirical, 'Economic market power as the primary enforcement mechanism of Article 3 scope').

omega_variable(
    reading_selection_ambiguity,
    'This constraint instantiates the territorial sovereignty reading of GDPR Article 3. But the kernel admits at least two other readings: an effects jurisdiction reading (which emphasizes Article 3 as legitimate extraterritorial scope based on effects on EU residents) and a market access reading (which emphasizes compliance as the price of EU market entry). Which reading is correct, or do all three coexist?',
    'This is a committer-frame question, not a empirical question. It depends on which institutional voice is adjudicating the kernel. The EU Commission, EU Courts, and the European Data Protection Board converge on the effects + market reading. Non-EU states (US, China, India) converge on the territorial reading. No neutral arbiter exists to resolve which is ''correct.'' Resolution comes through political negotiation (mutual recognition agreements, adequacy determinations) or power asymmetry (larger bloc imposes its reading).',
    'All three readings remain live. This omega documents why the constraint story must model one reading only (territorial sovereignty) and must link to sibling stories representing the other readings. The kernel itself is the irreducible uncertainty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_selection_ambiguity, conceptual, 'Kernel-level reading ambiguity for GDPR Article 3 scope').

omega_variable(
    adequacy_determination_legitimacy,
    'Do EU adequacy determinations (findings that a non-EU jurisdiction has equivalent protections) represent genuine assessment of equivalent protection or are they political recognition of functional submission to EU standards?',
    'Analysis of adequacy decisions that were later revoked or suspended (e.g., Schrems I, Schrems II, recent Switzerland suspension) vs those that remain stable. Correlation between adequacy status and whether the non-EU state has independently strong data protection norms vs those that essentially adopted EU standards.',
    'If genuinely equivalent: adequacy determinations resolve the territorial sovereignty tension by recognizing concurrent jurisdiction. If political submission: adequacy is a way to standardize on EU terms while maintaining the fiction of reciprocal recognition. The latter interpretation raises extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adequacy_determination_legitimacy, empirical, 'Legitimacy of EU adequacy determinations as reciprocal recognition vs political submission').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__territorial_sovereignty_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_terr_tr_t0, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(gdpr_terr_tr_t5, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 5, 0.45).
narrative_ontology:measurement(gdpr_terr_tr_t10, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(gdpr_terr_be_t0, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gdpr_terr_be_t5, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(gdpr_terr_be_t10, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_terr_su_t0, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(gdpr_terr_su_t5, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(gdpr_terr_su_t10, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__territorial_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__market_access_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, data_localization_mandates_counter_extraction).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, adequacy_determination_framework).

% DUAL FORMULATION NOTE:
% GDPR Article 3 scope is a contested kernel with at least three structurally distinct readings. This constraint story instantiates the territorial sovereignty reading (data protection exceeds territorial authority). Sibling stories instantiate effects jurisdiction (legitimate protection of EU residents) and market access (compliance pricing) readings. Each reading has different ε values, different beneficiary/victim declarations, and different analytical implications. All three remain live institutional positions: the EU interprets Article 3 via effects + market access frames; non-EU states interpret it via territorial sovereignty frame. No single reading is 'correct'—the irreducible uncertainty is that the kernel admits multiple coherent readings. Cross-reading analysis requires separate constraint stories linked via network.affects_constraints. This decomposition follows the ε-invariance principle: the observable used to evaluate the kernel (which reading you adopt) changes the structural beneficiary/victim classification and the ε value derived from it. Therefore, separate stories, not one story with reading parameters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gdpr_article_3_scope__territorial_sovereignty_reading, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
