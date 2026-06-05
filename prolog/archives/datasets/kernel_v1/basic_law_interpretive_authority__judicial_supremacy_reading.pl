% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy: Courts Hold Final Constitutional Authority
 *   domain: constitutional_law/institutional_design/political_theory
 *
 * SUMMARY:
 *   The judicial supremacy reading of basic law interpretive authority holds
 *   that courts, through specialized legal expertise and institutional
 *   independence from direct political pressure, must retain final authority
 *   to interpret constitutional meaning and constrain legislative action
 *   through constitutional review. This reading instantiates one position in
 *   a contested kernel where the fundamental structure of constitutional
 *   governance is at stake. Under the judicial supremacy reading, the
 *   Constitution becomes a fixed text whose meaning is professional-legal
 *   rather than populist-majoritarian; courts function as gatekeepers of
 *   meaning; and legislative attempts to override judicial interpretation
 *   require supermajority consensus to amend. This reading exhibits tangled
 *   rope structure: it provides genuine coordination benefit (stable
 *   constitutional framework, reduced factionalism, clear interpretive
 *   authority) while simultaneously extracting concentrated authority and
 *   gatekeeping power from alternative institutional locations (legislature,
 *   electoral majorities, popular contestation). The constraint's
 *   measurements show both theater and extractiveness rising over the 40-unit
 *   interval, reflecting doctrine creep: judicial review begins as a narrow
 *   power (check on manifest unconstitutionality) and expands to encompass
 *   broad substantive questions (legislative process, equal protection,
 *   fundamental rights). Theater rises as the legitimacy claim shifts from
 *   'courts enforce the constitutional text' to 'courts protect
 *   constitutional meaning from majoritarian degradation' — an increasingly
 *   theatrical legitimation. Extractiveness rises as the scope of judicial
 *   authority expands and amendment becomes effectively impossible on
 *   contested issues.
 *
 * KEY AGENTS:
 *   - Judiciary (institutional/arbitrage): Primary beneficiary — concentrates interpretive authority, derives legitimacy and institutional prestige from supremacy reading, experiences constraint as pure coordination (stable interpretive monopoly)
 *   - Legal profession (institutional/arbitrage): Secondary beneficiary — expertise gatekeeping creates monopoly on constitutional meaning-making, excludes non-professional interpretation, captures professional control over constitutional discourse
 *   - Electoral majorities blocked by judicial review (powerless/trapped): Primary victim — cannot override judicial veto without supermajority consensus and formal amendment process, bear full extraction of blocked legislation and delayed policy implementation
 *   - Legislative body (organized/constrained): Secondary victim — experiences judicial review as constraint on legislative sovereignty but retains theoretical amendment authority (constrained exit, not trapped)
 *   - Executive branch (powerful/constrained): Mixed position — benefits from judicial review that constrains legislative oversight but experiences extraction when courts block executive action
 *   - Analytical observer (analytical/analytical): Risks naturalizing institutional choice (judicial supremacy) as inherent to constitutional governance rather than one possible reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, 0.48).
domain_priors:suppression_score(basic_law_interpretive_authority__judicial_supremacy_reading, 0.62).
domain_priors:theater_ratio(basic_law_interpretive_authority__judicial_supremacy_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy: Courts Hold Final Constitutional Authority").
narrative_ontology:topic_domain(basic_law_interpretive_authority__judicial_supremacy_reading, "constitutional_law/institutional_design/political_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__judicial_supremacy_reading, '43b0bb87-d37b-4786-a72a-7aecda73b3d5').
narrative_ontology:cs_kernel_codification('43b0bb87-d37b-4786-a72a-7aecda73b3d5', fixed_text).
narrative_ontology:cs_authority_grounding('43b0bb87-d37b-4786-a72a-7aecda73b3d5', expertise).
narrative_ontology:cs_interpretation_layer_present('43b0bb87-d37b-4786-a72a-7aecda73b3d5').
narrative_ontology:cs_reading_relation('43b0bb87-d37b-4786-a72a-7aecda73b3d5', basic_law_interpretive_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('43b0bb87-d37b-4786-a72a-7aecda73b3d5', basic_law_interpretive_authority__popular_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('43b0bb87-d37b-4786-a72a-7aecda73b3d5', foundational, courts_possess_specialized_constitutional_expertise).
narrative_ontology:cs_axiom_status(courts_possess_specialized_constitutional_expertise, holdable).
narrative_ontology:cs_axiom_grounding('43b0bb87-d37b-4786-a72a-7aecda73b3d5', courts_possess_specialized_constitutional_expertise, empirically_contingent).
narrative_ontology:cs_axiom('43b0bb87-d37b-4786-a72a-7aecda73b3d5', foundational, institutional_independence_from_electoral_pressure_is_achievable).
narrative_ontology:cs_axiom_status(institutional_independence_from_electoral_pressure_is_achievable, holdable).
narrative_ontology:cs_axiom_grounding('43b0bb87-d37b-4786-a72a-7aecda73b3d5', institutional_independence_from_electoral_pressure_is_achievable, empirically_contingent).
narrative_ontology:cs_reference_frame('43b0bb87-d37b-4786-a72a-7aecda73b3d5', courts_as_neutral_interpreters).
narrative_ontology:cs_drift_state('43b0bb87-d37b-4786-a72a-7aecda73b3d5', contemporary_substantive_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('43b0bb87-d37b-4786-a72a-7aecda73b3d5', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary_as_institution).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession_expertise_gatekeepers).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities_blocked_by_review).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, legislative_process_gridlock).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, political_minorities_excluded_from_interpretation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ELECTORAL MAJORITIES BLOCKED BY REVIEW (SNARE) — Cannot exit or override judicial interpretation of constitutional text; bear full cost of judicial veto of democratically enacted legislation. Suppression is structural: constitutional amendment is prohibitively difficult (supermajority requirement, state ratification delays). Maximum experienced extraction relative to power position.
constraint_indexing:constraint_classification(basic_law_interpretive_authority__judicial_supremacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LEGISLATIVE BODY WITH AMENDMENT AUTHORITY (TANGLED ROPE) — Benefits from constitutional stability (coordination function: fixed framework prevents constant legislative revision and populist factionalism). Also experiences extraction: judicial review blocks legislation and requires supermajority coalition to override. Exit options are constrained by supermajority requirement but not nonexistent — constitutional amendment is difficult but theoretically available. Hybrid extraction-coordination.
constraint_indexing:constraint_classification(basic_law_interpretive_authority__judicial_supremacy_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JUDICIARY AS INSTITUTION (ROPE) — Primary beneficiary from judicial supremacy reading. Experiences the constraint as pure coordination: final interpretive authority concentrates legitimacy and reduces jurisdictional ambiguity. Judicial actors derive career prestige, institutional authority, and interpretive monopoly from supremacy. Arbitrage exit options: judges can interpret broadly or narrowly, expanding or contracting their own authority. Net beneficiary — extraction flow runs toward this agent through institutional authority concentration.
constraint_indexing:constraint_classification(basic_law_interpretive_authority__judicial_supremacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGAL PROFESSION EXPERTISE GATEKEEPING (ROPE) — Secondary beneficiary. Judicial supremacy grounds legitimacy in specialized legal expertise and independence from political pressure. This reading creates a gatekeeping mechanism: constitutional meaning becomes accessible only through professional legal interpretation, not through direct citizen contestation or legislative reasoning. Legal profession captures monopoly on interpretive authority. Arbitrage exit: profession can broaden or narrow scope of 'expertise,' expanding or contracting gatekeeper power. Net beneficiary.
constraint_indexing:constraint_classification(basic_law_interpretive_authority__judicial_supremacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the judicial supremacy reading naturalizes judicial independence and specialized expertise as inherent to constitutional governance: 'courts are institutionally designed to resist political pressure; expertise is necessary to interpret complex foundational law.' This perspective risks treating a contested institutional choice (one possible reading of basic law) as an immutable structural property of constitutional systems. Candidates for false summit detection: beneficiaries identified (judiciary, legal profession), and institutional authority concentration is documented as structural fact rather than natural law.
constraint_indexing:constraint_classification(basic_law_interpretive_authority__judicial_supremacy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: EXECUTIVE BRANCH (TANGLED ROPE) — Mixed position. Executive benefits from judicial review that strikes down legislative constraints on executive power (administrative law disputes). Executive also experiences extraction when courts block executive action or mandate restrictions. Exit options constrained by judicial review but not trapped: executive can propose constitutional amendments, negotiate with legislature, or use prosecutorial discretion. Moderate experienced extraction with some coordination benefit from constitutional predictability.
constraint_indexing:constraint_classification(basic_law_interpretive_authority__judicial_supremacy_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(basic_law_interpretive_authority__judicial_supremacy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(basic_law_interpretive_authority__judicial_supremacy_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high, reflecting the institutional authority concentration and gatekeeping mechanisms. The judiciary captures monopoly on final constitutional meaning-making; the legal profession captures gatekeeping on professional interpretation; electoral majorities face supermajority barriers to override. However, extractiveness is not maximal (0.72+) because amendment remains theoretically available and the coordination benefit (stable constitutional framework preventing constant revision) is genuine. The measurement trajectory (0.30 → 0.48 over 40 units) reflects doctrine creep: judicial review expands from narrow police-power checks to broad substantive interpretation, increasing extraction. Suppression (0.62): Moderate-high. Constitutional amendment is institutionally difficult (supermajority + state ratification or equivalent), creating structural barriers to alternative interpretations. Courts can reinterpret constitutionally without amendment, but legislatures cannot override judicial interpretation through normal legislation. This asymmetry raises suppression but does not maximize it (0.85+) because amendment remains possible for sustained majority coalitions, and courts cannot completely prevent legislative response. Theater ratio (0.55): Moderate. Judicial review involves real interpretation work (not purely theatrical) but increasingly relies on legitimacy claims about expertise and independence that exceed demonstrable epistemic advantage. As courts expand into substantive social questions (education, privacy, equal protection), the claim to specialized expertise becomes more attenuated, and theater rises. The trajectory (0.38 → 0.55) reflects this shift from narrow technical review to broad social-policy gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reflects fundamental disagreement about what 'final authority' means. For the judiciary, supremacy is coordination (stable rule-making, reduced uncertainty, institutional prestige). For electoral majorities blocked by judicial review, it is extraction (inability to override, supermajority veto against current preferences). For the legal profession, it is gatekeeping (monopoly on professional meaning-making). For the legislative body, it is mixed: coordination benefit from constitutional stability plus extraction from judicial override. The analytical observer risks collapsing this gap by naturalizing supremacy as inherent to constitutional systems, missing that supremacy is one institutional choice among alternatives (parliamentary sovereignty, popular constitutionalism). The snare perspective (blocked majorities) and the rope perspective (judiciary/legal profession) experience the same constraint mechanism (final interpretive authority through courts) as opposite structural phenomena — this perspectival reversal is diagnostic of tangled rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from the agent's structural relationship to the interpretive authority mechanism. Beneficiaries (judiciary, legal profession) occupy positions where final authority flows toward them: institutional power concentrates, expertise captures gatekeeping, career legitimacy accrues. Their d values are low (0.05-0.20): they benefit from the constraint. Victims (electoral majorities blocked by review) have d values near 1.0: they cannot override or exit. Organized agents (legislatures) with theoretical amendment authority have mid-range d values (0.50-0.65): they experience extraction but retain constrained exit. The analytical observer's d is derived from the institutional measurement context (analytical power, analytical exit): they occupy an observer position and derive d from how accessible the constraint's structure is to their analytical framework. The judiciary's arbitrage exit (ability to interpret broadly or narrowly, expanding or contracting their own authority) is the key structural fact: they can modulate the constraint's intensity without losing supremacy, giving them maximal directional advantage.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves tangled rope mandatrophy by showing that genuine coordination benefit (stable constitutional framework) coexists with real extraction (gatekeeping, blocked majorities, professional monopoly). The constraint is not pure coordination disguised as extraction (which would be a snare) nor pure extraction disguised as coordination (which would be a rope misclassified). It is structural mixture: courts genuinely do coordinate by providing a stable interpretive framework, AND they genuinely do extract by concentrating authority and blocking alternatives. The mandatrophy is resolved by acknowledging that both functions are real and that the extraction/coordination ratio changes over time and perspective. As doctrine creeps and courts expand into substantive social questions, the coordination function (stable framework) remains constant while the extraction function (gatekeeping scope) increases — this is visible in measurements showing rising extractiveness with relatively stable theater. The false summit analytical perspective is a diagnostic signal: naturalizing supremacy as inherent to constitutional governance is a common legitimacy move, but the structural data reveals it as an institutional choice with identifiable beneficiaries and victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_expertise_versus_democratic_contestation,
    'Does specialized legal expertise genuinely enable superior constitutional interpretation, or does expertise gatekeeping exclude democratic voice without epistemic justification?',
    'Longitudinal comparison of judicial interpretations vs legislative constitutional deliberations on the same issues; measurement of prediction accuracy (does legal expertise predict case outcomes better than demographic/political variables?); cross-cultural comparison of constitutional interpretation quality under parliamentary vs judicial supremacy regimes',
    'If expertise is genuinely epistemic: judicial supremacy is partially justified (transforms from pure extraction to tangled rope with real coordination benefit). If expertise is primarily gatekeeping: judicial supremacy is extraction with theatrical legitimacy (snare dynamics masked as rope). Classification boundary shifts from 0.48 to 0.62+ extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_expertise_versus_democratic_contestation, empirical, 'Whether legal expertise justifies gatekeeping or merely legitimates it').

omega_variable(
    judicial_independence_from_political_pressure_sustainability,
    'Can judicial independence actually be insulated from political pressure over generational timescales, or does political composition of courts inevitably drift toward majoritarian preferences?',
    'Longitudinal study of judicial voting patterns and stated legal reasoning relative to electoral outcomes; analysis of appointment processes and how judicial preferences correlate with nominating party positions; comparison of judicial supremacy systems with different appointment mechanisms (life tenure vs term limits vs election)',
    'If independence is sustainable: the special expertise claim is partially supported (judges can apply stable constitutional rules across political cycles). If independence erodes: judicial review becomes a delayed democratic reflection rather than a true veto (transforms snare perspective from ''blocked majorities'' to ''delayed implementation''). Suppression metric shifts from 0.62 to 0.40-0.50.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_independence_from_political_pressure_sustainability, empirical, 'Whether judicial independence from political pressure is structurally sustainable').

omega_variable(
    sibling_reading_foreclosure_or_coexistence,
    'Does the judicial supremacy reading logically foreclose the parliamentary sovereignty reading within a single constitutional framework, or can both coexist as different institutional expressions of constitutional authority?',
    'Constitutional law theory comparison: can a constitutional text simultaneously empower (a) courts with final interpretive authority AND (b) legislatures with constitutional amendment/override mechanisms without contradiction? Case analysis of mixed systems (Germany, Canada, Australia) where both exist. Normative assessment: does judicial supremacy REQUIRE legislative subordination, or is supremacy compatible with meaningful legislative constitutional authority?',
    'If supremacy forecloses parliamentary sovereignty: reading_relations should be ''forecloses'' (rare, structural contradiction). If both can coexist: reading_relations should be ''coexists_with'' (most likely — reflects real institutional competition). If judicial supremacy creates downstream pressure on parliament''s authority: reading_relations should be ''influences''. This affects how the three readings relate in the contested kernel structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_or_coexistence, conceptual, 'Whether judicial supremacy logically excludes parliamentary sovereignty or permits coexistence').

omega_variable(
    constitutional_amendment_accessibility_collapse,
    'To what degree does the high difficulty of constitutional amendment collapse alternative exit options, making the judicial veto effectively irreversible?',
    'Empirical: frequency and success rate of constitutional amendments across constitutional democracies; time cost and political capital required to amend; compare jurisdictions with different amendment thresholds (simple majority vs supermajority vs supermajority + state ratification). Structural: does the amendment mechanism remain accessible when a substantial electoral coalition opposes judicial interpretation?',
    'If amendment is genuinely difficult but accessible (true supermajority requirement reflects real coalition-building): suppression is 0.62 (correct). If amendment is effectively locked (supermajority + state veto + path-dependency makes it near-impossible): suppression approaches 0.85, constraint reclassifies toward snare from more perspectives. If amendment is relatively easy (simple majority or legislature-only): suppression drops to 0.40-0.45, tangled rope dynamics clearer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_amendment_accessibility_collapse, empirical, 'Degree to which constitutional amendment accessibility collapses exit options').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__judicial_supremacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsa_tr_t0, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(jsa_tr_t20, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(jsa_tr_t40, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(jsa_be_t0, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(jsa_be_t20, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(jsa_be_t40, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(jsa_su_t0, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(jsa_su_t20, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(jsa_su_t40, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the basic_law_interpretive_authority kernel. The three sibling readings (judicial supremacy, parliamentary sovereignty, popular constitutionalism) are separate constraint stories with different ε values, different beneficiary/victim structures, and different extracted types. The readings coexist as live institutional positions in constitutional theory and practice. Network edges link all three via affects_constraints to show the contested-kernel structure. Do NOT collapse the three readings into a single story with 'measurement basis' variation — they are genuinely different constraints with different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_authority__judicial_supremacy_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
