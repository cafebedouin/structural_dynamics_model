% ============================================================================
% CONSTRAINT STORY: border_legitimacy__freedom_of_movement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__freedom_of_movement_reading, []).

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
 *   constraint_id: border_legitimacy__freedom_of_movement_reading
 *   human_readable: Border Restriction as Violation of Freedom of Movement (Cosmopolitan Reading)
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   Under the freedom-of-movement reading, border restrictions are treated as
 *   violations of an inalienable human right. The constraint's primary
 *   function is not coordination but extraction: wealthy nation-states and
 *   their high-wage workers benefit from restricted labor supply, while
 *   displaced workers, economic migrants, and welfare recipients bear the
 *   enforcement cost. The reading rejects the sovereignty and humanitarian
 *   framings as cover stories for structural extraction. This story
 *   instantiates ONE reading of the contested border_legitimacy kernel; it
 *   does not adjudicate between readings. The high extractiveness and
 *   suppression scores reflect the reading's own assessment of how the
 *   constraint operates, not a neutral measurement.
 *
 * KEY AGENTS:
 *   - wealthy_nation_states — institutional agenda-setter controlling border enforcement, benefiting from wage suppression
 *   - displaced_workers and economic_migrants — powerless, trapped payers bearing the cost of exclusion
 *   - capital_holding_domestic_workers — powerful beneficiaries gaining from restricted labor supply
 *   - welfare_receiving_citizens — powerless payers and contingent beneficiaries, framed as under threat
 *   - enforcement_apparatus — institutional agenda-setter fused with constraint maintenance
 *   - cosmopolitan advocates — excluded challengers with no authority over enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, 0.81).
domain_priors:suppression_score(border_legitimacy__freedom_of_movement_reading, 0.88).
domain_priors:theater_ratio(border_legitimacy__freedom_of_movement_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__freedom_of_movement_reading, snare).
narrative_ontology:human_readable(border_legitimacy__freedom_of_movement_reading, "Border Restriction as Violation of Freedom of Movement (Cosmopolitan Reading)").
narrative_ontology:topic_domain(border_legitimacy__freedom_of_movement_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__freedom_of_movement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__freedom_of_movement_reading, 'e44bbe31-ce9b-4d1b-8600-b146198eac5b').
narrative_ontology:cs_kernel_codification('e44bbe31-ce9b-4d1b-8600-b146198eac5b', formalized).
narrative_ontology:cs_authority_grounding('e44bbe31-ce9b-4d1b-8600-b146198eac5b', extraction).
narrative_ontology:cs_interpretation_layer_present('e44bbe31-ce9b-4d1b-8600-b146198eac5b').
narrative_ontology:cs_reading_relation('e44bbe31-ce9b-4d1b-8600-b146198eac5b', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('e44bbe31-ce9b-4d1b-8600-b146198eac5b', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('e44bbe31-ce9b-4d1b-8600-b146198eac5b', foundational, freedom_of_movement_inalienable).
narrative_ontology:cs_axiom_status(freedom_of_movement_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('e44bbe31-ce9b-4d1b-8600-b146198eac5b', freedom_of_movement_inalienable, deontological).
narrative_ontology:cs_axiom('e44bbe31-ce9b-4d1b-8600-b146198eac5b', foundational, human_dignity_transcends_territory).
narrative_ontology:cs_axiom_status(human_dignity_transcends_territory, holdable).
narrative_ontology:cs_axiom_grounding('e44bbe31-ce9b-4d1b-8600-b146198eac5b', human_dignity_transcends_territory, deontological).
narrative_ontology:cs_reference_frame('e44bbe31-ce9b-4d1b-8600-b146198eac5b', cosmopolitan_moral_equality).
narrative_ontology:cs_drift_state('e44bbe31-ce9b-4d1b-8600-b146198eac5b', contemporary_border_hardening, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e44bbe31-ce9b-4d1b-8600-b146198eac5b', '').
narrative_ontology:cs_kernel_id(border_legitimacy__freedom_of_movement_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, wealthy_nation_states).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, capital_holding_domestic_workers).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, displaced_workers).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, economic_migrants).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, welfare_receiving_citizens).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, asylum_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, welfare_receiving_citizens).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, enforcement_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce border policy through military, immigration bureaucracy, and detention infrastructure. Justify restrictions as protecting citizens and managing resources. Benefit from wage suppression (reduced labor supply to high-wage jurisdictions), reduced welfare costs (excluding those without citizenship), and political stability (maintaining citizen coalitions). Can change policy unilaterally.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, wealthy_nation_states, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Prevented by law and force from moving to higher-opportunity jurisdictions. Bear poverty-level wages in origin countries or deportation/detention risk if attempting irregular crossing. No legal exit except through state discretion (visa lottery, asylum approval — all probabilistic and uncertain). Identity is bound to place of birth; mobility is legally forbidden and culturally delegitimized.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, displaced_workers, payer,
    powerless, biographical, trapped, global).

% Seek entry to labor markets with better wages and conditions. Blocked by law from legal pathways except narrow visa categories (work permits tied to employers — inducing dependency, exploitation). Must accept wage suppression in origin countries or undertake dangerous irregular migration. The constraint treats their mobility as a threat, not an exercise of human freedom.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, economic_migrants, payer,
    powerless, biographical, trapped, global).

% Framed by policy-makers as beneficiaries of border protection (job preservation, welfare access exclusive to citizens). In fact, constrained by the same labor-mobility limits that trap migrants. Benefit from state welfare that migrants are denied, but face pressure from low-wage competition framed as externally imposed rather than structurally embedded. Secondary beneficiary role is contingent on accepting that migrants are the problem.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, welfare_receiving_citizens, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__freedom_of_movement_reading, welfare_receiving_citizens, beneficiary).

% Benefit from restricted labor supply (wage suppression of lower-wage competitors prevented), ability to move themselves and capital across borders (high-skill visa preference, investor visas), and political coalition stability. Navigate restricted regimes through wealth and networks; receive preferential admission. Border control protects their relative advantage by keeping global labor supply fragmented by jurisdiction.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, capital_holding_domestic_workers, beneficiary,
    powerful, civilizational, arbitrage, global).

% Immigration authorities, border guards, detention operators. Enforces exclusion through surveillance, apprehension, detention, deportation. Institutional identity fused with constraint maintenance; cost of operation is budgeted and normalized. Subject to mandate creep: enforcement infrastructure persists and expands even when migration pressure varies. Secondary payer role: carries the cost of maintaining enforcement capacity, though wages are provided by the states they serve.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, enforcement_apparatus, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__freedom_of_movement_reading, enforcement_apparatus, payer).

% Argue that freedom of movement is a universal human right and that border restriction violates it. Excluded from border-setting authority; operate through courts, legislatures, advocacy, international forums. Structurally out of the enforcement decision-making loop. Subject to delegitimization as naive, traitorous, or utopian by political actors who benefit from borders.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, cosmopolitan_advocates, excluded,
    moderate, generational, constrained, global).

% Monitor whether border restriction violates freedom of movement rights. Produce reports, recommendations, litigation venues. Lack enforcement power; verdicts depend on whether nation-states consent to jurisdiction. Their seat is analytical but not neutral: they represent this reading (freedom of movement as human right) against others (sovereignty, humanitarian limitations).
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__freedom_of_movement_reading, wealthy_nation_states).
narrative_ontology:fixing_cost_class(border_legitimacy__freedom_of_movement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading: none. The constraint is not understood as solving a coordination problem but as pure extraction masquerading as sovereignty. (The sovereignty reading frames borders as coordinating territory; the humanitarian reading frames them as coordinating obligation. This reading rejects both framings.)
% TRANSFER_FUNCTION: Moves opportunity cost, dignity, and life trajectory from powerless displaced workers and economic migrants to wealthy nation-states and their high-wage citizens. Enforced through violence (walls, detention, deportation) and legal exclusion. The transfer is justified as protection of citizens, but the reading treats that justification as cover for structural extraction.
% ABSENT_VOICES: Migrants are the primary subjects of the constraint but largely excluded from border-policy deliberation, except through humanitarian exemptions that reinforce their subordinate status. Cosmopolitan advocates are present in some jurisdictions but excluded from enforcement authority. The constraint is set by those who benefit from it, not by those who bear it.
% DISAPPEARANCE_RATIONALE: If border restriction disappeared, labor would move to equalize opportunity; wage gaps between jurisdictions would compress; state revenues would rebalance; welfare systems would reorganize; political coalitions would shift (likely around capital vs. labor rather than citizen vs. migrant). Present distributions of wealth and opportunity would not persist in their current form.
% FOUNDING_PROBLEM: Under this reading, the stated founding problem (coordinating national projects, protecting citizens) is a retrospective rationalization. Borders were established through colonialism, warfare, and state consolidation — not through consent to solve a shared coordination problem. The reading treats the founding story as ideological cover for the extraction of rents through position monopoly.
% FOUNDING_PROBLEM_CORROBORATION: Historians document that borders were imposed through conquest and force, not through deliberation to solve coordination problems. Contemporary evidence: wealthy nations restrict movement not because coordination is impossible without borders, but because wage suppression and electoral politics favor restriction. External corroborators: scholars of colonialism (Parminder Bains, historians of partition), development economists (Michael Clemens on migration's poverty reduction), human rights bodies (UN Office on Drugs and Crime on migration as human right, International Labour Organization on freedom of movement as foundational).
narrative_ontology:disappearance_verdict(border_legitimacy__freedom_of_movement_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__freedom_of_movement_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__freedom_of_movement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_legitimacy__freedom_of_movement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__freedom_of_movement_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__freedom_of_movement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__freedom_of_movement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) because this reading denies any coordination benefit; the constraint functions purely to concentrate opportunity. Suppression is higher still (0.88) because the reading emphasizes that movement is prevented by violence (walls, detention, deportation) and law, not by neutral facts. Theater is moderate (0.42) because humanitarian asylum exceptions, refugee resettlement language, and integration programs provide performative legitimacy while the core extraction persists. The measurement series run on a shared time grid from t=0 to t=40 to model enforcement intensification: suppression_requirement rises as border infrastructure hardens (surveillance technology, biometric systems, detention capacity); extractiveness rises as enforcement becomes more costly to maintain, hence the wage suppression must deepen to justify it; theater ratio rises as humanitarian language is deployed to manage public opinion while exclusion intensifies. Accessibility collapse is high (0.78) because alternatives to border restriction are not merely suppressed but rendered cognitively unavailable in dominant discourse (borders are treated as natural/inevitable).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (wealthy nation-states) seats this constraint as legitimate sovereignty exercised to manage citizens' welfare. The payer seats experience it as unjust restriction preventing survival-level mobility. The engine computes per-seat type from power, exit, and beneficiary/victim structural data: institutional powerful beneficiaries will compute as Rope (coordination), while powerless trapped victims will compute as Snare (extraction). This divergence is structural, not a measurement error — the constraint operates differently at different power levels.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy nation-states hold d near 0.0 (full beneficiary): they set the rules, benefit from wage suppression, can move capital and citizens across borders, have arbitrage exit. Displaced workers hold d near 1.0 (full target): they bear the direct enforcement cost through poverty, legal exclusion, deportation risk, and identity-lock (place of birth determines opportunity). Economic migrants are similarly trapped at high d. Capital-holding workers hold d near 0.1–0.2 (beneficiary with minor constraint cost, since they can afford visa processes and benefit from wage suppression). Welfare-receiving citizens hold d near 0.5–0.6 (moderate target, since they are protected by citizenship but compete for low-wage work and are politically framed as under threat). The enforcement apparatus itself sits near 0.5–0.7 (payer role as secondary, since maintenance is institutionally mandatory but beneficiary role is weak — they do not collect rents, only wages). Directionality-override is not necessary; the derivation chain from beneficiary/victim + power + exit produces the right structure.
 *
 * MANDATROPHY ANALYSIS:
 *   Under this reading, the founding problem (if any) is already dead: nation-states do not restrict movement to solve a live coordination problem but to maintain a distribution of advantage established through colonial violence. The constraint's persistence despite the dead founding problem is a strong piton signal. However, the high suppression requirement (0.88) and rising extraction (over the interval) argue AGAINST piton classification. A piton would be mostly theater with low real enforcement cost. This constraint requires serious violence and bureaucratic investment to maintain. The reading classifies it as Snare (pure extraction, active enforcement, trapped victims) rather than Piton (degraded coordination, mostly performance). The mandatrophy question — whether the constraint's mandate has outlived its function — resolves as YES under this reading, but the constraint persists not through inertia but through active reinforcement by benefiting parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foundational_right_status,
    'Is freedom of movement truly a foundational inalienable human right, or is it a contingent good whose allocation states may legitimately regulate?',
    'Cross-cultural survey of moral intuitions and political philosophy traditions; empirical evidence on whether freedom of movement is cited as foundational in the same way as bodily autonomy or conscience.',
    'If foundational, border restriction is extraction by definition; if contingent, the extraction claim depends on consequentialist arguments about welfare effects. The constraint''s type shifts from Snare (under foundational) to potentially Tangled Rope (under contingent, if coordination benefits exist).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foundational_right_status, preference, 'Whether freedom of movement is axiomatically inalienable or contingently regulated.').

omega_variable(
    coordination_contribution_of_borders,
    'Do borders materially contribute to solving genuine coordination problems (national defense, welfare provision, public goods) that would otherwise fail, or are these coordination functions orthogonal to border restriction?',
    'Comparison of coordination outcomes in open and closed systems; empirical evidence on whether removing border restriction (while keeping territorial governance intact) degrades public goods provision.',
    'If orthogonal, borders are pure extraction; if borders are necessary for coordination, the extraction is mixed with coordination cost. This determines whether the computed type is Snare (pure extraction) or Tangled Rope (mixed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_contribution_of_borders, empirical, 'Whether border restriction is structurally necessary for the coordination benefits attributed to it.').

omega_variable(
    suppression_internalization_vs_structural,
    'Is the measured suppression primarily structural (legal barriers, physical walls, deportation threat) or internalized (migrants internalize the boundary as legitimate, accept poverty as deserved, have lost sense of entitlement to movement)?',
    'Post-legalization outcomes: if migrants who gain legal entry show rapid wage growth and reframe their trajectory, suppression was primarily structural; if internalization persists after legal barriers removed, suppression was internalized.',
    'If structural, the constraint is externally imposed; if internalized, the constraint''s effective suppression exceeds the authored measure, and targets carry it across exit. Identity-lock exit_option assignment would shift from ''trapped'' to something closer to ''constrained with internalized belief'' — a new exit class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_vs_structural, empirical, 'Mechanism of suppression: external barriers vs. internalized boundary acceptance.').

omega_variable(
    class_solidarity_vs_citizen_framing,
    'Do welfare-receiving citizens genuinely benefit from border restriction (via wage protection), or is their framing as beneficiaries a false coalition that extraction uses to divide potential allies?',
    'Comparative wage data controlling for border policy changes; evidence on whether welfare-receiving citizens gain more from labor mobility (accessing higher-wage markets) than from border protection (reduced wage competition).',
    'If false coalition, welfare-receiving citizens should be reclassified as victims; this would deepen the Snare classification by showing broader victim set. If genuine benefit, they remain secondary beneficiaries and the class-solidarity omega remains unresolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(class_solidarity_vs_citizen_framing, empirical, 'Whether welfare-receiving citizens are genuine beneficiaries or mistaken allies of extraction.').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Is the kernel''s legitimate locus of authority the nation-state (sovereignty_reading), the human person (freedom_of_movement_reading), or the international humanitarian community (humanitarian_obligation_reading)?',
    'Genealogical analysis of which authority locus is actually operative in border policy (states set policy; persons are subjects; humanitarian bodies lack enforcement); empirical observation of which reading produces predictive power for state behavior.',
    'If the nation-state is actually the operative locus (contra freedom_of_movement reading''s normative claim), the reading''s ε assessment becomes a critique rather than a description. This does not invalidate the reading but relocates it to preference/normative rather than empirical grounds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Which reading''s framing of legitimate authority is empirically operative vs. normatively endorsed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__freedom_of_movement_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_legitimacy__freedom_of_movement_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(bord_tr_t0, observed).
narrative_ontology:measurement(bord_tr_t8, border_legitimacy__freedom_of_movement_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(bord_tr_t8, observed).
narrative_ontology:measurement(bord_tr_t16, border_legitimacy__freedom_of_movement_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement_basis(bord_tr_t16, observed).
narrative_ontology:measurement(bord_tr_t24, border_legitimacy__freedom_of_movement_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement_basis(bord_tr_t24, observed).
narrative_ontology:measurement(bord_tr_t32, border_legitimacy__freedom_of_movement_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement_basis(bord_tr_t32, observed).
narrative_ontology:measurement(bord_tr_t40, border_legitimacy__freedom_of_movement_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(bord_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement_basis(bord_be_t0, observed).
narrative_ontology:measurement(bord_be_t8, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 8, 0.69).
narrative_ontology:measurement_basis(bord_be_t8, observed).
narrative_ontology:measurement(bord_be_t16, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 16, 0.74).
narrative_ontology:measurement_basis(bord_be_t16, observed).
narrative_ontology:measurement(bord_be_t24, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 24, 0.78).
narrative_ontology:measurement_basis(bord_be_t24, observed).
narrative_ontology:measurement(bord_be_t32, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 32, 0.8).
narrative_ontology:measurement_basis(bord_be_t32, observed).
narrative_ontology:measurement(bord_be_t40, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement_basis(bord_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(bord_su_t0, observed).
narrative_ontology:measurement(bord_su_t8, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 8, 0.76).
narrative_ontology:measurement_basis(bord_su_t8, observed).
narrative_ontology:measurement(bord_su_t16, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 16, 0.8).
narrative_ontology:measurement_basis(bord_su_t16, observed).
narrative_ontology:measurement(bord_su_t24, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 24, 0.84).
narrative_ontology:measurement_basis(bord_su_t24, observed).
narrative_ontology:measurement(bord_su_t32, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 32, 0.87).
narrative_ontology:measurement_basis(bord_su_t32, observed).
narrative_ontology:measurement(bord_su_t40, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 40, 0.88).
narrative_ontology:measurement_basis(bord_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__freedom_of_movement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_legitimacy__freedom_of_movement_reading, 0.12).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% The border_legitimacy kernel decomposes into three constraint stories, one per reading. All three share the same referent (border enforcement as a standing institutional practice) but instantiate different ε values, beneficiary/victim structures, and classifications depending on which normative reading grounds the assessment. This story (freedom_of_movement_reading) treats the kernel's legitimacy as grounded in human dignity and inalienable freedom. The sovereignty_reading grounds legitimacy in territorial sovereignty and state self-determination. The humanitarian_obligation_reading grounds legitimacy in minimalist obligation to those fleeing catastrophe. Each reading is a live position held by different parties; they coexist without logical resolution. The freedom_of_movement reading influences the other two by establishing freedom of movement as an axiom against which alternatives are measured (sovereignty and humanitarian readings are understood as limitations on or exceptions to an underlying right).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_legitimacy__freedom_of_movement_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
