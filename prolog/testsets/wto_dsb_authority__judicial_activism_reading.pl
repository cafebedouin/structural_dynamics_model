% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__judicial_activism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__judicial_activism_reading, []).

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
 *   constraint_id: wto_dsb_authority__judicial_activism_reading
 *   human_readable: WTO DSB Judicial Activism: Authority Creep Through Interpretive Drift
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   The WTO Dispute Settlement Body (DSB) exemplifies a core problem in
 *   international institutional governance: how an institution chartered with
 *   narrow, defined authority gradually expands its mandate through
 *   interpretive drift, creating new obligations that exceed what member
 *   states explicitly agreed to. This constraint examines specifically the
 *   JUDICIAL_ACTIVISM_READING — the claim that DSB panels and the Appellate
 *   Body have systematically exceeded their treaty mandate by creating novel
 *   obligations through expansive interpretation of existing text, and that
 *   this authority creep is not legitimate clarification but illegitimate
 *   judicial legislation. From this reading's perspective, weak member states
 *   and developing economies bear the extraction cost (binding rulings on
 *   measures far beyond the original treaty scope), while the DSB institution
 *   and wealthy states benefit (expanded jurisdiction, enforceability of
 *   preferred interpretations, retaliation authorization). The constraint
 *   shows rising extractiveness from 0.22 (early dispute resolution, narrow
 *   mandate) to 0.58 (contemporary expanded authority), with theater_ratio
 *   climbing from 0.35 to 0.65, indicating increasing performativity: the
 *   consensus fiction persists in reports while majority/supermajority voting
 *   drives outcomes. The suppression mechanism is enforcement through
 *   retaliation authorization — member states cannot exit WTO without
 *   catastrophic trade loss, making the DSB's interpretive authority binding
 *   regardless of legitimacy objections.
 *
 * KEY AGENTS:
 *   - Dispute Settlement Body (Institution): Institutional/arbitrage — benefits from expanded authority, increased relevance, caseload growth. Experiences mandate expansion as legitimate clarification; has procedural and jurisdictional flexibility.
 *   - Weak Member States (Victims): Powerless/trapped — face binding rulings on measures far beyond explicit treaty language. No exit without trade collapse. Bear full extraction cost.
 *   - Regulatory Sovereignty Claim (Victim): Abstract collective good — treaty-granted right to regulate health, safety, labor standards eroded by expansive DSB interpretation of 'trade restriction'. Cannot organize or negotiate directly.
 *   - Wealthy Member State Bloc (Beneficiary): Institutional/arbitrage — benefits from DSB rulings protecting trademark, patent, labor-mobility interests. Use DSB as litigation advantage. See expanded authority as legitimate.
 *   - Compliant Minority State (Mixed): Organized/constrained — benefits from coordination function (predictable dispute resolution, check on larger traders) but also bears extraction cost through interpretive surprises.
 *   - Analytical Observer: Analytical/analytical — risks naturalizing interpretive expansion as inherent to any legal system, naturalizing contingent institutional choices as immutable structural features of law-in-the-world.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, 0.58).
domain_priors:suppression_score(wto_dsb_authority__judicial_activism_reading, 0.72).
domain_priors:theater_ratio(wto_dsb_authority__judicial_activism_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__judicial_activism_reading, snare).
narrative_ontology:human_readable(wto_dsb_authority__judicial_activism_reading, "WTO DSB Judicial Activism: Authority Creep Through Interpretive Drift").
narrative_ontology:topic_domain(wto_dsb_authority__judicial_activism_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__judicial_activism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__judicial_activism_reading, 'e67b6773-0672-4272-a280-3558ef77704e').
narrative_ontology:cs_kernel_codification('e67b6773-0672-4272-a280-3558ef77704e', formalized).
narrative_ontology:cs_authority_grounding('e67b6773-0672-4272-a280-3558ef77704e', extraction).
narrative_ontology:cs_interpretation_layer_present('e67b6773-0672-4272-a280-3558ef77704e').
narrative_ontology:cs_reading_relation('e67b6773-0672-4272-a280-3558ef77704e', wto_dsb_authority__binding_referee_reading, coexists_with).
narrative_ontology:cs_reading_relation('e67b6773-0672-4272-a280-3558ef77704e', wto_dsb_authority__advisory_coordination_reading, influences).
narrative_ontology:cs_axiom('e67b6773-0672-4272-a280-3558ef77704e', foundational, dsb_authority_exceeds_treaty_text).
narrative_ontology:cs_axiom_status(dsb_authority_exceeds_treaty_text, holdable).
narrative_ontology:cs_axiom_grounding('e67b6773-0672-4272-a280-3558ef77704e', dsb_authority_exceeds_treaty_text, empirically_contingent).
narrative_ontology:cs_axiom('e67b6773-0672-4272-a280-3558ef77704e', foundational, member_state_compliance_extracted_not_consented).
narrative_ontology:cs_axiom_status(member_state_compliance_extracted_not_consented, holdable).
narrative_ontology:cs_axiom_grounding('e67b6773-0672-4272-a280-3558ef77704e', member_state_compliance_extracted_not_consented, empirically_contingent).
narrative_ontology:cs_reference_frame('e67b6773-0672-4272-a280-3558ef77704e', treaty_text_bound_authority).
narrative_ontology:cs_drift_state('e67b6773-0672-4272-a280-3558ef77704e', contemporary_post_appellate_body_reform, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e67b6773-0672-4272-a280-3558ef77704e', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, dispute_settlement_body).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, wealthy_member_states).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, treaty_letter_compliance).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, weak_member_states).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, regulatory_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WEAK MEMBER STATE (SNARE) — Faces binding arbitral rulings enforced through retaliation authorization on measures far beyond the original treaty text. Cannot exit WTO without catastrophic trade loss. Bears full extraction cost with no exit path. The DSB's interpretive authority is imposed, not negotiated.
constraint_indexing:constraint_classification(wto_dsb_authority__judicial_activism_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGULATORY SOVEREIGNTY CLAIM (SNARE) — Treaty-granted right to regulate domestic health, safety, and labor standards is eroded by DSB rulings that expand 'trade restriction' beyond the literal treaty language. Constrained by retaliation threat; exit means foregoing WTO benefits entirely.
constraint_indexing:constraint_classification(wto_dsb_authority__judicial_activism_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: COMPLIANT MINORITY STATE (TANGLED ROPE) — Benefits from predictable dispute resolution (genuine coordination function) and uses DSB to check larger traders. Also bears extraction cost through interpretive surprises. Constrained by precedent; cannot simply opt out without losing coordination benefits.
constraint_indexing:constraint_classification(wto_dsb_authority__judicial_activism_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: DISPUTE SETTLEMENT BODY (ROPE) — Experiences its mandate expansion as coordination function: panels and Appellate Body see themselves as clarifying ambiguous treaty language, enabling predictable rules. The institution has arbitrage options (jurisdiction, procedures, interpretive scope) and benefits from expanded authority through increased relevance and caseload.
constraint_indexing:constraint_classification(wto_dsb_authority__judicial_activism_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: WEALTHY MEMBER STATE BLOC (ROPE) — Benefits from DSB expansions that protect their trademark, patent, and labor-mobility interests via interpretive drift (e.g., TRIPS enforcement, labor standard implications). See the broadened DSB authority as legitimate clarification of intent. Have arbitrage options: can navigate retaliation, negotiate side agreements, use DSB as litigation advantage.
constraint_indexing:constraint_classification(wto_dsb_authority__judicial_activism_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ORIGINAL CONSENSUS FICTION (PITON) — The founding narrative that 'DSB decisions rest on consensus' has become largely performative. Member states pay lip service to the consensus principle while panels and Appellate Body rule by supermajority/majority. The theater persists (consensus language in reports) but the original check on unilateral authority has atrophied.
constraint_indexing:constraint_classification(wto_dsb_authority__judicial_activism_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, any standing dispute-resolution body necessarily interprets ambiguous rules — authority creep is unavoidable, inherent to the structure of law-in-the-world. Textual indeterminacy is a feature of human language, not a flaw. No framework can prevent DSB authority expansion. However, this perspective risks naturalizing what is a contingent institutional choice: the *degree* of interpretive expansion and the *mechanism* for binding member states to novel obligations reflect specific institutional design, not immutable law.
constraint_indexing:constraint_classification(wto_dsb_authority__judicial_activism_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__judicial_activism_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wto_dsb_authority__judicial_activism_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wto_dsb_authority__judicial_activism_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(wto_dsb_authority__judicial_activism_reading, TR),
    TR >= 0.70.

:- end_tests(wto_dsb_authority__judicial_activism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated. Base extraction reflects the structural asymmetry: weak states face binding obligations created through interpretation, while wealthy states navigate the same rulings via litigation capacity and side agreements. The rising trajectory (0.22→0.58 over 20 years) indicates authority creep — the DSB's interpretive scope has expanded systematically, creating new obligations beyond the original treaty text. This is not a coordination mechanism failing; it is a deliberately constructed institutional mechanism extracting value from interpretive authority. Suppression (0.72): High. Member states cannot exit WTO without accepting catastrophic trade loss (GDP decline, unemployment, supply-chain disruption). Retaliation authorization provides enforcement — losing the DSB disputes means authorized cross-sector retaliation, making compliance binding. The suppression is structural, not merely psychological. Theater ratio (0.65): Elevated and rising. The consensus fiction persists (panel reports reference consensus language) while the actual decision mechanism is majority/supermajority voting. The performative element increases as the DSB's authority claims grow louder while the institutional checks weaken. The Appellate Body's composition, staffing, and budget constraints are presented as technical limitations, not as deliberate suppression of alternative dispute mechanisms. Claimed type (Snare): Appropriate. The constraint has minimal coordination function (the DSB does provide binding dispute resolution, but this is not a novel coordination accomplishment — it is institutional authority claiming legitimacy through the coordination frame). The primary mechanism is extraction: expanding the scope of obligations from which weak states cannot exit.
 *
 * PERSPECTIVAL GAP:
 *   The most revealing perspectival gap appears between the DSB institution (Rope) and weak member states (Snare). The DSB sees its mandate expansion as legitimate clarification of ambiguous treaty language — the constraint is a coordination mechanism solving the problem of textual indeterminacy. Weak states see the same rulings as illegitimate authority creep — binding novel obligations they never explicitly agreed to. These are not compatible interpretations of facts; they are incompatible readings of institutional legitimacy. The analytical observer (Mountain/natural law) risks collapsing this gap by naturalizing the DSB's expansion as inevitable: any legal system must interpret ambiguous text, so authority creep is unavoidable. But this naturalizes a contingent institutional choice — the degree of expansion, the enforcement mechanism, the checks on interpretive scope, and the distribution of benefits are all designed choices, not immutable features of law. The piton perspective (original consensus fiction) shows that the institutional checks have atrophied while the performative claims have strengthened — the DSB maintains its legitimacy narrative while the actual decision mechanism has shifted.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural relationship to the constraint. The weak member state (powerless/trapped) has d ≈ 0.95 — full target of extraction, no exit capacity. The wealthy bloc (institutional/arbitrage) has d ≈ 0.05 — effective beneficiary with exit flexibility. The DSB institution (institutional/arbitrage) has d ≈ 0.10 — benefits from mandate expansion, no victim status. The compliant minority state (organized/constrained) has d ≈ 0.55 — mixed position, some benefits from coordination, some costs from extraction. These d values feed the sigmoid f(d) to produce effective extractiveness chi experienced by each agent. The perspectival gap is diagnostic: beneficiaries experience rope (legitimate coordination), while victims experience snare (pure extraction with suppression), despite the same base extraction value. This gap signals that the constraint's classification depends entirely on observer position — there is no neutral vantage point from which the DSB's authority is clearly legitimate or illegitimate.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through the kernel structure. The question 'Is DSB authority legitimate?' has no single answer because the kernel (DSB authority grounding) is contested. The three readings provide three different classification outcomes from identical structural facts: the same WTO agreement text and DSB caseload produce Snare (judicial activism reading), Rope (binding referee reading), and contested-advisory (advisory reading) depending on which legitimacy framework you adopt. The mandatrophy resolves not by discovering 'the correct' type, but by recognizing that legitimacy itself is the live question. The judicial_activism_reading wins explanatory power on the empirical measure: if member states are genuinely consenting to DSB authority, we would expect less resistance to rulings over time; instead, the historical record shows increasing formal objections, exit threats, and calls for reform. The rising extractiveness values (0.22→0.58) and theater ratio (0.35→0.65) support the judicial_activism reading's account that authority is expanding while institutional checks decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_mandate_boundary,
    'Where is the boundary between ''clarifying ambiguous treaty language'' (legitimate DSB function) and ''creating new obligations beyond the treaty text'' (illegitimate judicial legislation)?',
    'Textual analysis comparing panel/Appellate Body rulings to the actual WTO agreement language; historical record of treaty negotiator intent; member state reaction trajectories to rulings (acceptance vs. resistance)',
    'If boundary is permeable/continuously sliding: judicial activism reading is correct (Snare). If boundary is stable/DSB respects it consistently: binding_referee reading is correct (Rope). If boundary cannot exist in principle: natural law reading is correct (Mountain, but false summit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_mandate_boundary, conceptual, 'Boundary between legitimate interpretation and illegitimate judicial legislation').

omega_variable(
    member_state_waiver_authenticity,
    'Do member states that nominally ''accept'' DSB rulings extending beyond treaty language genuinely consent, or do they comply under duress (retaliation threat) while privately resisting?',
    'Qualitative analysis of negotiating statements, formal objections, subsequent amendments/opt-outs; correlation between ruling acceptance and retaliatory capacity of compliant state; tracking of unilateral withdrawal threats from affected states',
    'If genuine consent: binding_referee reading plausible (institutional coordination). If duress: judicial_activism reading confirmed (Snare suppression mechanism). If mixed/state-dependent: different readings are valid from different state positions (perspectival gap is real).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(member_state_waiver_authenticity, empirical, 'Whether member state acceptance of DSB rulings is genuine consent or duress compliance').

omega_variable(
    treaty_text_indeterminacy_scope,
    'How much of the DSB''s actual caseload involves textually ambiguous passages vs. textually clear language stretched through creative interpretation?',
    'Systematic review of panel/Appellate Body reasoning in 100+ major cases; classification of each case by textual clarity of the operative provision; statistical correlation between clarity level and ruling expansiveness',
    'If most cases involve genuine ambiguity: interpretive expansion is a necessary response to indeterminacy (natural law and binding_referee readings gain support). If substantial portion involves clear text stretched: judicial activism reading is empirically confirmed. The reading balance determines whether authority creep is inevitable or contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_text_indeterminacy_scope, empirical, 'Proportion of DSB caseload involving textual ambiguity vs. clear-text stretching').

omega_variable(
    alternative_obligation_source,
    'Are the ''new obligations'' that weak states experience genuinely novel creations by DSB interpretation, or are they implicit in the treaty text but latent (not activated until DSB ruling)?',
    'Comparative treaty analysis: other similar trade agreements'' language; expert testimony from negotiators about intent; examination of preparatory documents and negotiating records; test whether obligation would be discoverable by good-faith treaty interpretation under Vienna Convention Article 31',
    'If genuinely novel creations: judicial activism reading is strongest (Snare). If latent but implicit: binding_referee reading is stronger (legitimate clarification). If implicit-but-latent framing is contested: the kernel reading_relations structure is the answer (coexists_with, not forecloses).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_obligation_source, conceptual, 'Whether DSB-created obligations are novel or were implicit/latent in the treaty').

omega_variable(
    institutional_incentive_capture,
    'Does the DSB institution have structural incentives to expand its interpretive authority (caseload growth, institutional prestige, staffing expansion)?',
    'Institutional economics analysis: tracking of DSB budget, staff, and caseload over time; correlation between authority expansion and institutional growth; comparison to other international courts'' expansion trajectories; examination of panel/Appellate Body composition turnover and incentive structures',
    'If strong institutional capture incentives exist: judicial activism reading is reinforced (extraction mechanism is institutional self-interest). If incentives are neutral: DSB expansion may be principled. If incentives are actively constraining: DSB expansion is an honest struggle with indeterminacy (natural law reading gains plausibility).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_incentive_capture, empirical, 'Whether DSB institution has structural incentives to expand interpretive authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__judicial_activism_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_dsb_jud_theater_t0, wto_dsb_authority__judicial_activism_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(wto_dsb_jud_theater_t10, wto_dsb_authority__judicial_activism_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(wto_dsb_jud_theater_t20, wto_dsb_authority__judicial_activism_reading, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(wto_dsb_jud_extr_t0, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(wto_dsb_jud_extr_t10, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(wto_dsb_jud_extr_t20, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(wto_dsb_jud_supp_t0, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(wto_dsb_jud_supp_t10, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(wto_dsb_jud_supp_t20, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__judicial_activism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__advisory_coordination_reading).

% DUAL FORMULATION NOTE:
% The wto_dsb_authority kernel decomposes into three structurally distinct readings, each with its own constraint story. The judicial_activism_reading (this file) frames DSB authority as illegitimate expansion; the binding_referee_reading frames it as legitimate clarification; the advisory_coordination_reading frames the binding mechanism itself as illegitimate. These are not three observations of one constraint — they are three constraints instantiated by different legitimacy framings of the same institutional phenomenon. Linked via network.affects_constraints to enable committer-structure analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_dsb_authority__judicial_activism_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
