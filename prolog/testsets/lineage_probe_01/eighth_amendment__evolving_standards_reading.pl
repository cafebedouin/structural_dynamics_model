% ============================================================================
% CONSTRAINT STORY: eighth_amendment__evolving_standards_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eighth_amendment__evolving_standards_reading, []).

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
 *   constraint_id: eighth_amendment__evolving_standards_reading
 *   human_readable: Eighth Amendment Evolving Standards Reading: Cruel and Unusual Punishment
 *   domain: constitutional_law/doctrinal_interpretation
 *
 * SUMMARY:
 *   The Eighth Amendment's text — 'Excessive bail shall not be required, nor
 *   excessive fines imposed, nor cruel and unusual punishments inflicted' —
 *   anchors competing constitutional readings that divide not on textual
 *   meaning but on the mechanism by which 'cruel and unusual' acquires
 *   content. The evolving-standards reading holds that the Eighth Amendment
 *   grounds itself in contemporary standards of decency that courts identify
 *   and apply, permitting progressive narrowing of the scope of permissible
 *   punishment as societies mature morally. This reading instantiates one
 *   structural constraint — a tangled rope mixing genuine coordination (the
 *   doctrine legitimizes incremental constitutional evolution) with
 *   asymmetric extraction (it progressively constrains retributive policy
 *   choices and concentrates standard-setting authority in federal courts).
 *   The constraint's beneficiaries are the categories progressively deemed
 *   categorically ineligible (juveniles via Roper, intellectually disabled
 *   via Atkins, those with severe mental illness increasingly via evolving
 *   case law). The constraint's victims are retributive maximalism (the
 *   institutional commitment to proportional punishment without categorical
 *   exclusions) and individual defendants whose fates depend on whether their
 *   category has crossed the decency threshold when their appeal reaches
 *   court. The measurement interval (1972–2012) captures the doctrine's
 *   development from the reinstatement of capital punishment (Furman to Gregg
 *   transition) through the categorical exclusion era (Roper 2005, Miller
 *   2012), showing monotonic increases in extractiveness, theater ratio, and
 *   suppression requirement as the doctrine's scope expands and its
 *   gatekeeping function becomes more pronounced.
 *
 * KEY AGENTS:
 *   - Death-row inmates (powerless/trapped): Primary victims — live-or-die stakes depend on whether their demographic category has been deemed categorically ineligible; no prospective clarity; no exit.
 *   - Criminal justice reform advocates (moderate/constrained): Secondary beneficiaries — gain legitimate legal channel for narrowing capital punishment but constrained by doctrine's gradualism and consensus-gatekeeping.
 *   - Federal appellate judiciary (institutional/arbitrage): Primary beneficiary — framework provides interpretive flexibility and legitimacy for incremental constitutional evolution without appearing activist.
 *   - State legislatures & retributive coalition (powerful/mobile): Secondary victims — experience narrowing discretion over capital punishment eligibility; constrained by doctrine but mobile (can exit federal system, though courts override via supremacy).
 *   - Moral progress narrative (analytical): Beneficiary — doctrine legitimizes progressive moral advancement through judicial recognition; requires that contemporary standards are objectively superior to historical ones.
 *   - Retributive maximalism (analytical/institutional): Victim — doctrine treats retributive proportionality as constrained by evolving decency standards rather than as independent constitutional value.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eighth_amendment__evolving_standards_reading, 0.38).
domain_priors:suppression_score(eighth_amendment__evolving_standards_reading, 0.62).
domain_priors:theater_ratio(eighth_amendment__evolving_standards_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eighth_amendment__evolving_standards_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(eighth_amendment__evolving_standards_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(eighth_amendment__evolving_standards_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eighth_amendment__evolving_standards_reading, tangled_rope).
narrative_ontology:human_readable(eighth_amendment__evolving_standards_reading, "Eighth Amendment Evolving Standards Reading: Cruel and Unusual Punishment").
narrative_ontology:topic_domain(eighth_amendment__evolving_standards_reading, "constitutional_law/doctrinal_interpretation").

domain_priors:requires_active_enforcement(eighth_amendment__evolving_standards_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eighth_amendment__evolving_standards_reading, 'f1ce462e-efd4-4f3c-b8a2-d89a520cd0d0').
narrative_ontology:cs_kernel_codification('f1ce462e-efd4-4f3c-b8a2-d89a520cd0d0', fixed_text).
narrative_ontology:cs_authority_grounding('f1ce462e-efd4-4f3c-b8a2-d89a520cd0d0', lineage).
narrative_ontology:cs_interpretation_layer_present('f1ce462e-efd4-4f3c-b8a2-d89a520cd0d0').
narrative_ontology:cs_reading_relation('f1ce462e-efd4-4f3c-b8a2-d89a520cd0d0', eighth_amendment__fixed_original_meaning_reading, coexists_with).
narrative_ontology:cs_axiom('f1ce462e-efd4-4f3c-b8a2-d89a520cd0d0', foundational, moral_progress_via_consensus_evolution).
narrative_ontology:cs_axiom_status(moral_progress_via_consensus_evolution, holdable).
narrative_ontology:cs_axiom_grounding('f1ce462e-efd4-4f3c-b8a2-d89a520cd0d0', moral_progress_via_consensus_evolution, deontological).
narrative_ontology:cs_axiom('f1ce462e-efd4-4f3c-b8a2-d89a520cd0d0', secondary, judicial_consensus_determination_authority).
narrative_ontology:cs_axiom_status(judicial_consensus_determination_authority, holdable).
narrative_ontology:cs_axiom_grounding('f1ce462e-efd4-4f3c-b8a2-d89a520cd0d0', judicial_consensus_determination_authority, conventional).
narrative_ontology:cs_reference_frame('f1ce462e-efd4-4f3c-b8a2-d89a520cd0d0', founding_era_baseline_standards).
narrative_ontology:cs_drift_state('f1ce462e-efd4-4f3c-b8a2-d89a520cd0d0', contemporary_moral_consensus_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('f1ce462e-efd4-4f3c-b8a2-d89a520cd0d0', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(eighth_amendment__evolving_standards_reading, eighth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eighth_amendment__evolving_standards_reading, protected_categories).
narrative_ontology:constraint_beneficiary(eighth_amendment__evolving_standards_reading, moral_progress_narrative).
narrative_ontology:constraint_victim(eighth_amendment__evolving_standards_reading, retributive_maximalism).
narrative_ontology:constraint_victim(eighth_amendment__evolving_standards_reading, institutional_punishment_discretion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POWERLESS TRAPPED AGENT (SNARE) — A capital defendant whose constitutionality claim depends on whether their category (juvenile, intellectually disabled, or one newly deemed categorically ineligible) has crossed the evolving-standards threshold. They experience the constraint as maximum extraction: their life depends on a moving target of 'decency standards' that courts determine retrospectively. No exit; no alternatives; no agency in the standard-setting process. The constraint's suppression mechanism is the lack of prospective clarity — decency standards evolve, but individuals cannot know their status until appellate review.
constraint_indexing:constraint_classification(eighth_amendment__evolving_standards_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MODERATE CONSTRAINED AGENT (TANGLED ROPE) — Advocates benefit from the evolving-standards doctrine as a legitimate legal vehicle for narrowing capital punishment (real victories: Atkins, Roper, Miller). But they are constrained by the doctrine's gradualism — each category requires years of litigation, empirical briefs, and consensus-building. They experience genuine coordination (the doctrine channels reform through legitimate channels) and genuine extraction (the process is slow, resource-intensive, and subject to political backlash). The doctrine's suppression mechanism is the 'decency standards' gatekeeping: only changes already reflected in contemporary society can penetrate; reformers must wait for consensus to shift, then litigate to recognize it.
constraint_indexing:constraint_classification(eighth_amendment__evolving_standards_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL BENEFICIARY (ROPE) — Federal judges experience evolving-standards doctrine as a coordination mechanism: it provides a legitimate framework for incremental constitutional evolution without appearing to legislate. Judges benefit from the doctrine's flexibility (arbitrage capacity) — they can identify emerging consensus and recognize it without appearing activist. The doctrine generates no extraction from this perspective; it is pure coordination: legitimizing judicial discretion while maintaining fidelity to constitutional text. Suppression is minimal here — judges have substantial interpretive freedom within the evolving-standards framework.
constraint_indexing:constraint_classification(eighth_amendment__evolving_standards_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POWERFUL MOBILE AGENT (TANGLED ROPE) — State legislators in retributive-majority jurisdictions experience the evolving-standards doctrine as mixed extraction and coordination. They benefit from the coordination logic (capital punishment is still available for many categories; the doctrine creates predictability about which categories remain eligible). But they experience extraction: the doctrine progressively narrows the population eligible for capital punishment, constraining their retributive policy choices. They have high mobile exit capacity (states can diverge on punishment philosophy) but choose to operate within federal constitutional bounds. Suppressiveness: moderate — states retain discretion within the narrowing boundaries.
constraint_indexing:constraint_classification(eighth_amendment__evolving_standards_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL PITON (DEGRADED FUNCTION) — The original institutional function of the Eighth Amendment was to prevent torture and barbaric modes (drawing and quartering, burning alive, etc.) without substantially limiting retributive severity or scope. The evolving-standards reading has converted the amendment into an instrument of progressive moral revision — decency is now defined by contemporary Eighth Circuit majorities' assessment of moral progress, not by historical barbarism thresholds. This is institutional function degradation: the original purpose (prevent extraneous cruelty) has been absorbed into a new purpose (reflect evolving societal standards of acceptable punishment). The theater here is the invocation of 'evolving standards' as a neutral discovery mechanism when it is actually a value-laden framework permitting progressive reinterpretation. Theater ratio 0.55 reflects that the doctrine has real institutional legitimacy (judges genuinely are attempting to track consensus) but substantial performativity (the appearance of neutrality masks normative choices about which consensus matters).
constraint_indexing:constraint_classification(eighth_amendment__evolving_standards_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL MOUNTAIN (NATURAL LAW / FALSE SUMMIT CANDIDATE) — From a civilizational analytical perspective treating 'decency' as an objective, discoverable standard that evolves with moral progress, the constraint appears as a natural law: courts are discovering and applying an objective standard that exists independently of any court's declaring it. This perspective assumes that societies genuinely progress morally, that contemporary standards ARE more enlightened than historical ones, and that judicial recognition of this progression is constraint-following rather than norm-setting. However, the structural data identifies this as a false summit: 'decency' is grounded in contestable axioms about moral progress (whether contemporary standards are inherently superior); beneficiaries (reform-minded constituencies) are identifiable; and the extractiveness varies by temporal scope (from immediate perspective, the constraint extracts from retributive maximalists; from historical perspective, it constrains capital punishment). The mountain classification is aspirational — the reading CLAIMS to discover objective decency standards, but the structural analysis reveals this as a constructed constraint benefiting particular agents.
constraint_indexing:constraint_classification(eighth_amendment__evolving_standards_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: ANALYTICAL SNARE (FROM RETRIBUTIVE PERSPECTIVE) — From a retributive maximalist analytical standpoint, the evolving-standards reading appears as a snare: it claims to apply an objective constitutional standard while progressively narrowing the scope of permissible punishment through judicial reinterpretation. Retributivists experience this as extraction masquerading as discovery — each generation's courts impose their moral sensibilities retroactively, constraining earlier legislative choices. The constraint suppresses retributive alternatives (states cannot adopt retributive sentences without federal judicial veto) while maintaining the appearance of constitutional fidelity. From this perspective, extractiveness is high (0.72+) because the doctrine progressively voids retributive choices made under prior constitutional understandings. This perspective is analytically coherent but structurally opposite to the moral-progress perspective — both cannot simultaneously be correct.
constraint_indexing:constraint_classification(eighth_amendment__evolving_standards_reading, snare,
    context(agent_power(analytical),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eighth_amendment__evolving_standards_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eighth_amendment__evolving_standards_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eighth_amendment__evolving_standards_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(eighth_amendment__evolving_standards_reading, TR),
    TR >= 0.70.

:- end_tests(eighth_amendment__evolving_standards_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): The constraint extracts asymmetrically from retributive maximalism and individual defendants while benefiting reform advocates and the moral-progress narrative. However, the extractiveness is moderate (not severe) because: (1) coordination genuinely occurs — the doctrine provides a legitimate constitutional vehicle for incremental reform rather than forcing extrajudicial mechanisms; (2) beneficiaries of protection (juveniles, intellectually disabled) gain real constitutional immunity, not illusory protections; (3) the doctrine maintains capital punishment for broader categories, preserving retributive scope at higher thresholds. The measurement trajectory shows extractiveness rising from 0.15 (1972: Furman era, minimal categorical exclusions) to 0.38 (2012: multiple categorical exclusions established, doctrine's full scope apparent). Suppression (0.62): Moderate-high. The suppression mechanism operates through gatekeeping of the 'decency standard' determination. Federal courts determine which categories have crossed the consensus threshold, with individual defendants bearing full downside risk if their category is deemed not yet protected. The lack of prospective clarity about which categories will be recognized contributes significantly. Measurement trajectory shows suppression rising from 0.40 (1972: pre-categorical era) to 0.62 (2012: multiple exclusions, clear institutional hierarchy of determination). Theater ratio (0.55): Moderate-high. The doctrine invokes 'evolving standards' and 'decency' as neutral, discoverable principles when substantial normative choices are embedded in which consensus to recognize, how to measure it, and whether it binds lower courts. The performativity is not pure (the doctrine does track some real moral shifts and operates through legitimate interpretive channels) but substantial (the appearance of neutral discovery masks value-laden choices). Measurement trajectory shows theater rising from 0.30 (1972: recent reinstatement, doctrine underdeveloped) to 0.55 (2012: doctrine fully elaborated, multiple gatekeeping steps in place).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a fundamental perspectival inversion based on whether the observer accepts the 'moral progress' frame. Agents who view contemporary standards as objectively more enlightened (reform advocates, moral-progress narrative) experience the constraint as tangled rope or scaffold — genuine coordination with manageable extraction. Agents who view retributive proportionality as an independent constitutional value constrained by external 'decency' standards (retributive maximalists, fixed-original-meaning advocates) experience the constraint as snare — extraction masquerading as discovery. Death-row inmates experience pure snare regardless of which normative frame they accept — their lives depend on when the Supreme Court recognizes their category, a determination they cannot influence. The federal judiciary experiences pure rope — the doctrine provides legitimacy and interpretive flexibility. The piton perspective highlights institutional function degradation: the original Eighth Amendment purpose (preventing extraneous modes of torture) has been absorbed into a new purpose (reflecting evolving moral standards). The mountain perspective risks naturalizing the 'moral progress' frame as objective fact rather than contestable interpretation. The analytical snare perspective (from retributive standpoint) demonstrates that the same structural constraint can be classified as snare from one analytical position and tangled rope from another analytical position, depending on axioms about moral progress and retributive fairness.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by perspective based on the agent's power level, exit options, and beneficiary/victim status. Powerless trapped agents (death-row inmates) have d ≈ 0.95 (full targets of extraction). Moderate constrained agents (reform advocates) have d ≈ 0.55 (mixed — they benefit from the legal channel but constrained by its gradualism). Institutional arbitrage agents (federal judiciary) have d ≈ 0.15 (beneficiaries; experience low effective extraction). Powerful mobile agents (state legislatures) have d ≈ 0.65 (targets of constraint expansion but mobile enough to operate at the boundary). Analytical agents occupy d ≈ 0.72–0.73 depending on whether they adopt the moral-progress frame (lower d: beneficiary perspective) or reject it (higher d: victim perspective). The engine's sigmoid f(d) converts these d values to effective power modifiers: f(0.95) ≈ 1.42 (powerless experiencing maximum extraction); f(0.15) ≈ -0.01 (institutional beneficiary experiencing near-zero net extraction); f(0.65) ≈ 1.00 (moderate target experiencing strong extraction force). These f(d) values are then multiplied by ε (0.38) and scope modifier σ(1.0 for national scope) to produce constraint-as-experienced (χ) values. The perspectival gap emerges because the same base extractiveness (ε = 0.38) produces radically different experienced extraction (χ) depending on d: for powerless agents, χ ≈ 0.54; for institutional beneficiaries, χ ≈ -0.004; for analytical observers with opposing axioms, χ ≈ 0.38–0.54.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that 'evolving standards' is not a neutral constitutional discovery mechanism but a constructed framework with identifiable beneficiaries and victims. The mandatrophy between moral-progress narrative (which sees objective standards) and retributive maximalism (which sees judge-imposed values) is not resolvable within a single coherent framework — the readings logically coexist but do not harmonize. The tangled-rope classification is mandatrophy-stable: it acknowledges that the constraint has a genuine coordination function (legitimizing incremental reform) AND asymmetric extraction (constraining retributive discretion and concentrating standard-setting). The doctrine is not pure coordination (which would make it rope) and not pure extraction (which would make it snare) — it is authentically hybrid. The false-summit mountain perspective correctly identifies that the 'moral progress' framing naturalizes a constructed constraint, but the structural data shows this is not a natural law (beneficiaries are identifiable, extractiveness is moderate not minimal, suppression is active not absent). The measurement trajectory shows that extractiveness, theater, and suppression all increased monotonically across the 40-year interval, indicating that the doctrine's scope expanded over time — consistent with a constraint that accumulates extraction as it matures, not a law of nature that was always equally operative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decency_standard_objectivity,
    'Is ''evolving standards of decency'' a discoverable objective moral fact, or a constructed interpretive framework permitting value-laden choices by federal judges?',
    'Meta-jurisprudential analysis: does decency converge on fixed principles (objective) or diverge by observer positionality (constructed)? Historical analysis of which ''decency standards'' have been recognized and which rejected; analysis of whether recognition correlates with actual social consensus or with judicial ideology.',
    'If objective: mountain classification is valid; decency standards constrain judges. If constructed: tangled_rope/snare classification is valid; judges constrain themselves through rhetorical claims of objectivity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decency_standard_objectivity, conceptual, 'Whether decency standards are objective moral facts or constructed interpretive frameworks').

omega_variable(
    consensus_lag_mechanism,
    'Does the evolving-standards doctrine track actual social consensus, or does it lag behind consensus, or does it lead consensus by imposing judicial values?',
    'Longitudinal analysis of public opinion polling and state legislative action on capital punishment categories (juveniles, intellectually disabled, etc.) versus timing of Supreme Court recognition; lag-lead analysis between Court decisions and consensus shifts.',
    'If tracking: doctrine legitimately coordinates judicial recognition of moral progress. If lagging: doctrine extracts from reform advocates (slow recognition). If leading: doctrine extracts from retributive majority (imposes values).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consensus_lag_mechanism, empirical, 'Temporal relationship between judicial recognition and social consensus on punishment decency').

omega_variable(
    core_reading_foreclosure,
    'Does the evolving-standards reading logically foreclose the fixed-original-meaning reading, or do they coexist as live positions within competing constitutional traditions?',
    'Jurisprudential analysis: can a single constitutional framework hold both readings simultaneously? Can one acknowledge that ''decency standards have evolved'' while maintaining that the Eighth Amendment means only what it meant in 1791?',
    'If foreclosed: readings are logically incompatible; victory for one position excludes the other. If coexisting: readings are held by competing parties; both remain live positions in ongoing constitutional contest. If influencing: one reading creates structural pressure on the other without logically eliminating it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(core_reading_foreclosure, conceptual, 'Logical relationship between evolving-standards and fixed-original-meaning readings of the Eighth Amendment').

omega_variable(
    suppression_mechanism_identity,
    'Is the evolving-standards doctrine''s suppressiveness primarily from the lack of prospective clarity about which categories will be deemed categorically ineligible, or from the deliberate gatekeeping of the ''decency consensus'' determination by federal judges?',
    'Analysis of documented harm from prospective uncertainty versus documented harm from judicial gatekeeping; case studies of capital defendants whose category status was resolved adversarially; analysis of whether prospective rule-setting would reduce suppression or merely shift the gatekeeping locus.',
    'If clarity-lack dominates: suppression could be reduced by prospective judicial rules (Eighth Amendment could narrowly track contemporary actual consensus). If gatekeeping dominates: suppression is inherent to concentrating consensus determination in a nine-member court.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity, empirical, 'Whether suppression mechanisms are from prospective uncertainty or judicial gatekeeping').

omega_variable(
    naturalization_vs_reading_status,
    'Is this reading a legitimate constitutional interpretation, or a naturalization of judge-made law as ''objective decency''?',
    'Meta-legal analysis: does the reading claim to follow the constitutional text (interpretation) or to supplement it with a judicial gloss of evolving standards (judge-made law)? Comparison to competing constitutional readings with equivalent textual fidelity but opposite content direction.',
    'If legitimate interpretation: mountain classification is defensible (decency standards are a constitutional category). If naturalization: false-summit detector correctly identifies the claim as benefiting particular agents while masquerading as neutral principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalization_vs_reading_status, conceptual, 'Whether the reading interprets constitutional text or naturalizes judge-made doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eighth_amendment__evolving_standards_reading, 1972, 2012).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eighth_evo_theater_1972, eighth_amendment__evolving_standards_reading, theater_ratio, 1972, 0.3).
narrative_ontology:measurement(eighth_evo_theater_1989, eighth_amendment__evolving_standards_reading, theater_ratio, 1989, 0.42).
narrative_ontology:measurement(eighth_evo_theater_2002, eighth_amendment__evolving_standards_reading, theater_ratio, 2002, 0.5).
narrative_ontology:measurement(eighth_evo_theater_2012, eighth_amendment__evolving_standards_reading, theater_ratio, 2012, 0.55).

% Extraction over time
narrative_ontology:measurement(eighth_evo_extract_1972, eighth_amendment__evolving_standards_reading, base_extractiveness, 1972, 0.15).
narrative_ontology:measurement(eighth_evo_extract_1989, eighth_amendment__evolving_standards_reading, base_extractiveness, 1989, 0.28).
narrative_ontology:measurement(eighth_evo_extract_2002, eighth_amendment__evolving_standards_reading, base_extractiveness, 2002, 0.35).
narrative_ontology:measurement(eighth_evo_extract_2012, eighth_amendment__evolving_standards_reading, base_extractiveness, 2012, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(eighth_evo_suppress_1972, eighth_amendment__evolving_standards_reading, suppression_requirement, 1972, 0.4).
narrative_ontology:measurement(eighth_evo_suppress_1989, eighth_amendment__evolving_standards_reading, suppression_requirement, 1989, 0.52).
narrative_ontology:measurement(eighth_evo_suppress_2002, eighth_amendment__evolving_standards_reading, suppression_requirement, 2002, 0.58).
narrative_ontology:measurement(eighth_evo_suppress_2012, eighth_amendment__evolving_standards_reading, suppression_requirement, 2012, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eighth_amendment__evolving_standards_reading, identity_coordination).
narrative_ontology:affects_constraint(eighth_amendment__evolving_standards_reading, eighth_amendment__fixed_original_meaning_reading).

% DUAL FORMULATION NOTE:
% The eighth_amendment kernel admits at least two structurally distinct constraint readings: (1) evolving-standards reading (ε=0.38, this file), which treats 'cruel and unusual' as indexing contemporary moral standards and produces a tangled-rope classification with concentrated judicial determination authority; (2) fixed-original-meaning reading (ε=0.22, separate file), which treats 'cruel and unusual' as indexing 1791-era standards and produces a rope classification with less active judicial gatekeeping. The readings share a kernel (the constitutional text) but produce different extractiveness values because the observable used to evaluate decency diverges (contemporary standards vs. historical standards). The constraint family includes potential third member: application-context reading (ε=0.45), which treats each punishment's decency as dependent on specific circumstances (age, crime, mental capacity, alternative sentences available). Each reading has distinct beneficiary/victim structure and institutional consequences. All three should be modeled as separate constraints linked by network.affects_constraints; do not attempt to fold them into one constraint with measurement-parameter variation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
