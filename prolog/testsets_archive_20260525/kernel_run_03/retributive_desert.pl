% ============================================================================
% CONSTRAINT STORY: retributive_desert
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_retributive_desert, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: retributive_desert
 *   human_readable: Retributive Desert: Death for Death Proportionality Doctrine
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   The retributive desert doctrine — that murderers forfeit their right to
 *   life and that proportional punishment (lex talionis: death for death) is
 *   the legitimate form of state killing — is one reading of the contested
 *   kernel around state killing authority. This constraint coordinates
 *   genuine goods (victim vindication, acknowledgment of wrongdoing,
 *   proportional limitation on state power) while extracting from the
 *   condemned (forfeiture of personhood, execution, replication of the murder
 *   structure under state authority). The constraint exhibits a fundamental
 *   tension: it claims to constrain state killing through proportionality
 *   while simultaneously justifying and enabling it through forfeiture
 *   doctrine. The extractiveness trajectory shows rising theater
 *   (proportionality invoked increasingly as justification for outcomes
 *   driven by other factors — deterrence, incapacitation, political pressure)
 *   and rising base extractiveness as the empirical ground shifts:
 *   exonerations accumulate, racial/class disparities in capital sentences
 *   become undeniable, victim families report retraumatization rather than
 *   healing, and international human rights bodies reject the doctrine
 *   entirely. The retributive reading coexists with deterrence (instrumental)
 *   and categorical abolition (deontological alternative) readings of the
 *   same kernel.
 *
 * KEY AGENTS:
 *   - The Condemned Prisoner: Primary victim (powerless/trapped) — structurally removed from rights-holder set via forfeiture doctrine; maximum suppression; bears extraction directly via execution
 *   - Poor and Marginalized Defendants: Disproportionate victim group (powerless/trapped) — system targets this population through resource barriers, discrimination, and coercion; extraction operates via selection mechanism rather than uniform application of doctrine
 *   - The Retributive State Authority: Primary beneficiary (institutional/arbitrage) — gains legitimacy framing for killing, monopoly on ultimate sanction, constraining language (proportionality) that limits (but does not eliminate) discretion
 *   - The Murdered Victim: Posthumous beneficiary (powerful/constrained via death) — enters the benefit set through vindication doctrine; receives symbolic restoration but cannot receive actual restoration; victim's death instrumentalized to justify state killing
 *   - Victims' Families: Secondary beneficiary/victim (moderate/constrained) — experience both genuine closure/acknowledgment AND extraction (expected endorsement of killing, retraumatization by execution participation, false promise that execution heals grief)
 *   - The Proportionality Doctrine (Institutional Practice): Piton (institutional/constrained) — persists through legal precedent and inertia despite erosion of functional legitimacy; increasingly invoked performatively rather than as genuine constraint on state power
 *   - International Human Rights Bodies: Counter-authority (institutional/analytical) — reject the reading as incompatible with human dignity norms; fragment the authority_grounding from unified 'lineage' toward 'distributed' competing authorities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(retributive_desert, 0.58).
domain_priors:suppression_score(retributive_desert, 0.65).
domain_priors:theater_ratio(retributive_desert, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(retributive_desert, extractiveness, 0.58).
narrative_ontology:constraint_metric(retributive_desert, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(retributive_desert, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(retributive_desert, tangled_rope).
narrative_ontology:human_readable(retributive_desert, "Retributive Desert: Death for Death Proportionality Doctrine").
narrative_ontology:topic_domain(retributive_desert, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(retributive_desert).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(retributive_desert, formalized).
narrative_ontology:cs_authority_grounding(retributive_desert, lineage).
narrative_ontology:cs_interpretation_layer_present(retributive_desert).
narrative_ontology:cs_kernel_id(retributive_desert, state_killing_authority).
narrative_ontology:cs_reading_relation(retributive_desert, deterrence_instrument, coexists_with).
narrative_ontology:cs_reading_relation(retributive_desert, categorical_abolition, forecloses).
narrative_ontology:cs_axiom(retributive_desert, foundational, murderers_forfeit_rights).
narrative_ontology:cs_axiom_status(murderers_forfeit_rights, holdable).
narrative_ontology:cs_axiom_grounding(retributive_desert, murderers_forfeit_rights, deontological).
narrative_ontology:cs_axiom(retributive_desert, foundational, proportionality_equals_justice).
narrative_ontology:cs_axiom_status(proportionality_equals_justice, holdable).
narrative_ontology:cs_axiom_grounding(retributive_desert, proportionality_equals_justice, deontological).
narrative_ontology:cs_reference_frame(retributive_desert, proportional_retributive_justice).
narrative_ontology:cs_drift_state(retributive_desert, contemporary_human_rights_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(retributive_desert, murdered_victim_vindication).
narrative_ontology:constraint_beneficiary(retributive_desert, retributive_authority_legitimacy).
narrative_ontology:constraint_victim(retributive_desert, condemned_prisoners).
narrative_ontology:constraint_victim(retributive_desert, poor_and_marginalized_defendants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CONDEMNED PRISONER (SNARE) — No exit from the constraint. Forfeiture doctrine removes legal personhood; proportionality framing denies any alternative to execution. Maximum suppression: appeals exhausted, appeals courts bound by proportionality doctrine itself. No agency. Pure extraction: the constraint terminates existence.
constraint_indexing:constraint_classification(retributive_desert, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POOR/MARGINALIZED DEFENDANTS (SNARE) — Systemic targeting via resource barriers (inadequate legal defense, plea coercion, discrimination in jury selection and sentencing). Extraction runs toward the constraint enforcer (state apparatus); suppression is structural and total. The constraint disproportionately selects from this group despite identical crimes receiving different sentences in privileged cohorts.
constraint_indexing:constraint_classification(retributive_desert, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: THE RETRIBUTIVE AUTHORITY (ROPE) — State apparatus experiences the constraint as coordination of legitimate punishment authority. Proportionality doctrine constrains state discretion (Eighth Amendment, similar constitutional texts) while granting monopoly on the ultimate sanction. The state benefits from the constraint's legitimacy frame — proportionality makes killing appear just rather than arbitrary. Low theater because the doctrine is genuinely operationalized in sentencing law.
constraint_indexing:constraint_classification(retributive_desert, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE MURDERED VICTIM (TANGLED ROPE) — Enters the beneficiary set posthumously through retributive vindication doctrine. The constraint coordinates genuine goods: acknowledgment of wrongdoing, proportional consequence, restoration of cosmic order via parity punishment. But the mechanism also extracts from the condemned: transforms the victim's death into justification for the state's killing, potentially instrumentalizing grief. Asymmetric: the victim receives symbolic restoration but cannot receive actual restoration; the condemned bears actual forfeiture.
constraint_indexing:constraint_classification(retributive_desert, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: VICTIMS' FAMILIES (TANGLED ROPE) — Experience genuine coordination (acknowledgment of harm, legal closure, public recognition of wrongdoing) alongside extraction (compelled participation in state killing, retraumatization by execution, false promise that killing the perpetrator will heal the loss). High suppression: expected to endorse execution as closure; dissent treated as disloyalty to the victim. Mixed experience: some report catharsis, others report escalated trauma.
constraint_indexing:constraint_classification(retributive_desert, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THE PROPORTIONALITY DOCTRINE AS INSTITUTIONAL PRACTICE (PITON) — The doctrine persists through institutional inertia and legal precedent despite erosion of its functional legitimacy. Modern courts rely on proportionality language while de facto applying other rationales (deterrence, incapacitation, public safety). The doctrine's theater_ratio is high because courts invoke proportionality to justify outcomes driven by political pressure or risk aversion. The constraint shows signs of degradation: international human rights bodies reject it; domestic support is declining; executions are increasingly rare despite doctrinal continuity.
constraint_indexing:constraint_classification(retributive_desert, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: THE ANALYTICAL OBSERVER — NATURAL LAW READING (MOUNTAIN) — From a deontological standpoint, the constraint appears as an immutable moral law: those who deliberately end innocent life forfeit their own claim to life. Proportionality (death for death) follows from a cosmic order principle independent of consequences or social arrangements. This perspective treats the constraint as emerging naturally from the structure of justice itself. However, the structural data contradicts the mountain classification: identifiable beneficiaries (state authority, doctrine legitimacy), high suppression (legal system coercion, forfeiture doctrine), and active enforcement all indicate a constructed rather than natural constraint. The engine flags this as a false summit: naturalized institutions rather than laws of nature.
constraint_indexing:constraint_classification(retributive_desert, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(retributive_desert_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(retributive_desert, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(retributive_desert, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(retributive_desert, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(retributive_desert, TR),
    TR >= 0.70.

:- end_tests(retributive_desert_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint enables state killing of condemned prisoners while framing it as proportional justice. The extraction is not maximal (0.70+) because proportionality doctrine does constrain state discretion — courts must justify sentences, cannot execute arbitrarily, and face some appellate review. But the constraint extracts substantially: the condemned lose legal personhood (forfeiture), face a system biased against them (resource disparities), and encounter courts operating under doctrines that presuppose their guilt. The extractiveness trajectory rises over time as empirical evidence accumulates that proportionality doctrine fails to limit disparities or protect innocence. Suppression (0.65): High. Structural barriers to resistance include exhausted appeals, forfeiture doctrine that removes legal personhood mid-trial, inadequate defense resources (particularly for poor defendants), plea coercion, discrimination in jury selection, and public hostility to capital defendants. The constraint uses multiple suppression mechanisms: legal formalism (forfeiture removes personhood), resource barriers (inadequate representation), and social pressure (defendants are often politically undefended). Theater ratio (0.48): Moderate. Proportionality doctrine is genuinely operationalized in sentencing law and appellate review — it is not purely performative. However, the doctrine's theater has risen over time: courts increasingly invoke proportionality to justify outcomes driven by political pressure, fear of crime, or victim impact statements, rather than as a genuine constraint on discretion.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. From the condemned prisoner's perspective, it is a snare: no exit, maximum suppression, total extraction via execution. From the retributive authority's perspective, it is a rope: constraining but also empowering, legitimizing the state's ultimate sanction. From the victim's perspective (posthumous), it is a tangled rope: genuine coordination (acknowledgment of wrong, proportional response) mixed with extraction (instrumentalization of grief, replication of killing structure). From victims' families' perspective, it is also tangled rope: closure and acknowledgment alongside retraumatization and false promises. From the doctrine itself (institutional practice), it is a piton: persisting through precedent despite erosion of legitimacy and rising theater as the functional constraint weakens. From the analytical observer, it appears as a mountain (natural law of retributive justice) but the structural data reveals it as a false summit: a constructed institutional arrangement presented as an immutable principle. The perspectival gap reflects the constraint's hybrid nature: it is simultaneously a constraint on state power (proportionality limits discretion) and an enablement of state power (forfeiture justifies killing).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from the agent's structural position relative to the constraint. The condemned prisoner is a victim with no exit: d ≈ 0.95 (trapped + victim). The retributive authority is a beneficiary with arbitrage exit (can choose to execute or not, institutionally): d ≈ 0.15 (institutional + arbitrage + beneficiary). The murdered victim enters the beneficiary set posthumously but with constrained vindication (symbolic rather than actual restoration): d ≈ 0.45. Victims' families experience mixed positioning: beneficiaries of acknowledgment but victims of compelled participation in killing; constrained exit (expected to endorse execution or face social judgment): d ≈ 0.50. Poor and marginalized defendants face the same doctrinal suppression as wealthy defendants but additional systemic targeting; their d-value is higher than wealthy defendants' because the disproportionate selection mechanism creates an additional extraction layer: d ≈ 0.98 (trapped + victim + targeted). The proportionality doctrine itself, as an institutional practice, has low directionality (institutional actor, arbitrage exit via precedent change): d ≈ 0.20, but its classification as piton derives from the theater gate (theater_ratio ≥ 0.70) rather than from high chi. The analytical observer takes d ≈ 0.72 (canonical analytical), which maps to mountain in the deontological/natural law framing, triggering false summit detection.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that retributive desert doctrine is genuinely a tangled rope from the perspective of mixed beneficiaries and victims (state authority, victims, victims' families) but appears as a snare from the perspective of the condemned. The coordination function is real: proportionality does provide a limiting principle on state discretion and a framework for acknowledging wrongdoing. But the constraint also extracts substantially from the condemned (via execution) and from the marginalized (via selective application). The mandatrophy is resolved not by choosing a single type but by recognizing that the constraint has genuine coordination content (making it not a pure snare) alongside substantial asymmetric extraction (making it not a pure rope). The tangled rope classification captures this hybrid: 0.40 ≤ χ ≤ 0.90, beneficiaries present (state authority, victim vindication), victims present (condemned, marginalized defendants), active enforcement required (court systems, execution infrastructure). The rising theater_ratio and rising extractiveness trajectory suggest that the constraint is degrading over time: as empirical challenges accumulate and international authority fragments, the doctrine increasingly relies on performance (proportionality language) rather than function (actual proportionate limitation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    forfeiture_vs_incomparability,
    'Does proportional punishment require forfeiture of rights (the condemned loses personhood), or does it presuppose inalienable rights even for murderers (the state acts within justified limits while respecting the condemned''s residual dignity)?',
    'Jurisprudential analysis of how courts ground proportionality doctrine: does the reasoning depend on forfeiture logic, or does it restrict state power while treating the condemned as still-rights-bearing? Comparison with proportionality reasoning in non-capital contexts (mandatory minimums, sentencing guidelines) where forfeiture logic is not invoked.',
    'If forfeiture is necessary: the constraint is foundationally deontological and resists empirical challenge. If proportionality can constrain power without forfeiture: the constraint is instrumentally contingent and vulnerable to axiom_overriding (e.g., empirical evidence that execution does not satisfy legitimate retributive goals or causes unintended harms).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forfeiture_vs_incomparability, conceptual, 'Whether proportionality requires forfeiture or constraints on state power').

omega_variable(
    backward_looking_vs_instrumentalization,
    'Is retributive desert genuinely backward-looking (vindicating the past wrong through proportional consequence), or does it necessarily instrument the condemned''s death to serve state legitimacy (the state needs a justification story for its killing)?',
    'Historical analysis of how retributive doctrine emerged and how it functions in practice. Do courts invoke retribution primarily to justify punishment, or to limit punishment? Do victim-impact statements and execution protocols reflect backward-looking closure or forward-looking deterrence/incapacitation? Comparison with non-capital retributive schemes.',
    'If genuinely backward-looking: the constraint''s legitimacy claim is coherent and the tangled_rope classification is accurate. If inherently instrumentalized: the constraint is primarily a snare dressed in retributive language, and the beneficiary set (victim vindication) is a cover story for state extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(backward_looking_vs_instrumentalization, conceptual, 'Whether retributive desert is backward-looking or instrumentalizes the condemned').

omega_variable(
    proportionality_inversion_dilemma,
    'Does proportional punishment create a logical inversion where the state''s execution of a murderer replicates the original murder''s structure (intentional killing of a person in the state''s custody), thereby violating rather than satisfying retributive desert?',
    'Philosophical analysis of the structure of retributive parity. Does ''death for death'' treat killing as a symmetrical exchange, or does it introduce asymmetries (state vs. individual actor, legal authority vs. criminal act, consent vs. coercion) that break the parity logic? Examination of how courts address this inversion (via sovereign immunity, legitimate authority distinctions, or conceptual separation of punishment from murder).',
    'If the inversion is genuine: proportionality doctrine is self-refuting — it claims to satisfy retributive desert while replicating the structure it condemns. This undermines the deontological foundation and reveals the constraint as primarily a state power assertion (shifting classification toward snare). If the authority/criminal distinction is philosophically sufficient: the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_inversion_dilemma, conceptual, 'Whether proportional execution replicates rather than satisfies retributive desert').

omega_variable(
    empirical_challenge_to_forfeiture,
    'Has systematic evidence accumulated that execution does not satisfy legitimate retributive goals (victim families report escalated trauma, convicted persons are later exonerated, proportionality doctrine fails to constrain racial/class disparities in capital sentences)?',
    'Meta-analysis of victim impact studies, exoneration rates, sentencing disparity research, and jurisdictional variation in capital punishment outcomes. Has the empirical ground shifted such that forfeiture logic (the axiom) is no longer ''holdable'' within the retributive tradition?',
    'If empirical challenge is substantial: the foundational axiom (murderers forfeit rights via proportionality) shifts from ''holdable'' to ''overridden'' status within the retributive reading''s own standards. This would make the reading internally incoherent — claiming retributive legitimacy while empirically undermining it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_challenge_to_forfeiture, empirical, 'Whether systematic evidence undermines forfeiture axiom').

omega_variable(
    international_authority_eclipse,
    'As international human rights bodies (UN, ICC, regional courts) increasingly reject retributive capital punishment as incompatible with human dignity norms, does the authority_grounding of the retributive reading shift from ''lineage'' (constitutional tradition) to ''distributed'' (multiple competing authorities)?',
    'Tracking of authority structures: does the retributive reading''s reference frame (the constitutional and common-law lineage grounding state killing authority) still command allegiance among consequential institutional actors? Or has the authority fragmented into a weaker ''distributed'' grounding where retributive tradition competes with abolitionist human rights norms?',
    'If authority has eclipsed: the reading''s drift_state shifts toward ''authority_erosion / substantial''. The constraint''s legitimacy foundation weakens, and the piton classification (institutional inertia masquerading as functional legitimacy) becomes more accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_authority_eclipse, conceptual, 'Whether international human rights bodies eclipse lineage authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(retributive_desert, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(retr_tr_t0, retributive_desert, theater_ratio, 0, 0.38).
narrative_ontology:measurement(retr_tr_t20, retributive_desert, theater_ratio, 20, 0.44).
narrative_ontology:measurement(retr_tr_t40, retributive_desert, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(retr_be_t0, retributive_desert, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(retr_be_t20, retributive_desert, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(retr_be_t40, retributive_desert, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(retributive_desert, enforcement_mechanism).
narrative_ontology:affects_constraint(retributive_desert, deterrence_instrument).
narrative_ontology:affects_constraint(retributive_desert, categorical_abolition).
narrative_ontology:affects_constraint(retributive_desert, innocent_conviction_execution_risk).

% DUAL FORMULATION NOTE:
% The retributive desert constraint is one reading of a kernel that admits multiple structurally distinct constraints. The deterrence_instrument reading has different extractiveness (outcome-dependent; empirical evidence can refute it), different beneficiaries (society as a whole rather than victim vindication), and different omega variables (around causal efficacy rather than deontological coherence). The categorical_abolition reading is incompatible with retributive desert within a single framework (forecloses relation), while deterrence_instrument coexists with retributive desert across different institutional actors. See constraint_state_killing_authority kernel documentation for full family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(retributive_desert, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
