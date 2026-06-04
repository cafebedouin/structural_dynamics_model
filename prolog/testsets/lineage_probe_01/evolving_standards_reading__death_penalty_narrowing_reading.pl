% ============================================================================
% CONSTRAINT STORY: evolving_standards_reading__death_penalty_narrowing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_evolving_standards_reading__death_penalty_narrowing_reading, []).

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
 *   constraint_id: evolving_standards_reading__death_penalty_narrowing_reading
 *   human_readable: Evolving Standards: Death Penalty Narrowing Through Categorical Exemptions
 *   domain: constitutional_law/eighth_amendment/capital_punishment
 *
 * SUMMARY:
 *   The evolving standards death penalty narrowing reading instantiates one
 *   constraint interpretation of the Eighth Amendment's prohibition on cruel
 *   and unusual punishment: capital punishment persists, but its application
 *   is rationed through categorical exemptions discovered via accumulated
 *   consensus. The reading centers on the mechanism of
 *   suppression-through-exemption: the intellectually disabled, juveniles,
 *   and non-homicide offenders are carved out one consensus at a time,
 *   narrowing the death penalty toward its core (intentional murder of
 *   competent adult perpetrators). This is a contested reading within a
 *   larger kernel about the Eighth Amendment's authority to constrain
 *   punishment. Sibling readings interpret the same constitutional text
 *   differently: one emphasizes that the Clause follows the prisoner inside
 *   (conditions_confinement_reading), requiring humane treatment within
 *   confinement; another emphasizes that juvenile development science makes
 *   mandatory life without parole unconstitutional
 *   (juvenile_culpability_reading). This narrowing reading is distinctive in
 *   its focus on the categorical suppression mechanism itself — the work the
 *   Eighth Amendment does through exemption logic, accumulating consensus
 *   constraints on eligibility rather than requiring transformation of
 *   conditions or recognition of developmental capacity.
 *
 * KEY AGENTS:
 *   - Intellectually Disabled Defendants: Primary beneficiary (powerless/trapped) — categorical exemption from capital punishment coordinates protection of a constitutionally vulnerable class
 *   - Juvenile Offenders: Primary beneficiary (powerless/trapped) — categorical exemption based on developmental immaturity and capacity for change
 *   - Non-Homicide Capital Defendants: Secondary beneficiary/mixed (moderate/constrained) — exemption from execution in rape and felony-murder contexts, though some remain death-eligible under narrow criteria
 *   - Maximal Capital Punishment Framework: Primary victim (institutional framework) — categorical narrowing suppresses execution categories permanently; victim status is structural (the regime itself suffers scope reduction)
 *   - Sentencing Discretion Holders (Prosecutors, Judges): Secondary victim (organized/constrained) — narrowing constrains charging and sentencing authority; must operate within exemption boundaries
 *   - Supreme Court Authority Structure: Institutional actor (institutional/arbitrage) — maintains role as arbiter of constitutional capital punishment scope; benefits from authority maintenance through the doctrine itself
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the piecemeal consensus-counting mechanism as constitutional discovery rather than institutional construction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(evolving_standards_reading__death_penalty_narrowing_reading, 0.38).
domain_priors:suppression_score(evolving_standards_reading__death_penalty_narrowing_reading, 0.48).
domain_priors:theater_ratio(evolving_standards_reading__death_penalty_narrowing_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(evolving_standards_reading__death_penalty_narrowing_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(evolving_standards_reading__death_penalty_narrowing_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(evolving_standards_reading__death_penalty_narrowing_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(evolving_standards_reading__death_penalty_narrowing_reading, tangled_rope).
narrative_ontology:human_readable(evolving_standards_reading__death_penalty_narrowing_reading, "Evolving Standards: Death Penalty Narrowing Through Categorical Exemptions").
narrative_ontology:topic_domain(evolving_standards_reading__death_penalty_narrowing_reading, "constitutional_law/eighth_amendment/capital_punishment").

domain_priors:requires_active_enforcement(evolving_standards_reading__death_penalty_narrowing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(evolving_standards_reading__death_penalty_narrowing_reading, '4917fd06-1b84-44dc-9fcc-03d2e10e8e56').
narrative_ontology:cs_kernel_codification('4917fd06-1b84-44dc-9fcc-03d2e10e8e56', fixed_text).
narrative_ontology:cs_authority_grounding('4917fd06-1b84-44dc-9fcc-03d2e10e8e56', lineage).
narrative_ontology:cs_interpretation_layer_present('4917fd06-1b84-44dc-9fcc-03d2e10e8e56').
narrative_ontology:cs_reading_relation('4917fd06-1b84-44dc-9fcc-03d2e10e8e56', evolving_standards_reading__conditions_confinement_reading, coexists_with).
narrative_ontology:cs_reading_relation('4917fd06-1b84-44dc-9fcc-03d2e10e8e56', evolving_standards_reading__juvenile_culpability_reading, coexists_with).
narrative_ontology:cs_axiom('4917fd06-1b84-44dc-9fcc-03d2e10e8e56', foundational, capital_punishment_narrowable_via_consensus).
narrative_ontology:cs_axiom_status(capital_punishment_narrowable_via_consensus, holdable).
narrative_ontology:cs_axiom_grounding('4917fd06-1b84-44dc-9fcc-03d2e10e8e56', capital_punishment_narrowable_via_consensus, deontological).
narrative_ontology:cs_axiom('4917fd06-1b84-44dc-9fcc-03d2e10e8e56', secondary, consensus_operationalizable_via_state_survey).
narrative_ontology:cs_axiom_status(consensus_operationalizable_via_state_survey, holdable).
narrative_ontology:cs_axiom_grounding('4917fd06-1b84-44dc-9fcc-03d2e10e8e56', consensus_operationalizable_via_state_survey, empirically_contingent).
narrative_ontology:cs_reference_frame('4917fd06-1b84-44dc-9fcc-03d2e10e8e56', capital_punishment_with_evolving_exemptions).
narrative_ontology:cs_drift_state('4917fd06-1b84-44dc-9fcc-03d2e10e8e56', contemporary_post_graham, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4917fd06-1b84-44dc-9fcc-03d2e10e8e56', '').
narrative_ontology:cs_kernel_id(evolving_standards_reading__death_penalty_narrowing_reading, evolving_standards_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(evolving_standards_reading__death_penalty_narrowing_reading, intellectually_disabled).
narrative_ontology:constraint_beneficiary(evolving_standards_reading__death_penalty_narrowing_reading, juvenile_offenders).
narrative_ontology:constraint_beneficiary(evolving_standards_reading__death_penalty_narrowing_reading, non_homicide_capital_defendants).
narrative_ontology:constraint_victim(evolving_standards_reading__death_penalty_narrowing_reading, maximal_capital_punishment_framework).
narrative_ontology:constraint_victim(evolving_standards_reading__death_penalty_narrowing_reading, sentencing_discretion_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTELLECTUALLY DISABLED DEFENDANTS (ROPE) — Structural coordination function: the constitutional exemption coordinates protection of a vulnerable class against execution despite commission of capital offenses. The beneficiary experiences this as genuine coordination — their categorical exclusion from death eligibility solves a collective action problem (the polity's commitment not to execute the intellectually disabled). No extraction experienced; the constraint operates purely to organize their protection. Biographical time horizon reflects the individual defendant's lifespan-scale stakes.
constraint_indexing:constraint_classification(evolving_standards_reading__death_penalty_narrowing_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: JUVENILE OFFENDERS (ROPE) — Similar to intellectual disability exemption: genuine coordination function protecting a developmentally defined class. The constitutional holding (Roper, Miller, Graham) coordinates the recognition that juvenile culpability and capacity for change are constitutionally relevant. Beneficiaries see coordination, not extraction — their exclusion from death or mandatory life reflects collective commitment to accounting for developmental immaturity.
constraint_indexing:constraint_classification(evolving_standards_reading__death_penalty_narrowing_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: NON-HOMICIDE CAPITAL DEFENDANTS (TANGLED ROPE) — Coordination function is weaker here. The narrowing of capital eligibility from rape-of-child and felony-murder to intentional murder requires the court to coordinate recognition that execution should be reserved for the most culpable. But constrained exit and moderate power reflect the defendant's structural position: they remain subject to death if the offense pattern fits. Extractiveness emerges from the categorical narrowing itself — exemption is granted group-by-group, forcing each cohort to litigate their way out, creating delay, prolonged legal jeopardy, and institutional overhead.
constraint_indexing:constraint_classification(evolving_standards_reading__death_penalty_narrowing_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MAXIMAL CAPITAL PUNISHMENT FRAMEWORK (SNARE) — The framework itself (statutes permitting death for rape, felony-murder, non-homicide crimes) is the victim. The narrowing constraint suppresses alternatives within the capital regime: legislatures cannot re-expand capital eligibility without re-litigating constitutional boundaries. Each exemption reduces the framework's scope. From the perspective of those invested in broad capital eligibility, the narrowing operates as extraction — it takes categories off the table permanently. The framework is 'powerful' in the sense that statutes and prosecutorial discretion are built on it, but 'mobile' in that legislatures can technically revise statutes within constitutional limits (they cannot execute the exempt classes, but they can set sentencing ranges for remaining eligible offenders).
constraint_indexing:constraint_classification(evolving_standards_reading__death_penalty_narrowing_reading, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: SENTENCING DISCRETION HOLDERS (PROSECUTORS, JUDGES) (TANGLED ROPE) — Genuine coordination function: the categorical exemptions provide clear legal rules that simplify capital charging and sentencing decisions. A prosecutor need not litigate defendant intellectual capacity in every capital case; the law's recognition of the exemption coordinates expectation that certain categories are off-limits. But coordination is embedded with extraction: each exemption narrows the set of cases eligible for death, constraining prosecutorial and judicial discretion. The constraint requires active enforcement — courts must police boundary violations and apply the categorical rule even when specific defendants would satisfy non-exemption criteria. Constrained exit reflects that prosecutors/judges cannot simply ignore the exemptions; they must operate within the narrowed framework.
constraint_indexing:constraint_classification(evolving_standards_reading__death_penalty_narrowing_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SUPREME COURT AUTHORITY (PITON) — From a civilizational time horizon, the Court's evolving standards doctrine operates as an institutional ritual that performs constitutional constraint while maintaining core capital punishment authority. The doctrine says: capital punishment persists, but its application narrows as evolving standards of decency emerge. This frame preserves Court authority over both capital punishment AND its limitation. Theater ratio is elevated here (distinct from beneficiary perspectives): the doctrine appears to implement constitutional change through judicial interpretation of 'evolving standards,' but the underlying mechanism is political — Court decisions reflect accumulated political pressure from abolition movements, state legislation, and public opinion shifts. The theater diminishes only insofar as the Court explicitly acknowledges it is responding to observable consensus rather than discovering timeless constitutional principles. The piton classification reflects that the authority structure (the Court's role as arbiter of capital punishment scope) persists through institutional inertia despite the doctrine's apparent commitment to evolving standards — the Court could defer to legislatures entirely (as other democracies have), but instead maintains its role as final arbiter.
constraint_indexing:constraint_classification(evolving_standards_reading__death_penalty_narrowing_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT CANDIDATE (MOUNTAIN-CLASSIFIED) — From a universal/civilizational perspective, one could read the Eighth Amendment as instantiating a natural law limit: certain punishments are inherently cruel and unusual regardless of legislative choice. Under this reading, the exemptions are not policy choices but discoveries of constitutional law's deep structure. The narrowing appears inevitable — as society matures, the natural law boundary becomes clearer. This perspective risks naturalizing what the structural data reveals as a contingent institutional arrangement: the narrowing emerges from counted consensus (political mobilization), not from timeless constitutional principle. The engine's false summit detector will identify this as a false summit, revealing that the 'natural law' framing conceals the beneficiary-victim structure and the extractiveness of the piecemeal exemption process.
constraint_indexing:constraint_classification(evolving_standards_reading__death_penalty_narrowing_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(evolving_standards_reading__death_penalty_narrowing_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(evolving_standards_reading__death_penalty_narrowing_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(evolving_standards_reading__death_penalty_narrowing_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(evolving_standards_reading__death_penalty_narrowing_reading, TR),
    TR >= 0.70.

:- end_tests(evolving_standards_reading__death_penalty_narrowing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The narrowing mechanism does genuine work coordinating protection of vulnerable classes — the intellectual disability exemption genuinely exempts from capital punishment, not extracting from the beneficiary class. But extractiveness emerges in the mechanism itself: each new exemption requires litigation, judicial consensus-finding, and delay. Non-homicide defendants must wait and litigate for each category's recognition; the piecemeal process creates transaction costs and jeopardy duration. Extractiveness increases over the interval (0.18 → 0.38) because early exemptions are 'easy' politically (society increasingly opposes executing the intellectually disabled, juveniles), but as the narrowing proceeds toward the core, remaining expansion requires higher consensus thresholds, making the extraction mechanism more visible. By t30, the mechanism's inefficiency becomes apparent — those ineligible for death can still face decades of appellate litigation establishing their exemption status. Suppression (0.48): Moderate. Alternatives to categorical exemption (legislative abolition, comprehensive sentencing reform) are not structurally foreclosed, but the narrowing reading itself is built on a frame that presupposes capital punishment persists and is narrowed, not abolished. The suppression is internal to the doctrine's logic — once you accept the frame 'capital punishment with exemptions,' alternatives become harder to see. Theater ratio (0.35): Low-to-moderate, increasing over time. Early in the doctrine's development (Atkins, Roper), the reasoning appears to reflect genuine consensus discovery — the Court extensively surveys state legislation and public opinion. But as exemptions accumulate, the theater increases: the Court must justify why this category warrants exemption while that one does not, generating increasingly elaborate doctrinal reasoning. By t30, the tension between claimed consensus and the piecemeal nature of exemptions becomes visible (see false_summit_natural_law_vs_constructed omega), elevating theater as the Court performs constitutional constraint while maintaining capital punishment authority.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits classic tangled_rope perspectival gaps. Beneficiaries (exempted classes) see genuine coordination — their protection from execution solves a real collective action problem. Sentencing discretion holders see mixed coordination and constraint — clear rules simplify decisions, but narrowing limits their authority. The maximal capital punishment framework sees suppression — each exemption reduces the regime's scope. The Court sees itself as discovering constitutional principle (piton/authority-maintenance frame), while external observers see political responsiveness dressed in constitutional language (false summit). The gap between rope-from-beneficiary and snare-from-framework reveals that the same mechanism (categorical exemption accumulation) can be purely coordinative for those being protected but extractive for institutional actors invested in capital punishment scope. The false summit candidate (mountain perspective) risks naturalizing this frame as timeless constitutional law rather than contingent institutional arrangement responding to political pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values for each perspective reflect structural position relative to this specific constraint. Intellectually disabled defendants: d ≈ 0.05 (pure beneficiary, no exit costs from exemption). Juveniles: d ≈ 0.08 (pure beneficiary, no exit costs). Non-homicide defendants: d ≈ 0.45 (partial beneficiary — some exempted, some remain death-eligible; constrained exit reflects ongoing jeopardy). Maximal capital punishment framework: d ≈ 0.78 (victim status — the framework bears extraction costs through narrowed scope). Sentencing discretion holders: d ≈ 0.55 (mixed — benefit from clear legal rules, bear costs from constrained authority). Court authority: d ≈ 0.20 (institutional beneficiary — authority over capital punishment persists and is maintained through the doctrine; arbitrage exit reflects discretionary authority to interpret Eighth Amendment). Analytical observer: d ≈ 0.72 (observer position, derived from analytical power atom). Low d values for beneficiaries produce negative or minimal effective extraction (χ) from their perspective; high d values for the regime and constrained actors produce visible extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy (which classification is correct?) by revealing that the classification is INDEX-DEPENDENT, not observer-independent. From the beneficiary's perspective (exempted classes), the constraint is rope — pure coordination protecting vulnerable populations. From the framework's perspective (capital punishment regime scope), the constraint is snare — suppression of execution categories. From the sentencing authority's perspective, it is tangled_rope — coordination of clear rules embedded with constraint. From the Court's perspective, it is piton — the authority structure persists and maintains itself through the doctrine. From the analytical observer at civilizational scope, it risks appearing as mountain (natural law constraint) — a false summit revealing how institutional framing naturalizes political processes. No single type is 'correct'; instead, the indexed classification presheaf over all perspectives reveals the constraint's full structural reality: it coordinates protection (rope function), suppresses regime scope (snare function), constrains authority (tangled_rope function), and maintains institutional power (piton function) simultaneously. The mandatrophy is resolved by accepting all indexed classifications as perspectivally correct.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_operationalization,
    'What counts as ''evolving standards of decency''? How is consensus measured, and who determines when a consensus has ''evolved'' sufficiently to trigger a constitutional exemption?',
    'Analysis of Court''s actual consensus-detection methods (state legislation survey, polling data, international practice review, amicus input) across Atkins, Roper, Miller, and Graham. Comparison of stated consensus threshold with actual legislative/polling data.',
    'If consensus is objectively measurable: the narrowing mechanism is structural and predictable. If consensus is interpretively constructed: the Court has discretion to declare exemptions independent of actual consensus, and extractiveness increases (the doctrine becomes a cover for judicial policy preference). This affects whether the mechanism is truly ''coordinating'' consensus or ''producing'' it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consensus_operationalization, empirical, 'How evolving standards doctrine operationalizes consensus measurement').

omega_variable(
    piecemeal_vs_wholesale_narrowing,
    'Is categorical exemption the only available mechanism for narrowing capital punishment, or does the constraint foreclose alternatives like legislative abolition or comprehensive sentencing reform?',
    'Comparative constitutional analysis: do other democracies that have abolished capital punishment show similar doctrine of categorical exemptions, or do they use wholesale legislative action? Does the US doctrine''s frame (capital punishment persists, narrowed by exemptions) constrain the abolition pathway?',
    'If piecemeal exemptions are the only available mechanism within this reading: extractiveness is moderate (each defendant class must litigate separately). If alternatives exist but the doctrine forecloses them: extractiveness is higher (the mechanism constrains the scope of possible reforms to the capital punishment regime itself, not the regime''s continuation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piecemeal_vs_wholesale_narrowing, conceptual, 'Whether categorical exemption mechanism constrains alternative narrowing pathways').

omega_variable(
    false_summit_natural_law_vs_constructed,
    'Is the Eighth Amendment''s prohibition on cruel and unusual punishment a natural law constraint (independent of politics), or a constructed institutional reading that legitimates political negotiation through constitutional language?',
    'Historical analysis: do the Court''s consensus measurements actually precede exemptions, or do exemptions follow legislative/social momentum? If the former, natural law reading has warrant. If the latter, the ''constitutional'' framing naturalizes a political process. Comparative: do non-US legal systems show similar doctrinal patterns absent the natural-law inheritance?',
    'If natural law: mountain classification is appropriate, and the narrowing reflects discovery of constitutional truth. If constructed: mountain is a false summit, and the constraint is tangled_rope (genuine coordination of exempted classes + extraction from the capital punishment regime + theatrical authority maintenance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_vs_constructed, conceptual, 'Whether evolving standards doctrine reflects natural law or constructed institutional frame').

omega_variable(
    reading_contest_authority_grounding,
    'Which reading (death_penalty_narrowing, conditions_confinement, juvenile_culpability) grounds legitimate authority — that is, which reading''s core premise is THE constitutional constraint, and which readings are derivative interpretations?',
    'Doctrinal analysis of case reasoning: which reading appears first in constitutional genealogy (Weems, Trop, Furman chain), which is most frequently cited as authority, which provides the framework within which other readings operate. Textual analysis: does the Eighth Amendment text itself privilege one reading?',
    'If narrowing is primary: conditions and juvenile readings are derivative (constrained by the narrowing logic). If culpability is primary: narrowing is one consequence of recognizing developmental immaturity (sibling reading). If conditions is primary: narrowing and culpability are both instances of the proportionality principle applied to different contexts. Authority grounding determines reading_relations (forecloses vs coexists_with).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_authority_grounding, conceptual, 'Which reading provides primary constitutional authority in Eighth Amendment doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(evolving_standards_reading__death_penalty_narrowing_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(esdpn_theater_t0, evolving_standards_reading__death_penalty_narrowing_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(esdpn_theater_t15, evolving_standards_reading__death_penalty_narrowing_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(esdpn_theater_t30, evolving_standards_reading__death_penalty_narrowing_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(esdpn_extractiveness_t0, evolving_standards_reading__death_penalty_narrowing_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(esdpn_extractiveness_t15, evolving_standards_reading__death_penalty_narrowing_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(esdpn_extractiveness_t30, evolving_standards_reading__death_penalty_narrowing_reading, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(esdpn_suppression_t0, evolving_standards_reading__death_penalty_narrowing_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(esdpn_suppression_t15, evolving_standards_reading__death_penalty_narrowing_reading, suppression_requirement, 15, 0.46).
narrative_ontology:measurement(esdpn_suppression_t30, evolving_standards_reading__death_penalty_narrowing_reading, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(evolving_standards_reading__death_penalty_narrowing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(evolving_standards_reading__death_penalty_narrowing_reading, conditions_confinement_reading).
narrative_ontology:affects_constraint(evolving_standards_reading__death_penalty_narrowing_reading, juvenile_culpability_reading).

% DUAL FORMULATION NOTE:
% The narrowing reading is one interpretation of Eighth Amendment authority within a constraint family of sibling readings. Each reading interprets the same kernel (the Clause's power to constrain punishment) differently, producing distinct constraints with different mechanisms. The narrowing reading focuses on categorical exemption accumulation via consensus; conditions and culpability readings focus on dignity/treatment and developmental capacity respectively. All three share the kernel but instantiate different extraction-coordination structures. They are linked as structural siblings within the evolving_standards_reading kernel, not as sequential steps in doctrine evolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(evolving_standards_reading__death_penalty_narrowing_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
