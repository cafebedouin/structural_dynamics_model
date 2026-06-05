% ============================================================================
% CONSTRAINT STORY: warrant_preference_reading__good_faith_exception_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_warrant_preference_reading__good_faith_exception_reading, []).

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
 *   constraint_id: warrant_preference_reading__good_faith_exception_reading
 *   human_readable: Good Faith Exception to Warrant Exclusion Rule (Leon Doctrine)
 *   domain: legal/constitutional_doctrine
 *
 * SUMMARY:
 *   United States v. Leon (1984) introduced the 'good faith exception' to the
 *   exclusionary rule: when law enforcement officers reasonably rely on a
 *   warrant that later proves defective, evidence is admissible despite
 *   Fourth Amendment violation. The doctrine rests on a specific deterrence
 *   theory: suppression punishes only the sovereign (the prosecution loses
 *   evidence) but does not deter officers who acted in good faith. Therefore,
 *   suppression serves no remedial purpose in the good faith case and should
 *   not be applied. This constraint embodies one reading of the warrant
 *   preference kernel — the reading that prioritizes deterrence function over
 *   categorical Fourth Amendment enforcement. The constraint exhibits
 *   tangled_rope structure: it coordinates police behavior toward
 *   warrant-seeking (genuine coordination function) while simultaneously
 *   extracting exemption from suppression when the warrant defect is honest
 *   (asymmetric extraction). The key structural feature is the inversion of
 *   deterrence: where the exclusionary rule's deterrent effect is presumed
 *   absent, suppression becomes 'rule without remedy,' and the evidence is
 *   retained.
 *
 * KEY AGENTS:
 *   - Law Enforcement Officers: Primary beneficiary (institutional/arbitrage) — obtain warrant, search is defective but in good faith, evidence admitted without suppression. No institutional penalty.
 *   - Prosecutorial Agencies: Primary beneficiary (institutional/arbitrage) — evidence secured despite warrant defect; conviction proceeds without suppression barrier.
 *   - Defendants in Honest-Error Cases: Primary victim (powerless/trapped) — lose Fourth Amendment remedy despite constitutional violation. No exit, no protection, evidence used to convict.
 *   - Fourth Amendment Categorical Enforcement: Secondary victim (organized/constrained) — the categorical warrant requirement survives, but the exclusionary remedy is suppressed in the good faith class, reducing the rule's bite.
 *   - Courts Applying Leon: Institutional actor (institutional/constrained) — navigate doctrinal requirement to apply good faith exception while nominally preserving Fourth Amendment protections; constrained by precedent to grant exception when reasonable reliance applies.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(warrant_preference_reading__good_faith_exception_reading, 0.62).
domain_priors:suppression_score(warrant_preference_reading__good_faith_exception_reading, 0.68).
domain_priors:theater_ratio(warrant_preference_reading__good_faith_exception_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(warrant_preference_reading__good_faith_exception_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(warrant_preference_reading__good_faith_exception_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(warrant_preference_reading__good_faith_exception_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(warrant_preference_reading__good_faith_exception_reading, tangled_rope).
narrative_ontology:human_readable(warrant_preference_reading__good_faith_exception_reading, "Good Faith Exception to Warrant Exclusion Rule (Leon Doctrine)").
narrative_ontology:topic_domain(warrant_preference_reading__good_faith_exception_reading, "legal/constitutional_doctrine").

domain_priors:requires_active_enforcement(warrant_preference_reading__good_faith_exception_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(warrant_preference_reading__good_faith_exception_reading, '6825f7ce-a8d0-43d1-b0bf-5f2ffeb1810d').
narrative_ontology:cs_kernel_codification('6825f7ce-a8d0-43d1-b0bf-5f2ffeb1810d', fixed_text).
narrative_ontology:cs_authority_grounding('6825f7ce-a8d0-43d1-b0bf-5f2ffeb1810d', lineage).
narrative_ontology:cs_interpretation_layer_present('6825f7ce-a8d0-43d1-b0bf-5f2ffeb1810d').
narrative_ontology:cs_reading_relation('6825f7ce-a8d0-43d1-b0bf-5f2ffeb1810d', warrant_preference_reading__exclusionary_rule_reading, coexists_with).
narrative_ontology:cs_reading_relation('6825f7ce-a8d0-43d1-b0bf-5f2ffeb1810d', warrant_preference_reading__digital_carpenter_reading, influences).
narrative_ontology:cs_axiom('6825f7ce-a8d0-43d1-b0bf-5f2ffeb1810d', foundational, suppression_justified_only_by_deterrence).
narrative_ontology:cs_axiom_status(suppression_justified_only_by_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('6825f7ce-a8d0-43d1-b0bf-5f2ffeb1810d', suppression_justified_only_by_deterrence, empirically_contingent).
narrative_ontology:cs_axiom('6825f7ce-a8d0-43d1-b0bf-5f2ffeb1810d', foundational, good_faith_reliance_breaks_deterrence_chain).
narrative_ontology:cs_axiom_status(good_faith_reliance_breaks_deterrence_chain, holdable).
narrative_ontology:cs_axiom_grounding('6825f7ce-a8d0-43d1-b0bf-5f2ffeb1810d', good_faith_reliance_breaks_deterrence_chain, instrumental).
narrative_ontology:cs_reference_frame('6825f7ce-a8d0-43d1-b0bf-5f2ffeb1810d', constitutional_deterrence_framework).
narrative_ontology:cs_drift_state('6825f7ce-a8d0-43d1-b0bf-5f2ffeb1810d', contemporary_post_leon_jurisprudence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6825f7ce-a8d0-43d1-b0bf-5f2ffeb1810d', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(warrant_preference_reading__good_faith_exception_reading, warrant_preference_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(warrant_preference_reading__good_faith_exception_reading, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(warrant_preference_reading__good_faith_exception_reading, prosecutorial_agencies).
narrative_ontology:constraint_victim(warrant_preference_reading__good_faith_exception_reading, fourth_amendment_categorical_enforcement).
narrative_ontology:constraint_victim(warrant_preference_reading__good_faith_exception_reading, defendants_with_honest_error_searches).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEFENDANT IN HONEST-ERROR CASE (SNARE) — Cannot exit the suppression loss; bears full weight of the good faith exception without remedy. Officer reasonably relied on invalid warrant; defendant has no exclusionary protection. Trapped in biographical horizon: the loss is immediate and permanent. Zero effective exit options or protest capacity.
constraint_indexing:constraint_classification(warrant_preference_reading__good_faith_exception_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FOURTH AMENDMENT CATEGORICAL RULE (ROPE) — The constitutional rule still exists and still coordinates on the warrant requirement itself. What is lost is the exclusionary remedy, not the warrant obligation. Organized over generational horizon through doctrinal tradition. Constrained exit because courts cannot simply repudiate Leon; they navigate it through reading. The constraint coordinates warrant-seeking behavior even as it extracts exemption from exclusion.
constraint_indexing:constraint_classification(warrant_preference_reading__good_faith_exception_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LAW ENFORCEMENT / PROSECUTION (TANGLED ROPE) — Genuine coordination benefit from the warrant requirement itself: getting a warrant is still mandatory and still functions as a gatekeeping mechanism. But the good faith exception extracts the exclusionary penalty when the warrant is defective. Net beneficiary: incentivized to seek warrants (coordination) but shielded from suppression when reasonable reliance fails (extraction). Biographical horizon: the exemption operates immediately upon search; long-term institutional preference for warrants.
constraint_indexing:constraint_classification(warrant_preference_reading__good_faith_exception_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational view, the constraint exhibits both genuine coordination and asymmetric extraction. The warrant requirement coordinates police behavior toward Fourth Amendment-conscious searching. But the good faith exception permits extraction of evidence when the warrant defect is honest. The doctrine coordinates on process while exempting outcome. Extractiveness moderate-high (0.62) because the exemption class is large (reasonable reliance by officers is common) and the remedy removal is total (no suppression, evidence admitted, conviction proceeds).
constraint_indexing:constraint_classification(warrant_preference_reading__good_faith_exception_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(warrant_preference_reading__good_faith_exception_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(warrant_preference_reading__good_faith_exception_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(warrant_preference_reading__good_faith_exception_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(warrant_preference_reading__good_faith_exception_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(warrant_preference_reading__good_faith_exception_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The good faith exception removes suppression — the primary remedy for Fourth Amendment violations — in a large and growing class of cases (honest officer error). The extraction is not maximal because the warrant requirement itself remains binding; officers must still seek warrants. The extraction operates on the remedy, not on the rule. The trajectory shows rising extractiveness (0.38 → 0.62 over 30 years) as courts have broadened the good faith exception to cover increasingly attenuated warrant defects and increasingly broad categories of 'reasonable reliance.' Suppression (0.68): High. The constraint suppresses the exclusionary remedy through the doctrinal barrier of 'good faith.' Once good faith is found, suppression becomes unavailable — a categorical suppression of the remedy itself. Theater ratio (0.58): Moderate-high. The good faith exception creates performative warrant-seeking: officers obtain warrants partly to satisfy the legal requirement, partly because Leon makes the warrant a talisman (good faith reliance on it shields evidence even if defective). The warrant itself becomes partially theatrical — its defect does not trigger the intended remedy. The ratio has risen over time as judicial practice has normalized the exception and officers have learned to anticipate it.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Law enforcement sees a balanced incentive system (Rope): warrants must be sought, but honest compliance is rewarded by evidence retention — Rope classification because the coordination benefit dominates. Defendants see pure extraction (Snare): no remedy exists despite violation, and they have no agency. Fourth Amendment doctrine sees mixed coordination and extraction (Tangled Rope): the rule coordinates warrant-seeking but extracts the exclusionary remedy when error is 'honest.' The analytical observer, seeing the full structure, also classifies as Tangled Rope: genuine coordination function (warrant requirement) combined with asymmetric extraction (remedy suppression for honest officers). The divergence reveals that the classification depends critically on whether one privileges the coordination function (warrant-seeking behavior) or the remedy extraction (suppression of relief).
 *
 * DIRECTIONALITY LOGIC:
 *   Law enforcement and prosecution derive low d values (0.15–0.25 range) from beneficiary status and arbitrage exit options — they can choose to pursue warrants or not, and they benefit from the exception. This yields f(d) ≈ 0.0 to 0.15, dampening their experienced extraction. Defendants facing honest-error searches derive high d values (0.95) from victim status and trapped exit — they cannot exit or contest, and they bear the full cost. This yields f(d) ≈ 1.42, amplifying their experienced extraction to near-maximal. The Fourth Amendment rule itself derives moderate d (~0.50) from the hybrid structure: it coordinates warrant-seeking (beneficiary function) but loses remedy (victim function). The analytical observer derives canonical d ~0.72 from analytical power position, applying the universal scope modifier σ(global) = 1.2, which amplifies effective extraction to 0.62 × 1.15 × 1.2 ≈ 0.86 — highly extractive viewed from civilizational/universal perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The good faith exception resolves mandatrophy by reframing the exclusionary rule's purpose: instead of categorical constitutional protection, the rule is recharacterized as a tool of deterrence. Once deterrence is the metric, good faith errors (where suppression deters no one) fall outside the rule's justified scope. The mandatrophy dissolves if one accepts the premise that exclusion is remedial (for deterrence), not constitutional (for categorical protection). However, the constraint remains tangled because deterrence theory is empirically contested (omega variable 2): if suppression does deter institutional actors, then the good faith exception removes a functional remedy under false premises. The reading's authority rests on an empirical claim (suppression does not deter good-faith actors) that is not settled. Tangled Rope is the correct classification because the reading maintains genuine coordination (warrant requirement) while extracting remedy (good faith exception) under contested justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reasonable_reliance_threshold,
    'What degree of warrant defect is ''too obvious'' for reasonable reliance? Where does the threshold lie between clear illegality and defensible mistake?',
    'Case law analysis across circuits; mapping fact patterns where courts grant vs. deny good faith exception; empirical study of officer training and warrant interpretation practices',
    'If threshold is high (only egregious defects disqualify reliance): more evidence admitted, more extraction. If threshold is low (technical defects disqualify reliance): more suppressions, less extraction. Current doctrine is in substantial flux.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reasonable_reliance_threshold, empirical, 'Threshold for what warrant defect precludes good faith reliance').

omega_variable(
    deterrence_theory_validity,
    'Does suppression actually deter Fourth Amendment violations by police departments, or is the deterrence mechanism empirically weak for institutional actors and only punishes defendants?',
    'Empirical study of police behavior before/after suppression rulings; comparison of violation rates in high-suppression vs. low-suppression jurisdictions; analysis of departmental training response to suppression outcomes',
    'If suppression deters: Leon''s premise is false, and the good faith exception removes deterrence without justification — reclassifies toward Snare. If suppression does not deter police: Leon''s premise is true, and the exception removes a dead remedy — reclassifies toward Rope (coordination without meaningful extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_theory_validity, empirical, 'Whether exclusionary rule suppression actually deters police violations').

omega_variable(
    honest_error_categorization_ambiguity,
    'Does ''honest error'' describe actual officer mistake or systematic institutional gap-exploitation masked as innocence?',
    'Departmental practice analysis: are warrant requests routinely filed in forms that create predictable defects? Do officers know the defects are likely? Are errors ''honest'' or ''structurally anticipated but deniable''?',
    'If honest: good faith exception correctly targets cases where suppression serves no deterrence purpose — reclassifies toward Rope. If systematically anticipated: good faith exception licenses predictable illegality under the cover of honest mistake — reclassifies toward Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honest_error_categorization_ambiguity, empirical, 'Whether good faith errors are genuinely honest or systematically anticipated').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a reading of the warrant-preference kernel (which emphasis on good faith shields police from the exclusionary rule''s intended remedy), or is it a kernel in its own right (the deterrence-based exception as an independent doctrinal commitment)?',
    'Historical doctrine analysis: did Leon self-consciously modify the warrant preference kernel, or did it introduce a new doctrinal axis? Did subsequent doctrine treat Leon as a reading of the Fourth Amendment''s warrant requirement, or as a standalone exception that competes with it?',
    'If a reading: this constraint is one position in the warrant-preference kernel contest; reclassification depends on which sibling reading prevails. If a kernel itself: the good faith exception must be evaluated as an independent commitment system with its own reference frame and authority grounding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether good faith exception is a reading or an independent kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(warrant_preference_reading__good_faith_exception_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wprgf_tr_t0, warrant_preference_reading__good_faith_exception_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(wprgf_tr_t15, warrant_preference_reading__good_faith_exception_reading, theater_ratio, 15, 0.51).
narrative_ontology:measurement(wprgf_tr_t30, warrant_preference_reading__good_faith_exception_reading, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(wprgf_be_t0, warrant_preference_reading__good_faith_exception_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(wprgf_be_t15, warrant_preference_reading__good_faith_exception_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(wprgf_be_t30, warrant_preference_reading__good_faith_exception_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(wprgf_su_t0, warrant_preference_reading__good_faith_exception_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(wprgf_su_t15, warrant_preference_reading__good_faith_exception_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(wprgf_su_t30, warrant_preference_reading__good_faith_exception_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(warrant_preference_reading__good_faith_exception_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(warrant_preference_reading__good_faith_exception_reading, warrant_preference_reading__exclusionary_rule_reading).
narrative_ontology:affects_constraint(warrant_preference_reading__good_faith_exception_reading, warrant_preference_reading__digital_carpenter_reading).

% DUAL FORMULATION NOTE:
% The good faith exception is one reading of the warrant-preference kernel; it shares the kernel with exclusionary_rule_reading and digital_carpenter_reading. Each reading instantiates different extractiveness and different structural relationships to beneficiaries/victims. The constraint family models the doctrinal contest over what the warrant requirement commits us to enforce.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
