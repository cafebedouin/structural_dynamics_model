% ============================================================================
% CONSTRAINT STORY: sixteenth_amendment__pollock_overruled_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sixteenth_amendment__pollock_overruled_reading, []).

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
 *   constraint_id: sixteenth_amendment__pollock_overruled_reading
 *   human_readable: The Sixteenth Amendment as Pollock Overruled (Article V Popular Sovereignty Reading)
 *   domain: constitutional_law/tax_doctrine
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the Sixteenth Amendment — the
 *   reading that understands the Amendment as a deliberate, definitive
 *   reversal of Pollock v. Farmers' Loan & Trust Co. (1895). In Pollock, the
 *   Court held that direct taxes (including income taxes on wealth) required
 *   apportionment among the states according to population. This created a
 *   constitutional barrier to federal income taxation without massive
 *   administrative complexity. The Sixteenth Amendment (1913) explicitly
 *   exempted income taxes from the apportionment requirement: 'The Congress
 *   shall have power to collect taxes on incomes, from whatever source
 *   derived, without apportionment among the several States.' This reading
 *   interprets that language as popular sovereignty overruling the Court —
 *   Article V amendment used to bury a Court precedent. The constraint
 *   exhibits tangled-rope dynamics: the Amendment coordinates the solution to
 *   the apportionment problem (enabling federal progressive taxation) while
 *   extracting from the Pollock settlement's protected-wealth interests
 *   (which apportionment was designed to shield). The suppression is the
 *   eradication of apportionment's protective force; the coordination is the
 *   enabling of federal income tax capacity.
 *
 * KEY AGENTS:
 *   - Pollock-Protected Wealth Interests: Primary victim (powerless/trapped) — the apportionment shield that Pollock created is eliminated entirely; no exit mechanism or alternative protection
 *   - Progressive Taxation Constitutional Basis (Federal Revenue System): Primary beneficiary (institutional/arbitrage) — the Amendment eliminates the apportionment obstacle, enabling federal income taxation without state-by-state proportional allocation
 *   - State Revenue Authorities: Secondary actor (moderate/constrained) — lose apportionment leverage but gain coordination benefits from expanded federal tax base
 *   - Realization Doctrine Institutional Structure: Secondary actor (institutional/arbitrage) — persists post-Amendment as doctrinal boundary between reached (realized income) and protected (unrealized wealth)
 *   - Pollock Precedent Institutional Memory: Institutional inertia (institutional/arbitrage) — Pollock's doctrinal apparatus continues to circulate in legal discourse despite operative reversal (piton perspective)
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing the specific reading of the Amendment's purpose as settled constitutional bedrock when the kernel is actually contested
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sixteenth_amendment__pollock_overruled_reading, 0.38).
domain_priors:suppression_score(sixteenth_amendment__pollock_overruled_reading, 0.62).
domain_priors:theater_ratio(sixteenth_amendment__pollock_overruled_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sixteenth_amendment__pollock_overruled_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(sixteenth_amendment__pollock_overruled_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sixteenth_amendment__pollock_overruled_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sixteenth_amendment__pollock_overruled_reading, tangled_rope).
narrative_ontology:human_readable(sixteenth_amendment__pollock_overruled_reading, "The Sixteenth Amendment as Pollock Overruled (Article V Popular Sovereignty Reading)").
narrative_ontology:topic_domain(sixteenth_amendment__pollock_overruled_reading, "constitutional_law/tax_doctrine").

domain_priors:requires_active_enforcement(sixteenth_amendment__pollock_overruled_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sixteenth_amendment__pollock_overruled_reading, '049c59c3-f0c8-4aac-8a03-7095649c3115').
narrative_ontology:cs_kernel_codification('049c59c3-f0c8-4aac-8a03-7095649c3115', formalized).
narrative_ontology:cs_authority_grounding('049c59c3-f0c8-4aac-8a03-7095649c3115', lineage).
narrative_ontology:cs_interpretation_layer_present('049c59c3-f0c8-4aac-8a03-7095649c3115').
narrative_ontology:cs_reading_relation('049c59c3-f0c8-4aac-8a03-7095649c3115', sixteenth_amendment__realization_doctrine_reading, coexists_with).
narrative_ontology:cs_reading_relation('049c59c3-f0c8-4aac-8a03-7095649c3115', sixteenth_amendment__wealth_tax_question_reading, influences).
narrative_ontology:cs_axiom('049c59c3-f0c8-4aac-8a03-7095649c3115', foundational, pollock_apportionment_shield_suppressed).
narrative_ontology:cs_axiom_status(pollock_apportionment_shield_suppressed, holdable).
narrative_ontology:cs_axiom_grounding('049c59c3-f0c8-4aac-8a03-7095649c3115', pollock_apportionment_shield_suppressed, deontological).
narrative_ontology:cs_axiom('049c59c3-f0c8-4aac-8a03-7095649c3115', foundational, income_taxation_unrestricted_by_apportionment).
narrative_ontology:cs_axiom_status(income_taxation_unrestricted_by_apportionment, holdable).
narrative_ontology:cs_axiom_grounding('049c59c3-f0c8-4aac-8a03-7095649c3115', income_taxation_unrestricted_by_apportionment, conventional).
narrative_ontology:cs_reference_frame('049c59c3-f0c8-4aac-8a03-7095649c3115', article_v_popular_sovereignty_override).
narrative_ontology:cs_drift_state('049c59c3-f0c8-4aac-8a03-7095649c3115', contemporary_wealth_tax_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('049c59c3-f0c8-4aac-8a03-7095649c3115', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(sixteenth_amendment__pollock_overruled_reading, sixteenth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sixteenth_amendment__pollock_overruled_reading, progressive_taxation_constitutional_basis).
narrative_ontology:constraint_beneficiary(sixteenth_amendment__pollock_overruled_reading, federal_revenue_system).
narrative_ontology:constraint_victim(sixteenth_amendment__pollock_overruled_reading, pollock_protected_wealth_interests).
narrative_ontology:constraint_victim(sixteenth_amendment__pollock_overruled_reading, apportionment_doctrine_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POLLOCK-PROTECTED WEALTH (SNARE) — Trapped by the Amendment's direct reversal of Pollock's apportionment shield. The constitutional protection created by Pollock v. Farmers' Loan & Trust Co. (1895) is suppressed entirely — no alternative forum, no procedural escape. The Amendment operates as a direct constitutional confiscation of the prior ruling's protective architecture. Maximum extraction: the protected status is eliminated with no compensation mechanism or grandfathering provision.
constraint_indexing:constraint_classification(sixteenth_amendment__pollock_overruled_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE REVENUE AUTHORITIES (TANGLED ROPE) — Constrained by loss of apportionment coordination leverage, yet benefit from federal tax base expansion that creates interstate revenue cooperation opportunities. States cannot exit federal income tax system (apportionment shield suppressed), but coordination on complementary state income taxes generates shared benefit. Hybrid: extraction (apportionment leverage gone) + coordination (expanded federal revenue creates tax base spillovers).
constraint_indexing:constraint_classification(sixteenth_amendment__pollock_overruled_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL PROGRESSIVE TAXATION REGIME (ROPE) — Primary beneficiary. The Amendment eliminates the apportionment shield that Pollock created, enabling federal income taxation without state-by-state proportional apportionment math. The constraint is pure coordination from this perspective: the Amendment solves the apportionment problem by constitutional amendment — it does not extract from the taxation regime, it enables the taxation regime's coordination function. Net beneficiary with arbitrage options (can revise doctrine, can reinterpret the Amendment's bounds).
constraint_indexing:constraint_classification(sixteenth_amendment__pollock_overruled_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POLLOCK PRECEDENT INSTITUTIONAL MEMORY (PITON) — The doctrine of apportionment that Pollock established persists in institutional form despite the Amendment's direct reversal. Courts, tax academics, and constitutional lawyers maintain Pollock's interpretive frameworks (distinctions between direct and indirect taxes, apportionment mechanics) as skeletal structure, even though the Amendment explicitly suppressed the protective outcome. The institutional inertia of Pollock persists as theater — cited, debated, distinguished — despite loss of operative force. Theater ratio ≥0.70: the doctrinal apparatus performs constitutional deference to Pollock while enforcing its opposite.
constraint_indexing:constraint_classification(sixteenth_amendment__pollock_overruled_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REALIZATION DOCTRINE BOUNDARY (SCAFFOLD) — Organized legal actors see the Amendment as having a built-in sunset clause: it reaches realized income, but Macomber's realization doctrine (Moore v. United States questions notwithstanding) continues to protect unrealized wealth. This reading interprets the Amendment as coordinating a temporary regime — income taxation on realized gains — with an implicit boundary at realization. The sunset is cognitive/doctrinal rather than temporal: if courts or legislatures redefine 'income' to include unrealized appreciation (as in mark-to-market or wealth tax proposals), the realization scaffold collapses. Constraint has low χ from this perspective because the boundary is perceived as stable and meaningful, not as suppressive enforcement.
constraint_indexing:constraint_classification(sixteenth_amendment__pollock_overruled_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL CONSTITUTIONAL LAW (MOUNTAIN) — From a civilizational perspective, the Sixteenth Amendment represents an immutable reordering of constitutional tax authority: the people, via Article V, overruled the Court and established that income taxation requires no apportionment. This is presented as constitutional bedrock — a foundational alteration of the constitutional structure that cannot be revised without another amendment. However, the structural data reveals this as a false summit: the Amendment's reach is itself contested (realization doctrine question, wealth tax question), and the 'burial of Pollock' framing naturalizes a specific reading of a contested kernel rather than establishing a settled fact.
constraint_indexing:constraint_classification(sixteenth_amendment__pollock_overruled_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sixteenth_amendment__pollock_overruled_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sixteenth_amendment__pollock_overruled_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sixteenth_amendment__pollock_overruled_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sixteenth_amendment__pollock_overruled_reading, TR),
    TR >= 0.70.

:- end_tests(sixteenth_amendment__pollock_overruled_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, reflecting that the Amendment suppresses a prior constitutional protection (apportionment) while enabling a new federal power (income taxation). The extraction is not maximal because the Amendment operates through Article V (legitimate popular constitutional amendment), not through coercive suppression. The suppression of the apportionment shield is thorough (0.62), but the mechanism is formal and transparent. Theater ratio (0.55): Moderate, above the baseline for straightforward constitutional text but below piton threshold (0.70). The Amendment's language is direct ('without apportionment'), but institutional practice (courts still citing Pollock, maintaining apportionment doctrine for other tax forms, realization doctrine performing as a secondary boundary) creates performative elements. The institutional theater around Pollock's legacy persists despite the Amendment's operative reversal.
 *
 * PERSPECTIVAL GAP:
 *   The Pollock-protected wealth perspective sees a snare (apportionment shield entirely suppressed, no exit). The federal progressive taxation perspective sees a rope (the Amendment solves a coordination problem — enabling income taxation without administrative apportionment burden). State authorities see a tangled rope (loss of apportionment leverage, but gain from federal base expansion). The realization doctrine boundary perspective sees a scaffold (the Amendment reaches income but has a built-in sunset at realization — unrealized wealth remains protected pending doctrinal redefinition). Pollock's institutional memory perspective sees a piton (Pollock doctrine circulates as constitutional deference despite operative loss of force — theater ≥0.70). The civilizational analytical perspective risks seeing a mountain (Article V amendment as constitutional bedrock, immutable reordering), but the structural data reveals this as a false summit — the Amendment's scope is itself contested (realization doctrine, wealth tax question), and the 'burial of Pollock' framing is ONE reading, not a settled fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from the agent's structural position relative to the constraint. Pollock-protected wealth holders (victims, trapped, powerless) derive high d → high f(d) → high experienced extraction. The federal taxation regime (beneficiary, institutional, arbitrage) derives low d from beneficiary status + arbitrage exit → negative f(d) → experiences the constraint as enabling coordination. State authorities (constrained exit, mixed victim/beneficiary) derive moderate d → moderate f(d). The realization doctrine boundary (institutional, arbitrage, but constrained by doctrinal dependency on the Amendment's realization language) derives intermediate d reflecting both benefit and constraint. Pollock's institutional memory (institutional, arbitrage, but performing rather than governing) derives low d because its operative force is gone, yet medium d from the fact that doctrinal practice must continuously justify its survival despite the Amendment.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is one reading of a contested kernel. The 'mandatrophy' would be: 'Is the Amendment a coordination mechanism (enabling federal taxation) or an extraction mechanism (suppressing Pollock's protection of wealth)?' This reading treats it as tangled_rope — both coordination and extraction, depending on the agent's position. The federal government coordinates a solution to the apportionment problem; Pollock-protected interests experience extraction. No single type resolves the full constraint. The false summit mountain perspective (that the Amendment establishes constitutional bedrock) is an instance of the oracle gap — the natural-law framing naturalizes a specific reading rather than acknowledging that the Amendment's scope and purpose are themselves contested across the three readings of the kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the Sixteenth Amendment''s core function to definitively bury Pollock''s apportionment shield, or is it to enable *one form of* income taxation while leaving apportionment doctrine''s conceptual structure partially intact?',
    'Jurisprudential analysis of how courts have treated apportionment doctrine post-Amendment. Textual analysis: does Amendment language (''income'') foreclose apportionment or merely waive it for income specifically? Historical intent analysis of ratification debates.',
    'If Amendment definitively buries Pollock: this reading (snare from wealth perspective, rope from tax authority perspective) is correct. If Amendment merely waives apportionment for income: realization_doctrine_reading gains coherence — the Amendment is narrower than this reading claims, protecting unrealized wealth via realization doctrine as an implicit boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the Amendment''s function is to bury Pollock or to narrowly waive apportionment for realized income').

omega_variable(
    realization_doctrine_grounding,
    'Is the realization doctrine (Macomber, Moore) an independent constitutional principle that limits the Amendment''s reach, or is it a tax-policy choice that Congress could reverse?',
    'Constitutional text analysis: does ''income'' in the Amendment include unrealized appreciation? Case law trajectory: does Moore v. United States resolve the question or leave it open? Legislative experimentation: if Congress enacts a mark-to-market or wealth tax and courts strike it down, the doctrine is constitutional limit. If courts uphold it, the doctrine is policy choice.',
    'If realization doctrine is constitutional limit: realization_doctrine_reading forecloses or at least constrains this reading''s maximalist interpretation. If doctrine is policy choice: this reading''s suppression of apportionment is more complete than the realization perspective admits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(realization_doctrine_grounding, empirical, 'Whether realization doctrine is constitutional limit or tax-policy choice').

omega_variable(
    wealth_tax_scope,
    'Does the Amendment''s logic (income requires no apportionment) extend to wealth taxes, or are wealth taxes distinct direct taxes subject to apportionment?',
    'Moore v. United States decision on mark-to-market tax; any future wealth-tax litigation addressing whether net-worth tax is covered by the Amendment or requires apportionment. Doctrinal analysis: is wealth a form of ''income'' under the Amendment''s logic?',
    'If wealth taxes are covered by the Amendment: this reading''s suppression of apportionment is maximalist and extends to wealth. If wealth taxes remain subject to apportionment: the Amendment''s scope is narrower — limited to income flows, not stock of wealth — and the Pollock settlement''s protected-wealth concept partially survives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wealth_tax_scope, empirical, 'Whether the Amendment''s logic extends to wealth taxes or remains limited to income').

omega_variable(
    article_v_precedent_durability,
    'Does a successful Article V amendment that reverses a Court ruling establish a permanent constitutional barrier to that ruling''s recurrence, or can doctrinal reversion occur through reinterpretation?',
    'Historical: have prior Article V amendments been reinterpreted away by courts, or do they remain durably enforceable? Logical: can courts distinguish Pollock to death without technically overruling the Amendment? Empirical: what is the track record of post-amendment apportionment litigation?',
    'If Article V amendments are durable: Pollock is truly buried and this reading''s snare/rope dynamics are stable. If amendments can be reinterpreted: the ''burial'' is less complete than this reading claims, and doctrine could migrate back toward Pollock-like protection of wealth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_v_precedent_durability, empirical, 'Durability of Article V amendment against doctrinal reversion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sixteenth_amendment__pollock_overruled_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1913_functional, sixteenth_amendment__pollock_overruled_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(theater_1938_increasing, sixteenth_amendment__pollock_overruled_reading, theater_ratio, 25, 0.52).
narrative_ontology:measurement(theater_1963_ongoing, sixteenth_amendment__pollock_overruled_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(extractiveness_1913_initial, sixteenth_amendment__pollock_overruled_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(extractiveness_1938_expansion, sixteenth_amendment__pollock_overruled_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(extractiveness_1963_stable, sixteenth_amendment__pollock_overruled_reading, base_extractiveness, 50, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(suppression_1913_ratification, sixteenth_amendment__pollock_overruled_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(suppression_1938_new_deal, sixteenth_amendment__pollock_overruled_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(suppression_1963_warren_court, sixteenth_amendment__pollock_overruled_reading, suppression_requirement, 50, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sixteenth_amendment__pollock_overruled_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sixteenth_amendment__pollock_overruled_reading, sixteenth_amendment__realization_doctrine_reading).
narrative_ontology:affects_constraint(sixteenth_amendment__pollock_overruled_reading, sixteenth_amendment__wealth_tax_question_reading).
narrative_ontology:affects_constraint(sixteenth_amendment__pollock_overruled_reading, pollock_v_farmers_loan_apportionment_protection).
narrative_ontology:affects_constraint(sixteenth_amendment__pollock_overruled_reading, income_tax_direct_tax_classification).

% DUAL FORMULATION NOTE:
% The Sixteenth Amendment is a contested kernel instantiated in three constraint stories: pollock_overruled_reading (this), realization_doctrine_reading, and wealth_tax_question_reading. Each reading has its own epsilon value reflecting different assumptions about the Amendment's scope and purpose. The three stories form a constraint family linked by network edges. The upstream Pollock constraint (pollock_v_farmers_loan_apportionment_protection) is the structural antecedent; the Amendment is defined against it. The income_tax_direct_tax_classification constraint shares the taxonomic structure (direct vs. indirect tax) that the Amendment operates within.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
