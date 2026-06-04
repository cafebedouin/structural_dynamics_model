% ============================================================================
% CONSTRAINT STORY: bill_of_rights_1791__expression_conscience_amendments
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bill_of_rights_1791__expression_conscience_amendments, []).

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
 *   constraint_id: bill_of_rights_1791__expression_conscience_amendments
 *   human_readable: First Amendment: Conscience and Expression Against Federal Establishment and Censorship (1791)
 *   domain: constitutional_law/political_liberty
 *
 * SUMMARY:
 *   The First Amendment embodies one specific reading of the Bill of Rights
 *   kernel: protection of conscience, religious liberty, and expressive
 *   freedom against federal establishment, prohibition, and censorship. This
 *   reading frames the constitutional commitment as fundamentally about
 *   preventing federal orthodoxy-making and the suppression of dissenting
 *   speech, belief, and publication. The beneficiary set is dissenters,
 *   religious minorities, organized sects, and the press — those whose
 *   survival depends on freedom from federal censorship. The victim set is
 *   federal officials and enforcement mechanisms that would otherwise have
 *   the power to police belief and control publication. This constraint is a
 *   pure coordination mechanism from the dissenter's perspective: it solves
 *   the collective action problem of how heterodox speakers can participate
 *   in political and religious life without federal suppression. From the
 *   federal enforcement perspective, it is a tangled rope: it provides
 *   genuine coordination value (clear boundaries reduce jurisdictional
 *   uncertainty) while also extracting the tool of censorship from federal
 *   hands. The analytical observer at civilizational scope risks naturalizing
 *   this reading as a timeless principle of human conscience, when it is
 *   actually one specific political settlement. The constraint demonstrates
 *   false summit vulnerability: it appears as natural law ('freedom of
 *   conscience cannot be legitimately suppressed') but may actually
 *   naturalize contingent 1791 federal-state power allocation and
 *   20th-century incorporation doctrine.
 *
 * KEY AGENTS:
 *   - Religious Dissenters and Minority Sects (powerless/constrained, biographical) — primary beneficiaries; experience constraint as liberation enabling sect formation and survival
 *   - Political Opposition and Heterodox Speakers (powerless/constrained, biographical) — beneficiaries; depend on freedom to critique federal orthodoxy without federal suppression
 *   - Press and Publishers (moderate/mobile, biographical) — organized beneficiaries; experience constraint as coordination enabling publication without prior restraint or licensing
 *   - Federal Officials and Law Enforcement (institutional/constrained, biographical) — experience constraint as tangled rope: coordination benefit (clear jurisdictional boundaries) paired with extraction (loss of censorship tool)
 *   - States (institutional/arbitrage, generational) — ambiguous position; constraint limits federal power but does not speak to state-level establishment or censorship (pre-14th Amendment incorporation)
 *   - Analytical Observer (analytical/analytical, civilizational) — risks naturalizing contingent institutional reading as universal natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bill_of_rights_1791__expression_conscience_amendments, 0.18).
domain_priors:suppression_score(bill_of_rights_1791__expression_conscience_amendments, 0.12).
domain_priors:theater_ratio(bill_of_rights_1791__expression_conscience_amendments, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bill_of_rights_1791__expression_conscience_amendments, extractiveness, 0.18).
narrative_ontology:constraint_metric(bill_of_rights_1791__expression_conscience_amendments, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(bill_of_rights_1791__expression_conscience_amendments, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bill_of_rights_1791__expression_conscience_amendments, rope).
narrative_ontology:human_readable(bill_of_rights_1791__expression_conscience_amendments, "First Amendment: Conscience and Expression Against Federal Establishment and Censorship (1791)").
narrative_ontology:topic_domain(bill_of_rights_1791__expression_conscience_amendments, "constitutional_law/political_liberty").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bill_of_rights_1791__expression_conscience_amendments, '5b285f31-4bc5-4cbb-9237-b55eb226b965').
narrative_ontology:cs_kernel_codification('5b285f31-4bc5-4cbb-9237-b55eb226b965', formalized).
narrative_ontology:cs_authority_grounding('5b285f31-4bc5-4cbb-9237-b55eb226b965', lineage).
narrative_ontology:cs_interpretation_layer_present('5b285f31-4bc5-4cbb-9237-b55eb226b965').
narrative_ontology:cs_reading_relation('5b285f31-4bc5-4cbb-9237-b55eb226b965', bill_of_rights_1791__criminal_procedure_amendments, coexists_with).
narrative_ontology:cs_reading_relation('5b285f31-4bc5-4cbb-9237-b55eb226b965', bill_of_rights_1791__reserved_powers_amendments, coexists_with).
narrative_ontology:cs_reading_relation('5b285f31-4bc5-4cbb-9237-b55eb226b965', bill_of_rights_1791__security_arms_amendments, coexists_with).
narrative_ontology:cs_axiom('5b285f31-4bc5-4cbb-9237-b55eb226b965', foundational, federal_censorship_impermissible).
narrative_ontology:cs_axiom_status(federal_censorship_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('5b285f31-4bc5-4cbb-9237-b55eb226b965', federal_censorship_impermissible, deontological).
narrative_ontology:cs_axiom('5b285f31-4bc5-4cbb-9237-b55eb226b965', foundational, conscience_inviolable_to_federal_power).
narrative_ontology:cs_axiom_status(conscience_inviolable_to_federal_power, holdable).
narrative_ontology:cs_axiom_grounding('5b285f31-4bc5-4cbb-9237-b55eb226b965', conscience_inviolable_to_federal_power, deontological).
narrative_ontology:cs_reference_frame('5b285f31-4bc5-4cbb-9237-b55eb226b965', federal_no_orthodoxy_establishment).
narrative_ontology:cs_drift_state('5b285f31-4bc5-4cbb-9237-b55eb226b965', contemporary_enforcement_interpretation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5b285f31-4bc5-4cbb-9237-b55eb226b965', '').
narrative_ontology:cs_kernel_id(bill_of_rights_1791__expression_conscience_amendments, bill_of_rights_1791).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bill_of_rights_1791__expression_conscience_amendments, religious_dissenters).
narrative_ontology:constraint_beneficiary(bill_of_rights_1791__expression_conscience_amendments, political_opposition).
narrative_ontology:constraint_beneficiary(bill_of_rights_1791__expression_conscience_amendments, press).
narrative_ontology:constraint_beneficiary(bill_of_rights_1791__expression_conscience_amendments, conscience_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISSENTER / HETERODOX SPEAKER (ROPE) — A person holding minority religious belief or unpopular political view experiences the First Amendment constraint as genuine coordination: it solves the collective action problem of 'how can I speak without federal suppression?' The constraint enables their participation in public discourse; it carries them as much as constrains them. Extraction is minimal — the constraint benefits them. They experience this as a coordination mechanism protecting their ability to exist publicly.
constraint_indexing:constraint_classification(bill_of_rights_1791__expression_conscience_amendments, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: PRESS / PUBLISHERS (ROPE) — Publishers and journalists experience the First Amendment as coordination enabling information circulation and political commentary. The constraint solves the problem of how printing presses and publishing can function without federal censorship licensing. Beneficiary status is clear — the press enjoys protection against federal prior restraint. The constraint is largely unextractive from this position; it is a coordination mechanism that enables their profession.
constraint_indexing:constraint_classification(bill_of_rights_1791__expression_conscience_amendments, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: ORGANIZED RELIGIOUS DISSENTERS / SECTARIAN COMMUNITIES (ROPE) — Groups holding heterodox religious beliefs (Quakers, Unitarians, Baptists, Catholics in post-1791 Protestant-dominant America) experienced this constraint as liberation from federal religious establishment. The constraint coordinates protection for sect formation and practice. Organized communities have bargaining power through coalition; they can exit to state-level legal forums if federal enforcement falters. The constraint is a pure coordination mechanism from their perspective — no asymmetric extraction, high perceived mutability.
constraint_indexing:constraint_classification(bill_of_rights_1791__expression_conscience_amendments, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL OFFICIALS / LAW ENFORCEMENT (TANGLED ROPE) — Federal judges, marshals, and officials charged with enforcing federal law experience the First Amendment as a constraint that both coordinates judicial procedure AND extracts value from their enforcement capacity. They benefit from the coordination function: a clear rule against federal censorship eliminates uncertainty about jurisdictional boundaries and prevents interstate religious wars. But they also experience extraction: the amendment prevents them from using federal power to enforce orthodoxy, suppress sedition, or control publication. This is genuine tangled rope — both coordination (reduced jurisdictional chaos) and extraction (loss of enforcement tool).
constraint_indexing:constraint_classification(bill_of_rights_1791__expression_conscience_amendments, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational and universal perspective, the First Amendment expresses a natural-law commitment: that conscience and expression are intrinsic human capacities that cannot be legitimately suppressed by any government, not only the federal one. The principle appears immutable across time and scope — rooted in human dignity and the impossibility of authentic belief under duress. However, the omega variables and kernel context flag this perspective as potentially naturalizing what is actually a specific historical-institutional reading of the Bill of Rights. The mountain classification is structurally fragile.
constraint_indexing:constraint_classification(bill_of_rights_1791__expression_conscience_amendments, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bill_of_rights_1791__expression_conscience_amendments_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bill_of_rights_1791__expression_conscience_amendments, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bill_of_rights_1791__expression_conscience_amendments, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(bill_of_rights_1791__expression_conscience_amendments_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint exhibits minimal net extraction because it is primarily a coordination mechanism — it benefits the dissenter, press, and organized religious communities far more than it extracts from them. Federal officials experience some extraction (loss of censorship capacity), but this is intentional restraint, not hidden coercion. The low extractiveness reflects that this constraint genuinely solves a collective action problem (how can dissenters exist publicly without federal suppression) without creating asymmetric advantage. Suppression (0.12): Low. The constraint itself has minimal suppressive force — it is a prohibition on federal action, not an affirmative obligation requiring coercion. The small suppression value reflects the minimal enforcement machinery needed to prevent federal censorship. Theater ratio (0.35): Moderate-low. The constraint has some performative element — how courts interpret 'freedom of speech' or 'establishment' requires judicial articulation and can drift from textual meaning. But the core function is substantive: preventing federal censorship is a real barrier, not merely performative. The theater has increased slightly over time (from 0.25 to 0.35) as judicial interpretation has become more complex and less textually anchored, but the functional coordination work remains primary. The low theater ratio distinguishes this constraint from pitons, which are mostly performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a significant perspectival gap between the dissenter/beneficiary perspective (pure coordination, low extraction) and the federal official perspective (tangled rope, both coordination and extraction). The dissenter sees genuine protection; the federal official sees both a restraint and a jurisdictional clarification. The analytical observer risks seeing a timeless natural law when the constraint is actually a specific institutional arrangement. The gap between beneficiary and officer derives from the asymmetry of who benefits from censorship prohibition versus who loses enforcement capacity. No single perspective captures the full structure — the constraint is definitively not a mountain (different actors perceive it differently) and not a snare (beneficiaries outnumber victims, and benefits are substantial). The rope classification emerges as the primary type because the coordination function dominates, but the tangled_rope perspective from federal enforcement is legitimate and reveals that some extraction is present even if not dominant.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from its beneficiary/victim relationship to the constraint. Dissenters are beneficiaries (low d ~0.15), experiencing net protection; federal officials are asymmetrically affected but in a constrained position (moderate d ~0.50), experiencing both coordination benefit and extraction loss. The analytical observer (analytical power, analytical exit) derives d ~0.72 (canonical fallback). The low extractiveness value (0.18) reflects that beneficiaries substantially outnumber victims and benefit substantially, driving down the average directionality across all perspectives. The tangled_rope classification from the federal perspective shows that even constrained-exit officials experience genuine coordination alongside extraction — not pure snare. This is the diagnostic signal: if the federal official perspective produced snare, the constraint would be purely extractive, and the analytical observer's mountain would be revealed as false summit. Instead, the tangled_rope shows that the constraint has real coordination function, supporting the legitimacy of the natural-law mountain reading at the analytical context.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_institutional_grounding,
    'Is the First Amendment grounded in natural law (conscience and expression are inherent, inalienable capacities of human dignity) or in contingent institutional history (a specific negotiated restraint on federal power adopted in 1791 under particular political conditions)?',
    'Historical analysis of framers'' intent; comparison of this reading with alternative Bill of Rights readings (criminal procedure, reserved powers, security against standing army); examination of whether conscience/expression principles would hold at state level or globally without explicit amendment; tracking of how courts have treated the amendment when federal enforcement mechanisms change',
    'If natural law: mountain classification is correct; the constraint is immutable across observers and time horizons. If institutional: the constraint should reclassify as rope or tangled_rope; the mountain is a false summit, naturalizing a contingent political settlement. The entire kernel may decompose into context-dependent readings rather than a single authoritative principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_grounding, conceptual, 'Whether the First Amendment expresses natural law or contingent institutional history').

omega_variable(
    federal_vs_state_asymmetry,
    'Does this constraint''s logic apply symmetrically to state governments, or is the First Amendment solely a restraint on federal power, with states retaining capacity to establish religion and censor expression at the state level?',
    'Fourteenth Amendment incorporation doctrine analysis; historical record of state-level orthodoxy enforcement pre- and post-incorporation (14th Amendment ratification, 1868); textual analysis of ''Congress shall make no law'' (federal only) vs universality claims; empirical tracking of which constraints on expression were state-level vs federal',
    'If federal-only: extractiveness increases at state level; the constraint is not universal, only a federal-federal restraint; the mountain reading collapses. If incorporated/symmetric: the constraint expresses a universal principle; mountain classification is stronger. This asymmetry is the crux of the false summit question: natural laws should apply universally, but this amendment was drafted only for federal power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_vs_state_asymmetry, empirical, 'Whether the First Amendment restrains state governments or only federal government').

omega_variable(
    sectarian_privilege_vs_universal_access,
    'Does the First Amendment''s protection of ''free exercise of religion'' and ''free speech'' actually produce equal access to conscience-expression for all groups, or does it privileged certain powerful groups (established press, organized religions, property owners with public platforms) while leaving marginalized voices systemically silenced?',
    'Comparative analysis of who can actually exercise speech and religion rights in practice vs in law; historical tracking of suppressed groups (enslaved people, indigenous peoples, women, labor organizers) despite the amendment; measurement of actual expression capacity by power level; analysis of network effects in media and publishing that concentrate platform access',
    'If equal access: rope classification is correct; the constraint solves a genuine collective action problem symmetrically. If privileged: the constraint may reclassify as snare or tangled_rope with high suppression; the ''neutrality'' of the amendment masks structural extraction favoring the already-powerful. The false summit detection might trigger: the constraint appears as natural liberty but actually coordinates unequal access.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sectarian_privilege_vs_universal_access, empirical, 'Whether First Amendment provides equal access to expression or privileges certain groups').

omega_variable(
    reading_stability_across_political_regimes,
    'This constraint instantiates the reading that the Bill of Rights is a restraint on federal orthodoxy-making and censorship. But does this reading remain stable when the political regime or the power of the federal executive shifts? Would a regime committed to suppression interpret or enforce the amendment differently?',
    'Historical analysis of First Amendment enforcement under different presidents and courts; tracking of how the constraint''s interpretations have shifted (Alien and Sedition Acts era, WWI sedition cases, McCarthy era, civil rights era, modern); comparative law analysis of how democracies with identical constitutional text enforce freedom of speech differently based on regime stability and executive power',
    'If stable across regimes: the constraint''s institutional grounding is robust; rope classification holds. If regime-dependent: the constraint may be fragile; the mountain/rope distinction depends on regime commitment; the false summit is revealed when authoritarianism interprets the same text differently. This addresses whether the amendment itself or regime loyalty does the actual constraining work.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_stability_across_political_regimes, empirical, 'Whether the First Amendment reading remains stable across different political regimes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bill_of_rights_1791__expression_conscience_amendments, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(brights_expr_tr_t0, bill_of_rights_1791__expression_conscience_amendments, theater_ratio, 0, 0.25).
narrative_ontology:measurement(brights_expr_tr_t50, bill_of_rights_1791__expression_conscience_amendments, theater_ratio, 50, 0.32).
narrative_ontology:measurement(brights_expr_tr_t100, bill_of_rights_1791__expression_conscience_amendments, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(brights_expr_be_t0, bill_of_rights_1791__expression_conscience_amendments, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(brights_expr_be_t50, bill_of_rights_1791__expression_conscience_amendments, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(brights_expr_be_t100, bill_of_rights_1791__expression_conscience_amendments, base_extractiveness, 100, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(brights_expr_su_t0, bill_of_rights_1791__expression_conscience_amendments, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(brights_expr_su_t50, bill_of_rights_1791__expression_conscience_amendments, suppression_requirement, 50, 0.1).
narrative_ontology:measurement(brights_expr_su_t100, bill_of_rights_1791__expression_conscience_amendments, suppression_requirement, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bill_of_rights_1791__expression_conscience_amendments, information_standard).
narrative_ontology:affects_constraint(bill_of_rights_1791__expression_conscience_amendments, bill_of_rights_1791__criminal_procedure_amendments).
narrative_ontology:affects_constraint(bill_of_rights_1791__expression_conscience_amendments, bill_of_rights_1791__reserved_powers_amendments).
narrative_ontology:affects_constraint(bill_of_rights_1791__expression_conscience_amendments, bill_of_rights_1791__security_arms_amendments).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested Bill of Rights kernel. The constraint family includes four sibling readings corresponding to the four principal normative commitments of the Bill of Rights: expression/conscience (this story), criminal procedure (IV-VIII), reserved powers (IX-X), and security against standing armies (II-III). Each reading has its own extractiveness value, beneficiary/victim structure, and ε-invariant classification. They are not observables of a single constraint — they are structurally distinct constraints grounded in the same formalized kernel text. The network edges record mutual influence: this reading (expression/conscience) affects the others through shared constitutional authority and through potential conflicts of interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
