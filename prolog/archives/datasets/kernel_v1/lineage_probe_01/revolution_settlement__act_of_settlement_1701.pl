% ============================================================================
% CONSTRAINT STORY: revolution_settlement__act_of_settlement_1701
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_revolution_settlement__act_of_settlement_1701, []).

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
 *   constraint_id: revolution_settlement__act_of_settlement_1701
 *   human_readable: The Act of Settlement 1701: Statutory Succession and Judicial Independence
 *   domain: political/constitutional_history
 *
 * SUMMARY:
 *   The Act of Settlement 1701 completed the Glorious Revolution settlement
 *   by converting the Crown from a hereditary office secured by divine right
 *   into a parliamentary office whose occupant must satisfy statutory
 *   conditions: Protestant faith, parliamentary approval of the line of
 *   succession, and judicial tenure during good behaviour. The constraint
 *   exhibits hybrid character: it coordinates genuine institutional benefits
 *   (parliamentary supremacy, judicial independence, religious settlement)
 *   while simultaneously extracting from specific victim groups (the excluded
 *   Stuart line, the principle of hereditary absolutism). The constraint is
 *   one reading of the contested Revolution settlement kernel — competing
 *   against the Bill of Rights 1689 (which emphasizes parliamentary privilege
 *   and removal of Crown suspending/dispensing powers) and the Toleration Act
 *   1689 (which emphasizes religious accommodation through exclusion). This
 *   reading instantiates the specifically successional and monarchical
 *   transformation: the Crown became a parliamentary office, the succession
 *   became a parliamentary choice, and judges became officers of Parliament
 *   rather than servants of the Crown.
 *
 * KEY AGENTS:
 *   - Parliamentary Supremacy: Primary beneficiary (institutional/arbitrage) — gains constitutional power to determine succession and remove Crown prerogatives through statute
 *   - Judiciary: Secondary beneficiary (institutional/constrained) — gains security of tenure during good behaviour; also victim to parliamentary supremacy (constrained by statute)
 *   - Hanoverian Succession: Primary beneficiary (institutional/arbitrage) — gains legitimacy through parliamentary approval; institutional identity fused with statutory framework
 *   - Stuart Line: Primary victim (powerless/trapped) — permanently excluded from succession by statute with no exit mechanism short of constitutional revolution
 *   - Hereditary Absolutism: Philosophical victim (institutional/trapped) — the principle of hereditary right independent of parliamentary approval is suppressed by the statute
 *   - Protestant Established Church: Secondary beneficiary (institutional/constrained) — gains religious settlement (Protestant succession guaranteed); victim to parliamentary subordination of doctrine to politics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(revolution_settlement__act_of_settlement_1701, 0.28).
domain_priors:suppression_score(revolution_settlement__act_of_settlement_1701, 0.42).
domain_priors:theater_ratio(revolution_settlement__act_of_settlement_1701, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(revolution_settlement__act_of_settlement_1701, extractiveness, 0.28).
narrative_ontology:constraint_metric(revolution_settlement__act_of_settlement_1701, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(revolution_settlement__act_of_settlement_1701, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(revolution_settlement__act_of_settlement_1701, tangled_rope).
narrative_ontology:human_readable(revolution_settlement__act_of_settlement_1701, "The Act of Settlement 1701: Statutory Succession and Judicial Independence").
narrative_ontology:topic_domain(revolution_settlement__act_of_settlement_1701, "political/constitutional_history").

domain_priors:requires_active_enforcement(revolution_settlement__act_of_settlement_1701).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(revolution_settlement__act_of_settlement_1701, '55ab4340-41e5-4e99-a797-7966371d05cc').
narrative_ontology:cs_kernel_codification('55ab4340-41e5-4e99-a797-7966371d05cc', formalized).
narrative_ontology:cs_authority_grounding('55ab4340-41e5-4e99-a797-7966371d05cc', extraction).
narrative_ontology:cs_interpretation_layer_present('55ab4340-41e5-4e99-a797-7966371d05cc').
narrative_ontology:cs_reading_relation('55ab4340-41e5-4e99-a797-7966371d05cc', revolution_settlement__bill_of_rights_1689, coexists_with).
narrative_ontology:cs_reading_relation('55ab4340-41e5-4e99-a797-7966371d05cc', revolution_settlement__toleration_settlement_1689, coexists_with).
narrative_ontology:cs_axiom('55ab4340-41e5-4e99-a797-7966371d05cc', foundational, parliamentary_supremacy_in_succession).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_in_succession, holdable).
narrative_ontology:cs_axiom_grounding('55ab4340-41e5-4e99-a797-7966371d05cc', parliamentary_supremacy_in_succession, deontological).
narrative_ontology:cs_axiom('55ab4340-41e5-4e99-a797-7966371d05cc', foundational, judicial_independence_from_crown_pleasure).
narrative_ontology:cs_axiom_status(judicial_independence_from_crown_pleasure, holdable).
narrative_ontology:cs_axiom_grounding('55ab4340-41e5-4e99-a797-7966371d05cc', judicial_independence_from_crown_pleasure, deontological).
narrative_ontology:cs_reference_frame('55ab4340-41e5-4e99-a797-7966371d05cc', parliamentary_constitutional_supremacy).
narrative_ontology:cs_drift_state('55ab4340-41e5-4e99-a797-7966371d05cc', contemporary_constitutional_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('55ab4340-41e5-4e99-a797-7966371d05cc', '').
narrative_ontology:cs_kernel_id(revolution_settlement__act_of_settlement_1701, revolution_settlement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(revolution_settlement__act_of_settlement_1701, parliamentary_supremacy).
narrative_ontology:constraint_beneficiary(revolution_settlement__act_of_settlement_1701, judicial_independence).
narrative_ontology:constraint_beneficiary(revolution_settlement__act_of_settlement_1701, hanoverian_succession).
narrative_ontology:constraint_victim(revolution_settlement__act_of_settlement_1701, stuart_line_excluded).
narrative_ontology:constraint_victim(revolution_settlement__act_of_settlement_1701, hereditary_absolutism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED STUART LINE (SNARE) — The Act permanently forecloses the Stuart line from the throne through statutory mechanism that cannot be dissolved without constitutional revolution. Full victim status: extraction is total (the Crown itself), suppression is absolute (legal prohibition with no exit), and no coordination benefit accrues. The constraint's entire function — securing Protestant succession — depends on suppressing this agent's claim.
constraint_indexing:constraint_classification(revolution_settlement__act_of_settlement_1701, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PARLIAMENTARY SUPREMACY (ROPE) — Parliament sees the constraint as pure coordination: the statute establishes Parliament's power to determine the succession line, transforming hereditary right into parliamentary choice. Low base extraction (ε ≈ 0.28) because Parliament is the beneficiary and designer. The arbitrage exit option reflects Parliament's ability to amend the statute (at high constitutional cost, but structurally possible). Net beneficiary — coordination function delivers parliamentary power.
constraint_indexing:constraint_classification(revolution_settlement__act_of_settlement_1701, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: JUDICIARY (TANGLED ROPE) — Judges gain security from removal at pleasure (beneficiary status: good behaviour tenure is coordination that enables judicial independence). But the constraint also extracts: judges are now bound to the statutory line and Parliamentary sovereignty; they cannot interpret common law rights against the succession statute (victim status). Mixed extraction and coordination — beneficiary from independence, victim from supremacy.
constraint_indexing:constraint_classification(revolution_settlement__act_of_settlement_1701, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HANOVERIAN CROWN (PITON) — The Crown accepts the statute as the condition of legitimacy (good behaviour tenure, parliamentary override of succession). Theater is moderate (0.35) because the constraint's enforcement mechanisms are genuine (Parliament retains sanction power) even though the Crown's practical compliance is often ceremonial. The Crown's institutional inertia — accepting statutory constraint despite residual prerogative claims — generates the piton signature: functional constraint degraded by institutional memory of prior power.
constraint_indexing:constraint_classification(revolution_settlement__act_of_settlement_1701, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PROTESTANT ESTABLISHED CHURCH (TANGLED ROPE) — The constraint coordinates religious and political stability (beneficiary: the succession is explicitly secured as Protestant). But the church also bears extraction: the statute subordinates religious doctrine to parliamentary succession politics (victim: doctrinal autonomy constrained by what serves the Hanoverian line). Moderate agent (organized but subject to parliamentary statute); constrained exit (changing the succession requires constitutional amendment).
constraint_indexing:constraint_classification(revolution_settlement__act_of_settlement_1701, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal view, the constraint might appear as a natural outcome of constitutional logic: once Parliament asserted sovereignty, succession by statute became inevitable — any claim to hereditary right independent of parliamentary approval is logically incompatible with parliamentary supremacy. This perspective risks classifying the Act as a mountain (immutable logical consequence), but the structural data reveals this as false: the constraint is a contingent political settlement, benefiting specific agents (Parliament, judiciary, Hanoverian line) and extracting from others (Stuart line, hereditary absolutism). The engine's false summit detector will flag this.
constraint_indexing:constraint_classification(revolution_settlement__act_of_settlement_1701, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(revolution_settlement__act_of_settlement_1701_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(revolution_settlement__act_of_settlement_1701, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(revolution_settlement__act_of_settlement_1701, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(revolution_settlement__act_of_settlement_1701, TR),
    TR >= 0.70.

:- end_tests(revolution_settlement__act_of_settlement_1701_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate-low. The Act suppresses one specific claim (Stuart hereditary right) rather than extracting broadly from multiple groups. Judges benefit from good behaviour tenure (low extraction from them). Parliament gains supremacy (negative extraction for Parliament). The primary extraction is directed at the Stuart line specifically and at the broader principle of hereditary absolutism. Measuring extractiveness as 'total impact on the field' yields moderate value because most institutional actors (except the Stuart line) experience coordination benefit. Suppression (0.42): Moderate. The statute legally prohibits Stuart succession and the exercise of Crown prerogatives that Parliament claims (suspending laws, dispensing with statutes). But suppression is not total: the Crown retains significant practical power, and the prohibition is statutory rather than backed by overwhelming force (compared to later totalitarian suppression). Theater ratio (0.35): Moderate-low. The constraint's enforcement mechanisms are genuine (Parliament retains sanction power, courts enforce tenure), but the Crown's practical compliance is often ceremonial and negotiated rather than coerced. The ratio rises modestly over the twenty-year interval (to 0.38) as the initial coercive enforcement softens into institutional habit.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the Stuart line's perspective (snare: total extraction, no exit, no coordination benefit) and Parliament's perspective (rope: pure coordination, beneficiary status, arbitrage exit) is maximal. The judiciary occupies the hybrid position (tangled rope) — beneficiary from independence, victim from supremacy. The analytical observer risks collapsing this gap by seeing the constraint as a logically inevitable consequence of parliamentary supremacy (mountain), but the structural data reveals contingency: Parliament's power to exclude the Stuart line depends on the settlement being accepted as legitimate, which depends on the coercive and negotiating power that preceded the statute. The constraint is not a natural law — it is a political settlement enforced by a specific configuration of power.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from structural relationships: the Stuart line as victims with trapped exit experience maximum d (≈0.95), producing high f(d) and high experienced extraction. Parliament as beneficiaries with arbitrage exit experience low d (≈0.10), producing negative f(d) and low/negative experienced extraction. Judges as mixed beneficiaries/victims with constrained exit experience moderate d (≈0.55), producing moderate f(d) and moderate experienced extraction. The analytical observer at civilizational scope experiences d ≈0.72 (observer position), producing f(d) ≈1.15, but risks naturalizing contingent institutional arrangements as logical necessity — false summit signature.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the hybrid character (tangled rope) is not ambiguous — it is structurally overdetermined. The Act coordinates genuine institutional benefits (parliamentary supremacy, judicial independence) while extracting from specific victims (the Stuart line, hereditary absolutism). No reduction to pure coordination or pure extraction is possible: both functions operate simultaneously. The temptation to classify it as mountain (logical necessity) is refuted by recognizing that the constraint is a contingent settlement enforced by specific agents — not an immutable law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statutory_vs_common_law_supremacy,
    'Is the Act of Settlement a statute subordinate to common law rights, or a constitutional statute that establishes the supreme law of the realm?',
    'Legal history analysis: how courts treated challenges to the succession clause; whether judges asserted common law rights to override the statute; whether parliamentary amendment became the only recognized path to constitutional change',
    'If statutory subordinate: the constraint remains contestable and subject to common law interpretation (lower extractiveness from the bench). If constitutional supreme: the constraint is effectively immutable without revolutionary disruption (higher extractiveness and suppression).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statutory_vs_common_law_supremacy, conceptual, 'Whether the Act is statutory or constitutional law in supremacy').

omega_variable(
    good_behaviour_tenure_enforcement,
    'Did good behaviour tenure provide judges with genuine security from removal at displeasure, or was it theatrically applied while Crown pressure and political alignment remained effective removal mechanisms?',
    'Historical analysis of judicial removals post-1701; correlation between judicial independence from Crown and party politics; examination of whether judges who offended the reigning Crown retained office',
    'If enforced: judicial independence is genuine coordination benefit (beneficiary status for judges is real). If theatrical: judges remain victims of indirect removal pressure through political alignment (extraction persists despite the statute).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(good_behaviour_tenure_enforcement, empirical, 'Whether good behaviour tenure provided genuine judicial security').

omega_variable(
    reading_vs_sibling_supremacy,
    'Which settlement document is the kernel: the Act of Settlement 1701 (statutory succession), the Bill of Rights 1689 (parliamentary privilege and removal of Crown suspending/dispensing powers), or the Toleration Act 1689 (religious settlement)?',
    'Constitutional history: which document courts treat as supreme; which provisions override others in cases of conflict; which reading controls the interpretation of remaining settlement provisions',
    'If Act of Settlement supreme: this reading (statutory succession) is the master frame and the Bill of Rights is subordinate (influences relation). If Bill of Rights supreme: this reading is subordinate to parliamentary privilege (forecloses relation with different implications). If none supreme: coexists_with relation is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_sibling_supremacy, conceptual, 'Constitutional supremacy among the three settlement documents').

omega_variable(
    hereditary_right_vs_parliamentary_choice,
    'Does the statute suppress hereditary right as a principle, or merely direct hereditary right through a new line (Hanover instead of Stuart)?',
    'Philosophical analysis: whether the Act asserts Parliament''s power to choose any succession (rejecting hereditary principle) or asserts Parliament''s power to select which hereditary line succeeds (preserving hereditary principle but subordinating it to parliamentary confirmation)',
    'If suppresses principle: hereditary absolutism is the victim (extraction is from a political philosophy). If directs hereditary right: the principle survives but is now parliamentary-determined (extraction is specifically from the Stuart line, not from hereditary right per se).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hereditary_right_vs_parliamentary_choice, conceptual, 'Whether the Act rejects hereditary right as a principle or redirects it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(revolution_settlement__act_of_settlement_1701, 1701, 1721).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(revo_tr_t0, revolution_settlement__act_of_settlement_1701, theater_ratio, 0, 0.25).
narrative_ontology:measurement(revo_tr_t10, revolution_settlement__act_of_settlement_1701, theater_ratio, 10, 0.35).
narrative_ontology:measurement(revo_tr_t20, revolution_settlement__act_of_settlement_1701, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(revo_be_t0, revolution_settlement__act_of_settlement_1701, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(revo_be_t10, revolution_settlement__act_of_settlement_1701, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(revo_be_t20, revolution_settlement__act_of_settlement_1701, base_extractiveness, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(revolution_settlement__act_of_settlement_1701, enforcement_mechanism).
narrative_ontology:affects_constraint(revolution_settlement__act_of_settlement_1701, revolution_settlement__bill_of_rights_1689).
narrative_ontology:affects_constraint(revolution_settlement__act_of_settlement_1701, revolution_settlement__toleration_settlement_1689).

% DUAL FORMULATION NOTE:
% The Act of Settlement 1701 is one reading of the contested Revolution settlement kernel. The Bill of Rights 1689 and Toleration Act 1689 are sibling readings of the same kernel. These three constraints are linked by network.affects_constraints to indicate kernel family membership. Each story has its own ε value reflecting the structural specificity of that reading: the Act of Settlement centers on succession law (ε=0.28); the Bill of Rights centers on parliamentary privilege (ε may differ); the Toleration Act centers on religious settlement (ε may differ). The three readings are not reducible to a single constraint — each instantiates a distinct slice of the settlement's normative architecture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
