% ============================================================================
% CONSTRAINT STORY: udhr_article_3__procedural_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__procedural_hybrid_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: udhr_article_3__procedural_hybrid_reading
 *   human_readable: UDHR Article 3 — Procedural Hybrid Reading (Due Process Without Substantive Resolution)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the procedural hybrid reading of UDHR Article 3's
 *   security guarantee: the article obligates habeas corpus access and an
 *   absolute torture prohibition, but declines to resolve whether 'security'
 *   requires only freedom from arbitrary state violence (the
 *   negative_liberty_reading) or also positive provision of material
 *   conditions for life (the positive_entitlement_reading). The hybrid
 *   reading is the operative compliance standard in most international
 *   monitoring practice — states are assessed on procedural adequacy, not on
 *   welfare outcomes — which makes it the reading with the most institutional
 *   traction and also the reading most exposed to
 *   formal-compliance-without-substantive-protection drift. Emergency
 *   detention regimes since 2001 have substantially expanded the population
 *   for whom habeas review exists nominally but functions on extended
 *   timelines or with heavy executive deference, which is the extraction this
 *   story measures: not in the torture prohibition itself (near-absolute, low
 *   extraction) but in the gap between procedural existence and procedural
 *   efficacy for detained and stateless populations.
 *
 * KEY AGENTS:
 *   - states_seeking_flexible_compliance: institutional beneficiary/agenda_setter — cites procedural compliance without resolving substantive liberty/welfare claims
 *   - judicial_review_bodies: institutional beneficiary/agenda_setter — gains a stable, justiciable mandate under the procedural reading
 *   - indefinitely_detained_persons: powerless payer — trapped by nominal-but-inefficacious habeas review under emergency detention regimes
 *   - torture_survivors_denied_remedy: powerless payer — absolute prohibition in text, discretionary remedy in practice
 *   - stateless_and_undocumented_persons: powerless payer — fall outside the jurisdictional hooks the procedural guarantee presupposes
 *   - welfare_rights_advocates: excluded organized voice — contest the hybrid reading's neutrality as a substantive choice favoring negative liberty
 *   - treaty_monitoring_bodies: institutional observer — assess procedural compliance but lack mandate to adjudicate the welfare question this reading leaves open
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__procedural_hybrid_reading, 0.42).
domain_priors:suppression_score(udhr_article_3__procedural_hybrid_reading, 0.48).
domain_priors:theater_ratio(udhr_article_3__procedural_hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__procedural_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__procedural_hybrid_reading, "UDHR Article 3 — Procedural Hybrid Reading (Due Process Without Substantive Resolution)").
narrative_ontology:topic_domain(udhr_article_3__procedural_hybrid_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__procedural_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__procedural_hybrid_reading, '38e37993-a890-4839-8e00-4549f776ca6e').
narrative_ontology:cs_kernel_codification('38e37993-a890-4839-8e00-4549f776ca6e', fixed_text).
narrative_ontology:cs_authority_grounding('38e37993-a890-4839-8e00-4549f776ca6e', distributed).
narrative_ontology:cs_reading_relation('38e37993-a890-4839-8e00-4549f776ca6e', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('38e37993-a890-4839-8e00-4549f776ca6e', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_axiom('38e37993-a890-4839-8e00-4549f776ca6e', foundational, procedural_guarantee_is_severable_from_substantive_theory).
narrative_ontology:cs_axiom_status(procedural_guarantee_is_severable_from_substantive_theory, holdable).
narrative_ontology:cs_axiom_grounding('38e37993-a890-4839-8e00-4549f776ca6e', procedural_guarantee_is_severable_from_substantive_theory, conventional).
narrative_ontology:cs_axiom('38e37993-a890-4839-8e00-4549f776ca6e', secondary, justiciability_is_the_operative_compliance_standard).
narrative_ontology:cs_axiom_status(justiciability_is_the_operative_compliance_standard, holdable).
narrative_ontology:cs_axiom_grounding('38e37993-a890-4839-8e00-4549f776ca6e', justiciability_is_the_operative_compliance_standard, instrumental).
narrative_ontology:cs_reference_frame('38e37993-a890-4839-8e00-4549f776ca6e', id_1948_drafting_compromise_floor).
narrative_ontology:cs_drift_state('38e37993-a890-4839-8e00-4549f776ca6e', post_2001_emergency_detention_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('38e37993-a890-4839-8e00-4549f776ca6e', '').
narrative_ontology:cs_kernel_id(udhr_article_3__procedural_hybrid_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, states_seeking_flexible_compliance).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, judicial_review_bodies).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, detained_persons_with_counsel_access).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, indefinitely_detained_persons).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, torture_survivors_denied_remedy).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, stateless_and_undocumented_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ratify and cite Article 3 to claim human rights compliance while retaining maximal discretion over welfare provision and the scope of emergency detention, since the article commits them to procedural minimums (habeas, non-torture) but not to any substantive liberty or welfare floor. They can satisfy the letter of the norm through formal judicial review mechanisms that operate slowly or narrowly, and cite compliance internationally without altering underlying detention or welfare policy.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, states_seeking_flexible_compliance, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__procedural_hybrid_reading, states_seeking_flexible_compliance, agenda_setter).

% Courts and tribunals gain a stable jurisdictional mandate to review detention and torture claims — habeas petitions and torture prohibitions are justiciable, procedurally tractable questions, unlike the open-ended welfare and liberty disputes the hybrid reading declines to resolve. Their institutional standing and caseload depend on the procedural guarantees remaining the operative content of Article 3.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, judicial_review_bodies, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__procedural_hybrid_reading, judicial_review_bodies, agenda_setter).

% Detainees who can reach counsel and courts obtain real procedural protection — habeas review, protection from torture, access to a hearing. Their exit from detention runs through the judicial review mechanism the hybrid reading guarantees; for this group the constraint functions largely as advertised.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, detained_persons_with_counsel_access, beneficiary,
    moderate, biographical, constrained, national).

% Held under emergency detention regimes (national security, immigration, counter-terrorism) where habeas review exists on paper but is delayed, narrowed by executive-deference doctrines, or made practically inaccessible. The procedural guarantee is not extended in substance, and because Article 3 does not require a liberty floor beyond procedure, indefinite detention survives review so long as some hearing eventually occurs.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, indefinitely_detained_persons, payer,
    powerless, biographical, trapped, national).

% Experience the torture prohibition's absoluteness in principle but face high evidentiary burdens, state secrecy doctrines, and absent enforcement mechanisms in practice — the prohibition is unconditional in text but the remedy structure is discretionary, leaving survivors procedurally guaranteed and substantively unremedied.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, torture_survivors_denied_remedy, payer,
    powerless, biographical, trapped, national).

% Fall outside standard habeas frameworks because their legal status is itself contested, so the procedural protections the hybrid reading guarantees presuppose a legal personhood and jurisdictional hook these persons often lack (offshore detention, transit zones, non-refoulement gaps). The hybrid reading's silence on substantive entitlement compounds this: there is no welfare floor to fall back on when procedural access fails.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, stateless_and_undocumented_persons, payer,
    powerless, immediate, trapped, national).

% Argue that security without material conditions for survival is hollow, and that the hybrid reading's refusal to resolve the welfare question allows states to claim compliance while people die of preventable deprivation. Their position is a live alternative reading (positive_entitlement_reading) but is not adjudicated by this constraint; they contest the hybrid reading's neutrality as itself a substantive choice favoring the negative-liberty status quo.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, welfare_rights_advocates, excluded,
    organized, generational, constrained, global).

% UN human rights committees and special rapporteurs assess state compliance against the procedural minimums, issue findings on habeas and torture practice, but have no mandate under this reading to adjudicate welfare or substantive liberty claims, since the hybrid reading does not extend Article 3 that far.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, treaty_monitoring_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__procedural_hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(udhr_article_3__procedural_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a floor of procedural guarantees — access to judicial review of detention, absolute prohibition on torture — that states can be held to using tractable, justiciable standards, avoiding the need for international consensus on contested welfare and substantive liberty questions.
% TRANSFER_FUNCTION: Moves genuine protective value to detainees who can access functioning courts and counsel; for those without effective access, the same guarantee moves legitimacy and compliance credit to the state without a corresponding transfer of protection, since procedural satisfaction requires only that a review process exist, not that it produce timely or substantive relief.
% ABSENT_VOICES: Welfare rights advocates who read Article 3's security guarantee as requiring positive provision are structurally external to this reading's scope — their claims are neither affirmed nor rejected but rendered non-justiciable under a procedural-only construction. Indefinitely detained persons and stateless populations, who most need a stronger reading, are the least able to participate in the interpretive contest that would produce one.
% DISAPPEARANCE_RATIONALE: States and judicial bodies would argue the world rearranges catastrophically — habeas and anti-torture norms are load-bearing and their absence would remove the primary check on state detention power. Welfare rights advocates would argue the world is largely unchanged for those already failed by the procedural floor, since the hybrid reading already withholds the substantive protection they say is needed; removing it would mainly cost the legitimacy currently borrowed from an unfulfilled promise.
% FOUNDING_PROBLEM: Post-1948 drafters needed a security guarantee broad enough for near-universal ratification, which meant leaving the negative-liberty/positive-entitlement contest unresolved and settling on procedural minimums (habeas, torture prohibition) as the achievable common ground.
% FOUNDING_PROBLEM_CORROBORATION: Judicial review bodies and ratifying states attest the procedural floor remains live and functioning wherever courts retain independence. Independent monitoring bodies (UN Special Rapporteur on Torture, Human Rights Committee) and academic human rights scholarship outside the ratifying states corroborate that the procedural guarantee is frequently satisfied formally while failing substantively in emergency detention and offshore/transit contexts — suggesting the founding compromise persists institutionally but its protective function has partially decoupled from its textual guarantee for the most vulnerable populations.
narrative_ontology:disappearance_verdict(udhr_article_3__procedural_hybrid_reading, contested).
narrative_ontology:founding_problem_status(udhr_article_3__procedural_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__procedural_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(udhr_article_3__procedural_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__procedural_hybrid_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__procedural_hybrid_reading_tests).
:- end_tests(udhr_article_3__procedural_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the torture prohibition component is near-absolute and low-extraction, while the habeas/judicial-review component has drifted toward formal-compliance patterns that extract legitimacy from an unfulfilled promise — the two components pull the aggregate in opposite directions, landing at moderate rather than high ε, consistent with the expected structural delta for this reading. Theater ratio rises over the interval (0.18 to 0.40) tracking the growth of formalized-but-slow review processes (indefinite detention with periodic but non-binding hearings) that satisfy the procedural letter without producing timely substantive relief. Suppression is moderate (0.48) — lower than a pure extraction constraint because functioning judicial systems do provide real remedy for a meaningful subset of detainees, but non-trivial because executive-deference doctrines and state-secrecy privileges actively narrow the review's practical reach for the most vulnerable subgroups.
 *
 * PERSPECTIVAL GAP:
 *   From the state and judicial-body seats, Article 3 under this reading looks like a rope: a workable coordination solution that does not overreach into contested welfare territory. From the seat of an indefinitely detained person facing an emergency-detention hearing scheduled years out, or a stateless person outside any court's jurisdictional reach, the identical guarantee looks like tangled_rope shading toward snare — procedural in name, extractive of protection and legitimacy in operation. The engine computes both from the same structural data; the divergence is the object of study, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   States and judicial bodies sit near the beneficiary end: states gain compliance credit and retain policy discretion over welfare and detention scope; courts gain durable, tractable jurisdiction. Detained persons with functioning counsel access sit near symmetric-to-beneficiary — the guarantee genuinely protects them. Indefinitely detained persons, torture survivors denied remedy, and stateless/undocumented persons sit near the full-target end: trapped exit options, no coalition leverage against detaining states, and the procedural guarantee's substance depends on jurisdictional and resource access they structurally lack.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading was never mandated to resolve the welfare/liberty substantive contest — its founding function was to secure near-universal ratification via an achievable procedural floor. That founding function (procedural minimums as common ground) remains partially live: judicial review and torture prohibition retain real content in states with independent courts. But for detained and stateless populations facing emergency and offshore regimes, the procedural floor has been substituted for genuine protection — a Goodhart-style drift where formal hearing existence replaces substantive habeas efficacy as the measured compliance target. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (a workable, justiciable, near-universally-adoptable floor) while registering the asymmetric extraction that has grown alongside it, rather than either dismissing the whole article as pure extraction or crediting it with protections it does not, on this reading, provide.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_sufficiency_vs_substantive_gap,
    'Does satisfying the procedural floor (habeas access exists, torture is prohibited) constitute genuine compliance with Article 3''s security guarantee, or does the hybrid reading''s refusal to resolve the substantive liberty/welfare question let states claim compliance while producing no material change in detainee or vulnerable-population outcomes?',
    'Comparative empirical study of habeas efficacy rates (time-to-hearing, release rates, remedy availability) across ratifying states with independent versus executive-deferential judiciaries, cross-referenced against detainee outcome data.',
    'If procedural existence strongly correlates with substantive outcomes, the hybrid reading functions closer to rope; if the correlation is weak or negative in emergency/offshore contexts, the tangled_rope classification is conservative and the true structure trends toward snare for the most vulnerable subgroups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_sufficiency_vs_substantive_gap, empirical, 'Whether procedural compliance under the hybrid reading tracks substantive protection or diverges from it.').

omega_variable(
    hybrid_reading_as_substantive_choice,
    'Is declining to resolve the liberty/welfare contest itself a neutral procedural stance, or is it a substantive choice that structurally favors the negative_liberty_reading by defaulting to no welfare floor when procedural avenues fail?',
    'Cannot be resolved empirically — this is a conceptual question about whether procedural neutrality is achievable or whether all readings of a security guarantee necessarily encode a substantive theory of what security requires.',
    'If the hybrid reading is judged to covertly favor negative liberty, its claimed neutrality is itself a form of extraction (borrowing legitimacy from apparent even-handedness); if genuine neutrality is achievable, the hybrid reading''s moderate ε reflects real restraint rather than disguised alignment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_reading_as_substantive_choice, conceptual, 'Whether procedural neutrality between liberty and welfare readings is itself a substantive position.').

omega_variable(
    kernel_framing_alternative,
    'Could this kernel be framed instead around the drafting history and travaux préparatoires as the authoritative referent, rather than around the text''s plain procedural content — and would that framing produce a different cs_pattern classification (e.g., a more clearly `distributed` authority_grounding reflecting genuine drafter disagreement, versus the `distributed` grounding chosen here for institutional monitoring practice)?',
    'Historical-legal analysis of the 1948 drafting committee''s recorded debates to determine whether a decisive intent existed that later interpretive practice has obscured.',
    'If drafting history reveals a decisive original intent toward either negative liberty or positive entitlement, the hybrid reading''s claim to represent the kernel''s plain content (rather than a subsequent interpretive settlement) would be weakened, potentially reclassifying its authority_grounding from distributed toward lineage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether the hybrid reading is best grounded in text-as-written or in a contested drafting history.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__procedural_hybrid_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t0, udhr_article_3__procedural_hybrid_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(udhr_tr_t12, udhr_article_3__procedural_hybrid_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(udhr_tr_t24, udhr_article_3__procedural_hybrid_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(udhr_tr_t38, udhr_article_3__procedural_hybrid_reading, theater_ratio, 38, 0.31).
narrative_ontology:measurement(udhr_tr_t50, udhr_article_3__procedural_hybrid_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(udhr_tr_t63, udhr_article_3__procedural_hybrid_reading, theater_ratio, 63, 0.38).
narrative_ontology:measurement(udhr_tr_t76, udhr_article_3__procedural_hybrid_reading, theater_ratio, 76, 0.4).

% Extraction over time
narrative_ontology:measurement(udhr_be_t0, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(udhr_be_t12, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 12, 0.27).
narrative_ontology:measurement(udhr_be_t24, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 24, 0.31).
narrative_ontology:measurement(udhr_be_t38, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 38, 0.34).
narrative_ontology:measurement(udhr_be_t50, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(udhr_be_t63, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 63, 0.4).
narrative_ontology:measurement(udhr_be_t76, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 76, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t0, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(udhr_su_t12, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 12, 0.34).
narrative_ontology:measurement(udhr_su_t24, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 24, 0.38).
narrative_ontology:measurement(udhr_su_t38, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 38, 0.42).
narrative_ontology:measurement(udhr_su_t50, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 50, 0.45).
narrative_ontology:measurement(udhr_su_t63, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 63, 0.47).
narrative_ontology:measurement(udhr_su_t76, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 76, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__procedural_hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_article_3__procedural_hybrid_reading, 0.12).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__positive_entitlement_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the udhr_article_3 kernel. negative_liberty_reading treats Article 3 as prohibiting state violence via narrow procedural justice (lowest ε, least contested). positive_entitlement_reading treats it as obligating material welfare provision (highest ε, most contested, since it authorizes the broadest state and international obligation with the weakest enforcement infrastructure). procedural_hybrid_reading (this story) sits between them: it takes on genuine, moderate extraction from the gap between procedural existence and procedural efficacy, while declining to adjudicate the deeper substantive contest the other two readings resolve in opposite directions. All three share the same kernel text but are authored as separate constraints per the ε-invariance principle, since their beneficiary/victim structures and extraction profiles differ substantially.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
