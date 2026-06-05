% ============================================================================
% CONSTRAINT STORY: parliament_act_1911__coerced_consent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parliament_act_1911__coerced_consent_reading, []).

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
 *   constraint_id: parliament_act_1911__coerced_consent_reading
 *   human_readable: Parliament Act 1911: Coerced Consent Reading
 *   domain: legal/constitutional/doctrinal
 *
 * SUMMARY:
 *   The Parliament Act 1911 is one of the foundational statutes of modern
 *   British constitutional law. In this reading — the coerced_consent_reading
 *   — the Act is understood as extracted authority, passed by the House of
 *   Lords under the explicit threat that rejection would trigger the creation
 *   of 500 new Liberal peers, dissolving the Lords' institutional identity
 *   and power. The government made a credible institutional threat: accept
 *   the veto reduction (from absolute veto to a two-year delay), or face
 *   institutional destruction. The Lords, trapped between the choice to
 *   capitulate or to be flooded with party loyalists, consented under duress.
 *   This reading frames the Act not as consensual constitutional settlement
 *   but as coercive extraction — the government obtained institutional change
 *   via threat, not agreement. The constraint embeds a fundamental tension:
 *   constitutional authority grounded in coercion. The legitimacy question is
 *   irreducible: if law passed under duress is still law, what does that mean
 *   for the constitutional order's claim to rest on consent?
 *
 * KEY AGENTS:
 *   - House of Lords: Primary victim (powerless/trapped) — consents under institutional extinction threat; no meaningful exit; bearer of extraction cost
 *   - Liberal Government (Asquith): Primary beneficiary (institutional/arbitrage) — wields coercive threat; obtains legislative dominance; captures institutional change
 *   - Constitutional Legitimacy Order: Secondary victim (moderate/constrained) — authority grounded in coercion is perpetually contested; bears the cost of delegitimized law
 *   - Constitutional Reform Movement: Organized observer (organized/mobile) — recognizes hybrid coordination-extraction; can articulate alternatives but cannot undo the coercive mechanism now embedded in statute
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing coercion as institutional inevitability when alternatives existed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parliament_act_1911__coerced_consent_reading, 0.58).
domain_priors:suppression_score(parliament_act_1911__coerced_consent_reading, 0.72).
domain_priors:theater_ratio(parliament_act_1911__coerced_consent_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parliament_act_1911__coerced_consent_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(parliament_act_1911__coerced_consent_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(parliament_act_1911__coerced_consent_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parliament_act_1911__coerced_consent_reading, snare).
narrative_ontology:human_readable(parliament_act_1911__coerced_consent_reading, "Parliament Act 1911: Coerced Consent Reading").
narrative_ontology:topic_domain(parliament_act_1911__coerced_consent_reading, "legal/constitutional/doctrinal").

domain_priors:requires_active_enforcement(parliament_act_1911__coerced_consent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(parliament_act_1911__coerced_consent_reading, '2671217c-f04e-4c65-8807-0a4765a6b2dc').
narrative_ontology:cs_kernel_codification('2671217c-f04e-4c65-8807-0a4765a6b2dc', formalized).
narrative_ontology:cs_authority_grounding('2671217c-f04e-4c65-8807-0a4765a6b2dc', extraction).
narrative_ontology:cs_interpretation_layer_present('2671217c-f04e-4c65-8807-0a4765a6b2dc').
narrative_ontology:cs_reading_relation('2671217c-f04e-4c65-8807-0a4765a6b2dc', parliament_act_1911__commons_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('2671217c-f04e-4c65-8807-0a4765a6b2dc', parliament_act_1911__delegated_continuation_reading, influences).
narrative_ontology:cs_axiom('2671217c-f04e-4c65-8807-0a4765a6b2dc', foundational, authority_extracted_by_threat_is_delegitimized).
narrative_ontology:cs_axiom_status(authority_extracted_by_threat_is_delegitimized, holdable).
narrative_ontology:cs_axiom_grounding('2671217c-f04e-4c65-8807-0a4765a6b2dc', authority_extracted_by_threat_is_delegitimized, deontological).
narrative_ontology:cs_axiom('2671217c-f04e-4c65-8807-0a4765a6b2dc', foundational, institutional_threat_as_extraction_mechanism).
narrative_ontology:cs_axiom_status(institutional_threat_as_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('2671217c-f04e-4c65-8807-0a4765a6b2dc', institutional_threat_as_extraction_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('2671217c-f04e-4c65-8807-0a4765a6b2dc', consensual_constitutional_amendment).
narrative_ontology:cs_drift_state('2671217c-f04e-4c65-8807-0a4765a6b2dc', immediate_post_1911_passage, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2671217c-f04e-4c65-8807-0a4765a6b2dc', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(parliament_act_1911__coerced_consent_reading, parliament_act_1911).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parliament_act_1911__coerced_consent_reading, liberal_government).
narrative_ontology:constraint_victim(parliament_act_1911__coerced_consent_reading, house_of_lords).
narrative_ontology:constraint_victim(parliament_act_1911__coerced_consent_reading, constitutional_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HOUSE OF LORDS (SNARE) — Trapped by the threat of institutional dilution. No meaningful exit option: rejection triggers flood of 500 new peers, destroying the chamber's identity and effective power irreversibly. Consent is extracted under duress, not freely given. Maximum extraction — the Lords surrender constitutional prerogative to a subordinate veto while the threat remains credible.
constraint_indexing:constraint_classification(parliament_act_1911__coerced_consent_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSTITUTIONAL LEGITIMACY (SNARE) — The constitutional order's legitimacy depends on consent, not coercion. The 1911 Act, extracted by threat, corrupts the legitimacy of the resulting settlement. Future appeals to the Act's legitimacy are undermined by its coercive origin. High extraction — the constitutional order bears the cost of delegitimized law while the government captures the benefit.
constraint_indexing:constraint_classification(parliament_act_1911__coerced_consent_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LIBERAL GOVERNMENT (ROPE) — Sees the Act as legitimate coordination: resolving the budget deadlock between chambers, enabling legislative function. The threat is reframed as credible negotiating posture, not coercion. For the beneficiary, the Act solves a coordination problem and stabilizes their legislative capacity. Net beneficiary — extraction runs away from this agent.
constraint_indexing:constraint_classification(parliament_act_1911__coerced_consent_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL REFORM MOVEMENT (TANGLED ROPE) — Sees the Act as embedding both genuine coordination (resolving executive-legislative deadlock) and asymmetric extraction (coerced institutional change). Organized agents recognize the settlement as hybrid: the Act does solve a real coordination failure (the budget crisis), but via threat-extraction rather than consensus. Mobile agents (reformers, legal scholars) can articulate alternative paths, but the coercive mechanism is now locked into law. Moderate extraction with partial coordination benefit.
constraint_indexing:constraint_classification(parliament_act_1911__coerced_consent_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / INSTITUTIONAL INEVITABILITY (MOUNTAIN) — From civilizational perspective, the coercion appears as immutable institutional logic: when legislative deadlock becomes intolerable, the executive must threaten institutional disruption to break the impasse. The specific form (peer-flooding threat) is the mechanism available in 1911 parliamentary architecture. This view naturalizes coercion as inherent to democratic governance during constitutional crisis. However, this reading risks false summit status — the engine will detect that institutional 'inevitability' is being naturalized when alternative resolution paths existed.
constraint_indexing:constraint_classification(parliament_act_1911__coerced_consent_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parliament_act_1911__coerced_consent_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parliament_act_1911__coerced_consent_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parliament_act_1911__coerced_consent_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(parliament_act_1911__coerced_consent_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(parliament_act_1911__coerced_consent_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The government extracts institutional change via credible threat, not consensus. The beneficiary (government) captures all the gain (legislative dominance); the victim (Lords) bears the cost (institutional subordination). The extraction is not maximal (0.70+) because the Act also solves a genuine coordination failure (the budget deadlock), and the Lords retain some veto power (two-year delay). But the primary mechanism is coercive, not coordinative. Suppression (0.72): High. The credible threat of institutional destruction is a suppressive force of immense magnitude. The Lords cannot reject the Act without accepting institutional dilution. The threat structure — flood the chamber or accept subordination — eliminates genuine alternatives. Suppression decays slightly over time (0.80 → 0.72) as the institutional threat becomes historical rather than active; the initial coercive force is maximum at passage. Theater ratio (0.38): Low. The mechanism is transparent coercion, not performative ritual. The government makes an explicit institutional threat; the Lords understand the choice clearly. There is no elaborate theater around the legitimacy claim — the Act's legitimacy is precisely what is in question. The low theater distinguishes this from a constraint maintained through fiction or ceremony.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The beneficiary (Liberal Government) experiences the Act as legitimate coordination solving a deadlock — they see Rope. The trapped victim (House of Lords) experiences pure extraction — they see Snare. The constitutional order itself sees delegitimized authority — it experiences Snare as victim. The organized reformers see the hybrid (genuine deadlock solved, but via coercion) — they experience Tangled Rope. The analytical observer risks the false summit of naturalizing coercion as inevitable institutional logic when alternatives existed. The entire presheaf over observation contexts reveals that the Act's constitutional status depends on which frame is adopted.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the coercive mechanism. The government (beneficiary with arbitrage exit) has d ≈ 0.05 — they benefit, they can walk away, they experience minimal chi. The Lords (victim with trapped exit) have d ≈ 0.95 — they are the extraction target, they cannot walk away without institutional destruction, they experience maximum chi. The constitutional order (victim with constrained exit) has d ≈ 0.85 — it bears the cost of delegitimized authority; it could theoretically reject the Act's legitimacy but doing so would fracture the legal system. Each perspective's derived d maps to its experienced chi via the sigmoid and scope modifier. The perspectival gap emerges from this: beneficiaries and victims occupy inverse d-positions, producing inverse chi values despite identical base metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for this constraint is whether coerced law is law. This reading resolves the tension by declaring that coerced authority is extracted authority, not legitimate constitutional foundation. The snare classification captures this: the Lords' institutional survival is the suppressed alternative; the government's institutional threat is the extraction mechanism. The analysis rejects the false-summit framing (coercion as institutional inevitability) by documenting alternative resolution paths that were politically available but not chosen. If the government had pursued consensus reform (gradual veto reduction, sunset clauses, negotiated succession), the constraint would have been Tangled Rope or Scaffold. The choice to use coercive threat reveals the mechanism as extraction, not inherent institutional logic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_negotiation_framing,
    'Is the threat of 500 new peers a coercive extraction mechanism or a legitimate negotiating tactic within constitutional crisis management?',
    'Counterfactual analysis: what alternative paths existed for resolving the budget deadlock without coercion? If alternatives existed but were not pursued, coercion is the chosen mechanism, not structural necessity. If no alternatives existed, the framing shifts toward institutional inevitability.',
    'If coercion: constraint classification remains Snare; extracted authority is delegitimized. If negotiation: classification shifts toward Tangled Rope or Rope; authority gains legitimacy from breaking genuine deadlock.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_vs_negotiation_framing, conceptual, 'Whether the peer-flood threat is coercive extraction or legitimate crisis negotiation').

omega_variable(
    consensual_vs_extracted_legitimacy,
    'Does law passed under duress carry legitimacy sufficient to ground constitutional authority, or is coerced consent a contradiction that delegitimizes the entire settlement?',
    'Historical acceptance and challenge: does the constitutional order treat the Act as fully legitimate (no caveat), or does it acknowledge the coercive origin as a permanent legitimacy caveat? Do courts or reformers invoke the coercive origin to challenge the Act''s authority?',
    'If fully legitimate: the coercive origin is historical context, not structural problem. If caveat: the Act''s legitimacy is perpetually contested by its own history of extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensual_vs_extracted_legitimacy, conceptual, 'Whether coerced consent can ground lasting constitutional legitimacy').

omega_variable(
    institutional_flooding_credibility,
    'Was the threat to create 500 new peers credible in 1911, or merely a negotiating bluff that would not have been executed?',
    'Historical analysis: evidence of executive resolve (prior creation of peerages as political tool, budget constraints, institutional capacity). Does contemporaneous correspondence reveal actual willingness to execute, or was it tactical posturing?',
    'If credible threat: suppression is structural (coercion via credible institution-destruction). If bluff: suppression is theater (coercion via credible-appearing but unexecutable threat), and the constraint may reclassify downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_flooding_credibility, empirical, 'Whether the peer-flooding threat was credible institutional coercion or negotiating theater').

omega_variable(
    reading_coherence_via_kernel_contest,
    'Does the coerced_consent_reading coexist logically with the commons_supremacy_reading and delegated_continuation_reading, or does it foreclose one or both?',
    'Structural analysis of axioms: if this reading''s foundational axiom (authority extracted by threat is delegitimized) directly contradicts a sibling''s core premise, foreclosure is established. If both can be held within different institutional frameworks or by different parties, they coexist.',
    'If foreclosure: the kernel contest has logical winners and losers. If coexistence: the kernel is genuinely contested across irreconcilable worldviews (not just parties disagreeing on facts). Framework drift measurement: as institutions treat the Act, do they acknowledge all readings or progressively eliminate some?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coherence_via_kernel_contest, conceptual, 'Structural relationships between coerced_consent_reading and sibling readings of the Parliament Act 1911 kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parliament_act_1911__coerced_consent_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pa1911_coerced_theater_t0, parliament_act_1911__coerced_consent_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(pa1911_coerced_theater_t5, parliament_act_1911__coerced_consent_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(pa1911_coerced_theater_t10, parliament_act_1911__coerced_consent_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(pa1911_coerced_extract_t0, parliament_act_1911__coerced_consent_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(pa1911_coerced_extract_t5, parliament_act_1911__coerced_consent_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(pa1911_coerced_extract_t10, parliament_act_1911__coerced_consent_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(pa1911_coerced_suppress_t0, parliament_act_1911__coerced_consent_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(pa1911_coerced_suppress_t5, parliament_act_1911__coerced_consent_reading, suppression_requirement, 5, 0.76).
narrative_ontology:measurement(pa1911_coerced_suppress_t10, parliament_act_1911__coerced_consent_reading, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parliament_act_1911__coerced_consent_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(parliament_act_1911__coerced_consent_reading, parliament_act_1911__commons_supremacy_reading).
narrative_ontology:affects_constraint(parliament_act_1911__coerced_consent_reading, parliament_act_1911__delegated_continuation_reading).

% DUAL FORMULATION NOTE:
% The Parliament Act 1911 kernel is contested across three structurally distinct readings. This story (coerced_consent_reading) models the Act as coercive extraction with ε=0.58 (Snare). The commons_supremacy_reading models the same statutory text as legitimate democratic supremacy with different ε and classification. The delegated_continuation_reading models the same text as establishing subordinate legislative authority with yet different ε. The three stories are linked via network.affects_constraints; none is primary. The kernel contest is genuinely irreconcilable — it is not a factual disagreement that data can resolve, but a reading contest where different authority structures and legitimacy framings produce incommensurable interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
