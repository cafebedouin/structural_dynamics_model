% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__institutional_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__institutional_displacement_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dueling_disappearance_mechanism__institutional_displacement_reading
 *   human_readable: Institutional Substitution Reading of Dueling's Decline: Courts, Banking, and Libel Law as Superior Dispute-Resolution Coordination
 *   domain: historical_sociology/legal_history
 *
 * SUMMARY:
 *   Dueling as an honor-preserving dispute-resolution mechanism was gradually
 *   abandoned across the 19th century as alternative institutions matured:
 *   civil courts offered enforceable judgments for debt and slander,
 *   credit-reporting networks and commercial banking gave merchants a
 *   documented alternative to reputational combat, and libel law provided
 *   monetary and public vindication for reputational injury. This reading
 *   treats dueling's decline as institutional substitution — the same
 *   coordination function persisting, served by better-adapted institutions,
 *   rather than a break in the value system that made honor disputes matter.
 *
 * KEY AGENTS:
 *   - litigants_using_civil_courts: primary beneficiary of the substitute protocol
 *   - credit_reliant_merchants: beneficiary of commercial-credit substitution
 *   - libel_plaintiffs: beneficiary of legal-remedy substitution
 *   - professional_class_seeking_reputational_remedy: agenda-setter class that built and staffed the substitute institutions
 *   - dueling_practitioners_holding_out: excluded/marginal population in institutionally thin regions
 *   - historians_of_honor_culture: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__institutional_displacement_reading, 0.18).
domain_priors:suppression_score(dueling_disappearance_mechanism__institutional_displacement_reading, 0.12).
domain_priors:theater_ratio(dueling_disappearance_mechanism__institutional_displacement_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__institutional_displacement_reading, rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__institutional_displacement_reading, "Institutional Substitution Reading of Dueling's Decline: Courts, Banking, and Libel Law as Superior Dispute-Resolution Coordination").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__institutional_displacement_reading, "historical_sociology/legal_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__institutional_displacement_reading, 'b544e226-4af7-42f5-bd73-4129aa100b96').
narrative_ontology:cs_kernel_codification('b544e226-4af7-42f5-bd73-4129aa100b96', distributed).
narrative_ontology:cs_authority_grounding('b544e226-4af7-42f5-bd73-4129aa100b96', practice).
narrative_ontology:cs_interpretation_layer_present('b544e226-4af7-42f5-bd73-4129aa100b96').
narrative_ontology:cs_reading_relation('b544e226-4af7-42f5-bd73-4129aa100b96', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('b544e226-4af7-42f5-bd73-4129aa100b96', dueling_disappearance_mechanism__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('b544e226-4af7-42f5-bd73-4129aa100b96', foundational, protocol_efficiency_drives_norm_selection).
narrative_ontology:cs_axiom_status(protocol_efficiency_drives_norm_selection, holdable).
narrative_ontology:cs_axiom_grounding('b544e226-4af7-42f5-bd73-4129aa100b96', protocol_efficiency_drives_norm_selection, instrumental).
narrative_ontology:cs_axiom('b544e226-4af7-42f5-bd73-4129aa100b96', secondary, dueling_decline_requires_no_value_change).
narrative_ontology:cs_axiom_status(dueling_decline_requires_no_value_change, holdable).
narrative_ontology:cs_axiom_grounding('b544e226-4af7-42f5-bd73-4129aa100b96', dueling_decline_requires_no_value_change, empirically_contingent).
narrative_ontology:cs_reference_frame('b544e226-4af7-42f5-bd73-4129aa100b96', informal_honor_arbitration_baseline).
narrative_ontology:cs_drift_state('b544e226-4af7-42f5-bd73-4129aa100b96', post_bellum_institutional_maturity, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b544e226-4af7-42f5-bd73-4129aa100b96', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, litigants_using_civil_courts).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, credit_reliant_merchants).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, libel_plaintiffs).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, professional_class_seeking_reputational_remedy).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__institutional_displacement_reading, institutional_efficiency_hypothesis).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__institutional_displacement_reading, protocol_competition_drives_norm_change).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gentlemen with grievances over debt, slander, or business disputes increasingly took cases to expanding civil courts rather than issuing challenges. Courts offered documented judgments, enforceable remedies, and no physical risk, at the cost of slower resolution and public airing of financial details.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, litigants_using_civil_courts, beneficiary,
    moderate, biographical, mobile, national).

% As banking and credit-reporting networks matured, a merchant's reputation for solvency and honesty could be settled through credit bureaus and commercial arbitration rather than the field of honor. Formal creditworthiness records substituted for a duel's crude signal of a man's willingness to stand behind his word.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, credit_reliant_merchants, beneficiary,
    moderate, biographical, mobile, national).

% Men whose honor was impugned in print or speech found that suing for libel and defamation produced monetary damages and public vindication through a judgment, achieving the same reputational restoration a duel was meant to secure without exposing either party to death.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, libel_plaintiffs, beneficiary,
    moderate, biographical, mobile, national).

% Lawyers, bankers, editors, and civic reformers built and staffed the institutions (bar associations, credit bureaus, libel statutes) that offered these substitute remedies. They both benefited from the new protocols and actively administered them, expanding court jurisdiction and commercial recordkeeping as viable channels for honor disputes.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, professional_class_seeking_reputational_remedy, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__institutional_displacement_reading, professional_class_seeking_reputational_remedy, agenda_setter).

% A shrinking population of men in regions with weak court infrastructure (parts of the antebellum South, frontier territories) continued to duel because institutional substitutes were locally unavailable or too slow. Their preference for the older protocol was increasingly treated as anachronism rather than a live option, and they had little organized voice in the professional discourse that displaced it.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_practitioners_holding_out, excluded,
    powerless, biographical, constrained, regional).

% Scholars trace the correlation between the density of functioning courts, credit networks, and libel remedies in a given region and the decline of dueling there, treating institutional substitution as the operative causal mechanism distinct from cultural-attitude change or legal prohibition.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, historians_of_honor_culture, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Both dueling and its successors solve the same underlying coordination problem: how to resolve a dispute over honor, debt, or reputation in a way both parties and the community will accept as final and legitimate, without descending into private feud or violence spirals. Institutional substitution reads this as protocol competition — courts, banking records, and libel law offered a lower-cost, lower-risk equilibrium for the same coordination function dueling had served.
% TRANSFER_FUNCTION: The arrangement transfers dispute-resolution business away from informal honor codes and toward formal institutions: filing fees, legal costs, and reputational stakes move from the dueling ground to the courtroom and the ledger. No party is extracted from involuntarily — participants who once would have fought instead pay court costs or accept credit-bureau judgments, a voluntary reallocation of the same underlying transaction.
% ABSENT_VOICES: Practitioners in institutionally thin regions (frontier areas, parts of the antebellum South) had no equivalent voice in the professionalizing discourse that built courts and credit bureaus; their continued reliance on dueling was framed by the professional class as backwardness rather than as a rational response to genuinely absent institutional infrastructure where they lived.
% DISAPPEARANCE_RATIONALE: If civil courts, credit-reporting institutions, and libel law had not developed the capacity to resolve honor and debt disputes, the argument holds that dueling would have persisted far longer as the only functioning mechanism — the displacement account predicts that regions lacking these institutions did in fact retain dueling longest, and that removing the institutions today would not resurrect dueling only because other substitutes (police, digital reputation systems) now fill the same coordination gap.
% FOUNDING_PROBLEM: Pre-institutional societies needed some final, community-legitimated way to settle disputes over honor, debt default, and reputational injury among social equals, in the absence of reliable courts, credit records, or enforceable defamation remedies.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians studying regional variation in dueling's persistence (comparing jurisdictions with early versus late court and credit-bureau development) attest, from outside the professional class that built the substitute institutions, that dueling's decline tracks institutional density rather than simple cultural attitude change — though these same historians note the correlational evidence cannot fully rule out that cultural shift and institutional growth co-occurred rather than one causing the other.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__institutional_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__institutional_displacement_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__institutional_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).
:- end_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18 by 1900) because no party is coerced into using courts, credit bureaus, or libel suits instead of dueling — participants switch because the substitute is cheaper and less risky, a hallmark of genuine coordination improvement rather than extraction. Suppression is low (0.12) for the same reason: dueling was not banned into disuse by this mechanism (that would be a different, prohibition-centered reading), it was out-competed. Theater ratio stays low and roughly flat because the substitute institutions performed real dispute-resolution work throughout, not merely symbolic function. Resistance is low (0.2) because switching to courts/credit/libel was broadly welcomed by the growing professional and commercial classes who benefited from it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (litigants, merchants, libel plaintiffs, the professional class administering the new institutions) sit near the low-extraction end because the substitute protocols demonstrably serve their interests better than dueling did — lower risk, comparable or better reputational/financial remedy. There is no declared victim group: this reading's structural claim is that switching was voluntary utility-maximization by participants, not an imposed cost on any identifiable losing party. Dueling holdouts are marked 'excluded' rather than 'payer' because their disadvantage stems from absent local infrastructure, not from being extracted from by the winning institutions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (settling honor/debt/reputation disputes among equals absent reliable courts) is authored as dead by this reading's own lights: courts, credit bureaus, and libel law now perform that function more efficiently than dueling ever did, so treating dueling's disappearance as institutional obsolescence rather than mandatrophy is the correct read — nothing is being propped up past its function here, the coordination function migrated to better vehicles and the old vehicle simply lost market share. The classification remains rope precisely because this reading finds no active suppression required to keep dueling marginal — it lost the competition rather than being outlawed into extinction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_cultural_causal_priority,
    'Did institutional substitution (courts, banking, libel law) cause dueling''s decline, or did a prior cultural shift toward dignity-culture norms create the demand that made these institutions politically viable and well-funded in the first place?',
    'Fine-grained regional and temporal sequencing: does court/credit-bureau capacity expansion precede or follow measurable decline in dueling incidence in matched regions? The contraction_reading and this reading make different sequencing predictions.',
    'If cultural shift precedes and drives institutional buildout, this reading''s coordination-substitution story becomes downstream of the contraction_reading''s axiomatic account rather than an independent causal mechanism — the two readings would need to be reconciled into an overdetermined_composite framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_cultural_causal_priority, empirical, 'Whether institutional capacity or cultural-value change is the more upstream causal driver.').

omega_variable(
    voluntariness_of_substitution,
    'Was the shift to courts/credit/libel genuinely voluntary utility-maximization by dueling participants, or did emerging institutions actively delegitimize dueling participants (e.g., through legal penalties, social ostracism organized by the professional class) in ways that constitute a form of suppression this reading under-counts?',
    'Review of contemporaneous legal penalties for dueling (anti-dueling statutes, oath requirements for officeholders) co-occurring with the institutional-substitution period, to assess whether ''substitution'' was purely competitive or partly coercive.',
    'If substantial coercive delegitimization is found, extractiveness and suppression should be revised upward and the no-victim-set claim would need reconsideration, potentially shifting elements of this reading toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntariness_of_substitution, conceptual, 'Whether the institutional-displacement account under-states coercive elements bundled with legal/institutional change.').

omega_variable(
    regional_institutional_thinness,
    'In regions where courts, banking, and libel law were slow to develop robust capacity (parts of the antebellum South, frontier territories), does dueling''s persistence there confirm the institutional-substitution mechanism, or is dueling''s regional persistence better explained by a distinct, more durable honor-culture axiom set (the contraction_reading''s domain) that institutional thinness merely failed to challenge?',
    'Comparative case study of regions with similar institutional development timelines but divergent dueling-persistence trajectories, to isolate institutional capacity from cultural-value variables.',
    'Confirms or weakens the specificity of the institutional-displacement mechanism as opposed to being a proxy correlate of the cultural mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regional_institutional_thinness, empirical, 'Whether regional variation in dueling''s persistence isolates the institutional variable or confounds it with cultural variation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__institutional_displacement_reading, 1800, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(duel_tr_t1820, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1820, 0.11).
narrative_ontology:measurement(duel_tr_t1840, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1840, 0.12).
narrative_ontology:measurement(duel_tr_t1860, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1860, 0.13).
narrative_ontology:measurement(duel_tr_t1880, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1880, 0.14).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1900, 0.15).

% Extraction over time
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1800, 0.12).
narrative_ontology:measurement(duel_be_t1820, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1820, 0.14).
narrative_ontology:measurement(duel_be_t1840, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1840, 0.16).
narrative_ontology:measurement(duel_be_t1860, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1860, 0.17).
narrative_ontology:measurement(duel_be_t1880, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1880, 0.18).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1900, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(dueling_disappearance_mechanism__institutional_displacement_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__institutional_displacement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dueling_disappearance_mechanism__institutional_displacement_reading, 0.1).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the dueling_disappearance_mechanism kernel. contraction_reading treats the decline as axiomatic/cultural displacement of honor-culture by dignity-culture norms (a different ε profile centered on identity/value change rather than institutional competition). overdetermined_composite_reading treats all causal factors — including this institutional-substitution mechanism — as jointly sufficient and not cleanly separable, and so authors a composite ε that does not isolate this mechanism's contribution. All three share the same kernel (the historical fact of dueling's decline) but instantiate structurally distinct constraints with different ε, different stakeholder sets (this reading has no victims; the composite reading likely does via legal prohibition), and different classifications are possible depending on the reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
