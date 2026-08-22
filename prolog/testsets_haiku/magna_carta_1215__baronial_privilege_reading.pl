% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__baronial_privilege_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__baronial_privilege_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: magna_carta_1215__baronial_privilege_reading
 *   human_readable: Magna Carta 1215: Baronial Privilege Reading
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   Magna Carta in its 1215 baronial-privilege reading is a feudal contract
 *   between the crown and landowning magnates, not a universal rights
 *   document. The charter formalizes and constrains feudal incidents
 *   (wardship valuations, relief rates, scutage assessments, arbitrary
 *   distraint). 'Free men' in the charter refers to the feudal class — those
 *   holding land directly from the crown by military tenure. The famous
 *   Clause 39 ('no free man shall be arrested except by judgment of his peers
 *   or the law of the land') was understood as a restraint on arbitrary
 *   seizure of baronial estates, not as a guarantee of due process for all
 *   persons. This reading denies that the charter extends protection to
 *   peasants, townspeople outside narrow merchant cartels, women, or Jews.
 *   The charter is CLAIMED as rope (genuine coordination among equals solving
 *   a collective feudal dispute) while the authored metrics reflect modest
 *   extractiveness (the charter does constrain the crown within the baronial
 *   domain) and minimal theater (the constraining function is genuine and
 *   straightforward).
 *
 * KEY AGENTS:
 *   - landowning_barons: The primary beneficiaries and contracting parties; their grievances drove the charter's creation and their power enforces its reconfirmation.
 *   - king_henry_iii_and_successors: Bound by the charter; lose arbitrary wardship and scutage revenue but retain the feudal incidents themselves.
 *   - non_landowning_peasantry: Entirely excluded; the charter makes no provision for peasant rights and leaves them subject to feudal extraction.
 *   - merchant_and_borough_interests: Secured narrow carve-outs (free trade, uniform weights) but remain outside the core baronial compact.
 *   - church_authorities: Beneficiary of Clause 1 (freedom of episcopal election); secondary beneficiary of charter's stabilization of crown power.
 *   - women_and_heirs: Appear as objects of feudal property transfer; no personal autonomy rights.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, 0.38).
domain_priors:suppression_score(magna_carta_1215__baronial_privilege_reading, 0.12).
domain_priors:theater_ratio(magna_carta_1215__baronial_privilege_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__baronial_privilege_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__baronial_privilege_reading, "Magna Carta 1215: Baronial Privilege Reading").
narrative_ontology:topic_domain(magna_carta_1215__baronial_privilege_reading, "constitutional/legal/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__baronial_privilege_reading, '4f64f8d8-c583-4fe7-a631-59891b4ea521').
narrative_ontology:cs_kernel_codification('4f64f8d8-c583-4fe7-a631-59891b4ea521', fixed_text).
narrative_ontology:cs_authority_grounding('4f64f8d8-c583-4fe7-a631-59891b4ea521', lineage).
narrative_ontology:cs_interpretation_layer_present('4f64f8d8-c583-4fe7-a631-59891b4ea521').
narrative_ontology:cs_reading_relation('4f64f8d8-c583-4fe7-a631-59891b4ea521', magna_carta_1215__universal_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('4f64f8d8-c583-4fe7-a631-59891b4ea521', magna_carta_1215__living_document_reading, influences).
narrative_ontology:cs_axiom('4f64f8d8-c583-4fe7-a631-59891b4ea521', foundational, feudal_contractual_scope).
narrative_ontology:cs_axiom_status(feudal_contractual_scope, holdable).
narrative_ontology:cs_axiom_grounding('4f64f8d8-c583-4fe7-a631-59891b4ea521', feudal_contractual_scope, conventional).
narrative_ontology:cs_axiom('4f64f8d8-c583-4fe7-a631-59891b4ea521', secondary, original_meaning_permanence).
narrative_ontology:cs_axiom_status(original_meaning_permanence, holdable).
narrative_ontology:cs_axiom_grounding('4f64f8d8-c583-4fe7-a631-59891b4ea521', original_meaning_permanence, conventional).
narrative_ontology:cs_reference_frame('4f64f8d8-c583-4fe7-a631-59891b4ea521', feudal_contract_1215).
narrative_ontology:cs_drift_state('4f64f8d8-c583-4fe7-a631-59891b4ea521', contemporary_expanded_interpretation, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('4f64f8d8-c583-4fe7-a631-59891b4ea521', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__baronial_privilege_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, landowning_barons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, church_authorities).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, king_henry_iii_and_successors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary contracting parties to the 1215 compact with the crown. They secured written limits on feudal incidents, arbitrary taxation, and arbitrary justice. The protection set applies narrowly to their tenures and holdings: wardship abuse, relief rates, marriage rights, and scutage assessments are constrained by the charter. Exit option is credible (withdrawal from the compact triggers civil war, as 1215 itself proved) and was exercised repeatedly. Benefit accrues only to landholders with sufficient holdings to matter in feudal calculus.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, landowning_barons, beneficiary,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__baronial_privilege_reading, landowning_barons, agenda_setter).

% Bound by the charter to respect the agreed limitations on feudal revenue extraction, arbitrary justice, and military obligation from the baronage. The charter functions as a written constraint on arbitrary royal prerogative within the narrow domain of king-baron relations. The king bears opportunity cost: lost arbitrary wardship profits, constrained scutage rate, mandatory council on major taxes. Compliance enforcement depends on baronial military capacity — a structural constraint, not external coercion.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, king_henry_iii_and_successors, payer,
    institutional, generational, constrained, national).

% Entirely outside the charter's scope of protection. They remain subject to feudal obligations, arbitrary justice, and extraction. The charter mentions villeins once (Clause 20: freedom of will for widows not to remarry) but makes no provision for peasant rights or due process. Had they been in the room, they would have objected that the charter protects only the powerful against each other.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, non_landowning_peasantry, excluded,
    powerless, biographical, trapped, national).

% Merchants and urban burgesses secured narrow protections (Clause 41: merchants to trade freely without arbitrary levy; Clause 35: uniform weights and measures). These are merchant-guild concessions, not rights; they depend on the same ad-hoc royal grace that the baronial clauses formalize. They remain subject to royal will outside the merchant exceptions and have no seat at the bargaining table.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, merchant_and_borough_interests, excluded,
    organized, biographical, constrained, regional).

% Secured one major concession: freedom of election of bishops without royal interference (Clause 1). This is treated separately from the baronial compact and reflects ecclesiastical institutional power, not popular right. The church benefits from the charter's stabilization of crown power relative to the barons (predictable, enforceable law is preferable to arbitrary prerogative from an institutional planning horizon).
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, church_authorities, beneficiary,
    institutional, civilizational, mobile, continental).

% Reads the charter from within its original institutional context: a feudal contract between a lord (the king) and powerful vassals (the barons) about feudal incidents and military obligation. Interprets 'free men' as the feudal class, not a universal category. Sees the charter as genuine coordination (limiting arbitrary revenue and justice within the feudal relationship) without falsely extending its scope.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, historian_reader_baronial_frame, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__baronial_privilege_reading, landowning_barons).
narrative_ontology:fixing_cost_class(magna_carta_1215__baronial_privilege_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine feudal coordination problem: barons and the crown had recurring disputes over wardship valuations, relief rates (the price of entering an inheritance), scutage assessments (the cash payment in lieu of military service), and arbitrary seizure. Writing the terms down in a formal document and swearing them (initially) in open court substituted a negotiated schedule for ad-hoc royal extraction within that relationship.
% TRANSFER_FUNCTION: Moves negotiating power from the crown's unilateral will to a written contract. The barons extract a commitment to respect agreed feudal incident rates, due process before arbitrary distraint, and counsel before major tax levies. In exchange, the crown retains the feudal incidents themselves and the barons' military obligation. The transfer is one of constraining future ambiguity, not of material wealth directly.
% ABSENT_VOICES: Peasants, urban merchants (except in narrow clauses), women, Jews, and unfree populations had no seat and would object that the charter's protections do not extend to them. Merchants and townspeople were present in some charter councils but had far less structural power than barons and secured only narrow carve-outs. Peasants were never consulted and remain entirely outside the protection set.
% DISAPPEARANCE_RATIONALE: Under the baronial privilege reading, if the charter disappeared: the barons would lose a written constraint on feudal incidents and would return to disputing each incident ad-hoc (civil war results). For non-landowning people, disappearance changes nothing — they were not protected by it and remain subject to feudal extraction regardless. The 'world rearranges' verdict applies only to king-baron relations; the peasantry experiences no loss.
% FOUNDING_PROBLEM: Recurring feudal disputes over wardship abuse, relief rates, scutage assessments, and arbitrary distraint. Barons repeatedly complained that the king treated feudal incidents as arbitrary revenue sources rather than customary obligations with understood limits. Civil war in 1215 was the immediate trigger.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary chronicle evidence (Roger of Wendover, Matthew Paris) and surviving administrative records confirm baronial grievances over wardship valuations and arbitrary relief rates. The problem is live insofar as the charter continued to be issued, reissued, and litigated throughout the 13th century — barons extracted repeated reconfirmations from successive kings, indicating the problem persisted and the charter remained valuable.
narrative_ontology:disappearance_verdict(magna_carta_1215__baronial_privilege_reading, contested).
narrative_ontology:founding_problem_status(magna_carta_1215__baronial_privilege_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__baronial_privilege_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_1215__baronial_privilege_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__baronial_privilege_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__baronial_privilege_reading_tests).
:- end_tests(magna_carta_1215__baronial_privilege_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is modest (0.38 at interval end) because the constraint is a genuine negotiated settlement: barons extracted written limits on feudal incidents in exchange for confirming military obligation and feudal subordination. Both sides benefit from clarified terms. Suppression is very low (0.12) because the constraint relies on baronial military capacity to enforce, not coercion; it is stable only when the barons' power permits them to withdraw (civil war). Theater is near-zero (0.05) because there is little performative activity — the charter states clear feudal terms and both parties observe them until the next dispute. Accessibility collapse is low (0.28) because alternatives to the charter (ad-hoc feudal dispute, civil war, or a different written compact) remain structurally available and were exercised repeatedly. Resistance is substantial (0.41) because peasants and townspeople outside the compact would resist a reading that excludes them — opposition that the baronial frame must silence by asserting the original narrow scope. The measurement series shows slow drift: extractiveness and theater rise slightly over 100 years as reinterpretation begins to expand the charter's scope beyond its original feudal domain, but from the baronial reading's vantage point the constraint remains intact and genuinely coordinative.
 *
 * PERSPECTIVAL GAP:
 *   A baron reading this constraint sees genuine coordination achieved through negotiation — a mutual limitation on future disputes with a lord bound by written law. A peasant, if consulted, would see the charter as a compact excluding them, leaving them subject to the same arbitrary extraction. The crown sees a loss of prerogative and revenue but also gains predictability and baronial loyalty. The widening gap between these seats as the charter's scope is later expanded (in the universal-rights and living-document readings) is precisely the contestation the kernel frame tracks. From the baronial seat, the constraint is rope; from the peasant seat, it is piton (theater of protection without benefit) or irrelevant (protection for others, not them).
 *
 * DIRECTIONALITY LOGIC:
 *   The barons sit at the beneficiary end of directionality (d ≈ 0.2): they extracted a written constraint on feudal incidents, their exit option (withdrawal, military resistance) is credible, and they hold institutional power. The crown sits near-symmetric (d ≈ 0.5): it loses arbitrary revenue and justice but retains the feudal relationship and the barons' continued military obligation; exit is genuinely constrained (loss of the barons' support is catastrophic for a feudal monarch). Non-landowners sit at the target end (d ≈ 0.95) from the baronial frame: they are entirely excluded from the protection set and remain subject to arbitrary extraction; their exit is trapped (serfdom binds them to land and lord). The engine's directionality derivation should capture this asymmetry from the declared beneficiary/victim structure and exit options, producing different per-seat classifications: beneficiary seat seeing coordination, payer and excluded seats seeing indifference or extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy by remaining faithful to the charter's 1215 feudal context. The founding problem (feudal disputes over incidents and rates) is live throughout the 13th century — barons extract repeated reconfirmations. The constraint persists because both parties benefit from clarified terms and the barons retain power to enforce. The reading does not claim the charter solved a universal rights problem (which would be false: it did not) and does not claim it applies to all persons (which it did not). The narrow scope is the reading's structural commitment. Mandatrophy emerges in LATER readings (universal-rights, living-document) that claim the charter's original meaning has been superseded by interpretive tradition — that is where the founding problem and the constraint part ways.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    free_men_scope_ambiguity,
    'In 1215, does ''liber homo'' (free man) refer to the feudal class narrowly or to a broader category including merchants, townspeople, and all non-enslaved persons?',
    'Grammatical and contextual analysis of the charter''s uses of the term in relation to contemporary feudal law, charter reissues and confirmations, and close comparison with how the term is used in other 13th-century legal documents (such as the Provisions of Oxford).',
    'If ''free men'' is narrowly feudal (the baronial reading), the charter''s due-process clause applies only to the magnates; if broader, it covers a wider set. This is THE fault line between baronial-privilege and universal-rights readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_men_scope_ambiguity, empirical, 'Ambiguity in the original referent of ''liber homo'' in 1215 context.').

omega_variable(
    interpretive_succession_legitimacy,
    'Does the baronial reading''s original narrow scope constitute the charter''s true meaning permanently, or does legitimate interpretive succession allow reinterpretation in light of changed circumstances and accumulated case law?',
    'This is a conceptual question about the authority grounding of the charter itself: is its meaning fixed by the 1215 bargain, or does it evolve through interpretive tradition (lineage authority)? The committer frame records both as live options (coexists_with relation).',
    'If the baronial reading''s narrow scope is permanent, the universal-rights reading is a false expansion; if interpretive succession is legitimate, the baronial reading is historically true but has been superseded. The cs_structure.drift_state section models this explicitly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_succession_legitimacy, conceptual, 'Whether the 1215 baronial frame can be superseded by legitimate interpretive evolution.').

omega_variable(
    peasant_consent_counterfactual,
    'If peasants had been consulted and had rejected the charter (as an arrangement leaving them unprotected), would the baronial-crown compact still be legitimate?',
    'This is a preference and philosophical question: does a coordination mechanism among part of a population remain legitimate if excluded parties would reject it? No empirical test can settle it; the answer depends on the framework for legitimacy.',
    'If peasant consent is required, the baronial reading''s exclusion makes it illegitimate and extractive (snare) rather than coordinative (rope); if baronial-crown coordination can be legitimate without peasant consent, the reading stands. The committer frame records this as ongoing contestation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(peasant_consent_counterfactual, preference, 'Whether excluded parties'' non-consent undermines the legitimacy of a narrow-scope coordinative arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__baronial_privilege_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc_baronial_tr_t0, magna_carta_1215__baronial_privilege_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(mc_baronial_tr_t10, magna_carta_1215__baronial_privilege_reading, theater_ratio, 10, 0.03).
narrative_ontology:measurement(mc_baronial_tr_t25, magna_carta_1215__baronial_privilege_reading, theater_ratio, 25, 0.04).
narrative_ontology:measurement(mc_baronial_tr_t50, magna_carta_1215__baronial_privilege_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(mc_baronial_tr_t75, magna_carta_1215__baronial_privilege_reading, theater_ratio, 75, 0.05).
narrative_ontology:measurement(mc_baronial_tr_t100, magna_carta_1215__baronial_privilege_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(mc_baronial_be_t0, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(mc_baronial_be_t10, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(mc_baronial_be_t25, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 25, 0.32).
narrative_ontology:measurement(mc_baronial_be_t50, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 50, 0.36).
narrative_ontology:measurement(mc_baronial_be_t75, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 75, 0.37).
narrative_ontology:measurement(mc_baronial_be_t100, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 100, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(magna_carta_1215__baronial_privilege_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__baronial_privilege_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__baronial_privilege_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__universal_rights_reading).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__living_document_reading).

% DUAL FORMULATION NOTE:
% Magna Carta 1215 is a contested kernel with multiple structurally distinct readings. This file instantiates the baronial_privilege_reading (narrow scope, feudal context, 'free men' = magnates only). The universal_rights_reading and living_document_reading are separate constraint stories with different beneficiary/victim sets, different cs_structure.axioms, and different ε values. All three are linked via network.affects_constraints to mark kinship within the kernel family. The ε values differ because the readings assess the SAME standing arrangement (Magna Carta 1215) but from different frames: baronial frame sees genuine coordination (modest ε), universal-rights frame sees cover story for baronial privilege (high ε), living-document frame sees evolving institutional text (intermediate ε, focused on interpretive succession rather than the standing arrangement per se).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
