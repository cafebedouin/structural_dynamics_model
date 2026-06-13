% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__rhetorical_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__rhetorical_scaffold_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: deferential_realism_ontology__rhetorical_scaffold_reading
 *   human_readable: DR Ontology as Normative Rhetorical Scaffold (Rhetorical Reading)
 *   domain: epistemology/normative_theory
 *
 * SUMMARY:
 *   The Deferential Realism constraint typology is presented in the
 *   literature as a measurement apparatus for identifying how constraints
 *   extract from agents—'snare' is supposed to be a classification one
 *   discovers through analyzing extraction and suppression metrics. This
 *   reading contests that framing. Under the rhetorical-scaffold reading, the
 *   typology is not a discovery tool but a policy rhetoric: 'snare' means 'a
 *   mechanism whose beneficiaries I declare to be illegitimate,' and the
 *   typology's value lies in its capacity to reframe institutional
 *   arrangements as exploitative through normative vocabulary. The
 *   claim/metric independence rule is crucial here: the reading claims the
 *   typology IS a scaffold (temporary rhetoric for reframing), while the
 *   metrics describe how it operates as one—low suppression of alternatives
 *   (advocates and defenders both remain convinced they are right), high
 *   theater ratio (much of the activity is rhetorical performance, not
 *   measurement-driven), extractiveness that rises and plateaus (the
 *   constraint's rhetorical power peaks and stabilizes). This reading does
 *   not deny the typology has measurement content; it argues the
 *   classification function is primarily rhetorical.
 *
 * KEY AGENTS:
 *   - policy_advocates_for_illegitimate_extraction_critique — Use the typology to delegitimize mechanisms; benefit from vocabulary that permits normative reframing
 *   - institutional_defenders_of_mechanisms_under_critique — Defend their mechanisms as legitimate coordination; bear the rhetorical cost of being classified as snares
 *   - empirical_analysts_committed_to_measurement_fidelity — Excluded from the scaffold reading's frame; argue the typology colonizes measurement
 *   - beneficiaries_of_contested_mechanisms — Never seated; experience the constraint as rhetorical warfare
 *   - reform_policymakers_seeking_actionable_categories — Benefit from DR typology as policy argument; the scaffold reading amplifies their advocacy voice
 *   - epistemology_of_measurement_preservationists — Observe whether value-ladenness in classification leads to category collapse or creates useful guardrails
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, 0.62).
domain_priors:suppression_score(deferential_realism_ontology__rhetorical_scaffold_reading, 0.28).
domain_priors:theater_ratio(deferential_realism_ontology__rhetorical_scaffold_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__rhetorical_scaffold_reading, scaffold).
narrative_ontology:human_readable(deferential_realism_ontology__rhetorical_scaffold_reading, "DR Ontology as Normative Rhetorical Scaffold (Rhetorical Reading)").
narrative_ontology:topic_domain(deferential_realism_ontology__rhetorical_scaffold_reading, "epistemology/normative_theory").

domain_priors:requires_active_enforcement(deferential_realism_ontology__rhetorical_scaffold_reading).
narrative_ontology:has_sunset_clause(deferential_realism_ontology__rhetorical_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__rhetorical_scaffold_reading, '3d293021-592f-414e-8323-51c4e94cf520').
narrative_ontology:cs_kernel_codification('3d293021-592f-414e-8323-51c4e94cf520', distributed).
narrative_ontology:cs_authority_grounding('3d293021-592f-414e-8323-51c4e94cf520', distributed).
narrative_ontology:cs_reading_relation('3d293021-592f-414e-8323-51c4e94cf520', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d293021-592f-414e-8323-51c4e94cf520', deferential_realism_ontology__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('3d293021-592f-414e-8323-51c4e94cf520', foundational, classification_is_normative_declaration_not_measurement).
narrative_ontology:cs_axiom_status(classification_is_normative_declaration_not_measurement, holdable).
narrative_ontology:cs_axiom_grounding('3d293021-592f-414e-8323-51c4e94cf520', classification_is_normative_declaration_not_measurement, deontological).
narrative_ontology:cs_axiom('3d293021-592f-414e-8323-51c4e94cf520', foundational, beneficiary_legitimacy_determined_through_political_contestation).
narrative_ontology:cs_axiom_status(beneficiary_legitimacy_determined_through_political_contestation, holdable).
narrative_ontology:cs_axiom_grounding('3d293021-592f-414e-8323-51c4e94cf520', beneficiary_legitimacy_determined_through_political_contestation, conventional).
narrative_ontology:cs_reference_frame('3d293021-592f-414e-8323-51c4e94cf520', typology_as_neutral_measurement_apparatus).
narrative_ontology:cs_drift_state('3d293021-592f-414e-8323-51c4e94cf520', contemporary_policy_practice, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3d293021-592f-414e-8323-51c4e94cf520', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, policy_advocates_for_illegitimate_extraction_critique).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, reform_policymakers_seeking_actionable_categories).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, institutional_defenders_of_mechanisms_under_critique).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, empirical_analysts_committed_to_measurement_fidelity).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, illegitimate_beneficiary_concept_is_normatively_determinable).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, classification_is_value_laden_advocacy_act).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses the DR typology as a rhetorical and conceptual tool to delegitimize mechanisms that serve what they declare to be illegitimate beneficiaries. They benefit from having a vocabulary that permits declaring a mechanism a 'snare' based on normative judgment about whether its beneficiaries deserve their rents. Their use depends on the typology remaining open enough to admit contestation over who is 'legitimate'—the typology's persuasive power derives from its capacity to reframe mechanisms as exploitative through normative lens-shifting.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_advocates_for_illegitimate_extraction_critique, agenda_setter,
    organized, biographical, mobile, global).

% Bear the rhetorical cost of being classified as snares or tangled ropes by policy advocates using the DR typology. They argue their mechanisms serve legitimate coordination or are necessary extractions; the scaffold reading makes this argument harder to sustain because the typology explicitly frames classification as normative judgment rather than objective measurement. Their exit is limited—they can attempt to re-establish the immutable-diagnostic reading but cannot simply opt out of the rhetorical contest the scaffold reading instantiates.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, institutional_defenders_of_mechanisms_under_critique, payer,
    powerful, generational, constrained, global).

% Are structurally excluded from the scaffold reading's epistemic frame—their insistence on decoupling measurement from value judgment is treated as naive or complicit. They would argue the typology's value depends on having an objective classification engine that advocates cannot simply redeclare at will. Under the scaffold reading, they are the 'absent voices' defending an epistemically privileged position they believe already belongs to them.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, empirical_analysts_committed_to_measurement_fidelity, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__rhetorical_scaffold_reading, empirical_analysts_committed_to_measurement_fidelity, excluded).

% Are never seated at the table where their legitimacy is judged under the scaffold reading. The reading makes no provision for their voices on whether they are 'legitimate' beneficiaries—it assumes that determination is made through normative-political contestation external to the typology itself. They experience the constraint as rhetorical warfare where the rules of classification are authored in the voices of their critics.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, beneficiaries_of_contested_mechanisms, excluded,
    institutional, generational, trapped, global).

% Benefit from the DR typology because it gives them language and conceptual apparatus to argue for policy change. The scaffold reading particularly benefits them because it explicitly permits them to declare mechanisms 'snares' based on their judgment that the beneficiaries are illegitimate, without needing to resolve empirical disputes about marginal cost or measurement. The typology becomes a tool that amplifies their advocacy voice.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, reform_policymakers_seeking_actionable_categories, beneficiary,
    organized, biographical, mobile, national).

% Track the constraint's operation to understand how normative frameworks colonize measurement vocabularies. They observe whether the scaffold reading's explicit value-ladenness leads to category collapse (all mechanisms can be 'snares' if advocates declare beneficiaries illegitimate) or whether it creates useful guardrails (advocates must still make the normative case, not simply assert it).
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, epistemology_of_measurement_preservationists, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__rhetorical_scaffold_reading, policy_advocates_for_illegitimate_extraction_critique).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__rhetorical_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a normative vocabulary (mountain/rope/snare/tangled_rope/scaffold/piton) that policy advocates can deploy to delegitimize mechanisms whose beneficiaries they judge to be illegitimate. Solves the communicative problem: how to argue that a constraint serves the wrong people without needing to resolve empirical disputes about its function.
% TRANSFER_FUNCTION: Transfers rhetorical authority and persuasive power from empirical measurement frameworks to advocates' normative judgments. Makes it publicly acceptable to declare a mechanism a 'snare' based on one's view of legitimacy rather than one's measurement of extraction.
% ABSENT_VOICES: Empirical analysts who believe measurement should precede judgment; beneficiaries of mechanisms classified as snares, who are never consulted on whether they are 'legitimate'; and institutional defenders who argue the typology should remain epistemically neutral. They would object that the scaffold reading instrumentalizes a measurement vocabulary for rhetorical purposes and that this colonization makes honest measurement impossible.
% DISAPPEARANCE_RATIONALE: If the DR typology as a rhetorical scaffold vanished, policy advocates would lose a conceptual tool that permits them to reframe mechanisms as exploitative without measuring them; institutional defenders would regain argumentative space to claim their mechanisms are natural laws or genuine coordinations; the field would reorganize around either immutable-diagnostic rigor (measurement-first) or hybrid-pragmatic compromise (acknowledging both measurement and normative contestation in classification). The scaffold reading's disappearance would mean the loss of a specific form of rhetorical authority.
% FOUNDING_PROBLEM: Policy critique needs language to identify and challenge mechanisms that serve what the critic judges to be illegitimate beneficiaries, but traditional measurement-based frameworks treat 'legitimacy' as outside their scope, leaving advocates with no conceptual apparatus that carries institutional weight. The DR typology was built with fixed referents (mountains, coordination problems) but advocates need flexibility to declare beneficiaries illegitimate—the scaffold reading resolves this by making that flexibility explicit and intentional.
% FOUNDING_PROBLEM_CORROBORATION: Policy advocates and reform organizations attest the founding problem is live—they constantly need to argue against institutional defenses that 'our beneficiaries are natural/necessary/deserved.' Institutional defenders attest the founding problem is manufactured—advocates simply want rhetorical license to delegitimize without measuring. Epistemologists attest the founding problem reflects a genuine tension between measurement and value but dispute whether explicit value-ladenness in classification is a solution or a capitulation. No corroboration from outside the advocacy and institutional defense communities.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__rhetorical_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__rhetorical_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(deferential_realism_ontology__rhetorical_scaffold_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness plateaus at 0.62 because the constraint's rhetorical power derives from advocates' capacity to redeclare beneficiaries as illegitimate—there is a ceiling to how many mechanisms can be credibly called snares before institutional defenders mount effective counter-rhetoric. Theater ratio rises from 0.35 to 0.71 because the constraint's operation is increasingly dominated by rhetorical performance (advocates arguing 'this mechanism serves illegitimate beneficiaries,' defenders arguing 'our beneficiaries are essential/earned,' measurement playing a secondary role). Suppression falls from 0.42 to 0.28 because the scaffold reading explicitly permits alternative framings—it does NOT suppress the immutable-diagnostic reading or the hybrid-pragmatic reading, it coexists with them in contested space. The constraint is a SCAFFOLD because its founding problem (policy needs rhetorical language) is time-limited—once institutions internalize the critique or once advocates establish new institutional legitimacy criteria, the typology's value as a rhetorical tool decays.
 *
 * PERSPECTIVAL GAP:
 *   From the advocate's seat, the typology is precisely the tool they need: it permits them to declare beneficiaries illegitimate without settling empirical disputes. From the institutional defender's seat, the typology is category weaponization—measurement language deployed for rhetorical effect, not epistemic fidelity. From the empirical analyst's seat, both are gaming a measurement apparatus. From the reform policymaker's seat, the typology is a lever for change. The engine computes these divergences from the structural data; the reading does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Policy advocates and reform policymakers benefit from the constraint (d near 0.0—they gain rhetorical authority without needing to measure); institutional defenders and beneficiaries of contested mechanisms are targets (d near 1.0—they bear the cost of being rhetorically delegitimized); empirical analysts are identity-locked in opposing the constraint because it colonizes their epistemic domain; epistemologists are analytical observers tracking whether the constraint produces useful policy outcomes or category collapse.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold reading avoids mandatrophy by acknowledging its own time-limitedness. It does not claim the typology is a permanent measurement device; it claims it is a temporary rhetoric for reframing. When the founding problem (policy needs language to delegitimize) is resolved (through new institutional legitimacy criteria, or through advocates gaining sufficient institutional power), the scaffold can be dismantled. This is distinct from a mountain-that-is-secretly-a-snare (a false summit with hidden beneficiaries) or a snare-masquerading-as-coordination—the scaffold reading is honest about its rhetorical function and its time horizon.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_standard_underspecification,
    'What counts as an ''illegitimate beneficiary''? The scaffold reading assumes advocates can declare beneficiaries illegitimate, but the typology never codifies what makes a beneficiary illegitimate.',
    'Examination of how advocates actually use the typology: do they converge on shared criteria for illegitimacy, or does each advocate deploy their own? If convergence emerges, legitimacy is moving toward shared standard; if fragmentation, the constraint risks collapsing into pure rhetoric with no referent.',
    'If legitimacy becomes codified as a shared standard, the constraint shifts from pure rhetoric toward hybrid-pragmatic territory (legitimacy is a measurement, not just assertion). If legitimacy remains contested, the constraint remains maximally rhetorical and risks losing persuasive power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_standard_underspecification, conceptual, 'Whether the scaffold reading''s core move—declaring beneficiaries illegitimate—can be standardized or remains purely assertive.').

omega_variable(
    measurement_colonization_risk,
    'Does the explicit value-ladenness of the scaffold reading prevent measurement of actual extraction, or does it clarify measurement by naming the normative commitments that were always implicit?',
    'Comparative analysis of constraint classification under the three readings: do measurement-first (diagnostic) and normative-first (scaffold) approaches produce converging or diverging classifications? Can one predict the other''s verdicts?',
    'If convergence is high, the scaffold reading is making implicit commitments explicit without changing the actual classification work—it is clarifying. If convergence is low, the scaffold reading has colonized a measurement vocabulary for rhetorical purposes and measurement becomes impossible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_colonization_risk, empirical, 'Whether the scaffold reading clarifies or colonizes the measurement apparatus.').

omega_variable(
    temporal_sustainability_of_rhetorical_scaffolding,
    'Can a rhetorical scaffold persist beyond its founding problem? Once advocates have sufficient rhetorical authority to declare beneficiaries illegitimate, does the typology remain useful or become vestigial?',
    'Historical observation: does the typology''s persuasive power decay as advocates achieve reforms? Does it get replaced by more specific legitimacy frameworks? Or does it remain as a permanent vocabulary for contestation?',
    'If the typology''s usefulness is genuinely time-limited, the scaffold framing is correct and the constraint should include a sunset clause (authorized by has_sunset_clause: true). If the typology remains useful indefinitely, it is not a temporary scaffold but a permanent rhetorical framework—reclassify as rope or tangled_rope depending on whether it coordinates or extracts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(temporal_sustainability_of_rhetorical_scaffolding, empirical, 'Whether the constraint''s founding problem has a finite scope or indefinite recurrence.').

omega_variable(
    foreclosure_vs_coexistence_with_diagnostic_reading,
    'Does the scaffold reading''s claim that classification is primarily normative FORECLOSE the diagnostic reading''s claim that classification is primarily measurement-based, or do they coexist as competing framings?',
    'Institutional practice: do proponents of the scaffold reading actively suppress or refuse the diagnostic reading, or do both readings persist in the literature and policy discourse?',
    'If the readings foreclose each other (only one can be true), they cannot coexist in the same theoretical commitment. If they coexist, the reading-relation is COEXISTS_WITH, not FORECLOSES. Classification of the reading-relation is diagnostic of whether the framework permits dual-reading interpretation or demands commitment to one reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foreclosure_vs_coexistence_with_diagnostic_reading, empirical, 'The logical and institutional relationship between the scaffold and diagnostic readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__rhetorical_scaffold_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(defe_tr_t0, observed).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 5, 0.44).
narrative_ontology:measurement_basis(defe_tr_t5, observed).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 10, 0.56).
narrative_ontology:measurement_basis(defe_tr_t10, observed).
narrative_ontology:measurement(defe_tr_t15, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 15, 0.68).
narrative_ontology:measurement_basis(defe_tr_t15, observed).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 20, 0.71).
narrative_ontology:measurement_basis(defe_tr_t20, observed).
narrative_ontology:measurement(defe_tr_t25, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 25, 0.71).
narrative_ontology:measurement_basis(defe_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(defe_be_t0, observed).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(defe_be_t5, observed).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(defe_be_t10, observed).
narrative_ontology:measurement(defe_be_t15, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(defe_be_t15, observed).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(defe_be_t20, observed).
narrative_ontology:measurement(defe_be_t25, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(defe_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(defe_su_t0, observed).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement_basis(defe_su_t5, observed).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement_basis(defe_su_t10, observed).
narrative_ontology:measurement(defe_su_t15, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 15, 0.28).
narrative_ontology:measurement_basis(defe_su_t15, observed).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement_basis(defe_su_t20, observed).
narrative_ontology:measurement(defe_su_t25, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 25, 0.28).
narrative_ontology:measurement_basis(defe_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__rhetorical_scaffold_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__rhetorical_scaffold_reading, 0.12).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'deferential_realism_ontology'. The immutable-diagnostic reading frames the typology as a fixed measurement apparatus with objective referents. The hybrid-pragmatic reading frames it as having a fixed core (mountains/coordination facts) and contested periphery (where normative judgment matters). The rhetorical-scaffold reading (this constraint) frames it as normative rhetoric for reframing mechanisms through legitimacy judgments. Each reading has different ε values, different suppression profiles, and different beneficiary/victim structures. They are not alternatives in observation—they are different uses of the same text, and each use constitutes a different constraint. The network links show influence rather than competition: the diagnostic reading grounds the conversation in measurement; the hybrid reading acknowledges measurement and normative contestation; the scaffold reading makes the normative commitment explicit and weaponizes it for policy advocacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deferential_realism_ontology__rhetorical_scaffold_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
