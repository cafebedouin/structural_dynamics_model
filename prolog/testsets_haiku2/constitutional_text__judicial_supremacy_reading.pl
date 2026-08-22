% ============================================================================
% CONSTRAINT STORY: constitutional_text__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_text__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Over Constitutional Meaning
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   This constraint instantiates the judicial-supremacy reading of a
 *   contested constitutional kernel: the question of who holds final
 *   interpretive authority over constitutional text. Under this reading,
 *   courts possess exclusive power to determine what the constitution means,
 *   and their interpretation is binding and unrevisable through ordinary
 *   political process. This is ONE reading of the constitutional text;
 *   sibling readings (legislative sovereignty, popular sovereignty)
 *   distribute interpretive authority differently. The constraint is CLAIMED
 *   as tangled_rope because it coordinates the constitutional interpretation
 *   problem (solves ambiguity about meaning) while extracting from
 *   legislatures and electoral majorities (removes their policy-setting
 *   authority). The claim and the metrics are authored independently: the
 *   metrics describe substantially extractive operation with rising
 *   extraction over time as courts expand interpretive reach; the claim
 *   reflects the constraint's own legitimating narrative. The engine measures
 *   their divergence.
 *
 * KEY AGENTS:
 *   - institutional_courts: The agenda-setter; holds and actively defends interpretive authority over constitutional meaning. Courts justify the role as protecting rights against majorities.
 *   - rights_claimants: Powerless beneficiaries; gain a veto over majoritarian policies through court review. Their exit from the constraint is costly (lobbying legislatures, amendment advocacy).
 *   - legislative_bodies: Institutional payers; lose policy-setting authority to courts. Their exit option is constitutional amendment, a supermajority barrier.
 *   - electoral_majorities: Organized payers; cannot implement policies overruled by courts without amendment. Their accountability loop is broken when courts override electoral will.
 *   - non_right_holding_minorities: Structurally excluded; would challenge the beneficiary set but lack standing in the courts' own framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, 0.68).
domain_priors:suppression_score(constitutional_text__judicial_supremacy_reading, 0.72).
domain_priors:theater_ratio(constitutional_text__judicial_supremacy_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__judicial_supremacy_reading, "Judicial Supremacy Over Constitutional Meaning").
narrative_ontology:topic_domain(constitutional_text__judicial_supremacy_reading, "political/constitutional").

domain_priors:requires_active_enforcement(constitutional_text__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__judicial_supremacy_reading, 'cced714b-2b51-40df-8461-d0bf6945b03e').
narrative_ontology:cs_kernel_codification('cced714b-2b51-40df-8461-d0bf6945b03e', fixed_text).
narrative_ontology:cs_authority_grounding('cced714b-2b51-40df-8461-d0bf6945b03e', lineage).
narrative_ontology:cs_interpretation_layer_present('cced714b-2b51-40df-8461-d0bf6945b03e').
narrative_ontology:cs_reading_relation('cced714b-2b51-40df-8461-d0bf6945b03e', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('cced714b-2b51-40df-8461-d0bf6945b03e', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('cced714b-2b51-40df-8461-d0bf6945b03e', foundational, courts_hold_final_interpretive_authority).
narrative_ontology:cs_axiom_status(courts_hold_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('cced714b-2b51-40df-8461-d0bf6945b03e', courts_hold_final_interpretive_authority, conventional).
narrative_ontology:cs_axiom('cced714b-2b51-40df-8461-d0bf6945b03e', foundational, constitutional_meaning_is_judicially_determined).
narrative_ontology:cs_axiom_status(constitutional_meaning_is_judicially_determined, holdable).
narrative_ontology:cs_axiom_grounding('cced714b-2b51-40df-8461-d0bf6945b03e', constitutional_meaning_is_judicially_determined, conventional).
narrative_ontology:cs_axiom('cced714b-2b51-40df-8461-d0bf6945b03e', secondary, legislative_override_of_court_interpretation_is_unconstitutional).
narrative_ontology:cs_axiom_status(legislative_override_of_court_interpretation_is_unconstitutional, holdable).
narrative_ontology:cs_axiom_grounding('cced714b-2b51-40df-8461-d0bf6945b03e', legislative_override_of_court_interpretation_is_unconstitutional, conventional).
narrative_ontology:cs_reference_frame('cced714b-2b51-40df-8461-d0bf6945b03e', judicial_final_authority_framework).
narrative_ontology:cs_drift_state('cced714b-2b51-40df-8461-d0bf6945b03e', contemporary_rights_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cced714b-2b51-40df-8461-d0bf6945b03e', '').
narrative_ontology:cs_kernel_id(constitutional_text__judicial_supremacy_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, rights_claimants).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, institutional_courts).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, legislative_bodies).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, electoral_majorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts hold exclusive interpretive authority over constitutional text. When a legislature passes a law, courts can declare it unconstitutional, and this declaration is treated as final and binding. The court's reading becomes the constitutional meaning for all purposes. Courts justify this role as protecting individual rights against majoritarian overreach and maintaining rule of law through consistent interpretation. They bear no direct cost for this authority; they collect legitimacy from the role.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, institutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Citizens with constitutional claims (minority rights, due process objections, speech challenges) can petition courts for relief. They benefit from the judicial veto: a law restricting their conduct or infringing their claimed rights can be struck down by courts, even if the legislature enacted it with supermajority support and popular approval. Their alternative—lobbying legislatures directly—is less reliable when majoritarian pressure runs high.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, rights_claimants, beneficiary,
    powerless, biographical, constrained, national).

% Legislatures enact laws only to find them invalidated by courts. Even when the legislature believes it has correctly interpreted the constitution (and acts democratically, with electoral accountability), the court's contrary interpretation overwrites the legislature's judgment. In judicial-supremacy regimes, legislatures cannot override a court's constitutional reading through ordinary legislation—they must amend the constitution itself, a costlier process. Legislatures bear the cost of lost policy space and reduced accountability to voters.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legislative_bodies, payer,
    institutional, generational, constrained, national).

% Voters elect legislatures to enact policies reflecting their preferences. Courts can overturn those policies on constitutional grounds, even when voters supported them. Voters cannot easily recall or overturn court decisions without amendment (a supermajority bar that often exceeds the electoral mandate). Their exit option is amendment advocacy, a high-friction alternative.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, electoral_majorities, payer,
    organized, biographical, constrained, national).

% Groups lacking recognized constitutional claims (undocumented immigrants, non-citizens, future generations, non-human entities) cannot petition courts for constitutional protection. They are excluded from the rights-claimant beneficiary set even when courts' decisions affect them structurally. Their voice is absent from the constitutional conversation the constraint channels.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, non_right_holding_minorities, excluded,
    powerless, biographical, trapped, national).

% Legislatures in other constitutional regimes (with parliamentary sovereignty or popular-sovereignty readings) analyze this constraint as a case study. They observe how it concentrates interpretive authority and creates rigidity, and they make institutional design choices accordingly.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, comparative_legislatures, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__judicial_supremacy_reading, institutional_courts).
narrative_ontology:fixing_cost_class(constitutional_text__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, authoritative interpreter of constitutional meaning: instead of legislatures and courts reading the text independently and potentially conflicting, courts have final say. This solves the coordination problem of constitutional ambiguity—where does interpretive authority lodge?—by vesting it in one institutional seat. It also coordinates citizen expectations: the constitutional meaning is what courts say it is, not contested between branches.
% TRANSFER_FUNCTION: Transfers from legislatures and electoral majorities a share of their policy-setting authority to courts. Legislatures lose the ability to enact laws that courts deem unconstitutional (a veto transfer). Electoral majorities lose the ability to implement policies they support if courts block them (a legitimacy transfer). Courts gain interpretive authority and institutional prestige. Rights-claimants gain a veto over majoritarian policies that infringe their claims.
% ABSENT_VOICES: Groups without recognized constitutional claims (undocumented immigrants, future generations, non-citizens, institutionalized persons) are structurally excluded from the rights-claimant beneficiary set and cannot petition for judicial protection. They would argue that constitutional meaning should be accountable to them as well, but they have no seat at the table. Legislative bodies in non-judicial-supremacy regimes would argue that elected representatives are the proper locus of constitutional interpretation, but they are not present to make that case inside this constraint story.
% DISAPPEARANCE_RATIONALE: If judicial supremacy over constitutional meaning vanished overnight—if courts lost their power to invalidate legislation on constitutional grounds—legislatures would become de facto constitutional interpreters. Rights-claimants would shift their advocacy from courts to legislatures and popular amendment. Constitutional meaning would become contestable and revisable through ordinary legislative process. The separation of powers would reorganize; democratic accountability would shift upward to voters and elected bodies.
% FOUNDING_PROBLEM: In early constitutional democracies, ambiguous constitutional text left unclear who could authoritatively settle meaning disputes: legislatures? courts? the people through amendment? Without a clear answer, constitutional clauses could be read differently by different branches, producing jurisdictional conflicts and legal instability. Judicial supremacy solved this by designating courts as the authoritative voice.
% FOUNDING_PROBLEM_CORROBORATION: Judicial-supremacy advocates argue the founding problem is live: without courts as final interpreters, majorities could read the Constitution to eliminate minority protections, producing tyranny of the majority. Legislative-sovereignty advocates argue the founding problem is solved: modern legislatures are accountable institutions with internal checks and review procedures; courts' intervention creates a different instability (judicial override of electoral will). Comparative constitutional scholars document that many stable democracies operate without judicial supremacy, suggesting the founding problem can be managed through alternative institutional designs.
narrative_ontology:disappearance_verdict(constitutional_text__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__judicial_supremacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.68 over the interval, reflecting the historical trajectory of judicial authority expansion. Early in a regime's adoption of judicial supremacy, courts exercise restraint (lower extraction), respecting legislative domain. Over time, courts expand constitutional interpretation into areas legislatures thought secure (commerce, welfare, procedural rules), increasing the range of legislation subject to judicial veto. Theater rises from 0.20 to 0.38, reflecting growing performance: as courts invalidate more laws, they must justify each decision through elaborate doctrinal narratives that often obscure the redistributive fact that courts are vetoing legislatures. Suppression is high and stable (~0.72) because the constraint's persistence depends on actively preventing legislative override—legislatures cannot reverse court decisions through ordinary means (they must amend the constitution), which is the suppression mechanism. The measurements share one time grid: every metric is authored at every sampled point (0, 8, 16, 25, 35, 50) so the engine can synchronize temporal analysis.
 *
 * PERSPECTIVAL GAP:
 *   Courts and rights-claimants experience this constraint as coordination and protection; legislatures and electoral majorities experience it as extraction and constraint on democratic will. The engine computes per-seat classifications from the structural data: courts will likely classify the constraint as rope or mountain (coordination, no cost to them); legislative seats will classify it as snare (extraction, suppression via constitutional amendment bar); rights-claimant seats will classify it as rope (coordination, benefit from veto). This divergence is not an error—it is exactly what the apparatus measures. The structural fact is that courts set the terms of the game, rights-claimants benefit from the terms, and legislatures pay for the terms. Different seats perceive different constraint types because they occupy different positions in the distribution.
 *
 * DIRECTIONALITY LOGIC:
 *   Courts derive d ≈ 0.0 (full beneficiary): they hold interpretive authority and collect legitimacy from the role; they bear no direct cost. Rights-claimants derive d ≈ 0.3 (beneficiary-leaning): they collect a veto over majoritarian policies; their exit options are constrained but exist (amendment advocacy, legislative lobbying). Legislatures derive d ≈ 0.8 (target-leaning): they lose policy-setting authority; their exit is the amendment bar (trapped or constrained exit). Electoral majorities derive d ≈ 0.75 (target-leaning): they lose the ability to implement electoral mandates; exit is amendment (constrained to trapped). The directionality reflects the structural flow: who controls the constraint (courts, d low), who benefits without running it (rights-claimants, d moderate), who pays and cannot easily exit (legislatures and majorities, d high). No overrides are needed; the derivation from beneficiary/victim + exit already captures the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—constitutional ambiguity and jurisdictional conflict—is CONTESTED in status. Judicial-supremacy advocates argue it remains live: without courts as final arbiters, majorities could reread the constitution to justify tyranny. Legislative-sovereignty advocates argue it is solved: democracies can manage constitutional meaning through accountable legislative process (parliament with supermajority or supermajority + referendum). The disappearance verdict is world_rearranges: if courts lost interpretive supremacy, policy space would reorganize and democratic accountability would shift. The mismatch (founding_problem_status=contested + disappearance_verdict=world_rearranges) flags a mandatrophy-adjacent pattern: the constraint persists despite contestation over whether it solves the problem it claims to solve. If the founding problem were truly dead (solved by alternative institutions), we would expect either (a) disappearance_verdict=world_unchanged (constraint is decorative) or (b) a sharp decline in theater_ratio (the constraint becomes purely functional). Neither is true: theater rises, suggesting the constraint is performing a legitimating narrative (we need courts to prevent tyranny) that is contestable but effective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'Is the core function of judicial supremacy coordinating constitutional meaning (solving ambiguity), or is it extracting authority from legislatures and majorities (redistributing power)?',
    'Comparative institutional analysis: do regimes without judicial supremacy (e.g., parliamentary sovereignty, popular sovereignty) successfully coordinate constitutional meaning through alternative mechanisms? If yes, the ''coordination problem'' may be a post-hoc justification for a power transfer; if no, coordination may be the genuine function.',
    'If extraction is primary, the constraint reclassifies from tangled_rope (coordination + extraction) to snare (extraction with coordination cover). If coordination is primary, it remains tangled_rope. This is the crux of the mandatrophy question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'Whether judicial supremacy''s core function is solving genuine coordination problems or redistributing interpretive authority from legislatures to courts.').

omega_variable(
    founding_problem_liveness,
    'Does the founding problem (constitutional ambiguity leading to inter-branch conflict) remain a live threat in regimes with developed interpretive traditions, or has the problem been substantially solved by institutional norms and precedent?',
    'Empirical track: measure the frequency and severity of inter-branch constitutional conflicts in the regime over time. Compare to regimes using alternative interpretive authorities. If conflicts decline and stable constitutional meaning emerges despite alternative authority structures, the founding problem may be solved.',
    'If the founding problem is solved, the constraint is in early-stage mandatrophy: the authority claim persists but the grounding weakens. The theater_ratio rise supports this hypothesis. Reclassification to piton becomes possible if courts are primarily performing judicial authority rather than enforcing a live coordination need.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_liveness, empirical, 'Whether constitutional ambiguity and inter-branch conflict remain a persistent problem or have been substantially managed by institutional development.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the judicial-supremacy reading logically foreclose the legislative-sovereignty reading, or do they coexist as incompatible but live positions held by different parties?',
    'Doctrinal analysis: can a legal framework coherently hold both that courts have final interpretive authority AND legislatures can override court interpretations via statute? If the frameworks genuinely contradict (each asserts the other''s position is false), foreclosure holds. If they can be held by different parties without internal contradiction within each party''s framework, coexistence holds.',
    'If foreclosure is true, it signals that one reading must eventually be dominant in any integrated legal system—one side will lose. If coexistence is true, the contest is ongoing and both readings remain live. Coexistence supports the constraint-family framing; foreclosure supports a more binary competition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'The logical relationship between judicial-supremacy and legislative-sovereignty readings of constitutional authority.').

omega_variable(
    suppression_mechanism_source,
    'Is the measured suppression (0.72) structural (the constitutional amendment bar makes override impossible, so legislatures are forced to accept) or performative (courts maintain authority through legitimacy narratives and institutional prestige, and legislatures could theoretically override but choose not to)?',
    'Historical case study: examine cases where legislatures attempted constitutional amendment to override court decisions. What barriers did they face? How many succeeded? Did success depend on structural rules (amendment requirement) or on political consensus (legislatures accepted the court''s reading)? Post-suppression trajectory: if courts lost legitimacy, would legislative override become possible despite unchanged amendment procedures?',
    'If suppression is structural, the constraint is rigid and change requires formal amendment (hard exit). If suppression is performative, legitimacy is the binding mechanism and could shift if courts lose institutional prestige (softer exit). This affects the classification: structural suppression points to snare; performative suppression points to piton or weakening tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_source, empirical, 'Whether suppression of legislative override is enforced by legal rules or maintained by institutional legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__judicial_supremacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__judicial_supremacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cons_tr_t8, constitutional_text__judicial_supremacy_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(cons_tr_t16, constitutional_text__judicial_supremacy_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(cons_tr_t25, constitutional_text__judicial_supremacy_reading, theater_ratio, 25, 0.36).
narrative_ontology:measurement(cons_tr_t35, constitutional_text__judicial_supremacy_reading, theater_ratio, 35, 0.37).
narrative_ontology:measurement(cons_tr_t50, constitutional_text__judicial_supremacy_reading, theater_ratio, 50, 0.38).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__judicial_supremacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cons_be_t8, constitutional_text__judicial_supremacy_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(cons_be_t16, constitutional_text__judicial_supremacy_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(cons_be_t25, constitutional_text__judicial_supremacy_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(cons_be_t35, constitutional_text__judicial_supremacy_reading, base_extractiveness, 35, 0.67).
narrative_ontology:measurement(cons_be_t50, constitutional_text__judicial_supremacy_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__judicial_supremacy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(cons_su_t8, constitutional_text__judicial_supremacy_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(cons_su_t16, constitutional_text__judicial_supremacy_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(cons_su_t25, constitutional_text__judicial_supremacy_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(cons_su_t35, constitutional_text__judicial_supremacy_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement(cons_su_t50, constitutional_text__judicial_supremacy_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text__judicial_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__legislative_sovereignty_reading).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the constitutional-text kernel alongside legislative_sovereignty_reading and popular_sovereignty_reading. All three instantiate different distributions of interpretive authority over the same constitutional text. They are not alternative measurements of one constraint; they are structurally distinct constraints arising from different framings of the same contested kernel. The judicial-supremacy reading distributes authority to courts (courts hold final say); the legislative-sovereignty reading distributes it to legislatures (legislatures have final say, courts advise); the popular-sovereignty reading distributes it to the demos (the people retain ultimate authority through amendment or convention). Each reading produces different beneficiary/victim sets, different directionalities, and different classifications at different seats. Link all three via network.affects_constraints to model the constitutional contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
