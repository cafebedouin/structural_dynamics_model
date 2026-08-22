% ============================================================================
% CONSTRAINT STORY: us_constitution_text__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__positivist_reading, []).

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
 *   constraint_id: us_constitution_text__positivist_reading
 *   human_readable: Positivist Constitutional Validity via Formal Procedure
 *   domain: constitutional_law/legal_philosophy
 *
 * SUMMARY:
 *   The positivist reading of the Constitution holds that constitutional
 *   validity derives from formal enactment through Article V amendment
 *   procedure, not from moral content, historical intent, or contemporary
 *   meaning. Under this reading, judges are bound to recognize only what the
 *   text explicitly enumerates or what has been formally amended. Claims
 *   grounded in evolved social values, interpretive consensus, or
 *   aspirational meaning are extra-constitutional unless ratified through
 *   amendment—a supermajority-enforcing procedure designed for stability, not
 *   responsiveness. This constraint coordinates institutional stability and
 *   rule-of-law predictability while extracting effective voice from
 *   substantive-justice claimants (especially marginalized groups seeking
 *   unenumerated rights). The measurement trajectory shows extraction rising
 *   in early decades as the constraint became institutionally dominant (t=0
 *   to t=20), then plateauing as it achieved near-total acceptance in legal
 *   practice. Theater ratio rises as the constraint increasingly frames
 *   itself as procedural neutrality while selectively locking out certain
 *   categories of claims.
 *
 * KEY AGENTS:
 *   - Supreme Court: enforces the positivist frame, confines interpretation to formal procedures, collects institutional legitimacy from rule-of-law framing
 *   - Institutional stability advocates: benefit from predictability and resistance to doctrinal drift
 *   - Substantive justice claimants: powerless victims forced to seek Article V amendment for recognition
 *   - Marginalized groups seeking unenumerated rights: identity-locked victims bearing the cost of formal procedure
 *   - Originalist reading proponents: excluded—hold a related but distinct reading
 *   - Living constitutionalist proponents: excluded—directly opposed, their core premise is rejected by positivism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__positivist_reading, 0.68).
domain_priors:suppression_score(us_constitution_text__positivist_reading, 0.72).
domain_priors:theater_ratio(us_constitution_text__positivist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__positivist_reading, "Positivist Constitutional Validity via Formal Procedure").
narrative_ontology:topic_domain(us_constitution_text__positivist_reading, "constitutional_law/legal_philosophy").

domain_priors:requires_active_enforcement(us_constitution_text__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__positivist_reading, '8dd272a5-0838-4305-a0e8-287f5b7635c9').
narrative_ontology:cs_kernel_codification('8dd272a5-0838-4305-a0e8-287f5b7635c9', fixed_text).
narrative_ontology:cs_authority_grounding('8dd272a5-0838-4305-a0e8-287f5b7635c9', extraction).
narrative_ontology:cs_reading_relation('8dd272a5-0838-4305-a0e8-287f5b7635c9', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8dd272a5-0838-4305-a0e8-287f5b7635c9', us_constitution_text__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('8dd272a5-0838-4305-a0e8-287f5b7635c9', foundational, procedure_alone_determines_validity).
narrative_ontology:cs_axiom_status(procedure_alone_determines_validity, holdable).
narrative_ontology:cs_axiom_grounding('8dd272a5-0838-4305-a0e8-287f5b7635c9', procedure_alone_determines_validity, empirically_contingent).
narrative_ontology:cs_axiom('8dd272a5-0838-4305-a0e8-287f5b7635c9', foundational, formal_amendment_required_for_constitutional_change).
narrative_ontology:cs_axiom_status(formal_amendment_required_for_constitutional_change, holdable).
narrative_ontology:cs_axiom_grounding('8dd272a5-0838-4305-a0e8-287f5b7635c9', formal_amendment_required_for_constitutional_change, deontological).
narrative_ontology:cs_reference_frame('8dd272a5-0838-4305-a0e8-287f5b7635c9', formal_enactment_supremacy).
narrative_ontology:cs_drift_state('8dd272a5-0838-4305-a0e8-287f5b7635c9', contemporary_human_rights_discourse, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8dd272a5-0838-4305-a0e8-287f5b7635c9', '').
narrative_ontology:cs_kernel_id(us_constitution_text__positivist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, institutional_hierarchy).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, predictability_oriented_actors).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, substantive_justice_claimants).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, marginalized_groups_seeking_unenumerated_rights).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, federal_legislative_body).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, institutional_stability_advocates).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, federal_legislative_body).
narrative_ontology:constraint_vindicates(us_constitution_text__positivist_reading, rule_of_law_formal_legality).
narrative_ontology:constraint_vindicates(us_constitution_text__positivist_reading, separation_of_powers_judicial_restraint).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces the positivist frame by confining constitutional interpretation to textual procedures: recognizes only amendments ratified through Article V, rejects claims grounded in evolved moral consensus or aspirational meaning. Justifies restraint as fidelity to law; exercises discretion in what counts as 'plain meaning' and procedural validity. Collects institutional legitimacy (rule of law) and insulation from democratic revision when decisions are framed as procedural inevitability.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% Possesses formal enactment power (Article V amending authority). Benefits from the positivist frame's insistence that constitutional meaning changes only through formal amendment—this anchors legislative authority in procedural supremacy and prevents judges from reinterpreting enumerated powers. Also bears the cost: substantive claims it might want to recognize (e.g., unenumerated rights) are locked out unless it pursues formal amendment, a high-friction process designed for stability, not responsiveness.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, federal_legislative_body, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__positivist_reading, federal_legislative_body, payer).

% Includes conservative jurists, originalist scholars, institutional-design advocates who value predictability, settled expectations, and resistance to doctrinal drift. Benefits from the positivist frame because it prevents courts from rewriting constitutional meaning under the guise of interpretation; every change must be formal and public. Gains legitimacy through association with rule-of-law values.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, institutional_stability_advocates, beneficiary,
    organized, generational, mobile, national).

% Seek constitutional recognition of rights not enumerated in the 1787 text: reproductive autonomy, dignity in intimate relationships, freedom from caste-like subordination. The positivist frame tells them their moral and social urgency is irrelevant; only formal amendment (Article V) can change the constraint. Amendment requires supermajority consensus, which is designed to be nearly impossible. They bear the extraction: their claims are treated as extra-constitutional even when they reflect evolved social consensus or derive from principles textually embedded (equal protection, due process).
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, substantive_justice_claimants, payer,
    powerless, biographical, trapped, national).

% LGBTQ+ persons, racial minorities, women, immigrants, and others whose historical exclusion from the original Constitution leaves them dependent on judicial recognition of unenumerated rights (privacy, equal dignity, freedom from discrimination) that the positivist frame excludes. The identity lock is structural: they cannot exit the jurisdiction and cannot alter their status to fit the original enumerated classes. Their only formal recourse is Article V amendment, which gives minorities zero negotiating power in a supermajority process. The constraint extracts their hope for judicial responsiveness.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, marginalized_groups_seeking_unenumerated_rights, payer,
    moderate, biographical, identity_locked, national).

% Hold a related but distinct reading: constitutional meaning is fixed at ratification and interpretation recovers original public understanding. They would object to the positivist reading's claim that procedure alone determines validity—originalists argue historical meaning is part of the enactment that must be recovered. Excluded from this constraint because this positivist story brackets meaning-recovery entirely.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, originalist_reading_proponents, excluded,
    organized, generational, constrained, national).

% Hold a directly opposed reading: constitutional meaning evolves with contemporary values and judges have interpretive authority to recognize emerging rights and adapt principles. They argue the positivist frame is cover story for conservative entrenchment—that 'formal procedure only' preserves the status quo by locking in 18th-century baselines. Structurally excluded because the positivist constraint's enforcement depends on rejecting their core premise (that judges can legitimately adapt meaning).
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, living_constitutionalist_proponents, excluded,
    organized, generational, constrained, national).

% Philosophers, political scientists, democratic theorists who analyze whether Article V is a legitimate vehicle for constitutional change. They observe that positivism's 'amendment only' gate concentrates power in supermajority-requiring processes that disadvantage minorities and prevent responsive governance. They measure the extraction's legitimacy empirically.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, democratic_amendment_proponents, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__positivist_reading, federal_legislative_body).
narrative_ontology:fixing_cost_class(us_constitution_text__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes constitutional meaning by anchoring validity in formal procedure (Article V amendment) rather than interpretive drift: all actors know the rules will not change except through supermajority-enforced change. Coordinates expectations about what is and is not constitutionally binding.
% TRANSFER_FUNCTION: Moves effective voice over constitutional meaning from those who can persuade courts (substantive-justice claimants, marginalized groups seeking evolving rights) to those who control formal amendment (legislative supermajorities, entrenched majorities). Transfers the cost of recognition—unenumerated rights must jump a nearly-insurmountable procedural hurdle.
% ABSENT_VOICES: Those excluded from the original enactment (women, enslaved persons, immigrants, religious minorities, LGBTQ+ persons) whose contemporary voices are not heard in interpretation because the positivist frame declares their claims procedurally illegitimate until and unless they achieve Article V amendment. The constraint keeps substantive claims out of the constitutional conversation.
% DISAPPEARANCE_RATIONALE: If the positivist constraint vanished and judges could recognize unenumerated rights through evolving interpretation, constitutional law would immediately become more responsive to contemporary values and social change. Decisions on reproductive rights, intimate association, equal dignity, and emerging justice claims would shift from 'extra-constitutional' to 'constitutionally protected.' Institutional stability would decrease (doctrine would drift more) but substantive responsiveness would increase.
% FOUNDING_PROBLEM: Early constitutional jurisprudence treated the Constitution as subject to unlimited reinterpretation by judges, leading to doctrinal instability and unpredictable outcomes. A written constitution was meant to be stable across generations; the positivist reading argues that meaning must be fixed at formal enactment to prevent judges from rewriting it.
% FOUNDING_PROBLEM_CORROBORATION: Conservative jurists and originalist scholars attest the problem persists—they cite Warren Court expansion of unenumerated rights (privacy, bodily autonomy) as examples of judges rewriting meaning. Progressive scholars and living-constitutionalist jurists contest the framing: they argue the founding problem is institutional constraint of responsive governance, not doctrinal drift. Independent constitutional theorists note that both accounts are empirically plausible but rest on different values (stability vs. justice responsiveness).
narrative_ontology:disappearance_verdict(us_constitution_text__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_text__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__positivist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is coded at 0.68 because the constraint systematically transfers voice over constitutional meaning from those pursuing substantive justice to those controlling amendment procedure (legislative supermajorities, entrenched coalitions). Suppression is high (0.72) because the constraint enforces its boundary through judicial refusal to recognize claims as constitutionally cognizable—the suppression is institutional, not physical, but structurally binding. Theater ratio rises to 0.41 because the constraint increasingly justifies itself as 'neutral procedure' while its actual function is to lock in status-quo distributions of constitutional protection. The measurement grid uses a shared time axis (0–35) for all three metrics so temporal analysis can detect any divergence or coupling. Extraction plateaus around t=20 because by the 1990s–2000s the constraint became nearly institutionally complete—living constitutionalism was already marginalized in mainstream jurisprudence.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (Supreme Court) experiences the constraint as genuine stability coordination: it stabilizes meaning and prevents doctrinal chaos. The beneficiary institutional seats (legislature, stability advocates) experience it as rule-of-law anchoring. The victim seats (substantive justice claimants, marginalized groups) experience it as foreclosure and extraction—their moral urgency is treated as irrelevant to constitutional validity. This divergence is structural: the same constraint appears as coordination to those who set its boundaries and as extraction to those locked out. The engine computes this from the declared beneficiary/victim and exit-option structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural asymmetry: institutional hierarchy (Supreme Court, legislature) benefits from predictability and control of amendment authority—their d is near the beneficiary end (0.1–0.2). Substantive justice claimants and marginalized groups are forced to pursue a nearly-impossible amendment while their claims are treated as extra-constitutional—their d is near the target end (0.8–0.95). Identity-locked marginalized groups have no exit (they cannot leave the jurisdiction, cannot change their status to fit the original enumeration), so their effective directionality is maximum-target (d ≈ 0.95). The theater ratio's rise indicates increasing performative claim (formal neutrality) defending an extractive structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The positivist reading invokes mandatrophy risk: it claims the founding problem is doctrinal drift and need for stability. But the founding problem diagnosis is itself contested—the living constitutionalist reading denies that responsive evolution produces instability, and comparative evidence from other democracies supports the denial. The constraint's mandate (preserve stability through formal procedure) is live, but the empirical claim that formal procedure is necessary for stability is increasingly challenged. The six_questions grid shows founding_problem_status='contested'—both institutional actors and external analysts dispute whether the founding problem still exists or whether it has been replaced by a different problem (institutional constraint of responsive governance). This contested status should trigger mandatrophy review when independent evidence on comparative constitutionalism accumulates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_neutrality_claim,
    'Is the positivist frame truly procedurally neutral, or does ''formal procedure only'' systematically advantage status-quo preservers (entrenched majorities) over change-seekers (marginalized groups)?',
    'Historical analysis of Article V amendment success rates and beneficiaries; comparison with alternative amendment procedures in other democracies; measurement of whose substantive claims are locked out by the positivist requirement vs. who gains protection from it.',
    'If procedure systematically advantages stability over justice, the constraint is a false neutral disguising extraction as rule-of-law values. The classification would shift from tangled_rope (genuine coordination + asymmetric extraction) toward snare (extraction disguised as coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(procedural_neutrality_claim, empirical, 'Whether formal-procedure-only is genuinely neutral or systematically favors institutional stability over substantive change.').

omega_variable(
    committer_frame_sibling_coexistence,
    'Are the three readings (positivist, originalist, living-constitutionalist) genuinely coexistent positions within a single legal framework, or does the positivist reading''s institutional dominance foreclose the others as live interpretive options?',
    'Institutional analysis of which reading dominates Supreme Court doctrine at different historical moments; measurement of judicial career consequences for adopting each reading; count of decisions decided by explicitly invoking each reading''s core premise.',
    'If the positivist reading has achieved near-total institutional dominance and enforces its frame through professional gatekeeping (judges adopting other readings face career costs), the relation to siblings should be ''influences'' rather than ''coexists_with''—the procedural gate becomes a suppression mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_sibling_coexistence, empirical, 'Whether the three kernel readings coexist as live institutional positions or whether positivism has institutionally foreclosed its siblings.').

omega_variable(
    unenumerated_rights_recognition_as_extraction,
    'Does the positivist constraint''s exclusion of unenumerated rights represent a genuine coordination boundary (some rights must be formally enumerated for stability) or does it constitute extraction (the constraint forces marginalized groups to shoulder the cost of their own legal recognition)?',
    'Comparative constitutional law: examine jurisdictions with responsive unenumerated-rights doctrine; measure whether stability is actually compromised; document historical moments where the constraint''s exclusion prevented recognition of rights now considered fundamental.',
    'If the constraint prevents recognition of rights that evolved social consensus treats as foundational (e.g., marriage equality, reproductive autonomy), and those rights are the core claims of identity-locked groups, the extraction is severe and the theater ratio (claiming ''neutral procedure'' while locking out specific voices) is high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unenumerated_rights_recognition_as_extraction, empirical, 'Whether exclusion of unenumerated rights is necessary for stability or whether it constitutes deliberate extraction of justice claims.').

omega_variable(
    axiom_overriding_empirically_contingent,
    'The positivist reading grounds its authority on the empirically contingent claim that formal procedure is necessary for constitutional stability and rule-of-law predictability. Has this empirical premise been systematically challenged or overridden by evidence from comparative constitutional systems?',
    'Comparative analysis of democracies with living-constitution doctrine, unenumerated rights protections, and flexible amendment processes: have they achieved substantively worse stability or rule-of-law outcomes? Or do they maintain both stability and responsiveness?',
    'If empirical evidence accumulates that formal-procedure-only is not necessary for stability (other democracies have responsive constitutions and rule-of-law), the foundational empirical axiom is overridden. The reading would retain axiom_status=''overridden''—still held by institutional actors (US positivist courts) but empirically no longer defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_overriding_empirically_contingent, empirical, 'Whether the empirical premise grounding positivism (formal procedure necessary for stability) has been overridden by comparative evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__positivist_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_text__positivist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(us_c_tr_t5, us_constitution_text__positivist_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_text__positivist_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(us_c_tr_t15, us_constitution_text__positivist_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_text__positivist_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(us_c_tr_t25, us_constitution_text__positivist_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_text__positivist_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(us_c_tr_t35, us_constitution_text__positivist_reading, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_text__positivist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(us_c_be_t5, us_constitution_text__positivist_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(us_c_be_t10, us_constitution_text__positivist_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(us_c_be_t15, us_constitution_text__positivist_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(us_c_be_t20, us_constitution_text__positivist_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(us_c_be_t25, us_constitution_text__positivist_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(us_c_be_t30, us_constitution_text__positivist_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(us_c_be_t35, us_constitution_text__positivist_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_text__positivist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(us_c_su_t5, us_constitution_text__positivist_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(us_c_su_t10, us_constitution_text__positivist_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(us_c_su_t15, us_constitution_text__positivist_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(us_c_su_t20, us_constitution_text__positivist_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(us_c_su_t25, us_constitution_text__positivist_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(us_c_su_t30, us_constitution_text__positivist_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(us_c_su_t35, us_constitution_text__positivist_reading, suppression_requirement, 35, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_text__positivist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, us_constitution_text__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, us_constitution_text__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% The kernel 'us_constitution_text' decomposes into three constraint stories corresponding to three distinct readings of what makes constitutional interpretation legitimate. Each reading produces a different ε, different beneficiary/victim structure, and different computed type. Positivist reading: text is authority via formal procedure alone; originalist reading: text's meaning is authority via historical recovery; living reading: text's principles are authority via evolving interpretation. These are not the same constraint viewed from different angles—they differ fundamentally on what legitimates constitutional meaning. They are linked as coexistent alternatives in a single contested kernel. All three stories name their kernel_id and reading_id; all three populate cs_structure.reading_relations and cs_structure.axioms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
