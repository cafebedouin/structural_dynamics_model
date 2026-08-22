% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__evolutionary_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__evolutionary_framework, []).

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
 *   constraint_id: common_law_precedent_corpus__evolutionary_framework
 *   human_readable: Common Law Precedent Corpus — Evolutionary Reading
 *   domain: legal_theory/jurisprudence/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the EVOLUTIONARY READING of the common
 *   law precedent corpus — a framework that treats precedent as an adaptive
 *   institutional device permitting courts to reframe and overturn doctrine
 *   when contemporary normative understanding shifts. Under this reading,
 *   precedent is not a backward-binding constraint but a forward-looking
 *   guide that courts legitimately update when prior justifications no longer
 *   hold or when social conditions have fundamentally changed. This reading
 *   empowers appellate courts as normative updaters and gives litigants
 *   multiple pathways to challenge settled doctrine: distinguishing cases,
 *   reinterpreting holdings, and arguing for overruling on grounds of evolved
 *   understanding. The constraint's persistence depends on active judicial
 *   use of these reframing moves and on maintaining rhetorical coherence
 *   between stability and evolution. It is claimed as ROPE (genuine
 *   coordination problem: balancing precedent-reliance with normative
 *   flexibility) but operates with modest extractiveness (0.38) because the
 *   coordination function is real even though interpretive authority
 *   concentrates in appellate hands.
 *
 * KEY AGENTS:
 *   - Appellate Judiciary: Sets the norms for precedent interpretation and decides when overruling is justified by evolved understanding — institutional power to legitimate reframing
 *   - Progressive Advocates: Litigate to challenge standing doctrine under the evolutionary reading; benefit from expanded pathways to normative reframing
 *   - Conservative Parties: Bear the cost of precedential instability; their reliance on settled doctrine is exposed to reframing by appellate courts
 *   - Practicing Attorneys: Navigate between need for stable doctrine and expanded discretion in appellate reframing — biographical-horizon vulnerability
 *   - Lower Court Judges: Formally bound by precedent but given latitude to signal openness to overruling; trapped in tension between constraint and discretion
 *   - Constitutional Originalists: Excluded from authority structure; their epistemic claim (historical meaning binds) reframed as partial consideration rather than principle
 *   - Rule-of-Law Institutions: Monitor whether evolutionary reframing produces justified norm evolution or ad hoc overruling masked as interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, 0.38).
domain_priors:suppression_score(common_law_precedent_corpus__evolutionary_framework, 0.22).
domain_priors:theater_ratio(common_law_precedent_corpus__evolutionary_framework, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, extractiveness, 0.38).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__evolutionary_framework, rope).
narrative_ontology:human_readable(common_law_precedent_corpus__evolutionary_framework, "Common Law Precedent Corpus — Evolutionary Reading").
narrative_ontology:topic_domain(common_law_precedent_corpus__evolutionary_framework, "legal_theory/jurisprudence/constitutional_law").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__evolutionary_framework).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__evolutionary_framework, '9c9b6c3b-ae43-426d-9cff-3f5270708716').
narrative_ontology:cs_kernel_codification('9c9b6c3b-ae43-426d-9cff-3f5270708716', fixed_text).
narrative_ontology:cs_authority_grounding('9c9b6c3b-ae43-426d-9cff-3f5270708716', lineage).
narrative_ontology:cs_interpretation_layer_present('9c9b6c3b-ae43-426d-9cff-3f5270708716').
narrative_ontology:cs_reading_relation('9c9b6c3b-ae43-426d-9cff-3f5270708716', common_law_precedent_corpus__strict_stare_decisis, forecloses).
narrative_ontology:cs_reading_relation('9c9b6c3b-ae43-426d-9cff-3f5270708716', common_law_precedent_corpus__pluralist_balancing, coexists_with).
narrative_ontology:cs_axiom('9c9b6c3b-ae43-426d-9cff-3f5270708716', foundational, precedent_permits_normative_reinterpretation).
narrative_ontology:cs_axiom_status(precedent_permits_normative_reinterpretation, holdable).
narrative_ontology:cs_axiom_grounding('9c9b6c3b-ae43-426d-9cff-3f5270708716', precedent_permits_normative_reinterpretation, deontological).
narrative_ontology:cs_axiom('9c9b6c3b-ae43-426d-9cff-3f5270708716', foundational, contemporary_values_override_historical_meaning).
narrative_ontology:cs_axiom_status(contemporary_values_override_historical_meaning, holdable).
narrative_ontology:cs_axiom_grounding('9c9b6c3b-ae43-426d-9cff-3f5270708716', contemporary_values_override_historical_meaning, deontological).
narrative_ontology:cs_reference_frame('9c9b6c3b-ae43-426d-9cff-3f5270708716', precedent_as_binding_forward_guide).
narrative_ontology:cs_drift_state('9c9b6c3b-ae43-426d-9cff-3f5270708716', contemporary_normative_evolution_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('9c9b6c3b-ae43-426d-9cff-3f5270708716', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, progressive_advocates).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, normative_updating_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, practicing_attorneys).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, conservative_parties).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, practicing_attorneys).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, lower_court_judges).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__evolutionary_framework, law_evolves_with_society).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__evolutionary_framework, judicial_creativity_within_precedent).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__evolutionary_framework, norm_correction_as_legitimate_function).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets precedent and decides when overruling is justified by evolved understanding or changed social conditions. Under the evolutionary reading, judicial authority to update norms is legitimate and necessary; they frame overruling as corrective rather than activist. They hold substantial discretion in distinguishing vs. reinterpreting vs. overruling prior cases. They could change this framework entirely but would forfeit their interpretive authority in doing so.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Litigate to establish new normative frames or overturn precedents they view as obsolete. The evolutionary reading gives them multiple pathways to challenge standing doctrine: distinguishing cases on facts, reinterpreting holdings in light of evolved values, and arguing that precedent no longer serves its justifying function. They can exit to other jurisdictions or constituencies if the evolutionary reading is rejected, but maintain influence when it holds.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, progressive_advocates, beneficiary,
    organized, generational, mobile, national).

% Law schools, reform organizations, and academic scholarship communities benefit from the evolutionary reading's legitimation of their work — reframing law as an updating process rather than a fixed system. They validate the constraint by producing theory and evidence that precedent can and should evolve. They maintain influence by authoring the normative evolution narratives courts cite.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, normative_updating_institutions, beneficiary,
    organized, generational, mobile, national).

% Rely on settled precedent to maintain legal architecture they view as sound. Under the evolutionary reading, their precedential shields are more permeable — overruling is normalized and justified by reference to evolved norms rather than requiring extraordinary justification. Their investment in existing doctrine is more exposed to reframing. They cannot exit the common law system but can invest in persuading courts not to overrule favoring precedents.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, conservative_parties, payer,
    organized, generational, constrained, national).

% Need stable doctrine for counseling clients and predicting outcomes. The evolutionary reading introduces greater uncertainty in long-settled areas — they cannot confidently tell a client that a 40-year-old precedent will govern their case if normative evolution can justify overruling. They also benefit from expanded pathways to reframe losing doctrine, giving them more strategic flexibility. They are trapped in the constraint by career dependence on predicting legal outcomes.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, practicing_attorneys, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__evolutionary_framework, practicing_attorneys, beneficiary).

% Formally bound by appellate precedent but given wider latitude under the evolutionary reading to distinguish cases, predict overruling, and signal openness to normative updating. They bear the tension between formal constraint and growing discretion; their decisions are more exposed to appeal courts reframing what they thought was settled. They are trapped in the judicial hierarchy and cannot exit without abandoning their career.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, lower_court_judges, payer,
    moderate, biographical, trapped, national).

% Advocate a reading of precedent rooted in textual and historical fixity. Under the evolutionary framework, they are excluded from setting the norms that govern how precedent is treated — the constraint's authority structure privileges contemporary normative evolution over historical fidelity. Their objections to overruling are framed as departures from proper judicial method rather than as legitimate institutional interests. They argue within the constraint but are structurally disadvantaged in the evolutionary framework.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, constitutional_originalists, excluded,
    organized, generational, constrained, national).

% Monitor the constraint's consistency with predictability, reliance interests, and democratic legitimacy. They track whether the evolutionary reading produces justified norm evolution or ad hoc overruling masked as interpretation; whether litigants' expanded pathways yield coherent doctrine or fragmented outcomes. They analyze the constraint's operation from outside the beneficiary/victim dynamic.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, rule_of_law_institutions, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__evolutionary_framework, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes hierarchical legal authority around a common corpus of precedent, enabling lower courts to follow binding doctrine and litigants to predict outcomes based on written holdings, while permitting appellate courts to correct or reframe doctrine when normative understanding shifts or prior reasoning no longer justifies the rule.
% TRANSFER_FUNCTION: Moves interpretive authority from fixed historical context and textual meaning toward contemporary normative frameworks; transfers discretion from strict-following-rule to flexible-reframing-authority in the appellate judiciary; expands litigants' access to precedent-challenge pathways (distinguishing, reinterpreting, overruling arguments) rather than foreclosing them.
% ABSENT_VOICES: Originalist jurists, textualists, and strict rule-of-law advocates who argue that evolutionary reinterpretation abandons fidelity to law and substitutes judicial will. They are excluded from defining the constraint's authority structure — the evolutionary reading treats their epistemic claim (historical meaning is binding) as a partial consideration rather than as the fundamental principle of precedent constraint.
% DISAPPEARANCE_RATIONALE: If the evolutionary reading and its empowerment of appellate courts to normalize overruling vanished, doctrine would solidify under strict-stare-decisis or pluralist-balancing readings — precedent would bind more rigidly, overruling would require extraordinary justification, and litigants' pathways to challenge settled doctrine would narrow. The legal system would reorganize around different assumptions about precedent's role.
% FOUNDING_PROBLEM: Early common law developed rigid precedent doctrine that locked courts into historically contingent rules even when social conditions or moral understanding shifted. The constraint was developed to solve the problem: how to maintain respect for precedent (continuity, reliance, institutional memory) while allowing normative evolution (correction of injustice, adaptation to changed circumstances).
% FOUNDING_PROBLEM_CORROBORATION: Progressive and reform-oriented legal scholars and judges attest that the founding problem is live: precedent frequently perpetuates outdated or harmful doctrine and courts need latitude to correct it. Strict-stare-decisis advocates and conservative jurists attest the founding problem is either not live (precedent evolves through incremental case distinctions, not wholesale reframing) or that the evolutionary solution creates worse problems (instability, judicial discretion, unpredictability). Legislative testimony, law review analysis from both traditions, and comparative jurisprudence from common-law jurisdictions provide external corroboration.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__evolutionary_framework, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__evolutionary_framework, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__evolutionary_framework, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_law_precedent_corpus__evolutionary_framework, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__evolutionary_framework, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__evolutionary_framework_tests).
:- end_tests(common_law_precedent_corpus__evolutionary_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is modest (0.38, rising to 0.39 midway then stabilizing) because the constraint's core function — permitting normative evolution while maintaining precedent respect — is genuine coordination work. Appellate courts bear real costs in maintaining coherence across overrulings, and the system requires substantial judicial labor to distinguish cases legitimately from overruling them dishonestly. Suppression is low (0.22) because the constraint does not depend on excluding alternatives (strict-stare-decisis and pluralist-balancing readings remain live in doctrine and scholarship). Theater ratio is modest (0.18) because the evolutionary framing has real institutional work (distinguishing, reinterpreting, overruling) but also includes performative elements (casting overruling as evolution rather than will). The measurement series shows extractiveness rising through year 30 (as the evolutionary reading normalized) then stabilizing, indicating the constraint reached equilibrium as appellate courts internalized evolutionary justification as standard practice. Theater ratio similarly plateaus, suggesting the legitimation work is settled. One shared time grid across all three metrics: every metric authored at every examined point, enabling proper lifecycle analysis.
 *
 * PERSPECTIVAL GAP:
 *   The evolutionary reading appears as a solution to a genuine problem (how to permit precedent evolution) from the seat of appellate courts and progressive advocates. From the seat of strict-stare-decisis advocates and litigants relying on settled doctrine, it appears as a constraint mechanism permitting appellate courts to reframe doctrine at will under the cover of normative evolution — the same structure that looks like flexible coordination to one seat looks like discretionary extraction to another. The engine computes each seat's experienced type from this structural asymmetry; the claim (rope) does not predict or override the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: appellate courts (empowered to update norms, legitimate discretion), progressive advocates (expanded pathways to challenge doctrine), normative-updating institutions (law schools, reform movements benefit from evolutionary framing). Victims: conservative parties (precedential shields are permeable), strict-stare-decisis advocates (excluded from authority structure), and lower courts (trapped in tension between formal constraint and discretionary reframing). The evolutionary reading's beneficiary structure is ideological-institutional: it privileges those who advocate normative updating and disempowers those who prioritize stability and originalism. Exit options drive this: judges have analytical exit (they interpret the constraint); advocates have mobile exit (they choose cases and arguments); conservative parties have constrained exit (they must argue within the constraint they oppose). Directionality overrides are not needed — the structural derivation captures the true relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The evolutionary reading's founding problem is: 'Precedent locks courts into historically contingent rules; how do we permit normative evolution while maintaining respect for precedent?' The founding problem status is CONTESTED — progressive scholars and appellate judges say it is live and the evolutionary reading solves it. Strict-stare-decisis advocates say the founding problem is not live (precedent evolves gradually through case distinctions) or that the evolutionary solution creates worse problems (instability, judicial will). This contested genealogy is precisely the structure that requires the evolutionary reading: without it, the founding problem cannot be solved; with it, the solution is vulnerable to reframing as judicial activism. The mandatrophy signal emerges if the constraint's core justification (permitting normative correction) becomes widely viewed as a pretext for judicial will rather than legitimate evolution. The playwright watching this constraint would track whether overruling decisions cite normative evolution genuinely or as theatrical cover for other agendas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_evolution_vs_judicial_will,
    'When appellate courts invoke ''evolved normative understanding'' to overturn precedent, are they genuinely tracking changes in social values and moral philosophy, or are they exercising raw judicial will dressed in evolutionary language?',
    'Comparative analysis of judicial opinions before/after major normative shifts (e.g., civil rights movement, gender equality, LGBTQ+ equality) paired with independent historical/philosophical documentation of when normative understanding actually shifted in the broader society. If overruling decisions systematically lag or lead social movement rather than tracking it, the rhetoric is theatrical.',
    'If resolution shows genuine tracking, the constraint is rope coordinating stability with legitimate evolution. If resolution shows predominantly rhetorical cover, the constraint reclassifies toward snare — judicial power extraction masked by evolutionary legitimation narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_evolution_vs_judicial_will, empirical, 'Whether evolutionary justification tracks genuine normative change or serves as pretext for judicial discretion.').

omega_variable(
    competing_readings_foreclosure_vs_coexistence,
    'Do the evolutionary and strict-stare-decisis readings truly foreclose each other (logically incompatible frameworks), or do they coexist as competing live positions held by different judicial coalitions within the same legal system?',
    'Institutional analysis of whether appellate courts have integrated both readings in their doctrine (e.g., using evolutionary rationales in some domains while claiming stare-decisis deference in others) or whether one reading has formally displaced the other. Legal history showing whether strict-stare-decisis ever holds as controlling doctrine in any working legal system, or whether it is always contested.',
    'Foreclosure would mean one reading''s authority structure is incompatible with the other, supporting the reading_relations declaration. Coexistence would mean both readings remain live alternatives held by different factions, shifting the reading_relations to coexists_with and lowering confidence in the forecloses claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_readings_foreclosure_vs_coexistence, conceptual, 'Whether reading-level disagreement on precedent is logically structured as foreclosure or coexistence.').

omega_variable(
    lower_court_compliance_with_evolutionary_discretion,
    'When appellate courts expand the latitude for lower courts to distinguish and reinterpret precedent under evolutionary rationales, do lower courts exercise this discretion to update norms consistent with appellate intent, or do they use expanded latitude as permission for inconsistent overruling?',
    'Empirical study of lower-court patterns in distinguishing appellate precedent: do they systematically align with appellate signals about normative evolution, or do they produce fragmented doctrine inconsistent with appellate directives?',
    'If lower courts align with appellate intent, the constraint functions as intended rope. If lower courts fragment doctrine, the constraint shows theatrical coordination (theater_ratio rises) and extractive dysfunction — appellate power flows downward but does not produce coherent normative updating.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lower_court_compliance_with_evolutionary_discretion, empirical, 'Whether expanded lower-court discretion produces alignment or fragmentation in doctrine.').

omega_variable(
    originalism_exclusion_legitimacy,
    'Is the exclusion of originalist and textualist readings from the authority structure of precedent legitimate as a matter of institutional design, or is it an exercise of power by evolutionary advocates to suppress competing epistemic claims about what makes law binding?',
    'Meta-jurisprudential analysis: do evolutionary readings engage originalist premises and refute them, or do they simply reframe originalism as a ''consideration'' rather than addressing its logical force? Historical examination of whether originalism was considered and rejected, or whether it was never seated in the authority conversation.',
    'If originalism was genuinely engaged and found unpersuasive, the exclusion is legitimate institutional choice. If originalism is suppressed without address, the evolutionary reading functions partly as a snare mechanism — excluding parties who would contest the constraint''s naturalness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_exclusion_legitimacy, preference, 'Whether originalist exclusion reflects reasoned institutional choice or exercise of interpretive power.').

omega_variable(
    coordination_vs_extraction_boundary_reading_variant,
    'Can the evolutionary reading simultaneously maintain the coordination function (adapting precedent to social change) and the extraction function (concentrating interpretive power in appellate courts), or is one function inevitable chosen over the other?',
    'Comparative study of common-law jurisdictions with evolutionary readings vs. those with strict-stare-decisis or pluralist readings: do evolutionary jurisdictions show more normative updating AND more appellate power, or does normative benefit accrue to a narrow judicial elite?',
    'If both functions scale together, the constraint is genuinely rope with built-in asymmetry (appellate gain). If updating is concentrated (only appellate courts benefit, lower courts and litigants see constraint rigidity), the constraint approaches tangled-rope or snare — coordination as cover for appellate extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary_reading_variant, empirical, 'Whether evolutionary reading''s coordination and extraction functions are compatible or trade off.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__evolutionary_framework, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(comm_tr_t0, observed).
narrative_ontology:measurement(comm_tr_t10, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(comm_tr_t10, observed).
narrative_ontology:measurement(comm_tr_t20, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(comm_tr_t20, observed).
narrative_ontology:measurement(comm_tr_t30, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(comm_tr_t30, observed).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(comm_tr_t40, observed).
narrative_ontology:measurement(comm_tr_t50, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 50, 0.18).
narrative_ontology:measurement_basis(comm_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(comm_be_t0, observed).
narrative_ontology:measurement(comm_be_t10, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 10, 0.32).
narrative_ontology:measurement_basis(comm_be_t10, observed).
narrative_ontology:measurement(comm_be_t20, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 20, 0.36).
narrative_ontology:measurement_basis(comm_be_t20, observed).
narrative_ontology:measurement(comm_be_t30, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 30, 0.39).
narrative_ontology:measurement_basis(comm_be_t30, observed).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(comm_be_t40, observed).
narrative_ontology:measurement(comm_be_t50, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 50, 0.38).
narrative_ontology:measurement_basis(comm_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(comm_su_t0, observed).
narrative_ontology:measurement(comm_su_t10, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 10, 0.2).
narrative_ontology:measurement_basis(comm_su_t10, observed).
narrative_ontology:measurement(comm_su_t20, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 20, 0.21).
narrative_ontology:measurement_basis(comm_su_t20, observed).
narrative_ontology:measurement(comm_su_t30, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 30, 0.23).
narrative_ontology:measurement_basis(comm_su_t30, observed).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(comm_su_t40, observed).
narrative_ontology:measurement(comm_su_t50, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 50, 0.22).
narrative_ontology:measurement_basis(comm_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__evolutionary_framework, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__evolutionary_framework, 0.12).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__pluralist_balancing).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, judicial_legitimacy_through_reasoned_constraint).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, appellate_authority_and_institutional_hierarchy).

% DUAL FORMULATION NOTE:
% The common_law_precedent_corpus kernel constrains three structurally distinct readings: evolutionary_framework (this story) treats precedent as permitting normative reinterpretation; strict_stare_decisis treats precedent as backward-binding with limited exit; pluralist_balancing treats precedent weight as domain-variable. Each reading has different beneficiaries, different ε values, and different operative mechanisms. They are linked as sibling constraint stories — not one constraint with three perspectives, but three constraints on the same kernel. The evolutionary reading shows the lowest structural rigidity and the highest beneficiary concentration in appellate courts; strict_stare_decisis shows highest rigidity and lowest beneficiary concentration; pluralist_balancing sits between. The three stories together model the family structure of competing institutionalized readings of a single contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
