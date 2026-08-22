% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__positivist_reading, []).

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
 *   constraint_id: constitutional_text_authority__positivist_reading
 *   human_readable: Constitutional Text Authority — Positivist Reading
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   Legal positivism grounds constitutional validity in formal enactment
 *   procedure and institutional sources (legislative history, precedent,
 *   statutory plain meaning) rather than in moral content or natural law.
 *   This constraint instantiates the positivist reading of the contested
 *   constitutional kernel: the question of what makes the Constitution
 *   authoritative. The positivist reading answers: formal validity through
 *   procedure, not moral truth. This differs sharply from the originalist
 *   reading (which grounds authority in historical public understanding,
 *   often with natural law moorings) and the living constitutionalist reading
 *   (which locates authority in evolving moral principle). All three are live
 *   positions in contemporary jurisprudence; none currently rules out the
 *   others at the institutional level, though they compete for influence over
 *   doctrine.
 *
 * KEY AGENTS:
 *   - Legal positivist judges: institutional seats that adjudicate constitutionality by procedure and text
 *   - Formalist legal scholars: organized beneficiaries who develop positivist jurisprudence
 *   - Moral rights advocates: constrained targets whose arguments are excluded from constitutional validity
 *   - Natural law theorists: excluded parties whose fundamental premises are treated as irrelevant
 *   - Originalist and living constitutionalist judges: excluded seats whose competing readings coexist
 *   - Democratic legislatures: observers with structural interest in preserving procedure-based bounds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, 0.38).
domain_priors:suppression_score(constitutional_text_authority__positivist_reading, 0.22).
domain_priors:theater_ratio(constitutional_text_authority__positivist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__positivist_reading, rope).
narrative_ontology:human_readable(constitutional_text_authority__positivist_reading, "Constitutional Text Authority — Positivist Reading").
narrative_ontology:topic_domain(constitutional_text_authority__positivist_reading, "legal/constitutional").

domain_priors:requires_active_enforcement(constitutional_text_authority__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__positivist_reading, 'ed26c4a0-31d1-4bfb-86ac-5ac5473ffcb6').
narrative_ontology:cs_kernel_codification('ed26c4a0-31d1-4bfb-86ac-5ac5473ffcb6', fixed_text).
narrative_ontology:cs_authority_grounding('ed26c4a0-31d1-4bfb-86ac-5ac5473ffcb6', extraction).
narrative_ontology:cs_interpretation_layer_present('ed26c4a0-31d1-4bfb-86ac-5ac5473ffcb6').
narrative_ontology:cs_reading_relation('ed26c4a0-31d1-4bfb-86ac-5ac5473ffcb6', constitutional_text_authority__originalist_reading, influences).
narrative_ontology:cs_reading_relation('ed26c4a0-31d1-4bfb-86ac-5ac5473ffcb6', constitutional_text_authority__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_axiom('ed26c4a0-31d1-4bfb-86ac-5ac5473ffcb6', foundational, law_morality_distinction_constitutive).
narrative_ontology:cs_axiom_status(law_morality_distinction_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('ed26c4a0-31d1-4bfb-86ac-5ac5473ffcb6', law_morality_distinction_constitutive, conventional).
narrative_ontology:cs_axiom('ed26c4a0-31d1-4bfb-86ac-5ac5473ffcb6', foundational, procedural_validity_primacy).
narrative_ontology:cs_axiom_status(procedural_validity_primacy, holdable).
narrative_ontology:cs_axiom_grounding('ed26c4a0-31d1-4bfb-86ac-5ac5473ffcb6', procedural_validity_primacy, instrumental).
narrative_ontology:cs_reference_frame('ed26c4a0-31d1-4bfb-86ac-5ac5473ffcb6', textualist_procedural_authority).
narrative_ontology:cs_drift_state('ed26c4a0-31d1-4bfb-86ac-5ac5473ffcb6', contemporary_jurisprudence_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ed26c4a0-31d1-4bfb-86ac-5ac5473ffcb6', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__positivist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, legal_positivist_judges).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, formalist_legal_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, constitutional_citizens).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, moral_rights_advocates).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, natural_law_theorists).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, law_morality_distinction).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, procedural_validity_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and apply the Constitution by adherence to formal text, institutional sources (legislative history, precedent, enacted procedures), and formal validity criteria. Reject moral or natural law arguments as outside the judicial function. Maintain the law/morality distinction as core to the rule of law and judicial legitimacy. Their authority derives from fidelity to the procedure by which the Constitution was enacted and amended, not from moral truth.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, legal_positivist_judges, agenda_setter,
    institutional, generational, constrained, national).

% Develop and articulate the jurisprudential framework of legal positivism: that constitutional validity rests on formal enactment and procedure, not moral content. Their careers, publications, and intellectual credibility depend on the reading's coherence and institutional adoption. They argue this reading best protects democratic legitimacy by preventing judges from imposing personal moral views under the guise of constitutional interpretation.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, formalist_legal_scholars, beneficiary,
    organized, generational, mobile, national).

% Seek constitutional recognition of unenumerated moral rights (bodily autonomy, dignity, substantive equality) not expressly stated in the text. The positivist reading excludes moral argument from constitutional validity, forcing them to either reframe claims as textual interpretation or seek amendment — the constraint blocks their primary argumentative path.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, moral_rights_advocates, payer,
    moderate, biographical, constrained, national).

% Argue that constitutional authority ultimately derives from natural law or pre-political moral truth, and that formal procedure is merely the instrument through which that truth is expressed. The positivist reading defines their core premise (moral grounding of law) as irrelevant to constitutional validity, systematically excluding their tradition from authoritative constitutional argument.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, natural_law_theorists, payer,
    organized, generational, mobile, national).

% Would argue that constitutional meaning is fixed at ratification and derives authority from historical public understanding — a textual anchor that originalism shares with positivism. However, originalism often grounds its authority in natural law premises (Lockean natural rights as the Framers understood them), which the positivist reading treats as irrelevant to validity. Originalists can coexist in the same institution but their justificatory frameworks diverge fundamentally.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, originalist_judges, excluded,
    institutional, generational, mobile, national).

% Hold that constitutional meaning evolves with contemporary moral principles and social understanding. The positivist reading's law/morality distinction directly contradicts their core premise that moral values are constitutive of constitutional meaning. They would be heard if present but are structurally opposed at the level of fundamental authority.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, living_constitutionalist_judges, excluded,
    institutional, generational, mobile, national).

% Have the formal authority to amend the Constitution through prescribed procedures. The positivist reading supports their supremacy by restricting courts to procedure-based validity rather than moral substance — courts cannot override formal legislative action by invoking moral principle. They have structural interest in maintaining the procedure/morality boundary.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, democratic_legislatures, observer,
    institutional, generational, analytical, national).

% Are subject to constitutional rules as interpreted under this reading. They benefit from the predictability and restraint of judges who bind themselves to procedure and text rather than moral intuition. They also may be harmed by a reading that excludes novel moral claims from constitutional protection. Their exit is voting or emigration, both costly.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, constitutional_citizens, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__positivist_reading, constitutional_citizens, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__positivist_reading, legal_positivist_judges).
narrative_ontology:fixing_cost_class(constitutional_text_authority__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared authoritative method for determining what is constitutionally valid: procedure and text, not moral argument. This coordinates legal actors (judges, legislatures, lawyers) on a common interpretive method, preventing each judge from consulting their own moral views and producing incoherent constitutional law.
% TRANSFER_FUNCTION: Transfers interpretive authority from moral philosophers and rights advocates to judges and legislators constrained by procedure and text. Moral premises are excluded from constitutional validity; their proponents must reframe arguments as textual or seek amendment rather than judicial recognition.
% ABSENT_VOICES: Natural law theorists and moral-argument advocates would protest that the constraint excludes the highest sources of legitimacy (moral truth) from constitutional reasoning. Living constitutionalists would argue that moral evolution is not merely absent but actively suppressed. They are excluded from authoritative constitutional argument under this reading.
% DISAPPEARANCE_RATIONALE: If this constraint vanished (the law/morality distinction dissolved and moral argument became constitutionally valid), constitutional law would reorganize around competing moral frameworks. Judges would appeal to natural rights, dignity, equality, and substantive fairness instead of text and procedure. Amendment pathways would shrink as courts recognized unenumerated rights. The constitutional order would become more responsive to moral evolution and less stable under procedural law.
% FOUNDING_PROBLEM: Early American jurisprudence faced the problem that if constitutional validity rested on moral truth, different judges would consult different moral frameworks, producing incoherent law and usurping democratic amendment authority. The positivist reading was built to solve this: tether constitutional meaning to procedure and text, not the interpreter's moral beliefs.
% FOUNDING_PROBLEM_CORROBORATION: Legal positivist scholars (Hart, Shapiro, Coleman) and formalist judges attest the founding problem is still live: moral subjectivity threatens rule of law. Natural law and living constitutionalist traditions attest the problem has been misconceived: law divorced from morality is hollow formalism. Empirical legal studies examining judicial behavior diverge on whether judges actually constrain themselves to procedure or import moral views covertly.
narrative_ontology:disappearance_verdict(constitutional_text_authority__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text_authority__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__positivist_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__positivist_reading_tests).
:- end_tests(constitutional_text_authority__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end), not high, because the constraint serves a genuine coordination function: it creates a shared method that prevents judicial anarchy. However, moral rights advocates and natural law theorists bear costs (excluded arguments, blocked interpretive pathways) without directly causing the coordination problem the method solves. Suppression is low (0.22) because the constraint is not enforced through coercion — it operates through institutional legitimacy and jurisprudential coherence. Courts that follow positivism do so because the framework is intellectually coherent and democratically justified, not because they are forced. Theater is low (0.18) because the positivist constraint describes what institutional actors actually do (appeal to text and procedure, not moral philosophy). The measurement series are relatively flat: extractiveness and suppression rise slightly in the early interval as positivism gains institutional presence (post-Hart jurisprudence, 1960s–1980s), then plateau as the reading stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   The positivist judges and scholars authoring the constraint perceive it as genuine coordination protecting democratic legitimacy and rule of law. Moral rights advocates perceive it as enforced exclusion that blocks their constitutional claims. Natural law theorists perceive it as intellectually bankrupt formalism that severs law from its true authority. Living constitutionalists perceive it as a false procedure that covertly privileges settled text over evolved values. The engine computes directionality from structural data: positivist institutional seats collect interpretive authority (d near beneficiary); moral and natural law advocates bear exclusion (d near target). The gap is not a misperception — it reflects genuine asymmetry: the constraint benefits those whose premises align with procedure and text, harms those whose premises require moral argument.
 *
 * DIRECTIONALITY LOGIC:
 *   Legal positivist judges and formalist scholars benefit from the constraint: it grants them institutional authority to adjudicate constitutionality and insulates them from the charge that they are imposing moral views. They control the framework. Moral rights advocates and natural law theorists are targets: their core argumentative pathways are excluded from constitutional validity. They can reframe claims (moral arguments as hidden textual readings) but the constraint makes this costly and incoherent. Directionality for positivist seats approaches 0.0 (full beneficiaries); for natural law and moral-argument seats it approaches 1.0 (full targets). Democratic legislatures have low directionality (0.3–0.4) because they benefit from the procedure/morality boundary (it prevents judicial override) but also depend on constitutional rules that might be amended through procedure if moral pressure builds.
 *
 * MANDATROPHY ANALYSIS:
 *   The positivist reading appears to have mandatrophy tension: the founding problem (judge moral subjectivity, incoherent law) is disputed rather than dead, but the constraint persists as the dominant institutional frame because it is intellectually prestigious and democratic in form. However, this is not mandatrophy — it is genuine constitutional contest. The founding problem remains live because different jurisprudential traditions offer competing solutions (originalism and living constitutionalism each claim to solve judicial subjectivity better). The constraint persists not because its function has atrophied but because it is one live solution among three, each with institutional support. No theater-driven performance maintains it; it is actively taught, litigated, and defended. This is coexistence, not zombie constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positivism_vs_originalism_convergence,
    'Is legal positivism distinguishable from originalism on the level of constitutional interpretation, or are they converged on text-fidelity and differing only on the metaphysical grounding of that text-fidelity?',
    'Case-by-case analysis of originalist and positivist opinions: do they diverge on method (how meaning is derived from text) or only on justification (why procedure/text matters)? Can a positivist and an originalist reach different conclusions using their respective frameworks on the same constitutional question?',
    'If they converge on method, the distinction may be merely terminological and ''positivism'' may be a euphemism for methodological originalism without the natural law baggage. If they diverge, positivism is a distinct jurisprudential position. This affects whether sibling_relations should encode ''forecloses'' (if fundamentally different) or ''influences'' (if overlapping in method).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(positivism_vs_originalism_convergence, conceptual, 'Structural relationship between positivist method and originalist method on constitutional interpretation.').

omega_variable(
    judicial_adherence_to_positivism,
    'Do judges who claim positivist methodology actually constrain themselves to procedure and text, or do they covertly import moral reasoning under the guise of textual interpretation?',
    'Empirical legal studies analyzing patterns in judicial opinions: do self-identified positivist judges produce more text-bound decisions, more stable jurisprudence, and fewer moral-value pivots than judges identified with other frameworks? Does their actual method match their stated methodology?',
    'If judicial practice diverges significantly from positivist principle, the reading''s coordination function is partly performative — the suppression required to maintain it (excluding moral argument as formally invalid) enforces a discipline that judges do not internally honor, reclassifying the constraint toward snare. If practice aligns with principle, the coordination function is real and the constraint remains genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_adherence_to_positivism, empirical, 'Whether judicial behavior actually instantiates positivist constraints or whether positivism is a legitimacy narrative covering hidden moral reasoning.').

omega_variable(
    amendment_pathway_closure,
    'Does the law/morality distinction make the formal amendment pathway the only outlet for moral evolution in constitutional law, and does that pathway become politically inaccessible as a practical matter?',
    'Historical analysis of amendment frequency and political feasibility relative to Supreme Court docket in periods of high moral-value debate (civil rights, privacy, equality). Comparative institutional analysis: how many moral claims become constitutional law through amendment vs. through living constitutionalism in peer democracies (Canada, Australia, UK)?',
    'If the amendment pathway becomes inaccessible while living constitutionalism offers an alternative path for moral claims, the positivist constraint creates a form of constitutional stagnation — formally democratic (amendment is available) but practically exclusionary (amendment is unachievable). This would raise extractiveness and resistance substantially, moving the constraint toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_pathway_closure, empirical, 'Whether the law/morality distinction results in practical foreclosure of moral-evolution pathways despite formal amendment availability.').

omega_variable(
    reading_genealogy_dispute,
    'Is the positivist reading a coherent constitutional tradition with continuity to founding-era jurisprudence, or is it a twentieth-century philosophical imposition onto eighteenth-century practice?',
    'Historiographical analysis of founding-era jurisprudence: did the Framers and Ratifiers maintain a law/morality distinction, or did they understand the Constitution as embodying natural law? Examine original sources (Federalist papers, state constitutional traditions, legal treatises of the era) for evidence of positivist vs. natural-law framing.',
    'If the positivist reading is a late imposition, its authority claim (''procedure grounds validity'') is weaker and some of the measured extractiveness reflects the cost of imposing a novel framework on a natural-law tradition. If the Framers did maintain the distinction, the reading has deeper legitimacy. This affects whether the founding_problem_corroboration claim (that procedure solves subjectivity) is grounded in actual constitutional history or in twentieth-century jurisprudential theory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_genealogy_dispute, conceptual, 'Whether the positivist reading accurately represents constitutional tradition or imposes external philosophical framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__positivist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__positivist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cons_tr_t5, constitutional_text_authority__positivist_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(cons_tr_t10, constitutional_text_authority__positivist_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(cons_tr_t15, constitutional_text_authority__positivist_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement(cons_tr_t20, constitutional_text_authority__positivist_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(cons_tr_t25, constitutional_text_authority__positivist_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement(cons_tr_t30, constitutional_text_authority__positivist_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(cons_tr_t40, constitutional_text_authority__positivist_reading, theater_ratio, 40, 0.18).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__positivist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cons_be_t5, constitutional_text_authority__positivist_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(cons_be_t10, constitutional_text_authority__positivist_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(cons_be_t15, constitutional_text_authority__positivist_reading, base_extractiveness, 15, 0.36).
narrative_ontology:measurement(cons_be_t20, constitutional_text_authority__positivist_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(cons_be_t25, constitutional_text_authority__positivist_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(cons_be_t30, constitutional_text_authority__positivist_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(cons_be_t40, constitutional_text_authority__positivist_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__positivist_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(cons_su_t5, constitutional_text_authority__positivist_reading, suppression_requirement, 5, 0.19).
narrative_ontology:measurement(cons_su_t10, constitutional_text_authority__positivist_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(cons_su_t15, constitutional_text_authority__positivist_reading, suppression_requirement, 15, 0.21).
narrative_ontology:measurement(cons_su_t20, constitutional_text_authority__positivist_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(cons_su_t25, constitutional_text_authority__positivist_reading, suppression_requirement, 25, 0.22).
narrative_ontology:measurement(cons_su_t30, constitutional_text_authority__positivist_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement(cons_su_t40, constitutional_text_authority__positivist_reading, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text_authority__positivist_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% The constraint family 'constitutional_text_authority' decomposes the single natural-language concept (what makes the Constitution authoritative) into three structurally distinct constraint stories, each instantiating one reading of the contested kernel. The positivist reading (this story) grounds authority in formal procedure and text; it creates structural pressure on living constitutionalism (which requires moral argument to be authoritative) by excluding moral premises from constitutional validity. The originalist reading shares text-fidelity with positivism but diverges on grounding (historical intent vs. formal validity). All three readings coexist as live positions in institutional jurisprudence; none currently rules the others out. Each story carries its own ε, beneficiary/victim structure, and classification; the network edges model the downstream influence one reading's dominance exerts on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text_authority__positivist_reading, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
