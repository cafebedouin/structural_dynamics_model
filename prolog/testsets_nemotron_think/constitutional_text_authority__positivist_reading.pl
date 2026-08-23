% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   human_readable: Constitutional Validity from Formal Enactment (Positivist Reading)
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   This constraint story captures the positivist reading of constitutional
 *   text authority: validity derives solely from formal enactment procedures
 *   and institutional sources (legislative vote, ratification, judicial
 *   precedent), not from the moral content of the norms enacted. The
 *   law/morality distinction is maintained as a structural feature of the
 *   legal system. The constraint coordinates constitutional practice around a
 *   source-based validity test, suppressing moral argument as a criterion of
 *   validity. It is claimed as a coordination mechanism (rope/tangled_rope)
 *   but extracts interpretive freedom from moral-reading practitioners and
 *   excludes citizens whose claims require moral premises. The engine will
 *   compute per-seat classifications from the structural data; the
 *   claimed_type and metrics are authored independently.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, 0.45).
domain_priors:suppression_score(constitutional_text_authority__positivist_reading, 0.65).
domain_priors:theater_ratio(constitutional_text_authority__positivist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__positivist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__positivist_reading, "Constitutional Validity from Formal Enactment (Positivist Reading)").
narrative_ontology:topic_domain(constitutional_text_authority__positivist_reading, "legal/constitutional").

domain_priors:requires_active_enforcement(constitutional_text_authority__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__positivist_reading, '1847080b-12dc-4bf9-89db-dca62c2d00fc').
narrative_ontology:cs_kernel_codification('1847080b-12dc-4bf9-89db-dca62c2d00fc', formalized).
narrative_ontology:cs_authority_grounding('1847080b-12dc-4bf9-89db-dca62c2d00fc', expertise).
narrative_ontology:cs_interpretation_layer_present('1847080b-12dc-4bf9-89db-dca62c2d00fc').
narrative_ontology:cs_reading_relation('1847080b-12dc-4bf9-89db-dca62c2d00fc', constitutional_text_authority__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1847080b-12dc-4bf9-89db-dca62c2d00fc', constitutional_text_authority__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_axiom('1847080b-12dc-4bf9-89db-dca62c2d00fc', foundational, legal_validity_from_formal_sources_only).
narrative_ontology:cs_axiom_status(legal_validity_from_formal_sources_only, holdable).
narrative_ontology:cs_axiom_grounding('1847080b-12dc-4bf9-89db-dca62c2d00fc', legal_validity_from_formal_sources_only, conventional).
narrative_ontology:cs_axiom('1847080b-12dc-4bf9-89db-dca62c2d00fc', foundational, law_morality_separation_thesis).
narrative_ontology:cs_axiom_status(law_morality_separation_thesis, holdable).
narrative_ontology:cs_axiom_grounding('1847080b-12dc-4bf9-89db-dca62c2d00fc', law_morality_separation_thesis, conventional).
narrative_ontology:cs_reference_frame('1847080b-12dc-4bf9-89db-dca62c2d00fc', formal_enactment_framework).
narrative_ontology:cs_drift_state('1847080b-12dc-4bf9-89db-dca62c2d00fc', contemporary_originalist_convergence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1847080b-12dc-4bf9-89db-dca62c2d00fc', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__positivist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, formalist_judges).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, legal_positivist_scholars).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, institutional_actors).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, living_constitutionalist_practitioners).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, natural_law_theorists).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, moral_reading_advocates).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, legal_positivism).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, separation_thesis).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, formal_enactment_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate constitutional cases using only formal sources (text, precedent, enacted procedure). Their authority derives from institutional role; they benefit from a clear, manageable validity criterion that limits judicial discretion. Exit means leaving the bench or adopting a rival methodology — both institutionally costly.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, formalist_judges, agenda_setter,
    institutional, generational, arbitrage, national).

% Produce and teach the theoretical framework that treats legal validity as source-dependent. They gain professional recognition and doctrinal coherence from the constraint's dominance. Can exit to other jurisprudential traditions with moderate career cost.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, legal_positivist_scholars, beneficiary,
    organized, biographical, mobile, national).

% Legislatures and executives whose enactments gain constitutional validity through formal procedure alone, without moral scrutiny. They benefit from predictability and procedural finality. Exit is structurally improbable — they are the source-constituting institutions.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, institutional_actors, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__positivist_reading, institutional_actors, agenda_setter).

% Judges and scholars who argue constitutional meaning evolves with moral principles. The positivist constraint renders their methodology illegitimate within formal validity tests. They bear the cost of exclusion from authoritative interpretation; exit means conceding the positivist frame or moving to dissent.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, living_constitutionalist_practitioners, payer,
    organized, biographical, constrained, national).

% Scholars and advocates who hold that unjust enactments lack legal validity. The separation thesis directly forecloses their core claim. Their identity is fused to the moral-reading project; exit would require abandoning a career-defining theoretical commitment.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, natural_law_theorists, payer,
    organized, biographical, identity_locked, national).

% Civil rights litigators, progressive legal movements, and theorists who use moral argument to expand constitutional protections. They pay through lost doctrinal avenues when formal sources yield narrow rights. Exit means strategic pivot to textualism or political mobilization — possible but costly.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, moral_reading_advocates, payer,
    moderate, biographical, constrained, national).

% Individuals and groups whose constitutional claims depend on moral principles not found in formal sources (e.g., dignity-based rights, unenumerated protections). They have no voice in the validity framework; their claims are structurally inadmissible. Exit from the legal system is effectively impossible.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, excluded_citizens, excluded,
    powerless, biographical, trapped, national).

% Legal theorists, philosophers, and comparative scholars who study the constraint from outside the adjudicative practice. They neither collect nor pay; they map the structure of the validity debate across readings.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, procedurally grounded method for identifying valid constitutional law, replacing contested moral reasoning with institutional sources that are publicly ascertainable.
% TRANSFER_FUNCTION: Moves interpretive authority from moral reasoning to formal institutional procedures; the power to say what the constitution validly means shifts from moral argument to textual and procedural analysis conducted by authorized institutional actors.
% ABSENT_VOICES: Natural law theorists and living constitutionalist judges who would argue moral principles are necessary for constitutional validity; they are excluded from the positivist framework's criteria of validity. Also excluded: citizens whose rights claims depend on moral readings the framework declares inadmissible.
% DISAPPEARANCE_RATIONALE: If the formal validity constraint vanished overnight, courts and officials would legitimately appeal to moral principles, natural law, or evolving standards as grounds of constitutional validity, fundamentally changing how constitutional disputes are resolved and which claims succeed.
% FOUNDING_PROBLEM: The indeterminacy and contestation of moral reasoning in constitutional adjudication; the need for an objective, institutional criterion of legal validity that does not depend on contested moral truths and can constrain judicial discretion.
% FOUNDING_PROBLEM_CORROBORATION: Legal realists and critical legal studies scholars (outside the positivist beneficiary set) acknowledge the positivist project's aim of constraining judicial discretion through formal criteria, even while criticizing its feasibility. Historical institutionalists document the late-19th/early-20th century turn to formal validity as a response to Lochner-era judicial moralizing.
narrative_ontology:disappearance_verdict(constitutional_text_authority__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text_authority__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__positivist_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction (0.45) reflects the constraint's appropriation of interpretive authority from moral-reading approaches — it is not negligible but not dominant; the coordination function (determinate validity criteria) is real. Suppression (0.65) is higher: moral arguments are actively excluded from validity determinations, not merely disadvantaged. Theater (0.22) is low-moderate: formal procedures genuinely operate, but a growing share of doctrinal work (textualism, originalism convergence) performs formalism while importing contested historical/moral judgments. Accessibility collapse (0.52) and resistance (0.55) are moderate: moral-reading alternatives persist and contest the framework, but within the positivist frame they are structurally inadmissible. Measurements track the 20th-century rise of positivism (Hart, Kelsen), its mid-century dominance, and late-century challenge from living constitutionalism and originalism's moralized variants.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (formalist judges), the constraint is genuine coordination — it makes constitutional law determinate and administrable. From the payer seats (living constitutionalists, natural lawyers), it is extraction — their interpretive labor is devalued and their claims rendered inadmissible. The engine computes this divergence; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Formalist judges and institutional actors are structural beneficiaries (d near 0.0-0.2): they control and profit from the validity criterion. Legal positivist scholars benefit professionally (d ~0.2). Living constitutionalist practitioners and moral-reading advocates are targets (d ~0.7-0.8): their methodology is excluded from validity. Natural law theorists are identity-locked targets (d ~0.9): their core theoretical identity is foreclosed. Excluded citizens are trapped (d ~0.95): they bear costs with no voice. Analytical observers sit at d=0.5 (symmetric). The engine derives these from beneficiary/victim declarations, power, and exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constraining judicial moral discretion) remains contested: formalists say it persists; critics say formal validity itself became a vehicle for conservative moral outcomes. The constraint has not resolved into pure coordination or pure extraction — it is a tangled rope whose coordination function (determinate validity) and extraction function (excluding moral readings) are structurally fused. Mandatrophy is not resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Does the positivist reading of constitutional_text_authority represent a distinct constraint from its siblings, or a strategic framing of a shared practice?',
    'Compare the institutional enforcement patterns: if courts citing positivist validity criteria systematically reach different outcomes than courts citing originalist or living constitutionalist criteria in the same case types, the readings instantiate distinct constraints.',
    'If distinct, each reading gets its own ε and classification; if framing variants, they are one constraint with observer-dependent ε — violating ε-invariance and requiring decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the kernel''s declared readings are structurally distinct constraints or framing variants of one constraint.').

omega_variable(
    separation_thesis_collapse,
    'Does the law/morality distinction genuinely hold in hard cases, or does formal validity inevitably import moral judgment through ''formal'' criteria (e.g., original public meaning, textual ambiguity resolution)?',
    'Empirical study of judicial opinions: track whether formalist judges'' outcomes correlate with their moral/political priors in contested cases, controlling for formal source clarity.',
    'If the distinction collapses in practice, the constraint''s coordination function is theater; extraction is higher than measured; classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separation_thesis_collapse, empirical, 'Whether the positivist separation thesis operates as described or masks moralized decision-making.').

omega_variable(
    originalist_convergence_extraction,
    'Does the convergence with originalism on text-fidelity create a joint extraction mechanism against living constitutionalism, concentrating interpretive authority in a formalist coalition?',
    'Analyze citation networks and coalition voting: do formalist and originalist judges form a stable bloc that excludes living constitutionalist methodologies from validity?',
    'If yes, the constraint''s extraction is coalition-amplified; the beneficiary set expands to include originalist actors; the constraint may be a larger tangled_rope than the positivist reading alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(originalist_convergence_extraction, empirical, 'Whether positivist-originalist convergence functions as a coordinated extraction coalition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__positivist_reading, 0, 124).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__positivist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t31, constitutional_text_authority__positivist_reading, theater_ratio, 31, 0.15).
narrative_ontology:measurement(cons_tr_t62, constitutional_text_authority__positivist_reading, theater_ratio, 62, 0.18).
narrative_ontology:measurement(cons_tr_t93, constitutional_text_authority__positivist_reading, theater_ratio, 93, 0.2).
narrative_ontology:measurement(cons_tr_t124, constitutional_text_authority__positivist_reading, theater_ratio, 124, 0.22).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__positivist_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(cons_be_t31, constitutional_text_authority__positivist_reading, base_extractiveness, 31, 0.32).
narrative_ontology:measurement(cons_be_t62, constitutional_text_authority__positivist_reading, base_extractiveness, 62, 0.41).
narrative_ontology:measurement(cons_be_t93, constitutional_text_authority__positivist_reading, base_extractiveness, 93, 0.44).
narrative_ontology:measurement(cons_be_t124, constitutional_text_authority__positivist_reading, base_extractiveness, 124, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__positivist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cons_su_t31, constitutional_text_authority__positivist_reading, suppression_requirement, 31, 0.52).
narrative_ontology:measurement(cons_su_t62, constitutional_text_authority__positivist_reading, suppression_requirement, 62, 0.58).
narrative_ontology:measurement(cons_su_t93, constitutional_text_authority__positivist_reading, suppression_requirement, 93, 0.62).
narrative_ontology:measurement(cons_su_t124, constitutional_text_authority__positivist_reading, suppression_requirement, 124, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text_authority__positivist_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the constitutional_text_authority kernel family. The three readings (positivist, originalist, living constitutionalist) instantiate distinct constraints with different ε, beneficiaries, and victims. They are linked because each cites the same constitutional text as kernel but disputes the validity criterion. The positivist reading's formal-enactment criterion is the baseline; originalism adds historical fixation; living constitutionalism adds moral evolution. This decomposition follows the BGS pattern: upstream (formal enactment) → downstream (originalist fixation, living evolution).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text_authority__positivist_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
