% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__constitutional_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__constitutional_fidelity_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__constitutional_fidelity_reading
 *   human_readable: Qualified Immunity as Judicially Fabricated Doctrine (Constitutional Fidelity Reading)
 *   domain: constitutional_law/civil_rights/law_enforcement_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the constitutional_fidelity_reading of
 *   the qualified_immunity_doctrine kernel. It reads the doctrine as a
 *   judicial fabrication lacking any constitutional or statutory
 *   authorization — created in Pierson v. Ray (1967), hardened in Harlow v.
 *   Fitzgerald (1982), and expanded through Anderson, Saucier, and Pearson —
 *   that operates as an illegitimate constraint on constitutional rights
 *   enforcement regardless of its policy consequences. The doctrine extracts
 *   from both constitutional claimants (denied remedies for violations) and
 *   law enforcement officers (denied a legitimate legal framework governing
 *   their authority), while the judiciary expands its institutional power by
 *   authoritatively settling a question the Constitution assigns to Congress.
 *   The protective_scaffold_reading claims the doctrine enables vigorous
 *   policing; the accountability_void_reading claims it guarantees impunity.
 *   This reading claims both are secondary: the primary structural fact is
 *   the doctrine's illegitimate origin, which makes every downstream effect a
 *   consequence of constitutional infidelity rather than policy design.
 *
 * KEY AGENTS:
 *   - federal_judiciary: Primary beneficiary (institutional/arbitrage) — authors and expands doctrine, consolidating interpretive monopoly over Section 1983
 *   - state_appellate_courts: Secondary beneficiary (institutional/arbitrage) — replicate and entrench federal doctrine in state constitutional claims
 *   - constitutional_rights_claimants: Primary victim (powerless/trapped) — constitutional violations go unremedied; no legislative alternative exists
 *   - law_enforcement_officers: Secondary victim (organized/constrained) — denied clear legal boundaries; doctrine substitutes judicial improvisation for legislative guidance
 *   - congress: Excluded (institutional/analytical) — constitutional authority to define remedies ceded without acquiescence; legislative fixes repeatedly proposed and stalled
 *   - legal_academy: Observer (analytical/analytical) — documents doctrinal drift but lacks institutional leverage to alter trajectory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.78).
domain_priors:suppression_score(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.85).
domain_priors:theater_ratio(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__constitutional_fidelity_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__constitutional_fidelity_reading, "Qualified Immunity as Judicially Fabricated Doctrine (Constitutional Fidelity Reading)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__constitutional_fidelity_reading, "constitutional_law/civil_rights/law_enforcement_policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__constitutional_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__constitutional_fidelity_reading, '9c67b5b4-f314-4d9c-931c-b98e18ae5b9e').
narrative_ontology:cs_kernel_codification('9c67b5b4-f314-4d9c-931c-b98e18ae5b9e', fixed_text).
narrative_ontology:cs_authority_grounding('9c67b5b4-f314-4d9c-931c-b98e18ae5b9e', extraction).
narrative_ontology:cs_interpretation_layer_present('9c67b5b4-f314-4d9c-931c-b98e18ae5b9e').
narrative_ontology:cs_reading_relation('9c67b5b4-f314-4d9c-931c-b98e18ae5b9e', qualified_immunity_doctrine__protective_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('9c67b5b4-f314-4d9c-931c-b98e18ae5b9e', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_axiom('9c67b5b4-f314-4d9c-931c-b98e18ae5b9e', foundational, judicial_doctrine_requires_constitutional_or_statutory_authorization).
narrative_ontology:cs_axiom_status(judicial_doctrine_requires_constitutional_or_statutory_authorization, holdable).
narrative_ontology:cs_axiom_grounding('9c67b5b4-f314-4d9c-931c-b98e18ae5b9e', judicial_doctrine_requires_constitutional_or_statutory_authorization, deontological).
narrative_ontology:cs_axiom('9c67b5b4-f314-4d9c-931c-b98e18ae5b9e', foundational, section_1983_text_precludes_judicial_immunity_creation).
narrative_ontology:cs_axiom_status(section_1983_text_precludes_judicial_immunity_creation, holdable).
narrative_ontology:cs_axiom_grounding('9c67b5b4-f314-4d9c-931c-b98e18ae5b9e', section_1983_text_precludes_judicial_immunity_creation, conventional).
narrative_ontology:cs_reference_frame('9c67b5b4-f314-4d9c-931c-b98e18ae5b9e', section_1983_statutory_text).
narrative_ontology:cs_drift_state('9c67b5b4-f314-4d9c-931c-b98e18ae5b9e', post_pearson_callahan, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('9c67b5b4-f314-4d9c-931c-b98e18ae5b9e', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, state_appellate_courts).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_rights_claimants).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__constitutional_fidelity_reading, judicial_supremacy_doctrine).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__constitutional_fidelity_reading, separation_of_powers_violation_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors, expands, and administers the qualified immunity doctrine through Supreme Court precedent. Gains interpretive monopoly over Section 1983 remedies, displaces congressional authority, and claims finality on immunity questions. Can narrow or overrule doctrine at will (Pearson v. Callahan made the two-step sequence optional). No external accountability for doctrinal choices.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Replicate and entrench federal qualified immunity doctrine in state constitutional claims (via Howlett v. Rose incorporation). Gain similar interpretive authority over state civil rights enforcement. Structurally bound to follow Supreme Court lead but can expand doctrine further in state-law contexts.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, state_appellate_courts, beneficiary,
    institutional, generational, constrained, regional).

% Bring Section 1983 claims for constitutional violations (excessive force, unlawful search, due process). Face qualified immunity barrier at summary judgment: must identify 'clearly established' precedent with nearly identical facts. No legislative alternative forum; Congress has not overridden doctrine. State courts often apply same doctrine. Remedies systematically denied for entire categories of violations.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Receive immunity from damages liability — genuine benefit. But operate without clear legislative guidance on constitutional boundaries; doctrine substitutes judicial improvisation for democratic rulemaking. Training and policy must anticipate unpredictable 'clearly established' standards. Denied the legitimacy that comes from congressionally authorized authority. Police unions advocate for doctrine but also lobby for legislative clarity that courts preempt.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers, beneficiary).

% Holds constitutional authority under Section 5 of the Fourteenth Amendment to define remedies for constitutional violations. Has repeatedly proposed legislative fixes (Ending Qualified Immunity Act, Justice in Policing Act) that stall. Judicial doctrine claims finality on immunity questions, structurally locking Congress out of the remedial design space it constitutionally occupies.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, congress, excluded,
    institutional, generational, analytical, national).

% Documents doctrinal history, critiques the 'clearly established' standard, traces the doctrine's departure from statutory text and common law baselines. Produces the empirical and theoretical literature that maps the constraint's structure. Lacks institutional leverage to alter judicial trajectory; influence runs through amicus briefs, judicial clerkships, and long-term elite opinion shifts.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, legal_academy, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__constitutional_fidelity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No genuine coordination function. The doctrine does not solve a collective action problem; it displaces the constitutional coordination mechanism (congressional definition of remedies) with judicial improvisation. The protective_scaffold_reading claims coordination (enabling vigorous policing), but this reading holds that claim is a cover story for judicial power expansion — the actual coordination problem (clear rules for officers, reliable remedies for rights) is worsened, not solved.
% TRANSFER_FUNCTION: Moves constitutional remedies from rights-claimants to non-liability for officials, and moves interpretive authority over Section 1983 from Congress to the judiciary. The transfer is bidirectional: claimants lose damages and injunctive relief; officials lose democratically authorized legal boundaries; Congress loses its Section 5 remedial authority; judiciary gains all three.
% ABSENT_VOICES: Congress (constitutional remedial authority holder, structurally excluded by judicial finality claims). Victims of constitutional violations who never file suit because qualified immunity makes it futile (statistically larger than filed-claim population). State legislatures that could create independent state remedies but are preempted by federal doctrine incorporation. These voices are absent because the constraint's enforcement mechanism (stare decisis + judicial supremacy) actively suppresses their entry.
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished overnight, Section 1983 would revert to its statutory text: 'every person' liable for constitutional violations under color of law. Congress would face immediate pressure to legislate clear immunity rules (good faith, scope of duty, official capacity). Officers would demand legislative clarity. Courts would lose interpretive monopoly. The entire constitutional tort landscape would reorganize around democratic remedial design rather than judicial improvisation.
% FOUNDING_PROBLEM: Judicial concern (Pierson v. Ray, 1967) that Section 1983 would deter vigorous law enforcement by exposing officers to unpredictable liability for good-faith constitutional errors. The Court imported a common-law good-faith immunity that the enacting Congress had deliberately omitted.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (deterrence of vigorous policing) is attested by the Court itself in Pierson and Harlow. But the problem's status as 'dead' is corroborated by: (1) Harlow v. Fitzgerald (1982) eliminating the good-faith inquiry — if the problem were live, the subjective component would remain; (2) 40+ years of empirical research showing no deterrence effect on policing (Schwartz, Baude, etc.); (3) Congress's repeated failure to enact the immunity the Court claimed was necessary — if the problem were live, political branches would act; (4) police unions themselves lobbying for legislative clarity, not judicial immunity. No corroborating source outside the benefiting judiciary attests the founding problem remains live.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__constitutional_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__constitutional_fidelity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(qualified_immunity_doctrine__constitutional_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the doctrine systematically denies constitutional remedies without legislative authority — the transfer from rights-holders to non-liability is near-total for entire categories of violations. Suppression is very high (0.85) because the doctrine's persistence depends on stare decisis and the absence of congressional correction, not participant consent; claimants have no exit, officers have no clear rules, Congress is locked out by judicial finality claims. Theater ratio is low (0.22) because the doctrine's core function (immunizing officials) is real and actively enforced, not performative; the 'clearly established law' standard produces genuine (if perverse) adjudication. Accessibility collapse is moderate (0.45) because alternatives exist in theory (legislative reform, constitutional amendment) but are structurally blocked by the very doctrine that would need to be overturned. Resistance is high (0.72) from claimants, civil rights bar, some judges (dissents in Kisor, Ziglar, Baxter), and legislative proposals — but resistance is channeled into fora the doctrine itself controls.
 *
 * PERSPECTIVAL GAP:
 *   The federal_judiciary seat (agenda_setter, institutional power, arbitrage exit) computes as a beneficiary with negative effective extraction — the constraint subsidizes its interpretive authority. The constitutional_rights_claimants seat (payer, powerless, trapped) computes as a full target with maximal effective extraction — the constraint extracts their constitutional remedies entirely. The law_enforcement_officers seat (payer, organized, constrained) computes as a partial target: they gain immunity but lose legitimate legal framework, creating a corrupted coordination good. The engine computes this divergence from the structural data; the authored claim (snare) reflects the claimants' seat, not a unified classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared as federal_judiciary and state_appellate_courts: they gain interpretive monopoly over Section 1983, displacement of congressional authority, and institutional finality on immunity questions. Victims declared as constitutional_rights_claimants (primary, powerless/trapped) and law_enforcement_officers (secondary, organized/constrained): claimants lose remedies; officers lose legislative clarity and democratic legitimacy for their authority. Congress is excluded — it holds constitutional authority over remedies but is structurally locked out by judicial supremacy claims. The judiciary's arbitrage-grade exit (it can narrow or overrule at will) versus claimants' trapped exit (no alternative forum) drives the directionality asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (if any) was judicial concern about deterring vigorous law enforcement — but the doctrine has no statutory mandate, no congressional authorization, and no constitutional basis. Its mandate (if it ever had one) atrophied by 1982 when Harlow eliminated the good-faith inquiry and made immunity objective. The doctrine now persists purely through judicial inertia and institutional self-interest. The mandatrophy is resolved: the constraint has no legitimate founding mandate, only a judicially self-authorized persistence. This is not a scaffold that outlived its purpose — it never had a legitimate purpose to begin with.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the qualified_immunity_doctrine kernel, and does it foreclose or coexist with the protective_scaffold_reading and accountability_void_reading?',
    'Cross-reading structural comparison: map each reading''s beneficiary/victim sets, claimed types, and axiomatic commitments; identify which structural elements differ and whether differences are logical contradictions or political disagreements.',
    'If this reading forecloses protective_scaffold_reading, no single legal framework can hold both. If they coexist, the kernel remains an active site of contestation with no structural resolution. The engine''s inferred_coupling_protocol uses this to track kernel-internal fragmentation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Kernel membership and structural relationship to sibling readings').

omega_variable(
    judiciary_as_beneficiary_ambiguity,
    'Does the judiciary benefit from qualified immunity as institutional power expansion, or does it merely administer a doctrine imposed by political branches?',
    'Trace doctrinal history: who created the doctrine (Pierson v. Ray, Harlow v. Fitzgerald), who expanded it (Anderson v. Creighton, Saucier v. Katz, Pearson v. Callahan), and who benefits from the resulting institutional position. Compare with legislative acquiescence pattern.',
    'If judiciary is the primary beneficiary, the constraint is a self-authorizing power grab (snare with institutional beneficiary). If political branches are the true beneficiaries, judiciary is a captive administrator (different directional structure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judiciary_as_beneficiary_ambiguity, empirical, 'Whether judicial institutional interest drives doctrine or judicial passivity enables political extraction').

omega_variable(
    epsilon_indeterminacy_claim,
    'Is base extractiveness genuinely indeterminate because the legal framework itself is contested, or can it be measured against a stable referent?',
    'Test referent stability: measure extraction against (a) constitutional text as referent, (b) common law baseline as referent, (c) Section 1983 statutory purpose as referent. If all three yield materially different ε values, the kernel itself fragments the referent — confirming indeterminacy.',
    'If ε is referent-dependent, this constraint cannot be a single ε-invariant story — it must decompose further per DP-001 (ε-invariance principle). If ε is stable across referents, the reading''s claim of indeterminacy is a framing artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_indeterminacy_claim, conceptual, 'Whether the reading''s own claim of ε-indeterminacy holds under referent-disambiguation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__constitutional_fidelity_reading, 1967, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qid_cfr_tr_t1967, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1967, 0.08).
narrative_ontology:measurement(qid_cfr_tr_t1982, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1982, 0.12).
narrative_ontology:measurement(qid_cfr_tr_t1986, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1986, 0.15).
narrative_ontology:measurement(qid_cfr_tr_t2001, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2001, 0.18).
narrative_ontology:measurement(qid_cfr_tr_t2009, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2009, 0.2).
narrative_ontology:measurement(qid_cfr_tr_t2020, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2020, 0.22).

% Extraction over time
narrative_ontology:measurement(qid_cfr_be_t1967, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1967, 0.35).
narrative_ontology:measurement(qid_cfr_be_t1982, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1982, 0.52).
narrative_ontology:measurement(qid_cfr_be_t1986, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1986, 0.61).
narrative_ontology:measurement(qid_cfr_be_t2001, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2001, 0.68).
narrative_ontology:measurement(qid_cfr_be_t2009, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2009, 0.74).
narrative_ontology:measurement(qid_cfr_be_t2020, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2020, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(qid_cfr_su_t1967, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1967, 0.45).
narrative_ontology:measurement(qid_cfr_su_t1982, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1982, 0.62).
narrative_ontology:measurement(qid_cfr_su_t1986, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1986, 0.71).
narrative_ontology:measurement(qid_cfr_su_t2001, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2001, 0.78).
narrative_ontology:measurement(qid_cfr_su_t2009, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2009, 0.82).
narrative_ontology:measurement(qid_cfr_su_t2020, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2020, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__constitutional_fidelity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.12).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, section_1983_remedial_structure).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, monell_municipal_liability).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_tort_doctrine).

% DUAL FORMULATION NOTE:
% This is one of three constraints in the qualified_immunity_doctrine kernel family. constitutional_fidelity_reading claims the doctrine is illegitimate ab initio (snare with judiciary beneficiary). protective_scaffold_reading claims it is a necessary coordination mechanism (rope/tangled_rope with officer beneficiary). accountability_void_reading claims it is a designed extraction mechanism (snare with officer/police_union beneficiary). All three share the same doctrinal text but instantiate different constraints with different ε, different beneficiary/victim sets, and different claimed types — linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity_doctrine__constitutional_fidelity_reading, institutional, 0.15).
constraint_indexing:directionality_override(qualified_immunity_doctrine__constitutional_fidelity_reading, powerless, 0.95).
constraint_indexing:directionality_override(qualified_immunity_doctrine__constitutional_fidelity_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
