% ============================================================================
% CONSTRAINT STORY: us_constitution_text__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__originalist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: us_constitution_text__originalist_reading
 *   human_readable: Originalist Constitutional Interpretation (Ratification-Fixed Meaning)
 *   domain: legal/constitutional/interpretive
 *
 * SUMMARY:
 *   Originalism as a constitutional interpretive regime claims that the
 *   Constitution's meaning is fixed at the moment of ratification and
 *   interpretation must recover the public understanding of the text at that
 *   historical moment. This reading presents itself as a constraint on
 *   judicial discretion — judges are bound by evidence, not policy. However,
 *   the extraction analysis reveals a tangled structure: originalism does
 *   solve a genuine coordination problem (binding judges to law rather than
 *   preference), but it simultaneously enables systematic extraction from
 *   unenumerated-rights claimants and adaptive-amendment proponents by
 *   suppressing non-originalist interpretive pathways. The constraint
 *   persists because a powerful conservative legal movement has captured key
 *   institutional seats (the judiciary, law schools, executive branch
 *   appointments) and actively enforces the boundary against competing
 *   readings.
 *
 * KEY AGENTS:
 *   - originalist_judicial_faction: Agenda-setter; controls the evidentiary standard and binding interpretive authority
 *   - conservative_legal_movement: Beneficiary; benefits from narrowed rights recognition and federalism; has invested decades in judicial institutional capture
 *   - unenumerated_rights_claimants: Victims; face suppression of adaptive interpretation paths; must find Founding-era evidence or accept non-recognition
 *   - democratic_amendment_process: Victim (secondary); the high cost of formal amendment becomes the enforced path for all change
 *   - living_constitutionalist_faction: Excluded; powerful voices remain outside the binding judicial frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__originalist_reading, 0.68).
domain_priors:suppression_score(us_constitution_text__originalist_reading, 0.82).
domain_priors:theater_ratio(us_constitution_text__originalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__originalist_reading, "Originalist Constitutional Interpretation (Ratification-Fixed Meaning)").
narrative_ontology:topic_domain(us_constitution_text__originalist_reading, "legal/constitutional/interpretive").

domain_priors:requires_active_enforcement(us_constitution_text__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__originalist_reading, '27e5232b-2e95-4a57-ab6f-6dcb5d4f19de').
narrative_ontology:cs_kernel_codification('27e5232b-2e95-4a57-ab6f-6dcb5d4f19de', fixed_text).
narrative_ontology:cs_authority_grounding('27e5232b-2e95-4a57-ab6f-6dcb5d4f19de', extraction).
narrative_ontology:cs_interpretation_layer_present('27e5232b-2e95-4a57-ab6f-6dcb5d4f19de').
narrative_ontology:cs_reading_relation('27e5232b-2e95-4a57-ab6f-6dcb5d4f19de', us_constitution_text__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('27e5232b-2e95-4a57-ab6f-6dcb5d4f19de', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('27e5232b-2e95-4a57-ab6f-6dcb5d4f19de', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('27e5232b-2e95-4a57-ab6f-6dcb5d4f19de', meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('27e5232b-2e95-4a57-ab6f-6dcb5d4f19de', foundational, original_public_meaning_determines_application).
narrative_ontology:cs_axiom_status(original_public_meaning_determines_application, holdable).
narrative_ontology:cs_axiom_grounding('27e5232b-2e95-4a57-ab6f-6dcb5d4f19de', original_public_meaning_determines_application, empirically_contingent).
narrative_ontology:cs_axiom('27e5232b-2e95-4a57-ab6f-6dcb5d4f19de', secondary, post_ratification_practice_irrelevant_except_as_meaning_evidence).
narrative_ontology:cs_axiom_status(post_ratification_practice_irrelevant_except_as_meaning_evidence, holdable).
narrative_ontology:cs_axiom_grounding('27e5232b-2e95-4a57-ab6f-6dcb5d4f19de', post_ratification_practice_irrelevant_except_as_meaning_evidence, deontological).
narrative_ontology:cs_reference_frame('27e5232b-2e95-4a57-ab6f-6dcb5d4f19de', ratification_moment_meaning).
narrative_ontology:cs_drift_state('27e5232b-2e95-4a57-ab6f-6dcb5d4f19de', contemporary_post_2020_judicial_majority, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('27e5232b-2e95-4a57-ab6f-6dcb5d4f19de', '').
narrative_ontology:cs_kernel_id(us_constitution_text__originalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, federalist_institutional_framework).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, adaptive_constitutional_amendment_proponents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, democratic_amendment_process).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges and scholars who author and enforce originalist doctrine in constitutional adjudication. They set the evidentiary standard (historical dictionaries, Founding-era debates, common law references) and decide which post-ratification practice counts as evidence of original meaning. They enforce the constraint by rejecting non-originalist readings as illegitimate despite broader constitutional support.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, originalist_judicial_faction, agenda_setter,
    institutional, generational, mobile, national).

% Conservative legal theorists, institutions (Federalist Society, Heritage Foundation), and a significant faction of elected officials benefit from originalism's tendency to narrow the scope of recognized constitutional rights and to prioritize federalism and enumerated powers over adaptive interpretation. They have invested heavily in originalist legal education and institutional capture of the judiciary.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, conservative_legal_movement, beneficiary,
    organized, generational, mobile, national).

% Groups seeking recognition of rights (privacy, bodily autonomy, due process protection for non-enumerated interests) grounded in evolving social understanding but lacking clear textual or Founding-era pedigree. They must navigate the originalist gatekeeping function: either find evidence their right was understood in 1787/1868, or accept non-recognition. The suppression mechanism prevents them from advancing the adaptive interpretation path without frontal constitutional amendment.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, unenumerated_rights_claimants, payer,
    moderate, biographical, constrained, national).

% The constitutional doctrine of limited federal powers and reserved state sovereignty. Originalism vindicates this framework by treating the enumeration of federal powers as a binding constraint independent of contemporary necessity or social consensus. Non-originalist readings tend to read the enumerated powers more expansively.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, federalist_institutional_framework, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(us_constitution_text__originalist_reading, federalist_institutional_framework).

% Judges, scholars, and legal advocates who argue constitutional meaning evolves with society. They are excluded from the originalist interpretive community despite remaining live voices in academic and judicial discourse. They would argue that principles like equal protection should adapt to contemporary discrimination mechanisms, and that fundamental rights should not be confined to 18th-century understandings.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, living_constitutionalist_faction, excluded,
    powerful, generational, mobile, national).

% The formal amendment mechanism (two-thirds Congress, three-fourths states) is the institutionally designated path for constitutional change. Originalism suppresses informal adaptation by judicial doctrine, which effectively raises the cost of constitutional change by channeling all innovation attempts through the amendment gauntlet. This bears a cost: political will for formal amendment is higher than willingness to live with interpretive drift.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, democratic_amendment_process, payer,
    powerful, generational, constrained, national).

% Law professors and constitutional historians document the competing readings, historical evidence, and methodological disputes. They cannot resolve the kernel dispute but produce the evidence-base originalist adjudication claims to use.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, academic_constitutional_scholarship, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:fixing_cost_class(us_constitution_text__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes constitutional meaning at a bounded historical moment so that judges apply law rather than enact policy preferences. Enables predictability and democratic legitimacy: the Framers' choice is the law; judicial role is recovery, not revision. Coordinates judicial behavior around a shared (if contested) evidentiary standard.
% TRANSFER_FUNCTION: Moves political and institutional authority from judicial adapters of constitutional principle to those who benefit from fixed, narrow readings of enumerated rights and powers. Systematically channels rights claims through the formal amendment process rather than judicial recognition, raising the cost and slowing the pace of rights expansion.
% ABSENT_VOICES: Living constitutionalists, democratic theorists arguing for adaptive judicial responsiveness, and the unenumerated-rights claimants would argue that fixing meaning at ratification freezes the Constitution in the hands of the Founding generation and forecloses democratic evolution. They are present in scholarship but excluded from the binding interpretive authority within originalist judicial frames.
% DISAPPEARANCE_RATIONALE: If originalism as an interpretive regime disappeared from constitutional law, the scope of recognized constitutional protections would expand through adaptive judicial reasoning. Unenumerated rights claims would succeed more frequently. Federal regulatory authority would be read more expansively. The distribution of institutional authority between courts and legislatures would shift: judges would recover amendment-like authority to adapt constitutional principles. Constitutional law would become more democratic but also more contested.
% FOUNDING_PROBLEM: Post-Reconstruction constitutional jurisprudence in the mid-20th century saw judges reading constitutional provisions to justify policy preferences (economic substantive due process, then expansive individual rights) without clear textual or historical grounding. Originalism was developed as a constraint to bind judges to law rather than policy.
% FOUNDING_PROBLEM_CORROBORATION: Originalist theorists (Scalia, Thomas, contemporary Federalist Society scholars) attest the problem of judicial lawmaking persists and originalism is the cure. Critics outside the originalist camp (progressive legal scholars, living constitutionalists, legal historians) attest that mid-20th-century jurisprudence had real problems, but originalism does not solve them — it substitutes one judge-dependent reconstruction (historical meaning) for another, and it systematically privileges certain interest groups. Legislative testimony and constitutional history scholarship from non-originalist sources support the critique.
narrative_ontology:disappearance_verdict(us_constitution_text__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_text__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__originalist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.68 over the interval, reflecting the increasing consolidation of originalist institutional dominance in the judiciary (especially post-2020 appointments). Suppression remains high (0.82 at interval end) because the constraint's persistence depends not on participant preference but on active judicial gatekeeping — judges reject non-originalist arguments as illegitimate despite their historical pedigree and democratic support in some constituencies. Theater_ratio rises to 0.41, indicating that a growing share of originalist enforcement activity involves performative historical reconstruction (selective dictionaries, contested Founding-era glosses) defending the framework itself rather than pure application of binding principle. Measurements are authored on one shared grid: every metric is valued at every time point (t=0,5,10,15,20,25,30).
 *
 * PERSPECTIVAL GAP:
 *   The originalist judicial faction (agenda-setter) experiences this as a legitimate constraint on judicial discretion — binding judges to law. Unenumerated-rights claimants experience it as suppression: their rights claims are excluded by an evidentiary gate they did not author and cannot overcome without Founding-era evidence they cannot produce. The democratic amendment process experiences it as a cost-raising mechanism: change requires supermajority consensus rather than evolving judicial interpretation. The engine computes per-seat directionality: high d (near-target end) for claimants facing exclusion, low d (near-beneficiary end) for the conservative legal movement capturing institutional power, symmetric for the amendment process (both coordinated and harmed).
 *
 * DIRECTIONALITY LOGIC:
 *   Conservative legal movement: role=beneficiary, power=organized, exit_options=mobile (they can advocate their reading but have institutionalized it deeply — their mobility is real but bounded). Computed d approaches 0.0 (subsidy). Unenumerated-rights claimants: role=payer, power=moderate, exit_options=constrained (they cannot exit constitutional law; they can only hammer on the evidentiary gate or seek formal amendment — high cost both directions). Computed d approaches 1.0 (full target). Democratic amendment process: role=payer (bears the cost of raised amendment bar), power=powerful (the formal amendment process has structural authority), exit_options=constrained (the amendment bar is the amendment bar — cannot escape it). Computed d near 0.5 (symmetric: both coordinated by originalism and harmed by raised amendment costs).
 *
 * MANDATROPHY ANALYSIS:
 *   Originalism's founding problem — mid-20th-century judicial lawmaking without historical grounding — remains partially live. However, originalism itself generates new extraction: it suppresses adaptive interpretation and raises amendment costs for rights not covered by Founding-era evidence. This is not mandatrophy in the classical sense (extinct founding problem), but rather *mandate drift*: the solution (originalist constraint) has created a new problem (unenumerated-rights suppression) not present in the original problem statement. The constraint persists not by solving its founding mandate, but by institutional capture and gatekeeping against competing readings. Theater emerges as originalist scholars invest increasing effort in reconstructing historical meaning in contested cases (striking down unenumerated rights, constraining federal power) where the historical record is ambiguous or genuinely supports multiple readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_meaning_recovery,
    'How determinate is the ''original public meaning'' of contested constitutional text? Is historical evidence sufficient to yield a single correct reading, or does Founding-era ambiguity license modern judicial reconstruction?',
    'Historiographic analysis of Founding-era sources; examination of how frequently originalist judges reach divergent conclusions on the same text; study of whether historical ambiguity correlates with judicial ideological splits.',
    'If historical evidence is genuinely indeterminate on significant cases, originalism reduces to judge-dependent reconstruction dressed in historical language — the constraint becomes theater (high theater_ratio), not binding law. If historical evidence is determinate, originalism binds judicial discretion as claimed and suppression is a legitimate cost of the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_meaning_recovery, empirical, 'Whether historical meaning is sufficiently determinate to constrain judicial discretion or whether it licenses judicial reconstruction.').

omega_variable(
    foundational_axiom_foreclosure,
    'Does originalism''s core axiom (''meaning_fixed_at_ratification'') logically foreclose the living constitutionalist axiom (''meaning_evolves_with_society'') within a single coherent framework, or do they represent different commitments that parties can hold simultaneously in different institutional roles?',
    'Philosophical analysis of the logical structure of the axioms; examination of whether a judge could hold both principles (e.g., meaning is fixed for purposes of judicial review, but evolves for purposes of statutory interpretation or non-binding guidance).',
    'True logical foreclosure (genuine contradiction) supports the classification of originalism as a reading that makes living constitutionalism structurally impossible. Coexistence supports classification as competing readings held by different factions. This determines the reading_relations value: forecloses vs. coexists_with.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foundational_axiom_foreclosure, conceptual, 'Whether originalism and living constitutionalism are logically incompatible axioms or compatible commitments held in different institutional contexts.').

omega_variable(
    institutional_capture_stability,
    'Is originalism''s increasing institutional dominance (judicial appointments, law school hiring, executive branch influence) stable, or does it depend on sustained political investment by conservative legal movement?',
    'Long-term observation of judicial turnover, law school hiring trends, and political support for originalist appointments; natural experiments from jurisdictions where originalist dominance weakens.',
    'High stability implies originalism is entrenched as a permanent constitutional settlement and extraction from adaptive-interpretation paths is structural. Low stability implies extraction is contingent on ongoing political power concentration and could reverse with electoral shifts or institutional change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_capture_stability, empirical, 'Whether originalist institutional dominance is self-sustaining or dependent on political alignment.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.82) of adaptive interpretation structural (external gatekeeping by originalist judges rejecting non-originalist arguments as illegitimate), or partly internalized (legal scholars and advocates have adopted originalist evidentiary standards as legitimate, even when they disagree with specific applications)?',
    'Qualitative analysis of legal scholarship: how frequently do non-originalist scholars argue within originalist frameworks vs. arguing for wholesale replacement of the framework? Behavioral test: if originalist judicial gatekeeping were removed, would adaptive interpretation immediately flourish or would it take time for alternative evidentiary standards to legitimize?',
    'If suppression is purely structural, removing it (e.g., through judicial turnover) would restore adaptive interpretation quickly. If suppression is partly internalized, the constraint would persist through intellectual inertia even after external gatekeeping weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of adaptive interpretation is structural gatekeeping or internalized intellectual constraint.').

omega_variable(
    reading_instantiation_ambiguity,
    'Does this constraint instantiate originalism as an invariant interpretive principle (''meaning does not evolve''), or does it instantiate one particular institutional settlement where originalism has captured certain seats?',
    'Examine whether the constraint-as-authored describes the logical structure of originalism (a principle that binds all judges, regardless of ideology) or the institutional fact (conservative judges have adopted originalism while progressive judges have not, and conservative judges have institutional power).',
    'If the constraint is purely about the principle, the story should show low extraction (just binding law). If the constraint is about the institutional settlement, it correctly shows high extraction (one movement capturing seats). This is a framing ambiguity, not a measurement ambiguity — the authored metrics are correct, but the constraint''s identity is about whether it describes principle or power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_instantiation_ambiguity, conceptual, 'Whether this constraint represents originalism as an invariant principle or originalism as an institutional settlement captured by conservative movement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__originalist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_text__originalist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t5, us_constitution_text__originalist_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(us_c_tr_t5, observed).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_text__originalist_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(us_c_tr_t10, observed).
narrative_ontology:measurement(us_c_tr_t15, us_constitution_text__originalist_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(us_c_tr_t15, observed).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_text__originalist_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(us_c_tr_t20, observed).
narrative_ontology:measurement(us_c_tr_t25, us_constitution_text__originalist_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(us_c_tr_t25, observed).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_text__originalist_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(us_c_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_text__originalist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t5, us_constitution_text__originalist_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(us_c_be_t5, observed).
narrative_ontology:measurement(us_c_be_t10, us_constitution_text__originalist_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(us_c_be_t10, observed).
narrative_ontology:measurement(us_c_be_t15, us_constitution_text__originalist_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(us_c_be_t15, observed).
narrative_ontology:measurement(us_c_be_t20, us_constitution_text__originalist_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(us_c_be_t20, observed).
narrative_ontology:measurement(us_c_be_t25, us_constitution_text__originalist_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(us_c_be_t25, observed).
narrative_ontology:measurement(us_c_be_t30, us_constitution_text__originalist_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(us_c_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_text__originalist_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t5, us_constitution_text__originalist_reading, suppression_requirement, 5, 0.71).
narrative_ontology:measurement_basis(us_c_su_t5, observed).
narrative_ontology:measurement(us_c_su_t10, us_constitution_text__originalist_reading, suppression_requirement, 10, 0.76).
narrative_ontology:measurement_basis(us_c_su_t10, observed).
narrative_ontology:measurement(us_c_su_t15, us_constitution_text__originalist_reading, suppression_requirement, 15, 0.79).
narrative_ontology:measurement_basis(us_c_su_t15, observed).
narrative_ontology:measurement(us_c_su_t20, us_constitution_text__originalist_reading, suppression_requirement, 20, 0.81).
narrative_ontology:measurement_basis(us_c_su_t20, observed).
narrative_ontology:measurement(us_c_su_t25, us_constitution_text__originalist_reading, suppression_requirement, 25, 0.82).
narrative_ontology:measurement_basis(us_c_su_t25, observed).
narrative_ontology:measurement(us_c_su_t30, us_constitution_text__originalist_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement_basis(us_c_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_text__originalist_reading, 0.16).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of 'us_constitution_text' kernel. The constraint family decomposes the single natural-language concept 'Constitutional interpretation' into three structurally distinct claims with different ε values and beneficiary/victim structures. The originalist reading (this story) enforces meaning-fixation and high suppression of adaptive interpretation (high extraction, high suppression). The living constitutionalist reading permits meaning-evolution and lower suppression of adaptive claims (lower extraction, lower suppression). The positivist reading focuses on enactment validity rather than substantive meaning (orthogonal to both). These are not the same constraint viewed from different angles — their ε values, beneficiary structures, and resistance patterns differ substantially. They are linked via network.affects_constraints because originalist institutional dominance creates structural pressure on living constitutionalist readings (influences) and vice versa.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
