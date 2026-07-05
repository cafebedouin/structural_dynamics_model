% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__liberal_due_process_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__liberal_due_process_reading, []).

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
 *   constraint_id: magna_carta_clause_39__liberal_due_process_reading
 *   human_readable: Clause 39 as Universal Due Process Guarantee (Liberal Reading)
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested Clause 39 kernel:
 *   the liberal due-process reading, which treats the 1215 text's guarantee
 *   against imprisonment or dispossession 'except by the lawful judgment of
 *   his peers or the law of the land' as the origin of a universal,
 *   individual-rights-based constraint on arbitrary state power. Under this
 *   reading, Clause 39 is not a narrow feudal privilege (the
 *   feudal_prerogative_reading) or a historically bounded limitation on
 *   specific 1215 royal abuses (the originalist_limitation_reading) — it is a
 *   foundational proto-constitutional guarantee whose scope has properly
 *   expanded to cover all citizens against all forms of arbitrary executive
 *   action, from Petition of Right through Habeas Corpus Act, Fourteenth
 *   Amendment due process, and modern administrative law. The three readings
 *   are separate constraints with separate ε values, per the ε-invariance
 *   principle; this file addresses only the liberal reading.
 *
 * KEY AGENTS:
 *   - rights_bearing_citizens: primary beneficiary (powerless/trapped) — invoke the guarantee against state action
 *   - constitutional_courts: agenda_setter (institutional/analytical) — administer and expand the doctrine
 *   - executive_branch_discretion: primary target (powerful/constrained) — bears the constraint on summary action
 *   - national_security_apparatus: secondary target (institutional/constrained) — operational friction from due-process review
 *   - originalist_scholars: excluded voice (organized/mobile) — contest the reading's historical accuracy
 *   - legal_historians: analytical observer (analytical/analytical) — trace the doctrine's expansion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, 0.71).
domain_priors:suppression_score(magna_carta_clause_39__liberal_due_process_reading, 0.62).
domain_priors:theater_ratio(magna_carta_clause_39__liberal_due_process_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__liberal_due_process_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__liberal_due_process_reading, "Clause 39 as Universal Due Process Guarantee (Liberal Reading)").
narrative_ontology:topic_domain(magna_carta_clause_39__liberal_due_process_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__liberal_due_process_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__liberal_due_process_reading, '8b311b70-b234-42b9-9235-68147b68b126').
narrative_ontology:cs_kernel_codification('8b311b70-b234-42b9-9235-68147b68b126', fixed_text).
narrative_ontology:cs_authority_grounding('8b311b70-b234-42b9-9235-68147b68b126', lineage).
narrative_ontology:cs_interpretation_layer_present('8b311b70-b234-42b9-9235-68147b68b126').
narrative_ontology:cs_reading_relation('8b311b70-b234-42b9-9235-68147b68b126', magna_carta_clause_39__feudal_prerogative_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b311b70-b234-42b9-9235-68147b68b126', magna_carta_clause_39__originalist_limitation_reading, influences).
narrative_ontology:cs_axiom('8b311b70-b234-42b9-9235-68147b68b126', foundational, individual_rights_precede_and_bind_sovereign_authority).
narrative_ontology:cs_axiom_status(individual_rights_precede_and_bind_sovereign_authority, holdable).
narrative_ontology:cs_axiom_grounding('8b311b70-b234-42b9-9235-68147b68b126', individual_rights_precede_and_bind_sovereign_authority, deontological).
narrative_ontology:cs_axiom('8b311b70-b234-42b9-9235-68147b68b126', foundational, constitutional_meaning_expands_through_principled_precedent).
narrative_ontology:cs_axiom_status(constitutional_meaning_expands_through_principled_precedent, holdable).
narrative_ontology:cs_axiom_grounding('8b311b70-b234-42b9-9235-68147b68b126', constitutional_meaning_expands_through_principled_precedent, conventional).
narrative_ontology:cs_reference_frame('8b311b70-b234-42b9-9235-68147b68b126', baronial_feudal_settlement_1215).
narrative_ontology:cs_drift_state('8b311b70-b234-42b9-9235-68147b68b126', post_incorporation_administrative_state_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('8b311b70-b234-42b9-9235-68147b68b126', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, rights_bearing_citizens).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, constitutional_courts).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, liberal_legal_professions).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, executive_branch_discretion).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, national_security_apparatus).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, administrative_agencies).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, universal_due_process_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, rule_of_law_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individual persons who invoke Clause 39's descendant guarantees (due process, habeas corpus, judicial review of detention) against state action. They cannot exit the jurisdiction of the state that governs them, so the guarantee's strength determines whether arbitrary detention or seizure can be checked at all. Under this reading, every citizen — not just propertied freemen — is covered.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, rights_bearing_citizens, beneficiary,
    powerless, civilizational, trapped, national).

% Courts that read Clause 39 expansively as a living guarantee against arbitrary power, extending it via incorporation and interpretation well beyond feudal land tenure disputes. They administer the doctrine, expand its reach through precedent, and derive institutional authority and legitimacy from being the guarantors of this universal right.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__liberal_due_process_reading, constitutional_courts, beneficiary).

% Constitutional lawyers, civil liberties organizations, and legal academics whose professional standing and case flow depend on treating Clause 39 as the ancestral source of expansive due process protections. They benefit from the doctrine's broad, contestable scope, which generates litigation and scholarship.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, liberal_legal_professions, beneficiary,
    organized, generational, mobile, national).

% Executive officials whose capacity for summary action — detention, seizure, emergency measures — is constrained by the requirement to justify action through legal process. Under the expansive reading, discretion that would otherwise be available in a crisis is checked, and officials bear the cost of litigation, delay, and reversal when courts find due process violations.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, executive_branch_discretion, payer,
    powerful, immediate, constrained, national).

% Intelligence and security agencies that argue the expansive reading hampers legitimate state functions (surveillance, indefinite detention of suspected threats) by importing universal individual-rights language into contexts the 1215 text never contemplated. They pay in operational friction and adverse judicial rulings.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, national_security_apparatus, payer,
    institutional, immediate, constrained, national).

% Regulatory bodies whose adjudicative and enforcement actions (license revocation, benefits termination, deportation proceedings) are subject to due-process review derived from this reading of Clause 39. They must build procedural safeguards into routine administration, which the liberal reading treats as a rights guarantee and administrators often experience as an extraction of operational speed.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, administrative_agencies, payer,
    institutional, biographical, constrained, national).

% Historians and originalist jurists who would object that the liberal reading anachronistically projects Enlightenment individual-rights theory onto a 1215 baronial settlement concerned with feudal tenure and specific royal abuses. They are present in academic debate but structurally marginal in the courts that actually apply the doctrine.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, originalist_scholars, excluded,
    organized, generational, mobile, national).

% Scholars who trace how Clause 39's language ('no free man shall be... except by the lawful judgment of his peers or the law of the land') was progressively reinterpreted across centuries — from feudal procedural guarantee to universal constitutional principle — and can document the successive reinterpretations without being parties to the current dispute.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__liberal_due_process_reading, diffuse).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__liberal_due_process_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, citable textual anchor that lets courts and litigants coordinate expectations about limits on state power — a common reference point that different actors across centuries can invoke without renegotiating the underlying principle from scratch each time.
% TRANSFER_FUNCTION: Moves discretionary latitude away from executive and administrative actors and toward courts and rights-claimants: procedural burden and litigation risk flow to the state; predictability and a check against arbitrary action flow to individuals.
% ABSENT_VOICES: Originalist historians who would contest that the 1215 text supports universal individual rights are present in scholarly debate but structurally absent from the courts and doctrine-generating institutions that actually apply the expansive reading; their objection is documented but not adjudicated.
% DISAPPEARANCE_RATIONALE: If the liberal due-process reading of Clause 39 were repudiated overnight, the doctrinal lineage running through Magna Carta to modern due-process and habeas corpus jurisprudence would lose a foundational citation, and courts would need to rebuild the same protections on other doctrinal grounds (natural rights theory, international human rights instruments) or the protections would weaken during the transition — executive and administrative actors would gain discretion immediately.
% FOUNDING_PROBLEM: Barons in 1215 sought to constrain King John's arbitrary seizure of property and imprisonment without judicial process, within a feudal hierarchy of specific privileged obligations.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the beneficiary set (constitutional courts, civil liberties advocates) attest that the 1215 text addressed narrow baronial grievances, not universal individual rights; this corroboration comes from historical scholarship independent of the modern litigation apparatus that relies on the expansive reading, and it directly disputes whether the 'founding problem' this reading claims to solve is the one Clause 39 actually addressed.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__liberal_due_process_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__liberal_due_process_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__liberal_due_process_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_clause_39__liberal_due_process_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__liberal_due_process_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness under this reading is high (0.71 by 2025) because the expansive doctrine transfers substantial discretionary capacity away from executive and administrative actors — every act of summary detention, seizure, or agency adjudication must now clear a procedural threshold whose scope has grown far beyond 1215's baronial concerns. Suppression is elevated (0.62) because maintaining the expansive reading requires active judicial enforcement against executive resistance in every generation (wartime detention cases, administrative law challenges, national security litigation). Theater ratio is moderate (0.4): a genuine coordination function exists (a stable textual anchor for due-process claims), but a rising share of invocation is rhetorical — citing Magna Carta as ancient pedigree rather than as operative legal reasoning, especially in political rhetoric rather than case law.
 *
 * PERSPECTIVAL GAP:
 *   From the courts' and citizens' seats, this is coordination: a shared, ancient guarantee against arbitrary power that stabilizes expectations across the state-citizen relationship. From the executive and security-apparatus seats, the same doctrine looks like accumulating extraction — an ever-expanding set of procedural constraints justified by appeal to a 13th-century baronial charter whose original scope was far narrower. The tangled_rope claim reflects that both a genuine coordination function (predictable limits on state power) and asymmetric extraction (discretion moved from executive to judiciary/citizens) are present simultaneously, sustained by active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights-bearing citizens and the courts/legal professions that administer and elaborate the doctrine are structural beneficiaries — they gain protection or professional/institutional standing from the expansive interpretation. Executive discretion, security agencies, and administrative bodies are structural targets — the same doctrinal expansion that protects citizens constrains their operational latitude, and this cost has grown as due-process doctrine has been extended into new domains (administrative hearings, immigration proceedings, security detentions) the 1215 barons never envisioned.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constraining a specific medieval king's specific abuses of feudal prerogative) is largely dead in its original form — no baron today negotiates relief payments or wardship disputes under Clause 39. But the reading's proponents argue the underlying problem (arbitrary state power) is perpetually live, just manifesting in new forms (administrative detention, agency adjudication) — hence founding_problem_status is authored as contested rather than dead. This prevents the doctrine from being dismissed as pure zombie-mandate theater while also flagging that its claimed continuity with 1215 is itself doing significant legitimating work that the originalist and feudal readings dispute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anachronistic_projection_ambiguity,
    'Does the liberal due-process reading correctly identify a genuine continuity of principle from 1215 to modern constitutional law, or does it anachronistically project Enlightenment and post-Enlightenment individual-rights theory onto a text that addressed narrow feudal tenure disputes?',
    'Close textual and historical analysis of the 1215 context (what ''free man,'' ''peers,'' and ''law of the land'' meant to the barons and King John) versus tracing the actual citational and doctrinal chain through Coke, Blackstone, and American constitutional incorporation to determine whether each link represents genuine principle-extension or a rhetorical repurposing.',
    'If the doctrine is substantially a later invention retrofitted onto Magna Carta for legitimating pedigree, the extractiveness attributed to ''Clause 39 itself'' should properly be relocated to the later doctrinal innovations (Coke''s 17th-century interpretation, 14th Amendment due process) rather than the 1215 clause, altering where in the network the extraction is actually located.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anachronistic_projection_ambiguity, conceptual, 'Whether the liberal reading''s claimed historical continuity is genuine or retrofitted.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Which of the three readings (feudal_prerogative, liberal_due_process, originalist_limitation) best captures what Clause 39 ''is,'' and is that even a well-formed question given that the readings serve different institutional actors'' present-day legitimation needs?',
    'This is likely irresolvable by further historical evidence alone — the kernel is genuinely under-determined, and each reading is sustained by a different community of practice (constitutional courts and civil libertarians for the liberal reading, legal historians for the originalist reading, comparative feudal-law scholars for the feudal reading) with different institutional stakes in the outcome.',
    'If the liberal reading is understood as one contestable interpretation among three rather than ''the'' meaning of Clause 39, its extractiveness score should be read as the cost imposed by this SPECIFIC interpretive choice, not as an objective property of the historical clause itself — which is exactly what the network linkage across three sibling stories is meant to preserve.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether kernel-level meaning is even determinable, or only reading-relative.').

omega_variable(
    beneficiary_capture_of_doctrine,
    'Do constitutional courts and the liberal legal profession sustain the expansive reading partly because it generates litigation, doctrine, and institutional relevance for themselves, independent of whether it best protects citizens?',
    'Compare case outcomes and doctrinal expansion patterns against measures of actual due-process protection delivered to citizens (e.g., success rates in habeas petitions, administrative appeals) to see whether doctrinal growth tracks protective outcomes or professional/institutional self-interest.',
    'If doctrinal expansion outpaces protective outcomes, part of the measured extractiveness reflects professional and institutional rent-seeking by the legal apparatus rather than genuine transfer of protection to citizens, which would shift some of the tangled_rope''s extraction component toward the legal professions rather than solely toward citizens as beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_of_doctrine, empirical, 'Whether legal-professional self-interest, not citizen protection, drives some doctrinal expansion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__liberal_due_process_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1628, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1628, 0.15).
narrative_ontology:measurement(magn_tr_t1789, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1789, 0.2).
narrative_ontology:measurement(magn_tr_t1868, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1868, 0.28).
narrative_ontology:measurement(magn_tr_t1950, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1950, 0.34).
narrative_ontology:measurement(magn_tr_t2001, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 2001, 0.38).
narrative_ontology:measurement(magn_tr_t2025, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1215, 0.15).
narrative_ontology:measurement(magn_be_t1628, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1628, 0.25).
narrative_ontology:measurement(magn_be_t1789, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1789, 0.35).
narrative_ontology:measurement(magn_be_t1868, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1868, 0.45).
narrative_ontology:measurement(magn_be_t1950, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1950, 0.58).
narrative_ontology:measurement(magn_be_t2001, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 2001, 0.65).
narrative_ontology:measurement(magn_be_t2025, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 2025, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1215, 0.2).
narrative_ontology:measurement(magn_su_t1628, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1628, 0.3).
narrative_ontology:measurement(magn_su_t1789, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1789, 0.38).
narrative_ontology:measurement(magn_su_t1868, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1868, 0.45).
narrative_ontology:measurement(magn_su_t1950, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1950, 0.53).
narrative_ontology:measurement(magn_su_t2001, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 2001, 0.58).
narrative_ontology:measurement(magn_su_t2025, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39__feudal_prerogative_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39__originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the magna_carta_clause_39 kernel, each authored as its own constraint file with its own stable ε per the ε-invariance principle: feudal_prerogative_reading (narrow, hierarchy-preserving, low extractiveness), liberal_due_process_reading (this file — expansive, universal, high extractiveness), and originalist_limitation_reading (historically bounded to 1215 abuses, moderate extractiveness). The three are linked via affects_constraints because they compete for interpretive authority over the same textual kernel; a shift in which reading dominates judicial or scholarly consensus structurally affects the legitimacy and citational weight available to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
