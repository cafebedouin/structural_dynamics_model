% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__living_document_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__living_document_reading, []).

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
 *   constraint_id: magna_carta_1215__living_document_reading
 *   human_readable: Magna Carta as Living Constitutional Substrate (Interpretive-Tradition Reading)
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This story instantiates the LIVING-DOCUMENT reading of the Magna Carta
 *   kernel: the claim that the document's authoritative meaning is not fixed
 *   at 1215 but legitimately accumulates through centuries of judicial
 *   interpretation and precedent, such that the common-law tradition's
 *   current understanding of Clause 39 constitutes the document's real
 *   constitutional content. This is a meta-level reading distinct from both
 *   the baronial_privilege_reading (meaning fixed to feudal contract terms)
 *   and the universal_rights_reading (meaning fixed to a transhistorical
 *   universal claim latent in the 1215 text itself) — this reading instead
 *   claims that WHO gets to say what the document means, and HOW that
 *   authority legitimately evolves, is itself the operative constitutional
 *   fact. It does not resolve the contest between the other two readings; it
 *   explains the mechanism by which either reading, or a shifting blend,
 *   could be sustained as 'law' without being a mere historical error or
 *   fabrication.
 *
 * KEY AGENTS:
 *   - common_law_judiciary: primary agenda-setter and beneficiary of interpretive authority (institutional/arbitrage)
 *   - constitutional_courts: apex beneficiaries who deploy the pedigree claim for legitimacy
 *   - civil_rights_litigants: downstream beneficiaries of expansive readings, contingent on courts sustaining the doctrine
 *   - originalist_litigants and legislatures: bear the cost of doctrinal instability against fixed textual claims
 *   - colonial and postcolonial subjects: bear the diffuse cost of an imposed tradition dressed as organic development
 *   - legal historians: analytical observers tracking the doctrine's actual use pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, 0.38).
domain_priors:suppression_score(magna_carta_1215__living_document_reading, 0.28).
domain_priors:theater_ratio(magna_carta_1215__living_document_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__living_document_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__living_document_reading, "Magna Carta as Living Constitutional Substrate (Interpretive-Tradition Reading)").
narrative_ontology:topic_domain(magna_carta_1215__living_document_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__living_document_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__living_document_reading, '2853506a-fc5d-4659-8546-f75e9b7e28fc').
narrative_ontology:cs_kernel_codification('2853506a-fc5d-4659-8546-f75e9b7e28fc', fixed_text).
narrative_ontology:cs_authority_grounding('2853506a-fc5d-4659-8546-f75e9b7e28fc', practice).
narrative_ontology:cs_interpretation_layer_present('2853506a-fc5d-4659-8546-f75e9b7e28fc').
narrative_ontology:cs_reading_relation('2853506a-fc5d-4659-8546-f75e9b7e28fc', magna_carta_1215__baronial_privilege_reading, influences).
narrative_ontology:cs_reading_relation('2853506a-fc5d-4659-8546-f75e9b7e28fc', magna_carta_1215__universal_rights_reading, influences).
narrative_ontology:cs_axiom('2853506a-fc5d-4659-8546-f75e9b7e28fc', foundational, precedential_accumulation_constitutes_legitimate_development).
narrative_ontology:cs_axiom_status(precedential_accumulation_constitutes_legitimate_development, holdable).
narrative_ontology:cs_axiom_grounding('2853506a-fc5d-4659-8546-f75e9b7e28fc', precedential_accumulation_constitutes_legitimate_development, conventional).
narrative_ontology:cs_axiom('2853506a-fc5d-4659-8546-f75e9b7e28fc', foundational, original_intent_is_not_binding_once_superseded_by_settled_practice).
narrative_ontology:cs_axiom_status(original_intent_is_not_binding_once_superseded_by_settled_practice, holdable).
narrative_ontology:cs_axiom_grounding('2853506a-fc5d-4659-8546-f75e9b7e28fc', original_intent_is_not_binding_once_superseded_by_settled_practice, instrumental).
narrative_ontology:cs_reference_frame('2853506a-fc5d-4659-8546-f75e9b7e28fc', medieval_feudal_charter_practice).
narrative_ontology:cs_drift_state('2853506a-fc5d-4659-8546-f75e9b7e28fc', contemporary_constitutional_jurisprudence, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('2853506a-fc5d-4659-8546-f75e9b7e28fc', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__living_document_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, common_law_judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, constitutional_courts).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, civil_rights_litigants).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, legal_academy).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, originalist_litigants).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, legislatures_seeking_textual_finality).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, colonial_and_postcolonial_subjects_of_imposed_common_law).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, precedential_accumulation_constitutes_constitutional_development).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, interpretive_tradition_can_legitimately_supersede_original_meaning).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts in common-law jurisdictions treat Magna Carta's clauses (especially Clause 39) as a living root that later precedent may extend, reinterpret, or reweight without being bound to 1215 conditions. Judges cite it as authority for due-process expansions the barons never contemplated, and the doctrine of stare decisis lets each generation's holding become the operative meaning for the next. They administer the interpretive machinery and are its primary authors.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, common_law_judiciary, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Apex courts (US, UK, Commonwealth) draw on the living-tradition reading to ground modern rights doctrine in an ancient pedigree, which lends legitimacy to rulings that would otherwise look like judicial invention. The pedigree claim is a resource they can deploy or withhold depending on the case.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, constitutional_courts, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__living_document_reading, constitutional_courts, agenda_setter).

% Litigants seeking due-process or habeas protections benefit when courts read Clause 39 expansively through the accumulated tradition rather than through its 1215 baronial scope. Their access to the constraint's benefit depends entirely on courts continuing to accept the living-document premise.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, civil_rights_litigants, beneficiary,
    moderate, biographical, constrained, national).

% Legal scholars build careers on tracing, justifying, and critiquing the interpretive chain from 1215 to the present. The living-document frame is the raw material of an entire subfield of constitutional theory and legal history.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, legal_academy, beneficiary,
    organized, generational, mobile, global).

% Parties who argue from the original 1215 text and its narrow feudal scope find courts routinely override that argument with 'the tradition has developed since.' They bear the cost of an interpretive move that can always be invoked against textual specificity, with no fixed answer to when tradition legitimately supersedes text.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, originalist_litigants, payer,
    moderate, biographical, constrained, national).

% Legislative bodies that attempt to settle a constitutional question by statute or amendment find courts can still reach back through the living tradition to reinterpret the underlying constitutional substrate, eroding the finality legislatures sought. Their exit is limited to further legislation or constitutional amendment, both costly and uncertain.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, legislatures_seeking_textual_finality, payer,
    powerful, generational, constrained, national).

% Populations under common-law systems imposed through empire inherit the living-tradition doctrine as part of an imported legal architecture. The doctrine's claim to organic, legitimate development obscures that the tradition was transplanted by force, not grown locally; they have no practical exit from a legal system whose legitimacy story erases that history.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, colonial_and_postcolonial_subjects_of_imposed_common_law, payer,
    powerless, generational, trapped, global).

% Historians and originalist jurists who insist the document's meaning is fixed to 1215 feudal terms are structurally sidelined by the living-tradition frame, which treats their reading as historically interesting but doctrinally superseded. They would object that 'development' is a euphemism for judicial license, but the interpretive authority structure gives their objection no formal veto.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, baronial_privilege_reading_advocates, excluded,
    moderate, generational, constrained, national).

% Study how and when courts invoke 'living tradition' versus 'original meaning' and can trace which invocations track genuine doctrinal necessity versus rhetorical convenience for a preferred outcome.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism by which constitutional meaning can adapt to changed social conditions without requiring formal textual amendment for every incremental extension of a protection — allowing due process and rule-of-law norms to keep pace with governance structures the medieval barons never anticipated.
% TRANSFER_FUNCTION: Moves interpretive authority from the historical text and its drafters to the contemporary judiciary and legal academy; moves the burden of proof in constitutional argument from 'what did the text mean' to 'what has the tradition developed into,' which shifts practical power toward whoever currently sits on the bench or teaches in the academy.
% ABSENT_VOICES: The 1215 barons and the medieval legal order they operated within have no standing to contest what has been made of their instrument. Colonial and postcolonial populations who received common law by imposition rather than organic development are rarely consulted on whether the 'living tradition' framing accurately describes their relationship to it.
% DISAPPEARANCE_RATIONALE: If courts abandoned the living-document premise and reverted strictly to 1215 baronial scope, a large body of due-process and habeas jurisprudence built on Clause 39's extended reading would lose its doctrinal anchor overnight, forcing either fresh constitutional grounding or the collapse of protections currently justified by appeal to this lineage.
% FOUNDING_PROBLEM: How can a legal order maintain continuity with a foundational text while governance conditions, populations, and moral consensus change over eight centuries, without either freezing the law to obsolete feudal categories or abandoning textual authority altogether?
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars outside the common-law judiciary (e.g., civil-law academics studying interpretive method) corroborate that the underlying problem — reconciling textual fidelity with social change — is a genuine and unresolved feature of long-lived constitutional orders generally, not merely a self-serving story told by the judges who benefit from interpretive discretion. However, those same scholars also note the living-tradition doctrine conveniently expands exactly the authority of the institution that maintains it, so the corroboration is partial: the problem is real, but the specific resolution chosen is not obviously the only legitimate one.
narrative_ontology:disappearance_verdict(magna_carta_1215__living_document_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__living_document_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__living_document_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_1215__living_document_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__living_document_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__living_document_reading_tests).
:- end_tests(magna_carta_1215__living_document_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 by 2024, up from 0.10 at founding) because the living-tradition doctrine functions as genuine coordination (adapting old text to new conditions without constant amendment) AND as a mechanism by which judicial and academic elites accumulate interpretive authority at the expense of textualist claimants and legislatures seeking finality — hence tangled_rope rather than pure rope. Suppression is comparatively low (0.28) because the doctrine does not physically coerce compliance; its force is doctrinal and precedential, operating through stare decisis and institutional legitimacy rather than direct coercion. Theater ratio (0.32) reflects that a meaningful share of invocations of 'the living tradition has developed' function as rhetorical cover for outcome-driven reasoning rather than genuine doctrinal necessity, though the coordination function is real and not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, the living-document reading is simply how any long-lived legal text must function — genuine, necessary adaptation. From the originalist litigant's or legislature's seat, the same doctrine looks like an unbounded escape hatch: whatever answer the court wants, 'the tradition developed' can supply it, with no fixed criterion for when supersession is legitimate versus opportunistic. The engine's per-seat computation should reflect this: agenda-setter seats compute closer to rope/tangled_rope; payer seats experience the same structure as extraction of interpretive finality.
 *
 * DIRECTIONALITY LOGIC:
 *   Common-law judiciary and constitutional courts are structural beneficiaries — they administer and are the primary authors of the interpretive expansion, and their institutional authority grows with each doctrinal move that treats precedent as constitutive. Civil rights litigants and the legal academy benefit contingently and derivatively. Originalist litigants, finality-seeking legislatures, and colonial/postcolonial subjects of imposed common law are targets: they bear the cost of a doctrine that can always be invoked to override their preferred fixed reading, with the postcolonial case carrying the added weight of a legitimacy narrative (organic development) that obscures an imposed origin.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling textual fidelity with social change over centuries — remains genuinely live; this is not a pure zombie mandate. But the SPECIFIC institutional resolution (vesting the judiciary and legal academy with primary interpretive authority over what 'development' counts as legitimate) is itself contestable and self-reinforcing: the beneficiaries of the doctrine are also its principal interpreters and defenders. Classifying this as tangled_rope rather than snare or pure rope avoids two mandatrophy errors: treating the entire edifice as illegitimate extraction (which would deny the real coordination value of adaptable constitutional meaning) and treating it as costless coordination (which would ignore the real transfer of authority and the real losers in doctrinal contests).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    living_tradition_vs_motivated_reasoning,
    'When a court invokes ''the interpretive tradition has developed'' to reach a new holding, is this a genuine application of an evolving doctrinal method, or is the appeal to tradition a post-hoc legitimation device for a result reached on other (e.g., policy or ideological) grounds?',
    'Systematic study of judicial opinions invoking Magna Carta/Clause 39 lineage: track whether the doctrinal move correlates with genuinely novel factual circumstances the 1215/founding-era text could not have addressed, versus cases where a textualist or narrower reading was equally available but rejected.',
    'If predominantly genuine, the living-document reading functions closer to rope (real adaptive coordination). If predominantly motivated, effective extraction is higher than authored here and the constraint drifts toward tangled_rope with a larger extractive share, or even toward snare in specific doctrinal lineages.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(living_tradition_vs_motivated_reasoning, empirical, 'Whether invocations of interpretive development are principled or rhetorical.').

omega_variable(
    authority_of_interpretive_meta_claim,
    'Does the living-document reading itself require external validation (e.g., from constitutional theory, democratic ratification of interpretive method) to be legitimate, or is judicial self-authorization of its own interpretive authority sufficient?',
    'Comparative analysis across jurisdictions with different formal constraints on judicial interpretive authority (e.g., explicit constitutional amendment clauses vs. common-law systems with no codified amendment process) to see whether legitimacy correlates with external checks.',
    'If judicial self-authorization is treated as sufficient, the doctrine is closer to self-enforcing authority (a structurally unusual and potentially unstable legitimacy basis); if external validation is required and generally present, the coordination function is more robustly grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_of_interpretive_meta_claim, conceptual, 'Whether the interpretive-authority meta-claim is self-grounding or requires external legitimation.').

omega_variable(
    postcolonial_imposition_vs_organic_development,
    'In jurisdictions where common law and its interpretive tradition arrived via colonial imposition rather than indigenous development, does the ''living tradition'' framing misdescribe the actual mechanism of legal change (imposed continuity dressed as organic evolution)?',
    'Historical and legal-anthropological comparison between common-law development in England versus its reception and subsequent local evolution in former colonies — assess whether post-independence jurisprudence shows genuine local adaptation or continued deference to metropolitan doctrinal lineage.',
    'If imposition dominates, the extraction borne by colonial/postcolonial subjects is understated in this story and the constraint''s beneficiary/victim asymmetry in those jurisdictions is more severe than the global-average metrics suggest — potentially warranting a separate decomposed story for postcolonial common-law reception specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(postcolonial_imposition_vs_organic_development, empirical, 'Whether the living-tradition frame accurately describes postcolonial legal development or obscures imposed continuity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__living_document_reading, 1215, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__living_document_reading, theater_ratio, 1215, 0.15).
narrative_ontology:measurement(magn_tr_t1400, magna_carta_1215__living_document_reading, theater_ratio, 1400, 0.18).
narrative_ontology:measurement(magn_tr_t1689, magna_carta_1215__living_document_reading, theater_ratio, 1689, 0.22).
narrative_ontology:measurement(magn_tr_t1791, magna_carta_1215__living_document_reading, theater_ratio, 1791, 0.26).
narrative_ontology:measurement(magn_tr_t1950, magna_carta_1215__living_document_reading, theater_ratio, 1950, 0.29).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_1215__living_document_reading, theater_ratio, 2024, 0.32).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__living_document_reading, base_extractiveness, 1215, 0.1).
narrative_ontology:measurement(magn_be_t1400, magna_carta_1215__living_document_reading, base_extractiveness, 1400, 0.14).
narrative_ontology:measurement(magn_be_t1689, magna_carta_1215__living_document_reading, base_extractiveness, 1689, 0.2).
narrative_ontology:measurement(magn_be_t1791, magna_carta_1215__living_document_reading, base_extractiveness, 1791, 0.25).
narrative_ontology:measurement(magn_be_t1950, magna_carta_1215__living_document_reading, base_extractiveness, 1950, 0.32).
narrative_ontology:measurement(magn_be_t2024, magna_carta_1215__living_document_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__living_document_reading, suppression_requirement, 1215, 0.15).
narrative_ontology:measurement(magn_su_t1400, magna_carta_1215__living_document_reading, suppression_requirement, 1400, 0.17).
narrative_ontology:measurement(magn_su_t1689, magna_carta_1215__living_document_reading, suppression_requirement, 1689, 0.2).
narrative_ontology:measurement(magn_su_t1791, magna_carta_1215__living_document_reading, suppression_requirement, 1791, 0.22).
narrative_ontology:measurement(magn_su_t1950, magna_carta_1215__living_document_reading, suppression_requirement, 1950, 0.25).
narrative_ontology:measurement(magn_su_t2024, magna_carta_1215__living_document_reading, suppression_requirement, 2024, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__living_document_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__living_document_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, universal_rights_reading).

% DUAL FORMULATION NOTE:
% This story is the meta-level reading in the magna_carta_1215 kernel family. baronial_privilege_reading and universal_rights_reading are substantive competing claims about what the 1215 text means; this reading is the procedural/authority claim about how meaning legitimately changes over time, and is compatible with either substantive reading being 'correct' at different historical moments. All three stories share the same underlying text (Clause 39 and the 1215 charter) but instantiate structurally distinct constraints with distinct ε values: baronial_privilege_reading measures a narrow, low-extraction historical-fidelity claim; universal_rights_reading measures a broad, contested claim asserting transhistorical universal scope; this story measures the moderate, tangled extraction inherent in vesting ongoing interpretive authority in a specific institutional lineage (the judiciary and legal academy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
