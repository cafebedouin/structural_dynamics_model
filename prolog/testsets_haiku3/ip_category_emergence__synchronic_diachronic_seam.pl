% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__synchronic_diachronic_seam
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__synchronic_diachronic_seam, []).

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
 *   constraint_id: ip_category_emergence__synchronic_diachronic_seam
 *   human_readable: IP Category Emergence and First-Holding Independence (Synchronic-Diachronic Seam)
 *   domain: legal_philosophy/intellectual_property/jurisprudence
 *
 * SUMMARY:
 *   This constraint instantiates the synchronic-diachronic seam reading of
 *   the IP category emergence kernel. The question at the center is
 *   structural: Can legal category emergence (when 'ownable expression'
 *   became a coherent concept) and first-holding occupancy change (when the
 *   first claimant entered the legitimate holder set) vary independently? Or
 *   are they co-determined — a single temporal event framed in two different
 *   ways (M4/M5 collapse test)? This reading asserts they ARE formally
 *   independent, that the independence is not a temporal framing artifact,
 *   and that jurisprudence must enforce this distinction to maintain
 *   doctrinal coherence. The reading competes with two siblings: the
 *   thinkability_reading (which emphasizes category emergence as the
 *   fundamental event) and the first_holding_reading (which emphasizes
 *   occupancy redistribution). This constraint's role is to police the
 *   boundary between the two, asserting they are separable, and to suppress
 *   evidence that they collapse into each other.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, 0.68).
domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, 0.45).
domain_priors:theater_ratio(ip_category_emergence__synchronic_diachronic_seam, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, extractiveness, 0.68).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__synchronic_diachronic_seam, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__synchronic_diachronic_seam, "IP Category Emergence and First-Holding Independence (Synchronic-Diachronic Seam)").
narrative_ontology:topic_domain(ip_category_emergence__synchronic_diachronic_seam, "legal_philosophy/intellectual_property/jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__synchronic_diachronic_seam).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__synchronic_diachronic_seam, '33e2fd57-462f-4cd1-a310-d0ec1cc250ae').
narrative_ontology:cs_kernel_codification('33e2fd57-462f-4cd1-a310-d0ec1cc250ae', formalized).
narrative_ontology:cs_authority_grounding('33e2fd57-462f-4cd1-a310-d0ec1cc250ae', lineage).
narrative_ontology:cs_interpretation_layer_present('33e2fd57-462f-4cd1-a310-d0ec1cc250ae').
narrative_ontology:cs_reading_relation('33e2fd57-462f-4cd1-a310-d0ec1cc250ae', ip_category_emergence__thinkability_reading, influences).
narrative_ontology:cs_reading_relation('33e2fd57-462f-4cd1-a310-d0ec1cc250ae', ip_category_emergence__first_holding_reading, influences).
narrative_ontology:cs_axiom('33e2fd57-462f-4cd1-a310-d0ec1cc250ae', foundational, emergence_occupancy_formal_independence).
narrative_ontology:cs_axiom_status(emergence_occupancy_formal_independence, holdable).
narrative_ontology:cs_axiom_grounding('33e2fd57-462f-4cd1-a310-d0ec1cc250ae', emergence_occupancy_formal_independence, deontological).
narrative_ontology:cs_axiom('33e2fd57-462f-4cd1-a310-d0ec1cc250ae', secondary, temporal_framing_robustness).
narrative_ontology:cs_axiom_status(temporal_framing_robustness, holdable).
narrative_ontology:cs_axiom_grounding('33e2fd57-462f-4cd1-a310-d0ec1cc250ae', temporal_framing_robustness, empirically_contingent).
narrative_ontology:cs_reference_frame('33e2fd57-462f-4cd1-a310-d0ec1cc250ae', independent_category_and_holder_separation).
narrative_ontology:cs_drift_state('33e2fd57-462f-4cd1-a310-d0ec1cc250ae', contemporary_m4m5_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('33e2fd57-462f-4cd1-a310-d0ec1cc250ae', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, category_coherence_defenders).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, alternative_temporal_framings).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, mathematical_collapse_theorists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legal theorists, bar associations, and academic institutions that benefit from the independence assumption. They get stable categories, predictable precedent, and a framework that avoids costly re-examination of IP law's temporal foundations. They have analytical exit: they could in principle re-frame the problem, but doing so would require institutional reorganization and academic prestige costs.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, category_coherence_defenders, beneficiary,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__synchronic_diachronic_seam, category_coherence_defenders, agenda_setter).

% Scholarly and theoretical communities offering temporal-collapse hypotheses (mathematical, historical, philosophical) that propose emergence and occupancy cannot vary independently. They bear the cost of institutional exclusion: their work is marginalixed as confusing or methodologically suspect, even when the analytical case is strong. They have constrained exit: they can publish outside IP law, but doing so removes them from influence over actual jurisprudence.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, alternative_temporal_framings, payer,
    organized, generational, constrained, universal).

% Courts and appellate bodies that operationalize the independence claim when issuing precedent. They must decide whether a copyright question is about category emergence (when did the category become available to new parties) or occupancy (who is the legitimate first claimant). The constraint enforces that these are distinct questions; if they collapse, judicial reasoning becomes incoherent.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, judicial_authority, agenda_setter,
    institutional, generational, analytical, national).

% Researchers in complexity theory and phase transitions who find evidence that emergence and occupancy are coupled under M4/M5 conditions. Their disciplinary identity is tied to rigorous mathematical analysis; accepting that their work is excluded from IP jurisprudence on gatekeeping grounds (not analytical merit) creates cognitive dissonance. Exiting means abandoning the identity of being a theorist who can speak to real legal problems.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, mathematical_collapse_theorists, payer,
    moderate, biographical, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__synchronic_diachronic_seam, mathematical_collapse_theorists, observer).

% Historians and textual scholars who argue that ownable expression was already thinkable in medieval and Renaissance law, and that 1710 marks an occupancy restructuring, not emergence. Accepting their framing would dissolve the independence the constraint depends on; they are structurally excluded from the debate because institutional actors view their work as historially interesting but doctrinally irrelevant.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, historical_narrativists, excluded,
    powerful, generational, constrained, universal).

% Neutral analytical seat examining whether the constraint's independence claim is genuine (formal logical independence, testable empirically) or an artifact of temporal framing conventions. This seat experiences the constraint as presenting a testable hypothesis: independent emergence and occupancy, or M4/M5 collapse?
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, observer_analytical, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__synchronic_diachronic_seam, category_coherence_defenders).
narrative_ontology:fixing_cost_class(ip_category_emergence__synchronic_diachronic_seam, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes jurisprudential reasoning by enforcing separation of two questions: (1) When did the category 'ownable expression' become coherent? (emergence) versus (2) When did a new party become eligible to hold it? (occupancy). Without this separation, courts cannot reason cleanly about IP law; category shifts and occupancy redistributions become indistinguishable, and precedent becomes unstable.
% TRANSFER_FUNCTION: Moves institutional authority and doctrinal weight away from collapse-hypothesis framings toward the two-step narrative. Researchers offering mathematical or historical evidence for coupling must pay gatekeeping costs (journal rejection, precedent weight reduction, curriculum exclusion). Courts treating emergence and occupancy as independent can issue precedent without engaging collapse arguments.
% ABSENT_VOICES: Historical narrativists, mathematical collapse theorists, and interdisciplinary researchers who propose emergence and occupancy are co-determined. If they were seated in the conversation, they would offer evidence (historical, mathematical, formal) that the independence is spurious and that jurisprudence is manufacturing stability by forbidding examination of whether the two events actually coincide. Their presence would raise the evidential bar for independence claims and force courts to adjudicate empirical questions about phase transitions and category formation.
% DISAPPEARANCE_RATIONALE: If the independence assumption vanished, IP jurisprudence would have to re-examine its temporal foundation. Courts could no longer treat category emergence and first-holding as separable doctrinal axes; instead, they would need to ask whether any given change (expansion of eligible owners, broadening of ownable subject matter) is a category-boundary shift, an occupancy redistribution, or an indication that emergence and occupancy collapsed into a single integrated transition. Precedent would become reviewable under the new framework, and institutional reorganization would be necessary.
% FOUNDING_PROBLEM: Early modern IP law needed a framework to reason about new forms of ownership that were becoming technically and socially possible (printing, reproduction, authorial attribution) without collapsing into incoherence. Courts needed to answer: When did the category become available? (synchronic question) and When did a new party become eligible? (diachronic question). The independence assumption allowed these to be distinct, each with its own historical date and causal logic, preventing the temporal narrative from becoming a circular justification.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars attest the founding problem is live: category coherence remains essential for IP doctrine (Lemley on property theory; Aoki on framing). Mathematical theorists attest the problem is misframed or dead: emergence and occupancy are likely coupled under plausible models, and the independence assumption is either false or an artifact of temporal framing (evidence from complex-systems theory and phase-transition models, published in mathematics and physics journals, not yet in law reviews). Courts have not adjudicated which testimony is controlling; institutional gatekeeping keeps collapse arguments out of precedent.
narrative_ontology:disappearance_verdict(ip_category_emergence__synchronic_diachronic_seam, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__synchronic_diachronic_seam, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__synchronic_diachronic_seam, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ip_category_emergence__synchronic_diachronic_seam, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__synchronic_diachronic_seam, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at interval end) because the constraint enforces a doctrinal boundary that prevents jurisprudence from re-examining its temporal foundation. Courts that accept the independence assumption can avoid costly re-examination of precedent; those who would challenge it must bear the burden of justifying a framework shift. Suppression is moderate (0.45) because the constraint does not entirely foreclose alternative framings — it is enforced through institutional gatekeeping (journal review, precedent weight, curriculum design) rather than formal prohibition. Theater ratio rises over time (0.38→0.52) because the constraint's function shifts: early on, the independence claim was genuinely necessary for establishing coherent categories; later, it becomes partly performative — the constraint persists because institutional actors have invested in the two-step framework, not because the underlying empirical or logical case strengthens. The measurement series model emergence of the M4/M5 debate: early measurements are projected (before collapse-hypothesis research became visible in IP circles); later measurements are observed as institutional gatekeeping intensifies in response to challenges.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (category coherence defenders, judicial authority) experience the constraint as enabling coherent reasoning — they need the independence assumption to adjudicate cases cleanly. The payer seats (alternative temporal framings, mathematical collapse theorists) experience it as institutional suppression: their evidence that emergence and occupancy are coupled is marginalized as confusing rather than clarifying. The observer seat (analytical) experiences a fundamental structural ambiguity: Is the independence genuine, or is it a temporal framing artifact? The engine computes directionality from these divergent situations — beneficiaries get low d (the constraint subsidizes coherent reasoning for them), payers get high d (the constraint extracts from them by closing debate), making the per-seat classifications diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary seats benefit by getting stable categories and predictable precedent; they have analytical exit (can always re-frame the problem, but at institutional cost). Payer seats bear the cost of exclusion; they have constrained exit (they can publish in mathematics or philosophy journals, but IP courts will not take up their work). Mathematical collapse theorists are identity-locked: accepting that emergence and occupancy might collapse would require intellectual reorganization of their entire theoretical framework — they cannot exit without abandoning their disciplinary identity.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows signs of mandatrophy — the founding problem (need for stable categories in early modern IP law) may be dead or substantially resolved, yet the institutional apparatus enforces the independence claim with increasing theater (gatekeeping without substantive doctrinal innovation). The constraint persists not because courts and legislatures still need the independence assumption to reason about IP, but because organized institutional actors (courts, law schools, bar associations) have built their practices around it and avoid the cost of revision. The measurement series shows theater rising faster than extractiveness levels off, consistent with mandatrophy patterns: performative maintenance replaces functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formal_independence_vs_collapse,
    'Are emergence and first-holding formally independent variables, or do they co-occur necessarily under conditions modeled by M4/M5 collapse scenarios?',
    'Formal mathematical analysis of phase-transition and emergence models applied to category formation; empirical study of other legal systems where emergence and occupancy redistribution occurred at different times; post-hoc analysis of historical IP law to determine whether the Statute of Anne marks one event (occupancy redistribution on a pre-existing category) or two independent events.',
    'If independent, the synchronic-diachronic seam is genuine and the constraint is structurally necessary for jurisprudence. If co-determined (M4/M5 collapse), the two-step narrative is a temporal framing artifact and the constraint manufactures false stability; jurisprudence would need to reorganize around a single integrated transition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(formal_independence_vs_collapse, empirical, 'Whether the independence claim reflects formal structure or temporal framing conventions.').

omega_variable(
    institutional_gatekeeping_mechanism,
    'Is suppression of alternative framings (mathematical collapse, historical narrativism) structural (these framings genuinely confuse doctrine) or performative (institutional actors suppress them to preserve doctrinal stability)?',
    'Comparative analysis of IP jurisprudence in legal systems that did NOT enforce the independence assumption; discourse analysis of gatekeeping decisions in law reviews and appellate opinions; tracking whether courts'' reasoning improves when collapse arguments are admitted vs. excluded.',
    'If structural, the suppression is justified as protecting coherent reasoning. If performative, the constraint is extractive: it excludes voices not because they are wrong, but because admitting them would require institutional reorganization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_gatekeeping_mechanism, empirical, 'Whether gatekeeping serves doctrinal clarity or institutional inertia.').

omega_variable(
    temporal_framing_dependency,
    'Does the appearance of independence depend on how one time-slices the historical record (choosing 1710 as a boundary), or is independence robust across different temporal framings?',
    'Mathematical analysis of how M4/M5 collapse behavior varies with time-slicing resolution; historical analysis of whether the independence claim holds if one uses different dates (1476 printing press, 1790 US Constitution, 1886 Berne Convention) as the reference point for category emergence.',
    'If independence is time-slicing artifact, the entire jurisprudential foundation is contingent on an arbitrary choice of historical boundary; the constraint would lose its claim to enforce genuine logical structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_framing_dependency, conceptual, 'Whether the independence survives alternative temporal framings or collapses into a single integrated transition.').

omega_variable(
    category_coherence_genuine_necessity,
    'Do modern courts actually need the independence assumption to reason about IP, or has the category stabilized sufficiently that the assumption is now redundant?',
    'Discourse analysis of contemporary appellate opinions: do courts invoke category-emergence reasoning independently from occupancy reasoning, or have the two fused in practice? Controlled legal reasoning task: can judges reach sound decisions on novel IP scenarios without presupposing independence?',
    'If necessary, the constraint serves genuine coordination function. If redundant, it is a vestigial mandate (piton candidate): institutional actors maintain it through theater because dismantling it would require acknowledging the founding problem is solved.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(category_coherence_genuine_necessity, empirical, 'Whether the independence assumption remains functionally necessary for jurisprudence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__synchronic_diachronic_seam, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t0, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ip_c_tr_t5, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 5, 0.42).
narrative_ontology:measurement(ip_c_tr_t10, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 10, 0.46).
narrative_ontology:measurement(ip_c_tr_t15, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 15, 0.49).
narrative_ontology:measurement(ip_c_tr_t25, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 25, 0.51).
narrative_ontology:measurement(ip_c_tr_t40, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t0, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ip_c_be_t5, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(ip_c_be_t10, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(ip_c_be_t15, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(ip_c_be_t25, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(ip_c_be_t40, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t0, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ip_c_su_t5, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(ip_c_su_t10, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(ip_c_su_t15, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(ip_c_su_t25, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 25, 0.44).
narrative_ontology:measurement(ip_c_su_t40, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 40, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__synchronic_diachronic_seam, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ip_category_emergence__synchronic_diachronic_seam, 0.12).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__first_holding_reading).

% DUAL FORMULATION NOTE:
% The IP category emergence kernel decomposes into three constraint stories representing three competing readings. Thinkability_reading emphasizes category emergence as the fundamental event (ownable expression became a coherent legal concept ca. 1710). First_holding_reading emphasizes occupancy redistribution (authors entered the legitimate claimant set ca. 1710). This reading (synchronic_diachronic_seam) asserts that emergence and occupancy change are formally independent and that jurisprudence must enforce the boundary between them (M4/M5 collapse test determines whether the independence is real or spurious). All three stories are linked: if emergence and occupancy collapse into a single event (M4/M5), then the synchronic-diachronic seam dissolves and the thinkability and first_holding readings become indistinguishable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ip_category_emergence__synchronic_diachronic_seam, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
