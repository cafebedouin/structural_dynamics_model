% ============================================================================
% CONSTRAINT STORY: personhood_boundary__potential_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__potential_based_reading, []).

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
 *   constraint_id: personhood_boundary__potential_based_reading
 *   human_readable: Personhood Grounded in Potential for Rational Agency, with Exclusion Judgments Delegated to Parental-Medical Authority
 *   domain: moral_philosophy/bioethics/commitment_systems
 *
 * SUMMARY:
 *   A moral-legal arrangement conditions an infant's moral standing on an
 *   assessed potential for rational agency. Under this reading, standing is
 *   not conferred by the birth event as such; it attaches to entities judged
 *   capable, eventually, of the agentic capacities the tradition holds to
 *   matter. Parents and clinical authorities conduct that assessment; infants
 *   judged to lack personhood-relevant potential fall outside the protective
 *   perimeter, and treatment-withdrawal or non-treatment determinations about
 *   them proceed under parental-medical authority rather than as contests
 *   over a rights-holder's fate. The arrangement presents the criterion as a
 *   discovered fact about minds; the structural record shows a maintained
 *   boundary requiring enforcement machinery (protocols, review committees,
 *   best-interest jurisprudence) and meeting sustained resistance from
 *   disability-led critique. This story instantiates ONE reading of the
 *   personhood_boundary kernel (see kernel_context and the committer omega);
 *   sibling readings are separate constraints with different victim sets. The
 *   epsilon referent is the standing arrangement under contest — the
 *   assessment-and-exclusion regime itself — never the rights-respecting
 *   alternative any party would substitute.
 *
 * KEY AGENTS:
 *   - infants_lacking_assessed_potential: primary target (powerless/trapped) — the class whose moral standing is the object of third-party assessment; bears the arrangement's terminal cost with no voice and no proxy inside the determination forum
 *   - disabled_persons_broadly: structural target (organized/trapped) — their class membership is the assessed property; standing made contingent ripples across the entire class
 *   - parental_decision_makers: primary beneficiary with dual position (moderate/identity_locked) — receives decision authority and release from the unconditional preservation default while absorbing grief the framework scripts as loving sacrifice
 *   - neonatal_clinical_teams: agenda setter and secondary beneficiary (institutional/constrained) — administers the assessment protocol, convenes determinations, implements outcomes including non-treatment
 *   - disability_self_advocacy_movements: excluded voice (organized/constrained) — contests the boundary publicly but stands outside the rooms where determinations are made
 *   - human_rights_monitoring_bodies: analytical observer (institutional/analytical) — documents discriminatory effects from treaty and NGO seats without adjudicating individual cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, 0.62).
domain_priors:suppression_score(personhood_boundary__potential_based_reading, 0.55).
domain_priors:theater_ratio(personhood_boundary__potential_based_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__potential_based_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__potential_based_reading, "Personhood Grounded in Potential for Rational Agency, with Exclusion Judgments Delegated to Parental-Medical Authority").
narrative_ontology:topic_domain(personhood_boundary__potential_based_reading, "moral_philosophy/bioethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__potential_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__potential_based_reading, 'cf0cb608-90a7-4a05-931d-bc0928dd8f40').
narrative_ontology:cs_kernel_codification('cf0cb608-90a7-4a05-931d-bc0928dd8f40', distributed).
narrative_ontology:cs_authority_grounding('cf0cb608-90a7-4a05-931d-bc0928dd8f40', expertise).
narrative_ontology:cs_interpretation_layer_present('cf0cb608-90a7-4a05-931d-bc0928dd8f40').
narrative_ontology:cs_reading_relation('cf0cb608-90a7-4a05-931d-bc0928dd8f40', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('cf0cb608-90a7-4a05-931d-bc0928dd8f40', personhood_boundary__fitness_contingent_reading, coexists_with).
narrative_ontology:cs_axiom('cf0cb608-90a7-4a05-931d-bc0928dd8f40', foundational, standing_grounds_in_rational_agency_potential).
narrative_ontology:cs_axiom_status(standing_grounds_in_rational_agency_potential, holdable).
narrative_ontology:cs_axiom_grounding('cf0cb608-90a7-4a05-931d-bc0928dd8f40', standing_grounds_in_rational_agency_potential, deontological).
narrative_ontology:cs_axiom('cf0cb608-90a7-4a05-931d-bc0928dd8f40', foundational, potential_is_assessable_for_exclusion_judgments).
narrative_ontology:cs_axiom_status(potential_is_assessable_for_exclusion_judgments, holdable).
narrative_ontology:cs_axiom_grounding('cf0cb608-90a7-4a05-931d-bc0928dd8f40', potential_is_assessable_for_exclusion_judgments, empirically_contingent).
narrative_ontology:cs_reference_frame('cf0cb608-90a7-4a05-931d-bc0928dd8f40', agency_capacity_graduated_standing).
narrative_ontology:cs_drift_state('cf0cb608-90a7-4a05-931d-bc0928dd8f40', contemporary_disability_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cf0cb608-90a7-4a05-931d-bc0928dd8f40', '').
narrative_ontology:cs_kernel_id(personhood_boundary__potential_based_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, parental_decision_makers).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, neonatal_clinical_teams).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, infants_lacking_assessed_potential).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, disabled_persons_broadly).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, parental_decision_makers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive a newborn with profound impairments and sit inside a protocol that asks them, with clinical counsel, to judge whether their child has a future of the kind the framework treats as making protection owed — and, when the answer is no, to authorize withholding or withdrawing life-sustaining treatment. The arrangement hands them the deciding voice and releases them from an unconditional preservation default. It also binds the decision to their identity as parents, scripted as an act of love, so that stepping outside the protocol's terms feels like abandoning the child rather than contesting the framework; there is no version of exit that leaves the parental relationship intact.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, parental_decision_makers, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, parental_decision_makers, payer).

% Staff the intensive-care units where these cases arrive; run the prognostic assessments, convene ethics reviews, document best-interest determinations, and implement whatever is decided, including non-treatment. Collect professional authority over the boundary and case-by-case resolution of otherwise open-ended conflicts. Individual clinicians may dissent or decline particular cases, but the assessment framework follows the profession across institutions, and a career built in neonatology carries its standards along.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, neonatal_clinical_teams, agenda_setter,
    institutional, generational, constrained, national).

% Are born with impairments the assessment process classifies as incompatible with a future of rational agency. Everything that happens next — treatment, its withholding, the timing of death — is decided about them by others. They have no capacity to participate, no proxy seated for them in the determination forum, and no exit of any kind from the category the assessment assigns.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, infants_lacking_assessed_potential, payer,
    powerless, immediate, trapped, national).

% Live inside the shadow of the criterion: their class membership is the very property the assessments evaluate, so every determination that turns on absent potential re-announces that their standing is conditional. Many organize — disability movements, self-advocacy networks — but organization changes public argument, not the fact that the assessed property is theirs permanently and cannot be exited.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, disabled_persons_broadly, payer,
    organized, generational, trapped, global).

% Contest the criterion in legislatures, courts, journals, and public campaigns; submit testimony to inquiries; litigate. What they cannot do is sit inside neonatal determination rooms as voices for the class at issue — their participation is admitted as advocacy about the framework, never as standing within it. Leaving the fight is conceivable for individuals but abandons the constituency to the framework's terms.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, disability_self_advocacy_movements, excluded,
    organized, generational, constrained, global).

% Treaty committees, ombuds offices, and NGOs that review national practice for discrimination against disabled persons, compile findings, and press states toward equal-standing commitments. They see the aggregate pattern across jurisdictions but do not adjudicate individual determinations and hold no vote in any single case.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, human_rights_monitoring_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__potential_based_reading, parental_decision_makers).
narrative_ontology:fixing_cost_class(personhood_boundary__potential_based_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves otherwise irresoluble tragic cases: when an infant presents with impairments judged compatible with no future agency, someone must decide whether treatment continues, and the arrangement assigns the assessment to parents and clinical authorities under a best-interest protocol instead of leaving each case to unbounded family-clinical-legal conflict.
% TRANSFER_FUNCTION: Moves the decisive stake — recognized moral standing and its protective consequences (treatment obligations, legal personhood, default preservation) — from infants assessed as lacking potential for rational agency to the authority structure: parents receive discretion over existence-affecting choices and release from the unconditional-care default; clinical teams receive protocol authority and case resolution.
% ABSENT_VOICES: The excluded infants themselves — the only parties whose existence is at issue — cannot participate and have no proxy inside the determination forum; the class's perspective enters only as advocacy from outside (disability self-advocacy movements, seated as excluded), which the framework treats as interested commentary rather than constituent voice. Human-rights monitors see the aggregate pattern but arrive after individual determinations conclude.
% DISAPPEARANCE_RATIONALE: If the potential criterion and its assessment machinery vanished overnight, borderline neonatal cases would revert to contested terrain between an equal-standing presumption and capacity-based triage: treatment defaults would shift toward maximal preservation, best-interest jurisprudence would lose its organizing doctrine, parental authority over withdrawal would narrow sharply, and disability-status assessments would lose their standing-altering force — the neonatal-decision world reorganizes around the surviving arrangements.
% FOUNDING_PROBLEM: Pre-modern and early-modern societies faced pervasive infant mortality, congenital impairment without therapeutic recourse, and scarce care resources; the potential criterion reconciled protecting newborns expected to develop into agentic persons with permitting non-treatment where no such development was thought possible.
% FOUNDING_PROBLEM_CORROBORATION: Historians of medicine and demographers — outside the benefiting parties — corroborate the founding problem's historical reality: scarcity-era triage of impaired newborns is documented across cultures. On present liveness, corroboration splits by seat: pediatric palliative-care literature attests a residual hard-case core, while disability-led scholarship and human-rights bodies attest the founding arithmetic is dissolved and the arrangement persists as boundary maintenance. No out-of-set source attests the founding problem at its original scope as still fully live.
narrative_ontology:disappearance_verdict(personhood_boundary__potential_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__potential_based_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__potential_based_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(personhood_boundary__potential_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__potential_based_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__potential_based_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__potential_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.62 reflects the interval's end state: a voiceless class's standing is decided by third-party capacity assessment — the terminal stake possible — even though formalized mass exclusion receded after mid-century. Suppression 0.55: enforcement today runs through professional gatekeeping, protocol discipline, and litigation defense rather than statute-backed coercion; dissent survives in academia but not in determination rooms. Theater 0.42: best-interest documentation and ethics consultation mix genuine deliberation with legitimation ritual — the share functioning to certify pre-framed conclusions. Accessibility collapse 0.58: inside the framework, once the potential criterion is granted, the equal-standing alternative is nearly unavailable to its own adjudicators; externally the sibling readings remain live, so collapse cannot approach mountain levels. Resistance 0.6: disability movements, faith bodies (the von Galen precedent), and human-rights law mount continuous organized opposition. The three metric series share one grid (T = 0, 24, 48, 72, 96, 120): extractiveness peaks at T=48 when exclusion is bureaucratized at widest reach, theater peaks alongside it (the assessment apparatus at maximum legitimation load), and suppression peaks with statutory enforcement. The postwar arc — reform, rights frameworks, deinstitutionalization — is a historical wave, not intermittent reinforcement; base_properties scalars report the interval's end state.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setting seat compute differently from one structure. From the clinical-parental seat the arrangement is compassionate necessity: someone must decide when treatment only prolongs suffering, and the criterion spares families open-ended conflict. From the target seats the identical procedure is existence-allocation by parties with interests in the outcome, executed over entities that cannot contest it. The engine derives this divergence from the power/exit asymmetries (institutional/constrained versus powerless/trapped); the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive derivation. infants_lacking_assessed_potential (victim, trapped, powerless) sits near the full-target pole. disabled_persons_broadly (victim, trapped despite organization — the assessed property is class membership itself) sits high. parental_decision_makers are declared beneficiaries, so derivation would land them near the subsidy pole; their dual position (authority received, grief borne, identity fused with the authorized-choice script) is invisible to the declaration, so a directionality override sets the moderate power atom to d=0.35 — that atom is occupied in this story solely by parental_decision_makers, so the override targets them uniquely. neonatal_clinical_teams combine agenda-setting with collection (professional authority, case resolution), giving moderately low d. human_rights_monitoring_bodies are analytical observers with no extraction in either direction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification disciplines two opposite errors. Reading the criterion as a mountain ('standing simply is capacity-grounded; this is what minds are') launders a maintained boundary into natural law — had beneficiaries been declared on that mountain, the false-summit signature would fire, and the naturality ambiguity is documented in the omegas regardless. Reading it as a pure snare erases the genuine coordination problem: tragic neonatal cases must be decided by someone, and an unstructured free-for-all serves no one, including future disabled children. Tangled rope keeps both faces visible — real coordination (a decision procedure for otherwise irresoluble cases) and real extraction (terminal allocation away from a voiceless class by interested assessors). Founding-problem status is contested rather than dead: the scarcity arithmetic that founded the arrangement is largely dissolved, but a residue of genuinely hard cases keeps the founding problem nominally alive — the zombie risk is left visible for the mismatch consumer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    personhood_kernel_reading_position,
    'This constraint is one reading of the personhood_boundary kernel: the potential_based_reading, under which moral standing is grounded in attributed potential for rational agency rather than in the birth event or demonstrated fitness. What would change structurally if a sibling reading held the kernel instead?',
    'Not resolvable by evidence internal to this story: sibling readings are separate constraint files (birth_threshold_reading, fitness_contingent_reading). Resolution arrives through whichever reading achieves framework dominance in law and clinical practice, or through a meta-framework (e.g., relational accounts of moral standing) that dissolves the event-versus-capacity dichotomy.',
    'If birth_threshold_reading prevailed, this constraint''s victim set would empty (every born human covered) and the assessment-and-exclusion machinery loses its warrant; if fitness_contingent_reading prevailed, the victim set widens beyond infancy to any entity failing demonstrated-fitness testing. This story''s beneficiary/victim structure and classification are indexed to the potential reading''s ontology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(personhood_kernel_reading_position, conceptual, 'Committer-frame record: this story instantiates the potential_based_reading of the personhood_boundary kernel; sibling readings are other constraints.').

omega_variable(
    prognostic_reliability_of_potential,
    'Can possession or absence of potential for rational agency be assessed reliably at the stage when exclusion judgments are made, or is ''lacking potential'' partly an artifact of prognostic pessimism and assessment bias against disabled infants?',
    'Longitudinal follow-up cohorts comparing neonatal prognosis against realized developmental outcomes; inter-rater reliability studies of prognostication; audits of prediction-error rates by diagnosis and by treating institution.',
    'Systematic pessimistic error means many infants classified as lacking potential were wrongly excluded: effective extraction exceeds the authored value and the constraint trends toward snare. Reliable assessment bounds the exclusion component by genuine tragic necessity and supports the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prognostic_reliability_of_potential, empirical, 'Whether the empirical premise beneath exclusion judgments is epistemically sound.').

omega_variable(
    assessor_interest_alignment,
    'Do exclusion determinations track the infant''s condition, or the assessors'' interests — family burden relief, unit bed capacity, treatment-cost avoidance, institutional liability management?',
    'Cross-institutional variation analysis comparing determination rates and thresholds across settings that differ in reimbursement structure, bed pressure, and liability regime while holding casemix constant.',
    'If determination rates move with assessor incentives rather than casemix, the coordination function degrades into cover and the classification shifts toward snare; incentive-insensitive determinations support retention of a genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assessor_interest_alignment, empirical, 'Whether the judging apparatus serves the assessed condition or the assessors'' stakes.').

omega_variable(
    potential_threshold_indeterminacy,
    'Where does ''potential for rational agency'' begin and end — what degree or certainty of impairment places an infant outside the standing-conferring class? The doctrine does not fix the line.',
    'Conceptual consolidation within the reading''s own tradition: published criteria, professional-guideline harmonization, and case-law settlement would reveal whether a stable line exists or whether the line floats with each determination.',
    'Every candidate line-placement moves the victim-set boundary and therefore epsilon; an unfixed line leaves the exclusion component structurally open-ended and strengthens the extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(potential_threshold_indeterminacy, conceptual, 'Indeterminacy of the boundary the doctrine is supposed to enforce.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__potential_based_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__potential_based_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pers_tr_t24, personhood_boundary__potential_based_reading, theater_ratio, 24, 0.55).
narrative_ontology:measurement(pers_tr_t48, personhood_boundary__potential_based_reading, theater_ratio, 48, 0.68).
narrative_ontology:measurement(pers_tr_t72, personhood_boundary__potential_based_reading, theater_ratio, 72, 0.5).
narrative_ontology:measurement(pers_tr_t96, personhood_boundary__potential_based_reading, theater_ratio, 96, 0.44).
narrative_ontology:measurement(pers_tr_t120, personhood_boundary__potential_based_reading, theater_ratio, 120, 0.42).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__potential_based_reading, base_extractiveness, 0, 0.63).
narrative_ontology:measurement(pers_be_t24, personhood_boundary__potential_based_reading, base_extractiveness, 24, 0.73).
narrative_ontology:measurement(pers_be_t48, personhood_boundary__potential_based_reading, base_extractiveness, 48, 0.81).
narrative_ontology:measurement(pers_be_t72, personhood_boundary__potential_based_reading, base_extractiveness, 72, 0.69).
narrative_ontology:measurement(pers_be_t96, personhood_boundary__potential_based_reading, base_extractiveness, 96, 0.64).
narrative_ontology:measurement(pers_be_t120, personhood_boundary__potential_based_reading, base_extractiveness, 120, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__potential_based_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(pers_su_t24, personhood_boundary__potential_based_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(pers_su_t48, personhood_boundary__potential_based_reading, suppression_requirement, 48, 0.77).
narrative_ontology:measurement(pers_su_t72, personhood_boundary__potential_based_reading, suppression_requirement, 72, 0.59).
narrative_ontology:measurement(pers_su_t96, personhood_boundary__potential_based_reading, suppression_requirement, 96, 0.57).
narrative_ontology:measurement(pers_su_t120, personhood_boundary__potential_based_reading, suppression_requirement, 120, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__potential_based_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, fitness_contingent_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'personhood boundary' decomposes into three structurally distinct constraints — event-threshold, demonstrated-fitness, and potential-based readings — because each confers standing on a different extension and therefore names a different victim set. Forcing them into one story would make epsilon observer-dependent, violating epsilon-invariance. The potential-based reading is historically downstream of capacity-talk that radicalizes into demonstrated-fitness tests, and its victim set is disjoint from the birth-threshold reading's; each member links the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personhood_boundary__potential_based_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
