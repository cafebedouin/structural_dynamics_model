% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__constitutional_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__constitutional_floor_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: fisa_702_statutory_text__constitutional_floor_reading
 *   human_readable: Fourth Amendment Warrant Requirement for FISA 702 U.S. Person Content Queries (Constitutional Floor Reading)
 *   domain: constitutional_law/national_security/surveillance
 *
 * SUMMARY:
 *   This constraint story instantiates the constitutional_floor_reading of
 *   the fisa_702_statutory_text kernel. The standing arrangement under
 *   contest is the executive practice of querying Section 702-acquired
 *   databases containing U.S. person communications content without obtaining
 *   individualized probable cause warrants. This reading reframes the
 *   practice not as a foreign intelligence statutory matter but as a criminal
 *   procedure question governed by the Fourth Amendment. The constraint is
 *   the constitutional warrant requirement itself, which coordinates privacy
 *   protection for U.S. persons while extracting operational speed and
 *   secrecy from the executive branch. KEY AGENTS (by structural
 *   relationship): U.S. persons (beneficiary, powerless/constrained) â
 *   receive privacy protection; Executive surveillance agencies (payer,
 *   institutional/constrained) â bear compliance costs and operational
 *   friction; FISA Court (agenda_setter, institutional/analytical) â
 *   administers warrant gatekeeping; Civil liberties organizations (observer,
 *   organized/analytical) â litigate the reading; Congress (observer,
 *   institutional/constrained) â bound by constitutional floor.
 *
 * KEY AGENTS:
 *   - U.S. persons: Primary beneficiary (powerless/constrained) â receive constitutional privacy protection against warrantless queries
 *   - Executive surveillance agencies: Primary payer (institutional/constrained) â bear extraction in the form of legal delay, probable cause preparation, and disclosure
 *   - FISA Court: Agenda-setter (institutional/analytical) â administers the warrant requirement and derives institutional authority from it
 *   - Civil liberties organizations: Analytical observer (organized/analytical) â represent privacy interests without direct cost or benefit flow
 *   - Congress: Secondary observer (institutional/constrained) â statutorily active but subordinate to constitutional floor
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__constitutional_floor_reading, 0.25).
domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, 0.35).
domain_priors:theater_ratio(fisa_702_statutory_text__constitutional_floor_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__constitutional_floor_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__constitutional_floor_reading, "Fourth Amendment Warrant Requirement for FISA 702 U.S. Person Content Queries (Constitutional Floor Reading)").
narrative_ontology:topic_domain(fisa_702_statutory_text__constitutional_floor_reading, "constitutional_law/national_security/surveillance").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__constitutional_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__constitutional_floor_reading, '08ef0873-1b84-48ce-be5f-c5df6124eba7').
narrative_ontology:cs_kernel_codification('08ef0873-1b84-48ce-be5f-c5df6124eba7', fixed_text).
narrative_ontology:cs_authority_grounding('08ef0873-1b84-48ce-be5f-c5df6124eba7', lineage).
narrative_ontology:cs_interpretation_layer_present('08ef0873-1b84-48ce-be5f-c5df6124eba7').
narrative_ontology:cs_reading_relation('08ef0873-1b84-48ce-be5f-c5df6124eba7', fisa_702_statutory_text__foreign_target_strict_reading, coexists_with).
narrative_ontology:cs_reading_relation('08ef0873-1b84-48ce-be5f-c5df6124eba7', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_axiom('08ef0873-1b84-48ce-be5f-c5df6124eba7', foundational, warrant_required_for_us_person_content_queries).
narrative_ontology:cs_axiom_status(warrant_required_for_us_person_content_queries, holdable).
narrative_ontology:cs_axiom_grounding('08ef0873-1b84-48ce-be5f-c5df6124eba7', warrant_required_for_us_person_content_queries, deontological).
narrative_ontology:cs_axiom('08ef0873-1b84-48ce-be5f-c5df6124eba7', foundational, foreign_intelligence_purpose_no_warrant_exemption).
narrative_ontology:cs_axiom_status(foreign_intelligence_purpose_no_warrant_exemption, holdable).
narrative_ontology:cs_axiom_grounding('08ef0873-1b84-48ce-be5f-c5df6124eba7', foreign_intelligence_purpose_no_warrant_exemption, deontological).
narrative_ontology:cs_reference_frame('08ef0873-1b84-48ce-be5f-c5df6124eba7', fourth_amendment_warrant_framework).
narrative_ontology:cs_drift_state('08ef0873-1b84-48ce-be5f-c5df6124eba7', contemporary_fisa_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('08ef0873-1b84-48ce-be5f-c5df6124eba7', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, us_persons).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, executive_surveillance_agencies).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, fourth_amendment_warrant_clause).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, carpenter_digital_privacy_doctrine).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, individualized_probable_cause_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive protection against warrantless government search of their communications content. The constitutional guarantee attaches by virtue of U.S. person status and is enforced regardless of individual consent. Exit requires leaving the territorial and legal scope of the Fourth Amendment, which is costly and incomplete.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, us_persons, beneficiary,
    powerless, generational, constrained, national).

% Conduct foreign intelligence surveillance and query Section 702-acquired databases. Under this reading, they must obtain individualized probable cause warrants from the FISA Court before accessing U.S. person content, imposing legal preparation, delay, and disclosure costs that degrade operational speed and secrecy.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, executive_surveillance_agencies, payer,
    institutional, biographical, constrained, national).

% Administers the warrant requirement for U.S. person content queries under this reading. Must apply individualized probable cause scrutiny rather than deferring to foreign intelligence purpose. The court's gatekeeping authority is constituted by the constraint; it cannot exit its interpretive role without abandoning the judicial function.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, fisa_court, agenda_setter,
    institutional, generational, analytical, national).

% Litigate and advocate for the constitutional floor reading, representing U.S. person privacy interests in public and judicial fora. They do not directly receive the constraint's protection nor bear its costs, but serve as the primary analytical voice asserting the Fourth Amendment warrant requirement against executive practice.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_organizations, observer,
    organized, generational, analytical, national).

% Enacted the FISA statute and controls appropriations for surveillance programs, but under this reading the constitutional warrant requirement operates independently of statutory language. Congress cannot legislatively authorize warrantless queries of U.S. person content; its exit options are bounded by constitutional amendment politics.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, congress, observer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__constitutional_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__constitutional_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents arbitrary executive intrusion into private communications by interposing a neutral magistrate between the surveillance agency and the U.S. person, coordinating state power and individual privacy through probable cause review.
% TRANSFER_FUNCTION: Moves operational freedom, speed, and secrecy from executive surveillance agencies to judicial oversight and privacy protection for U.S. persons; transfers the evidentiary and procedural burden to the government before any content query occurs.
% ABSENT_VOICES: Non-U.S. persons abroad and foreign targets are structurally excluded from the Fourth Amendment warrant conversation; their interests in global communication privacy and in not being collateral data subjects are not represented. The executive branch's foreign intelligence operational community is present in the policy conversation but is treated as a payer rather than a legitimate interlocutor in this reading.
% DISAPPEARANCE_RATIONALE: If the constitutional warrant requirement for 702 U.S. person queries vanished, executive agencies would query acquired databases without individualized judicial scrutiny, the FISA Court's domestic-content oversight function would collapse, and the equilibrium between foreign intelligence collection and privacy would shift toward unilateral executive discretion.
% FOUNDING_PROBLEM: Unchecked executive exercise of general search and surveillance power against individuals, exemplified by colonial writs of assistance, which the Fourth Amendment was ratified to prohibit.
% FOUNDING_PROBLEM_CORROBORATION: Independent constitutional scholars and federal judiciary attest through Fourth Amendment jurisprudence (e.g., Katz, Carpenter) that the founding problem of arbitrary executive search persists into the digital age. The executive branch attests the problem is managed by statutory minimization procedures, but this attestation originates from the seat that operationally benefits from the contested reading. Corroboration from outside the beneficiary set is present in the judicial and academic record.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__constitutional_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__constitutional_floor_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__constitutional_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fisa_702_statutory_text__constitutional_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__constitutional_floor_reading, 0.25, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).
:- end_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.25 because the constitutional floor imposes moderate compliance costs on the executive (legal preparation, delay, probable cause showing) but does not extinguish the surveillance mission. Suppression at 0.35 reflects the procedural coercion of the warrant requirement; it is not violent but is structurally binding on institutional actors. Accessibility collapse is high (0.80) because, within the U.S. legal framework, alternatives to the Fourth Amendment warrant clause are legally foreclosedâCongress cannot statutorily authorize warrantless searches of U.S. person content. Resistance at 0.55 captures persistent executive-branch litigation, legislative advocacy, and administrative resistance to this reading. Theater ratio is modest (0.20): the warrant application process contains performative elements, but this reading demands substantive individualized probable cause, which resists pure theatricality. The measurement series share one time grid (0â16) to prevent misaligned temporal substitution.
 *
 * PERSPECTIVAL GAP:
 *   The executive surveillance agencies (payer seat) experience the constraint as extraction: every U.S. person content query requires legal friction, probable cause documentation, and delay. U.S. persons (beneficiary seat) experience it as protective coordination against arbitrary search. The FISA Court (agenda-setter) experiences it as a grant of institutional authority and interpretive role. The engine computes divergent per-seat classifications from these structural asymmetries; the payer seat will compute toward snare/tangled_rope territory while the beneficiary seat computes toward rope/mountain protection.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. persons are declared beneficiaries with constrained exit; their directionality is near the beneficiary pole (low d), meaning effective extraction is damped into constitutional subsidy and privacy protection. Executive surveillance agencies are declared victims/payers with constrained exit; their directionality is near the target pole (high d), amplifying effective extraction into substantial operational burden. The FISA Court, as agenda-setter with analytical exit, sits near the beneficiary/agenda side because the constraint's existence empowers its gatekeeping function. Congress and civil liberties organizations, as observers, sit outside the direct extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by separating the constitutional warrant requirement (genuine coordination protecting against arbitrary search) from either pure executive discretion (a snare on U.S. persons if unconstrained) or a toothless statutory minimization rule (a rope with no enforcement). The Tangled Rope classification captures that judicial oversight is real coordinationâprobable cause review is substantiveâwhile the cost is asymmetrically borne by executive operational tempo. If the warrant process were degraded to a rubber stamp (rising theater_ratio), the constraint would drift toward Piton; if the executive captured the FISA Court, shifting the agenda_setter role toward beneficiary status at payer expense, it would drift toward Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_fisa_702,
    'This constraint instantiates the constitutional_floor_reading of kernel fisa_702_statutory_text; the sibling incidental_collection_reading treats 702 queries of U.S. person content as foreign-intelligence collection exempt from warrant, while foreign_target_strict_reading treats statutory language as dispositive. Does the constitutional warrant requirement logically foreclose the incidental_collection_reading or merely exert structural pressure on it?',
    'Supreme Court adjudication of a direct challenge to warrantless 702 U.S. person queries, or legislative amendment codifying one reading.',
    'If foreclosed, the constitutional floor reading becomes the dominant legal framework and epsilon shifts toward compliance-cost extraction from the executive; if coexisting, the kernel remains contested with cyclical enforcement variation and seat divergence persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_fisa_702, conceptual, 'Committer omega documenting the kernel reading contest for FISA 702 and the foreclosure boundary between readings.').

omega_variable(
    us_person_boundary_ambiguity,
    'Does the ''U.S. person'' category in 702 map cleanly onto the Fourth Amendment''s protected scope, or do dual-nationals, permanent residents, and entities create boundary ambiguity that bleeds the warrant requirement''s protection?',
    'Judicial line-drawing in specific cases testing the constitutional floor against novel fact patterns (dual-status individuals, corporate entities, protected foreign entities).',
    'Boundary ambiguity would raise extractiveness for groups near the margin who lose clear protection; definitional clarity would stabilize the constraint''s seat classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_person_boundary_ambiguity, conceptual, 'Boundary ambiguity in U.S. person definition under the constitutional floor reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__constitutional_floor_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fisa_tr_t4, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(fisa_tr_t8, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(fisa_tr_t12, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(fisa_tr_t16, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 16, 0.2).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(fisa_be_t4, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement(fisa_be_t8, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(fisa_be_t12, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 12, 0.24).
narrative_ontology:measurement(fisa_be_t16, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 16, 0.25).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(fisa_702_statutory_text__constitutional_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__constitutional_floor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__incidental_collection_reading).

% DUAL FORMULATION NOTE:
% The fisa_702_statutory_text kernel decomposes into three structurally distinct constraints because the label 'FISA 702' conflates: (1) the constitutional floor reading (Fourth Amendment warrant requirement for U.S. person content queries), (2) the foreign target strict reading (statutory collection limits), and (3) the incidental collection reading (warrantless query of incidentally collected U.S. person data). Each has a different epsilon referent, beneficiary structure, and classification. This story authors the constitutional floor reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
