% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__hybrid_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__hybrid_encoding_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__hybrid_encoding_reading
 *   human_readable: Dual-Register Encoding Norm for Catastrophe-Memory Ritual (Hybrid Encoding Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Ancient tsunami stones on the Sanriku coast, flood-plain festival
 *   calendars, expulsion commemorations with household protocols attached:
 *   across unrelated traditions, the rites that reliably carry catastrophe
 *   memory across generations fuse two layers in the same performance — a
 *   symbolic layer that marks who the community is and what it lost, and an
 *   instructional layer that moves watermarks, planting dates, storage rules,
 *   and mutual-obligation rosters into bodies that have never seen the
 *   disaster. A scholarly debate over how such memory aids survival has
 *   crystallized into three readings of a single kernel. This file authors
 *   the hybrid reading as a constraint: the operative norm — in community
 *   self-understanding and increasingly in the literature — that the two
 *   registers be treated as inseparable, such that accounts, archives, or
 *   curricula isolating one register fail. Its epsilon is authored low:
 *   little is taken from anyone, and the norm's principal cost falls on a
 *   small analytic population as foregone rather than transferred value.
 *   Constraint-family note (eps differ by referent, per the decomposition
 *   rule): the symbol_survival_reading's referent — preservation regimes that
 *   strip practice content down to displayable symbol — authors high epsilon
 *   against practice-bearing lineages; the competence_transmission_reading's
 *   referent — archiving technique while letting obligation lapse — authors
 *   moderate epsilon against tradition-bearers; this reading's referent, the
 *   anti-separation norm itself, authors low epsilon. The three files link
 *   through network.affects_constraints and are not averaged anywhere in this
 *   one. KEY AGENTS (by structural relationship): -
 *   post_catastrophe_ritual_communities: Primary beneficiary
 *   (organized/constrained) — holds the fused practice the norm protects -
 *   binary_framework_analysts: Primary target (institutional/identity_locked)
 *   — bears the norm's epistemic-career costs - ritual_officiants_and_elders:
 *   Practice-level agenda setter (moderate/identity_locked) — composes and
 *   leads the fused rites - memory_studies_institutions: Discursive agenda
 *   setter (institutional/arbitrage) — allocates pages, grants, and
 *   curricular space - secular_emergency_planning_agencies: Excluded voice
 *   (institutional/mobile) — holds parallel hazard knowledge outside the
 *   debate - disaster_historians_and_archaeologists: Analytical observer
 *   (institutional/analytical) — produces the retention and mortality record
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__hybrid_encoding_reading, 0.22).
domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, 0.3).
domain_priors:theater_ratio(catastrophe_memory_survival__hybrid_encoding_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__hybrid_encoding_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_survival__hybrid_encoding_reading, "Dual-Register Encoding Norm for Catastrophe-Memory Ritual (Hybrid Encoding Reading)").
narrative_ontology:topic_domain(catastrophe_memory_survival__hybrid_encoding_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__hybrid_encoding_reading, '574ebc44-27cb-4638-a971-232ed99a819c').
narrative_ontology:cs_kernel_codification('574ebc44-27cb-4638-a971-232ed99a819c', distributed).
narrative_ontology:cs_authority_grounding('574ebc44-27cb-4638-a971-232ed99a819c', distributed).
narrative_ontology:cs_reading_relation('574ebc44-27cb-4638-a971-232ed99a819c', catastrophe_memory_survival__symbol_survival_reading, forecloses).
narrative_ontology:cs_reading_relation('574ebc44-27cb-4638-a971-232ed99a819c', catastrophe_memory_survival__competence_transmission_reading, influences).
narrative_ontology:cs_axiom('574ebc44-27cb-4638-a971-232ed99a819c', foundational, survival_requires_register_fusion).
narrative_ontology:cs_axiom_status(survival_requires_register_fusion, holdable).
narrative_ontology:cs_axiom_grounding('574ebc44-27cb-4638-a971-232ed99a819c', survival_requires_register_fusion, empirically_contingent).
narrative_ontology:cs_axiom('574ebc44-27cb-4638-a971-232ed99a819c', secondary, register_separation_is_destructive_not_clarifying).
narrative_ontology:cs_axiom_status(register_separation_is_destructive_not_clarifying, holdable).
narrative_ontology:cs_axiom_grounding('574ebc44-27cb-4638-a971-232ed99a819c', register_separation_is_destructive_not_clarifying, empirically_contingent).
narrative_ontology:cs_reference_frame('574ebc44-27cb-4638-a971-232ed99a819c', dual_register_necessity_frame).
narrative_ontology:cs_drift_state('574ebc44-27cb-4638-a971-232ed99a819c', contemporary_integrative_turn, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('574ebc44-27cb-4638-a971-232ed99a819c', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, post_catastrophe_ritual_communities).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, binary_framework_analysts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, memory_studies_institutions).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__hybrid_encoding_reading, dual_register_complementarity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities that lived through floods, tsunamis, fires, or expulsions and keep annual rites that do two things at once: mark who they are and what was lost, and carry operational instructions — where the water reached, when to move the livestock, how to cure the stores, which households check on which elders. The two layers are fused in the same songs, processions, and prohibitions; elders resist splitting them because each layer gives the other its force. Dropping either layer is possible but visibly degrades what the rite accomplishes, so the communities hold both and feel no need to resolve academically which layer really matters.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, post_catastrophe_ritual_communities, beneficiary,
    organized, generational, constrained, global).

% Scholars trained in single-register programs — ritual as symbolic communication, or ritual as technique transmission — whose classifications no longer pass review unmarked. Referees ask what the other register is doing; grant panels favor integrative designs. Rebuilding a framework mid-career is costly, and many experience the integrative expectation as a penalty on work they spent decades perfecting. Leaving the field entirely is conceivable but would sever professional identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, binary_framework_analysts, payer,
    institutional, biographical, identity_locked, global).

% The people who compose, schedule, and lead the rites. They decide which warnings enter the liturgy, which dates anchor the calendar, which stories name the dead. Their office exists only inside the fused practice; setting the two-layer pattern year after year is the job itself. They answer to their congregations and predecessors, not to any scholarly body.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, ritual_officiants_and_elders, agenda_setter,
    moderate, generational, identity_locked, local).

% Journals, departments, museums, and funders that host the debate over how ritual memory works. They allocate pages, chairs, gallery space, and grants; whichever framing is ascendant shapes call-for-papers themes and exhibition design. They can pivot between framings at low cost and currently find the integrative framing productive — it generates conferences, cross-listed courses, and funded collaborations.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, memory_studies_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__hybrid_encoding_reading, memory_studies_institutions, beneficiary).

% Civil-protection bodies that map hazards and issue warnings through official channels. They do not attend the ritual-theory debate and rarely consult liturgies, though the knowledge embedded in commemorative practice overlaps their hazard maps. From where they stand, survival-critical content belongs in audited systems; some officers privately note that villages with strong commemorative practice evacuate earlier than the models predict.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, secular_emergency_planning_agencies, excluded,
    institutional, biographical, mobile, national).

% Researchers who read tsunami stones, flood strata, commemorative inscriptions, and parish records to reconstruct which lineages kept both layers of practice across generations and what happened to each. They take no side in the framing dispute but produce the retention and mortality figures both camps cite.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, disaster_historians_and_archaeologists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__hybrid_encoding_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__hybrid_encoding_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective problem of keeping two fragile goods together across generations: the identity-boundary content that motivates continued observance, and the operational content that actually saves lives. Neither survives alone for long — symbol without practice loses its referents and becomes empty observance; technique without ceremony loses the obligation structure that transmits it to people with no living memory of the disaster. The norm coordinates scholars, archivists, and practitioners around maintaining the fusion.
% TRANSFER_FUNCTION: Moves little materially. What it moves is epistemic and temporal: attention and legitimacy away from single-register programs toward integrative ones, and survival-relevant knowledge forward across generations by welding it to obligations — descendant households inherit duties (walk the high road on the anniversary, check the elder, keep the store above the marked line) they did not choose and would not otherwise know.
% ABSENT_VOICES: Secular emergency-planning authorities would object that life-critical content belongs in audited warning systems rather than liturgy, and that the debate never consulted them; non-literate tradition-bearers are described far more often than they describe themselves — few sit on the editorial boards and panels where the kernel is adjudicated. Both absences flatter the integrative consensus: its unanimity partly reflects who was in the room.
% DISAPPEARANCE_RATIONALE: Community practice would continue — it predates the norm and does not depend on it — so the ritual world itself would stand. What would unravel is the coordination layer built on the norm: integrated memorial-hazard inventories, cross-disciplinary preservation programs, and the research agenda correlating register-retention with mortality. Communities say that layer is the scholars' world, not theirs; analysts and funders say the integrative layer is what keeps the registers welded as transmission conditions degrade. The parties dispute whether that counts as the world rearranging.
% FOUNDING_PROBLEM: Recurring preservation failures in which exactly one register survived: communities that kept the memorial but forgot why the stone stood below the ridge line; archives that kept the technique manual with no remaining obligation to perform it before the next flood. The norm was articulated to stop analysts, archivists, and curators from decomposing what had to stay welded.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: disaster archaeology and after-action epidemiology corroborate the underlying problem — documented cases where register-loss preceded elevated mortality, and agency reports noting earlier evacuation in villages retaining commemorative-warning practice. No one outside the reading's own camp corroborates the stronger claim that the scholarly norm itself, as opposed to the fused practice, preserves survival value; that portion rests on the reading's internal argument.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__hybrid_encoding_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__hybrid_encoding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_survival__hybrid_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).
:- end_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.22 at interval end) because the norm takes little from anyone: communities pay nothing they would not already pay, and the main cost — blocked single-register careers — is foregone value rather than transferred rent (see omega analyst_cost_receipt_status). Suppression (0.30) is discursive, not structural: referee expectations and panel preferences discourage single-register submissions; it is authored as a raw structural property and is not scaled by power or scope. Theater (0.15) is low because the norm's activity — insisting both registers appear in accounts and curricula — is functional; the slow rise tracks 'integrative' becoming a label some work wears without performing. Accessibility collapse (0.45) is moderate: the sibling readings remain publishable positions, so alternatives contract without vanishing. Resistance (0.35) reflects active pushback from identity-invested analysts and indifference from practitioners who never asked for the theory. The claimed type rope is authored from structure — a genuine coordination problem (the registers die apart), net-beneficiary participants, live alternatives — independently of the metrics; the engine computes per-seat types from the structural data. Coordination type identity_coordination is declared because the norm's dominant function is boundary and membership maintenance for communities of memory; the FNL gaming check passes because measured extraction sits near the type floor rather than hiding beneath identity rhetoric. All three series share one time grid (points 0-24, step 4) so no metric row is sampled against a substituted scalar.
 *
 * PERSPECTIVAL GAP:
 *   From the analyst seat the arrangement computes as enforced orthodoxy taxing a research program; from the community seat it computes as protection of an inheritance that predates and outlives the theory; from the institution seat it is a productive controversy allocating attention and funding; from the excluded agency seat it is barely visible academicism sitting on top of usable hazard data. The engine derives these divergences from power, exit, and role — the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (post_catastrophe_ritual_communities) derive low directionality — the norm subsidizes the practice they already run. Declared victims (binary_framework_analysts) derive high directionality, amplified by identity_locked exit: their professional selves are fused to the frameworks the norm retires, so they sit near the full-target end despite bearing only epistemic-career costs. One override is authored: ritual_officiants_and_elders are the story's only moderate-power seat, and the derivation's agenda-setter fallback would place them near symmetric; structurally they sit near the beneficiary end (d 0.15) because the fused practice is their office and the norm shields it from decomposition. memory_studies_institutions derive a moderately low d from their secondary beneficiary position; no override is authored for them because an institutional-power override would also strike the analyst seat, which must remain near-target.
 *
 * MANDATROPHY ANALYSIS:
 *   Two misclassification traps surround this constraint. First, survivorship bias makes fused ritual look like natural law: among traditions that survived catastrophe, both registers are present, because lineages that let either lapse left fewer descendants and fewer records — a mountain reading would naturalize a selection effect. Second, the audible complaints of the analyst seat invite a snare reading; but the costs analysts bear are destroyed, not captured (gain_flow is affirmatively diffuse, and fixing the norm is cheap for the distributed scholarly community that could abandon it), so no seat collects them. The rope classification holds both traps off: genuine coordination function, net-beneficiary participants, alternatives alive. Mandatrophy is not resolved because the founding problem is live — digitization, diaspora, and climate-driven hazard escalation are splitting registers again (memorial sites without drills, drill manuals without commemorations) — so the anti-decomposition mandate retains function and no sunset applies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is one reading of the kernel catastrophe_memory_survival (reading: hybrid_encoding_reading). What structurally changes if a sibling reading is adopted instead?',
    'Cross-file comparison of the three reading stories: each sibling''s victim set, beneficiary set, and epsilon referent are authored independently; adopting a sibling reroutes directionality and recomputes every seat.',
    'Under symbol_survival_reading the harmed set becomes practice-bearing lineages stripped to displayable symbol and epsilon rises sharply; under competence_transmission_reading the beneficiary set becomes risk-exposed populations and symbolic content is demoted to payload. Either adoption dissolves this file''s low-epsilon profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: which kernel, which reading this file instantiates, and what each sibling would change.').

omega_variable(
    dependency_claim_location,
    'Is the contest between readings located at the sufficiency claim (what survival depends on) or merely at compositional emphasis (which register matters more)?',
    'Formalize each sibling''s dependency claim and test the set for joint satisfiability within one framework; adversarial seminars between the reading camps.',
    'If located at sufficiency, the authored forecloses edge to symbol_survival_reading stands; if merely emphasis, that edge softens to coexists_with and the kernel contest becomes a preference dispute rather than a logical one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependency_claim_location, conceptual, 'Where the disagreement between readings is actually located.').

omega_variable(
    fusion_survival_causal_weight,
    'How much of the observed survival differential traces to register fusion specifically, rather than to either register alone or to confounds such as wealth, terrain, and state warning infrastructure?',
    'Matched-community studies correlating retention profiles of both registers with disaster mortality, controlling for exposure and resources; natural experiments where one register was forcibly separated by ban or archive-only preservation.',
    'If fusion adds little beyond the best single register, this reading collapses toward the stronger sibling and its coordination claim loses its warrant; if fusion is decisive, the low-extraction protective profile is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fusion_survival_causal_weight, empirical, 'Whether joint retention causally outperforms single-register retention.').

omega_variable(
    analyst_cost_receipt_status,
    'Are the costs borne by single-register analysts transferred to any seat (captured) or destroyed (foregone publications and revised careers benefiting no one)?',
    'Trace whether blocked reductive output frees journal space, grant share, or positions that rival programs absorb, versus simply vanishing.',
    'If captured, gain_flow names a seat and the arrangement drifts toward enforced extraction with an asymmetric beneficiary; if destroyed, receipt remains diffuse and the low-extraction coordination reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(analyst_cost_receipt_status, empirical, 'Whether the analyst seat''s costs are captured rent or destroyed value.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__hybrid_encoding_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cms_hybrid_enc_tr_t0, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement_basis(cms_hybrid_enc_tr_t0, observed).
narrative_ontology:measurement(cms_hybrid_enc_tr_t4, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 4, 0.06).
narrative_ontology:measurement_basis(cms_hybrid_enc_tr_t4, observed).
narrative_ontology:measurement(cms_hybrid_enc_tr_t8, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 8, 0.08).
narrative_ontology:measurement_basis(cms_hybrid_enc_tr_t8, observed).
narrative_ontology:measurement(cms_hybrid_enc_tr_t12, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement_basis(cms_hybrid_enc_tr_t12, observed).
narrative_ontology:measurement(cms_hybrid_enc_tr_t16, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement_basis(cms_hybrid_enc_tr_t16, observed).
narrative_ontology:measurement(cms_hybrid_enc_tr_t20, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement_basis(cms_hybrid_enc_tr_t20, observed).
narrative_ontology:measurement(cms_hybrid_enc_tr_t24, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 24, 0.15).
narrative_ontology:measurement_basis(cms_hybrid_enc_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(cms_hybrid_enc_be_t0, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(cms_hybrid_enc_be_t0, observed).
narrative_ontology:measurement(cms_hybrid_enc_be_t4, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 4, 0.11).
narrative_ontology:measurement_basis(cms_hybrid_enc_be_t4, observed).
narrative_ontology:measurement(cms_hybrid_enc_be_t8, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 8, 0.14).
narrative_ontology:measurement_basis(cms_hybrid_enc_be_t8, observed).
narrative_ontology:measurement(cms_hybrid_enc_be_t12, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 12, 0.16).
narrative_ontology:measurement_basis(cms_hybrid_enc_be_t12, observed).
narrative_ontology:measurement(cms_hybrid_enc_be_t16, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 16, 0.18).
narrative_ontology:measurement_basis(cms_hybrid_enc_be_t16, observed).
narrative_ontology:measurement(cms_hybrid_enc_be_t20, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement_basis(cms_hybrid_enc_be_t20, observed).
narrative_ontology:measurement(cms_hybrid_enc_be_t24, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 24, 0.22).
narrative_ontology:measurement_basis(cms_hybrid_enc_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(cms_hybrid_enc_su_t0, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(cms_hybrid_enc_su_t0, observed).
narrative_ontology:measurement(cms_hybrid_enc_su_t4, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 4, 0.12).
narrative_ontology:measurement_basis(cms_hybrid_enc_su_t4, observed).
narrative_ontology:measurement(cms_hybrid_enc_su_t8, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 8, 0.16).
narrative_ontology:measurement_basis(cms_hybrid_enc_su_t8, observed).
narrative_ontology:measurement(cms_hybrid_enc_su_t12, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 12, 0.2).
narrative_ontology:measurement_basis(cms_hybrid_enc_su_t12, observed).
narrative_ontology:measurement(cms_hybrid_enc_su_t16, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 16, 0.24).
narrative_ontology:measurement_basis(cms_hybrid_enc_su_t16, observed).
narrative_ontology:measurement(cms_hybrid_enc_su_t20, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 20, 0.27).
narrative_ontology:measurement_basis(cms_hybrid_enc_su_t20, observed).
narrative_ontology:measurement(cms_hybrid_enc_su_t24, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 24, 0.3).
narrative_ontology:measurement_basis(cms_hybrid_enc_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__hybrid_encoding_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__competence_transmission_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'how does ritual help communities survive catastrophe?' decomposes into three structurally distinct claims previously conflated under one label: identity persistence through symbolic continuity, competence transmission through embedded technique, and joint necessity of both. Each now has its own file, its own epsilon, and its own stakeholder structure. Edges run from this hybrid reading to both siblings because the hybrid frame currently sits upstream in legitimacy conditions: each sibling account must now answer what the other register contributes or concede partiality. The symbol reading additionally stands in logical tension with this reading's dependency claim (see cs_structure.reading_relations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_survival__hybrid_encoding_reading, moderate, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
