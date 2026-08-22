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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Dual-Register Ritual Encoding of Catastrophe Memory (Hybrid Encoding Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Ceremonial practice in catastrophe-exposed communities carries two kinds
 *   of content in one repeated form: the boundary-stories that mark who the
 *   community is, and the embedded instructions — when to move, where to
 *   gather, which ground is safe, how to rebuild — that past disasters paid
 *   for. This file instantiates ONE reading of the contested kernel
 *   catastrophe_memory_survival: the hybrid encoding reading, on which
 *   survival depends on both registers jointly. Per the epsilon-invariance
 *   discipline, the sibling readings (symbol-only continuity;
 *   competence-transmission only) are separate constraints in separate files
 *   and are not averaged into this one; the contest is routed to omega
 *   variables and the kernel_context note. The epsilon referent is the
 *   standing arrangement under contest — communities maintaining
 *   dual-register practice without theoretical resolution of which register
 *   'really' matters — assessed by this reading's own lights: both registers
 *   do real work, nothing material is transferred out of the practicing
 *   communities, and the only costs the arrangement imposes fall on analysts
 *   whose single-register frameworks fail against it. Claimed type and
 *   metrics are authored independently: the claim is rope (a coordination
 *   solution to a generational memory-transmission problem, alternatives
 *   unsuppressed, participants net beneficiaries), while the metrics describe
 *   low-extraction, lightly-resistant, low-theater operation with mild drift
 *   as heritage commodification layers performance onto practice.
 *
 * KEY AGENTS:
 *   - catastrophe_exposed_heritage_communities: primary beneficiary (organized/generational) — maintains both registers; the continuity benefits and the rehearsal costs both land here
 *   - ritual_officiants_and_elders: transmission agents and beneficiaries (moderate/generational) — administer sequencing and correction; authority bound to the practice
 *   - binary_classifying_analysts: cost-bearers (institutional/biographical) — single-register research programs fail against the dual structure; nothing material is extracted, framework revision remains open
 *   - disaster_memory_scholars: analytical observer (analytical/analytical) — documents cases where both registers are jointly load-bearing; the seat from which the full two-register structure is visible
 *   - secular_emergency_planners: excluded (institutional/biographical) — absent from the interpretive conversation; would press the operationalization question their absence leaves unasked
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__hybrid_encoding_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, 0.12).
domain_priors:theater_ratio(catastrophe_memory_survival__hybrid_encoding_reading, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__hybrid_encoding_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_survival__hybrid_encoding_reading, "Dual-Register Ritual Encoding of Catastrophe Memory (Hybrid Encoding Reading)").
narrative_ontology:topic_domain(catastrophe_memory_survival__hybrid_encoding_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__hybrid_encoding_reading, '96f2fe8d-6f4d-4a52-8621-9445a326c6a2').
narrative_ontology:cs_kernel_codification('96f2fe8d-6f4d-4a52-8621-9445a326c6a2', distributed).
narrative_ontology:cs_authority_grounding('96f2fe8d-6f4d-4a52-8621-9445a326c6a2', distributed).
narrative_ontology:cs_reading_relation('96f2fe8d-6f4d-4a52-8621-9445a326c6a2', catastrophe_memory_survival__symbol_survival_reading, forecloses).
narrative_ontology:cs_reading_relation('96f2fe8d-6f4d-4a52-8621-9445a326c6a2', catastrophe_memory_survival__competence_transmission_reading, forecloses).
narrative_ontology:cs_axiom('96f2fe8d-6f4d-4a52-8621-9445a326c6a2', foundational, survival_requires_both_registers).
narrative_ontology:cs_axiom_status(survival_requires_both_registers, holdable).
narrative_ontology:cs_axiom_grounding('96f2fe8d-6f4d-4a52-8621-9445a326c6a2', survival_requires_both_registers, empirically_contingent).
narrative_ontology:cs_axiom('96f2fe8d-6f4d-4a52-8621-9445a326c6a2', secondary, unbundling_registers_defeats_transmission).
narrative_ontology:cs_axiom_status(unbundling_registers_defeats_transmission, holdable).
narrative_ontology:cs_axiom_grounding('96f2fe8d-6f4d-4a52-8621-9445a326c6a2', unbundling_registers_defeats_transmission, empirically_contingent).
narrative_ontology:cs_reference_frame('96f2fe8d-6f4d-4a52-8621-9445a326c6a2', dual_register_survival_infrastructure).
narrative_ontology:cs_drift_state('96f2fe8d-6f4d-4a52-8621-9445a326c6a2', contemporary_disaster_anthropology_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('96f2fe8d-6f4d-4a52-8621-9445a326c6a2', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_exposed_heritage_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, ritual_officiants_and_elders).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, binary_classifying_analysts).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__hybrid_encoding_reading, dual_register_encoding_hypothesis).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__hybrid_encoding_reading, cultural_memory_selection_dynamics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities on flood plains, seismic margins, cyclone coasts, and fire-prone landscapes whose ceremonial life carries both the stories that mark who they are and the embedded instructions previous disasters paid for: when to move to high ground, which wells survive, how to rebuild, which kin owe whom shelter. Every cycle costs them rehearsal time, goods, and the labor of teaching the young; the return arrives irregularly, as lives and continuity when the next event lands. Leaving the practice would mean dissolving the web of obligation and recognition that holds them together as the community they are, so exit is not a live option in the way relocation is.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_exposed_heritage_communities, beneficiary,
    organized, generational, identity_locked, regional).

% The people who lead the ceremonies and carry the transmission: they set sequencing, correct errors in performance, and teach the young. Standing and authority within the community flow to them; the labor of keeping both the story and the instructions intact across generations flows from them. Their authority is bound up with the practice itself, so they cannot hand it on without handing on themselves.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, ritual_officiants_and_elders, beneficiary,
    moderate, generational, identity_locked, regional).

% Scholars and framework-builders who classify ceremonial practice under a single register — reading it as pure symbolic expression or as pure technique — and whose research programs, curricula, and published claims are organized around that single-register commitment. When the practice they classify keeps doing work their framework assigns to the other register, their explanations fail and revision costs a career's accumulated materials. Nothing material is taken from them; the cost is epistemic, and moving to a two-register framework remains open to them at any time.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, binary_classifying_analysts, payer,
    institutional, biographical, constrained, global).

% Researchers in disaster anthropology and collective-memory studies who document cases in which ceremonial practice preserved operationally accurate hazard knowledge alongside identity content, and who treat the two registers as jointly load-bearing. They work from community testimony, geological and archival records, and comparative case studies; their seat is the one from which the full two-register structure is visible.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, disaster_memory_scholars, observer,
    analytical, biographical, analytical, global).

% Government agencies and relief organizations that build evacuation, early-warning, and reconstruction systems from instrumented data. They have historically treated ceremonial hazard knowledge as ornament and left it out of plans; where they have begun consulting it, they ask what in the practice is actionable and by whom. They are largely absent from the scholarly venues where the practice is interpreted, and their absence leaves the operationalization question unasked in exactly the places where the practice gets characterized.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, secular_emergency_planners, excluded,
    institutional, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__hybrid_encoding_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__hybrid_encoding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps two kinds of communal memory alive across generational turnover and catastrophic disruption by bundling them into a single repeatedly rehearsed practice: the boundary-stories that say who the community is, and the embedded instructions — timing, locations, protocols, adaptation rules — that past disasters paid for. Bundling solves a transmission problem neither kind of memory solves alone: instruction without story loses the salience that drives rehearsal; story without instruction loses the content that makes rehearsal worth surviving.
% TRANSFER_FUNCTION: Moves encoded hazard knowledge and boundary-norms from elders to the young through participation; moves rehearsal labor and goods from community members into the ceremonial cycle; and, in the scholarly register, moves interpretive labor toward frameworks that can hold both registers at once. Nothing material is transferred out of the practicing communities to any external seat.
% ABSENT_VOICES: The practicing communities are mostly absent from the academic venues where their practice is classified; secular emergency-management agencies are absent from ritual-studies debates and would ask the operationalization question their absence leaves unasked; and within communities, the members who bear the heaviest rehearsal labor are rarely the ones consulted when outside observers characterize what the practice is for.
% DISAPPEARANCE_RATIONALE: If the bundled practice vanished overnight, the next flood, storm, or seismic event would arrive against communities holding either their identity-stories without the instructions or the instructions without the salience to rehearse them; reconstruction would reorganize around whatever explicit records survived, and communities without records would re-learn hazard knowledge at the price the original events charged.
% FOUNDING_PROBLEM: Keeping a community's identity and its survival-relevant knowledge intact across generations in environments where catastrophe can destroy records, kill specialized transmitters, and scatter the population — when neither written archives nor occasional instruction reliably survive the events that make the knowledge needed.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set by the geology and archaeology that independently confirm the encoded content — paleo-geological sea-level records matching Aboriginal oral traditions, tsunami sediment layers matching Japanese stone markers and inundation lore — and by disaster-risk-reduction bodies and national geological surveys that now consult oral tradition as hazard input. Proponents of single-register frameworks do not attest the two-register structure, but the accuracy of the encoded knowledge is corroborated by researchers with no stake in the practice.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__hybrid_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__hybrid_encoding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__hybrid_encoding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_survival__hybrid_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 0.15, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is low (0.15) because the arrangement transfers nothing out of the practicing communities: its costs are rehearsal labor and goods recycled inside the community, plus epistemic costs to analysts whose frameworks misclassify — real, but captured by no seat. Suppression is low (0.12) because nothing enforces the dual-register structure: single-register frameworks persist as live research programs, and alternative transmission technologies (writing, schooling) coexist with rather than being suppressed by the bundled form. Theater is low (0.14) because the function is real — the encoded content is operationally accurate often enough to be corroborated independently by geology — though the series rises as heritage commodification and state folklorization convert some practice into performance for outsiders. Accessibility collapse is moderate-low (0.32): once the dual-register structure is understood, single-register explanations lose completeness but remain live as partial descriptions, and the sibling readings persist as held positions. Resistance is moderate (0.30): framework-committed analysts resist the two-register synthesis, and some communities resist external characterization of their practice altogether. The measurement series run on one shared time grid (decades 0 through 60, seven points, both metrics at every point). No suppression_requirement series is authored: there is no enforcement-capacity dynamic to trace — suppression is stable and low, and the scalar carries it. The mild upward drift in both series is commodification-driven, not enforcement-driven.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the practicing community's seat the arrangement is not experienced as a constraint at all — it is continuity itself, and its costs are invisible as costs because they are the community's ordinary life. From the single-register analyst's seat the same structure operates as a refutation machine: every well-documented case where the 'other' register does survival work is a cost their framework must absorb. From the emergency-planner's seat the structure is a legibility problem: actionable content locked in a form their instruments do not read. The analytical seat sees the joint structure. The engine computes these per-seat classifications from the structural data; the divergence between the near-beneficiary community seat and the cost-bearing analyst seat is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: the practicing communities and their officiants sit near the beneficiary end (d near 0.0) — the arrangement subsidizes their continuity, and their identity-lock is the lock of participants in a practice that constitutes them, not the lock of extraction targets. The binary-classifying analysts are declared victims, and the derivation will read them toward the target end; that placement is approximately right — they do bear the arrangement's costs — with one qualification the low scalar epsilon already carries: the cost is falsification of their frameworks, not transfer of resources, and their exit (adopting a two-register framework) is open at any time, so they are cost-bearers with open revision rather than trapped payers. No directionality override is authored: the derivation's victim-to-cost-bearer placement is descriptively defensible, and the low base extraction plus the analysts' open exit keep their effective extraction modest. The excluded planners feed no directionality — they sit outside the arrangement's operation, which is precisely what their absence consists in.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. Read from the victim declaration alone, the arrangement can look like extraction with analysts as the payers; but the extraction is epistemic, uncaptured, and avoidable at will — the signature of a framework losing an argument, not of a structure collecting rent. Read from the receipt surface alone (diffuse gains, prohibitive fixing cost), the arrangement can look like the degraded pattern in which nobody captures and nobody can afford change; but that pattern's test — an administrator who could change it and does not because the cost exceeds what the administrator bears — fails here: there is no administrator, maintenance is distributed and motivated, theater is low, and the founding problem (catastrophe exposure) is live. The founding problem is corroborated from outside the beneficiary set by geology and by disaster-risk bodies; mandatrophy is not resolved, and the arrangement persists because it works, not because it once worked.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint instantiates the hybrid_encoding_reading of the catastrophe_memory_survival kernel — the claim that the ceremonial survival function is jointly carried by symbolic boundary-maintenance and embedded practical knowledge. Would instantiating either sibling reading instead (symbol-only continuity of practice, or competence-transmission only) produce a structurally different constraint, and where exactly does this reading''s commitment diverge from theirs?',
    'Adjudication by the sibling files'' own structural data: compare victim sets, beneficiary structure, and epsilon across the three readings of the shared kernel. The disagreement is located in which register is load-bearing for survival; this file authors epsilon for the standing arrangement (communities maintaining both registers) assessed by this reading''s lights only.',
    'If a sibling reading is adopted, this file''s victim set (analysts forced into binary classification) and beneficiary structure (communities maintaining both registers without theoretical resolution) reorganize: the symbol-only reading has no practical register whose loss it must explain, and the competence-only reading has no boundary-maintenance function coordinated with transmission.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: this file is one reading of a contested kernel; the sibling readings are separate constraints, not positions inside this one.').

omega_variable(
    register_load_bearing_ambiguity,
    'Is each register genuinely load-bearing — does loss of the practical register cause survival failure in the next hazard event, and does loss of the symbolic register cause the rehearsal salience (and hence the knowledge) to atrophy — or is one register doing the work while the other rides along?',
    'Natural experiments: communities that lost one register but not the other (post-missionization communities retaining ceremony with thinned content; secularized communities retaining recorded hazard knowledge without rehearsal practice), tracked against outcomes in subsequent events.',
    'If only one register is load-bearing, this reading collapses toward the corresponding sibling, the dual-register beneficiary claim weakens, and the unbundling prohibition (this reading''s secondary axiom) loses its ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(register_load_bearing_ambiguity, empirical, 'Whether both registers are genuinely load-bearing or one rides on the other.').

omega_variable(
    intra_community_cost_distribution,
    'Does the rehearsal and transmission labor of maintaining both registers fall evenly across community members, or is it concentrated on subgroups (by gender, age-grade, or status) who bear the costs while the community as a whole collects the continuity benefits?',
    'Ethnographic cost accounting of who performs, who teaches, who is excused, and who holds the authority the practice confers, across a sample of catastrophe-exposed communities.',
    'If costs concentrate on powerless subgroups while benefits accrue community-wide, base extraction rises above the low level authored here and the arrangement''s character shades toward a structure with internal extraction — the community-level victim and beneficiary declarations would need subgroup decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intra_community_cost_distribution, empirical, 'Whether rehearsal costs concentrate on powerless subgroups inside the beneficiary communities.').

omega_variable(
    modern_substitutability,
    'Do modern recording and schooling technologies now transmit hazard knowledge and identity content well enough that the bundled ceremonial form is no longer the only — or the best — transmission technology, making the arrangement transitional rather than standing?',
    'Comparative retention studies: hazard-knowledge retention and community continuity in communities that shifted to explicit archival and curricular transmission versus communities that kept bundled practice, measured across real events.',
    'If substitutes perform equivalently, the arrangement''s persistence is partly inertial, its fixing cost drops from prohibitive toward cheap, and the constraint drifts toward a transitional-support profile with a live sunset question; if substitutes underperform — records lost in the same events that create the need, content without salience atrophying — the bundling remains load-bearing and the low extraction is the true price of the coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_substitutability, empirical, 'Whether modern recording and schooling make the bundled ceremonial form substitutable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__hybrid_encoding_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_register_reading_tr_t0, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(dual_register_reading_tr_t0, observed).
narrative_ontology:measurement(dual_register_reading_tr_t10, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement_basis(dual_register_reading_tr_t10, observed).
narrative_ontology:measurement(dual_register_reading_tr_t20, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement_basis(dual_register_reading_tr_t20, observed).
narrative_ontology:measurement(dual_register_reading_tr_t30, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement_basis(dual_register_reading_tr_t30, observed).
narrative_ontology:measurement(dual_register_reading_tr_t40, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement_basis(dual_register_reading_tr_t40, observed).
narrative_ontology:measurement(dual_register_reading_tr_t50, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement_basis(dual_register_reading_tr_t50, observed).
narrative_ontology:measurement(dual_register_reading_tr_t60, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 60, 0.14).
narrative_ontology:measurement_basis(dual_register_reading_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(dual_register_reading_be_t0, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(dual_register_reading_be_t0, observed).
narrative_ontology:measurement(dual_register_reading_be_t10, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement_basis(dual_register_reading_be_t10, observed).
narrative_ontology:measurement(dual_register_reading_be_t20, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement_basis(dual_register_reading_be_t20, observed).
narrative_ontology:measurement(dual_register_reading_be_t30, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 30, 0.13).
narrative_ontology:measurement_basis(dual_register_reading_be_t30, observed).
narrative_ontology:measurement(dual_register_reading_be_t40, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 40, 0.14).
narrative_ontology:measurement_basis(dual_register_reading_be_t40, observed).
narrative_ontology:measurement(dual_register_reading_be_t50, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement_basis(dual_register_reading_be_t50, observed).
narrative_ontology:measurement(dual_register_reading_be_t60, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 60, 0.15).
narrative_ontology:measurement_basis(dual_register_reading_be_t60, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_survival__hybrid_encoding_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__hybrid_encoding_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__competence_transmission_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'ritual helps communities survive catastrophe' covers three structurally distinct claims, decomposed per the epsilon-invariance principle into a three-file family sharing the kernel catastrophe_memory_survival: the symbol_survival_reading (survival as continuity of practice; symbolic register alone), the competence_transmission_reading (survival as transmitted technique; practical register alone), and this file's hybrid_encoding_reading (joint necessity of both registers). The readings carry different epsilon because they defend different things: each single-register reading bears the contest costs of denying the other register's documented work, while this reading's low epsilon reflects that it denies no documented function and extracts from no seat. This file links both siblings; the upstream empirical record (geological and archaeological corroboration of encoded hazard knowledge) is cited by all three members of the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
