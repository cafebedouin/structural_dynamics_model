% ============================================================================
% CONSTRAINT STORY: naskh_principle__progressive_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__progressive_restriction, []).

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
 *   constraint_id: naskh_principle__progressive_restriction
 *   human_readable: Progressive Restriction Reading of Naskh (Divine Pedagogy Frame)
 *   domain: religious/legal/hermeneutical
 *
 * SUMMARY:
 *   Within Islamic legal theory, the label 'naskh' covers several
 *   structurally distinct accounts of what happens when commands differ
 *   across the order of revelation. This story instantiates ONE of them: the
 *   progressive_restriction reading, under which revelation deliberately
 *   educated the early community — first accommodating existing practice,
 *   then tightening — so that earlier permissive passages were transitional
 *   stages rather than enduring law, and no verse was ever invalidated. The
 *   reading performs a real service: it yields one operative command per
 *   topic from a text whose commands differ, while keeping the whole
 *   scripture valid. It also concentrates finality: whoever stands latest in
 *   the presumed sequence holds the field, and those who cite earlier
 *   permissive passages for contemporary practice find their best textual
 *   evidence ruled inadmissible. The ε referent is the standing hermeneutical
 *   arrangement itself — the rule that converts revelation-order into legal
 *   finality — assessed as it actually operates, not as the endorsed
 *   alternative (a harmonized plural law) would operate. KEY AGENTS (by
 *   structural relationship): classical_fiqh_establishment — administrator
 *   and primary collector of the arrangement's authority
 *   (institutional/identity_locked); qadis_and_muftis — adjudicators spared
 *   contradictory citations (powerful/constrained); ordinary_believers —
 *   recipients of coherent law who bear the final restrictions
 *   (moderate/identity_locked); reformist_citers_of_early_verses — organized
 *   challengers whose citations are reclassified as stage-quotation
 *   (organized/constrained); individual_permissive_practice_seekers —
 *   isolated believers corrected by teachers and family (powerless/trapped);
 *   textual_chronology_historians — excluded falsifiers of the chronology the
 *   whole arc depends on (moderate/mobile); comparative_hermeneutics_scholars
 *   — analytical observers of the contest (analytical/analytical).
 *
 * KEY AGENTS:
 *   - classical_fiqh_establishment: Administrator and principal collector — maintains the revelation-order tables, certifies operative rulings, and holds teaching authority constituted by the pedagogical frame (institutional/identity_locked)
 *   - qadis_and_muftis: Secondary beneficiary — adjudicate with a single operative ruling per topic and a clean chain of textual warrant (powerful/constrained)
 *   - ordinary_believers: Dual-positioned — receive coherent practice guidance and intact scripture, bear the compliance burden of the final restrictions (moderate/identity_locked)
 *   - reformist_citers_of_early_verses: Primary target among organized actors — their citations of earlier permissive passages are stripped of operative force (organized/constrained)
 *   - individual_permissive_practice_seekers: Primary target among unorganized actors — corrected socially, with no recourse inside the interpretive community (powerless/trapped)
 *   - textual_chronology_historians: Excluded voice — would contest the revelation-order on which the entire arc rests, but sit outside the seminary conversation (moderate/mobile)
 *   - comparative_hermeneutics_scholars: Analytical observer — documents which schools operationalize which account and how the balance shifted (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__progressive_restriction, 0.58).
domain_priors:suppression_score(naskh_principle__progressive_restriction, 0.52).
domain_priors:theater_ratio(naskh_principle__progressive_restriction, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, extractiveness, 0.58).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__progressive_restriction, tangled_rope).
narrative_ontology:human_readable(naskh_principle__progressive_restriction, "Progressive Restriction Reading of Naskh (Divine Pedagogy Frame)").
narrative_ontology:topic_domain(naskh_principle__progressive_restriction, "religious/legal/hermeneutical").

domain_priors:requires_active_enforcement(naskh_principle__progressive_restriction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__progressive_restriction, '5dc0779a-5abf-4357-a03d-d0b062ec3cc3').
narrative_ontology:cs_kernel_codification('5dc0779a-5abf-4357-a03d-d0b062ec3cc3', fixed_text).
narrative_ontology:cs_authority_grounding('5dc0779a-5abf-4357-a03d-d0b062ec3cc3', lineage).
narrative_ontology:cs_interpretation_layer_present('5dc0779a-5abf-4357-a03d-d0b062ec3cc3').
narrative_ontology:cs_reading_relation('5dc0779a-5abf-4357-a03d-d0b062ec3cc3', naskh_principle__classical_abrogation, coexists_with).
narrative_ontology:cs_reading_relation('5dc0779a-5abf-4357-a03d-d0b062ec3cc3', naskh_principle__contextual_harmonization, coexists_with).
narrative_ontology:cs_axiom('5dc0779a-5abf-4357-a03d-d0b062ec3cc3', foundational, later_restriction_is_final_divine_intent).
narrative_ontology:cs_axiom_status(later_restriction_is_final_divine_intent, holdable).
narrative_ontology:cs_axiom_grounding('5dc0779a-5abf-4357-a03d-d0b062ec3cc3', later_restriction_is_final_divine_intent, theological).
narrative_ontology:cs_axiom('5dc0779a-5abf-4357-a03d-d0b062ec3cc3', foundational, earlier_permissive_verses_are_transitional_accommodations).
narrative_ontology:cs_axiom_status(earlier_permissive_verses_are_transitional_accommodations, holdable).
narrative_ontology:cs_axiom_grounding('5dc0779a-5abf-4357-a03d-d0b062ec3cc3', earlier_permissive_verses_are_transitional_accommodations, theological).
narrative_ontology:cs_reference_frame('5dc0779a-5abf-4357-a03d-d0b062ec3cc3', progressive_pedagogical_sequence).
narrative_ontology:cs_drift_state('5dc0779a-5abf-4357-a03d-d0b062ec3cc3', contemporary_critical_historiography_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5dc0779a-5abf-4357-a03d-d0b062ec3cc3', '').
narrative_ontology:cs_kernel_id(naskh_principle__progressive_restriction, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, classical_fiqh_establishment).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, qadis_and_muftis).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, reformist_citers_of_early_verses).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, individual_permissive_practice_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, ordinary_believers).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, ordinary_believers).
narrative_ontology:constraint_vindicates(naskh_principle__progressive_restriction, divine_pedagogy_doctrine).
narrative_ontology:constraint_vindicates(naskh_principle__progressive_restriction, revelation_chronology_authority).
narrative_ontology:constraint_vindicates(naskh_principle__progressive_restriction, final_intent_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trains jurists in the principles of legal interpretation, maintains the tables pairing each ruling with its place in the order of revelation, and certifies through curricula, fatwa institutions, and judicial training which commands remain operative. Its teaching authority rests on holding every verse valid as revelation while fixing the last word on each contested topic. Leaving the arrangement would mean renouncing the interpretive office itself.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, classical_fiqh_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% Adjudicate disputes and answer ritual questions using a single operative ruling per topic. Because the reading fixes the final command, they rarely face litigants armed with contradictory verse citations, and their decisions carry a clean chain of textual warrant. Their careers are embedded in the institutions that administer the reading.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, qadis_and_muftis, beneficiary,
    powerful, biographical, constrained, regional).

% Receive a coherent body of practice guidance and keep the whole scripture as living revelation — no verse is discarded. They bear the compliance burden of the final restrictions (on intoxicants, inheritance procedure, marital dissolution, among others) and are taught that earlier, looser passages were stages in their community's education rather than licenses. Their faith identity binds them to the community that administers the correction.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, ordinary_believers, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__progressive_restriction, ordinary_believers, payer).

% Scholars, movements, and publications that invoke earlier permissive passages — on gradual intoxicant prohibition, on divorce before the later tightening, on wartime spoils distribution — to argue for contemporary latitude. Under the reading their citations are reclassified as quotation of a pedagogical stage, stripped of operative force, and their proponents face charges of ignorance of interpretive principles or of innovation. They retain publishing platforms and transnational networks but no seat in the certifying institutions.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, reformist_citers_of_early_verses, payer,
    organized, biographical, constrained, global).

% Believers who encounter an early verse that appears to authorize a practice they prefer or already follow, and are told by teachers and family that the passage marked a stage now superseded by the final command. Their recourse is limited to accepting the correction or leaving the community that disciplines their practice; they have no independent access to the interpretive machinery.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, individual_permissive_practice_seekers, payer,
    powerless, immediate, trapped, local).

% Academic historians and philologists who study the sequence of revelation and regard large parts of the traditional chronology as unsettled. They would contest the ordering on which the whole permissive-to-restrictive arc depends, but they stand outside the seminaries and fatwa councils where the reading is administered and are rarely answered there.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, textual_chronology_historians, excluded,
    moderate, generational, mobile, continental).

% Academic specialists in Islamic legal theory who compare the rival accounts of how revelation sequences into law, document which schools operationalize which account in practice, and trace how the balance shifted across centuries. They take no side in adjudication.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, comparative_hermeneutics_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__progressive_restriction, classical_fiqh_establishment).
narrative_ontology:fixing_cost_class(naskh_principle__progressive_restriction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem that a fixed scripture containing commands that differ across the order of revelation cannot by itself yield one operative rule per topic: the reading converts the sequence into a curriculum, letting a community hold every verse as valid revelation while knowing which command governs practice now.
% TRANSFER_FUNCTION: Moves interpretive finality to whichever ruling stands latest in the presumed order of revelation, and with it the authority to declare other citations out of order; moves compliance obligations shaped by the final restrictive commands onto all believers; moves the cost of textual contradiction away from adjudicators and onto those who would cite the earlier passages.
% ABSENT_VOICES: Textual historians who doubt the chronology, lay believers who experience the final restrictions as loss without any seat in the interpretive councils, and non-Sunni hermeneutical traditions that resolve the same tensions differently — none sits inside the seminary conversation where the reading is maintained.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, every school would need a different account of which commands govern: classical abrogation would strip earlier verses of legal force, contextual harmonization would multiply operative rules by situation, and curricula, fatwa practice, and citation discipline would reorganize around whichever replaced it. Adjudication could not proceed unchanged.
% FOUNDING_PROBLEM: The first generations received rulings that changed mid-revelation — the direction of prayer, the fast, the shares of inheritance, the status of intoxicants — and needed an account of which command bound them now that did not require calling any verse false.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of Islamic law outside the seminaries document the historical reality of evolving rulings and the early community's struggle with them; reformist jurists outside the madhhab establishments attest the problem remains live whenever they reopen a topic. The benefiting establishment, by contrast, tends to attest the problem as solved rather than live — corroboration of liveness comes from outside the beneficiary set.
narrative_ontology:disappearance_verdict(naskh_principle__progressive_restriction, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__progressive_restriction, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__progressive_restriction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(naskh_principle__progressive_restriction, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__progressive_restriction, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__progressive_restriction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__progressive_restriction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__progressive_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial but bounded (0.58 at interval end): the reading disqualifies an entire class of textual citations and channels practice toward the final restrictive commands, yet it leaves the verses themselves intact as revelation and leaves rival readings publishable — the cost falls on citers, not on the text. Suppression (0.52) runs through training, certification, and social sanction (charges of ignorance of interpretive principles or of innovation) rather than coercion; the series shows enforcement machinery maturing with the madhhab consolidation and easing slightly under contemporary pluralization. Theater (0.22) is low-moderate: the pedagogical frame does real integrative work in curricula and adjudication, though a growing share of its deployment defends institutional authority in controversies rather than resolving new cases. Accessibility_collapse (0.48) reflects that alternatives do NOT fully collapse — two sibling readings remain live and practiced, so mastering this reading closes no field. Resistance (0.55) is sustained: reformist movements, academic historiography, and lay dissatisfaction keep the arrangement contested. Claim and metrics are independent: I claim tangled_rope because the structure carries both a genuine coordination function (determinate law from a contradictory-bearing canon) and asymmetric extraction (finality concentrated in the latest rulings, permissive citations disqualified), enforced actively by the interpretive institutions; the metrics above describe operation without being tuned to that claim. All three temporal series share one grid (points 0–1200 at 200-year steps, mapping roughly onto the eight centuries from the reading's consolidation to the present), so no metric borrows another's end-state at earlier times.
 *
 * PERSPECTIVAL GAP:
 *   The administrator seat and the citer seats compute differently from identical structure. From the classical_fiqh_establishment position, the reading IS the tradition's own self-understanding: nothing was lost, every verse stands, and the law simply matured — the arrangement looks like faithful transmission. From the reformist_citers_of_early_verses and individual_permissive_practice_seekers positions, the same arrangement operates as a rule of admissibility that renders their strongest evidence categorically inoperable while leaving them no forum in which to argue it. The qadis seat sits between: they receive the benefit (clean adjudication) without administering the exclusion. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The establishment derives a directionality near the beneficiary end: it collects interpretive finality, controls the chronology tables and curricula, and its exit would mean dissolving the office itself (identity_locked amplifies its attachment to the arrangement). Qadis and muftis derive low-to-moderate d as beneficiaries of simplified adjudication. Ordinary believers sit near symmetric: genuine coherence gained, compliance borne, exit locked by faith identity. The two victim groups derive d near the full-target end: their citations are stripped of force, and exit is constrained (organized movements) or trapped (isolated individuals whose correction is administered by their own teachers and families). Textual chronology historians are excluded rather than coordinated — their potential falsification of the chronology is the arrangement's unmanaged exposure, not a seat inside it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — commands that differ across revelation inside a fixed text — remains live wherever new circumstances reopen old topics, so this is not a mandate outliving its function; no mandatrophy is declared. The tangled_rope classification prevents two opposite misreadings. Reading the arrangement as pure coordination would hide who pays: the citers whose best evidence is ruled inadmissible and the believers whose latitude is narrowed by a frame they never chose. Reading it as pure extraction would erase the real service: without some such principle, a scripture containing differing commands yields no stable adjudication at all, and the alternative that avoids finality (contextual harmonization) pays for pluralism with indeterminacy. Should the founding problem die — a community adopting contextual harmonization wholesale, or critical historiography collapsing the chronology — the reading would decay toward performed maintenance, and the theater series would be the leading indicator.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naskh_kernel_reading_contest,
    'Which reading of the naskh kernel governs operative law — this progressive-pedagogy reading, classical abrogation, or contextual harmonization?',
    'Comparative analysis of working fatwa and court practice: which account do sitting muftis and qadis actually deploy when a litigant cites an earlier permissive verse?',
    'If classical_abrogation dominates, earlier verses lose revelatory validity outright (a different victim structure, with the verses themselves as casualties); if contextual_harmonization dominates, operative law pluralizes and permissive citations regain standing, collapsing this reading''s extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naskh_kernel_reading_contest, conceptual, 'Kernel-level contest among three readings of how differing commands across revelation relate.').

omega_variable(
    revelation_chronology_uncertainty,
    'Is the traditional order of revelation reliable enough to fix the direction of the permissive-to-restrictive arc for the verse pairs this reading governs?',
    'Manuscript evidence, historical-critical dating, and independent analysis of the chronology-report transmission chains.',
    'Reversed or indeterminate sequences dissolve the pedagogical pattern for those pairs; the reading would survive only where chronology is secure, shrinking its scope and its extraction accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_chronology_uncertainty, empirical, 'Dependence of the progressive arc on an uncertain revelatory chronology.').

omega_variable(
    pedagogy_vs_establishment_interest,
    'Is the permissive-to-restrictive direction a genuine feature of the revelation''s design, or a post-hoc frame that happens to serve a legal establishment whose interest lies in final restrictive law?',
    'Test against cases where the revelatory movement ran restrictive-to-permissive or terminated in a permissive ruling: a genuine pedagogy predicts the frame tracks the actual arc; an establishment artifact predicts selective invocation only where the outcome is restrictive.',
    'If invocation is selective, the frame functions as authorization for concentrated finality and the structure shifts toward pure extraction dressed as theology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogy_vs_establishment_interest, conceptual, 'Whether the pedagogical frame is descriptive of the revelation or self-serving for its administrators.').

omega_variable(
    domain_scope_of_progressive_pattern,
    'Does the progressive pattern govern only the domains where revelation demonstrably moved (intoxicants, prayer direction, fasting, inheritance shares), or is it extended to every textual tension, including pairs with no documented sequence?',
    'Catalog of applied usage: which contested verse pairs are actually resolved by the pedagogical frame versus by specification, contextual limitation, or other interpretive devices.',
    'Overextension multiplies the set of disqualified citations and raises effective extraction beyond what the well-documented core cases warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_scope_of_progressive_pattern, empirical, 'Scope boundary of progressive-restriction application across contested verse pairs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__progressive_restriction, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__progressive_restriction, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(nask_tr_t0, observed).
narrative_ontology:measurement(nask_tr_t200, naskh_principle__progressive_restriction, theater_ratio, 200, 0.12).
narrative_ontology:measurement_basis(nask_tr_t200, observed).
narrative_ontology:measurement(nask_tr_t400, naskh_principle__progressive_restriction, theater_ratio, 400, 0.15).
narrative_ontology:measurement_basis(nask_tr_t400, observed).
narrative_ontology:measurement(nask_tr_t600, naskh_principle__progressive_restriction, theater_ratio, 600, 0.18).
narrative_ontology:measurement_basis(nask_tr_t600, observed).
narrative_ontology:measurement(nask_tr_t800, naskh_principle__progressive_restriction, theater_ratio, 800, 0.2).
narrative_ontology:measurement_basis(nask_tr_t800, observed).
narrative_ontology:measurement(nask_tr_t1000, naskh_principle__progressive_restriction, theater_ratio, 1000, 0.24).
narrative_ontology:measurement_basis(nask_tr_t1000, observed).
narrative_ontology:measurement(nask_tr_t1200, naskh_principle__progressive_restriction, theater_ratio, 1200, 0.22).
narrative_ontology:measurement_basis(nask_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__progressive_restriction, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(nask_be_t0, observed).
narrative_ontology:measurement(nask_be_t200, naskh_principle__progressive_restriction, base_extractiveness, 200, 0.38).
narrative_ontology:measurement_basis(nask_be_t200, observed).
narrative_ontology:measurement(nask_be_t400, naskh_principle__progressive_restriction, base_extractiveness, 400, 0.44).
narrative_ontology:measurement_basis(nask_be_t400, observed).
narrative_ontology:measurement(nask_be_t600, naskh_principle__progressive_restriction, base_extractiveness, 600, 0.5).
narrative_ontology:measurement_basis(nask_be_t600, observed).
narrative_ontology:measurement(nask_be_t800, naskh_principle__progressive_restriction, base_extractiveness, 800, 0.55).
narrative_ontology:measurement_basis(nask_be_t800, observed).
narrative_ontology:measurement(nask_be_t1000, naskh_principle__progressive_restriction, base_extractiveness, 1000, 0.6).
narrative_ontology:measurement_basis(nask_be_t1000, observed).
narrative_ontology:measurement(nask_be_t1200, naskh_principle__progressive_restriction, base_extractiveness, 1200, 0.58).
narrative_ontology:measurement_basis(nask_be_t1200, observed).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__progressive_restriction, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(nask_su_t0, observed).
narrative_ontology:measurement(nask_su_t200, naskh_principle__progressive_restriction, suppression_requirement, 200, 0.36).
narrative_ontology:measurement_basis(nask_su_t200, observed).
narrative_ontology:measurement(nask_su_t400, naskh_principle__progressive_restriction, suppression_requirement, 400, 0.42).
narrative_ontology:measurement_basis(nask_su_t400, observed).
narrative_ontology:measurement(nask_su_t600, naskh_principle__progressive_restriction, suppression_requirement, 600, 0.46).
narrative_ontology:measurement_basis(nask_su_t600, observed).
narrative_ontology:measurement(nask_su_t800, naskh_principle__progressive_restriction, suppression_requirement, 800, 0.5).
narrative_ontology:measurement_basis(nask_su_t800, observed).
narrative_ontology:measurement(nask_su_t1000, naskh_principle__progressive_restriction, suppression_requirement, 1000, 0.54).
narrative_ontology:measurement_basis(nask_su_t1000, observed).
narrative_ontology:measurement(nask_su_t1200, naskh_principle__progressive_restriction, suppression_requirement, 1200, 0.52).
narrative_ontology:measurement_basis(nask_su_t1200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__progressive_restriction, enforcement_mechanism).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_principle__classical_abrogation).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_principle__contextual_harmonization).

% DUAL FORMULATION NOTE:
% The colloquial label 'naskh' conflates three structurally distinct claims about how differing commands relate: cancellation (classical_abrogation), contextual validity (contextual_harmonization), and directed pedagogy (this story). Decomposed per the ε-invariance principle: each reading assigns a different operative status to the same verse pairs, hence a different ε, different beneficiaries, and different victims. They form a constraint family linked via affects_constraints; the upstream classical_abrogation reading historically supplied the chronological machinery this reading repurposes from cancellation into pedagogy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
