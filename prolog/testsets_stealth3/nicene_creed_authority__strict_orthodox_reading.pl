% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__strict_orthodox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__strict_orthodox_reading, []).

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
 *   constraint_id: nicene_creed_authority__strict_orthodox_reading
 *   human_readable: Strict Orthodox Reading: Creed as Binding Metaphysical Ontology with Sanctioned Heresy Policing
 *   domain: systematic_theology/ecclesiology/history_of_doctrine
 *
 * SUMMARY:
 *   Under the strict orthodox reading, the Nicene Creed is not a summary of
 *   opinion but a binding metaphysical ontology: to be a believer is to
 *   assent to one specified account of God, Christ, and creation, and offices
 *   exist to fix that account's sense and to sanction deviation. The
 *   arrangement couples a real coordination achievement — a single confession
 *   usable across languages, classes, and centuries — to an enforcement
 *   economy: councils define, tribunals and episcopal courts punish, and
 *   belonging (sacraments, standing, sometimes civil existence) is priced in
 *   assent. Gains concentrate in the offices that adjudicate; costs land on
 *   heterodox communities, licensed-but-dissenting scholars, and lay readers
 *   whose private judgments are subordinated. This file instantiates one
 *   reading of the contested creed-authority kernel; the committer structure
 *   is carried in commentary.kernel_context and the omega variables, not
 *   averaged into the metrics. KEY AGENTS (by structural relationship): -
 *   hierarchical_episcopate: agenda-setting beneficiary
 *   (institutional/identity_locked) — fixes the binding sense, administers
 *   sanction, collects adjudicative authority - imperial_authorities:
 *   secondary enforcer-beneficiary (powerful/arbitrage) — lent coercion for
 *   cohesion, withdrew when the calculus shifted - theological_faculties:
 *   beneficiary (organized/constrained) — staff the interpretive machine;
 *   chairs and patronage flow to defenders of the standard -
 *   heterodox_communities: primary payers (organized/constrained) — bear
 *   anathema, dispossession, exclusion - lay_interpreters: payers
 *   (powerless/trapped) — assent extracted as the condition of belonging -
 *   dissenting_scholars: payers (moderate/constrained) — censorship and
 *   career destruction - vernacular_translators: excluded (moderate/trapped)
 *   — barred from putting the sources into lay hands -
 *   historians_of_doctrine: analytical observer — attests the record from
 *   outside the economy
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, 0.66).
domain_priors:suppression_score(nicene_creed_authority__strict_orthodox_reading, 0.55).
domain_priors:theater_ratio(nicene_creed_authority__strict_orthodox_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__strict_orthodox_reading, tangled_rope).
narrative_ontology:human_readable(nicene_creed_authority__strict_orthodox_reading, "Strict Orthodox Reading: Creed as Binding Metaphysical Ontology with Sanctioned Heresy Policing").
narrative_ontology:topic_domain(nicene_creed_authority__strict_orthodox_reading, "systematic_theology/ecclesiology/history_of_doctrine").

domain_priors:requires_active_enforcement(nicene_creed_authority__strict_orthodox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__strict_orthodox_reading, '15cc27cd-20a0-469c-870a-d7e928dfe75a').
narrative_ontology:cs_kernel_codification('15cc27cd-20a0-469c-870a-d7e928dfe75a', fixed_text).
narrative_ontology:cs_authority_grounding('15cc27cd-20a0-469c-870a-d7e928dfe75a', lineage).
narrative_ontology:cs_interpretation_layer_present('15cc27cd-20a0-469c-870a-d7e928dfe75a').
narrative_ontology:cs_reading_relation('15cc27cd-20a0-469c-870a-d7e928dfe75a', nicene_creed_authority__symbolic_confessional_reading, forecloses).
narrative_ontology:cs_reading_relation('15cc27cd-20a0-469c-870a-d7e928dfe75a', nicene_creed_authority__liturgical_habituation_reading, forecloses).
narrative_ontology:cs_axiom('15cc27cd-20a0-469c-870a-d7e928dfe75a', foundational, creed_states_binding_metaphysical_truth).
narrative_ontology:cs_axiom_status(creed_states_binding_metaphysical_truth, holdable).
narrative_ontology:cs_axiom_grounding('15cc27cd-20a0-469c-870a-d7e928dfe75a', creed_states_binding_metaphysical_truth, theological).
narrative_ontology:cs_axiom('15cc27cd-20a0-469c-870a-d7e928dfe75a', foundational, doctrinal_deviation_warrants_sanction).
narrative_ontology:cs_axiom_status(doctrinal_deviation_warrants_sanction, holdable).
narrative_ontology:cs_axiom_grounding('15cc27cd-20a0-469c-870a-d7e928dfe75a', doctrinal_deviation_warrants_sanction, conventional).
narrative_ontology:cs_axiom('15cc27cd-20a0-469c-870a-d7e928dfe75a', secondary, episcopal_office_sole_authorized_interpreter).
narrative_ontology:cs_axiom_status(episcopal_office_sole_authorized_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('15cc27cd-20a0-469c-870a-d7e928dfe75a', episcopal_office_sole_authorized_interpreter, conventional).
narrative_ontology:cs_reference_frame('15cc27cd-20a0-469c-870a-d7e928dfe75a', apostolic_metaphysical_consensus).
narrative_ontology:cs_drift_state('15cc27cd-20a0-469c-870a-d7e928dfe75a', contemporary_religious_liberty_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('15cc27cd-20a0-469c-870a-d7e928dfe75a', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, hierarchical_episcopate).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, imperial_authorities).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, theological_faculties).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, heterodox_communities).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, lay_interpreters).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, dissenting_scholars).
narrative_ontology:constraint_vindicates(nicene_creed_authority__strict_orthodox_reading, magisterial_interpretive_authority).
narrative_ontology:constraint_vindicates(nicene_creed_authority__strict_orthodox_reading, creedal_metaphysical_binding).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes and presides over councils, fixes the creed's binding sense, administers anathema and excommunication, and licenses who may teach. Collects deference, adjudicative supremacy, and institutional continuity. Its authority is constituted by being the arbiter of the standard, so stepping back from enforcement would dissolve the office's own warrant; the men who hold it have become its function.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, hierarchical_episcopate, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__strict_orthodox_reading, hierarchical_episcopate, beneficiary).

% Convoked councils and lent state coercion to doctrinal enforcement in exchange for a uniform confession that stabilized taxation, army loyalty, and administrative cohesion. Could ramp enforcement up or down as the cohesion calculus shifted, and did so unevenly across empires and kingdoms; their gains were cohesion rents they could abandon.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, imperial_authorities, beneficiary,
    powerful, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__strict_orthodox_reading, imperial_authorities, agenda_setter).

% Cathedral schools and universities staff the interpretive machine: chairs, patronage, and publishing privilege flow to those who defend the standard. Their expertise is necessary only so long as the creed requires authorized interpretation; they also bear censorship when their own proposals outrun the settled text.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, theological_faculties, beneficiary,
    organized, biographical, constrained, continental).

% Communities confessing rival christologies bear anathema, dispossession, forced conversion, and exclusion from civil life. Exit means forming separated bodies at the price of sacramental recognition, property, and often physical survival; migration to frontiers beyond enforcement reach was possible but ruinous.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, heterodox_communities, payer,
    organized, generational, constrained, regional).

% Ordinary baptized believers receive the creed's sense as delivered from above. Private readings that diverge attract correction, penance, or exclusion; leaving means losing community, marriage validity, and burial. Metaphysical assent is extracted from them as the standing condition of belonging.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, lay_interpreters, payer,
    powerless, immediate, trapped, local).

% Theologians proposing revision face censorship, index placement, revocation of the teaching license, and career destruction. Some migrate to separated academies or secular universities; most absorb the cost and conform their published claims to the enforced sense.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, dissenting_scholars, payer,
    moderate, biographical, constrained, continental).

% Those who would render scripture and creed into vernacular languages for lay judgment were barred and their translations condemned. They would argue that assent cannot honestly be extracted from people denied access to the sources; they are outside the conversation by explicit prohibition.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, vernacular_translators, excluded,
    moderate, biographical, trapped, national).

% Reconstruct the enforcement record from council acts, tribunal registers, and correspondence. Attest what the machinery did and what it cost, from a seat outside its economy of gains and penalties.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, historians_of_doctrine, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_creed_authority__strict_orthodox_reading, hierarchical_episcopate).
narrative_ontology:fixing_cost_class(nicene_creed_authority__strict_orthodox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains one shared metaphysical grammar across dispersed, mostly illiterate congregations and across generations: a fixed confession lets distant churches recognize each other's teaching and baptisms, gives catechesis a stable curriculum, and draws a durable boundary against syncretic absorption — a collective-action solution to doctrinal drift in a movement with no central communications.
% TRANSFER_FUNCTION: Moves metaphysical assent and interpretive authority upward from individual believers and rival teachers to the conciliar and episcopal center; moves belonging (sacramental access, standing, sometimes civil existence) back down to conformers; and moves state coercive capacity into doctrinal enforcement.
% ABSENT_VOICES: Deposed rival bishops, delegates of condemned communities, and vernacular translators were never seated at the councils that fixed the binding sense; the unanimity recorded in the acts arose in rooms from which the people later sanctioned for deviation had been excluded in advance.
% DISAPPEARANCE_RATIONALE: Overnight removal would reopen every christological question the machinery froze: congregations would sort along rival ontologies as they did whenever enforcement lapsed, mutual recognition of baptisms and orders would break, and the episcopal office would lose the adjudicative function that constitutes it — the religious landscape rearranges around the missing enforcement.
% FOUNDING_PROBLEM: In the Arian crisis, churches could no longer tell apostolic teaching from philosophical innovation; communion broke over baptismal validity; and imperial civic peace frayed along liturgical lines. The arrangement was built to fix, once and centrally, a test of faithful confession so that teaching authority and communion had a stable criterion.
% FOUNDING_PROBLEM_CORROBORATION: Non-confessional historians of doctrine corroborate the original crisis from council records and imperial correspondence. The still-live status, however, is attested chiefly by the enforcement apparatus itself; ecumenical bodies document persistent christological division but dispute that sanction-backed uniformity is the remedy. Partial external corroboration; the live-status claim leans on interested testimony.
narrative_ontology:disappearance_verdict(nicene_creed_authority__strict_orthodox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__strict_orthodox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__strict_orthodox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nicene_creed_authority__strict_orthodox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__strict_orthodox_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__strict_orthodox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_creed_authority__strict_orthodox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.66) but short of predation: the arrangement delivers a real coordination service alongside the transfer, and sanction severity at the margins tracks challenges to the enforcing office more than threats to the confessed content — the signature of extraction riding on coordination rather than replacing it. Suppression (0.55) is the current residual force — exclusion from communion and teaching rather than state coercion — and is authored as a raw structural property, deliberately unscaled; the suppression_requirement series separately traces the enforcement machinery's build-up (Theodosius through the medieval tribunals) and decay (Enlightenment through the religious-liberty settlement), which is the dynamic this story specifically tracks. Theater (0.32) rises steadily as anathema ritualizes ahead of the enforcement behind it. Accessibility_collapse (0.52): alternatives collapse inside the governed population, but schism and migration exits persist at ruinous cost. Resistance (0.62): heterodox movements persisted continuously, producing schisms and reformations rather than extinction. All three series run on one shared eight-point grid aligned to council dates, so every metric is authored at every examined time point. Receipt surface: gains demonstrably accrue to the episcopal seat, whose adjudicative authority is constituted by the enforcement it administers, so gain_flow names that seat rather than asserting diffuseness. Fixing is prohibitive for the seat that could fix it: relaxing enforcement dissolves the office's own warrant, and historical relaxations triggered schismatic defection — the cost to the fixer exceeds the benefit.
 *
 * PERSPECTIVAL GAP:
 *   From the episcopal seat the arrangement is a community guarding a gift: sanction is medicine, and the office's authority simply is fidelity to the standard it transmits. From the trapped lay seat and the condemned-community seat the same structure is a toll gate on belonging: assent extracted under penalty by parties who alone decide what counts as deviation. The engine computes these per-seat classifications from the power, exit, and role data above; the divergence between the seats is the measurement the corpus exists to take, not a defect to be reconciled in authoring.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (hierarchical_episcopate, imperial_authorities, theological_faculties) derive low directionality for those seats — the constraint subsidizes them. Victim declarations (heterodox_communities, lay_interpreters, dissenting_scholars) derive high directionality, amplified by trapped and constrained exit: a baptized layperson with no affordable exit sits near the full-target end, while a scholar with academy mobility sits somewhat lower. Imperial authorities sit nearer the beneficiary pole than their enforcement activity suggests because their gains were cohesion rents they could abandon at arbitrage-grade exit; the structural derivation captures this without an override. Scope amplification applies modestly at the arrangement's continental-to-global reach, where verification of private belief is hardest.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification guards both failure modes. Reading the arrangement as pure snare would erase the genuine coordination achievement — a portable confession that held a trans-local movement together for centuries and solved a real doctrinal-drift problem no informal mechanism could. Reading it as pure rope would erase the asymmetric extraction — sanction severity tracking office-challenge, adjudicative rents concentrating in the enforcing seat, victims identifiable by name and community. Mandatrophy is not declared: the founding problem (doctrinal drift, communion breakdown) remains live from this reading's own lights, and enforcement retains function even as its capacity decays. The drift to watch is theater_ratio rising faster than extractiveness falls — the signature of a constraint sliding toward piton rather than resolving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story authors epsilon for one reading of the nicene_creed_authority kernel — the strict_orthodox_reading, in which the creed binds all believers to one metaphysical ontology and deviation warrants sanction. Would instantiating a sibling reading change the constraint''s structure?',
    'Compare the compiled family: under symbolic_confessional_reading the creed is contingent witness with authority from community discernment (no sanction warrant; epsilon falls toward coordination-only levels); under liturgical_habituation_reading the binding surface is liturgical performance, not cognitive assent (victims shrink to coerced-performance cases). The disagreement is located in the locus of binding: enforced cognitive assent versus performed identity versus witnessed testimony.',
    'If the enforcing institutions shift to a sibling reading, this constraint''s beneficiary/victim sets dissolve, epsilon collapses, and the classification migrates from tangled_rope toward rope; the present story''s high-extraction profile is indexical to this reading, not to the creed as such.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame index: epsilon is authored for the strict_orthodox_reading only.').

omega_variable(
    sanction_discipline_or_extraction,
    'Seen from inside the strict orthodox frame, is the sanction machinery medicinal discipline protecting a shared grasp of reality, or extraction of assent that concentrates adjudicative rent in the enforcing office?',
    'Locate where sanction proceeds diverge from protection of the confessed content: if enforcement intensity tracks threats to the ontology, discipline dominates; if it tracks challenges to the enforcing office''s authority — as when licensed teachers are silenced for proposals the creed text itself does not settle — extraction dominates.',
    'Discipline-dominant supports the tangled_rope classification; extraction-dominant pushes the computed type toward snare despite the genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanction_discipline_or_extraction, preference, 'Normative status of sanction: discipline versus extraction.').

omega_variable(
    enforcement_capacity_revival,
    'Will enforcement capacity continue to decay after the religious-liberty settlement, or do traditionalist revivals rebuild sanction infrastructure?',
    'Track traditionalist jurisdiction growth, restored doctrinal-tribunal caseload, and synodal-governance outcomes over coming decades.',
    'Continued decay drives theater_ratio up and suppression down (piton drift); revival restores the tangled_rope enforcement profile and re-amplifies effective extraction on trapped laity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_revival, empirical, 'Post-settlement trajectory of the sanction machinery.').

omega_variable(
    creedal_realism_vs_constructed_consensus,
    'Does the creed''s ontological content describe a mind-independent divine reality (making uniformity epistemically compelled and giving the constraint a mountain-like epistemic floor), or a constructed conciliar consensus (making uniformity purely enforced)?',
    'Not resolvable by data internal to the tradition; conceptual analysis within theology of the realism claim, cross-checked against whether the binding sense has ever been revised by evidence rather than by authority.',
    'If realist, part of the measured suppression is the cost of truth-maintenance and the reading''s self-account gains force; if constructivist, the entire apparatus is coordination plus extraction with no epistemic floor beneath it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(creedal_realism_vs_constructed_consensus, conceptual, 'Metaphysical status of the creed''s content: discovered or constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__strict_orthodox_reading, 325, 1965).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 325, 0.1).
narrative_ontology:measurement_basis(nice_tr_t325, observed).
narrative_ontology:measurement(nice_tr_t381, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 381, 0.12).
narrative_ontology:measurement_basis(nice_tr_t381, observed).
narrative_ontology:measurement(nice_tr_t451, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 451, 0.15).
narrative_ontology:measurement_basis(nice_tr_t451, observed).
narrative_ontology:measurement(nice_tr_t1054, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1054, 0.2).
narrative_ontology:measurement_basis(nice_tr_t1054, observed).
narrative_ontology:measurement(nice_tr_t1215, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1215, 0.18).
narrative_ontology:measurement_basis(nice_tr_t1215, observed).
narrative_ontology:measurement(nice_tr_t1545, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1545, 0.24).
narrative_ontology:measurement_basis(nice_tr_t1545, observed).
narrative_ontology:measurement(nice_tr_t1870, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1870, 0.29).
narrative_ontology:measurement_basis(nice_tr_t1870, observed).
narrative_ontology:measurement(nice_tr_t1965, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1965, 0.32).
narrative_ontology:measurement_basis(nice_tr_t1965, observed).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 325, 0.42).
narrative_ontology:measurement_basis(nice_be_t325, observed).
narrative_ontology:measurement(nice_be_t381, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 381, 0.55).
narrative_ontology:measurement_basis(nice_be_t381, observed).
narrative_ontology:measurement(nice_be_t451, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 451, 0.63).
narrative_ontology:measurement_basis(nice_be_t451, observed).
narrative_ontology:measurement(nice_be_t1054, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1054, 0.6).
narrative_ontology:measurement_basis(nice_be_t1054, observed).
narrative_ontology:measurement(nice_be_t1215, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1215, 0.71).
narrative_ontology:measurement_basis(nice_be_t1215, observed).
narrative_ontology:measurement(nice_be_t1545, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1545, 0.69).
narrative_ontology:measurement_basis(nice_be_t1545, observed).
narrative_ontology:measurement(nice_be_t1870, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1870, 0.67).
narrative_ontology:measurement_basis(nice_be_t1870, observed).
narrative_ontology:measurement(nice_be_t1965, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1965, 0.66).
narrative_ontology:measurement_basis(nice_be_t1965, observed).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 325, 0.35).
narrative_ontology:measurement_basis(nice_su_t325, observed).
narrative_ontology:measurement(nice_su_t381, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 381, 0.55).
narrative_ontology:measurement_basis(nice_su_t381, observed).
narrative_ontology:measurement(nice_su_t451, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 451, 0.65).
narrative_ontology:measurement_basis(nice_su_t451, observed).
narrative_ontology:measurement(nice_su_t1054, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1054, 0.6).
narrative_ontology:measurement_basis(nice_su_t1054, observed).
narrative_ontology:measurement(nice_su_t1215, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1215, 0.8).
narrative_ontology:measurement_basis(nice_su_t1215, observed).
narrative_ontology:measurement(nice_su_t1545, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1545, 0.78).
narrative_ontology:measurement_basis(nice_su_t1545, observed).
narrative_ontology:measurement(nice_su_t1870, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1870, 0.74).
narrative_ontology:measurement_basis(nice_su_t1870, observed).
narrative_ontology:measurement(nice_su_t1965, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1965, 0.55).
narrative_ontology:measurement_basis(nice_su_t1965, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__strict_orthodox_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__symbolic_confessional_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__liturgical_habituation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Nicene Creed authority' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints. This file instantiates the strict_orthodox_reading (binding metaphysical ontology with sanction-warranted heresy policing; epsilon 0.66, tangled_rope). The symbolic_confessional_reading (contingent witness, community-discerned authority) and the liturgical_habituation_reading (identity boundary via performance, assent-independent) are separate stories with their own epsilon, beneficiaries, and victims; the links here route contamination analysis across the family. Historically the strict reading was upstream: it supplied the enforcement conditions under which the other two readings' practices operated, and its erosion changes their operating environment without resolving the contest among them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
