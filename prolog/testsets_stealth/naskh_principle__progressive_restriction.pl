% ============================================================================
% CONSTRAINT STORY: naskh_principle__progressive_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Progressive-Restriction Hermeneutic of Quranic Legal Development
 *   domain: religious/legal-hermeneutic
 *
 * SUMMARY:
 *   Within Sunni jurisprudence, the progressive-restriction reading orders
 *   the revelatory corpus so that earlier permissive provisions — the
 *   graduated drink rulings preceding final prohibition, transitional
 *   marriage and inheritance arrangements, pre-consolidation procedural rules
 *   — are classified as pedagogical stages whose normative force concluded
 *   when the restrictive final rulings arrived, with no verse ever declared
 *   void. The arrangement solves a real problem (a corpus with internal
 *   variation needs an ordering principle to yield determinate law) while
 *   concentrating stage-certification authority in the scholarly class that
 *   administers the chronological map. This file instantiates ONE reading of
 *   the naskh_principle kernel; the colloquial label decomposes into three
 *   structurally distinct constraints with different epsilon values and
 *   beneficiary structures: classical_abrogation concedes textual
 *   invalidation (different victim set, weaker coordination story),
 *   contextual_harmonization denies chronological finality (no
 *   stage-certification beneficiary, extraction collapses toward zero). The
 *   claim/metric gap is deliberate: the reading self-presents as pure divine
 *   pedagogy — a rope-like account in which nobody extracts — while the
 *   authored metrics describe hybrid operation with identifiable
 *   beneficiaries and payers. The engine measures that divergence; the claim
 *   is not reconciled to the metrics.
 *
 * KEY AGENTS:
 *   - jurisprudential_scholarship: Primary beneficiary and agenda-setter (institutional/identity_locked) — administers the chronological map, certifies final-stage rulings, collects the authority rents
 *   - madhhab_legal_traditions: Secondary beneficiary (institutional/constrained) — canonical compendia rest on final-stage rulings the reading shields from reopening
 *   - early_permissive_text_citers: Primary target (organized/constrained) — bear foreclosure of early-text grounding for contemporary practice
 *   - lay_direct_textual_readers: Secondary target (powerless/trapped) — cannot act on any stage-classified verse without scholarly certification
 *   - contextual_harmonization_advocates: Excluded rival hermeneuts (organized/constrained) — their exclusion from certification venues is the enforcement object
 *   - academic_islamic_studies: Analytical observer (institutional/analytical) — documents the gap between the applied ordering and its evidentiary basis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__progressive_restriction, 0.62).
domain_priors:suppression_score(naskh_principle__progressive_restriction, 0.7).
domain_priors:theater_ratio(naskh_principle__progressive_restriction, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, extractiveness, 0.62).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__progressive_restriction, tangled_rope).
narrative_ontology:human_readable(naskh_principle__progressive_restriction, "Progressive-Restriction Hermeneutic of Quranic Legal Development").
narrative_ontology:topic_domain(naskh_principle__progressive_restriction, "religious/legal-hermeneutic").

domain_priors:requires_active_enforcement(naskh_principle__progressive_restriction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__progressive_restriction, 'c5d07c74-44bd-478d-b6a3-8853a7adea0d').
narrative_ontology:cs_kernel_codification('c5d07c74-44bd-478d-b6a3-8853a7adea0d', fixed_text).
narrative_ontology:cs_authority_grounding('c5d07c74-44bd-478d-b6a3-8853a7adea0d', lineage).
narrative_ontology:cs_interpretation_layer_present('c5d07c74-44bd-478d-b6a3-8853a7adea0d').
narrative_ontology:cs_reading_relation('c5d07c74-44bd-478d-b6a3-8853a7adea0d', naskh_principle__classical_abrogation, forecloses).
narrative_ontology:cs_reading_relation('c5d07c74-44bd-478d-b6a3-8853a7adea0d', naskh_principle__contextual_harmonization, influences).
narrative_ontology:cs_axiom('c5d07c74-44bd-478d-b6a3-8853a7adea0d', foundational, revelatory_sequence_is_divine_pedagogy).
narrative_ontology:cs_axiom_status(revelatory_sequence_is_divine_pedagogy, holdable).
narrative_ontology:cs_axiom_grounding('c5d07c74-44bd-478d-b6a3-8853a7adea0d', revelatory_sequence_is_divine_pedagogy, theological).
narrative_ontology:cs_axiom('c5d07c74-44bd-478d-b6a3-8853a7adea0d', foundational, no_verse_invalidated_only_staged).
narrative_ontology:cs_axiom_status(no_verse_invalidated_only_staged, holdable).
narrative_ontology:cs_axiom_grounding('c5d07c74-44bd-478d-b6a3-8853a7adea0d', no_verse_invalidated_only_staged, theological).
narrative_ontology:cs_reference_frame('c5d07c74-44bd-478d-b6a3-8853a7adea0d', pedagogical_revelatory_sequence).
narrative_ontology:cs_drift_state('c5d07c74-44bd-478d-b6a3-8853a7adea0d', contemporary_reformist_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c5d07c74-44bd-478d-b6a3-8853a7adea0d', '').
narrative_ontology:cs_kernel_id(naskh_principle__progressive_restriction, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, jurisprudential_scholarship).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, madhhab_legal_traditions).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, early_permissive_text_citers).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, lay_direct_textual_readers).
narrative_ontology:constraint_vindicates(naskh_principle__progressive_restriction, divine_pedagogy_doctrine).
narrative_ontology:constraint_vindicates(naskh_principle__progressive_restriction, chronological_finality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the chronological map of revelation, certifies which rulings count as final-stage, trains jurists in the sequencing method, and polices citations of verses classified as transitional accommodations. Their professional standing, curricular authority, and role as arbiters of the text-law relationship are constituted by administering the sequence; abandoning the method would mean dissolving the office they occupy.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, jurisprudential_scholarship, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__progressive_restriction, jurisprudential_scholarship, beneficiary).

% Canonical compendia encode the final-stage rulings as settled positions. The progressive-restriction reading protects those positions from reopening: a practitioner who cites an earlier permissive verse can be answered with stage-classification rather than fresh argument. The traditions did not build the method, but their accumulated case law rides on it.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, madhhab_legal_traditions, beneficiary,
    institutional, generational, constrained, global).

% Reform-minded jurists and intellectuals who argue for contemporary practices by citing early permissive provisions — graduated drink rulings before the final prohibition, transitional marriage and inheritance provisions, pre-consolidation procedural rules. Each citation draws the response that the verse was a pedagogical stage whose normative force expired; they retain institutional footholds in universities and reform seminaries but are locked out of the credentialing bodies that certify final-stage rulings.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, early_permissive_text_citers, payer,
    organized, biographical, constrained, global).

% Ordinary believers who read the text directly and find permissive language on topics they care about. They cannot self-certify which verses are transitional and which are final; the method makes scholarly mediation a precondition for acting on any verse a scholar has stage-classified. Exit would mean leaving the interpretive community altogether, which for most is not a live option.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, lay_direct_textual_readers, payer,
    powerless, biographical, trapped, global).

% Modernist and scripturist scholars who hold that every verse remains valid within its revelatory context and deny that position in the sequence confers binding force. They publish, teach in marginal institutions, and are largely absent from the mainstream curricula and certification structures where the progressive-restriction reading is administered. Their exclusion from those rooms is maintained by the same stage-certification machinery the reading runs on.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, contextual_harmonization_advocates, excluded,
    organized, biographical, constrained, global).

% Historians and philologists inside and outside the tradition who document the piecemeal revelation, the formative-period disputes over ordering principles, and the divergence between the applied chronological map and the underlying evidence. They take no seat in the enforcement structure and collect nothing from its operation.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, academic_islamic_studies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__progressive_restriction, jurisprudential_scholarship).
narrative_ontology:fixing_cost_class(naskh_principle__progressive_restriction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Orders a revelatory corpus delivered piecemeal over twenty-three years, containing differing rulings on the same topics, into a single sequence so that the community has one operative ruling per question at any time while no verse is discarded as void. It converts internal textual variation into determinate law without conceding contradiction in the text.
% TRANSFER_FUNCTION: Moves certification of which stage binds now — and with it the legitimacy of permissive practice — from believers reading the text directly to the scholarly class that controls the chronological map; moves interpretive authority, curricular standing, and dispute-ending power toward holders of the final-stage rulings.
% ABSENT_VOICES: Practitioners whose conduct rests on early permissive verses, non-scholarly readers, and rival hermeneuts — contextual harmonizers and scripturist movements — are absent from the curricula, certification bodies, and endowment structures where the reading is administered. Present, they would object that stage-classification is asserted rather than demonstrated verse by verse, and that the chronological ordering outruns its evidentiary basis in exactly the cases where permissive citation is foreclosed.
% DISAPPEARANCE_RATIONALE: If the progressive-restriction ordering vanished overnight, the corpus's internal tensions would reopen unresolved: either classical abrogation or contextual harmonization would have to carry the load, thousands of settled positions resting on final-stage rulings would lose their warrant, and the scholarly gatekeeping structure built on stage-certification would lose its function. The legal system built on the sequence would reorganize around whichever rival ordering won.
% FOUNDING_PROBLEM: A corpus revealed incrementally over twenty-three years changed rulings on the same topics — drink, prayer discipline, fasting, divorce procedure, inheritance shares. The early community needed to know which provision governed now, without treating any part of revelation as void or contradictory.
% FOUNDING_PROBLEM_CORROBORATION: Attested outside the benefiting parties by the hadith corpus itself, which records companions asking which provision governed after successive revelations; by formative-period and academic historiography documenting the juristic demand for ordering principles; and by the existence of the two rival readings — classical abrogation and contextual harmonization — which attest the same underlying problem while disputing this solution. No source outside the benefiting parties attests that the problem is dead.
narrative_ontology:disappearance_verdict(naskh_principle__progressive_restriction, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__progressive_restriction, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__progressive_restriction, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(naskh_principle__progressive_restriction, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__progressive_restriction, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is 0.62 because the method performs a genuine hermeneutic service (resolving corpus tension into usable law) while simultaneously converting verse-order knowledge into a monopolized certification asset: the payer seats lose access to textual grounding precisely where the permissive-to-restrictive pattern is invoked. Suppression is higher (0.70) because persistence depends on active enforcement — credentialing, curricular control, and deviance-labeling of harmonizers and scripturists — not on voluntary uptake. Theater is moderate-low (0.30): the pedagogical narrative does real classificatory work, but a growing share of its deployment defends gatekeeping rather than resolving textual difficulty. Accessibility collapse is 0.48 because alternatives remain live: harmonization, scripturism, and academic reconstruction persist outside the certified venues. Resistance is 0.58, reflecting sustained modernist and reformist contestation of chronological finality. The three measurement series run on one shared seven-point grid so every metric is authored at every examined time point; the rising trajectories track the doctrine's consolidation from problem-solving tool into authority structure across the formative-to-classical arc.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently. From jurisprudential_scholarship's position, the arrangement is faithful stewardship of a divinely sequenced curriculum — coordination it maintains, with costs (defending against resistance) it willingly bears. From the constrained payer seats, the same structure operates as foreclosure: a text they can read but not act on without permission from the class that owns the sequence. Madhhab traditions sit between — beneficiaries of closure they did not build. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Jurisprudential_scholarship sits nearest the beneficiary end: it administers the method, collects the certification authority, and is identity-locked (its professional self is the method). Madhhab_legal_traditions derive low directionality as protected beneficiaries. Early_permissive_text_citers sit near the target end — they pay in foreclosed practice-grounding, with constrained exit through marginal institutions. Lay_direct_textual_readers sit at the full-target end: trapped, unable to certify anything themselves, bearing the arrangement's costs in dependency. Contextual_harmonization_advocates are excluded rather than coordinated; their exclusion is what the enforcement machinery maintains. Academic observers carry analytical seats with no extraction exposure. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and the global spatial scope of the certification structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — ordering a piecemeal corpus with shifting rulings — remains live for every generation of jurists, so this is not a mandatrophy case and no sunset applies. The tangled_rope classification prevents both symmetrical errors: reading the arrangement as rope (its own self-presentation) would hide the gatekeeping extraction that flows through stage-certification discretion; reading it as snare would erase the genuine coordination service — a corpus with internal variation genuinely requires an ordering principle, and rivals propose rival orderings rather than none. The hybrid verdict keeps both the service and the rent visible. The temporal series show the characteristic tangled-rope signature: extraction and suppression requirements rising together as the method hardened from ad hoc problem-solving into an administered authority structure, with theater climbing modestly behind them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_positioning,
    'This constraint instantiates the progressive_restriction reading of the naskh_principle kernel; which structural facts would change if a sibling reading were adopted instead?',
    'Track adoption across juristic factions, curricula, and certification bodies; recompute the beneficiary/victim sets under each sibling''s mechanism.',
    'Under classical_abrogation the victim set extends to holders of precedents built on invalidated rulings and the coordination story weakens (the text is openly voided rather than staged). Under contextual_harmonization the stage-certification beneficiary disappears entirely and measured extraction collapses toward zero. The classification of THIS story is conditional on this reading remaining the operative one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_positioning, conceptual, 'Committer structure: one of three readings of the naskh kernel; sibling adoption would restructure beneficiaries and victims.').

omega_variable(
    revelation_chronology_underdetermination,
    'The reading''s operation requires a definitive chronological ordering of verses, but revelation order is underdetermined by the sources for a substantial fraction of the corpus — who fixes the applied order, and on what evidence?',
    'Comparative analysis of chronology reports (occasions of revelation, companion-era testimony) against the ordering the enforcement machinery actually applies, case by case.',
    'Wherever the applied order outruns the evidence, stage-certification is an act of scholarly discretion rather than textual fact — and the extraction channel runs through exactly that discretion. Tightening the evidentiary basis would shrink discretionary space and lower measured extraction; confirming the applied order would legitimize it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_chronology_underdetermination, empirical, 'Chronological underdetermination as the load-bearing discretion point of the whole arrangement.').

omega_variable(
    pedagogy_framing_sincerity,
    'Is the divine-pedagogy framing a sincerely held theological account of the revelatory sequence, or a post-hoc rationalization that protects the gatekeeping function while conceding minimal ground to harmonizers?',
    'Internal doctrinal history: examine whether the framing''s advocates accept its implications symmetrically — including that permissive stages were genuinely licensed by God and that stage-classification must yield where the evidence fails — or deploy it only defensively against rival readings.',
    'If primarily defensive, the coordination component is thinner than authored and the constraint sits nearer the snare boundary; if sincere, the tangled_rope verdict stands with the authored balance between service and rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogy_framing_sincerity, conceptual, 'Sincerity of the pedagogical framing versus authority preservation.').

omega_variable(
    finality_scope_ambiguity,
    'Does final-stage binding extend uniformly across all legal topics, or does its force vary between ritual, familial, and transactional domains where the permissive-to-restrictive pattern is differently documented?',
    'Domain-by-domain audit of which rulings the certification machinery actually treats as stage-final versus open to contextual argument.',
    'A narrower effective scope lowers measured suppression and extraction (fewer foreclosed citations); a uniform scope raises them. Current metrics assume broad-but-not-uniform application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finality_scope_ambiguity, empirical, 'Topical reach of stage-finality across legal domains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__progressive_restriction, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__progressive_restriction, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(nask_tr_t0, observed).
narrative_ontology:measurement(nask_tr_t5, naskh_principle__progressive_restriction, theater_ratio, 5, 0.13).
narrative_ontology:measurement_basis(nask_tr_t5, observed).
narrative_ontology:measurement(nask_tr_t10, naskh_principle__progressive_restriction, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(nask_tr_t10, observed).
narrative_ontology:measurement(nask_tr_t15, naskh_principle__progressive_restriction, theater_ratio, 15, 0.2).
narrative_ontology:measurement_basis(nask_tr_t15, observed).
narrative_ontology:measurement(nask_tr_t20, naskh_principle__progressive_restriction, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(nask_tr_t20, observed).
narrative_ontology:measurement(nask_tr_t25, naskh_principle__progressive_restriction, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(nask_tr_t25, observed).
narrative_ontology:measurement(nask_tr_t30, naskh_principle__progressive_restriction, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(nask_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__progressive_restriction, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(nask_be_t0, observed).
narrative_ontology:measurement(nask_be_t5, naskh_principle__progressive_restriction, base_extractiveness, 5, 0.44).
narrative_ontology:measurement_basis(nask_be_t5, observed).
narrative_ontology:measurement(nask_be_t10, naskh_principle__progressive_restriction, base_extractiveness, 10, 0.49).
narrative_ontology:measurement_basis(nask_be_t10, observed).
narrative_ontology:measurement(nask_be_t15, naskh_principle__progressive_restriction, base_extractiveness, 15, 0.53).
narrative_ontology:measurement_basis(nask_be_t15, observed).
narrative_ontology:measurement(nask_be_t20, naskh_principle__progressive_restriction, base_extractiveness, 20, 0.57).
narrative_ontology:measurement_basis(nask_be_t20, observed).
narrative_ontology:measurement(nask_be_t25, naskh_principle__progressive_restriction, base_extractiveness, 25, 0.6).
narrative_ontology:measurement_basis(nask_be_t25, observed).
narrative_ontology:measurement(nask_be_t30, naskh_principle__progressive_restriction, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(nask_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__progressive_restriction, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(nask_su_t0, observed).
narrative_ontology:measurement(nask_su_t5, naskh_principle__progressive_restriction, suppression_requirement, 5, 0.41).
narrative_ontology:measurement_basis(nask_su_t5, observed).
narrative_ontology:measurement(nask_su_t10, naskh_principle__progressive_restriction, suppression_requirement, 10, 0.47).
narrative_ontology:measurement_basis(nask_su_t10, observed).
narrative_ontology:measurement(nask_su_t15, naskh_principle__progressive_restriction, suppression_requirement, 15, 0.53).
narrative_ontology:measurement_basis(nask_su_t15, observed).
narrative_ontology:measurement(nask_su_t20, naskh_principle__progressive_restriction, suppression_requirement, 20, 0.59).
narrative_ontology:measurement_basis(nask_su_t20, observed).
narrative_ontology:measurement(nask_su_t25, naskh_principle__progressive_restriction, suppression_requirement, 25, 0.65).
narrative_ontology:measurement_basis(nask_su_t25, observed).
narrative_ontology:measurement(nask_su_t30, naskh_principle__progressive_restriction, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(nask_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__progressive_restriction, enforcement_mechanism).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, classical_abrogation).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, contextual_harmonization).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'naskh' conflates three structurally distinct claims about the relationship between earlier and later Quranic rulings. classical_abrogation (invalidation mechanism, epsilon driven by precedent-destruction), progressive_restriction (this file: staging mechanism, epsilon driven by certification-authority concentration), and contextual_harmonization (context-validity mechanism, negligible extraction, no stage-certification beneficiary). Each story carries its own epsilon, beneficiaries, and victims; they are linked here because the upstream readings are cited as evidence within the downstream contests — classical abrogation's concession that movement occurs is the premise this reading modifies, and this reading's concession that no verse is voided is the premise harmonization extends. Changing the observable (which mechanism operated in a given case) changes epsilon, which is why the label was decomposed rather than parameterized.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
