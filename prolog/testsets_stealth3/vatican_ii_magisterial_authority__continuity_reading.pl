% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__continuity_reading
 *   human_readable: Vatican II Continuity Hermeneutic (Organic-Development Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   The constraint under authorship is the operative hermeneutic rule that
 *   Vatican II's sixteen documents may be received, translated, taught, and
 *   implemented only as organic development within the pre-existing
 *   magisterium — never as a new founding. Its enforcement surface is
 *   concrete: curial approval of translations and liturgical books, seminary
 *   curriculum control, disciplinary measures against dissenting teachers,
 *   and the systematic ruling-out of 'spirit of the council' implementations
 *   as unauthorized. The standing arrangement this reading assesses includes
 *   the persistent gap between the continuity claim and actual practice:
 *   vernacular-only liturgy despite the Council's own Latin-preservation
 *   prescription, pastoral practice outrunning the letter, and periodic
 *   restriction of the pre-conciliar rite by the same authority that
 *   professes continuity. This file is one of three readings of the same
 *   kernel (see kernel_context and network note); it generates ONLY the
 *   continuity reading as a clean, epsilon-invariant constraint. KEY AGENTS
 *   (by structural relationship): - holy_see_magisterium: Agenda-setter and
 *   principal beneficiary (institutional/identity_locked) — promulgates and
 *   enforces the interpretive rule; collects the legitimacy dividend of an
 *   unbroken magisterium - progressive_theologians: Primary payer
 *   (moderate/constrained) — readings ruled unauthorized ex ante; bear
 *   censorship and career costs - traditionalist_communities: Payer with
 *   secondary beneficiary position (organized/identity_locked) — hold this
 *   very reading as their defense; pay in canonical irregularity when
 *   enforcement restricts the patrimony the mandate nominally protects -
 *   diocesan_clergy_mainstream: Payer (moderate/constrained) — bear the daily
 *   interpretive labor between the flanks - lay_faithful: Beneficiary with
 *   secondary payer exposure (powerless/constrained) — receive
 *   guaranteed-stable doctrine; bear transition whiplash with no interpretive
 *   voice - secular_historians_of_council: Excluded (moderate/arbitrage) —
 *   archival scholarship complicating the continuity narrative sits outside
 *   the conversation - ecumenical_observers: Observer
 *   (institutional/analytical) — Orthodox and Protestant partners test the
 *   continuity claim against their own records
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, 0.64).
domain_priors:suppression_score(vatican_ii_magisterial_authority__continuity_reading, 0.63).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__continuity_reading, "Vatican II Continuity Hermeneutic (Organic-Development Reading)").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__continuity_reading, '38cd84ec-04f3-4224-9b47-c281d0d9bd9c').
narrative_ontology:cs_kernel_codification('38cd84ec-04f3-4224-9b47-c281d0d9bd9c', fixed_text).
narrative_ontology:cs_authority_grounding('38cd84ec-04f3-4224-9b47-c281d0d9bd9c', lineage).
narrative_ontology:cs_interpretation_layer_present('38cd84ec-04f3-4224-9b47-c281d0d9bd9c').
narrative_ontology:cs_reading_relation('38cd84ec-04f3-4224-9b47-c281d0d9bd9c', vatican_ii_magisterial_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('38cd84ec-04f3-4224-9b47-c281d0d9bd9c', vatican_ii_magisterial_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('38cd84ec-04f3-4224-9b47-c281d0d9bd9c', foundational, conciliar_texts_bind_within_prior_magisterium).
narrative_ontology:cs_axiom_status(conciliar_texts_bind_within_prior_magisterium, holdable).
narrative_ontology:cs_axiom_grounding('38cd84ec-04f3-4224-9b47-c281d0d9bd9c', conciliar_texts_bind_within_prior_magisterium, deontological).
narrative_ontology:cs_axiom('38cd84ec-04f3-4224-9b47-c281d0d9bd9c', secondary, promulgated_letter_over_invoked_spirit).
narrative_ontology:cs_axiom_status(promulgated_letter_over_invoked_spirit, holdable).
narrative_ontology:cs_axiom_grounding('38cd84ec-04f3-4224-9b47-c281d0d9bd9c', promulgated_letter_over_invoked_spirit, conventional).
narrative_ontology:cs_axiom('38cd84ec-04f3-4224-9b47-c281d0d9bd9c', secondary, dh_syllabus_reconcilable_by_development).
narrative_ontology:cs_axiom_status(dh_syllabus_reconcilable_by_development, holdable).
narrative_ontology:cs_axiom_grounding('38cd84ec-04f3-4224-9b47-c281d0d9bd9c', dh_syllabus_reconcilable_by_development, empirically_contingent).
narrative_ontology:cs_reference_frame('38cd84ec-04f3-4224-9b47-c281d0d9bd9c', unbroken_magisterial_continuum).
narrative_ontology:cs_drift_state('38cd84ec-04f3-4224-9b47-c281d0d9bd9c', contemporary_postconciliar_praxis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('38cd84ec-04f3-4224-9b47-c281d0d9bd9c', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, holy_see_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, lay_faithful).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, progressive_theologians).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, traditionalist_communities).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, diocesan_clergy_mainstream).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, traditionalist_communities).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, lay_faithful).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promulgates the interpretive rule for the Council's sixteen documents, approves translations and liturgical books, directs seminary curricula, and disciplines teachers whose readings depart from it. The rule's operation returns to the office the standing of an unbroken teaching voice: every prior century's definitions remain citable as one continuous act. Stepping outside the rule would mean adjudicating its own past claims against its present ones, which the office cannot do without dissolving the ground it stands on.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, holy_see_magisterium, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, holy_see_magisterium, beneficiary).

% Read the Council as authorizing developments the letter does not spell out — a revised footing for religious liberty, a re-founded liturgy, a collegial exercise of authority. Under the mandate these readings are ruled unauthorized in advance: publication is censored, teaching posts are conditional on conformity, and a viable career requires encoding disagreement in approved vocabulary. Leaving the Catholic academy forfeits the audience and the questions that define the work.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, progressive_theologians, payer,
    moderate, biographical, constrained, global).

% Hold this very reading as their shield: if the letter binds and spirit-of-the-council claims are unauthorized, then the abuses around them are implementer error rather than conciliar teaching, and the inherited rite and Latin text remain the standard. They maintain the old liturgy and formation where permitted. Yet enforcement has repeatedly moved against the patrimony the mandate nominally protects — restricting access to the pre-conciliar rite while professing continuity — leaving them canonically irregular and suspect, and unable to walk away without abandoning the identity the patrimony constitutes.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, traditionalist_communities, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, traditionalist_communities, beneficiary).

% Implement the texts at parish level under the mandate: preach continuity, use approved translations, answer both flanking constituencies. They carry the daily labor of reconciling documents with practice — explaining why the older liturgy is restricted while the Council's own text prescribed its preservation, or why pastoral practice outruns the letter. Clear central guidance spares them adjudicating doctrine themselves, which is the quiet benefit attached to the same dependency.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, diocesan_clergy_mainstream, payer,
    moderate, biographical, constrained, continental).

% Receive doctrine presented as unchanged across the Council and inherit the practical results of its implementation — a new liturgy, revised catechesis, reordered institutions. They hold no seat in interpretation; their recourse is attendance, giving, and departure. Stability of teaching is the good they draw; disorientation when practice shifts faster than the promised continuity is the cost they carry without a voice.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, lay_faithful, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, lay_faithful, payer).

% Work the archives of the conciliar process — drafting histories, minority positions, pressure campaigns inside the aula. Their findings routinely show contingency, lobbying, and late revision where the organic-development narrative shows smooth maturation. They stand entirely outside the mandate's jurisdiction: no canonical penalty reaches them, but neither does any hearing; their scholarship circulates in the academy while the interpretive rule is set without them.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, secular_historians_of_council, excluded,
    moderate, biographical, arbitrage, global).

% Orthodox and Protestant partners track whether Rome's continuity claim survives contact with the record. The Orthodox test it against their own charge that Rome innovated repeatedly across the second millennium; Protestants test the religious-liberty reversal against their own history. They take no side in the intra-Catholic dispute, but their assessments condition the claim's external credibility, and they watch from a seat no Catholic enforcement reaches.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, ecumenical_observers, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__continuity_reading, holy_see_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies one authorized hermeneutic for a global communion: every bishop, seminary, translator, and catechist reads the Council under the same rule — the texts bind only as interpreted within prior teaching — preventing interpretive fragmentation across languages, faculties, and generations.
% TRANSFER_FUNCTION: Moves interpretive authority from the periphery (theologians, local churches, historians) to the Apostolic See; moves the labor of reconciliation — Dignitatis Humanae against the Syllabus, the Latin mandate against vernacular practice — onto implementers; and converts the pre-conciliar magisterium's accumulated credibility into cover for post-conciliar change.
% ABSENT_VOICES: Secular historians and archivists of the conciliar process are structurally outside the conversation: their drafting-history findings would complicate the organic-development narrative, but the mandate governs only intra-ecclesial reception. Rank-and-file laity hold no seat in interpretation. Progressive theologians are present but pre-emptively muted — their readings ruled unauthorized before they are argued.
% DISAPPEARANCE_RATIONALE: If the continuity mandate vanished overnight, the communion would promptly formalize into the rival camps the sibling readings describe: a rupture party re-founding authority on the Council alone, a traditionalist party treating pre-conciliar teaching as the sole standard, and a pluralist party managing ambiguity locally. Papal authority claims would need re-founding; translation approval, seminary curricula, and ecumenical dialogue all presuppose the shared rule.
% FOUNDING_PROBLEM: An institution that claims its definitive teaching cannot err promulgated a council containing provisions that appear to reverse prior binding teaching — religious liberty against the Syllabus, liturgical reform against the inherited rite, collegiality against centralized definitions of the papal office. The mandate was built to solve how such a council can be received without falsifying the institution's own past claims.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: historians of doctrine across confessional lines treat the Dignitatis Humanae–Syllabus tension as a genuine scholarly problem rather than an artifact of traditionalist complaint, and Orthodox observers attest the problem is live from their own seat — they locate the discontinuity earlier and treat Rome's continuity claim as precisely the thing to be tested. No corroborating source outside the beneficiary set attests that the problem is dead.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__continuity_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.64 because the reading's own assessment of the standing arrangement finds substantial appropriation: the currency of continuity (trust, obedience, the pre-conciliar magisterium's credibility) is spent on changes the texts do not authorize, while implementers bear the unpaid labor of reconciliation. Suppression is 0.63 as a RAW STRUCTURAL property — unscaled by power or scope; the engine scales only extractiveness. It reflects real enforcement machinery: translation control, curricular mandates, canonical discipline, and the pre-emptive ruling-out of whole reading classes. Theater ratio 0.42: the interpretive function is real, but a growing share of activity is performative continuity-affirmation — anniversary declarations, ceremonial professions of seamlessness — staged over visible practice-drift. Accessibility collapse is low (0.35): understanding the mandate does NOT collapse alternatives; both sibling readings survive it, which is precisely why enforcement must stay active. Resistance 0.60: sustained, two-flanked, and institutionalized. Boltzmann coordination_type is enforcement_mechanism (offset 0.08, floor 0.10, no override): the constraint's failure mode is governance fragmentation requiring dedicated enforcement infrastructure, not mere convention drift. CYCLICAL PATTERN: all three series share one nine-point grid and trace a full enforcement cycle — tightening to a 1986 peak (traditionalist crisis era), relaxation to a 2007 trough (accommodation era), re-tightening to 2025. The oscillation is itself part of the mechanism: each relaxation licenses renewed divergence on the flanks, each tightening manufactures renewed reaction, sustaining dependence on central arbitration — an intermittent-reinforcement dynamic, not noise. ASSUMPTIONS: base_properties scalars are dated at interval end (2025, the tightening phase), so they sit at a cycle peak rather than a cycle average; the interval 1965–2025 spans promulgation to the present. CLAIM/METRIC INDEPENDENCE: claimed_type tangled_rope is asserted from structure (genuine coordination function + asymmetric extraction + active enforcement, all three canonically required); the metrics are authored as descriptive facts and were not tuned to the claim or to any predicted engine output.
 *
 * PERSPECTIVAL GAP:
 *   Three seats compute materially different types from the same structure. From the Holy See's seat the arrangement is the necessary form of doctrinal order for a global communion — an experience near the coordination pole, with enforcement felt as stewardship. From the progressive theologians' seat the same structure operates as enforced closure of reading-space: the coordination is real but its price is the pre-emptive illegitimacy of their entire research program. The traditionalist seat is the sharpest divergence: they HOLD this reading, deploy it rhetorically as their shield, and yet compute as trapped payers — the enforcement apparatus costs them canonical irregularity while the mandate's letter (SC §36's Latin provision) is the very thing enforcement declines to honor. IDENTITY-LOCK DYNAMICS: the Holy See's lock is institutional identity — the office has become its continuity claim, and admitting rupture dissolves the self-justification of the magisterium as such; the traditionalists' lock is relational-formational identity constituted by the pre-conciliar patrimony, where concluding that continuity fails routes directly to sedevacantism or exit rather than to mere opinion-change. If either frame broke, the seat classifications shift sharply: a See that could adjudicate its own past would face the arrangement as a choice rather than a necessity, and traditionalists who stopped needing the continuity proof would convert from trapped defenders to mobile critics.
 *
 * DIRECTIONALITY LOGIC:
 *   The Holy See sits nearest the beneficiary pole: it declares the rule, enforces it, and collects its return (institutional legitimacy, unity, the citability of every prior century), with identity-lock amplifying its structural investment. Lay faithful derive genuine but incidental benefit (stable doctrine) at low damped cost. Progressive theologians sit near the full-target pole: they transfer interpretive authority and career security under constrained exit. Diocesan clergy sit mid-high: net payers of interpretive labor with a modest stabilizing offset. Traditionalist communities are the engineered complication: declared victims whose secondary beneficiary position and organized power damp their derived d below the pure-target end — they pay the arrangement's costs while drawing real protection from its letter. Excluded and observer seats contribute no directional pull; their absence and observation are recorded, not weighted.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — receiving an apparently doctrine-changing council without falsifying infallibility claims — is LIVE, corroborated from outside the beneficiary set, so the mismatch consumer finds status=live paired with verdict=world_rearranges: no zombie flag, and mandatrophy_resolved is honestly false. The classification work the type assignment performs: labeling this a pure Rope (the official framing — mere coordination of interpretation) would hide the flanks' real extraction and the pre-emptive suppression of whole reading classes; labeling it a Snare (the traditionalist framing — coercion dressed as fidelity) would erase the genuine coordination function, without which a billion-member communion fragments into private readings within a generation. Tangled Rope holds both truths: the coordination is real enough that its loss would rearrange the world, and the extraction is real enough that identifiable seats pay for it continuously. The piton question is checked and rejected: the interpretive function has not atrophied — enforcement is energetic, not theatrical maintenance of a corpse — though the rising theater_ratio series marks the performative layer thickening and is the number to watch for future drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates only the continuity_reading of kernel vatican_ii_magisterial_authority; what classification would the SAME standing arrangement receive under the rupture_reading or the composite_overdetermination_reading?',
    'Generate the sibling stories (vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading) with their own epsilon, beneficiaries, victims, and axioms; compare computed per-seat classifications across the family via the network edges already declared.',
    'Under the rupture reading the same arrangement should compute as far more extractive (the texts themselves become the extraction instrument and the enforcement becomes suppression of the true reading); under the composite reading extraction relocates into the ambiguity-management machinery itself. Cross-family divergence is the measurement; convergence would suggest the readings are not structurally distinct after all.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame routing: this file is one reading of a contested kernel; sibling readings are separate constraints, not hedges inside this one.').

omega_variable(
    dh_syllabus_reconciliation_status,
    'Is Dignitatis Humanae genuinely reconcilable with the Syllabus''s condemnation of religious indifferentism via the thesis/hypothesis distinction or development of doctrine, or does the reconciliation require interpretive machinery whose labor is itself part of the arrangement''s cost?',
    'Comparative doctrinal analysis tracking whether the reconciliation holds under the texts'' own categories without restating either document, plus historical study of whether the pre-conciliar condemnation was framed as universally binding or as conditioned on a political thesis the Council dropped.',
    'If the reconciliation is sound, a major block of the measured extraction is the legitimate price of doctrinal precision; if it fails, the continuity claim is carrying a permanent unfunded liability and the reading''s effective extractiveness rises toward the sibling readings'' estimates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dh_syllabus_reconciliation_status, conceptual, 'Whether the flagship reconciliation case succeeds on its own terms or is sustained by interpretive labor.').

omega_variable(
    latin_mandate_binding_force,
    'Is SC §36''s Latin-preservation provision binding law whose practical violation constitutes rupture-in-practice, or a directive legitimately subordinated to pastoral adaptation by subsequent authority?',
    'Canonical analysis of the provision''s juridical force at promulgation and of the competence of post-conciliar authority to adapt it, tested against the reading''s own principle that the letter binds and spirit-claims do not.',
    'If binding, the standing arrangement violates its own mandate and the practice-drift magnitude rises toward severe; if adaptable, the continuity reading''s internal consistency survives but its strongest traditionalist talking point weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latin_mandate_binding_force, conceptual, 'Binding force of the Latin-preservation provision under the reading''s own letter-over-spirit rule.').

omega_variable(
    enforcement_target_ambiguity,
    'Does the enforcement apparatus protect the continuity the mandate declares, or does it manage a reinterpretation while performing continuity — given that enforcement has periodically restricted the pre-conciliar patrimony the mandate''s own letter prescribes preserving?',
    'Track enforcement actions against both flanks over the next cycle: if restrictive acts concentrate on the letter-honoring flank while spirit-implementations are accommodated, enforcement is managing reinterpretation; if the letter is progressively honored, enforcement is protecting continuity.',
    'If enforcement manages reinterpretation, the theater component of the arrangement is higher than authored and the Holy See''s derived directionality is wrong in the beneficiary direction — an override toward d=0.3-0.4 would be warranted; if it protects continuity, the authored profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_target_ambiguity, conceptual, 'Whether enforcement serves the declared mandate or a managed divergence performed as continuity.').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the measured suppression among clergy and theologians is structural (canonical penalties, translation control, career conditionality) versus internalized (formation-shaped interpretive caution that persists when enforcement relaxes)?',
    'Post-relaxation trajectory test: during accommodation phases (1993–2007), did previously disciplined readings revive, or did their holders continue self-censoring? Persistence of caution after barrier removal indicates internalized carryover.',
    'If substantially internalized, effective suppression exceeds the structural measure — relaxation cycles under-deliver the freedom they announce, deepening the cyclical trap; if mostly structural, the 2007 trough represents genuine relief and the cycle is driven by policy rather than formation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized share of the arrangement''s suppressive force on clerical and scholarly seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__continuity_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1965, 0.17).
narrative_ontology:measurement(vati_tr_t1972, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1972, 0.27).
narrative_ontology:measurement(vati_tr_t1979, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1979, 0.34).
narrative_ontology:measurement(vati_tr_t1986, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1986, 0.39).
narrative_ontology:measurement(vati_tr_t1993, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1993, 0.35).
narrative_ontology:measurement(vati_tr_t2000, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2000, 0.33).
narrative_ontology:measurement(vati_tr_t2007, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2007, 0.3).
narrative_ontology:measurement(vati_tr_t2016, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2016, 0.35).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1965, 0.44).
narrative_ontology:measurement(vati_be_t1972, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1972, 0.55).
narrative_ontology:measurement(vati_be_t1979, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1979, 0.61).
narrative_ontology:measurement(vati_be_t1986, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1986, 0.63).
narrative_ontology:measurement(vati_be_t1993, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1993, 0.58).
narrative_ontology:measurement(vati_be_t2000, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(vati_be_t2007, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2007, 0.53).
narrative_ontology:measurement(vati_be_t2016, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2016, 0.57).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2025, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1965, 0.36).
narrative_ontology:measurement(vati_su_t1972, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1972, 0.5).
narrative_ontology:measurement(vati_su_t1979, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1979, 0.58).
narrative_ontology:measurement(vati_su_t1986, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1986, 0.62).
narrative_ontology:measurement(vati_su_t1993, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1993, 0.56).
narrative_ontology:measurement(vati_su_t2000, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2000, 0.51).
narrative_ontology:measurement(vati_su_t2007, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2007, 0.48).
narrative_ontology:measurement(vati_su_t2016, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2016, 0.55).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2025, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__continuity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Family member 1 of 3 of kernel vatican_ii_magisterial_authority. The colloquial label 'was Vatican II continuous?' decomposes into three structurally distinct constraints with different epsilon: this continuity reading authors epsilon approximately 0.64 for the standing arrangement (the gap between the continuity claim and actual practice is the extraction it perceives); the rupture reading authors substantially higher epsilon (the texts themselves are the instrument); the composite reading locates extraction in the ambiguity machinery itself. The continuity reading is the upstream, officially enforced position whose enforcement shapes the operating environment of both siblings; each file carries its own beneficiaries, victims, and classification, and cross-family comparison is the measurement the corpus takes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
