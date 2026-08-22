% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__unitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__unitarian_reading, []).

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
 *   constraint_id: biblical_divine_nature__unitarian_reading
 *   human_readable: Unitarian Reading of Biblical Divine Nature: Numerical Singularity, Father Alone Is God
 *   domain: theology/religious authority/doctrinal history
 *
 * SUMMARY:
 *   This file instantiates the unitarian_reading of the contested kernel
 *   biblical_divine_nature: the commitment that God is numerically singular,
 *   that the Father alone is God, and that the Son and Spirit are subordinate
 *   to or created by him. The standing arrangement under contest - and
 *   therefore the referent of epsilon - is the credal-trinitarian settlement
 *   over the same texts (Nicaea and its enforcement apparatus through the
 *   present), assessed by this reading's own lights: a settlement that
 *   demands assent to metaphysical formulae the plain text does not state,
 *   maintains a professional interpretive class whose authority depends on
 *   that complexity, and for most of the interval backed assent with
 *   anathema, exile, and execution. The metrics below therefore describe the
 *   standing arrangement as the unitarian seat assesses it, never the
 *   reading's endorsed alternative; the reading's own community form is flat,
 *   congregational, and minimally coercive. Claim and metrics are independent
 *   authored facts: the claimed type states what this reading's coordination
 *   structure is, while the metrics state what the contested arrangement
 *   looks like from this seat. The colloquial label 'the doctrine of the
 *   Trinity' decomposes into three structurally distinct constraints over
 *   this kernel; the sibling files instantiate the others. KEY AGENTS (by
 *   structural relationship): - unitarian_congregations: primary beneficiary
 *   (organized/constrained) - gathered communities holding the reading; they
 *   pay social and historical persecution costs through holding it -
 *   antitrinitarian_teachers: agenda setter (moderate/mobile) - articulate
 *   and maintain the reading across generations -
 *   plain_sense_scripture_readers: beneficiary (powerless/constrained) - lay
 *   readers with direct, unmediated access to the text -
 *   institutional_church_hierarchy: primary target
 *   (institutional/identity_locked) - administers credal uniformity; loses
 *   jurisdiction and revenue where the reading spreads -
 *   credal_orthodoxy_establishment: secondary target
 *   (institutional/identity_locked) - professional guardianship of the Nicene
 *   settlement - abrahamic_monotheist_outsiders: excluded voice
 *   (organized/trapped) - affirm the singularity premise from outside the
 *   ecclesial conversation - historians_of_doctrine: analytical observer -
 *   traces the contest and collects from neither side
 *
 * KEY AGENTS:
 *   - unitarian_congregations: primary beneficiary seat - gathered churches whose teaching members can verify directly from the text; they bear real social and historical costs through holding the position
 *   - antitrinitarian_teachers: agenda setter seat - the movement's writers and ministers (in their eras: Arius, Faustus Socinus, Joseph Priestley, William Ellery Channing) who set the interpretive agenda and carry its public risks
 *   - plain_sense_scripture_readers: beneficiary seat - lay believers reading without credal mediation, holding interpretive autonomy at the price of standing in credal-majority settings
 *   - institutional_church_hierarchy: primary target seat - bishops, councils, and offices whose jurisdiction and revenue depend on administering a uniform credal faith; constitutionally unable to adopt the reading without dissolving their own warrant
 *   - credal_orthodoxy_establishment: secondary target seat - theologians, seminaries, and confessional institutions whose vocation and authority presuppose the triune reading
 *   - abrahamic_monotheist_outsiders: excluded seat - Jewish and Muslim communities affirming numerical monotheism, structurally outside the adjudication their premise speaks to
 *   - historians_of_doctrine: analytical observer seat - academic scholarship tracing the contest, collecting from neither outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, 0.5).
domain_priors:suppression_score(biblical_divine_nature__unitarian_reading, 0.2).
domain_priors:theater_ratio(biblical_divine_nature__unitarian_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__unitarian_reading, rope).
narrative_ontology:human_readable(biblical_divine_nature__unitarian_reading, "Unitarian Reading of Biblical Divine Nature: Numerical Singularity, Father Alone Is God").
narrative_ontology:topic_domain(biblical_divine_nature__unitarian_reading, "theology/religious authority/doctrinal history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__unitarian_reading, '6235acb7-591d-4279-95b5-ab496a847814').
narrative_ontology:cs_kernel_codification('6235acb7-591d-4279-95b5-ab496a847814', fixed_text).
narrative_ontology:cs_authority_grounding('6235acb7-591d-4279-95b5-ab496a847814', distributed).
narrative_ontology:cs_reading_relation('6235acb7-591d-4279-95b5-ab496a847814', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('6235acb7-591d-4279-95b5-ab496a847814', biblical_divine_nature__modalist_reading, forecloses).
narrative_ontology:cs_axiom('6235acb7-591d-4279-95b5-ab496a847814', foundational, father_alone_is_god).
narrative_ontology:cs_axiom_status(father_alone_is_god, holdable).
narrative_ontology:cs_axiom_grounding('6235acb7-591d-4279-95b5-ab496a847814', father_alone_is_god, theological).
narrative_ontology:cs_axiom('6235acb7-591d-4279-95b5-ab496a847814', foundational, plain_sense_scriptural_sufficiency).
narrative_ontology:cs_axiom_status(plain_sense_scriptural_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('6235acb7-591d-4279-95b5-ab496a847814', plain_sense_scriptural_sufficiency, theological).
narrative_ontology:cs_axiom('6235acb7-591d-4279-95b5-ab496a847814', secondary, son_subordinate_not_coessential).
narrative_ontology:cs_axiom_status(son_subordinate_not_coessential, holdable).
narrative_ontology:cs_axiom_grounding('6235acb7-591d-4279-95b5-ab496a847814', son_subordinate_not_coessential, theological).
narrative_ontology:cs_reference_frame('6235acb7-591d-4279-95b5-ab496a847814', apostolic_monotheism_plain_text).
narrative_ontology:cs_drift_state('6235acb7-591d-4279-95b5-ab496a847814', contemporary_post_critical_scholarship, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('6235acb7-591d-4279-95b5-ab496a847814', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__unitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, unitarian_congregations).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, plain_sense_scripture_readers).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, institutional_church_hierarchy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, credal_orthodoxy_establishment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, antitrinitarian_teachers).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, unitarian_congregations).
narrative_ontology:constraint_vindicates(biblical_divine_nature__unitarian_reading, numerical_monotheism_of_the_father).
narrative_ontology:constraint_vindicates(biblical_divine_nature__unitarian_reading, subordinationist_christology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gathered churches that hold and teach that scripture reveals one God, the Father, and that the Son and Spirit are subordinate to or originated from him. Members receive a community whose central teaching they can verify directly from the text without an authorized interpreter class. They pay heavily in the wider society around them: historically legal disability, confiscation, exile, and death; today reduced standing in ecumenical and civic religious life. Leaving means losing the community; staying means carrying the cost the surrounding credal culture attaches to the position.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, unitarian_congregations, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__unitarian_reading, unitarian_congregations, payer).

% Writers and ministers who articulate the reading and set the movement's interpretive agenda - in their own eras figures such as Arius, Faustus Socinus, Joseph Priestley, and William Ellery Channing. They publish arguments, train congregations, and decide which texts and objections the movement addresses. They collect standing and authority within the movement and bear the public risks that attach to the position: censorship, dismissal, prosecution, and in the harshest periods execution. Their mobility across borders and print networks is what kept the reading alive through suppression waves.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, antitrinitarian_teachers, agenda_setter,
    moderate, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__unitarian_reading, antitrinitarian_teachers, beneficiary).

% Lay believers who read the texts without credal mediation and find the Father-supreme reading in passages such as 'Hear, O Israel: the LORD our God is one LORD,' Jesus praying to the Father, and 'the Father is greater than I.' They receive interpretive autonomy - no authorized class stands between them and the text - and they bear whatever social cost their setting attaches to the conclusion, from family friction to exclusion from communion in credal bodies. Most have no organization and no lever except assent.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, plain_sense_scripture_readers, beneficiary,
    powerless, biographical, constrained, global).

% Bishops, councils, synods, and offices whose jurisdiction, revenue, and disciplinary power depend on administering a uniform credal faith. Wherever the reading spreads, compulsory assent weakens, the interpretive monopoly thins, and jurisdiction over doctrine erodes. The hierarchy cannot adopt the reading without dissolving the warrant of its own offices: its identity is constituted by the creed it administers, so the option of conceding is experienced as self-annihilation rather than as a choice.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, institutional_church_hierarchy, payer,
    institutional, generational, identity_locked, continental).

% The professional guardianship of the Nicene settlement - theologians, seminaries, confessional institutions, and the scholarly apparatus that defends the triune reading. Their vocation, employment, and authority presuppose that the texts teach coessential divinity; a victorious plain-sense reading would retire the apparatus along with the formulae it guards. Like the hierarchy, its members cannot take up the rival reading without forfeiting the identity their training and office constitute.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, credal_orthodoxy_establishment, payer,
    institutional, generational, identity_locked, global).

% Jewish and Muslim communities affirm numerical monotheism and have historically disputed triune formulae, sometimes explicitly. They corroborate the singularity premise from outside, yet stand outside the ecclesial conversation in which the reading is adjudicated: boundary rules on both sides discount their testimony, and they have no standing in the councils, synods, or academies where the question is formally decided.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, abrahamic_monotheist_outsiders, excluded,
    organized, generational, trapped, global).

% Academic scholars of early Christianity and doctrinal history who trace how the contest developed - the pre-Nicene diversity of christologies, the council proceedings, the enforcement records, the minority survivals. They take no side in the communities' dispute, collect nothing from either outcome, and provide the only seat from which the full structure of the contest is visible at once.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, historians_of_doctrine, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_divine_nature__unitarian_reading, institutional_church_hierarchy).
narrative_ontology:fixing_cost_class(biblical_divine_nature__unitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates believers around a shared plain-sense reading of scripture concerning God's nature, solving the interpretive-authority problem without credal mediation: any reader can verify the claim that the texts present one God, the Father, with the Son subordinate or originated, so the community forms around the text itself rather than around an authorized interpreter class.
% TRANSFER_FUNCTION: Moves interpretive authority and religious legitimacy from credal institutions (councils, hierarchies, professional theologians) to individual readers and gathered congregations; historically, holding the position also moved persecution costs onto its holders, imposed by the rival arrangement's enforcement machinery rather than collected by this one.
% ABSENT_VOICES: Jewish and Muslim monotheists would corroborate the numerical-singularity premise but are structurally outside the ecclesial adjudication; credal-body laity would often engage the plain-sense case but are buffered from encountering it by catechetical systems that pre-empt the question. Both groups are represented here only as the excluded seat.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, the gathered communities organized around it would dissolve or regroup, the kernel contest would lose its plain-sense pole and become an intra-credal quarrel, credal bodies would face no internal monotheist witness, and the ecumenical architecture that currently accommodates unitarian bodies would rearrange around their absence.
% FOUNDING_PROBLEM: The reading was articulated to resolve the tension between the scriptural record (a Father who is plainly supreme, a Son who prays to, is sent by, and is lesser than the Father) and the emerging credal formulae (coessential divinity, later triune metaphysics): does scripture teach one God who is the Father alone, or three-in-one? It was founded as a corrective to what its holders regarded as philosophically accreted doctrine layered over the text.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: academic New Testament scholarship attests that the texts carry genuine tension (subordinationist grammar alongside high christology), and Jewish and Muslim interlocutors historically attested the numerical-singularity premise; neither collects from the reading's success. The credal establishment, the principal interested party, disputes the founding problem's framing entirely, which is itself signal.
narrative_ontology:disappearance_verdict(biblical_divine_nature__unitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__unitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__unitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_divine_nature__unitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__unitarian_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__unitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__unitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon (0.50 at interval end, anchoring base_properties.extractiveness) is authored for the standing credal arrangement as this reading assesses it: assent to extra-scriptural formulae, maintenance of an interpretive priesthood, and - for most of the interval - enforcement under penalty. The series peaks at the execution of Servetus (t=1235) and decays as disestablishment proceeds. Suppression (0.20 at end) tracks enforcement capacity rather than extraction: it ratchets with imperial and confessional machinery (Nicaea at t=7, Justinian's suppression of Arian institutions at t=235, the confessional age, the 1658 expulsion at t=1340) and collapses with toleration acts and formal religious liberty. Theater rises monotonically (0.30 to 0.60): as enforcement receded, the arrangement's activity shifted toward rehearsing formulae whose scriptural derivation this reading disputes - Goodhart drift from truth-tracking toward assent-performance. The oscillations in the series (imperial favor under Constantius at t=40, the Polish toleration window at t=1291 followed by the 1658 ratchet) are driven by external political alignment, not by intermittent reinforcement operated by this constraint; the cycle is a side effect of who held the sword, documented here so the temporal analyzer does not mistake it for an extraction mechanism. All three series run on one shared twelve-point grid. Accessibility collapse is low (0.40) because understanding this reading closes no alternative: the trinitarian and modalist readings remain fully available and this reading competes by persuasion alone. Resistance is high (0.85): the reading has met anathema, expulsion, and execution across nearly the whole interval. Claimed type is rope on the reading's own structure: it coordinates communities around a verifiable plain-sense claim with flat ecclesiology and minimal coercive overhead; the costs it imposes on the credal establishment are competitive displacement of that establishment's rents, not extraction routed through this structure. The engine's per-seat computation may disagree with the claim, and that divergence is the datum this corpus exists to take.
 *
 * PERSPECTIVAL GAP:
 *   The holder seats and the establishment seats should compute differently. From the congregational seats the same two millennia read as enforced extraction their predecessors died under; from the hierarchy and establishment seats the identical record reads as faithful transmission defended against error, and this reading appears as an attack on unity rather than a competing coordination. The establishment's identity lock is constitutive: its offices' warrant is the creed, so exiting the credal frame equals self-dissolution - the engine should compute that seat near the full-target end regardless of its institutional power. Holder seats sit near the beneficiary end despite real costs, because the costs are imposed by the rival arrangement's enforcement machinery, not collected by this one; the reading's own overhead is teaching, publishing, and congregational maintenance.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (unitarian_congregations, plain_sense_scripture_readers) derive low directionality for the holder seats; victim declarations (institutional_church_hierarchy, credal_orthodoxy_establishment) derive high directionality, amplified by identity_locked exit and institutional power, and further scaled by continental-to-global scope, which makes verification of the arrangement's claims harder and tilts effective extraction upward at the targeted seats. No directionality overrides are declared: the derivation from declared structure already places each seat correctly. The receipt surface records where the contested arrangement's gains land: the hierarchy seat captures jurisdiction, revenue, and authority, so gain_flow names institutional_church_hierarchy even though that same seat is a payer of this reading's spread - receipt-of-gain and payer-role answer different questions about different flows.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - whether the texts teach Father-alone monotheism - remains live on both sides, so no mandatrophy is declared and no sunset clause exists. The classification guards against two mislabels. Reading this constraint as a snare would convert voluntary conviction-bearing into predation: holders are not farmed, they pay for a position they can verify from the text and can leave at real but finite cost, and the constraint's own machinery extracts nothing from them. Conversely, accepting the establishment's own framing of the credal settlement as natural law ('the faith once delivered') would be precisely the false-summit move this reading exists to contest - which is why emerges_naturally is false here, and why any naturality claim for the credal settlement belongs in the sibling files authored from the establishment's seat, not in this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This file instantiates the unitarian_reading of the biblical_divine_nature kernel; what structurally changes if a sibling reading is instantiated instead?',
    'Compare against the trinitarian_reading and modalist_reading files: victim sets, authority grounding, reference frames, and epsilon all shift with the reading adopted.',
    'Classification is reading-indexed; merging the readings into one constraint would break epsilon-invariance, since each reading assigns a different victim set and different extraction to the same texts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame placement of this story within the kernel''s reading set.').

omega_variable(
    epsilon_referent_assessment,
    'Epsilon here measures the standing credal-trinitarian arrangement as the unitarian reading assesses it; would the same arrangement measure differently from the establishment''s own seat?',
    'Author the trinitarian_reading file over the same referent and compare its epsilon and metric profile.',
    'Divergent epsilon across readings over a fixed referent is expected and diagnostic; convergence would indicate the referent, not the reading, dominates the measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_referent_assessment, conceptual, 'Reading-indexed epsilon over a fixed referent: the credal settlement seen from the unitarian seat.').

omega_variable(
    plain_sense_resolvability,
    'Is the claim that the plain sense of the texts favors Father-alone monotheism resolvable by historical-grammatical scholarship, or is ''plain sense'' itself theory-laden?',
    'Reception-history and philological study of the proof-texts (Mark 13:32; John 17:3; 1 Corinthians 15:28; the Wisdom traditions of Proverbs 8) across interpretive communities.',
    'A resolvable plain sense strengthens the reading''s coordination claim (text sufficiency without credal mediation); demonstrated theory-ladenness routes the dispute to the conceptual level and weakens the reading''s distinctive warrant against its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plain_sense_resolvability, empirical, 'Whether the reading''s hermeneutic foundation is empirically settleable.').

omega_variable(
    persistence_without_coercion,
    'With coercive enforcement largely dismantled, credal orthodoxy retains the overwhelming majority of adherents; does the unitarian assessment of the standing arrangement as substantially extractive survive, or does genuine function explain the retention?',
    'Comparative sociology of religious switching: conversion rates toward unitarian positions where freely available, and retention drivers inside credal bodies.',
    'If retention is preference-driven, this reading''s high-epsilon authorship of the arrangement overstates extraction and the arrangement retains unmeasured coordination value; if switching is blocked by identity and social cost, the suppression is internalized and the assessment stands with the residual enforcement understood as only part of the coercive picture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_without_coercion, empirical, 'Whether the arrangement''s persistence reflects extraction or unmeasured function, tested in the post-enforcement era.').

omega_variable(
    hierarchy_identity_lock_mechanism,
    'Is the establishment''s incapacity to concede the reading a case of identity fusion (the institution has become the creed it administers) or of rational evidential commitment?',
    'Observe institutions that revised doctrine without dissolving (mainline doctrinal relaxations) against those that treat revision as self-annihilation; compare their internal discourse about the possibility of concession.',
    'Identity fusion fixes the payer seats'' exit at identity_locked and amplifies computed extraction at those seats; evidential commitment would permit constrained exit and soften the computed extraction at the establishment seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hierarchy_identity_lock_mechanism, conceptual, 'Mechanism behind the establishment seats'' locked exit from the credal frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__unitarian_reading, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_divine_nature__unitarian_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(bibl_tr_t0, observed).
narrative_ontology:measurement(bibl_tr_t7, biblical_divine_nature__unitarian_reading, theater_ratio, 7, 0.38).
narrative_ontology:measurement_basis(bibl_tr_t7, observed).
narrative_ontology:measurement(bibl_tr_t40, biblical_divine_nature__unitarian_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement_basis(bibl_tr_t40, observed).
narrative_ontology:measurement(bibl_tr_t235, biblical_divine_nature__unitarian_reading, theater_ratio, 235, 0.42).
narrative_ontology:measurement_basis(bibl_tr_t235, observed).
narrative_ontology:measurement(bibl_tr_t1213, biblical_divine_nature__unitarian_reading, theater_ratio, 1213, 0.46).
narrative_ontology:measurement_basis(bibl_tr_t1213, observed).
narrative_ontology:measurement(bibl_tr_t1235, biblical_divine_nature__unitarian_reading, theater_ratio, 1235, 0.5).
narrative_ontology:measurement_basis(bibl_tr_t1235, observed).
narrative_ontology:measurement(bibl_tr_t1291, biblical_divine_nature__unitarian_reading, theater_ratio, 1291, 0.44).
narrative_ontology:measurement_basis(bibl_tr_t1291, observed).
narrative_ontology:measurement(bibl_tr_t1340, biblical_divine_nature__unitarian_reading, theater_ratio, 1340, 0.47).
narrative_ontology:measurement_basis(bibl_tr_t1340, observed).
narrative_ontology:measurement(bibl_tr_t1456, biblical_divine_nature__unitarian_reading, theater_ratio, 1456, 0.52).
narrative_ontology:measurement_basis(bibl_tr_t1456, observed).
narrative_ontology:measurement(bibl_tr_t1507, biblical_divine_nature__unitarian_reading, theater_ratio, 1507, 0.55).
narrative_ontology:measurement_basis(bibl_tr_t1507, observed).
narrative_ontology:measurement(bibl_tr_t1643, biblical_divine_nature__unitarian_reading, theater_ratio, 1643, 0.58).
narrative_ontology:measurement_basis(bibl_tr_t1643, observed).
narrative_ontology:measurement(bibl_tr_t1700, biblical_divine_nature__unitarian_reading, theater_ratio, 1700, 0.6).
narrative_ontology:measurement_basis(bibl_tr_t1700, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_divine_nature__unitarian_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(bibl_be_t0, observed).
narrative_ontology:measurement(bibl_be_t7, biblical_divine_nature__unitarian_reading, base_extractiveness, 7, 0.62).
narrative_ontology:measurement_basis(bibl_be_t7, observed).
narrative_ontology:measurement(bibl_be_t40, biblical_divine_nature__unitarian_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(bibl_be_t40, observed).
narrative_ontology:measurement(bibl_be_t235, biblical_divine_nature__unitarian_reading, base_extractiveness, 235, 0.7).
narrative_ontology:measurement_basis(bibl_be_t235, observed).
narrative_ontology:measurement(bibl_be_t1213, biblical_divine_nature__unitarian_reading, base_extractiveness, 1213, 0.76).
narrative_ontology:measurement_basis(bibl_be_t1213, observed).
narrative_ontology:measurement(bibl_be_t1235, biblical_divine_nature__unitarian_reading, base_extractiveness, 1235, 0.82).
narrative_ontology:measurement_basis(bibl_be_t1235, observed).
narrative_ontology:measurement(bibl_be_t1291, biblical_divine_nature__unitarian_reading, base_extractiveness, 1291, 0.74).
narrative_ontology:measurement_basis(bibl_be_t1291, observed).
narrative_ontology:measurement(bibl_be_t1340, biblical_divine_nature__unitarian_reading, base_extractiveness, 1340, 0.8).
narrative_ontology:measurement_basis(bibl_be_t1340, observed).
narrative_ontology:measurement(bibl_be_t1456, biblical_divine_nature__unitarian_reading, base_extractiveness, 1456, 0.66).
narrative_ontology:measurement_basis(bibl_be_t1456, observed).
narrative_ontology:measurement(bibl_be_t1507, biblical_divine_nature__unitarian_reading, base_extractiveness, 1507, 0.58).
narrative_ontology:measurement_basis(bibl_be_t1507, observed).
narrative_ontology:measurement(bibl_be_t1643, biblical_divine_nature__unitarian_reading, base_extractiveness, 1643, 0.52).
narrative_ontology:measurement_basis(bibl_be_t1643, observed).
narrative_ontology:measurement(bibl_be_t1700, biblical_divine_nature__unitarian_reading, base_extractiveness, 1700, 0.5).
narrative_ontology:measurement_basis(bibl_be_t1700, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_divine_nature__unitarian_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(bibl_su_t0, observed).
narrative_ontology:measurement(bibl_su_t7, biblical_divine_nature__unitarian_reading, suppression_requirement, 7, 0.6).
narrative_ontology:measurement_basis(bibl_su_t7, observed).
narrative_ontology:measurement(bibl_su_t40, biblical_divine_nature__unitarian_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement_basis(bibl_su_t40, observed).
narrative_ontology:measurement(bibl_su_t235, biblical_divine_nature__unitarian_reading, suppression_requirement, 235, 0.68).
narrative_ontology:measurement_basis(bibl_su_t235, observed).
narrative_ontology:measurement(bibl_su_t1213, biblical_divine_nature__unitarian_reading, suppression_requirement, 1213, 0.78).
narrative_ontology:measurement_basis(bibl_su_t1213, observed).
narrative_ontology:measurement(bibl_su_t1235, biblical_divine_nature__unitarian_reading, suppression_requirement, 1235, 0.85).
narrative_ontology:measurement_basis(bibl_su_t1235, observed).
narrative_ontology:measurement(bibl_su_t1291, biblical_divine_nature__unitarian_reading, suppression_requirement, 1291, 0.62).
narrative_ontology:measurement_basis(bibl_su_t1291, observed).
narrative_ontology:measurement(bibl_su_t1340, biblical_divine_nature__unitarian_reading, suppression_requirement, 1340, 0.78).
narrative_ontology:measurement_basis(bibl_su_t1340, observed).
narrative_ontology:measurement(bibl_su_t1456, biblical_divine_nature__unitarian_reading, suppression_requirement, 1456, 0.55).
narrative_ontology:measurement_basis(bibl_su_t1456, observed).
narrative_ontology:measurement(bibl_su_t1507, biblical_divine_nature__unitarian_reading, suppression_requirement, 1507, 0.42).
narrative_ontology:measurement_basis(bibl_su_t1507, observed).
narrative_ontology:measurement(bibl_su_t1643, biblical_divine_nature__unitarian_reading, suppression_requirement, 1643, 0.25).
narrative_ontology:measurement_basis(bibl_su_t1643, observed).
narrative_ontology:measurement(bibl_su_t1700, biblical_divine_nature__unitarian_reading, suppression_requirement, 1700, 0.2).
narrative_ontology:measurement_basis(bibl_su_t1700, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__unitarian_reading, information_standard).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__modalist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the doctrine of the Trinity' decomposes into three structurally distinct constraints over the fixed kernel biblical_divine_nature: trinitarian_reading, modalist_reading, and unitarian_reading (this file). Each reading has its own epsilon, beneficiary/victim structure, and authority grounding; the upstream establishment reading (trinitarian) has historically supplied the enforcement conditions under which this downstream reading's costs were imposed, which is why the family links run in both directions. This file authors the unitarian instance: victim set = the credal establishment, authority distributed among readers, reference frame apostolic monotheism read plainly. Sibling files must link back via their own network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
