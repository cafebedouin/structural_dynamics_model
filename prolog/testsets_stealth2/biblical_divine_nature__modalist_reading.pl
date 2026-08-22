% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__modalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__modalist_reading, []).

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
 *   constraint_id: biblical_divine_nature__modalist_reading
 *   human_readable: Modalist Reading of the Divine Nature (One Person, Successive Modes)
 *   domain: theology/religious authority/doctrinal history
 *
 * SUMMARY:
 *   The kernel 'biblical divine nature' — what the apostolic witness commits
 *   believers to about God's inner constitution — is read three ways. This
 *   story instantiates the modalist reading: Father, Son, and Spirit are
 *   successive modes or roles of one divine person, not three simultaneous
 *   persons. The reading first appears in late-second-century Asia Minor
 *   (Noetus, Praxeas), is refuted by Tertullian and Hippolytus, is associated
 *   with Sabellius in the early third century, loses institutional ground
 *   decisively at Nicaea (325) and Constantinople (381), survives marginally
 *   for centuries, and re-emerges at scale in the 1913-1916 Pentecostal 'New
 *   Issue,' crystallizing into Oneness (Apostolic/Jesus-Name) denominations
 *   that persist today. Per the epsilon-invariance principle this file
 *   authors ONLY the modalist reading as a clean constraint with its own
 *   stable epsilon, beneficiary/victim structure, and enforcement history;
 *   the trinitarian and unitarian readings are separate stories linked
 *   through network.affects_constraints. Claim and metrics are independent:
 *   the reading is CLAIMED as tangled_rope (genuine coordination function
 *   plus asymmetric costs borne through the same structure) while the metrics
 *   describe its actual operation across the interval.
 *
 * KEY AGENTS:
 *   - oneness_movement_leadership: agenda-setter and principal beneficiary (organized/identity_locked) — administers the doctrine, collects distinctiveness, credential authority, and institutional identity from its operation
 *   - oneness_laity: beneficiary with payer overlay (moderate/constrained) — receive the coherent devotional frame and community, bear rebaptism and conformity costs
 *   - doctrinal_dissenters_in_oneness_bodies: primary target (powerless/trapped) — bear expulsion, credential revocation, and severance costs
 *   - rebaptized_trinitarian_background_converts: payer-beneficiary (moderate/constrained) — bear the rebaptism cost as the price of membership
 *   - trinitarian_majority_churches: excluded rival adjudicators (institutional/trapped) — locked out of the Oneness conversation, their condemnations shape the reading's status from outside
 *   - academic_patristics_scholarship: analytical observer (analytical/analytical) — reconstructs the debate's history outside all confessional enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__modalist_reading, 0.42).
domain_priors:suppression_score(biblical_divine_nature__modalist_reading, 0.55).
domain_priors:theater_ratio(biblical_divine_nature__modalist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__modalist_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__modalist_reading, "Modalist Reading of the Divine Nature (One Person, Successive Modes)").
narrative_ontology:topic_domain(biblical_divine_nature__modalist_reading, "theology/religious authority/doctrinal history").

domain_priors:requires_active_enforcement(biblical_divine_nature__modalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__modalist_reading, '16af5cda-3e9d-4936-8687-d6de285f2953').
narrative_ontology:cs_kernel_codification('16af5cda-3e9d-4936-8687-d6de285f2953', fixed_text).
narrative_ontology:cs_authority_grounding('16af5cda-3e9d-4936-8687-d6de285f2953', lineage).
narrative_ontology:cs_interpretation_layer_present('16af5cda-3e9d-4936-8687-d6de285f2953').
narrative_ontology:cs_reading_relation('16af5cda-3e9d-4936-8687-d6de285f2953', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('16af5cda-3e9d-4936-8687-d6de285f2953', biblical_divine_nature__unitarian_reading, forecloses).
narrative_ontology:cs_axiom('16af5cda-3e9d-4936-8687-d6de285f2953', foundational, divine_person_is_numerically_one).
narrative_ontology:cs_axiom_status(divine_person_is_numerically_one, holdable).
narrative_ontology:cs_axiom_grounding('16af5cda-3e9d-4936-8687-d6de285f2953', divine_person_is_numerically_one, theological).
narrative_ontology:cs_axiom('16af5cda-3e9d-4936-8687-d6de285f2953', secondary, successive_redemptive_manifestations).
narrative_ontology:cs_axiom_status(successive_redemptive_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('16af5cda-3e9d-4936-8687-d6de285f2953', successive_redemptive_manifestations, theological).
narrative_ontology:cs_reference_frame('16af5cda-3e9d-4936-8687-d6de285f2953', apostolic_jesus_name_monotheism).
narrative_ontology:cs_drift_state('16af5cda-3e9d-4936-8687-d6de285f2953', contemporary_oneness_revival_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('16af5cda-3e9d-4936-8687-d6de285f2953', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__modalist_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, oneness_movement_leadership).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, oneness_laity).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, doctrinal_dissenters_in_oneness_bodies).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, rebaptized_trinitarian_background_converts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, rebaptized_trinitarian_background_converts).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, oneness_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% General superintendents, district boards, and doctrinal committees of Oneness denominations administer the teaching that Father, Son, and Holy Spirit are successive manifestations of one divine person. They license ministers, require Jesus-name baptism for credentialing, screen doctrinal statements, and discipline deviation. Their organizations' distinctiveness among Christian bodies rests on this teaching; abandoning it would mean dissolving the ministries, Bible colleges, and fellowships they have spent their lives building.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, oneness_movement_leadership, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__modalist_reading, oneness_movement_leadership, beneficiary).

% Members attend congregations where worship, baptism, and preaching assume the one-person reading. They receive a coherent devotional frame centered on Jesus and a close-knit community. Costs arrive as required rebaptism for those arriving from other churches, expectations of doctrinal conformity, and separation from trinitarian relatives and ecumenical activity. Leaving typically means losing their primary social world.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, oneness_laity, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__modalist_reading, oneness_laity, payer).

% Members who study their way to trinitarian or unitarian conclusions find their position untenable inside the movement: teaching licenses are revoked, congregational fellowship withdraws, and in close-knit congregations family and employment ties come under strain. Most recant quietly; those who leave forfeit nearly everything the community provided.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, doctrinal_dissenters_in_oneness_bodies, payer,
    powerless, biographical, trapped, global).

% Converts from trinitarian churches accept baptism a second time, in the name of Jesus only, as the price of membership. The act severs their former congregation's recognition of their first baptism and enrolls them in the new community; many describe it as both a sincere spiritual commitment and a costly public repudiation of their past.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, rebaptized_trinitarian_background_converts, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__modalist_reading, rebaptized_trinitarian_background_converts, beneficiary).

% The Catholic, Orthodox, and mainstream Protestant bodies that ratified the Nicene settlement hold the one-person reading to be the ancient Sabellian error and decline to recognize Oneness ordination or sacramental validity. They stand outside the Oneness conversation; their objections are well known inside Oneness bodies but carry no vote there.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, trinitarian_majority_churches, excluded,
    institutional, generational, trapped, global).

% Historians of doctrine reconstruct the second- and third-century debates from Noetus and Praxeas through Sabellius, documenting both the seriousness of the problem the one-person reading addressed and the textual record of its rejection. They publish outside any confessional enforcement structure and answer to peer review rather than to any of the confessions involved.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, academic_patristics_scholarship, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_divine_nature__modalist_reading, oneness_movement_leadership).
narrative_ontology:fixing_cost_class(biblical_divine_nature__modalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of affirming the full deity of Christ and the experiential reality of Father/Son/Spirit language while preserving strict numerical monotheism, without requiring Greek metaphysical vocabulary (ousia, hypostasis). Provides a single object of worship, a unified baptismal formula, and a shared identity marker distinguishing the community from both trinitarian and unitarian neighbors.
% TRANSFER_FUNCTION: Moves doctrinal allegiance and ritual compliance (rebaptism, annual doctrinal assent, credential dependence) from members to the movement's leadership; moves distinctiveness capital to the movement vis-a-vis the wider Christian world; moves dissenters' standing in the community to zero upon reaching forbidden conclusions.
% ABSENT_VOICES: Trinitarian theologians and historians, who would contest the apostolic-originality genealogy by citing the conciliar record, are absent from Oneness doctrinal adjudication; unitarian biblical scholars, who would contest the inference from Jesus' exaltation to his identity with the Father, are likewise absent. Inside Oneness bodies, members who reach alternative conclusions are removed through credential revocation and fellowship withdrawal before their arguments circulate widely.
% DISAPPEARANCE_RATIONALE: If the one-person reading and its enforcement vanished overnight, roughly thirty million adherents' worship, baptismal practice, and denominational identity would reorganize: Oneness bodies would either merge into trinitarian evangelicalism or fragment, ministerial credentialing structures built on the Jesus-name standard would dissolve, and the rebaptism economy connecting converts to the movement would lose its object.
% FOUNDING_PROBLEM: In the late second century the church faced the question of how the exalted Jesus relates to the one God of Israel: how to worship Christ as divine without abandoning inherited monotheism. The one-person successive-modes reading was built to answer this — Father and Son are the same person appearing in different redemptive roles — preserving monotheism at zero metaphysical cost.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by academic patristics scholarship: historians of dogma (from Harnack's History of Dogma to Hanson's The Search for the Christian Doctrine of God) attest that reconciling Christ-devotion with monotheism was the live second-century problem and that the one-person reading was a serious, internally coherent attempt at it — while the same scholarship documents the reading's rejection. Trinitarian historians concede the genuineness of the problem even as they condemn the solution; no Oneness body's self-attestation is relied on.
narrative_ontology:disappearance_verdict(biblical_divine_nature__modalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__modalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__modalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_divine_nature__modalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__modalist_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__modalist_reading_tests).
:- end_tests(biblical_divine_nature__modalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42 at interval end) reflects real costs borne through the structure — mandatory rebaptism, doctrinal assent, credential dependence, expulsion of dissenters — exceeding the doctrine's service to those who bear them, while stopping short of arrangements whose coordination story is pure cover. Suppression (0.55) is the current enforcement picture: ministerial credential control, congregational discipline, and social shunning, without physical coercion. Theater (0.25) is low-moderate: rebaptism and boundary-marking do real entry work, but a growing share of denominational activity defends distinctiveness itself rather than performing the doctrine's devotional function. Accessibility collapse is low (0.35): the rival readings are not merely available but demographically dominant, so grasping the one-person claim collapses no one's alternatives. Resistance is high (0.75): seventeen centuries of official condemnation and continuous polemics. The three measurement series share one grid (190, 220, 325, 381, 1054, 1913, 1916, 2026) so every metric is authored at every examined point. Suppression_requirement is authored because enforcement capacity is this story's central dynamic: it rises with local disciplinary machinery in the second and third centuries, collapses when imperial-backed rejection destroys the reading's institutional footing after 381, goes near-zero during the long dormancy, and rebuilds with twentieth-century denominationalization. Identity-lock binds the leadership seat: the movement's organizations have become their doctrine, so exit for leaders is self-annihilation rather than career change; if that fusion broke, the leadership seat's persistence motive would weaken and the arrangement would drift toward voluntary association.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the leadership seat the arrangement is apostolic Christianity restored — pure coordination under siege from a philosophized rival. From the dissenter seat the same structure is enforced conformity with expulsion for reaching wrong conclusions. From the convert seat it is a costly but chosen entry rite. From the excluded trinitarian seat it is simply the Sabellian error resurfacing. The engine computes these per-seat classifications from the structural data (power, exit, role); the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Leadership sits near the beneficiary end: it administers the arrangement and collects distinctiveness, credential authority, and institutional identity from its operation. Dissenters sit near the target end: they bear expulsion and severance costs with no offsetting benefit and no exit that preserves their social world. Laity and rebaptized converts are genuinely dual-positioned — they receive the devotional and communal goods AND bear conformity and rebaptism costs — so the automatic derivation from their beneficiary declarations alone would push their directionality too low; the directionality_overrides entry for the moderate power atom corrects this to near-symmetric (0.45). Excluded and observer seats fall outside the extraction arithmetic: the trinitarian majority is locked out of adjudication rather than governed by the arrangement, and the scholarly seat observes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to worship the exalted Christ as divine without abandoning the monotheism of Israel — remains live wherever Christianity exists, so this is not a mandate outliving its function; founding_problem_status 'live' crossed with disappearance verdict 'world_rearranges' produces no zombie flag. The classification guards against two symmetrical errors: reading the arrangement as pure extraction (the classical heresy frame) erases the genuine devotional and communal work it performs for millions of adherents; reading it as pure coordination (the Oneness self-description) erases the dissenters who pay expulsion costs through the same structure that coordinates everyone else. Tangled rope holds both halves. Mandatrophy is not resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    apostolic_originality_ambiguity,
    'Does the modalist reading recover the actual teaching of the apostolic era, or is it a second-century construction — and again a nineteenth/twentieth-century reconstruction — retrojected onto Acts 2:38?',
    'Pre-Nicene textual record: if undisputed first-generation sources showed the one-person successive-modes formula as normative, the lineage claim would strengthen; the extant record (the New Testament''s silence on the question''s later technical terms, Tertullian''s Praxeas treating the teaching as an innovation of his own lifetime) currently cuts the other way.',
    'If innovation, the reading''s lineage authority grounding weakens toward practice-based or organizationally sustained authority, and the founding-problem genealogy shifts from ''restoration'' to ''response''; if recovery, the reading''s legitimacy claims strengthen against both siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apostolic_originality_ambiguity, empirical, 'Whether the reading is apostolic recovery or retrospective construction.').

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of kernel biblical_divine_nature; how would instantiating a sibling reading change the structural data?',
    'Author the sibling stories separately and compare per-seat classifications across the three files: the trinitarian reading carries an imperial-scale enforcement ledger (its suppression series runs through Nicaea and the penal laws against dissent), and the unitarian reading carries a different victim set (those condemned for denying the Son''s deity). Never average the readings'' metrics.',
    'Classification is reading-indexed: the same historical episodes enter each reading''s ledger with opposite signs (Nicaea is the modalist reading''s catastrophe and the trinitarian reading''s foundation), so cross-reading comparison must run through the network edges, not merged numbers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one kernel, three readings, disagreement located in the divine personhood predicate.').

omega_variable(
    suppression_mechanism_internalization,
    'How much of the measured suppression among Oneness members is structural (credential control, congregational discipline) versus internalized (trained expectation that trinitarian reasoning is deceptive, so members police their own questions before any authority acts)?',
    'Post-exit trajectory of leavers: if questioning and doubt persist as burdensome patterns after departure, a substantial internalized component is present; if leavers shed the reflexes quickly, suppression was mostly structural.',
    'If largely internalized, effective suppression exceeds the structural measure and travels with members beyond the arrangement''s reach; if structural, removing credential and fellowship penalties would dissolve most of it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural versus internalized suppression in member conformity.').

omega_variable(
    mode_sequencing_temporality,
    'Are the modes strictly sequential in redemptive history (the classical Sabellian shape, implying the Father suffered on the cross) or eternally co-present functional distinctions (the softened modern Oneness shape)?',
    'Confessional comparison across Oneness bodies and their doctrinal statements over time; the major bodies shift formulation depending on whether an interlocutor raises the patripassian objection.',
    'Strict sequencing exposes the reading to the classical objection and raises its defensive enforcement load; eternal functional distinction blurs the line toward the trinitarian sibling and erodes the distinctiveness on which the movement''s identity rents rest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mode_sequencing_temporality, conceptual, 'Temporal versus eternal articulation of the three modes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__modalist_reading, 190, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bdn_modalist_tr_t190, biblical_divine_nature__modalist_reading, theater_ratio, 190, 0.12).
narrative_ontology:measurement(bdn_modalist_tr_t220, biblical_divine_nature__modalist_reading, theater_ratio, 220, 0.14).
narrative_ontology:measurement(bdn_modalist_tr_t325, biblical_divine_nature__modalist_reading, theater_ratio, 325, 0.16).
narrative_ontology:measurement(bdn_modalist_tr_t381, biblical_divine_nature__modalist_reading, theater_ratio, 381, 0.1).
narrative_ontology:measurement(bdn_modalist_tr_t1054, biblical_divine_nature__modalist_reading, theater_ratio, 1054, 0.06).
narrative_ontology:measurement(bdn_modalist_tr_t1913, biblical_divine_nature__modalist_reading, theater_ratio, 1913, 0.2).
narrative_ontology:measurement(bdn_modalist_tr_t1916, biblical_divine_nature__modalist_reading, theater_ratio, 1916, 0.24).
narrative_ontology:measurement(bdn_modalist_tr_t2026, biblical_divine_nature__modalist_reading, theater_ratio, 2026, 0.25).

% Extraction over time
narrative_ontology:measurement(bdn_modalist_be_t190, biblical_divine_nature__modalist_reading, base_extractiveness, 190, 0.22).
narrative_ontology:measurement(bdn_modalist_be_t220, biblical_divine_nature__modalist_reading, base_extractiveness, 220, 0.28).
narrative_ontology:measurement(bdn_modalist_be_t325, biblical_divine_nature__modalist_reading, base_extractiveness, 325, 0.34).
narrative_ontology:measurement(bdn_modalist_be_t381, biblical_divine_nature__modalist_reading, base_extractiveness, 381, 0.18).
narrative_ontology:measurement(bdn_modalist_be_t1054, biblical_divine_nature__modalist_reading, base_extractiveness, 1054, 0.12).
narrative_ontology:measurement(bdn_modalist_be_t1913, biblical_divine_nature__modalist_reading, base_extractiveness, 1913, 0.4).
narrative_ontology:measurement(bdn_modalist_be_t1916, biblical_divine_nature__modalist_reading, base_extractiveness, 1916, 0.46).
narrative_ontology:measurement(bdn_modalist_be_t2026, biblical_divine_nature__modalist_reading, base_extractiveness, 2026, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(bdn_modalist_su_t190, biblical_divine_nature__modalist_reading, suppression_requirement, 190, 0.18).
narrative_ontology:measurement(bdn_modalist_su_t220, biblical_divine_nature__modalist_reading, suppression_requirement, 220, 0.26).
narrative_ontology:measurement(bdn_modalist_su_t325, biblical_divine_nature__modalist_reading, suppression_requirement, 325, 0.38).
narrative_ontology:measurement(bdn_modalist_su_t381, biblical_divine_nature__modalist_reading, suppression_requirement, 381, 0.1).
narrative_ontology:measurement(bdn_modalist_su_t1054, biblical_divine_nature__modalist_reading, suppression_requirement, 1054, 0.03).
narrative_ontology:measurement(bdn_modalist_su_t1913, biblical_divine_nature__modalist_reading, suppression_requirement, 1913, 0.32).
narrative_ontology:measurement(bdn_modalist_su_t1916, biblical_divine_nature__modalist_reading, suppression_requirement, 1916, 0.48).
narrative_ontology:measurement(bdn_modalist_su_t2026, biblical_divine_nature__modalist_reading, suppression_requirement, 2026, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__modalist_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, biblical_divine_nature__unitarian_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Trinity debate' covers three structurally distinct constraints instantiated from one kernel (biblical_divine_nature): this modalist reading, the trinitarian reading, and the unitarian reading. Their epsilon values differ because their enforcement histories differ — the trinitarian reading carries the imperial settlement's enforcement ledger, the modalist reading carries conciliar condemnation plus modern denominational discipline, the unitarian reading carries its own condemnation trail. Each is authored separately with its own stable epsilon and linked here. Upstream/downstream: the trinitarian settlement's condemnations shape the conditions under which the modalist reading persists (see revival_pressure in cs_structure.drift_state); the modalist reading's modern revival in turn pressures both siblings by contesting the exclusivity of the Nicene settlement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_divine_nature__modalist_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
