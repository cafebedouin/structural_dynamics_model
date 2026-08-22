% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__modalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: biblical_divine_nature__modalist_reading
 *   human_readable: Modalist Reading of the Divine Nature: Sequential Modes of One Person
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the divine-nature kernel: the
 *   modalist confession that Father, Son, and Spirit are successive modes or
 *   roles of one numerically singular person. Where communities hold this
 *   reading, it organizes baptismal practice (single Jesus-name baptism),
 *   ministerial authority, and membership boundaries. The reading emerged in
 *   the late-second-century monarchian controversy (Noetus, Praxeas,
 *   Sabellius), was displaced by the Nicene settlement, and was reconstructed
 *   in the 1913 Pentecostal 'New Issue,' which made rebaptism in Jesus' name
 *   the entry rite of modern oneness denominations. The arrangement genuinely
 *   coordinates Jesus-centered monotheistic piety without Greek metaphysical
 *   apparatus; it also extracts conformity, tithes, and the invalidation of
 *   converts' prior religious identity through doctrinal gatekeeping and
 *   disfellowship. KEY AGENTS (by structural relationship):
 *   oneness_denominational_leadership — agenda-setter (organized/arbitrage) —
 *   administers the boundary and receives the flows;
 *   jesus_centered_laybelievers — dual-positioned beneficiary/payer
 *   (moderate/constrained) — receive the coordination, pay the conformity;
 *   dissenting_oneness_members — primary internal target
 *   (powerless/identity_locked); trinitarian_background_converts — secondary
 *   target paying an identity toll at entry (moderate/constrained);
 *   trinitarian_church_authorities and unitarian_theologians — excluded
 *   rivals whose exclusion the enforcement machinery maintains;
 *   historians_of_doctrine — analytical observer. Claimed type and metrics
 *   are authored independently: the claim states the structure believed true;
 *   the metrics describe observed operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__modalist_reading, 0.58).
domain_priors:suppression_score(biblical_divine_nature__modalist_reading, 0.62).
domain_priors:theater_ratio(biblical_divine_nature__modalist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__modalist_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__modalist_reading, "Modalist Reading of the Divine Nature: Sequential Modes of One Person").
narrative_ontology:topic_domain(biblical_divine_nature__modalist_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__modalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__modalist_reading, 'e5dc1b4c-b22c-45e6-90b3-8dd6aa8fc057').
narrative_ontology:cs_kernel_codification('e5dc1b4c-b22c-45e6-90b3-8dd6aa8fc057', fixed_text).
narrative_ontology:cs_authority_grounding('e5dc1b4c-b22c-45e6-90b3-8dd6aa8fc057', lineage).
narrative_ontology:cs_interpretation_layer_present('e5dc1b4c-b22c-45e6-90b3-8dd6aa8fc057').
narrative_ontology:cs_reading_relation('e5dc1b4c-b22c-45e6-90b3-8dd6aa8fc057', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('e5dc1b4c-b22c-45e6-90b3-8dd6aa8fc057', biblical_divine_nature__unitarian_reading, forecloses).
narrative_ontology:cs_axiom('e5dc1b4c-b22c-45e6-90b3-8dd6aa8fc057', foundational, divine_person_numerically_one).
narrative_ontology:cs_axiom_status(divine_person_numerically_one, holdable).
narrative_ontology:cs_axiom_grounding('e5dc1b4c-b22c-45e6-90b3-8dd6aa8fc057', divine_person_numerically_one, theological).
narrative_ontology:cs_axiom('e5dc1b4c-b22c-45e6-90b3-8dd6aa8fc057', foundational, successive_modal_manifestation).
narrative_ontology:cs_axiom_status(successive_modal_manifestation, holdable).
narrative_ontology:cs_axiom_grounding('e5dc1b4c-b22c-45e6-90b3-8dd6aa8fc057', successive_modal_manifestation, theological).
narrative_ontology:cs_axiom('e5dc1b4c-b22c-45e6-90b3-8dd6aa8fc057', secondary, jesus_name_baptism_normative).
narrative_ontology:cs_axiom_status(jesus_name_baptism_normative, holdable).
narrative_ontology:cs_axiom_grounding('e5dc1b4c-b22c-45e6-90b3-8dd6aa8fc057', jesus_name_baptism_normative, conventional).
narrative_ontology:cs_reference_frame('e5dc1b4c-b22c-45e6-90b3-8dd6aa8fc057', apostolic_jesus_name_monotheism).
narrative_ontology:cs_drift_state('e5dc1b4c-b22c-45e6-90b3-8dd6aa8fc057', contemporary_restorationist_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e5dc1b4c-b22c-45e6-90b3-8dd6aa8fc057', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__modalist_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, oneness_denominational_leadership).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, jesus_centered_laybelievers).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, dissenting_oneness_members).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, trinitarian_background_converts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, jesus_centered_laybelievers).
narrative_ontology:constraint_vindicates(biblical_divine_nature__modalist_reading, strict_monotheism_preservation).
narrative_ontology:constraint_vindicates(biblical_divine_nature__modalist_reading, acts_238_baptismal_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pastors, district superintendents, and foreign-missions boards of oneness denominations. They administer baptism in Jesus' name, license ministers, publish doctrinal standards, and discipline deviation. Tithes and offerings flow through offices they control. If a fellowship fractures, their credentials and skills transfer to another oneness body or a new church plant, so their personal stake in any single enforcement structure is portable.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, oneness_denominational_leadership, agenda_setter,
    organized, generational, arbitrage, global).

% Ordinary members of oneness congregations. They receive a direct, philosophically lightweight way to worship Jesus as the one God of Israel, with a single baptismal formula and no metaphysical homework. They pay tithes, adhere to dress and attendance standards, and accept rebaptism if they arrived from a trinitarian background. Leaving means losing the congregation, friendships, often family expectation, and — in the community's own teaching — the validity of their baptism.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, jesus_centered_laybelievers, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__modalist_reading, jesus_centered_laybelievers, payer).

% Members, usually raised in the movement, who privately doubt the reading or feel drawn toward trinitarian teaching. Questioning is framed as rebellion rather than inquiry; correction, probation, or disfellowship follows open dissent. Their self-concept, their assurance of salvation, and their entire social world are bound up with the community, so walking out feels like becoming a different person and risking their soul.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, dissenting_oneness_members, payer,
    powerless, biographical, identity_locked, local).

% People from mainstream churches who join oneness congregations seeking deeper spiritual experience. Entry requires rebaptism in Jesus' name and public repudiation of their prior baptism as invalid, along with absorption of anti-trinitarian instruction. Returning to their former churches afterward carries stigma and severed relationships, so the door they came through narrows behind them.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, trinitarian_background_converts, payer,
    moderate, biographical, constrained, regional).

% Catholic, Orthodox, and Protestant bodies that classify the reading as the Sabellian heresy. They hold no seat in oneness deliberation; their condemnations and their vast institutional alternative shape the external environment and raise the cost of exit for members who look outward.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, trinitarian_church_authorities, excluded,
    institutional, generational, mobile, global).

% Nontrinitarian Christians who reject the reading from the opposite flank: in their view, affirming the Son's full deity compromises monotheism regardless of how the persons are counted. They engage oneness apologists in print and debate but hold no position inside the movement's teaching offices.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, unitarian_theologians, excluded,
    moderate, generational, mobile, continental).

% Academic scholars of patristics and modern religious movements. They document the second-century monarchian controversy, the Nicene displacement of modalism, and the 1913 oneness revival, and they assess whether the modern movement is continuous with ancient modalism or an independent reconstruction. They neither collect from nor pay into the arrangement.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, historians_of_doctrine, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_divine_nature__modalist_reading, oneness_denominational_leadership).
narrative_ontology:fixing_cost_class(biblical_divine_nature__modalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how strict monotheists can worship Jesus as fully God without either dividing God into multiple persons or demoting the Son: one divine person who manifests successively as Father (creator and lawgiver), Son (redeemer), and Spirit (indwelling presence) gives Jesus-centered devotion a direct warrant with no philosophical apparatus. The same confession coordinates a single baptismal formula and a shared community identity.
% TRANSFER_FUNCTION: Moves money (tithes and offerings), labor (ministry service and standards compliance), and doctrinal deference from lay members to denominational leadership; moves converts' prior religious identity into the movement by declaring their earlier baptisms invalid and replacing them with Jesus-name rebaptism.
% ABSENT_VOICES: Trinitarian and unitarian Christians are structurally absent from oneness teaching settings — their objections are known mainly through polemical summaries produced inside the movement. Dissenting members are physically present but silenced through correction and disfellowship mechanisms. Former members who left are absent by design, their testimony discounted as bitterness.
% DISAPPEARANCE_RATIONALE: If the modalist discipline vanished overnight, oneness denominations would lose the boundary that distinguishes them from trinitarian Christianity: rebaptism practice would collapse, ministerial licensing tied to the reading would lose its object, and millions of members would re-sort into trinitarian churches, unitarian assemblies, or a looser undifferentiated Jesus-piety. The leadership's authority structure, funded by the boundary it polices, would dissolve with it.
% FOUNDING_PROBLEM: In the late-second-century monarchian controversy, and again in the 1913 Pentecostal revival, the problem was how to preserve the monarchy (sole rule) of God — strict numerical monotheism — while maintaining full divine worship of Jesus, without importing Greek metaphysical categories (ousia, hypostasis) that seemed to compromise the simplicity of the apostolic preaching.
% FOUNDING_PROBLEM_CORROBORATION: Patristic witnesses outside the benefiting parties — Tertullian's Adversus Praxeam and Hippolytus' Elenchus — document that the tension between monotheism and the worship of Christ drove real second- and third-century conflict, and modern academic historians of doctrine corroborate the same driver behind the 1913 'New Issue.' Trinitarian and unitarian theologians alike attest the tension existed; none attests that the modalist solution resolves it. Corroboration covers the existence of the founding problem, not the correctness of this reading's answer.
narrative_ontology:disappearance_verdict(biblical_divine_nature__modalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__modalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__modalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_divine_nature__modalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__modalist_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__modalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__modalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__modalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.58 at interval end): the rebaptism requirement, tithe flows through leadership-controlled offices, and the invalidation of converts' prior baptism are real transfers, but the doctrine itself is free to adopt and delivers a genuine theological good. Suppression (0.62) reflects active enforcement — doctrinal gatekeeping, probation, disfellowship — operating on members whose physical exit is legally unobstructed; much of the suppressive force is social and internalized rather than structural. Theater ratio is low-moderate (0.25): worship, teaching, and baptism administration perform real functions for participants; the theatrical share is concentrated in boundary-rehearsal polemics against trinitarianism. Accessibility collapse is moderate-low (0.45): trinitarian and unitarian alternatives remain physically available everywhere, though identity fusion makes them feel closed to insiders. Resistance (0.55) is sustained — external condemnation from the overwhelming majority of Christendom plus steady internal attrition — yet the reading persists and grows in its enclaves. The temporal series runs on one shared nine-point grid (every tracked metric authored at every point) and traces a full rise-suppression-dormancy-revival cycle: ancient ascent (190-325), imperial-era squeeze and medieval dormancy (325-1500), and rapid twentieth-century reconstruction (1913-2026). The cycle's driver is environmental — external suppression capacity and periodic restorationist revivals — not intermittent reinforcement; the oscillation is a side effect of the surrounding religious economy, not an extraction mechanism. Base properties are measured at the interval endpoint (2026).
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the leadership seat the arrangement is faithful stewardship of recovered apostolic truth: the boundary is the gospel, and enforcing it is protection, not extraction. From the laybeliever seat it is a fair trade — a coherent, worshipable God in exchange for conformity and tithe — with the costs visible mainly at moments of doubt. From the dissenter seat the same structure is a sealed room: questioning is pathologized, and exit is experienced as self-destruction rather than relocation. From the excluded rival seats (trinitarian, unitarian) the whole arrangement is simply error with an enforcement budget. The engine computes these divergent classifications from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Leadership sits near the beneficiary end of directionality: they collect the flows, set the rules, and hold arbitrage-grade exit (portable credentials across oneness fellowships), which further damps their effective extraction. Laybelievers sit near-symmetric: the beneficiary declaration captures their genuine gain, the secondary payer role and constrained exit capture their real costs — the derivation should land them mid-scale, not at the subsidized pole. Dissenting members sit near the full-target end: they bear the enforcement's costs with identity-locked exit, the worst combination the structure produces. Trinitarian-background converts bear a concentrated entry toll (invalidated baptism, severed ties) with constrained exit — high d, though less than dissenters since the toll is paid once at the door. The excluded rivals are outside the transfer surface; their structural significance is that their exclusion is the object the enforcement machinery exists to maintain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preserving strict monotheism while worshipping Jesus without Greek metaphysics — is disputed but not dead: for holders it is as live as it was in 190 or 1913, and the arrangement's enforcement is functional, not vestigial. Theater ratio stays low, no sunset clause exists or is claimed, and the administrator class demonstrably profits from maintenance, so the piton signature is absent. The tangled_rope classification does real work here in both directions: it prevents reading the arrangement as a pure snare (which would erase the genuine coordination of accessible Jesus-centered monotheism that millions sincerely use), and it prevents reading it as a pure rope (which would erase the rebaptism toll, the silenced dissenters, and the leadership-captured revenue that the same structure produces). Mandatrophy is not resolved; the mandate and the extraction matured together.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (modalist_reading) of the kernel biblical_divine_nature; would instantiating a sibling reading instead change the constraint''s structure wholesale?',
    'Not resolvable by evidence within any single framework — the readings are logically exclusive commitments. Resolution occurs only through adherence migration: track whether communities shift between readings and observe the resulting structural rewrite (victim sets, enforcement objects, beneficiary flows all change with the reading adopted).',
    'If a community moved to the trinitarian_reading, this constraint''s enforcement machinery (Jesus-name rebaptism, anti-modalist boundary policing) dissolves and is replaced by creedal-conformity enforcement with a different victim set; if it moved to the unitarian_reading, the victims become those affirming the Son''s full deity. The classification computed here is valid only while this reading governs the community.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer-frame routing: one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.62) structural, internalized, or both?',
    'Post-exit suppression trajectory: interview former members who physically left oneness communities. If fear of damnation for their (to them valid) baptism, reflexive doctrinal deference, and social isolation persist years after exit with no external barrier present, a large internalized share is established.',
    'If substantially internalized, the constraint''s effective suppression exceeds the structural measure — targets carry the enforcement with them after exit, raising effective extraction for the dissenter seat and strengthening the case that the arrangement holds members by identity fusion rather than by mere preference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in oneness membership discipline.').

omega_variable(
    rebaptism_boundary_separability,
    'Is the rebaptism-and-enforcement machinery separable from the genuine coordination of Jesus-centered monotheistic piety?',
    'Comparative study of oneness bodies that relaxed the rebaptism requirement or softened disfellowship practices versus those that retained them: measure retention, doctrinal coherence, and growth. If relaxed bodies retain coherent Jesus-centered monotheism, the functions are separable.',
    'If separable, the extraction component is removable without dissolving the coordination function — supporting a reform pathway that keeps the rope and sheds the tangle. If inseparable, part of the measured extraction is the price of the boundary that constitutes the community''s identity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rebaptism_boundary_separability, empirical, 'Whether the constraint''s coordination and extraction components are structurally separable.').

omega_variable(
    ancient_modern_continuity,
    'Is the modern oneness movement continuous with second-third-century modalism, or an independent 1913 reconstruction that merely converges on the same reading?',
    'Genealogical scholarship: trace documentary transmission paths (was ancient modalist literature available and cited by 1913 revival figures?) versus independent reinvention from shared proof-texts (Acts 2:38, Isaiah 9:6, John 10:30).',
    'If discontinuous, the dormant-period segment of the measurement series (roughly 500-1900) describes a different instantiation than the modern one, and the modern series should be interpreted as a fresh constraint lifecycle rather than a revival of the old — changing how drift detection reads the 1913 inflection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ancient_modern_continuity, empirical, 'Historical continuity between ancient modalism and the modern oneness movement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__modalist_reading, 190, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bdn_modalist_tr_t190, biblical_divine_nature__modalist_reading, theater_ratio, 190, 0.18).
narrative_ontology:measurement_basis(bdn_modalist_tr_t190, observed).
narrative_ontology:measurement(bdn_modalist_tr_t215, biblical_divine_nature__modalist_reading, theater_ratio, 215, 0.24).
narrative_ontology:measurement_basis(bdn_modalist_tr_t215, observed).
narrative_ontology:measurement(bdn_modalist_tr_t325, biblical_divine_nature__modalist_reading, theater_ratio, 325, 0.36).
narrative_ontology:measurement_basis(bdn_modalist_tr_t325, observed).
narrative_ontology:measurement(bdn_modalist_tr_t500, biblical_divine_nature__modalist_reading, theater_ratio, 500, 0.3).
narrative_ontology:measurement_basis(bdn_modalist_tr_t500, observed).
narrative_ontology:measurement(bdn_modalist_tr_t1500, biblical_divine_nature__modalist_reading, theater_ratio, 1500, 0.1).
narrative_ontology:measurement_basis(bdn_modalist_tr_t1500, observed).
narrative_ontology:measurement(bdn_modalist_tr_t1913, biblical_divine_nature__modalist_reading, theater_ratio, 1913, 0.31).
narrative_ontology:measurement_basis(bdn_modalist_tr_t1913, observed).
narrative_ontology:measurement(bdn_modalist_tr_t1945, biblical_divine_nature__modalist_reading, theater_ratio, 1945, 0.27).
narrative_ontology:measurement_basis(bdn_modalist_tr_t1945, observed).
narrative_ontology:measurement(bdn_modalist_tr_t1985, biblical_divine_nature__modalist_reading, theater_ratio, 1985, 0.23).
narrative_ontology:measurement_basis(bdn_modalist_tr_t1985, observed).
narrative_ontology:measurement(bdn_modalist_tr_t2026, biblical_divine_nature__modalist_reading, theater_ratio, 2026, 0.25).
narrative_ontology:measurement_basis(bdn_modalist_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(bdn_modalist_be_t190, biblical_divine_nature__modalist_reading, base_extractiveness, 190, 0.32).
narrative_ontology:measurement_basis(bdn_modalist_be_t190, observed).
narrative_ontology:measurement(bdn_modalist_be_t215, biblical_divine_nature__modalist_reading, base_extractiveness, 215, 0.44).
narrative_ontology:measurement_basis(bdn_modalist_be_t215, observed).
narrative_ontology:measurement(bdn_modalist_be_t325, biblical_divine_nature__modalist_reading, base_extractiveness, 325, 0.56).
narrative_ontology:measurement_basis(bdn_modalist_be_t325, observed).
narrative_ontology:measurement(bdn_modalist_be_t500, biblical_divine_nature__modalist_reading, base_extractiveness, 500, 0.38).
narrative_ontology:measurement_basis(bdn_modalist_be_t500, observed).
narrative_ontology:measurement(bdn_modalist_be_t1500, biblical_divine_nature__modalist_reading, base_extractiveness, 1500, 0.14).
narrative_ontology:measurement_basis(bdn_modalist_be_t1500, observed).
narrative_ontology:measurement(bdn_modalist_be_t1913, biblical_divine_nature__modalist_reading, base_extractiveness, 1913, 0.48).
narrative_ontology:measurement_basis(bdn_modalist_be_t1913, observed).
narrative_ontology:measurement(bdn_modalist_be_t1945, biblical_divine_nature__modalist_reading, base_extractiveness, 1945, 0.6).
narrative_ontology:measurement_basis(bdn_modalist_be_t1945, observed).
narrative_ontology:measurement(bdn_modalist_be_t1985, biblical_divine_nature__modalist_reading, base_extractiveness, 1985, 0.63).
narrative_ontology:measurement_basis(bdn_modalist_be_t1985, observed).
narrative_ontology:measurement(bdn_modalist_be_t2026, biblical_divine_nature__modalist_reading, base_extractiveness, 2026, 0.58).
narrative_ontology:measurement_basis(bdn_modalist_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(bdn_modalist_su_t190, biblical_divine_nature__modalist_reading, suppression_requirement, 190, 0.28).
narrative_ontology:measurement_basis(bdn_modalist_su_t190, observed).
narrative_ontology:measurement(bdn_modalist_su_t215, biblical_divine_nature__modalist_reading, suppression_requirement, 215, 0.42).
narrative_ontology:measurement_basis(bdn_modalist_su_t215, observed).
narrative_ontology:measurement(bdn_modalist_su_t325, biblical_divine_nature__modalist_reading, suppression_requirement, 325, 0.72).
narrative_ontology:measurement_basis(bdn_modalist_su_t325, observed).
narrative_ontology:measurement(bdn_modalist_su_t500, biblical_divine_nature__modalist_reading, suppression_requirement, 500, 0.55).
narrative_ontology:measurement_basis(bdn_modalist_su_t500, observed).
narrative_ontology:measurement(bdn_modalist_su_t1500, biblical_divine_nature__modalist_reading, suppression_requirement, 1500, 0.1).
narrative_ontology:measurement_basis(bdn_modalist_su_t1500, observed).
narrative_ontology:measurement(bdn_modalist_su_t1913, biblical_divine_nature__modalist_reading, suppression_requirement, 1913, 0.46).
narrative_ontology:measurement_basis(bdn_modalist_su_t1913, observed).
narrative_ontology:measurement(bdn_modalist_su_t1945, biblical_divine_nature__modalist_reading, suppression_requirement, 1945, 0.59).
narrative_ontology:measurement_basis(bdn_modalist_su_t1945, observed).
narrative_ontology:measurement(bdn_modalist_su_t1985, biblical_divine_nature__modalist_reading, suppression_requirement, 1985, 0.64).
narrative_ontology:measurement_basis(bdn_modalist_su_t1985, observed).
narrative_ontology:measurement(bdn_modalist_su_t2026, biblical_divine_nature__modalist_reading, suppression_requirement, 2026, 0.62).
narrative_ontology:measurement_basis(bdn_modalist_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__modalist_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, biblical_divine_nature__unitarian_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the Trinity debate.' The label conflates three structurally distinct constraints — three readings of one kernel (biblical_divine_nature) — each with its own stable epsilon, beneficiary/victim structure, and enforcement object. This file is the modalist_reading; the trinitarian_reading and unitarian_reading are separate stories. The upstream claim each reading cites as evidence is the shared scriptural corpus, but the readings extract from different populations and are enforced by different machinery, so no single story can carry one epsilon across them. All family members link via network.affects_constraints; contamination propagates across the family when one reading's institutional position shifts (e.g., a trinitarian body absorbing oneness congregations rewrites the victim sets of this constraint).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
