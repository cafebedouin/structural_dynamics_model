% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__liturgical_habituation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__liturgical_habituation_reading, []).

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
 *   constraint_id: nicene_creed_authority__liturgical_habituation_reading
 *   human_readable: Nicene Creed as Liturgical Identity Boundary (Habituation Reading)
 *   domain: religious/ecclesial/historical-doctrinal
 *
 * SUMMARY:
 *   This story authors the liturgical-habituation member of the
 *   nicene_creed_authority constraint family. The constraint under
 *   description: a fixed baptismal formula, the Nicene Creed, is performed
 *   corporately at regular intervals of common worship; the performance
 *   itself - shared words, shared rhythm, shared posture - constitutes and
 *   signals membership in the community, and it does so whether or not any
 *   given reciter holds the formula's metaphysical claims as settled personal
 *   conviction. Entry into the community runs through learning the words;
 *   continuity across generations runs through repeating them; the edge of
 *   the community is wherever the shared idiom stops. Nothing material is
 *   collected from reciters, no office audits private belief, and departure
 *   is open. Two sibling readings of the same creed-text exist as separate
 *   constraint stories with their own epsilon values and victim structures;
 *   this file authors only the liturgical mechanism, and the family edges are
 *   recorded under network. See commentary.kernel_context for the
 *   committer-frame placement. KEY AGENTS (by structural relationship): -
 *   worshipping_congregations: primary beneficiary (organized/constrained) -
 *   receives shared identity, entry doorway, intergenerational continuity -
 *   parish_clergy_and_liturgists: agenda_setter with secondary beneficiary
 *   position (institutional/constrained) - administers the rite, vocationally
 *   fused to its continuance - newcomers_and_catechumens: beneficiary
 *   (moderate/mobile) - the formula is their learnable entry into belonging -
 *   dissenting_private_believers: payer with secondary beneficiary position
 *   (moderate/constrained) - recite while privately wrestling with clauses;
 *   bear dissonance, receive belonging - nontrinitarian_communities: excluded
 *   (organized/mobile) - marked outside by the boundary; maintain parallel
 *   communities - ecumenical_dialogue_bodies: beneficiary
 *   (institutional/mobile) - use the creed as the one shared starting text -
 *   liturgical_scholars: analytical observer (analytical/analytical) - study
 *   the mechanism, hold no office in it
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__liturgical_habituation_reading, 0.05).
domain_priors:suppression_score(nicene_creed_authority__liturgical_habituation_reading, 0.06).
domain_priors:theater_ratio(nicene_creed_authority__liturgical_habituation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0.06).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__liturgical_habituation_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__liturgical_habituation_reading, "Nicene Creed as Liturgical Identity Boundary (Habituation Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__liturgical_habituation_reading, "religious/ecclesial/historical-doctrinal").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__liturgical_habituation_reading, '2e4b44c9-537c-4ff8-949c-3cd34c7ce31c').
narrative_ontology:cs_kernel_codification('2e4b44c9-537c-4ff8-949c-3cd34c7ce31c', fixed_text).
narrative_ontology:cs_authority_grounding('2e4b44c9-537c-4ff8-949c-3cd34c7ce31c', practice).
narrative_ontology:cs_interpretation_layer_present('2e4b44c9-537c-4ff8-949c-3cd34c7ce31c').
narrative_ontology:cs_reading_relation('2e4b44c9-537c-4ff8-949c-3cd34c7ce31c', nicene_creed_authority__strict_orthodox_reading, influences).
narrative_ontology:cs_reading_relation('2e4b44c9-537c-4ff8-949c-3cd34c7ce31c', nicene_creed_authority__symbolic_confessional_reading, influences).
narrative_ontology:cs_axiom('2e4b44c9-537c-4ff8-949c-3cd34c7ce31c', foundational, performance_precedes_assent).
narrative_ontology:cs_axiom_status(performance_precedes_assent, holdable).
narrative_ontology:cs_axiom_grounding('2e4b44c9-537c-4ff8-949c-3cd34c7ce31c', performance_precedes_assent, conventional).
narrative_ontology:cs_axiom('2e4b44c9-537c-4ff8-949c-3cd34c7ce31c', secondary, lex_orandi_establishes_lex_credendi).
narrative_ontology:cs_axiom_status(lex_orandi_establishes_lex_credendi, holdable).
narrative_ontology:cs_axiom_grounding('2e4b44c9-537c-4ff8-949c-3cd34c7ce31c', lex_orandi_establishes_lex_credendi, conventional).
narrative_ontology:cs_reference_frame('2e4b44c9-537c-4ff8-949c-3cd34c7ce31c', performed_formula_constitutes_belonging).
narrative_ontology:cs_drift_state('2e4b44c9-537c-4ff8-949c-3cd34c7ce31c', post_disestablishment_secularized_worship, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('2e4b44c9-537c-4ff8-949c-3cd34c7ce31c', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, worshipping_congregations).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, parish_clergy_and_liturgists).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, newcomers_and_catechumens).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, ecumenical_dialogue_bodies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, dissenting_private_believers).
narrative_ontology:constraint_victim(nicene_creed_authority__liturgical_habituation_reading, dissenting_private_believers).
narrative_ontology:constraint_vindicates(nicene_creed_authority__liturgical_habituation_reading, lex_orandi_lex_credendi).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gathered communities that recite the creed together at scheduled services. The shared formula gives them a common idiom recognizable across languages and centuries; learning it is part of joining. Leaving is possible but means surrendering the community's shared life, so most stay and pass the words to children and converts.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, worshipping_congregations, beneficiary,
    organized, generational, constrained, global).

% Ordained leaders and liturgy committees who schedule the recitation, choose translations and musical settings, teach the formula to catechumens, and decide when it may be omitted. They administer an inherited text they did not write; altering it requires synodical action beyond any single parish. Their vocation is bound up with the rite they conduct.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, parish_clergy_and_liturgists, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__liturgical_habituation_reading, parish_clergy_and_liturgists, beneficiary).

% People joining the community. Memorizing and reciting the creed is the standard doorway: it gives them something concrete to learn, rehearse, and eventually perform with everyone else. They arrive voluntarily and can leave freely; the formula lowers the cost of entry by making belonging learnable.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, newcomers_and_catechumens, beneficiary,
    moderate, biographical, mobile, local).

% Members who recite the creed while privately wrestling with one clause or another - the filioque, a particular title, a miracle clause. They keep participating because the community's life matters to them; the liturgy asks their voices, not their certainties. Their alternative is to fall silent during a line, a visible but generally tolerated gesture.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, dissenting_private_believers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__liturgical_habituation_reading, dissenting_private_believers, beneficiary).

% Groups - historic Unitarians, Latter-day Saints, Jehovah's Witnesses, and others - whose theology cannot accommodate the creed's Trinitarian formulas. They stand outside the reciting communities by their own conviction; the boundary marks them as other, and they maintain parallel communities with their own identity practices. They take no part in any conversation about how the rite is conducted.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, nontrinitarian_communities, excluded,
    organized, generational, mobile, global).

% Bilateral and multilateral councils that use the creed as the one text all major branches already share. It gives them a common starting document; disagreements such as the filioque and jurisdictional questions are negotiated around it. They can cite or drop references at will and answer to no single congregation.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, ecumenical_dialogue_bodies, beneficiary,
    institutional, generational, mobile, global).

% Academic historians and rituologists who study how the creed entered the liturgy, how its performance spread, and what recitation does to communities. They publish analyses the communities may ignore or absorb; they hold no office in the rite.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, liturgical_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_creed_authority__liturgical_habituation_reading, diffuse).
narrative_ontology:fixing_cost_class(nicene_creed_authority__liturgical_habituation_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the dispersed-community identification problem once, centrally: a fixed, memorizable, publicly performable formula lets a community spanning languages and centuries recognize itself, admit newcomers through a learnable doorway, and mark its edge - without case-by-case doctrinal examination of any member.
% TRANSFER_FUNCTION: Moves minutes of attention and memorization effort from each participant into a collective public act; moves belonging, recognition, and a shared idiom back to participants; moves no material wealth. Net flow: individual attention into communal identity maintenance, returned distributed as membership.
% ABSENT_VOICES: Nontrinitarian communities would object that the boundary marks them outside a conversation conducted about a text they reject; they are absent because the boundary defines them out of the reciting communities' deliberations, and they deliberate in parallel bodies of their own. Within the communities, private doubters rarely voice reservations in any liturgical forum - the rite provides no slot for negotiated assent, only for voiced formula - so their objections surface, if at all, in private conversation or exit. Historically, coerced professions under enforcement eras left no recorded dissenting voice inside the rite at all.
% DISAPPEARANCE_RATIONALE: If the recitation vanished overnight, entry would lose its fixed doorway (catechesis would scatter into improvised instruction), intergenerational continuity of shared idiom would fray within a generation or two, ecumenical bodies would lose their one universally shared starting text, and the community edge would blur until some replacement boundary - doctrinal testing, political alignment, moral code - re-emerged. Worship would rearrange around substitutes; the rearrangement, not the disappearance, is the verdict.
% FOUNDING_PROBLEM: Fourth-century congregations faced competing accounts of Christ's relation to God (the Nicene homoousion versus subordinationist alternatives) with no fixed public formula to distinguish communities; bishops needed a compact, memorizable, performable summary that any baptized person could carry, repeat, and be recognized by, binding scattered congregations to one confession amid imperial politics and rival episcopal networks.
% FOUNDING_PROBLEM_CORROBORATION: External academic historiography of the fourth-century controversies attests both the founding problem and its resolution - the Arian crisis is treated as closed in the standard scholarly literature, a literature produced outside any benefiting congregation. Liturgical-history scholarship further attests that the recitation generalized into routine identity formation after the original crisis passed. No beneficiary body claims the Arian emergency persists; no external source disputes that it ended. Corroboration is thus external, and it attests the death of the founding problem alongside the vitality of the successor function.
narrative_ontology:disappearance_verdict(nicene_creed_authority__liturgical_habituation_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__liturgical_habituation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__liturgical_habituation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nicene_creed_authority__liturgical_habituation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__liturgical_habituation_reading, 0.05, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__liturgical_habituation_reading_tests).
:- end_tests(nicene_creed_authority__liturgical_habituation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.05: the act costs reciters minutes and memorization, collects no fees, and pays no office-holder; exit is open and exercised. Suppression 0.06 reflects the post-disestablishment surface: no sanction attaches to private disbelief, and falling silent during a clause is tolerated in most congregations. Theater_ratio 0.22 is deliberately low for a liturgical constraint: the metric tracks performative activity substituting for lost function, and here performance IS the function - uncomprehended recitation still performs identity work, which is this reading's core claim. The modest rise across the interval tracks heritage-mode recitation in secularized contexts where community ties have thinned. Accessibility_collapse 0.30: alternatives remain fully workable - attend elsewhere, fall silent, depart; the formula collapses no option space. Resistance 0.15: occasional conscientious refusal (clause-sitting, non-reciting traditions) without organized opposition inside the reciting communities. The three measurement series share one eight-point grid (381-2026) so every metric is authored at every examined time point. Suppression_requirement is authored because the story specifically traces enforcement-capacity change: imperial and medieval enforcement machinery rises to a Lateran-era peak and collapses after toleration and disestablishment, while the liturgical mechanism persists beneath it - that divergence is the empirical spine of the low-suppression profile. Boltzmann identity_coordination is declared because the constraint's primary function is membership-boundary maintenance; the identity-framing gaming check passes structurally: no seat extracts, so no Power-by-Scope concentration of extraction exists for identity framing to excuse.
 *
 * PERSPECTIVAL GAP:
 *   From the clergy seat the arrangement is inherited stewardship: conducting a rite older than any incumbent, alterable only by synodical action, with professional-vocational identity fused to its continuance - were that vocational frame to break, the administrative seat would experience the formula as one revisable element among others. From the newcomer seat it is a learnable doorway. From the dissenting-believer seat it is a tolerated tension - asked for voices, not certainties. From the excluded nontrinitarian seat the same performance is a wall. The engine computes these divergent per-seat classifications from the power, exit, and role data; the authored rope claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (congregations, newcomers, ecumenical bodies, and clergy in their secondary capacity) derive low directionality - the arrangement subsidizes them with belonging, entry, and a common touchstone. The one declared payer seat, dissenting_private_believers, bears a real but small cost - minutes of attention and occasional dissonance - against received belonging, placing them near symmetric rather than at the target pole; no directionality override is authored because the role-plus-exit derivation already lands near that judgment. Excluded nontrinitarian communities sit outside the arrangement: nothing flows from them to any beneficiary seat, and they have exited into parallel communities. Suppression is authored as a raw structural property and is not scaled by power or scope; extractiveness is scaled by directionality and global scope in the engine's computation, but with base epsilon at 0.05 even amplified effective extraction remains near the coordination-cost floor.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - supplying a fixed public formula against fourth-century subordinationism - is dead as originally posed: the Arian emergency is extinct, and external historiography attests its resolution. The arrangement persists because the mechanism solves a recurring general problem (how a dispersed community maintains shared identity without case-by-case doctrinal testing), not because anyone maintains a fiction. The R5 mismatch signature (dead founding problem paired with a world_rearranges verdict) is therefore authored knowingly: the cross-check against theater_ratio finds 0.22, far below piton territory, and no seat profits from the persistence, so the correct resolution is function migration rather than capture or zombie maintenance. The classification prevents mislabeling in both directions: reading the enforcement-era suppression numbers forward would misclassify a rope as a snare; reading the theatrical surface (widespread uncomprehended recitation) as atrophy would misclassify a live coordination function as a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'This story instantiates the liturgical_habituation_reading of the nicene_creed_authority kernel; the strict_orthodox_reading and symbolic_confessional_reading are separate constraints authored from the same text. Which reading governs a given community''s actual operation?',
    'Observe sanction behavior: whether any office audits private belief or penalizes deviation (strict reading operative), whether the text is treated as revisable witness (symbolic reading), or whether recitation alone marks belonging (this reading).',
    'Strict adoption in a community raises epsilon sharply, adds private dissenters as victims, and shifts classification toward enforced hybrid; symbolic adoption dissolves the fixed-formula coordination and lowers boundary stability. This story''s low-extraction profile holds only under this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Committer-frame allocation: one kernel, three readings, three distinct constraints.').

omega_variable(
    authority_locus_disagreement,
    'Where is the kernel contest located: does the creed''s authority inhere in the propositional content of the text (binding an ontology) or in the communal act of performing it (constituting a people) - and is cognitive assent a membership requirement?',
    'Comparative analysis of doctrinal canons and liturgical law across traditions: whether assent is examined at admission, whether formula revision requires ontological unanimity, whether performance without assent is counted as participation.',
    'Resolution toward content-locus converts this constraint into the strict sibling with a different victim set; resolution toward act-locus with revisability converts it toward the symbolic sibling. The epsilon gap between siblings is wide, so the location decision dominates classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_locus_disagreement, conceptual, 'Structural location of the kernel contest: text-content versus performed act.').

omega_variable(
    enforcement_independence,
    'Has the liturgical mechanism ever sustained communal identity without coercive backup, or is the current low-suppression state an artifact of modern disestablishment?',
    'Compare pre-Constantinian house-church practice, records of coerced profession under imperial and medieval enforcement, and post-toleration retention curves: if identity markers held where enforcement was absent, the mechanism is enforcement-independent.',
    'If the mechanism historically rode on enforcement, the rope classification is regime-contingent and the post-disestablishment suppression collapse predicts boundary erosion; if independent, the profile is robust across regimes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_independence, empirical, 'Whether low suppression is structural or a disestablishment artifact.').

omega_variable(
    assent_absorption_limit,
    'How much private disbelief can the performed formula absorb before the boundary function degrades into heritage recitation with no constitutive effect?',
    'Longitudinal survey work correlating reciter belief, recitation retention, and community reproduction rates across congregations with differing belief profiles.',
    'Within limits, the reading''s independence claim holds and theater stays low; beyond the limit, theater_ratio climbs, the identity function hollows, and the constraint drifts toward inertial maintenance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(assent_absorption_limit, empirical, 'Upper bound on assent-independence before boundary function fails.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__liturgical_habituation_reading, 381, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t381, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 381, 0.05).
narrative_ontology:measurement(nice_tr_t787, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 787, 0.08).
narrative_ontology:measurement(nice_tr_t1054, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1054, 0.12).
narrative_ontology:measurement(nice_tr_t1215, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1215, 0.18).
narrative_ontology:measurement(nice_tr_t1546, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1546, 0.14).
narrative_ontology:measurement(nice_tr_t1700, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1700, 0.16).
narrative_ontology:measurement(nice_tr_t1965, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(nice_tr_t2026, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 2026, 0.22).

% Extraction over time
narrative_ontology:measurement(nice_be_t381, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 381, 0.06).
narrative_ontology:measurement(nice_be_t787, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 787, 0.07).
narrative_ontology:measurement(nice_be_t1054, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1054, 0.09).
narrative_ontology:measurement(nice_be_t1215, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1215, 0.11).
narrative_ontology:measurement(nice_be_t1546, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1546, 0.09).
narrative_ontology:measurement(nice_be_t1700, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1700, 0.07).
narrative_ontology:measurement(nice_be_t1965, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1965, 0.05).
narrative_ontology:measurement(nice_be_t2026, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 2026, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t381, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 381, 0.15).
narrative_ontology:measurement(nice_su_t787, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 787, 0.2).
narrative_ontology:measurement(nice_su_t1054, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1054, 0.3).
narrative_ontology:measurement(nice_su_t1215, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1215, 0.45).
narrative_ontology:measurement(nice_su_t1546, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1546, 0.4).
narrative_ontology:measurement(nice_su_t1700, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1700, 0.28).
narrative_ontology:measurement(nice_su_t1965, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1965, 0.08).
narrative_ontology:measurement(nice_su_t2026, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 2026, 0.06).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__liturgical_habituation_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__symbolic_confessional_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'authority of the Nicene Creed' covers multiple structurally distinct claims and decomposes per the epsilon-invariance principle into a three-member family: this liturgical-habituation member (epsilon 0.05, no victims, coordination-shaped), a strict-orthodox member (substantially extractive: sanction machinery extracts metaphysical conformity, private dissenters are victims), and a symbolic-confessional member (low extraction, different structure: revisable witness, authority from communal discernment, no fixed formula). Measuring the creed's authority one way yields near-zero extraction and another way yields high extraction precisely because they are different constraints. The family edges record the dependency: this member feeds both siblings by supplying the habituated reciting population that enforcement acts upon and the shared performed text that reinterpretation works on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
