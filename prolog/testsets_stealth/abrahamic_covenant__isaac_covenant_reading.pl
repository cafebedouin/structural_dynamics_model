% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__isaac_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__isaac_covenant_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: abrahamic_covenant__isaac_covenant_reading
 *   human_readable: Exclusive Isaac-Line Covenant Transmission (Genesis 17:19-21)
 *   domain: religious/comparative-theology/institutional-authority
 *
 * SUMMARY:
 *   Genesis 17 records God establishing an everlasting covenant with Abraham
 *   and specifying that it will pass through Isaac ('my covenant I will
 *   establish with Isaac'), while Ishmael receives blessing, a great nation,
 *   and his own deliverance narrative — but not the covenant. The
 *   isaac_covenant_reading fixes this as an exclusive transmission rule:
 *   covenantal standing, election, and the obligation-sign of circumcision
 *   flow only through Isaac's line. Rabbinic institutions consolidated the
 *   reading across the Mishnah, Talmud, and midrash eras; it anchors Jewish
 *   self-definition, conversion practice, and the community's answer to 'who
 *   is Israel?' under statelessness. The claim/metric gap is deliberate and
 *   is the point of this file: the reading presents the rule as divine decree
 *   — the strongest naturality claim available, hence claimed_type mountain
 *   with emerges_naturally true — while the authored metrics describe an
 *   arrangement that requires continuous interpretive enforcement,
 *   concentrates legitimacy in identifiable institutions, and imposes real
 *   costs on excluded claimants. That divergence is the false-summit
 *   measurement; it is not reconciled here. Interval mapping: t=0 is the
 *   rabbinic consolidation era (c. 200 CE), t=300 the Talmud closure (c.
 *   500), t=600 the rise of Islam (c. 800), t=1000 the disputation era (c.
 *   1200), t=1200 the expulsion-era hardening (c. 1400), t=1800 the
 *   contemporary pluralist situation.
 *
 * KEY AGENTS:
 *   - rabbinic_interpretive_institutions: agenda-setter (institutional / identity_locked) — administers the reading, trains the community in it, performs the boundary rite; collects interpretive authority
 *   - jewish_covenant_communities: primary beneficiary with obligation burden (organized / identity_locked) — receives identity, continuity, and election-standing; carries circumcision and commandment obligations
 *   - ishmaelite_lineage_claimants: target (moderate / constrained) — Near Eastern genealogical claimants whose Abrahamic inheritance the ruling voids
 *   - islamic_abrahamic_claimants: target (powerful / identity_locked) — civilization-scale counter-claim through Ishmael to Muhammad, denied wholesale by the reading
 *   - comparative_scripture_scholars: analytical observer — documents the reading's consolidation and the rival readings from outside the confessional dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, 0.66).
domain_priors:suppression_score(abrahamic_covenant__isaac_covenant_reading, 0.56).
domain_priors:theater_ratio(abrahamic_covenant__isaac_covenant_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__isaac_covenant_reading, mountain).
narrative_ontology:human_readable(abrahamic_covenant__isaac_covenant_reading, "Exclusive Isaac-Line Covenant Transmission (Genesis 17:19-21)").
narrative_ontology:topic_domain(abrahamic_covenant__isaac_covenant_reading, "religious/comparative-theology/institutional-authority").

domain_priors:requires_active_enforcement(abrahamic_covenant__isaac_covenant_reading).
domain_priors:emerges_naturally(abrahamic_covenant__isaac_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__isaac_covenant_reading, '59ddc284-8ac8-4064-8c5b-e70c117aef40').
narrative_ontology:cs_kernel_codification('59ddc284-8ac8-4064-8c5b-e70c117aef40', fixed_text).
narrative_ontology:cs_authority_grounding('59ddc284-8ac8-4064-8c5b-e70c117aef40', lineage).
narrative_ontology:cs_interpretation_layer_present('59ddc284-8ac8-4064-8c5b-e70c117aef40').
narrative_ontology:cs_reading_relation('59ddc284-8ac8-4064-8c5b-e70c117aef40', abrahamic_covenant__ishmael_covenant_reading, forecloses).
narrative_ontology:cs_reading_relation('59ddc284-8ac8-4064-8c5b-e70c117aef40', abrahamic_covenant__christian_supersessionist_reading, forecloses).
narrative_ontology:cs_axiom('59ddc284-8ac8-4064-8c5b-e70c117aef40', foundational, covenant_established_exclusively_with_isaac).
narrative_ontology:cs_axiom_status(covenant_established_exclusively_with_isaac, holdable).
narrative_ontology:cs_axiom_grounding('59ddc284-8ac8-4064-8c5b-e70c117aef40', covenant_established_exclusively_with_isaac, theological).
narrative_ontology:cs_axiom('59ddc284-8ac8-4064-8c5b-e70c117aef40', secondary, ishmael_blessed_but_outside_covenant).
narrative_ontology:cs_axiom_status(ishmael_blessed_but_outside_covenant, holdable).
narrative_ontology:cs_axiom_grounding('59ddc284-8ac8-4064-8c5b-e70c117aef40', ishmael_blessed_but_outside_covenant, theological).
narrative_ontology:cs_reference_frame('59ddc284-8ac8-4064-8c5b-e70c117aef40', exclusive_isaac_election_order).
narrative_ontology:cs_drift_state('59ddc284-8ac8-4064-8c5b-e70c117aef40', contemporary_pluralist_era, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('59ddc284-8ac8-4064-8c5b-e70c117aef40', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, rabbinic_interpretive_institutions).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, jewish_covenant_communities).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, ishmaelite_lineage_claimants).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, islamic_abrahamic_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, jewish_covenant_communities).
narrative_ontology:constraint_vindicates(abrahamic_covenant__isaac_covenant_reading, particular_election_doctrine).
narrative_ontology:constraint_vindicates(abrahamic_covenant__isaac_covenant_reading, eternal_covenant_with_isaac_seed).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts, academies, and teaching lineages that adjudicate what Genesis 17:19-21 means, train the community in that meaning, and mark the boundary ritually through circumcision performed on the eighth day as the sign of the Isaac-line covenant. Their standing rests on being the covenant's authorized interpreters; adopting an inclusive transmission reading would dissolve the warrant for their adjudicating role. Exit would mean disbanding the institutions themselves.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, rabbinic_interpretive_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Descendant communities who receive identity, continuity, and election-standing from the exclusive transmission rule, and who carry its obligations: circumcision, Sabbath and dietary discipline, and the educational labor of reproducing the community across dispersion. Leaving has historically meant assimilation under social penalty and loss of the inherited role; staying means bearing the hostility the boundary draws from rival claimants.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, jewish_covenant_communities, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__isaac_covenant_reading, jewish_covenant_communities, payer).

% Near Eastern peoples and genealogical traditions tracing descent through Ishmael — Arab tribal lineages and related groups — whose claims to share in Abraham's inheritance the ruling voids. They held no seat in the conversations that fixed the reading; their genealogy is the object being ruled on, so they cannot exit the dispute except by abandoning Abrahamic self-understanding altogether.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, ishmaelite_lineage_claimants, payer,
    moderate, generational, constrained, regional).

% From the seventh century onward, a civilization-scale community claiming covenant continuity through Ishmael to Muhammad, with its own scripture asserting the broader lineage. The exclusive reading denies this claim wholesale. The claim is constitutive of Islamic self-understanding, so exit is unavailable; the dispute persists as scripture against scripture, and the community's size and cohesion make it the reading's principal organized counterparty.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, islamic_abrahamic_claimants, payer,
    powerful, civilizational, identity_locked, global).

% Academic readers of Genesis and of its reception history who sit outside the confessional dispute. They date the texts, trace how the exclusive reading consolidated in rabbinic literature, and document the rival readings; they collect nothing from the arrangement and bear none of its obligations.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, comparative_scripture_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__isaac_covenant_reading, rabbinic_interpretive_institutions).
narrative_ontology:fixing_cost_class(abrahamic_covenant__isaac_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Answers 'who is Israel?' with a single teachable rule — descent through Isaac, marked by circumcision — letting a dispersed, stateless minority reproduce membership, obligations, and leadership across generations without territory or centralized coercion.
% TRANSFER_FUNCTION: Moves covenantal standing and recognition as Abraham's heir toward Isaac-line descendants and the institutions that adjudicate the line, and away from Ishmaelite and later Islamic claimants; moves the obligation burden onto community members; moves interpretive authority to the rabbinic chain of transmission.
% ABSENT_VOICES: Ishmaelite claimants and later Muslim theologians had no seat in the Mishnah-, Talmud-, and midrash-era conversations that consolidated the exclusive reading; within Genesis itself, Ishmael's side of the story is told entirely by the winning line's narrators. Their objection — that Ishmael receives blessing, a great nation, and his own deliverance narrative (Genesis 17:20, 21:13-18) — survives only as refracted through the tradition that excludes them.
% DISAPPEARANCE_RATIONALE: If the exclusive reading vanished overnight, Jewish self-definition, conversion practice, and the Abrahamic contest with Christianity and Islam would all rearrange: the descent-based boundary loses its textual anchor, rabbinic authority loses its adjudication warrant, and Islam's counter-claim loses its principal denial. The identity architectures of three religious civilizations shift.
% FOUNDING_PROBLEM: Preserving a small covenant community's distinct identity and transmission chain under dispersion, conquest, and assimilation pressure — answering 'who counts, and how is the chain continued?' with a rule simple enough to survive statelessness.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: historians of religion corroborate that descent-and-ritual rules solved real continuity problems for diaspora minorities, and Islamic tradition attests from its own seat that the transmission question remains live — it answers it differently. No party outside the beneficiary set attests that exclusivity specifically, rather than some transmission rule, was required; that gap is recorded as signal, not oversight.
narrative_ontology:disappearance_verdict(abrahamic_covenant__isaac_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__isaac_covenant_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__isaac_covenant_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(abrahamic_covenant__isaac_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__isaac_covenant_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__isaac_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, ExtMetricName, E),
    domain_priors:suppression_score(abrahamic_covenant__isaac_covenant_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(abrahamic_covenant__isaac_covenant_reading),
    narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(abrahamic_covenant__isaac_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.66: the exclusion voids rival Abrahamic claims while concentrating covenantal legitimacy in one line and its adjudicators — high, per the manifest's expected structural delta, but short of snare levels because insiders carry genuine mutual obligations and outsiders are not bodily coerced, only ruled on. Suppression 0.56 is a raw structural property, unscaled by power or scope: enforcement is hermeneutical, ritual, and social (authorized interpretation, the eighth-day rite, education, historic herem sanctions against deviant readings), not physical compulsion of outsiders. Theater ratio 0.26: transmission practices (rite, study, liturgy) are functionally load-bearing — the community demonstrably reproduced itself through them — while a growing medieval share of activity was polemical rehearsal of election against rivals, easing somewhat in the modern period. Accessibility collapse 0.60: inside the framework the plain sense of Genesis 17:19-21 narrows alternatives sharply, but the Ishmael-blessing verses and midrashic mercy traditions keep cracks open, and Islam keeps the inclusive alternative alive externally. Resistance 0.58: sustained civilization-scale counter-claim from the seventh century onward, supersessionist rivalry, and modern critical scholarship. The three measurement series run on one shared eight-point grid; the arc rises with Islam's emergence, peaks in the disputation/expulsion era, and partially relaxes in modernity. The rising base_extractiveness series is authored deliberately: on a claimed mountain it feeds the T17 accumulation hypothesis, which is the intended diagnostic. Identity-lock note: the rabbinic institutions exhibit institutional identity fusion (the organization has become its function — authority equals stewardship of the exclusive reading), and the communities exhibit ideological-relational fusion (chosenness as self-concept); if the frame broke, the change would be conversion-flow reversal and mass redefinition, not policy adjustment.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the insider seat the arrangement is gift-plus-obligation: election received, burdens borne as constitutive goods, extraction near nil. From the excluded-claimant seats the same structure is legitimacy-denial administered by institutions they never sat with: a cost imposed by a ruling about their own genealogy. From the agenda-setter seat it is stewardship of a trust. The engine derives these per-seat classifications from power, exit, and directionality; this file supplies the structural data and does not adjudicate between the frames.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Rabbinic institutions: declared beneficiary plus agenda-setter, identity_locked exit — d near the beneficiary end (~0.08); they collect interpretive authority and cannot abandon the reading without dissolving their own warrant. Jewish covenant communities: declared beneficiary but genuinely dual-positioned — they also carry the obligation burden (secondary payer) — so the pure-beneficiary derivation would undershoot; a directionality override sets d to 0.32 for the organized power atom. Ishmaelite claimants: declared victims, constrained exit (their genealogy is the object of the ruling) — d near the target end (~0.75). Islamic claimants: declared victims, powerful, identity_locked (the Abrahamic claim is constitutive of Islamic self-understanding), global scope — d nearest full target (~0.9), with the reading's global scope amplifying effective extraction modestly through verification difficulty. Suppression enters the computation unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy declaration: the founding problem — transmitting communal identity across dispersion — is live, and the R5 interview records live status with outside corroboration. The classification guards against two opposite mislabels. Calling the whole arrangement a snare erases the real coordination that carried a stateless minority eighteen centuries (the identity_coordination floor exists precisely so that belonging's genuine price is not read as pure rent). Calling it a mountain erases the enforced exclusion of rival claimants and the identifiable institutions that collect from the boundary. The piton test fails cleanly: the function is alive, theater is low, and the administrator bears a real cost of change — the institutions could alter the reading, but doing so would dissolve their own warrant, which is cost-asymmetry in the opposite direction from inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_decree_or_interpretive_construction,
    'Is the covenant''s exclusivity a divine decree that would bind regardless of who enforces it (a genuine mountain), or an interpretive construction maintained because it serves institutional continuity and boundary clarity?',
    'History-of-interpretation analysis of Genesis 17:19-21 alongside the Ishmael-blessing material (Genesis 17:20, 21:13-18), tracking whether the exclusive reading required ongoing adjudicative labor or settled itself the way natural regularities do.',
    'If divine decree, the mountain claim stands and the beneficiaries are incidental; if interpretive construction, the constraint reclassifies toward tangled_rope — genuine coordination carrying asymmetric exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_decree_or_interpretive_construction, conceptual, 'Natural-law vs constructed status of the exclusivity rule (FSM-documented ambiguity).').

omega_variable(
    excluded_claimant_cost_status,
    'Does legitimacy-denial impose a real cost on excluded claimants, or does the covenant simply fail to govern them, leaving nothing extracted?',
    'Trace claimant-side sources — Islamic reception of Genesis, Arab genealogical literature, Ishmaelite self-designations — for evidence of experienced denial and of resources spent contesting it.',
    'If denial is a real imposed cost, epsilon sits near the authored 0.66; if outsiders are simply ungoverned by the arrangement, epsilon drops toward 0.35 and the constraint moves toward plain rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_claimant_cost_status, conceptual, 'Whether the exclusion produces extractable loss in the denied parties or merely non-membership.').

omega_variable(
    committer_sibling_delta,
    'This constraint is one reading (isaac_covenant_reading) of the kernel abrahamic_covenant; what structurally changes under the sibling readings?',
    'Instantiate the sibling files (ishmael_covenant_reading, christian_supersessionist_reading) and compare beneficiary sets, victim sets, and epsilon; the disagreement is located in the transmission channel — who inherits the covenant.',
    'Under the ishmael reading the victim set inverts (the denying institutions become the denied claimants) and epsilon re-bases onto a different exclusion; under the supersessionist reading the victim set becomes post-temple Israel. This file''s values are valid only for the isaac reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_sibling_delta, conceptual, 'Committer structure: sibling readings would swap the victim/beneficiary architecture entirely.').

omega_variable(
    modern_enforcement_trajectory,
    'Is the modern decline in coercive enforcement (herem disuse, softened conversion barriers, interfaith engagement) a durable secularization trend or a cyclical trough?',
    'Longitudinal tracking of boundary strictness and interfaith posture across jurisdictions and denominations over multiple generations.',
    'A durable decline confirms drift toward hermeneutical-only enforcement and lowers effective suppression further; a reversal supports a cyclical enforcement model in which crisis periods re-harden the boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modern_enforcement_trajectory, empirical, 'Durability of the modern enforcement relaxation.').

omega_variable(
    suppression_structural_internalized,
    'Is the measured suppression structural (sanctions, boundary rules, conversion barriers) or internalized (chosenness fused with self-concept, obligation experienced as identity)?',
    'Post-assimilation and post-secularization cohort studies: if boundary-anxiety and obligation-scrupulosity persist after sanctions lapse, the internalized share is high.',
    'If substantially internalized, effective suppression exceeds the structural measure — members carry the boundary with them after exit becomes legally available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_internalized, empirical, 'Structural vs internalized split of the suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__isaac_covenant_reading, 0, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t0, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(abra_tr_t300, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 300, 0.18).
narrative_ontology:measurement(abra_tr_t600, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 600, 0.28).
narrative_ontology:measurement(abra_tr_t800, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 800, 0.31).
narrative_ontology:measurement(abra_tr_t1000, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1000, 0.33).
narrative_ontology:measurement(abra_tr_t1200, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1200, 0.35).
narrative_ontology:measurement(abra_tr_t1500, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1500, 0.3).
narrative_ontology:measurement(abra_tr_t1800, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1800, 0.26).

% Extraction over time
narrative_ontology:measurement(abra_be_t0, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(abra_be_t300, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 300, 0.48).
narrative_ontology:measurement(abra_be_t600, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 600, 0.6).
narrative_ontology:measurement(abra_be_t800, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 800, 0.63).
narrative_ontology:measurement(abra_be_t1000, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1000, 0.65).
narrative_ontology:measurement(abra_be_t1200, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1200, 0.67).
narrative_ontology:measurement(abra_be_t1500, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1500, 0.64).
narrative_ontology:measurement(abra_be_t1800, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1800, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t0, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(abra_su_t300, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 300, 0.45).
narrative_ontology:measurement(abra_su_t600, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 600, 0.58).
narrative_ontology:measurement(abra_su_t800, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 800, 0.62).
narrative_ontology:measurement(abra_su_t1000, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1000, 0.65).
narrative_ontology:measurement(abra_su_t1200, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1200, 0.68).
narrative_ontology:measurement(abra_su_t1500, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1500, 0.6).
narrative_ontology:measurement(abra_su_t1800, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1800, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__isaac_covenant_reading, identity_coordination).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, ishmael_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, christian_supersessionist_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, land_promise_constraint).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Abrahamic covenant' conflates structurally distinct claims with different victim sets and different epsilon values: exclusive transmission through Isaac (this file), inclusive transmission through Ishmael to Muhammad (ishmael_covenant_reading), transfer to the Church (christian_supersessionist_reading), and the territorial-grant axis (land_promise_constraint). Per the epsilon-invariance principle each is authored as its own story with its own beneficiaries, victims, and classification; the shared element is the Genesis kernel, and the readings diverge on the transmission channel. This file links the family via affects_constraints; the upstream textual claim is cited by each downstream reading as evidence for its own transmission account.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(abrahamic_covenant__isaac_covenant_reading, organized, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
