% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__allegorical_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__allegorical_displacement_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: herem_command_dt7__allegorical_displacement_reading
 *   human_readable: Herem as Internal Spiritual Warfare (Allegorical Displacement Reading)
 *   domain: biblical hermeneutics/religious ethics/commitment-system analysis
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Deuteronomy 7 herem command:
 *   the allegorical displacement reading, in which the command's 'nations'
 *   are typological placeholders for sin and temptation, and the commanded
 *   conquest is internal moral warfare. Under this reading the standing
 *   arrangement under contest — the ε referent — is the resulting ascetic
 *   discipline: a perpetual, mercy-forbidden campaign of self-examination,
 *   confession, and mortification administered by an interpretive class. The
 *   reading's own self-presentation is medicinal (a rope-shaped claim: the
 *   discipline heals, no one is targeted, the text is saved); the authored
 *   metrics below describe the arrangement's actual operation independently
 *   of that claim, and the engine measures the divergence. Per the
 *   ε-invariance principle, the kernel decomposes into three reading-stories
 *   — this file, durable_separation_reading, and
 *   contextual_supersession_reading — each with its own ε, victim set, and
 *   classification; the contest between readings is recorded in the omegas
 *   and cs_structure, not inside this constraint.
 *
 * KEY AGENTS:
 *   - - clerical_interpreter_class: Agenda-setter and principal collector (institutional/arbitrage) — administers the typological key, defines sin, hears confession, receives the deference and continuity the discipline generates
 *   - - practicing_believers: Primary bearer of the relocated discipline (moderate/identity_locked) — performs the interior war; dual-positioned as recipient of formation and belonging
 *   - - scrupulosity_prone_devotees: Concentrated-harm seat (powerless/trapped) — carries the discipline's pathological tail, with no stable stopping rule
 *   - - believing_community: Collective beneficiary (organized/constrained) — receives moral curriculum, cohesion, and canon integrity
 *   - - abstract_vices_personified_as_canaanite_nations: Non-agent designee of the displaced violence — records where the victim slot was moved; bears and collects nothing
 *   - - religious_trauma_survivors: Excluded voice (powerless/mobile) — objects from outside the interpretive conversation
 *   - - psychologists_of_religion: Analytical observer (analytical/analytical) — measures scrupulosity prevalence and formation outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__allegorical_displacement_reading, 0.46).
domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, 0.55).
domain_priors:theater_ratio(herem_command_dt7__allegorical_displacement_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__allegorical_displacement_reading, tangled_rope).
narrative_ontology:human_readable(herem_command_dt7__allegorical_displacement_reading, "Herem as Internal Spiritual Warfare (Allegorical Displacement Reading)").
narrative_ontology:topic_domain(herem_command_dt7__allegorical_displacement_reading, "biblical hermeneutics/religious ethics/commitment-system analysis").

domain_priors:requires_active_enforcement(herem_command_dt7__allegorical_displacement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__allegorical_displacement_reading, 'd6cba15b-c54e-4b71-8a9f-732f8dd253a9').
narrative_ontology:cs_kernel_codification('d6cba15b-c54e-4b71-8a9f-732f8dd253a9', fixed_text).
narrative_ontology:cs_authority_grounding('d6cba15b-c54e-4b71-8a9f-732f8dd253a9', lineage).
narrative_ontology:cs_interpretation_layer_present('d6cba15b-c54e-4b71-8a9f-732f8dd253a9').
narrative_ontology:cs_reading_relation('d6cba15b-c54e-4b71-8a9f-732f8dd253a9', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('d6cba15b-c54e-4b71-8a9f-732f8dd253a9', herem_command_dt7__contextual_supersession_reading, coexists_with).
narrative_ontology:cs_axiom('d6cba15b-c54e-4b71-8a9f-732f8dd253a9', foundational, herem_nations_denote_vices_not_ethnic_groups).
narrative_ontology:cs_axiom_status(herem_nations_denote_vices_not_ethnic_groups, holdable).
narrative_ontology:cs_axiom_grounding('d6cba15b-c54e-4b71-8a9f-732f8dd253a9', herem_nations_denote_vices_not_ethnic_groups, theological).
narrative_ontology:cs_axiom('d6cba15b-c54e-4b71-8a9f-732f8dd253a9', foundational, perpetual_mortification_of_sin_obligatory).
narrative_ontology:cs_axiom_status(perpetual_mortification_of_sin_obligatory, holdable).
narrative_ontology:cs_axiom_grounding('d6cba15b-c54e-4b71-8a9f-732f8dd253a9', perpetual_mortification_of_sin_obligatory, instrumental).
narrative_ontology:cs_reference_frame('d6cba15b-c54e-4b71-8a9f-732f8dd253a9', typological_moral_pedagogy).
narrative_ontology:cs_drift_state('d6cba15b-c54e-4b71-8a9f-732f8dd253a9', contemporary_historical_critical_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('d6cba15b-c54e-4b71-8a9f-732f8dd253a9', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, believing_community).
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, clerical_interpreter_class).
narrative_ontology:constraint_victim(herem_command_dt7__allegorical_displacement_reading, abstract_vices_personified_as_canaanite_nations).
narrative_ontology:constraint_victim(herem_command_dt7__allegorical_displacement_reading, practicing_believers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, practicing_believers).
narrative_ontology:constraint_victim(herem_command_dt7__allegorical_displacement_reading, scrupulosity_prone_devotees).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, typological_exegesis_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Preaches, teaches, and administers the typological reading: trains members to map the Canaanite nations onto classes of sin, prescribes examination of conscience, hears confessions, and sets the standards by which progress in the interior war is judged. The reading's continuance sustains the office's authority over both text and conscience; those who hold the method can move between institutions and eras, and it travels with them.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, clerical_interpreter_class, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__allegorical_displacement_reading, clerical_interpreter_class, beneficiary).

% Receives a shared moral curriculum, a common vocabulary for struggle, and retention of its canon as scripture without ethnic targeting. Its cohesion and boundary-marking depend on the discipline continuing; members who drift from the practice meet informal sanction rather than expulsion.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, believing_community, beneficiary,
    organized, generational, constrained, global).

% Carry out the daily work the reading prescribes: self-examination, confession, resistance to desire, testimony of struggle. They receive formation, belonging, and assurance in return, and most affirm the trade. Leaving the practice means leaving the community and the salvation-framework it mediates, so exit is rare even for the exhausted; the war they are commanded to wage has no terminal victory, only maintenance.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, practicing_believers, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__allegorical_displacement_reading, practicing_believers, beneficiary).

% A minority of practitioners for whom the commanded vigilance becomes compulsive: repeated confession, inability to accept absolution, intrusive guilt. Pastoral literature within the tradition recognizes the pattern and counsels moderation, but the discipline's demand for mercilessness toward sin supplies no stable stopping rule, and their identity-fusion with the struggle makes stepping back feel like surrender.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, scrupulosity_prone_devotees, payer,
    powerless, biographical, trapped, global).

% Not an actor: the personified sins and temptations that the reading installs as the command's true object. The text orders their total destruction; they are the designated target of the relocated warfare and can bear no cost and collect no gain. Their presence here records where the victim slot was moved, not a party that pays.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, abstract_vices_personified_as_canaanite_nations, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(herem_command_dt7__allegorical_displacement_reading, abstract_vices_personified_as_canaanite_nations).

% Former members whose scrupulosity or guilt-architecture outlasted their membership. They attribute lasting psychological harm to the discipline and would contest its benevolent self-description, but they have exited the communities and liturgies where the reading is taught and hold no seat in its interpretive conversations.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, religious_trauma_survivors, excluded,
    powerless, biographical, mobile, global).

% Researchers who measure scrupulosity prevalence, treatment outcomes, and correlations between perfectionist piety and religious distress. They publish outside the tradition's authority structure and are consulted erratically by pastoral programs.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, psychologists_of_religion, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__allegorical_displacement_reading, clerical_interpreter_class).
narrative_ontology:fixing_cost_class(herem_command_dt7__allegorical_displacement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives a scriptural community a workable moral pedagogy and a canon-retention strategy: a shared mapping of the text's nations onto a taxonomy of vices, a common regimen (examination, confession, mortification) that organizes formation, and a holiness boundary drawn between virtue and vice instead of between peoples.
% TRANSFER_FUNCTION: Moves continuous self-scrutiny labor, attention, and deference from practicing believers to the interpretive institution; historically also material flows (penitential economies, alms and fees attached to confession and absolution). In return believers receive formation, belonging, and assurance — goods the institution administers and renders partly scarce through the discipline's own guilt-generation.
% ABSENT_VOICES: Religious-trauma survivors and scrupulosity-harmed former members are outside the interpretive conversation; mental-health professionals whose evidence on scrupulosity rarely enters homiletic training are similarly absent; and the reading's rivals (literalist and supersessionist) are handled within the tradition's internal discourse as errors to be corrected rather than interlocutors to be answered.
% DISAPPEARANCE_RATIONALE: Communities formed by the discipline would lose their moral curriculum and their mechanism for retaining the canon: the raw command would resurface, forcing a choice between revived separation readings, accelerated supersession or canon-revision pressure, and abandonment of the confession-mortification apparatus. Preaching calendars, spiritual-direction practices, and testimony cultures organized around the interior war would dissolve or reorganize around imported therapeutic formats.
% FOUNDING_PROBLEM: How can a community whose canon contains a command of total extermination keep that canon authoritative without licensing ethnic violence or discarding scripture? The allegorical displacement was built to solve this canon-integrity problem: preserve the text while redirecting its force away from ethnic targets.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the second-century pagan critic Celsus attacked the literal extermination command as immoral, and Origen's response (Contra Celsum) allegorizes it — a hostile witness attesting the problem was real and pressing before any beneficiary framed it. Modern secular historiography of Alexandrian exegesis, and recurring public controversy over the conquest texts, independently attest that the difficulty persists; no corroborating source attests that the problem is solved.
narrative_ontology:disappearance_verdict(herem_command_dt7__allegorical_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__allegorical_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__allegorical_displacement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(herem_command_dt7__allegorical_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__allegorical_displacement_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__allegorical_displacement_reading_tests).
:- end_tests(herem_command_dt7__allegorical_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.46: the displacement removes interethnic extraction entirely (no ethnic party pays), but the relocated discipline still transfers attention, self-scrutiny labor, and deference upward through a guilt-absolution cycle whose scarcity the discipline itself manufactures — the war is unwinnable by design, guaranteeing permanent mobilization and permanent dependence on the means of grace the institution administers. That places ε well above the identity-coordination floor (0.08) while far below the ethnic-target readings. Suppression 0.55 (raw, unscaled): exit requires abandoning the community and the salvation-framework it mediates; alternatives within the frame — self-acceptance, negotiated truce with desire — are condemned as backsliding. Theater 0.40: testimony cultures and public struggle-narratives ritualize parts of the practice, but the core work occurs in private conscience and is substantively performed. Accessibility_collapse 0.65: within the devotional frame alternatives collapse almost completely once the discipline is understood; outside the frame they persist, keeping the value below natural-law range. Resistance 0.42: quietist critiques, antinomian episodes, Enlightenment ridicule of 'monkish' rigor, and modern therapeutic pushback meet the discipline continuously without displacing it. The temporal series runs on one shared six-point grid (200/600/1100/1550/1850/2026) across all three tracked metrics; the suppression_requirement series is authored because enforcement capacity is a traced dynamic here — it built up through the medieval penitential system (mandatory annual confession), peaked in the confessional-state era (~1550), and partially decayed under disestablishment and voluntary affiliation, while extraction peaked slightly earlier and has partially receded under pastoral softening.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the clerical seat the discipline is a medicine it compounds and dispenses — genuine formation, canon preserved, no victims visible. From the typical believer's seat the same structure is a livable trade: costly but affirmed. From the scrupulous believer's seat it is a machine with no off switch. From the survivor's seat (outside, post-exit) it is the source of lasting harm. The engine derives these per-seat classifications from the structural data — power, exit options, and position — not from the tradition's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for believing_community and clerical_interpreter_class; the clergy sit nearest the beneficiary pole because they run the mechanism and hold arbitrage-grade exit (the method travels with them across institutions and eras). Practicing_believers derive high-mid directionality: they bear the transfer but receive formation in return, tempering the target-side amplification. Scrupulosity_prone_devotees derive near-full-target directionality: trapped exit, concentrated harm, no offsetting collection. The abstract_vices entry is authored with agent=false and is excluded from the directionality computation — its presence in the victim set documents the displacement of the victim slot onto personifications, not a paying party. No directionality overrides were needed: the derivation from beneficiary/victim data plus exit options captures each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how a community keeps a canon containing an extermination command authoritative without licensing ethnic violence or discarding scripture — is live, not dead: every generation re-encounters the text's difficulty, and external witnesses (ancient pagan critics, modern secular controversy) keep attesting it. Live founding problem blocks the piton pathway: the apparatus is maintained because it still performs its canon-integrity and formation functions, not by inertia alone. The guilt-absolution cycle additionally regenerates demand continuously, preventing mandate-atrophy even where doctrinal intensity fades. Mislabeling risks run both ways: taking the tradition's self-description at face value would land this as pure coordination; reading only its enforcement machinery would land it as pure extraction. The structural data — genuine formation function plus asymmetric interior costs plus concentrated harm on a trapped subgroup plus concentrated receipt — supports the hybrid classification authored here. Counterfactually, if the canon were abandoned or the literal sense definitively repudiated, the testimony-and-confession apparatus would likely persist theatrically for a period before dissolving — the observable piton trajectory — but that is not the current state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of kernel herem_command_dt7 (the allegorical_displacement_reading). What structural deltas would the sibling readings introduce if adopted?',
    'Comparative classification across the three reading-stories linked in network.affects_constraints; convergence or divergence of computed types locates where the disagreement carries structural weight.',
    'durable_separation_reading restores ethnic-outsider victims and high interethnic extraction; contextual_supersession_reading retires the command''s present force and leaves at most vestigial discipline. This file''s epsilon (~0.46, intra-psychic referent) is valid only under displacement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer-frame routing: kernel membership, sibling deltas, and the location of the contest (the referent of ''nations'' and the command''s present normative force).').

omega_variable(
    referent_displacement_completeness,
    'Does the allegorical displacement fully delete the ethnic referent of ''nations'', or does the literal sense remain latent beneath the typological overlay, available for reactivation?',
    'Historical-semantic analysis of the tradition''s own layering practices (fourfold-sense precedents) and of episodes where literalist pressure resurfaced; test whether displacement ever operated without a retained literal substrate.',
    'If the ethnic sense is fully deleted, interethnic extraction is zero as the reading claims; if latent, the constraint carries a dormant interethnic vector that reactivates under literalist revival, raising effective extraction and creating causal linkage to durable_separation_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(referent_displacement_completeness, conceptual, 'Whether the displacement of the ethnic referent is complete or layered over a latent literal sense.').

omega_variable(
    scrupulosity_intrinsic_vs_excess,
    'Is the scrupulosity burden an intrinsic product of the discipline''s design (a war with no terminal victory, mercy toward sin forbidden) or an excess pathology that proper pastoral moderation eliminates?',
    'Longitudinal outcome studies comparing communities with differing pastoral-moderation regimes, controlling for selection effects; dose-response analysis of rigor against scrupulosity incidence.',
    'If intrinsic, extraction is structural and epsilon holds near the authored value; if excess, epsilon falls toward the identity-coordination floor and the tangled structure softens toward coordination plus negligent implementation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scrupulosity_intrinsic_vs_excess, empirical, 'Attribution of the discipline''s psychological harm: design feature or implementation failure.').

omega_variable(
    mortification_efficacy,
    'Does relentless mortification of sin actually produce the humility and holiness it promises, or does it predominantly produce compulsive guilt and brittleness?',
    'Prospective studies of formation outcomes across rigor levels; historical comparison of traditions emphasizing mortification against those emphasizing acceptance.',
    'The reading''s instrumental grounding (axiom: perpetual_mortification_of_sin_obligatory) routes to engine foreclosure assessment if efficacy fails systematically; sustained disconfirmation would undermine this reading''s warrant and shift weight toward the supersession sibling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mortification_efficacy, empirical, 'Empirical fate of the instrumental axiom underpinning this reading.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of exit and of alternative ethics structural (communal sanction, salvation stakes) or internalized (guilt architecture that persists after leaving)?',
    'Post-exit trajectory studies of former members: if guilt-vigilance patterns persist after removal from the enforcing community, the internalized share is substantial.',
    'Internalized suppression raises effective suppression above the structural measure, explains why exit remains rare even where external barriers have fallen, and predicts persistent demand for the discipline in detached therapeutic forms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism split for suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__allegorical_displacement_reading, 200, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t200, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 200, 0.18).
narrative_ontology:measurement_basis(here_tr_t200, observed).
narrative_ontology:measurement(here_tr_t600, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 600, 0.22).
narrative_ontology:measurement_basis(here_tr_t600, observed).
narrative_ontology:measurement(here_tr_t1100, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1100, 0.28).
narrative_ontology:measurement_basis(here_tr_t1100, observed).
narrative_ontology:measurement(here_tr_t1550, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1550, 0.33).
narrative_ontology:measurement_basis(here_tr_t1550, observed).
narrative_ontology:measurement(here_tr_t1850, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1850, 0.36).
narrative_ontology:measurement_basis(here_tr_t1850, observed).
narrative_ontology:measurement(here_tr_t2026, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 2026, 0.4).
narrative_ontology:measurement_basis(here_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(here_be_t200, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 200, 0.38).
narrative_ontology:measurement_basis(here_be_t200, observed).
narrative_ontology:measurement(here_be_t600, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 600, 0.44).
narrative_ontology:measurement_basis(here_be_t600, observed).
narrative_ontology:measurement(here_be_t1100, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1100, 0.55).
narrative_ontology:measurement_basis(here_be_t1100, observed).
narrative_ontology:measurement(here_be_t1550, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1550, 0.58).
narrative_ontology:measurement_basis(here_be_t1550, observed).
narrative_ontology:measurement(here_be_t1850, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1850, 0.52).
narrative_ontology:measurement_basis(here_be_t1850, observed).
narrative_ontology:measurement(here_be_t2026, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 2026, 0.46).
narrative_ontology:measurement_basis(here_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t200, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 200, 0.32).
narrative_ontology:measurement_basis(here_su_t200, observed).
narrative_ontology:measurement(here_su_t600, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 600, 0.44).
narrative_ontology:measurement_basis(here_su_t600, observed).
narrative_ontology:measurement(here_su_t1100, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 1100, 0.66).
narrative_ontology:measurement_basis(here_su_t1100, observed).
narrative_ontology:measurement(here_su_t1550, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 1550, 0.7).
narrative_ontology:measurement_basis(here_su_t1550, observed).
narrative_ontology:measurement(here_su_t1850, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 1850, 0.6).
narrative_ontology:measurement_basis(here_su_t1850, observed).
narrative_ontology:measurement(here_su_t2026, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 2026, 0.55).
narrative_ontology:measurement_basis(here_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__allegorical_displacement_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, contextual_supersession_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the herem command' covers three structurally distinct claims and decomposes into three reading-stories per the ε-invariance principle — one kernel (herem_command_dt7), three constraints. This member authors ε ≈ 0.46 over the internal-warfare discipline, with the victim set collapsed to personified vices plus the practitioners' interior autonomy; durable_separation_reading authors high interethnic ε with ethnic-outsider victims; contextual_supersession_reading authors a retired-law profile with minimal present-tense extraction. The allegorical reading historically influenced both siblings: by keeping the text canonical and preachable it preserved the resource on which the other readings operate, while denying their ethnic referents. Each family member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
