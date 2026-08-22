% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__allegorical_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Herem as Typological Warfare Against Vice (Allegorical-Displacement Reading of Deuteronomy 7)
 *   domain: religious/ethical/hermeneutical
 *
 * SUMMARY:
 *   This story authors the allegorical-displacement reading of the herem
 *   command in Deuteronomy 7: the 'nations' Israel is commanded to destroy
 *   without mercy are read as typological figures for internal vices — pride,
 *   idolatry, lust, doubt — and the conquest campaign becomes a metaphor for
 *   the disciplined believer's warfare against sin. Under this reading the
 *   constraint's extraction on actual ethnic or interethnic relations
 *   collapses to near zero: there is no real population being targeted,
 *   because the reading relocates the referent entirely into the
 *   moral-psychological domain of the individual or community's inner life.
 *   The 'victims' of herem, under this reading, are abstractions (vice,
 *   temptation) that cannot be victims in the structural sense the schema
 *   requires, so the victims array is empty. This is a sharply different
 *   structure from the durable-separation reading (which retains real
 *   outsider populations as the referent and treats the boundary as a live
 *   communal mandate) and the contextual-supersession reading (which retains
 *   the historical referent but treats the command as time-bound and morally
 *   superseded). All three are distinct constraints sharing one kernel text.
 *
 * KEY AGENTS:
 *   - practicing_adherents_seeking_moral_formation: primary beneficiary of the reading's devotional utility
 *   - allegorical_interpretive_tradition: agenda-setting lineage that develops and sustains the reading
 *   - ancient_canaanite_populations_as_historical_referents: non-agent historical entity whose erasure-as-referent is the reading's defining move
 *   - historical_critical_scholars: excluded voices who read the text as a genuine historical-ethnic claim
 *   - descendant_communities_and_interfaith_observers: analytical observers tracking cross-historical reuse of the text
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__allegorical_displacement_reading, 0.08).
domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, 0.35).
domain_priors:theater_ratio(herem_command_dt7__allegorical_displacement_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__allegorical_displacement_reading, rope).
narrative_ontology:human_readable(herem_command_dt7__allegorical_displacement_reading, "Herem as Typological Warfare Against Vice (Allegorical-Displacement Reading of Deuteronomy 7)").
narrative_ontology:topic_domain(herem_command_dt7__allegorical_displacement_reading, "religious/ethical/hermeneutical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__allegorical_displacement_reading, 'db11db7c-d458-46a5-a28d-ca2548c0bba3').
narrative_ontology:cs_kernel_codification('db11db7c-d458-46a5-a28d-ca2548c0bba3', fixed_text).
narrative_ontology:cs_authority_grounding('db11db7c-d458-46a5-a28d-ca2548c0bba3', practice).
narrative_ontology:cs_interpretation_layer_present('db11db7c-d458-46a5-a28d-ca2548c0bba3').
narrative_ontology:cs_reading_relation('db11db7c-d458-46a5-a28d-ca2548c0bba3', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('db11db7c-d458-46a5-a28d-ca2548c0bba3', herem_command_dt7__contextual_supersession_reading, coexists_with).
narrative_ontology:cs_axiom('db11db7c-d458-46a5-a28d-ca2548c0bba3', foundational, nations_are_typological_not_ethnic_referents).
narrative_ontology:cs_axiom_status(nations_are_typological_not_ethnic_referents, holdable).
narrative_ontology:cs_axiom_grounding('db11db7c-d458-46a5-a28d-ca2548c0bba3', nations_are_typological_not_ethnic_referents, conventional).
narrative_ontology:cs_axiom('db11db7c-d458-46a5-a28d-ca2548c0bba3', foundational, conquest_language_denotes_internal_moral_struggle).
narrative_ontology:cs_axiom_status(conquest_language_denotes_internal_moral_struggle, holdable).
narrative_ontology:cs_axiom_grounding('db11db7c-d458-46a5-a28d-ca2548c0bba3', conquest_language_denotes_internal_moral_struggle, instrumental).
narrative_ontology:cs_reference_frame('db11db7c-d458-46a5-a28d-ca2548c0bba3', patristic_typological_exegesis).
narrative_ontology:cs_drift_state('db11db7c-d458-46a5-a28d-ca2548c0bba3', contemporary_devotional_use, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('db11db7c-d458-46a5-a28d-ca2548c0bba3', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, practicing_adherents_seeking_moral_formation).
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, allegorical_interpretive_tradition).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, scriptural_moral_coherence_doctrine).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, typological_hermeneutic_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reads the herem texts as a discipline manual for internal struggle against sin, pride, idolatry-as-metaphor. Uses the violent language as a rhetorical intensifier for the seriousness of moral self-examination. Free to adopt or drop this reading without institutional penalty; many traditions offer it as one hermeneutic option among several.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, practicing_adherents_seeking_moral_formation, beneficiary,
    moderate, biographical, mobile, national).

% A lineage of commentators (patristic allegorists, later devotional and homiletic traditions) that develops and transmits the reading that Canaanite nations typify vices to be extirpated from the soul. Sustains its interpretive authority by offering a morally coherent account of otherwise troubling conquest texts; benefits reputationally and doctrinally when the reading holds, since it resolves an apologetic problem without requiring revision of scriptural inerrancy claims.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, allegorical_interpretive_tradition, agenda_setter,
    institutional, civilizational, arbitrage, global).

% The historical peoples named in the text are, under this reading, evacuated of their status as actual referents — their historical particularity is displaced by typological function. They cannot object; the reading does not concern them as an ethnic or historical claim, and this is exactly what the reading asserts. Listed for completeness as a non-agent historical entity whose erasure-as-referent is the reading's defining move, not as a party with standing in this constraint's operation.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, ancient_canaanite_populations_as_historical_referents, excluded,
    powerless, civilizational, trapped, regional).
narrative_ontology:stakeholder_non_agent(herem_command_dt7__allegorical_displacement_reading, ancient_canaanite_populations_as_historical_referents).

% Textual and archaeological scholars who read Deuteronomy 7 as addressed to an actual, historically situated conflict over land and cultic practice among real populations. They would object that the allegorical move dissolves a text's plain historical claims into metaphor to avoid ethical reckoning, but their objection operates in a different discourse (historical-critical exegesis) that this devotional/typological reading does not engage as an authority.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, historical_critical_scholars, excluded,
    organized, generational, analytical, global).

% Jewish, interfaith, and post-colonial commentators who track how conquest narratives get read and reused across history. Note that the allegorical reading, whatever its internal coherence, has historically coexisted with periods when the same text was read literally to license real violence against real populations; they observe the reading's function without being harmed by its present internal-warfare content.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, descendant_communities_and_interfaith_observers, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__allegorical_displacement_reading, diffuse).
narrative_ontology:fixing_cost_class(herem_command_dt7__allegorical_displacement_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a way for a religious community to retain the full canon, including morally difficult conquest narratives, as a coherent source of ethical instruction by relocating the referent of 'nations' from historical peoples to internal vices — solving the coordination problem of sustaining doctrinal continuity without either excising or literally endorsing the violent text.
% TRANSFER_FUNCTION: Moves interpretive authority and apologetic labor: the burden of explaining Deuteronomy 7's violence shifts from historical-ethical justification (why did God command this against real people) to devotional pedagogy (what vice does this figure represent). No material transfer between persons; the transfer is discursive — from a historical claim to a moral-formation claim.
% ABSENT_VOICES: Historical-critical scholars and descendant/interfaith communities who read the text as making genuine historical-ethnic claims are not parties to this reading's internal logic; they would argue the allegorization evacuates the text's actual moral stakes rather than resolving them. Their objection is noted but does not enter as a corrective to this reading's own operation.
% DISAPPEARANCE_RATIONALE: If this specific reading vanished, adherents who use it for devotional self-examination would lose one available frame, but the underlying text and its other readings (durable-separation, contextual-supersession) would remain in circulation; whether 'the world rearranges' depends entirely on which reading a given community was actually relying on to hold the canon together — for communities where this is the load-bearing reconciliation move, its loss would force a doctrinal reckoning; for others it is one option among several and its loss is immaterial.
% FOUNDING_PROBLEM: Early and patristic interpreters faced the problem that the plain historical sense of Deuteronomy 7 (total destruction of named peoples) sat in severe tension with a theology of a universally loving God and with post-conquest Christian universalism; typological/allegorical reading solved this by reassigning the text's referent from history to soul.
% FOUNDING_PROBLEM_CORROBORATION: Patristic sources (e.g., Origen's allegorical exegesis) attest the problem and this solution from within the tradition itself. Outside corroboration comes from historians of exegesis and comparative theologians who document the allegorical move as a documented, recurring apologetic strategy across centuries when a canon's violent content threatens communal self-understanding — this corroborates that the problem was and remains real, without adjudicating whether the allegorical solution is the correct one.
narrative_ontology:disappearance_verdict(herem_command_dt7__allegorical_displacement_reading, contested).
narrative_ontology:founding_problem_status(herem_command_dt7__allegorical_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__allegorical_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(herem_command_dt7__allegorical_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__allegorical_displacement_reading, 0.08, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored near zero (0.08) because, by this reading's own lights, there is no real party from whom anything is extracted — the conquest is entirely internalized as spiritual self-discipline. Suppression is moderate (0.35): the reading does exert real interpretive pressure by foreclosing (for its adherents) the plain historical reading as morally troubling, and by treating the historical-critical alternative as simply a different discourse rather than a live contender within its own community. Theater ratio is elevated (0.4, drifting down slightly over the interval) because a portion of the reading's operation is homiletic performance — dramatic 'spiritual warfare' rhetoric that intensifies the moral stakes of ordinary vice beyond what the underlying pastoral function requires; this has moderated slightly over centuries as devotional rhetoric has become somewhat less martial in mainstream use. Accessibility collapse is low-moderate (0.3): this reading exists alongside durable-separation and contextual-supersession readings within the same broad tradition, so alternatives are not eliminated, only one is offered as the resolving option for troubled readers. Resistance is moderate (0.45): historical-critical scholarship and post-colonial commentary actively contest the allegorical move as evasive.
 *
 * PERSPECTIVAL GAP:
 *   From the interpretive tradition's own seat, this reading is a genuine rope: it solves the coordination problem of canonical coherence without requiring anyone to renounce or literally re-enact violence. From the historical-critical seat (excluded from this constraint's operation but structurally aware of it), the same move looks like theater — an evasion that resolves the ethical problem by definitional fiat rather than confronting the plain sense of the text. The engine computes these divergently from the same structural data; this story does not adjudicate between them, it authors the allegorical seat's structure honestly.
 *
 * DIRECTIONALITY LOGIC:
 *   Adherents and the interpretive tradition are declared beneficiaries: the reading gives adherents a usable devotional framework and gives the tradition an apologetic solution, so directionality sits near the beneficiary end for both. No victim group is declared because, within this reading's own structural claim, there is no real party bearing the cost of 'conquest' — vices are not agents. The historical referents are marked as a non-agent excluded entity specifically to register, without contaminating directionality math, that the reading's defining move is precisely to stop treating a historical population as the referent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling a violent conquest text with a theology of universal love) is authored as still live, which blocks a false 'mandatrophy resolved' framing: this is not a vestigial reading defending a dead function, it is an active, continuously re-deployed solution to a persistent theological tension. Because founding_problem_status is 'live' and disappearance_verdict is 'contested' rather than 'world_rearranges', no capture/zombie mismatch flag should fire — the reading is doing real, ongoing devotional work for the communities that hold it, even though outside observers dispute whether it is the right work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    referent_displacement_or_evasion,
    'Does the allegorical-displacement reading genuinely resolve the ethical difficulty of the herem text, or does it evade the difficulty by definitionally removing the historical referent from consideration?',
    'Compare how the reading''s adherent communities respond when confronted with the historical-critical case that Deuteronomy 7 was composed as, and originally received as, a claim about actual populations — does the community''s allegorical commitment survive engagement with that evidence, or does it depend on non-engagement with it?',
    'If the allegorical reading depends on non-engagement with the historical-critical case, its near-zero extractiveness score is an artifact of scope-restriction (it never allows the historical referent into view) rather than a genuine resolution — this would suggest the reading functions partly as a shield against the sibling readings'' ethical weight rather than as an independent hermeneutic. If the reading survives full engagement with historical-critical evidence and adherents still find it theologically coherent, the near-zero score is more robustly earned.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(referent_displacement_or_evasion, conceptual, 'Whether the allegorical reading''s low extraction score reflects genuine resolution or scope-restriction away from the historical referent.').

omega_variable(
    cross_reading_contamination_risk,
    'Even though this reading itself has near-zero interethnic extraction, does its wide circulation provide interpretive cover that makes the durable_separation_reading''s literal, higher-extraction use of the same text easier to sustain or harder to dislodge in mixed communities that hold multiple readings simultaneously?',
    'Track historical periods and communities where allegorical and literal-separationist readings of herem circulated together; assess whether the allegorical reading''s presence correlated with reduced or unchanged uptake of the literal-separationist reading in the same population.',
    'If allegorical availability reduces literal-separationist uptake, the reading functions as a genuine safety valve; if the two coexist without tension (people hold the allegorical reading devotionally while a different faction holds the literal reading operationally), the readings are more independent than the kernel-sharing implies, and this story''s zero-extraction claim should not be read as evidence bearing on the sibling readings'' actual social effects.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_reading_contamination_risk, empirical, 'Whether this reading''s low-extraction profile has any bearing on the extraction profile of sibling readings sharing the same kernel text in mixed communities.').

omega_variable(
    typological_method_naturalness,
    'Is typological/allegorical reading itself a natural, longstanding hermeneutic method (a Mountain-like feature of the interpretive tradition) or a constructed apologetic move adopted specifically because the plain sense of this text was ethically embarrassing?',
    'Compare the use of typological reading on herem specifically against its use on other Old Testament texts that carry no ethical difficulty (e.g., typological readings of the Exodus or the tabernacle) — if the method is applied evenly regardless of ethical stakes, it looks more like a general interpretive tool; if it is disproportionately invoked for morally difficult texts, it looks more purpose-built for apologetic relief.',
    'This bears on whether the allegorical_interpretive_tradition stakeholder should be read as a neutral custodian of a general method (weaker beneficiary claim) or as an agenda-setter selectively deploying the method where it serves doctrinal coherence (stronger beneficiary claim, closer to the FSM pattern even though this constraint is not claimed as a mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(typological_method_naturalness, conceptual, 'Whether typological reading is applied as a general method or selectively deployed for ethically difficult texts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__allegorical_displacement_reading, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement_basis(here_tr_t0, observed).
narrative_ontology:measurement(here_tr_t300, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 300, 0.46).
narrative_ontology:measurement_basis(here_tr_t300, observed).
narrative_ontology:measurement(here_tr_t700, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 700, 0.44).
narrative_ontology:measurement_basis(here_tr_t700, observed).
narrative_ontology:measurement(here_tr_t1100, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1100, 0.42).
narrative_ontology:measurement_basis(here_tr_t1100, observed).
narrative_ontology:measurement(here_tr_t1500, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1500, 0.41).
narrative_ontology:measurement_basis(here_tr_t1500, observed).
narrative_ontology:measurement(here_tr_t1900, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1900, 0.4).
narrative_ontology:measurement_basis(here_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement_basis(here_be_t0, observed).
narrative_ontology:measurement(here_be_t300, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 300, 0.07).
narrative_ontology:measurement_basis(here_be_t300, observed).
narrative_ontology:measurement(here_be_t700, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 700, 0.07).
narrative_ontology:measurement_basis(here_be_t700, observed).
narrative_ontology:measurement(here_be_t1100, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1100, 0.08).
narrative_ontology:measurement_basis(here_be_t1100, observed).
narrative_ontology:measurement(here_be_t1500, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1500, 0.08).
narrative_ontology:measurement_basis(here_be_t1500, observed).
narrative_ontology:measurement(here_be_t1900, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1900, 0.08).
narrative_ontology:measurement_basis(here_be_t1900, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(herem_command_dt7__allegorical_displacement_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__allegorical_displacement_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__contextual_supersession_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from a single natural-language label ('the herem command of Deuteronomy 7') per the ε-invariance principle. The allegorical_displacement_reading authors near-zero extractiveness because it relocates the constraint's operation entirely into the internal-moral domain. The durable_separation_reading and contextual_supersession_reading retain a real historical/ethnic referent and correspondingly author substantially different ε and victim structures. All three share the kernel_id herem_command_dt7 and are linked bidirectionally via affects_constraints; each carries its own base_properties, stakeholders, and six_questions rather than averaging or hedging across the readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
