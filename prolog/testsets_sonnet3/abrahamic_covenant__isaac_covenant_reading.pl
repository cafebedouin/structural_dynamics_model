% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__isaac_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: abrahamic_covenant__isaac_covenant_reading
 *   human_readable: Isaac-Exclusive Reading of the Abrahamic Covenant
 *   domain: religious/institutional
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested Abrahamic covenant
 *   kernel: the interpretation of Genesis 17:19-21 as transmitting covenantal
 *   legitimacy exclusively through Isaac, with Ishmael's line explicitly and
 *   permanently excluded from covenantal (though not from all divine blessing
 *   — the text does bless Ishmael separately). This reading has been
 *   institutionally load-bearing for Jewish theological self-understanding
 *   across the diaspora and remains authoritative within rabbinic tradition.
 *   It is generated as a clean, ε-invariant constraint distinct from the
 *   sibling readings (the Ishmael-inclusive reading culminating in Islamic
 *   prophetic succession, and the land-promise reading concerning territorial
 *   grant) — those are separate constraints with their own ε, stakeholders,
 *   and classification, linked via network.affects_constraints, not folded
 *   into this file's arithmetic.
 *
 * KEY AGENTS:
 *   - rabbinic_jewish_institutional_authority: agenda-setter, administers and transmits the exclusive reading
 *   - isaacite_lineage_claimants: beneficiary, derives identity and legal standing from the reading
 *   - ishmaelite_claimants: payer, textually excluded with no interpretive voice
 *   - islamic_prophetic_tradition: payer at the level of THIS reading's genealogical claim, though institutionally powerful in its own right
 *   - comparative_religion_scholars: analytical observer of reception history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, 0.62).
domain_priors:suppression_score(abrahamic_covenant__isaac_covenant_reading, 0.58).
domain_priors:theater_ratio(abrahamic_covenant__isaac_covenant_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__isaac_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__isaac_covenant_reading, "Isaac-Exclusive Reading of the Abrahamic Covenant").
narrative_ontology:topic_domain(abrahamic_covenant__isaac_covenant_reading, "religious/institutional").

domain_priors:requires_active_enforcement(abrahamic_covenant__isaac_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__isaac_covenant_reading, 'f877b80b-d474-4715-a351-d5ba581f876f').
narrative_ontology:cs_kernel_codification('f877b80b-d474-4715-a351-d5ba581f876f', fixed_text).
narrative_ontology:cs_authority_grounding('f877b80b-d474-4715-a351-d5ba581f876f', lineage).
narrative_ontology:cs_interpretation_layer_present('f877b80b-d474-4715-a351-d5ba581f876f').
narrative_ontology:cs_reading_relation('f877b80b-d474-4715-a351-d5ba581f876f', abrahamic_covenant__ishmael_covenant_reading, forecloses).
narrative_ontology:cs_reading_relation('f877b80b-d474-4715-a351-d5ba581f876f', abrahamic_covenant__christian_supersessionist_reading, influences).
narrative_ontology:cs_axiom('f877b80b-d474-4715-a351-d5ba581f876f', foundational, covenantal_transmission_is_singular_and_exclusive).
narrative_ontology:cs_axiom_status(covenantal_transmission_is_singular_and_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('f877b80b-d474-4715-a351-d5ba581f876f', covenantal_transmission_is_singular_and_exclusive, conventional).
narrative_ontology:cs_axiom('f877b80b-d474-4715-a351-d5ba581f876f', secondary, genesis_17_21_textually_restricts_berit_to_isaac).
narrative_ontology:cs_axiom_status(genesis_17_21_textually_restricts_berit_to_isaac, holdable).
narrative_ontology:cs_axiom_grounding('f877b80b-d474-4715-a351-d5ba581f876f', genesis_17_21_textually_restricts_berit_to_isaac, conventional).
narrative_ontology:cs_reference_frame('f877b80b-d474-4715-a351-d5ba581f876f', sinaitic_rabbinic_transmission).
narrative_ontology:cs_drift_state('f877b80b-d474-4715-a351-d5ba581f876f', post_diaspora_institutional_consolidation, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('f877b80b-d474-4715-a351-d5ba581f876f', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, rabbinic_jewish_institutional_authority).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, isaacite_lineage_claimants).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, ishmaelite_claimants).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, islamic_prophetic_tradition).
narrative_ontology:constraint_vindicates(abrahamic_covenant__isaac_covenant_reading, mosaic_covenantal_singularity).
narrative_ontology:constraint_vindicates(abrahamic_covenant__isaac_covenant_reading, textual_priority_of_genesis_17_21).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and transmits the canonical reading of Genesis 17:19-21 through liturgy, halakhic commentary, and communal identity practice. Determines which readings of the covenant text carry institutional legitimacy within Jewish tradition and trains successive generations of interpreters. Collects continuity, legal standing, and identity coherence from the exclusive reading being maintained as authoritative.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, rabbinic_jewish_institutional_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__isaac_covenant_reading, rabbinic_jewish_institutional_authority, beneficiary).

% Communities and individuals who trace covenantal descent through Isaac and Jacob. Their religious identity, inheritance claims, and communal belonging are affirmed and stabilized by the exclusive reading; they bear little cost from the exclusion of Ishmael's line and derive standing directly from it.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, isaacite_lineage_claimants, beneficiary,
    organized, generational, mobile, global).

% Historically and textually positioned as the excluded line despite receiving an explicit (if lesser) blessing in the same passage (Gen 17:20). Their descendants' claim to covenantal inheritance is foreclosed by the reading; they cannot exit the text or the historical narrative that names them, only contest the interpretation from outside the institutions that control it.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, ishmaelite_claimants, payer,
    powerless, civilizational, trapped, global).

% A later, powerful institutional tradition that regards Ishmael as the covenantal line culminating in Muhammad. Structurally institutional and far from powerless in the present, but with respect to THIS specific reading it bears the cost of exclusion: the Isaac-exclusive interpretation, where held authoritative in interfaith or scholarly contexts, delegitimizes the Islamic tradition's own genealogical claim at its root. It can contest the reading polemically and theologically but cannot alter the Jewish institutional transmission of the text.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, islamic_prophetic_tradition, payer,
    institutional, civilizational, constrained, global).

% Christian tradition typically defers to the Isaac line for the literal genealogy while relocating ultimate covenantal fulfillment onto a spiritualized reading (see the sibling supersessionist constraint). Christian voices are not parties to the specific Isaac/Ishmael dispute in the same way and are largely absent from this particular contest, though their supersessionist reading depends on this reading's outcome.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, christian_readers, excluded,
    institutional, civilizational, analytical, global).

% Study the textual history, redaction layers, and reception history of Genesis 17 across Jewish, Christian, and Islamic traditions without institutional stake in any single reading's authority. Document how the exclusive reading functions historically to consolidate boundary and identity.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__isaac_covenant_reading, rabbinic_jewish_institutional_authority).
narrative_ontology:fixing_cost_class(abrahamic_covenant__isaac_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides Jewish communities a stable, textually-grounded genealogical and legal basis for covenantal identity, inheritance law, and communal boundary-setting across millennia of dispersion — a coordination problem of maintaining coherent peoplehood without centralized political sovereignty for most of that history.
% TRANSFER_FUNCTION: Moves covenantal legitimacy, land-promise standing, and chosen-people identity status exclusively onto the Isaac line, while explicitly and permanently withholding equivalent status from the Ishmael line, despite the same textual unit blessing both.
% ABSENT_VOICES: Ishmaelite claimants as a textual category have no institutional voice in how rabbinic tradition interprets their own exclusion — they appear only as the object of the verse, not as parties to its interpretation. Islamic tradition developed its own independent reading much later and was never in dialogue with the rabbinic interpretive process that fixed this reading.
% DISAPPEARANCE_RATIONALE: If this exclusive reading vanished and were replaced by an inclusive or symmetrical reading, Jewish covenantal exceptionalism as historically constructed would lose a key textual anchor; interfaith polemics around 'true' Abrahamic descent would be substantially reconfigured; and long-standing theological arguments used to contest Islamic claims to Abrahamic legitimacy would need new textual grounding.
% FOUNDING_PROBLEM: Ancient Israelite communities needed a textual warrant to explain why covenantal promise, land, and chosen status attached to their specific lineage and not to neighboring peoples who also claimed Abrahamic descent (notably Ishmaelite/Arab tribal groups).
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic tradition itself attests the problem as originally live (distinguishing Israel from surrounding peoples) and treats it as permanently settled by the text. Outside the benefiting tradition, comparative religion scholars and Islamic theologians attest that the exclusion functions as retrospective boundary-construction rather than a live theological necessity — the same verse's blessing of Ishmael (Gen 17:20) is read by outside observers as evidence the text itself is more ambiguous than the exclusive reading claims.
narrative_ontology:disappearance_verdict(abrahamic_covenant__isaac_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__isaac_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__isaac_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(abrahamic_covenant__isaac_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__isaac_covenant_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__isaac_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__isaac_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 (substantial but not maximal) because the reading's cost falls on a genealogical/theological claim rather than on material resources directly — the harm is standing-denial and identity-foreclosure, which is real but less immediately coercive than, say, land dispossession (handled in the sibling land_promise_constraint). Suppression is authored at 0.58: the reading is maintained by institutional transmission (liturgy, commentary, communal teaching) rather than by physical coercion, but it does foreclose alternative readings within the tradition that maintains it. Accessibility collapse is high (0.7) because, once the reading is institutionally fixed within rabbinic tradition, alternative readings of the SAME text within that tradition become very difficult to sustain — the text is treated as settled. Resistance is moderate (0.55): Islamic tradition mounted a full alternative theological system rather than contesting this reading from within, so resistance manifests as parallel tradition-building rather than internal contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic institutional seat, this reading is settled scripture performing a genuine coordination function (peoplehood continuity). From the Ishmaelite-claimant seat (as textually constructed) and from the Islamic-tradition seat, the same textual operation reads as an act of exclusionary boundary-drawing that forecloses a co-equal claim to the same patriarch. The engine computes these as different seat classifications from the same structural data; neither seat's self-description is privileged as the story's ground truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic institutional authority and Isaacite lineage claimants sit near the beneficiary end: the reading is the textual ground of their covenantal standing and communal coherence, low cost, low d. Ishmaelite claimants sit near the full-target end: trapped by an ancient text they cannot re-author, with no institutional lever to contest the reading from within the tradition that fixed it. Islamic prophetic tradition is institutionally powerful in the present but, with respect to THIS SPECIFIC constraint (whether the Isaac-exclusive reading is granted interpretive authority), it is a payer — the reading directly negates its own genealogical self-understanding. This is why islamic_prophetic_tradition is declared institutional power but payer role: power and directionality are not the same axis.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing Israelite covenantal identity from neighboring Abrahamic-descent claimants) is contested rather than resolved as dead: rabbinic tradition treats it as permanently settled scripture, not as a live administrative solution to an ongoing problem, which is one signal of drift from live coordination toward maintained boundary. Classifying this as tangled_rope rather than pure snare or pure mountain avoids two mislabeling errors: treating it as mountain would ignore the identifiable beneficiary structure (institutional Jewish continuity) and requires an omega (below) on naturalness; treating it as pure snare would ignore the genuine coordination function it serves for Isaacite-descended communities in maintaining coherent identity absent political sovereignty for most of their history.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genesis_17_20_blessing_scope,
    'Does Genesis 17:20''s explicit blessing of Ishmael (''I will make him a great nation'') constitute a partial covenantal inclusion that the exclusive reading understates, or is it categorically distinct from covenantal transmission proper (v.19, 21)?',
    'Close philological and redaction-critical analysis of the Hebrew text''s distinction between berit (covenant) language applied to Isaac versus barak (blessing) language applied to Ishmael; comparison with ANE covenant-treaty formulae.',
    'If the blessing/covenant distinction is textually robust, the exclusive reading has stronger internal textual warrant. If the distinction is a later interpretive imposition, the exclusive reading is more clearly a constructed boundary rather than a discovered one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genesis_17_20_blessing_scope, empirical, 'Whether the text itself supports a hard covenant/blessing distinction between the two sons.').

omega_variable(
    kernel_reading_selection_pressure,
    'Is the isaac_covenant_reading the historically prior reading that the ishmael_covenant_reading later contested, or did both readings emerge from genuinely ambiguous source material with no clear interpretive priority?',
    'Comparative dating of earliest attestations of each reading in rabbinic, and later Islamic, sources; examination of whether pre-Islamic rabbinic sources already treat the exclusion as contested or settled.',
    'If isaac_covenant_reading has clear historical priority, ishmael_covenant_reading is better modeled as a later counter-reading responding to an established boundary; if both are live from early on, neither reading has a defensible claim to being the ''original'' or ''default'' interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Historical priority question between the two genealogical readings of the kernel.').

omega_variable(
    naturalness_of_identity_boundary,
    'Is the exclusive covenantal boundary a discovered theological fact (this is simply what the text and tradition establish) or a constructed boundary serving identifiable institutional continuity interests?',
    'Cross-tradition comparison of how covenantal-boundary claims function institutionally across multiple unrelated religious traditions with similar founder-genealogy disputes; assessment of whether the boundary correlates with periods of institutional consolidation.',
    'If constructed, the tangled_rope classification with named institutional beneficiary is directly supported. If genuinely discovered/settled by the text with no institutional benefit contingent on the reading, a mountain-adjacent classification would be more defensible — though the declared beneficiaries here already route this toward FSM-relevant scrutiny rather than pure mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_identity_boundary, conceptual, 'Whether the covenantal boundary is discovered or constructed — bears on false-summit risk even though this story is not claimed as mountain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__isaac_covenant_reading, 0, 3000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t0, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(abra_tr_t600, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 600, 0.22).
narrative_ontology:measurement(abra_tr_t1200, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1200, 0.25).
narrative_ontology:measurement(abra_tr_t1800, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1800, 0.27).
narrative_ontology:measurement(abra_tr_t2400, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 2400, 0.29).
narrative_ontology:measurement(abra_tr_t3000, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 3000, 0.3).

% Extraction over time
narrative_ontology:measurement(abra_be_t0, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(abra_be_t600, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 600, 0.58).
narrative_ontology:measurement(abra_be_t1200, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1200, 0.6).
narrative_ontology:measurement(abra_be_t1800, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1800, 0.6).
narrative_ontology:measurement(abra_be_t2400, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 2400, 0.61).
narrative_ontology:measurement(abra_be_t3000, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 3000, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t0, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(abra_su_t600, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 600, 0.52).
narrative_ontology:measurement(abra_su_t1200, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1200, 0.53).
narrative_ontology:measurement(abra_su_t1800, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1800, 0.55).
narrative_ontology:measurement(abra_su_t2400, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 2400, 0.57).
narrative_ontology:measurement(abra_su_t3000, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 3000, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__isaac_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(abrahamic_covenant__isaac_covenant_reading, 0.08).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, ishmael_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, land_promise_constraint).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, christian_supersessionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of (at least) three sibling readings of the abrahamic_covenant kernel. isaac_covenant_reading and ishmael_covenant_reading share the same textual kernel (Genesis 17) but produce opposed genealogical verdicts and different victim sets — their epsilon values are authored independently per the ε-invariance principle and must not be averaged or reconciled. land_promise_constraint operates on a distinct axis (territorial grant) and can combine with either genealogical reading; christian_supersessionist_reading depends structurally on the isaac line being genealogically settled before relocating fulfillment onto a further spiritualized reading, hence the influences-shaped downstream relationship recorded in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
