% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__continuationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__continuationist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: divine_marriage_command__continuationist_reading
 *   human_readable: Continuationist Reading: Plural Marriage as Undissolved Divine Command
 *   domain: religious/political theology/family structure
 *
 * SUMMARY:
 *   This story instantiates the continuationist reading of the divine
 *   marriage command kernel: the claim held by fundamentalist Mormon splinter
 *   groups that the 1890 Manifesto suspending plural marriage was a
 *   prudential, duress-driven accommodation to federal prosecution, not a
 *   doctrinal rescission of the original revelation. Under this reading, the
 *   command to practice plural marriage remains theologically binding and
 *   unbroken; the mainline church's subsequent institutional path is a
 *   departure from continuity, not its fulfillment. This is a distinct
 *   constraint from the substitutionist reading (monogamy as superseding new
 *   revelation) and the coercion-visibility reading (Manifesto as
 *   acknowledged coercion-response that itself grounds legitimacy) — each
 *   reading produces a different beneficiary/victim structure and a different
 *   ε, and each is authored as its own story per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - fundamentalist_patriarchs: agenda_setter/beneficiary (institutional/arbitrage) — administer the doctrine and its material benefits
 *   - plural_wives_in_splinter_communities: payer (powerless/trapped) — bear the costs of enforced continuity
 *   - children_of_splinter_marriages: payer (powerless/trapped) — inherit legal and social ambiguity
 *   - excommunicated_dissenters: excluded (powerless/trapped) — silenced objectors, expelled to preserve consensus
 *   - mainline_church_leadership: observer/excluded (institutional/analytical) — disclaims the reading but cannot suppress it
 *   - state_and_federal_authorities: agenda_setter (institutional/analytical) — external enforcement that paradoxically feeds the doctrine's persecution narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, 0.58).
domain_priors:suppression_score(divine_marriage_command__continuationist_reading, 0.71).
domain_priors:theater_ratio(divine_marriage_command__continuationist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__continuationist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__continuationist_reading, "Continuationist Reading: Plural Marriage as Undissolved Divine Command").
narrative_ontology:topic_domain(divine_marriage_command__continuationist_reading, "religious/political theology/family structure").

domain_priors:requires_active_enforcement(divine_marriage_command__continuationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__continuationist_reading, '0b2d77c9-2f36-4e31-847b-2d85ffc0593b').
narrative_ontology:cs_kernel_codification('0b2d77c9-2f36-4e31-847b-2d85ffc0593b', fixed_text).
narrative_ontology:cs_authority_grounding('0b2d77c9-2f36-4e31-847b-2d85ffc0593b', lineage).
narrative_ontology:cs_interpretation_layer_present('0b2d77c9-2f36-4e31-847b-2d85ffc0593b').
narrative_ontology:cs_reading_relation('0b2d77c9-2f36-4e31-847b-2d85ffc0593b', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_reading_relation('0b2d77c9-2f36-4e31-847b-2d85ffc0593b', divine_marriage_command__coercion_visibility_reading, influences).
narrative_ontology:cs_axiom('0b2d77c9-2f36-4e31-847b-2d85ffc0593b', foundational, plural_marriage_revelation_undissolved).
narrative_ontology:cs_axiom_status(plural_marriage_revelation_undissolved, holdable).
narrative_ontology:cs_axiom_grounding('0b2d77c9-2f36-4e31-847b-2d85ffc0593b', plural_marriage_revelation_undissolved, theological).
narrative_ontology:cs_axiom('0b2d77c9-2f36-4e31-847b-2d85ffc0593b', foundational, manifesto_is_prudential_not_revelatory).
narrative_ontology:cs_axiom_status(manifesto_is_prudential_not_revelatory, holdable).
narrative_ontology:cs_axiom_grounding('0b2d77c9-2f36-4e31-847b-2d85ffc0593b', manifesto_is_prudential_not_revelatory, theological).
narrative_ontology:cs_axiom('0b2d77c9-2f36-4e31-847b-2d85ffc0593b', secondary, civil_law_external_to_doctrinal_authority).
narrative_ontology:cs_axiom_status(civil_law_external_to_doctrinal_authority, holdable).
narrative_ontology:cs_axiom_grounding('0b2d77c9-2f36-4e31-847b-2d85ffc0593b', civil_law_external_to_doctrinal_authority, conventional).
narrative_ontology:cs_reference_frame('0b2d77c9-2f36-4e31-847b-2d85ffc0593b', pre_manifesto_original_revelation).
narrative_ontology:cs_drift_state('0b2d77c9-2f36-4e31-847b-2d85ffc0593b', contemporary_post_raids_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('0b2d77c9-2f36-4e31-847b-2d85ffc0593b', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__continuationist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, fundamentalist_patriarchs).
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, splinter_group_leadership).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, plural_wives_in_splinter_communities).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, children_of_splinter_marriages).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, excommunicated_dissenters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lead splinter communities that broke from the mainline church at or after 1890, holding that the original revelation on plural marriage was never rescinded, only suspended under coercion. They administer marriage sealings, control communal property and social standing, and enforce continuity doctrine through excommunication threats against dissenters. They personally accumulate wives, status, and control over communal assets under the doctrine they administer.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, fundamentalist_patriarchs, agenda_setter,
    institutional, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__continuationist_reading, fundamentalist_patriarchs, beneficiary).

% Enter or are placed into plural marriages as young women, often with limited education and no independent economic base outside the community. Leaving means losing family, children, community standing, and often literacy in navigating outside institutions. The doctrine that legitimizes their marriage is the same doctrine that forecloses their exit.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, plural_wives_in_splinter_communities, payer,
    powerless, biographical, trapped, local).

% Born into households whose legal status is ambiguous or fraudulent under civil law (only one marriage is legally recognized), leaving inheritance, custody, and welfare-eligibility questions unresolved. Their belonging in the community depends on their parents' continued compliance with continuationist doctrine.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, children_of_splinter_marriages, payer,
    powerless, biographical, trapped, local).

% Former community members, often young men expelled to reduce competition for wives ('lost boys') or women who challenged plural marriage, cast out with no support network. Their objection to the continuationist reading is structurally excluded from communal discourse — expulsion is the mechanism by which the reading maintains internal consensus.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, excommunicated_dissenters, excluded,
    powerless, biographical, trapped, local).

% Holds the substitutionist reading (monogamy as new revelation) and treats the continuationist reading as apostate doctrine. Excommunicates splinter adherents and disclaims any relationship to their practices, but has no direct enforcement power over splinter communities' internal doctrine — only over its own institutional boundary.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, mainline_church_leadership, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__continuationist_reading, mainline_church_leadership, excluded).

% Enforce bigamy and child-welfare statutes against splinter communities, periodically raiding compounds and prosecuting leaders. Their enforcement is what forces plural marriage underground, which in turn is cited by continuationist leaders as evidence that the community is living under the same 'duress' the Manifesto originally responded to — the state's coercion becomes internal proof of doctrinal continuity.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, state_and_federal_authorities, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides fundamentalist splinter communities with a coherent theological identity and continuity claim: it resolves the question of which communal authority is the legitimate heir to the founding revelation, allowing members to organize family structure, inheritance, and religious obligation around a stable interpretive framework rather than the mainline church's revised one.
% TRANSFER_FUNCTION: Moves marital choice, economic security, education access, and exit capacity away from young women and excluded men and toward patriarchal leadership, who accumulate wives, communal authority, and control over property justified by continuity with the undissolved command.
% ABSENT_VOICES: Plural wives who entered as minors or under family pressure rarely have a forum to contest the doctrine from within; excommunicated dissenters who might attest to the coercive mechanics of leadership are physically and socially removed from the community before they can be heard by remaining members.
% DISAPPEARANCE_RATIONALE: If the continuationist reading were abandoned by its adherents, splinter communities would lose their primary claim to legitimate succession from the founding revelation; plural marriages would need to be reframed as either historical practice or independent choice rather than divine command, communal property arrangements built on patriarchal marriage would need restructuring, and the excommunication mechanism that currently enforces compliance would lose its doctrinal justification.
% FOUNDING_PROBLEM: The original 19th-century revelation on plural marriage was presented as solving a problem of restoring ancient patriarchal order and providing for the anticipated demographic imbalance and economic support structure of a persecuted, isolated religious community.
% FOUNDING_PROBLEM_CORROBORATION: Splinter leadership and their adherents attest the founding problem (restoring divine patriarchal order) remains live and unaddressed by the mainline church's 1890 suspension. Independent historians of Mormon fundamentalism, former plural wives now outside these communities, and law enforcement records from state prosecutions attest from outside the benefiting leadership that the doctrine's primary function today is consolidating young wives and communal resources under a small number of aging patriarchs — a function distinct from, and in excess of, the originally stated founding problem.
narrative_ontology:disappearance_verdict(divine_marriage_command__continuationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__continuationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__continuationist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_marriage_command__continuationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__continuationist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__continuationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__continuationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) and rising over the interval: the doctrine's material costs to plural wives and excluded men have not diminished as splinter communities consolidated wealth and marriage arrangements around fewer, more powerful patriarchs (as seen in documented 20th/21st century splinter group consolidation). Suppression is high (0.71) and driven by two overlapping mechanisms: internal excommunication threats against dissent, and external state prosecution that paradoxically reinforces internal cohesion by supplying the 'duress' narrative the continuationist reading depends on. Theater ratio is moderate (0.42) — genuine communal coordination (mutual aid, agricultural cooperation, large extended kin networks) coexists with increasingly performative religious justification for patriarchal accumulation. The suppression_requirement series shows a dip around 1980 reflecting a period of reduced federal enforcement pressure before renewed prosecutions in the 2000s drove it back up — enforcement intensity is not monotonic in this domain.
 *
 * PERSPECTIVAL GAP:
 *   From the patriarchal agenda-setter seat, the arrangement reads as coordination: preserving continuity with authentic revelation against an accommodationist institutional drift. From the payer seats — plural wives and their children — the same structure computes as extraction: their marital and economic futures are decided by doctrine administered by those who benefit from it, with no meaningful voice in whether the doctrine holds. The engine's per-seat computation should register this divergence without either seat's framing controlling the other's.
 *
 * DIRECTIONALITY LOGIC:
 *   Fundamentalist patriarchs are the clear structural beneficiary: they administer marriage sealings, accumulate wives and communal capital, and hold arbitrage-grade exit (they can leave or relocate communities with resources; their followers largely cannot). Plural wives and children are targets: trapped exit options, no independent economic base, and the doctrine that grants their marriages legitimacy is the same doctrine foreclosing their exit. Excommunicated dissenters are targets of the suppression mechanism directly — their expulsion IS the enforcement action, structurally distinct from ordinary payers. State and federal authorities are agenda-setters in a different register: their enforcement doesn't benefit from the doctrine but does shape its content by supplying the coercion narrative that continuationist leaders cite as ongoing proof of doctrinal necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (restoring ancient patriarchal marital order for a persecuted 19th-century community) is contested as either still-live (per patriarchs) or dead-but-persisting (per outside historians and former members). The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges is itself diagnostic: communities organized around this doctrine would materially restructure if it vanished, which argues against treating the doctrine as mere vestigial theater and for treating it as an active tangled-rope structure — genuine communal coordination bundled with concentrated extraction that requires continuous enforcement (excommunication, patriarchal control of sealings) to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_authenticity_vs_institutional_convenience,
    'Is the continuationist reading a sincere theological claim about an undissolved divine command, or a constructed doctrine that happens to concentrate marital and economic power in patriarchal leadership?',
    'Comparative analysis of doctrinal statements before and after leadership succession events within splinter communities: if doctrinal emphasis on plural marriage''s continuity shifts systematically to favor whichever leader currently holds power, that supports a constructed-convenience reading over a sincere-continuity reading.',
    'If constructed, the coordination story (preserving authentic revelation) is cover for concentrated extraction, supporting a snare-leaning classification for the patriarchal leadership seat specifically. If sincere, the tangled_rope classification (genuine coordination + real cost asymmetry) is the more accurate description.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_authenticity_vs_institutional_convenience, conceptual, 'Whether the continuationist doctrine is sincere theology or extraction-serving convenience.').

omega_variable(
    kernel_reading_incommensurability,
    'Can the continuationist, substitutionist, and coercion-visibility readings be evaluated on a shared evidentiary basis, or are they incommensurable theological frameworks that cannot be adjudicated by outside evidence?',
    'Examine whether any of the three readings has historically been abandoned or revised by its own adherents in response to new documentary evidence (e.g., newly surfaced 19th-century correspondence); a reading that updates in response to evidence is empirically contestable, one that does not is a closed theological commitment.',
    'If empirically contestable, the classification of this reading as tangled_rope vs snare could shift with new historical evidence. If closed, the classification is stable regardless of new documentary discovery and rests entirely on structural/behavioral data (as authored here) rather than doctrinal truth-value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether kernel readings can be adjudicated by shared evidence or are theologically closed.').

omega_variable(
    external_coercion_internal_doctrine_boundary,
    'Where does state enforcement pressure end and internally-generated doctrinal enforcement begin, given that continuationist leaders cite ongoing state prosecution as evidence supporting their duress narrative?',
    'Track periods of reduced federal enforcement (e.g., the 1980s lull reflected in the suppression_requirement dip) against internal excommunication rates: if internal enforcement remains high even when external pressure drops, that indicates the suppression is substantially internally generated rather than merely reactive to state coercion.',
    'If internal enforcement persists independent of external pressure, the suppression metric should be read as substantially self-sustaining (supporting tangled_rope/snare readings of the leadership seat), not merely a defensive response to state action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_coercion_internal_doctrine_boundary, empirical, 'Whether communal suppression is driven by external state coercion or self-sustaining internal enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__continuationist_reading, 1890, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__continuationist_reading, theater_ratio, 1890, 0.2).
narrative_ontology:measurement(divi_tr_t1920, divine_marriage_command__continuationist_reading, theater_ratio, 1920, 0.25).
narrative_ontology:measurement(divi_tr_t1953, divine_marriage_command__continuationist_reading, theater_ratio, 1953, 0.3).
narrative_ontology:measurement(divi_tr_t1980, divine_marriage_command__continuationist_reading, theater_ratio, 1980, 0.34).
narrative_ontology:measurement(divi_tr_t2008, divine_marriage_command__continuationist_reading, theater_ratio, 2008, 0.39).
narrative_ontology:measurement(divi_tr_t2024, divine_marriage_command__continuationist_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__continuationist_reading, base_extractiveness, 1890, 0.35).
narrative_ontology:measurement(divi_be_t1920, divine_marriage_command__continuationist_reading, base_extractiveness, 1920, 0.4).
narrative_ontology:measurement(divi_be_t1953, divine_marriage_command__continuationist_reading, base_extractiveness, 1953, 0.46).
narrative_ontology:measurement(divi_be_t1980, divine_marriage_command__continuationist_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(divi_be_t2008, divine_marriage_command__continuationist_reading, base_extractiveness, 2008, 0.56).
narrative_ontology:measurement(divi_be_t2024, divine_marriage_command__continuationist_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__continuationist_reading, suppression_requirement, 1890, 0.55).
narrative_ontology:measurement(divi_su_t1920, divine_marriage_command__continuationist_reading, suppression_requirement, 1920, 0.6).
narrative_ontology:measurement(divi_su_t1953, divine_marriage_command__continuationist_reading, suppression_requirement, 1953, 0.68).
narrative_ontology:measurement(divi_su_t1980, divine_marriage_command__continuationist_reading, suppression_requirement, 1980, 0.62).
narrative_ontology:measurement(divi_su_t2008, divine_marriage_command__continuationist_reading, suppression_requirement, 2008, 0.7).
narrative_ontology:measurement(divi_su_t2024, divine_marriage_command__continuationist_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, substitutionist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__continuationist_reading, coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the divine_marriage_command kernel. The substitutionist_reading (mainline church: monogamy as new revelation superseding the original command) is logically foreclosed by this reading's foundational axiom that the original revelation was never rescinded — a single doctrinal framework cannot simultaneously hold that plural marriage remains commanded and that monogamy is now commanded by new revelation. The coercion_visibility_reading (Manifesto as acknowledged coercion-response conferring its own legitimacy) is influenced by, but not foreclosed by, this reading: both readings agree the Manifesto was a response to federal duress, but this reading treats that response as merely prudential and non-binding on doctrine, while the coercion_visibility_reading treats the acknowledged coercion itself as generating new legitimacy. Each reading carries its own ε, its own beneficiary/victim structure, and its own classification; they are not measurement variants of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
