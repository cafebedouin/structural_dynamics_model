% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__land_promise_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__land_promise_constraint, []).

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
 *   constraint_id: abrahamic_covenant__land_promise_constraint
 *   human_readable: Territorial Reading of the Abrahamic Land Covenant (Genesis Grant of Canaan)
 *   domain: religious/political/territorial
 *
 * SUMMARY:
 *   This story isolates the territorial-grant reading of the Genesis covenant
 *   (the promise of the Land of Canaan to Abraham's descendants) as a
 *   distinct structural claim from the lineage-transmission question (which
 *   is handled in sibling readings isaac_covenant_reading and
 *   ishmael_covenant_reading). Whereas lineage readings dispute WHO carries
 *   the covenant, this reading concerns WHAT the covenant grants and WHEN it
 *   is operative — conditional on fidelity, historically fulfilled and
 *   closed, or perpetually ongoing and presently binding. The reading that
 *   treats the grant as unconditional and currently operative is the one with
 *   material downstream consequence: it is invocable by state actors as a
 *   legitimating layer for territorial policy, with displaced populations
 *   bearing the resulting costs. This is not a claim about the lineage
 *   kernel; it is a claim about the territorial-operative kernel, evaluated
 *   as its own constraint with its own epsilon.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, 0.81).
domain_priors:suppression_score(abrahamic_covenant__land_promise_constraint, 0.87).
domain_priors:theater_ratio(abrahamic_covenant__land_promise_constraint, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, extractiveness, 0.81).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__land_promise_constraint, snare).
narrative_ontology:human_readable(abrahamic_covenant__land_promise_constraint, "Territorial Reading of the Abrahamic Land Covenant (Genesis Grant of Canaan)").
narrative_ontology:topic_domain(abrahamic_covenant__land_promise_constraint, "religious/political/territorial").

domain_priors:requires_active_enforcement(abrahamic_covenant__land_promise_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__land_promise_constraint, 'c04c3b19-b394-4fca-a97d-bb1bfc646983').
narrative_ontology:cs_kernel_codification('c04c3b19-b394-4fca-a97d-bb1bfc646983', fixed_text).
narrative_ontology:cs_authority_grounding('c04c3b19-b394-4fca-a97d-bb1bfc646983', extraction).
narrative_ontology:cs_interpretation_layer_present('c04c3b19-b394-4fca-a97d-bb1bfc646983').
narrative_ontology:cs_reading_relation('c04c3b19-b394-4fca-a97d-bb1bfc646983', abrahamic_covenant__isaac_covenant_reading, influences).
narrative_ontology:cs_reading_relation('c04c3b19-b394-4fca-a97d-bb1bfc646983', abrahamic_covenant__ishmael_covenant_reading, influences).
narrative_ontology:cs_axiom('c04c3b19-b394-4fca-a97d-bb1bfc646983', foundational, land_promise_perpetually_unconditional).
narrative_ontology:cs_axiom_status(land_promise_perpetually_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('c04c3b19-b394-4fca-a97d-bb1bfc646983', land_promise_perpetually_unconditional, theological).
narrative_ontology:cs_axiom('c04c3b19-b394-4fca-a97d-bb1bfc646983', secondary, covenant_land_grant_confers_present_political_title).
narrative_ontology:cs_axiom_status(covenant_land_grant_confers_present_political_title, holdable).
narrative_ontology:cs_axiom_grounding('c04c3b19-b394-4fca-a97d-bb1bfc646983', covenant_land_grant_confers_present_political_title, conventional).
narrative_ontology:cs_reference_frame('c04c3b19-b394-4fca-a97d-bb1bfc646983', unconditional_perpetual_land_grant).
narrative_ontology:cs_drift_state('c04c3b19-b394-4fca-a97d-bb1bfc646983', post_1967_settlement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c04c3b19-b394-4fca-a97d-bb1bfc646983', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__land_promise_constraint, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, state_actors_invoking_territorial_covenant).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, displaced_palestinian_communities).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, non_state_claimants_without_covenant_standing).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, diaspora_religious_communities).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, divine_territorial_grant_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cites the Genesis land grant as a legitimating layer for territorial claims and settlement policy, often alongside secular legal, historical, and security arguments. Controls administrative and military apparatus that can translate the reading into facts on the ground (settlement expansion, land registration, permitting regimes). Can shift emphasis between religious and secular justifications depending on audience, which functions as an exit option unavailable to other seats.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, state_actors_invoking_territorial_covenant, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__land_promise_constraint, state_actors_invoking_territorial_covenant, agenda_setter).

% Bears the material consequences of the territorial reading: displacement, restricted movement, loss of land access, and permit regimes that reference or are defended by appeal to covenantal legitimacy. Has no standing within the covenant framework itself to contest the reading on its own terms, since the text names Abraham's line, not the current residents. Exit from the territory is frequently unavailable; exit from the argument is impossible since the claim is asserted over land they occupy.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, displaced_palestinian_communities, payer,
    powerless, biographical, trapped, regional).

% Includes Bedouin and other historically resident groups whose land claims predate or run alongside the disputed territory but who have no lineage-based standing to contest a covenant argument framed in exclusively Abrahamic terms. Their claims are litigated in secular courts that are themselves influenced by the political weight the covenant reading carries in public discourse.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, non_state_claimants_without_covenant_standing, payer,
    powerless, generational, trapped, regional).

% Argue from within Jewish and Christian tradition that the land promise in Genesis and Deuteronomy is explicitly conditioned on covenant fidelity (obedience, justice, treatment of the sojourner) and can be forfeited or deferred — a reading with deep textual support (e.g., Leviticus 26, Deuteronomy 28) that is largely absent from state-level political discourse, where the unconditional/fulfilled reading dominates because it is more usable for territorial claims.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, religious_conditionalist_theologians, excluded,
    moderate, civilizational, constrained, global).

% Adjudicate territorial disputes through international law frameworks (UN resolutions, armistice lines, occupation law) that explicitly do not recognize scriptural covenant as a source of territorial title, yet must operate in a political environment where the covenant reading materially shapes domestic political will and negotiating positions on all sides.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, international_legal_and_diplomatic_bodies, observer,
    institutional, generational, analytical, global).

% Derive theological and identity meaning from the unconditional/fulfilled land-promise reading without bearing the direct territorial or displacement costs; some organize politically and financially to support the state actor's territorial project. Their stake is symbolic and financial rather than physically extractive, but their advocacy sustains the reading's political durability.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, diaspora_religious_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__land_promise_constraint, diaspora_religious_communities, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__land_promise_constraint, state_actors_invoking_territorial_covenant).
narrative_ontology:fixing_cost_class(abrahamic_covenant__land_promise_constraint, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: At its narrowest, the covenant text coordinates a community's self-understanding of continuity, chosenness, and relationship to a homeland — a genuine identity and memory function independent of any specific territorial administration.
% TRANSFER_FUNCTION: The territorial reading, once operationalized politically, moves land access, residency rights, and physical security from populations without covenant standing to the state actor able to invoke the covenant as one legitimating strand among several (alongside security and historical claims).
% ABSENT_VOICES: Conditionalist theologians within the tradition itself, and the displaced communities whose land claims predate or run parallel to the covenant frame, are largely absent from the political discourse that decides how the covenant reading is operationalized — the loudest voices are state actors and organized diaspora advocates, not the textual tradition's internal dissent or the affected non-covenant populations.
% DISAPPEARANCE_RATIONALE: If the territorial reading of the covenant vanished as a legitimating argument overnight, the underlying territorial dispute would persist on security, historical, and demographic grounds — the covenant is one legitimating layer among several, not the sole cause. But it would materially weaken one side's domestic and diaspora political mobilization and remove a rhetorical resource used in settlement policy debates, so the parties dispute how much would actually rearrange versus how much is causally downstream of other factors.
% FOUNDING_PROBLEM: The Genesis narrative was composed, in the traditions that transmit it, to explain a people's origin, covenant relationship with God, and connection to a specific land, in a context of exile, wandering, or contested settlement in antiquity.
% FOUNDING_PROBLEM_CORROBORATION: Historical-critical biblical scholars (largely outside any state or advocacy interest) attest the text's likely composition context involves theological identity-formation during or after exile, not a real-estate title deed. Conditionalist theologians within the tradition itself attest the promise's conditional textual framing. State actors and allied advocacy organizations, who benefit from the unconditional/fulfilled reading, are the primary source asserting the founding problem remains fully live and unconditionally binding today — corroboration from outside the benefiting parties leans toward the conditional or memorial reading rather than the operative territorial-title reading.
narrative_ontology:disappearance_verdict(abrahamic_covenant__land_promise_constraint, contested).
narrative_ontology:founding_problem_status(abrahamic_covenant__land_promise_constraint, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__land_promise_constraint, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(abrahamic_covenant__land_promise_constraint, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__land_promise_constraint, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__land_promise_constraint_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__land_promise_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) and rising over the interval because the unconditional/operative reading has become progressively more embedded in policy instruments — settlement law, land registration, permitting — that translate a textual claim into material displacement. Suppression is even higher (0.87) because the reading's political durability depends on suppressing the internal conditionalist counter-tradition and on foreclosing non-covenant claimants' standing to contest it on the reading's own terms; this is a raw structural fact about the reading's persistence, not scaled by scope. Theater ratio is moderate (0.42) — real settlement and security infrastructure exists, but an increasing share of covenant-invocation functions as legitimating rhetoric layered onto policy already justified on other grounds.
 *
 * DIRECTIONALITY LOGIC:
 *   The state actor sits near the beneficiary end: it can selectively deploy the covenant argument alongside secular claims, giving it argumentative arbitrage unavailable to other seats. Displaced communities sit at the full-target end: trapped exit, no covenant standing, and the argument is asserted directly over land they occupy — the derivation chain correctly pushes their d high. Diaspora communities benefit symbolically and financially while being geographically insulated from the extraction they help sustain politically, which is why they carry role beneficiary with a secondary excluded — they are not in the room when policy costs are allocated, but they are far from powerless in shaping the discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a covenant identity narrative composed for an ancient community's self-understanding) is plausibly dead or radically transformed as an operative property claim, while the arrangement persists — the mismatch between founding_problem_status (contested, leaning dead-as-title-claim) and disappearance_verdict (contested, leaning world_rearranges for political mobilization even if not sole causal driver) is exactly the signature that should be surfaced rather than resolved by fiat: the reading endures because it is politically useful now, independent of whether its founding function is live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_conditionality_ambiguity,
    'Is the Genesis land grant conditional on covenant fidelity (as Leviticus 26 and Deuteronomy 28 suggest), historically fulfilled and closed (as some rabbinic and Christian supersessionist traditions hold), or perpetually ongoing and presently binding regardless of conduct?',
    'No empirical resolution is possible; this is a live theological-interpretive dispute within the source traditions themselves, resolvable only by appeal to which hermeneutic tradition and textual corpus (Torah, Prophets, later rabbinic commentary, Christian typology) is treated as authoritative — an internal-to-tradition question with no external adjudicator.',
    'A conditional or fulfilled-and-closed reading would collapse the territorial legitimating function this constraint models to near zero; an ongoing-unconditional reading sustains it. The extraction and suppression scores authored here assume the ongoing-unconditional variant is the one with live political uptake, which is a defensible but contestable empirical claim about current discourse, not a theological adjudication.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covenant_conditionality_ambiguity, conceptual, 'Whether the land promise is conditional, fulfilled, or ongoing — the core interpretive fork this reading resolves one way for structural purposes.').

omega_variable(
    religious_versus_secular_causal_weight,
    'How much of the actual territorial policy and displacement is causally driven by the covenant reading itself, versus security, historical, demographic, and geopolitical arguments that would persist independent of any theological claim?',
    'Comparative analysis of policy justification documents, political rhetoric across secular versus religious-nationalist factions, and counterfactual case comparison with territorial disputes lacking a scriptural legitimating layer.',
    'If covenant rhetoric is largely post-hoc legitimation for policy driven by other factors, this constraint''s extraction score overstates the reading''s independent causal contribution; if covenant rhetoric materially shapes policy choices (e.g., which settlements get state support), the score is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_versus_secular_causal_weight, empirical, 'Disentangling the covenant reading''s independent causal weight from other legitimating and causal factors in the territorial dispute.').

omega_variable(
    sibling_reading_independence,
    'Given that this reading concerns territorial operativeness rather than lineage, is it truly independent of the isaac_covenant_reading and ishmael_covenant_reading siblings, or does the land grant''s addressee (Isaac''s line versus a broader Abrahamic line) partially determine who can even invoke this reading?',
    'Textual and traditions analysis of whether the land-grant passages (Genesis 15, 17) are read by each lineage tradition as attaching to the same territorial referent, or whether Islamic tradition reads the Canaan land grant differently than the Jewish/Christian traditions do.',
    'If the land grant''s applicability is lineage-dependent, this reading is not fully separable from the sibling readings and the network edges should carry stronger directional weight (lineage reading materially gates who can invoke the land reading) rather than being merely thematically linked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_independence, conceptual, 'Whether the territorial-operativeness reading is structurally independent of, or gated by, the lineage-transmission readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__land_promise_constraint, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t1948, abrahamic_covenant__land_promise_constraint, theater_ratio, 1948, 0.25).
narrative_ontology:measurement(abra_tr_t1967, abrahamic_covenant__land_promise_constraint, theater_ratio, 1967, 0.3).
narrative_ontology:measurement(abra_tr_t1980, abrahamic_covenant__land_promise_constraint, theater_ratio, 1980, 0.34).
narrative_ontology:measurement(abra_tr_t1995, abrahamic_covenant__land_promise_constraint, theater_ratio, 1995, 0.37).
narrative_ontology:measurement(abra_tr_t2010, abrahamic_covenant__land_promise_constraint, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(abra_tr_t2024, abrahamic_covenant__land_promise_constraint, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(abra_be_t1948, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement(abra_be_t1967, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1967, 0.68).
narrative_ontology:measurement(abra_be_t1980, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1980, 0.72).
narrative_ontology:measurement(abra_be_t1995, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1995, 0.75).
narrative_ontology:measurement(abra_be_t2010, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2010, 0.79).
narrative_ontology:measurement(abra_be_t2024, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2024, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t1948, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1948, 0.58).
narrative_ontology:measurement(abra_su_t1967, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement(abra_su_t1980, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1980, 0.76).
narrative_ontology:measurement(abra_su_t1995, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1995, 0.8).
narrative_ontology:measurement(abra_su_t2010, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2010, 0.84).
narrative_ontology:measurement(abra_su_t2024, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2024, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__land_promise_constraint, identity_coordination).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, ishmael_covenant_reading).

% DUAL FORMULATION NOTE:
% This story is the territorial-operativeness member of a three-story kernel decomposition of 'the Abrahamic covenant.' isaac_covenant_reading and ishmael_covenant_reading dispute WHO transmits the covenant (lineage); this story assumes a lineage answer is settled elsewhere and asks WHAT the land grant means and WHEN it applies (conditional/fulfilled/ongoing), then measures the material downstream consequence of the ongoing-unconditional variant on the modern Israeli-Palestinian territorial dispute. The lineage readings causally upstream this one: whichever lineage reading a state actor holds partially determines whether they can invoke this land-grant reading as their own inheritance at all, which is why the omega on sibling_reading_independence flags a possible gating relationship rather than pure parallelism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
