% ============================================================================
% CONSTRAINT STORY: john_1_1_logos__subordinationist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_john_1_1_logos__subordinationist, []).

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
 *   constraint_id: john_1_1_logos__subordinationist
 *   human_readable: Subordinationist Reading Regime of the Johannine Logos
 *   domain: theology/biblical_hermeneutics/christology
 *
 * SUMMARY:
 *   The subordinationist reading of John 1:1 - the Logos as first-created,
 *   subordinate divine agent, not co-eternal or consubstantial with the
 *   Father - operates wherever it governs as an interpretive-disciplinary
 *   regime: it fixes how the Logos passages may be read, orders worship
 *   around a strict Father/Son distinction, and maintains a boundary against
 *   Nicene Christendom. This story authors THAT arrangement as the
 *   subordinationist reading itself assesses it: the epsilon referent is the
 *   standing subordinationist regime, not the Nicene settlement this reading
 *   contests and not any endorsed alternative. The kernel 'the Logos of John
 *   1:1' decomposes into three readings - non_incarnational_monotheist (no
 *   distinct hypostasis), subordinationist (created/subordinate hypostasis),
 *   orthodox_christological (uncreated, consubstantial hypostasis) - each a
 *   separate constraint story linked through network.affects_constraints; the
 *   label's apparent singularity conceals three structurally distinct claims
 *   with different victim sets, enforcement histories, and epsilon values.
 *   Claim and metrics are authored independently: the reading presents itself
 *   as restored primitive monotheism, while the authored metrics describe a
 *   moderately extractive, actively enforced hybrid.
 *
 * KEY AGENTS:
 *   - - subordinationist_movement_leadership: Agenda-setter and primary beneficiary (institutional/arbitrage) - administers the reading, captures interpretive authority, loyalty, and funds
 *   - - subordinationist_lay_members: Beneficiary with secondary payer position (moderate/identity_locked) - receive coherent monotheism and community; pay labor, funds, and social separation
 *   - - high_church_traditions: Payer and excluded voice (institutional/identity_locked) - bear delegitimation of the authority claim their identity rests on; locked out of the community's conversation
 *   - - internal_dissenters_in_subordinationist_communities: Primary target among members (powerless/trapped) - disciplined, shunned, socially severed
 *   - - academic_new_testament_scholars: Analytical observer (analytical/analytical) - documents the grammar and reception history both sides draw on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(john_1_1_logos__subordinationist, 0.52).
domain_priors:suppression_score(john_1_1_logos__subordinationist, 0.58).
domain_priors:theater_ratio(john_1_1_logos__subordinationist, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, extractiveness, 0.52).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(john_1_1_logos__subordinationist, tangled_rope).
narrative_ontology:human_readable(john_1_1_logos__subordinationist, "Subordinationist Reading Regime of the Johannine Logos").
narrative_ontology:topic_domain(john_1_1_logos__subordinationist, "theology/biblical_hermeneutics/christology").

domain_priors:requires_active_enforcement(john_1_1_logos__subordinationist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(john_1_1_logos__subordinationist, '87f88457-1199-4f85-89ef-0f8cd3af1bcd').
narrative_ontology:cs_kernel_codification('87f88457-1199-4f85-89ef-0f8cd3af1bcd', fixed_text).
narrative_ontology:cs_authority_grounding('87f88457-1199-4f85-89ef-0f8cd3af1bcd', lineage).
narrative_ontology:cs_interpretation_layer_present('87f88457-1199-4f85-89ef-0f8cd3af1bcd').
narrative_ontology:cs_reading_relation('87f88457-1199-4f85-89ef-0f8cd3af1bcd', john_1_1_logos__orthodox_christological, forecloses).
narrative_ontology:cs_reading_relation('87f88457-1199-4f85-89ef-0f8cd3af1bcd', john_1_1_logos__non_incarnational_monotheist, forecloses).
narrative_ontology:cs_axiom('87f88457-1199-4f85-89ef-0f8cd3af1bcd', foundational, logos_first_created_agent).
narrative_ontology:cs_axiom_status(logos_first_created_agent, holdable).
narrative_ontology:cs_axiom_grounding('87f88457-1199-4f85-89ef-0f8cd3af1bcd', logos_first_created_agent, empirically_contingent).
narrative_ontology:cs_axiom('87f88457-1199-4f85-89ef-0f8cd3af1bcd', secondary, exclusive_worship_reserved_for_the_father).
narrative_ontology:cs_axiom_status(exclusive_worship_reserved_for_the_father, holdable).
narrative_ontology:cs_axiom_grounding('87f88457-1199-4f85-89ef-0f8cd3af1bcd', exclusive_worship_reserved_for_the_father, deontological).
narrative_ontology:cs_reference_frame('87f88457-1199-4f85-89ef-0f8cd3af1bcd', pre_nicene_subordinationist_consensus).
narrative_ontology:cs_drift_state('87f88457-1199-4f85-89ef-0f8cd3af1bcd', post_nicene_imperial_settlement, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('87f88457-1199-4f85-89ef-0f8cd3af1bcd', '').
narrative_ontology:cs_kernel_id(john_1_1_logos__subordinationist, john_1_1_logos).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, subordinationist_movement_leadership).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, subordinationist_lay_members).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, high_church_traditions).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, internal_dissenters_in_subordinationist_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, subordinationist_lay_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teaches the reading as binding interpretation, publishes the study materials that mediate members' access to the texts, and administers the discipline that marks departure from it. Historically this seat included imperial-court bishops who secured coerced subscription to homoian formulas; today it includes governing bodies and publishing houses whose differentiation from Nicene Christendom depends on maintaining the reading. They control the arrangement and could revise it, but revision would dissolve the distinctiveness their authority rests on.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, subordinationist_movement_leadership, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(john_1_1_logos__subordinationist, subordinationist_movement_leadership, beneficiary).

% Receive a coherent strict-monotheist framework, a bounded scriptural curriculum, and a dense community whose friendships, marriage pool, and weekly rhythm run through the congregation. They contribute labor, funds, and evangelizing hours, and absorb the social cost of separation from broader Christian culture. Leaving means forfeiting the entire social world, not merely revising an opinion about one verse.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, subordinationist_lay_members, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(john_1_1_logos__subordinationist, subordinationist_lay_members, payer).

% Catholic, Orthodox, and magisterial Protestant bodies whose teaching authority rests on the claim that the Logos is fully divine and consubstantial with the Father. Every circulation of the subordinationist reading erodes the exclusivity of their interpretive authority and arms their critics with a 'you departed from the original faith' argument. They cannot concede the point without dissolving the authority structure itself, so they answer through counter-catechesis, anathema, and scholarship - and they are kept outside the subordinationist community's authoritative conversation, entering it only as 'tradition-bound error' to be corrected.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, high_church_traditions, payer,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(john_1_1_logos__subordinationist, high_church_traditions, excluded).

% Members who read the texts independently and reach conclusions the teaching authority rejects - drifting toward Nicene language or toward dissolving the Logos's personal agency altogether. Discipline falls on them: removal from roles, shunning by family and friends inside the community, loss of the social world the community constitutes. Their objections circulate afterward only as cautionary examples.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, internal_dissenters_in_subordinationist_communities, payer,
    powerless, biographical, trapped, global).

% Document the grammatical state of John 1:1c, the pre-Nicene reception history, and the political mechanics of the fourth-century controversy. Bound by neither side's confessional commitments, they supply ammunition to both - the subordinationist movements cite their philology, the orthodox cite their patristics - and they track which reading the combined evidence will bear.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, academic_new_testament_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(john_1_1_logos__subordinationist, subordinationist_movement_leadership).
narrative_ontology:fixing_cost_class(john_1_1_logos__subordinationist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the monotheist's problem posed by the Logos passages: how a community confessing one supreme God can read 'the Word was god' (John 1:1c), 'firstborn of all creation' (Colossians 1:15), and the Wisdom typology of Proverbs 8 without either collapsing the Father/Word distinction or admitting a second uncreated God. The reading supplies one answer - a first-created, subordinate agent - and with it a unified curriculum, a worship protocol (prayer to the Father, veneration of the Son as God's agent rather than adoration as God), and a boundary against Nicene Christendom.
% TRANSFER_FUNCTION: Moves interpretive authority and loyalty upward to the teaching leadership, whose publications, conventions, and disciplinary rulings center members' judgment; moves members' labor, funds, and evangelizing hours into the organization; and moves reputational capital away from high-church traditions by contesting the antiquity and exclusivity of their full-divinity claim.
% ABSENT_VOICES: High-church theologians and expelled dissenters would object - the former that the reading severs the grammar of worship the tradition received, the latter that the community's discipline punishes independent reading. Both stand outside the authoritative conversation: their objections enter only as apostate material or tradition-bound error, never as positions answered on the merits.
% DISAPPEARANCE_RATIONALE: If the subordinationist discipline vanished overnight, member worship practice would drift toward ambient Nicene piety within a generation, the leadership's differentiation-based authority would lose its doctrinal floor, and the high-church traditions would lose a standing counter-claim they currently define part of themselves against. Communities organized around the reading would reorganize or dissolve.
% FOUNDING_PROBLEM: Second-century Christians confessing the God of Israel while praying to and about the exalted Christ needed an account of the Word's relation to the Father that preserved monotheism; the subordinationist solution made the Word the first and highest creature, God's agent in creation and revelation, venerated but not worshipped as God.
% FOUNDING_PROBLEM_CORROBORATION: Academic patristics and New Testament scholarship attest the question's persistence from outside any benefiting party: the continuing scholarly debate over early high Christology versus agency and angelomorphic readings, and the fact that every major tradition still teaches a settled answer to the same question, corroborate that the underlying problem remains live. The subordinationist movements' own liveness claim is thus independently attested by literature produced with no stake in their survival.
narrative_ontology:disappearance_verdict(john_1_1_logos__subordinationist, world_rearranges).
narrative_ontology:founding_problem_status(john_1_1_logos__subordinationist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(john_1_1_logos__subordinationist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(john_1_1_logos__subordinationist, 'none', 1).
narrative_ontology:epsilon_provenance(john_1_1_logos__subordinationist, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(john_1_1_logos__subordinationist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(john_1_1_logos__subordinationist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(john_1_1_logos__subordinationist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.52: real transfers (member labor, funds, evangelizing hours; dissenters' social worlds) ride on a regime that also delivers genuine goods - coherent monotheism, community, a scriptural curriculum - hence hybrid rather than pure. Suppression 0.58: enforcement is active and organized (disfellowshipping, information control; historically, coerced imperial subscription), but physical exit remains possible, keeping it below snare-grade. Theater 0.28: publishing volume, meeting attendance, and convention counts carry a performative loyalty-display component atop a still-functioning teaching core. Accessibility collapse 0.42: the rival readings stay grammatically and historically available - the Greek admits the qualitative reading, the Fathers are cited by all sides - so the regime can police alternatives but not close them. Resistance 0.62: seventeen centuries of orthodox counter-pressure, internal dissent, and academic critique. Measurements share one seven-point grid (years since circa 100 CE: 0, 255, 281, 1453, 1779, 1879, 1925 correspond roughly to 100, 355, 381, 1553, 1779, 1879, and 2025 CE); the series trace two arcs - imperial-era enforcement peaking under the Homoian court, collapsing after the Nicene coalition's victory, then rebuilding with modern organizational discipline. Fixing cost is prohibitive for the seat that could fix it: the leadership has revised adjacent doctrines before, but relaxing the reading itself would dissolve the distinctiveness that binds members and legitimates the center, a cost exceeding any benefit to that seat.
 *
 * PERSPECTIVAL GAP:
 *   The leadership seat computes a coordination story it administers and profits from; the lay seat computes a mixed ledger of belonging and cost; the dissenter seat computes enforced extraction with no exit; the high-church seat computes a delegitimation attack on its authority rather than extraction from its resources. Same texts, same discipline, four different constraints - the engine derives this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Leadership declares beneficiary-plus-agenda-setter: gains demonstrably accrue to it, so d sits near the beneficiary end. Lay members declare beneficiary with secondary payer: genuine goods received, real costs borne, landing near symmetric. Dissenters declare payer with trapped exit: near full target. High-church traditions declare payer: they bear the reading's costs as authority erosion rather than resource transfer - high d, though their independent legitimacy sources (apostolic succession, sacramental life) mean the damage lands on their exclusivity claim, not their existence. No directionality overrides are authored: the derivation chain from role, power, and exit reproduces these relationships, and a power-atom-keyed override would miskey the leadership seat, which shares the institutional atom with the high-church traditions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - relating the exalted Christ to the one God of Israel - remains live wherever the texts are read, so this is not a mandate outliving its function; mandatrophy_resolved stays undeclared. The classification guards both errors: reading the regime as pure extraction would erase the genuine monotheistic coordination its members experience and the scholarly respectability of parts of its exegetical case; reading it as pure coordination would erase the disciplined dissenters and the leadership's capture of gains. Tangled rope names both halves. The mismatch consumer should note founding_problem_status=live with verdict world_rearranges - no zombie flag fires; the open question carried by the omegas is whether the regime's enforcement intensity is proportionate to its coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'This constraint instantiates only the subordinationist reading of the john_1_1_logos kernel; what structural changes follow if the orthodox_christological or non_incarnational_monotheist reading is instantiated instead?',
    'Author and compile the sibling stories; compare victim sets, enforcement profiles, and computed types across the constraint family.',
    'The orthodox reading reverses the victim structure (its enforcement machinery historically targeted subordinationist communities after Nicaea) and raises suppression; the non-incarnational reading removes the hypostasis question entirely, shrinking the worship-practice constraint and eliminating the veneration-versus-adoration boundary this reading maintains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Committer structure: one reading of a three-reading kernel').

omega_variable(
    anarthrous_theos_determinacy,
    'Is the grammatical force of the anarthrous theos in John 1:1c determinate enough to settle creaturehood versus full deity - definite, qualitative, or convertible?',
    'Philological analysis of anarthrous pre-verbal predicate nominatives in Johannine and wider Koine usage; peer-reviewed syntax studies and their critiques.',
    'If the qualitative force is granted as decisive for a created divine agent, this reading''s foundational axiom gains empirical footing and its enforcement looks more like truth-defense; if indeterminate, the regime''s policing of one grammatical outcome over others is harder to distinguish from identity protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anarthrous_theos_determinacy, empirical, 'Whether the key grammatical datum settles the exegetical dispute').

omega_variable(
    pre_nicene_consensus_claim,
    'Was the subordinationist reading actually the pre-Nicene majority consensus, as the reading''s lineage authority claims, or already one voice among several?',
    'Comprehensive reception-history analysis of second- and third-century witnesses, weighing the revisionist patristic scholarship against confessional counters.',
    'If the consensus claim fails, the reading''s lineage grounding weakens sharply and its enforcement looks more like sectarian identity maintenance than restoration; if it holds, the Nicene settlement reads as innovation and this reading''s coordination framing strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre_nicene_consensus_claim, empirical, 'Whether the reading''s pre-Nicene-majority genealogy is historically accurate').

omega_variable(
    member_retention_mechanism,
    'Is member adherence driven primarily by conviction in the reading''s exegetical case or by identity and social lock-in - family, community, sunk lifetime investment?',
    'Exit interviews and longitudinal studies of leavers: if belief typically survives exit while community ties do not, identity lock dominates; if belief collapses on contact with contrary scholarship, conviction was load-bearing.',
    'If identity lock dominates, the measured suppression understates effective coercion - the constraint travels with the member after exit - and the payer seat''s effective extraction is higher than the scalar suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(member_retention_mechanism, conceptual, 'Conviction versus identity-lock as the retention mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(john_1_1_logos__subordinationist, 0, 1925).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(john_tr_t0, john_1_1_logos__subordinationist, theater_ratio, 0, 0.14).
narrative_ontology:measurement(john_tr_t255, john_1_1_logos__subordinationist, theater_ratio, 255, 0.31).
narrative_ontology:measurement(john_tr_t281, john_1_1_logos__subordinationist, theater_ratio, 281, 0.26).
narrative_ontology:measurement(john_tr_t1453, john_1_1_logos__subordinationist, theater_ratio, 1453, 0.19).
narrative_ontology:measurement(john_tr_t1779, john_1_1_logos__subordinationist, theater_ratio, 1779, 0.21).
narrative_ontology:measurement(john_tr_t1879, john_1_1_logos__subordinationist, theater_ratio, 1879, 0.25).
narrative_ontology:measurement(john_tr_t1925, john_1_1_logos__subordinationist, theater_ratio, 1925, 0.28).

% Extraction over time
narrative_ontology:measurement(john_be_t0, john_1_1_logos__subordinationist, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(john_be_t255, john_1_1_logos__subordinationist, base_extractiveness, 255, 0.6).
narrative_ontology:measurement(john_be_t281, john_1_1_logos__subordinationist, base_extractiveness, 281, 0.44).
narrative_ontology:measurement(john_be_t1453, john_1_1_logos__subordinationist, base_extractiveness, 1453, 0.3).
narrative_ontology:measurement(john_be_t1779, john_1_1_logos__subordinationist, base_extractiveness, 1779, 0.33).
narrative_ontology:measurement(john_be_t1879, john_1_1_logos__subordinationist, base_extractiveness, 1879, 0.47).
narrative_ontology:measurement(john_be_t1925, john_1_1_logos__subordinationist, base_extractiveness, 1925, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(john_su_t0, john_1_1_logos__subordinationist, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(john_su_t255, john_1_1_logos__subordinationist, suppression_requirement, 255, 0.68).
narrative_ontology:measurement(john_su_t281, john_1_1_logos__subordinationist, suppression_requirement, 281, 0.4).
narrative_ontology:measurement(john_su_t1453, john_1_1_logos__subordinationist, suppression_requirement, 1453, 0.24).
narrative_ontology:measurement(john_su_t1779, john_1_1_logos__subordinationist, suppression_requirement, 1779, 0.28).
narrative_ontology:measurement(john_su_t1879, john_1_1_logos__subordinationist, suppression_requirement, 1879, 0.5).
narrative_ontology:measurement(john_su_t1925, john_1_1_logos__subordinationist, suppression_requirement, 1925, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(john_1_1_logos__subordinationist, identity_coordination).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__orthodox_christological).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__non_incarnational_monotheist).

% DUAL FORMULATION NOTE:
% 'The Logos of John 1:1' is a colloquial label covering three structurally distinct claims - no hypostasis, created hypostasis, uncreated hypostasis. Per the epsilon-invariance principle they are decomposed into three stories (this one plus the two siblings), each with its own epsilon, victim set, and enforcement history, linked as a constraint family. The upstream/downstream gradient runs through reception history: the subordinationist reading dominated the second-century stratum, the orthodox reading consolidated at Nicaea and Constantinople and became upstream in institutional authority, and the non-incarnational reading draws on the exegetical raw material of both strata.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
