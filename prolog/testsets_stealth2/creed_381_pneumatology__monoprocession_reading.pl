% ============================================================================
% CONSTRAINT STORY: creed_381_pneumatology__monoprocession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creed_381_pneumatology__monoprocession_reading, []).

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
 *   constraint_id: creed_381_pneumatology__monoprocession_reading
 *   human_readable: 381 Pneumatological Clause as Inviolable Fixity — Monoprocession Reading (Ecumenical-Consent Wall)
 *   domain: historical_theology/ecclesiastical_authority/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel is
 *   the Second Ecumenical Council's (Constantinople, 381) pneumatological
 *   clause — 'the Spirit proceeds from the Father' — together with the
 *   authority question of who may amend the common creed. The
 *   monoprocession_reading holds the clause as fixed revelation ('from the
 *   Father alone'), the creed as inviolable without the consent of an
 *   ecumenical council, and unilateral amendment (the Western Filioque
 *   insertion) as breach. The constraint operates as a wall: no single see
 *   may legislate doctrine for the whole Church. Its self-presentation is
 *   that of inviolable revealed fixity — hence the mountain claim with
 *   emerges_naturally true, authored as the reading's own framing. The
 *   metrics are authored independently as what I judge descriptively true of
 *   the wall's operation: an actively enforced, beneficiary-bearing structure
 *   with substantial asymmetric extraction. That claim/metric gap is the
 *   measurement this story exists to take; the false-summit signature is
 *   intentionally armed by declaring beneficiaries on a mountain claim. Per
 *   Rule 1, the reading contest is NOT described inside this constraint: the
 *   sibling readings (filioque_reading, ecumenical_reunion_reading) are
 *   separate constraint files, linked via network.affects_constraints, with
 *   the committer structure routed to omegas. KEY AGENTS (by structural
 *   relationship): - eastern_autocephalous_churches: Primary beneficiary
 *   (institutional/identity_locked) — decentralized conciliar polity
 *   preserved, received text secured - eastern_patriarchal_synods: Agenda
 *   setter (institutional/arbitrage) — administers the consent gate, defines
 *   ecumenicity, issues anathemas - roman_apostolic_see: Primary target
 *   (institutional/identity_locked) — its amendment ruled breach, its
 *   clarifying authority denied - western_latin_clergy_and_faithful:
 *   Secondary target (organized/constrained) — inherit breach-status and
 *   communion rupture - western_dogmatic_theologians: Target
 *   (moderate/identity_locked) — doctrinal development paths blocked at the
 *   gate - oriental_orthodox_communion: Excluded voice (organized/trapped) —
 *   outside the conversation entirely - joint_ecumenical_commission:
 *   Analytical observer (institutional/analytical) — sees the full structure,
 *   holds no enforcement power
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creed_381_pneumatology__monoprocession_reading, 0.72).
domain_priors:suppression_score(creed_381_pneumatology__monoprocession_reading, 0.68).
domain_priors:theater_ratio(creed_381_pneumatology__monoprocession_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creed_381_pneumatology__monoprocession_reading, mountain).
narrative_ontology:human_readable(creed_381_pneumatology__monoprocession_reading, "381 Pneumatological Clause as Inviolable Fixity — Monoprocession Reading (Ecumenical-Consent Wall)").
narrative_ontology:topic_domain(creed_381_pneumatology__monoprocession_reading, "historical_theology/ecclesiastical_authority/commitment_systems").

domain_priors:requires_active_enforcement(creed_381_pneumatology__monoprocession_reading).
domain_priors:emerges_naturally(creed_381_pneumatology__monoprocession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(creed_381_pneumatology__monoprocession_reading, '630fb93f-726f-4284-b059-745b90e72e08').
narrative_ontology:cs_kernel_codification('630fb93f-726f-4284-b059-745b90e72e08', fixed_text).
narrative_ontology:cs_authority_grounding('630fb93f-726f-4284-b059-745b90e72e08', lineage).
narrative_ontology:cs_interpretation_layer_present('630fb93f-726f-4284-b059-745b90e72e08').
narrative_ontology:cs_reading_relation('630fb93f-726f-4284-b059-745b90e72e08', creed_381_pneumatology__filioque_reading, forecloses).
narrative_ontology:cs_reading_relation('630fb93f-726f-4284-b059-745b90e72e08', creed_381_pneumatology__ecumenical_reunion_reading, forecloses).
narrative_ontology:cs_axiom('630fb93f-726f-4284-b059-745b90e72e08', foundational, spirit_eternal_procession_from_father_alone).
narrative_ontology:cs_axiom_status(spirit_eternal_procession_from_father_alone, holdable).
narrative_ontology:cs_axiom_grounding('630fb93f-726f-4284-b059-745b90e72e08', spirit_eternal_procession_from_father_alone, theological).
narrative_ontology:cs_axiom('630fb93f-726f-4284-b059-745b90e72e08', foundational, creed_amendment_requires_ecumenical_consent).
narrative_ontology:cs_axiom_status(creed_amendment_requires_ecumenical_consent, holdable).
narrative_ontology:cs_axiom_grounding('630fb93f-726f-4284-b059-745b90e72e08', creed_amendment_requires_ecumenical_consent, conventional).
narrative_ontology:cs_axiom('630fb93f-726f-4284-b059-745b90e72e08', secondary, unilateral_amendment_constitutes_breach).
narrative_ontology:cs_axiom_status(unilateral_amendment_constitutes_breach, holdable).
narrative_ontology:cs_axiom_grounding('630fb93f-726f-4284-b059-745b90e72e08', unilateral_amendment_constitutes_breach, conventional).
narrative_ontology:cs_reference_frame('630fb93f-726f-4284-b059-745b90e72e08', pentarchic_consensual_fixity).
narrative_ontology:cs_drift_state('630fb93f-726f-4284-b059-745b90e72e08', contemporary_ecumenical_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('630fb93f-726f-4284-b059-745b90e72e08', '').
narrative_ontology:cs_kernel_id(creed_381_pneumatology__monoprocession_reading, creed_381_pneumatology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__monoprocession_reading, eastern_autocephalous_churches).
narrative_ontology:constraint_victim(creed_381_pneumatology__monoprocession_reading, roman_apostolic_see).
narrative_ontology:constraint_victim(creed_381_pneumatology__monoprocession_reading, western_latin_clergy_and_faithful).
narrative_ontology:constraint_victim(creed_381_pneumatology__monoprocession_reading, western_dogmatic_theologians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__monoprocession_reading, eastern_patriarchal_synods).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__monoprocession_reading, spirit_eternal_procession_from_father_alone).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__monoprocession_reading, ecumenical_consent_requirement).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__monoprocession_reading, pentarchic_canonical_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A communion of self-governing churches (Constantinople, Alexandria, Antioch, Jerusalem, Moscow, and the younger autocephalies) that confess the 381 creed in its received form. The wall secures what they hold: no single see can alter their common text, and their decentralized conciliar order — many thrones, no legislator — is protected against Western centralization. Their exit is unthinkable from where they stand: accepting the amended creed would dissolve the fidelity-to-the-received-tradition identity that constitutes them; they could not abandon the wall without ceasing to be what they understand themselves to be.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, eastern_autocephalous_churches, beneficiary,
    institutional, civilizational, identity_locked, global).

% The synodal organs and first-thrones that administer the wall in practice: they determine what counts as ecumenical consent, monitor creed usage across jurisdictions, issue and renew the anathemas against unauthorized additions (the annual Synodikon commemorations), and control whether any pan-Orthodox council that might revisit the gate ever convenes. They collect definitional authority from the gate's operation — the power to say what the whole Church's consent requires. Unlike their constituents, they hold working levers: they can modulate enforcement intensity, convene or withhold councils, and negotiate dialogue formats.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, eastern_patriarchal_synods, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__monoprocession_reading, eastern_patriarchal_synods, beneficiary).

% The see that inserted the Filioque into its liturgical creed (gradually through the medieval West, definitively at Constantinople in 1014 for the coronation rite) and maintains it as a legitimate clarification of implicitly held doctrine. Under this reading's wall, its amendment is breach, its clarifying authority is denied, and its communion with the Eastern churches is ruptured. It cannot retract the addition without overturning its own defined dogmatic development — the magisterial self-concept consolidated through Vatican I is fused with the authority the wall denies. Its historic exits were deal-shaped (Lyons, Florence) and failed on reception; from where it stands, leaving its own doctrine is not an option.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, roman_apostolic_see, payer,
    institutional, civilizational, identity_locked, global).

% The clergy and laity of the Latin communion, who inherit the amended creed as the only liturgical text they have known and bear the wall's costs without having chosen either premise: they stand inside the breach by inheritance, are anathematized by association in annual commemorations they do not hear, and live the communion rupture as missing sacramental unity. Individual exit means leaving their church entirely; voice within it runs through structures that uphold the addition.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, western_latin_clergy_and_faithful, payer,
    organized, biographical, constrained, global).

% Scholars formed in the scholastic and magisterial framework, for whom the Filioque is load-bearing: Trinitarian systematic theology, seminary curricula, and confessional documents are built on the procession-from-Father-and-Son premise. The wall rules their development path breach-adjacent and blocks the consensual route by which their tradition might test its own formulation against the East. Their professional identity is constituted within the framework the wall freezes; abandoning the premise would unravel their life's systematic work.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, western_dogmatic_theologians, payer,
    moderate, biographical, identity_locked, continental).

% The non-Chalcedonian churches (Armenian, Coptic, Ethiopian, Syriac and others), already outside the Chalcedonian-family conversation and holding no seat in it. Their own creedal traditions include their own additions and formulations, which bears directly on any claim that one received text is the uniquely inviolable common possession. They would contest the conversation's premise — that the 381 text as received by the two disputing parties is the fixed center — but they are not in the room, and their absence makes the unanimity of the two-party dispute look more settled than it is.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, oriental_orthodox_communion, excluded,
    organized, civilizational, trapped, continental).

% The bilateral dialogue bodies (the Joint International Commission and its national analogues) that take testimony from both seats, produce agreed statements on the procession language (Munich 1982; the North American agreed statement of 2003), and map what convergence would require. They see the full structure from outside it, hold no enforcement power, and depend on both communions' goodwill for access; their analyses enter the record but bind no one.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__monoprocession_reading, joint_ecumenical_commission, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(creed_381_pneumatology__monoprocession_reading, eastern_autocephalous_churches).
narrative_ontology:fixing_cost_class(creed_381_pneumatology__monoprocession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds a single confessional text constant across many autocephalous churches by requiring that no single see or regional synod alter it alone: doctrinal change in the common creed requires the consent of an ecumenical council. This solves the collective-action problem of creed fragmentation among peer churches that recognize no common legislator — the problem the fourth-century proliferation of local creeds made vivid.
% TRANSFER_FUNCTION: Moves doctrinal-legislative authority into the ecumenical-consent gate, administered in practice by the Eastern synods' recognition processes, and moves the costs of doctrinal stasis — development-blockage, breach-status, communion rupture — onto the Western churches. In the other direction it transfers security: the Eastern churches' received text and autonomous polity are insulated from Western definition.
% ABSENT_VOICES: The Oriental Orthodox communion is wholly outside the conversation, though its own creedal history bears directly on creed-fixity claims. Ordinary laity of both communions bear the communion costs and hold no seat. The medieval Spanish-Frankish liturgists who first added the Filioque are dead; their rationale survives only in texts. Historical-critical theologians who regard all credal formulas as historically conditioned and revisable are systematically excluded from the gatekeeping conversation — the constituency for whom 'inviolable' is a category error has no chair at the gate.
% DISAPPEARANCE_RATIONALE: If the inviolability-and-consent wall vanished overnight, the creed would become amendable at the level of each see: the Eastern churches would face immediate accept-the-Western-text-or-fragment pressure, the conciliar-decentralized polity would reorganize around either papal definition or free regional variation, and the schism's doctrinal core would convert from fixed breach into renegotiable difference. Arrangements on both sides — liturgical texts, anathema calendars, dialogue agendas, identity narratives — depend on the wall's standing.
% FOUNDING_PROBLEM: Two problems, layered: (1) complete Nicaea's confession against the Pneumatomachians, who denied the Spirit's full deity — the 381 fathers expanded the third article to affirm the Spirit's co-divinity; (2) secure the common creed against unilateral regional alteration — the conciliar text was received as the whole Church's possession, alterable only by a council of the whole, a lesson written by the era's proliferating local creeds.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated from outside the beneficiary set: the acts and reception record of 381 itself — the creed was received by the West at Chalcedon (451) as 'the faith of the 150 fathers,' attesting the anti-Pneumatomachian purpose from both sides of the later divide — and academic patristics scholarship, which reconstructs the Pneumatomachian controversy independently of either communion's claims. On current status the parties dispute: the Eastern churches attest the anti-unilateral problem as live and the wall as its guardian; the Roman see and Western theologians attest that the live problem is now the wall itself, which froze a resolvable difference into permanent breach. The Joint International Commission's agreed statements — bodies seated by both communions — attest from outside either beneficiary set that the consent problem remains unresolved and live.
narrative_ontology:disappearance_verdict(creed_381_pneumatology__monoprocession_reading, world_rearranges).
narrative_ontology:founding_problem_status(creed_381_pneumatology__monoprocession_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(creed_381_pneumatology__monoprocession_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(creed_381_pneumatology__monoprocession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(creed_381_pneumatology__monoprocession_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creed_381_pneumatology__monoprocession_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(creed_381_pneumatology__monoprocession_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, ExtMetricName, E),
    domain_priors:suppression_score(creed_381_pneumatology__monoprocession_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(creed_381_pneumatology__monoprocession_reading),
    narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(creed_381_pneumatology__monoprocession_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(creed_381_pneumatology__monoprocession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the wall freezes doctrinal development for the whole Church on terms the gatekeeper set: the consent gate is presently unpassable post-schism, the Western churches bear permanent breach-status for a text they inherited, and the Eastern received text gains exclusive legitimacy. Suppression (0.68) is authored as a RAW structural property — sacramental-coercive machinery (anathematization, communion discipline, synodical monitoring) — and is NOT scaled by power or scope; only extractiveness is scaled by the engine from directionality and scope. Theater (0.40) reflects a growing share of ritual maintenance: annual Synodikon anathemas directed at long-dead opponents, polemical literature detached from live negotiation, ceremonial reaffirmations — alongside a still-real guard function. Accessibility collapse is moderate (0.55): the sibling readings remain live positions and reunion proposals persist, so alternatives are suppressed but not eliminated. Resistance is high (0.72): a millennium of Western non-compliance, counter-theology, and modern bilateral dialogue. The measurement series run on ONE shared grid (t = 0,5,10,15,20,25,30; every tracked metric authored at every point). Interval anchoring: t0 approximates the late eighth-century ignition of the Filioque controversy (Carolingian attack on Greek practice), t=10 approximates the 1054 rupture, t=30 the contemporary ecumenical era. The suppression_requirement series is authored deliberately: this story tracks enforcement-capacity change — an enforcement ratchet from polemic (0.30) through schism-era institutionalization (0.55) to hardened annual anathematization (0.68). The trajectories are monotonic, not cyclical: the ratchet, once built, has not relaxed. Rising base_extractiveness on a mountain-claimed story will trip the T17 accumulation hypothesis; that firing is anticipated and is data, not noise.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the eastern_patriarchal_synods seat, the wall is the coordination structure they administer: a consent gate protecting a multi-center communion from fragmentation — experienced as legitimate order. From the roman_apostolic_see seat, the same structure is enforced extraction: its considered doctrinal development is ruled breach by a gate it is barred from passing, administered by the party that benefits from the freeze. The western clergy and faithful inherit the dispute as fact, bearing communion rupture without having chosen either premise. Coalition potential is real but historically frustrated: the Roman see twice attempted exit-by-agreement (Lyons 1274, Florence 1439), trading recognition for reunion; both collapsed on reception — evidence that its exit is constrained-to-identity-locked rather than mobile, and that coalition offers fail when the beneficiary set's own constituents are identity-fused. The excluded Oriental Orthodox seat would contest the conversation's premise itself: their own creedal traditions show that 'the' received text is already a Chalcedonian-family artifact.
 *
 * DIRECTIONALITY LOGIC:
 *   The structural derivation from beneficiary/victim declarations plus exit options suffices; no directionality_overrides are authored. eastern_autocephalous_churches sit near the beneficiary end (d low): the wall subsidizes their polity and text. eastern_patriarchal_synods sit near-beneficiary with a slight upward shift: they administer the gate and collect definitional authority from its operation. roman_apostolic_see sits near the full-target end (d high): identity_locked exit amplifies effective extraction — the see cannot repudiate its own defined dogma without dissolving its magisterial self-concept. western_latin_clergy_and_faithful are near-full targets with constrained exit; western_dogmatic_theologians are targets whose identity_lock (systematic formation in which the Filioque is load-bearing) traps them nearer the target end than their mobility alone would predict. Observers (joint_ecumenical_commission) sit symmetric. The excluded oriental_orthodox_communion feeds no directionality as a non-seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate is layered, and the classification must not flatten it. The anti-Pneumatomachian layer — completing Nicaea's confession against deniers of the Spirit's deity — is dead: no party to this dispute denies the Spirit's full divinity. The anti-unilateral layer — preventing any regional power from altering the common confession alone — is live, arguably more live than at founding, since the feared event occurred. The wall therefore is neither a zombie (its operative function persists) nor a clean rope (the gate's administration concentrates definitional authority in the beneficiary set while imposing stasis costs on the other side). The theater_ratio trajectory (0.15 to 0.40) tracks the growth of ritual maintenance over live function, but the constraint is not yet pitonal: enforcement still bites (communion discipline is real), resistance is still met, and the administrator could not cheaply change the arrangement without fracturing its own identity-fused constituents. The classification guards against both mislabels: reading the wall as pure coordination erases the asymmetric extraction; reading it as pure extraction erases the genuine multi-center creed-stability problem that any communion of peer churches without a common legislator must solve somehow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revealed_fixity_vs_polity_wall,
    'Is the 381 creed''s inviolability a matter of revealed, Spirit-guided fixity (a genuine mountain of the tradition), or a constructed ecclesiastical-political rule that preserves Eastern conciliar polity against Western centralization (a false summit with identifiable beneficiaries)?',
    'Selectivity test across the tradition''s own practice: if the same authorities treat other credal and doctrinal formulations as definable and revisable when their polity benefits (e.g., the Palamite councils of 1341-1351 defined newly), then fixity attaches selectively to the 381 text and the naturality claim is constructed; if the fixity claim is applied uniformly to all conciliar text, the mountain reading stands.',
    'Resolved as constructed, the false-summit reclassification away from mountain is confirmed and the beneficiary structure becomes the primary explanatory variable; resolved as genuinely revelatory, the mountain certification stands and the extraction measured is the price of guarding revelation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revealed_fixity_vs_polity_wall, conceptual, 'Whether the constraint''s presented naturality is revelation-grounded fixity or constructed polity defense.').

omega_variable(
    consent_gatekeeper_identity,
    'Who counts as ''the whole Church'' whose consent the amendment gate requires, and who administers that determination?',
    'Canonical analysis of what the tradition requires for ecumenicity: pentarchic five-see concurrence, autocephalous synodal consensus, or reception by the whole people. Each criterion assigns the gate to a different body.',
    'If consent requires all ancient sees including Rome, the gate is presently unpassable and the wall is a mutual lock binding both sides; if consent means the Eastern communion''s consensus alone, the wall is a one-way instrument administered by the beneficiary set, and effective extraction concentrates accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_gatekeeper_identity, conceptual, 'Whether the consent gate is a mutual lock or a beneficiary-administered one-way wall.').

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is one reading of kernel creed_381_pneumatology; how would the sibling readings (filioque_reading, ecumenical_reunion_reading) change the structural classification if instantiated?',
    'Author the sibling files and compare: the filioque_reading inverts the beneficiary/victim structure (Roman magisterium as agenda_setter, dissenting East as payer); the ecumenical_reunion_reading dissolves the wall entirely, replacing the consent gate with bilateral recognition, collapsing epsilon toward the coordination-cost floor.',
    'Confirms that epsilon is reading-indexed over a shared referent: the disagreement is located in the amendment-authority premise (who may legislate doctrine for the whole Church), not in the creed text itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer-frame omega recording kernel membership, this reading''s identity, and the structural effect of sibling readings.').

omega_variable(
    enforcement_vs_identity_persistence,
    'How much of the wall''s persistence is active enforcement (anathemas, communion discipline, synodical monitoring) versus internalized identity (constituents on both sides fused with their creedal inheritance)?',
    'Post-enforcement-relaxation trajectory: where enforcement machinery has been suspended or softened (local suspensions of anathema commemorations during dialogue periods), observe whether positions, liturgical practice, and breach-attribution persist unchanged; persistence under relaxed enforcement indicates identity-carried maintenance.',
    'If identity-carried, the constraint''s effective suppression exceeds the structural measure and drift toward inertial maintenance accelerates; if enforcement-carried, relaxation would produce rapid renegotiation and the wall remains a live enforced structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_identity_persistence, empirical, 'Structural versus internalized share of the constraint''s persistence.').

omega_variable(
    founding_text_reception_asymmetry,
    'Was the 381 creed received as universally inviolable at its promulgation, or is the inviolability claim retroactive?',
    'Reception-history analysis: the Council of 381 was a Constantinopolitan gathering initially resisted in the West; the creed achieved Western liturgical standing only through its reception at Chalcedon (451) as ''the faith of the 150 fathers.'' Document the gap between promulgation and universal reception.',
    'A qualified founding reception weakens the historical grounding of the inviolability norm''s universality and strengthens the constructed-rule reading; an unqualified reception would support the mountain claim''s genealogy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_text_reception_asymmetry, empirical, 'Whether the founding text''s own universality was immediate or retrospectively asserted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creed_381_pneumatology__monoprocession_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cree_tr_t0, creed_381_pneumatology__monoprocession_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(cree_tr_t0, observed).
narrative_ontology:measurement(cree_tr_t5, creed_381_pneumatology__monoprocession_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(cree_tr_t5, observed).
narrative_ontology:measurement(cree_tr_t10, creed_381_pneumatology__monoprocession_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(cree_tr_t10, observed).
narrative_ontology:measurement(cree_tr_t15, creed_381_pneumatology__monoprocession_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement_basis(cree_tr_t15, observed).
narrative_ontology:measurement(cree_tr_t20, creed_381_pneumatology__monoprocession_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(cree_tr_t20, observed).
narrative_ontology:measurement(cree_tr_t25, creed_381_pneumatology__monoprocession_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement_basis(cree_tr_t25, observed).
narrative_ontology:measurement(cree_tr_t30, creed_381_pneumatology__monoprocession_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(cree_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(cree_be_t0, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(cree_be_t0, observed).
narrative_ontology:measurement(cree_be_t5, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(cree_be_t5, observed).
narrative_ontology:measurement(cree_be_t10, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(cree_be_t10, observed).
narrative_ontology:measurement(cree_be_t15, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(cree_be_t15, observed).
narrative_ontology:measurement(cree_be_t20, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement_basis(cree_be_t20, observed).
narrative_ontology:measurement(cree_be_t25, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 25, 0.71).
narrative_ontology:measurement_basis(cree_be_t25, observed).
narrative_ontology:measurement(cree_be_t30, creed_381_pneumatology__monoprocession_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement_basis(cree_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(cree_su_t0, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(cree_su_t0, observed).
narrative_ontology:measurement(cree_su_t5, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(cree_su_t5, observed).
narrative_ontology:measurement(cree_su_t10, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement_basis(cree_su_t10, observed).
narrative_ontology:measurement(cree_su_t15, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(cree_su_t15, observed).
narrative_ontology:measurement(cree_su_t20, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(cree_su_t20, observed).
narrative_ontology:measurement(cree_su_t25, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 25, 0.67).
narrative_ontology:measurement_basis(cree_su_t25, observed).
narrative_ontology:measurement(cree_su_t30, creed_381_pneumatology__monoprocession_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement_basis(cree_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creed_381_pneumatology__monoprocession_reading, identity_coordination).
narrative_ontology:affects_constraint(creed_381_pneumatology__monoprocession_reading, creed_381_pneumatology__filioque_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__monoprocession_reading, creed_381_pneumatology__ecumenical_reunion_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of kernel creed_381_pneumatology per the epsilon-invariance principle. The colloquial label 'the Filioque dispute' covers three structurally distinct constraints: this monoprocession_reading (wall-type, high epsilon, Eastern-beneficiary), the filioque_reading (gatekeeping-type with inverted beneficiary structure, Roman-magisterium-beneficiary), and the ecumenical_reunion_reading (dissolution-type, epsilon near coordination floor). The monoprocession and filioque readings are upstream siblings sharing the referent and citing the same reception history as evidence for opposed conclusions; the reunion reading is downstream, presupposing both as positions to be mediated. Every member links the others via affects_constraints; each file carries its own epsilon, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
