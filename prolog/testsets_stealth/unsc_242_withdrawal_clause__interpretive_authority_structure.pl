% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__interpretive_authority_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__interpretive_authority_structure, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__interpretive_authority_structure
 *   human_readable: Resolution 242 Withdrawal Clause — Contested Interpretive Authority Structure
 *   domain: international law / diplomatic history
 *
 * SUMMARY:
 *   Security Council Resolution 242 (November 1967) calls for withdrawal of
 *   Israel armed forces 'from territories occupied in the recent conflict' in
 *   its equally authoritative English text, and 'des territoires occupés' in
 *   its French text; the articles diverge, and the divergence was a
 *   deliberate drafting expedient adopted so the resolution could pass. This
 *   story authors the meta-level constraint that divergence created: the
 *   structure of interpretive authority over the clause. Three claimants hold
 *   incompatible authority claims — the International Court of Justice claims
 *   judicial interpretation and has asserted it (2004 wall opinion; 2024
 *   advisory opinion on the occupation); the surviving drafting states claim
 *   authorial intent grounded in their negotiated choice of words; the
 *   occupying state claims customary practice grounded in subsequent
 *   agreements and conduct — and no claimant is accepted by the others.
 *   Because no authority commands assent, the substantive question is never
 *   definitively resolved, which is what keeps both substantive readings of
 *   the clause (the maximal and partial readings, authored as sibling
 *   constraint files) live. The arrangement's gains accrue to the occupying
 *   state, which holds the territory under a formally open obligation, and to
 *   the veto-holding patron whose shielding keeps every resolution path
 *   blocked; its costs fall on the occupied population, denied closure across
 *   generations, and on the organs — the General Assembly and the Court —
 *   whose interpretive requests are returned as defiance. This file's ε
 *   (0.78) indexes authority extraction: definitive closure and institutional
 *   authority diverted to power. The siblings' ε values index the territorial
 *   obligation each substantive reading would impose. The authority structure
 *   is upstream of both siblings: its contestation is the mechanism that
 *   keeps them live. Claim and metrics are authored independently: the
 *   claimed type is snare; the metrics describe the arrangement's observed
 *   operation.
 *
 * KEY AGENTS:
 *   - occupying_state: primary beneficiary (powerful / arbitrage) — holds the territory, advances the customary-practice claim, declines every adverse venue
 *   - veto_holding_patron_state: beneficiary and enforcement seat (institutional / arbitrage) — its veto is the machinery keeping the ambiguity unresolvable; advances the authorial-intent claim
 *   - drafting_states: secondary beneficiaries (institutional / arbitrage) — their authorship claim and drafting technique stay live
 *   - occupied_population: primary victim (powerless / trapped; structurally excluded from every deciding venue) — bears the occupation across generations
 *   - international_court_of_justice: contested agenda-setter (institutional / constrained) — claims and asserts judicial interpretation; is repudiated each time
 *   - un_general_assembly: victim and payer (organized / constrained) — requests closure, receives defiance
 *   - legal_scholarship_community: analytical observer — sees the full structure, binds no one
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.78).
domain_priors:suppression_score(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.72).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, extractiveness, 0.78).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__interpretive_authority_structure, snare).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__interpretive_authority_structure, "Resolution 242 Withdrawal Clause — Contested Interpretive Authority Structure").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__interpretive_authority_structure, "international law / diplomatic history").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__interpretive_authority_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__interpretive_authority_structure, '5e7a7357-b4b1-45ae-abd5-bf4a0dea0d34').
narrative_ontology:cs_kernel_codification('5e7a7357-b4b1-45ae-abd5-bf4a0dea0d34', fixed_text).
narrative_ontology:cs_authority_grounding('5e7a7357-b4b1-45ae-abd5-bf4a0dea0d34', distributed).
narrative_ontology:cs_reading_relation('5e7a7357-b4b1-45ae-abd5-bf4a0dea0d34', unsc_242_withdrawal_clause__maximal_withdrawal_reading, influences).
narrative_ontology:cs_reading_relation('5e7a7357-b4b1-45ae-abd5-bf4a0dea0d34', unsc_242_withdrawal_clause__partial_withdrawal_reading, influences).
narrative_ontology:cs_axiom('5e7a7357-b4b1-45ae-abd5-bf4a0dea0d34', foundational, interpretive_closure_requires_accepted_authority).
narrative_ontology:cs_axiom_status(interpretive_closure_requires_accepted_authority, holdable).
narrative_ontology:cs_axiom_grounding('5e7a7357-b4b1-45ae-abd5-bf4a0dea0d34', interpretive_closure_requires_accepted_authority, conventional).
narrative_ontology:cs_axiom('5e7a7357-b4b1-45ae-abd5-bf4a0dea0d34', foundational, equal_authentic_texts_leave_meaning_open).
narrative_ontology:cs_axiom_status(equal_authentic_texts_leave_meaning_open, holdable).
narrative_ontology:cs_axiom_grounding('5e7a7357-b4b1-45ae-abd5-bf4a0dea0d34', equal_authentic_texts_leave_meaning_open, conventional).
narrative_ontology:cs_reference_frame('5e7a7357-b4b1-45ae-abd5-bf4a0dea0d34', deliberate_ambiguity_adoption_settlement).
narrative_ontology:cs_drift_state('5e7a7357-b4b1-45ae-abd5-bf4a0dea0d34', post_2024_advisory_opinion, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('5e7a7357-b4b1-45ae-abd5-bf4a0dea0d34', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, veto_holding_patron_state).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, occupied_population).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, un_general_assembly).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__interpretive_authority_structure, deliberate_ambiguity_drafting_technique).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__interpretive_authority_structure, consent_based_jurisdiction_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the territories whose withdrawal the clause addresses. It argues that subsequent agreements and the parties' actual conduct show the withdrawal's scope was always meant for negotiation. It accepts the resolution as the region's governing framework while declining every venue that might fix its meaning against it: it has not accepted the Court's jurisdiction over the dispute and treats advisory opinions as advisory. Leaving the arrangement would mean accepting some authority as decisive, and each candidate authority is one it would lose under, so it keeps the contest open. It continues to hold the territory while the question of its obligation remains formally open.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state, beneficiary,
    powerful, generational, arbitrage, regional).

% A permanent member of the Security Council and the resolution's principal drafter. Its vote blocks any enforcement resolution that would attach consequences to either reading of the clause, and it argues that the English text's wording was deliberately chosen and the drafters' understanding should control. It endorses the resolution's framework while shielding the occupying state from consequences, and it has a wider interest at stake: no precedent of compulsory interpretation of its own conduct or its partners' conduct. Backing a single interpretive authority would cost it either the alliance or that prerogative, so it maintains the open question. Its veto is what the arrangement's continuation runs through.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, veto_holding_patron_state, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, veto_holding_patron_state, agenda_setter).

% The states that negotiated the 1967 text — its surviving authors include permanent Council members. They argue the words were theirs, that the divergence between the English and French texts was a known and accepted drafting choice, and that their negotiated understanding should carry weight. The standing arrangement keeps their authorship claim live as one of the recognized sources of meaning and vindicates the technique by which they got the text adopted when a clearer one would have failed. They do not agree among themselves on the substantive answer, which is why their claim sustains the contest rather than settling it.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states, beneficiary,
    institutional, generational, arbitrage, global).

% Lives under the occupation the clause addresses — displacement, settlement expansion, administrative control — across generations. It has standing in none of the venues that might decide the withdrawal question: no access to contentious proceedings at the Court, no seat in the Council, no place among the drafters. Its case reaches those venues only indirectly, through Assembly sponsorship and the advocacy of other states. It cannot leave the territory or the open question; each generation inherits both.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, occupied_population, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__interpretive_authority_structure, occupied_population, excluded).

% The principal judicial organ of the United Nations. It holds that interpreting the Charter and the Council's resolutions is a judicial task, and it has asserted that task over the territory question — most fully in its 2004 and 2024 advisory opinions, which treat the occupation's continuance as unlawful and withdrawal as required. It can act only when an organ asks or states consent, and it commands no enforcement: its opinions bind no one against their will. Each assertion is met with repudiation from the parties whose conduct is at issue, which erodes the authority the assertion invokes.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, international_court_of_justice, agenda_setter,
    institutional, civilizational, constrained, global).

% The organ that requests the advisory opinions and passes the resolutions seeking a settled answer. It commands numbers but not compliance: each request it makes has been answered by an opinion the parties whose conduct is at issue repudiate. It is structurally committed to the resolution-based process it administers and cannot step outside it, and each defied opinion devalues the next request it makes.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, un_general_assembly, payer,
    organized, generational, constrained, global).

% The scholars and practitioners who document the drafting history, the divergence between the two authentic texts, and the successive interpretive claims. They see the whole structure — that each claimant's authority argument is selective, and that the contest's persistence tracks the interests of the parties with power. They hold seats in no venue and command no enforcement; their analyses are cited by every side and settle nothing.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, legal_scholarship_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__interpretive_authority_structure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The structure lets states that read the clause incompatibly all endorse the same resolution: by leaving interpretive authority distributed, it converted a substantive disagreement that would have split the Council in 1967 into a shared framework each party could accept while keeping its own reading. Agreement on the formula substituted for agreement on the meaning.
% TRANSFER_FUNCTION: Definitive legal closure is withheld from the occupied population and the rule-seeking organs, and the decades it is withheld for are converted by the occupying state into continued territorial control; interpretive authority is diverted from the judicial venue to whichever party's claim the moment favors — authorial intent when the drafters speak, custom when the occupier acts, judicial interpretation when the Court's audience is receptive.
% ABSENT_VOICES: The occupied population holds a seat in none of the three claimed venues — no standing in contentious proceedings at the Court, no vote in the Council, no place among the drafters — so the party whose conduct and territory the clause governs is represented only through Assembly sponsorship and third-state advocacy. The Council's elected members are likewise present but overridable. Both would insist that no interpretive authority claim constructed without them can bind them; their absence from every deciding venue is what lets the beneficiaries' unanimity pass as the regime's voice.
% DISAPPEARANCE_RATIONALE: If a single interpretive authority were accepted overnight, the withdrawal question becomes determinate: either the occupation is placed under a definitive obligation to withdraw from all the territories — forcing compliance or overt, costly defiance — or the negotiated-adjustment reading is ratified and the parties bargain on a settled baseline. Either outcome dissolves the current equilibrium, in which perpetual contest sustains the status quo; the parties' present positions are constituted by the ambiguity, so its removal rearranges them.
% FOUNDING_PROBLEM: In 1967 the Council needed a resolution both superpower blocs and all the belligerents could accept, but the blocs demanded incompatible withdrawal scopes — one side insisted on total withdrawal, the other required scope for territorial adjustment. The arrangement was built to solve that adoption problem: engineer a bilingual text whose articles diverge, adopt it without settling whose reading controls, and leave interpretive authority distributed so no adoption-blocking question is ever forced.
% FOUNDING_PROBLEM_CORROBORATION: The adoption problem was solved at adoption — the resolution passed the Council in November 1967 — and no party outside the beneficiary set attests that it remains live. The General Assembly's repeated requests for advisory opinions, the Court's 2024 finding that the occupation's continuance is unlawful, and the scholarly travaux record documenting the bilingual divergence as a deliberate drafting expedient all attest, from outside the beneficiary set, that the deferral now serves the beneficiaries' positions rather than any adoption purpose.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__interpretive_authority_structure, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__interpretive_authority_structure, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__interpretive_authority_structure, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78 at interval end) because the arrangement's product is deferral: five decades of withheld legal closure, converted by the occupying state into continued territorial control and by the patron into cost-free alliance maintenance, while the judicial and Assembly venues pay in eroded authority. Suppression (0.72) is the operative mechanism, not a side effect: the ambiguity persists because resolution paths are actively blocked — the veto forecloses Council enforcement, the consent requirement forecloses contentious adjudication, and formal repudiation forecloses the binding force of advisory opinions. Suppression is authored as a raw structural property; only extractiveness is scaled by the engine. Theater (0.42) reflects a mix of genuine interpretive work — the Court's opinions are reasoned law, the travaux scholarship is real history — with performative maintenance: selective citation of whichever authority favors the speaker, periodic re-endorsement of language each endorser knows is contested, and process activity sustained to substitute for outcome. Accessibility collapse is moderate (0.48): the alternatives — binding adjudication, enforcement action, negotiated settlement — remain formally available, which is the snare's signature, but each is blocked in operation by a beneficiary, so understanding the structure does not make an exit usable. Resistance (0.62) is real and sustained — Assembly requests, Court assertions, scholarly contest, population advocacy — and has so far been absorbed without cost to the beneficiaries. The occupied population's coalition power runs through the Assembly, the one vehicle where numbers matter, and the arrangement absorbs it: Assembly numbers produce opinions, and opinions are repudiated, so the coalition's output is deflected rather than crushed. The measurement series run on one shared time grid (t = 0, 11, 26, 37, 49, 57, mapping to 1967 adoption, the 1978 Camp David accords, the 1993 Oslo accords, the 2004 wall opinion, the 2016 settlement-resolution episode, and the 2024 occupation opinion) so every tracked metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats the arrangement is a framework each helped build and can inhabit indefinitely: the occupying state experiences the contest as legitimate pluralism among authorities it never submitted to; the patron experiences its veto as stewardship of a negotiated text; the drafters experience the contest as vindication of authorship. From the payer seats the same structure is a deferral machine: the occupied population experiences five decades of the occupation the clause names, with no venue open to it; the Assembly experiences each request for closure returned as defiance. The Court's seat is genuinely dual — it experiences the structure as an affront to its claimed function, yet each of its assertions feeds the contest that sustains the arrangement, and its institutional stake in the dispute's existence gives it a partial interest in the contest's continuation even as its authority is the thing defied. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the occupying state captures the arrangement's primary gain — the territory stays under its control while its obligation stays formally open — and holds arbitrage-grade exit, citing the Council text, invoking bilateral practice, and repudiating advisory opinions selectively, exploiting the very multiplicity the arrangement maintains. The patron collects alliance maintenance and precedent protection and holds the veto, the arrangement's enforcement machinery. The drafting states collect the standing of authorial intent as a live source of meaning. Victim declarations drive high directionality: the occupied population is trapped — no exit from the territory or the ambiguity, no seat in any deciding venue — and sits nearest the full-target end; the Assembly is a victim with partial mobility (constrained, not trapped): it can keep requesting and condemning, and what it pays is authority erosion rather than direct occupation. The Court carries no beneficiary or victim declaration; its directionality falls to the engine's fallback for its power atom, and this commentary records its dual position explicitly rather than forcing it into either array.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — adopting a resolution both blocs and all belligerents could accept — was a genuine coordination problem, solved once, in 1967, by the engineered ambiguity. Classifying the surviving structure as a tangled_rope would credit it with a live coordination function it no longer performs: the acceptance it coordinates today is acceptance of the deferral itself, which is the extraction mechanism. Classifying it as a rope would erase the identifiable victims. The snare classification, read against the R5 genealogy (founding problem dead; world would rearrange on resolution), prevents both errors: it preserves the historical fact of real coordination while naming the current operation as extraction sustained by active suppression. The mandatrophy question — has the mandate outlived its function? — resolves to yes: the adoption mandate expired at adoption, and the dead-founding-problem-plus-world-rearranges mismatch is the corpus's capture signal for exactly this shape.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_separation,
    'This constraint instantiates the interpretive_authority_structure reading of the unsc_242_withdrawal_clause kernel; the kernel''s sibling readings (maximal_withdrawal_reading, partial_withdrawal_reading) are separate constraints — what would change if this story were authored as one of the siblings instead?',
    'Read the sibling files: their epsilon is indexed to the territorial obligation each substantive reading would impose, while this file''s epsilon is indexed to the extraction of definitive closure and institutional authority. The disagreement among readings is located in who holds decisive interpretive authority — Court, drafters, or practice — not in withdrawal scope; merging the readings would make epsilon observer-relative and violate epsilon-invariance.',
    'If the readings were merged into one story, the authority structure''s high extraction would be averaged against substantive-reading profiles, corrupting both classifications and destroying the family''s upstream/downstream structure; kept separate, each reading holds one stable epsilon and the engine can compute foreclosure and influence across the constraint family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_separation, conceptual, 'Committer structure: this is one of three readings of the Resolution 242 withdrawal-clause kernel; sibling readings are separate constraint files.').

omega_variable(
    coordination_extraction_boundary,
    'Is the distributed interpretive authority load-bearing for the acceptance-coordination function the 1967 text performed, or is that function separable from the extraction the distribution now sustains?',
    'Counterfactual adoption history: if a determinate text had been adoptable in 1967 (single controlling language, agreed article), would the Council have passed any withdrawal resolution at all? If no resolution was adoptable without the ambiguity, part of the structure''s cost is the standing price of the coordination; if a determinate text was available and rejected for strategic reasons, the distribution is extraction wearing coordination''s clothes.',
    'If separable, the arrangement''s coordination story is exhausted and the snare classification stands unqualified; if inseparable, the classification must weight the genuine coordination the structure performed in keeping all parties inside one framework, and the effective extraction attributable to the authority contest shrinks accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the authority ambiguity''s coordination and extraction components are structurally separable.').

omega_variable(
    consent_adjudication_collapse,
    'Would a single consent event collapse the structure — an occupying-state acceptance of contentious jurisdiction over the withdrawal question, or a Council enforcement reference surviving the veto — and if so, which substantive reading would the accepted authority produce?',
    'Track docket and consent developments: any contentious case reaching the Court with the relevant parties'' consent, or any enforcement resolution that survives the veto and is implemented, would convert the distributed-authority contest into a settled determination and date the collapse of this constraint.',
    'If consent arrives, this constraint dissolves into whichever substantive reading the accepted authority endorses — the extraction machinery loses its load-bearing ambiguity; if consent never arrives, the structure persists indefinitely and the snare classification hardens with time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_adjudication_collapse, empirical, 'Whether a consented adjudication or surviving enforcement act would collapse the contested authority structure.').

omega_variable(
    beneficiary_composition_stability,
    'Does the structure depend on the current veto alignment — specifically on the patron''s continued shielding — such that a realignment of Council politics (a patron withdrawing shielding, or the drafting-state claim changing hands) would reconfigure or collapse the arrangement?',
    'Observe Council voting and follow-through across successive enforcement attempts: a patron abstention followed by enforcement follow-through, or a realigned drafting-state bloc asserting authorial intent against the occupation, would test whether the ambiguity survives without active shielding.',
    'If the structure is patron-dependent, its persistence is a policy variable rather than a structural equilibrium and the arrangement''s expected lifetime is shorter than its fifty-seven-year history suggests; if it survives alignment shifts, the extraction is more robust than any single beneficiary and the snare classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_composition_stability, empirical, 'Whether the arrangement''s persistence depends on the current identity of its shielding beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__interpretive_authority_structure, 0, 57).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(unsc_tr_t0, observed).
narrative_ontology:measurement(unsc_tr_t11, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 11, 0.22).
narrative_ontology:measurement_basis(unsc_tr_t11, observed).
narrative_ontology:measurement(unsc_tr_t26, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 26, 0.28).
narrative_ontology:measurement_basis(unsc_tr_t26, observed).
narrative_ontology:measurement(unsc_tr_t37, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 37, 0.34).
narrative_ontology:measurement_basis(unsc_tr_t37, observed).
narrative_ontology:measurement(unsc_tr_t49, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 49, 0.38).
narrative_ontology:measurement_basis(unsc_tr_t49, observed).
narrative_ontology:measurement(unsc_tr_t57, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 57, 0.42).
narrative_ontology:measurement_basis(unsc_tr_t57, observed).

% Extraction over time
narrative_ontology:measurement(unsc_be_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(unsc_be_t0, observed).
narrative_ontology:measurement(unsc_be_t11, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 11, 0.55).
narrative_ontology:measurement_basis(unsc_be_t11, observed).
narrative_ontology:measurement(unsc_be_t26, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 26, 0.6).
narrative_ontology:measurement_basis(unsc_be_t26, observed).
narrative_ontology:measurement(unsc_be_t37, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 37, 0.68).
narrative_ontology:measurement_basis(unsc_be_t37, observed).
narrative_ontology:measurement(unsc_be_t49, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 49, 0.73).
narrative_ontology:measurement_basis(unsc_be_t49, observed).
narrative_ontology:measurement(unsc_be_t57, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 57, 0.78).
narrative_ontology:measurement_basis(unsc_be_t57, observed).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t0, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(unsc_su_t0, observed).
narrative_ontology:measurement(unsc_su_t11, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 11, 0.55).
narrative_ontology:measurement_basis(unsc_su_t11, observed).
narrative_ontology:measurement(unsc_su_t26, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 26, 0.6).
narrative_ontology:measurement_basis(unsc_su_t26, observed).
narrative_ontology:measurement(unsc_su_t37, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 37, 0.66).
narrative_ontology:measurement_basis(unsc_su_t37, observed).
narrative_ontology:measurement(unsc_su_t49, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 49, 0.69).
narrative_ontology:measurement_basis(unsc_su_t49, observed).
narrative_ontology:measurement(unsc_su_t57, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 57, 0.72).
narrative_ontology:measurement_basis(unsc_su_t57, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__interpretive_authority_structure, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause__maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause__partial_withdrawal_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'what does Resolution 242 require?' decomposes into three structurally distinct claims per the ε-invariance principle. This file authors the meta-level constraint: the contested structure of interpretive authority (judicial interpretation vs. authorial intent vs. customary practice), whose ε indexes the extraction of definitive closure and institutional authority. The sibling files author the substantive readings it keeps unresolved — the maximal reading (French definite article controls; territorial-integrity default) and the partial reading (drafters' indefinite-article intent; secure-boundaries adjustment) — whose ε values index the territorial obligation each would impose. The authority structure is upstream: it determines which substantive reading can prevail, and its contestation is the mechanism that keeps both siblings live, which is why this file declares influences edges to both. All three files are linked through network.affects_constraints as one constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
