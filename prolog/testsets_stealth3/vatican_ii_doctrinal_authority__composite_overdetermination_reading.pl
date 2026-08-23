% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Unified-Reform Packaging with Managed Doctrinal Ambiguity (Composite Overdetermination Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This story instantiates the composite_overdetermination_reading of the
 *   kernel vatican_ii_doctrinal_authority. The standing arrangement under
 *   contest — and the sole referent of the authored epsilon — is the
 *   post-conciliar governance arrangement in which the Council's
 *   heterogeneous reforms (liturgical, ecumenical, ecclesiological,
 *   church-state) are administered as a single authoritative package whose
 *   contested terms are deliberately left unresolved and managed rather than
 *   settled. On this reading the continuity-versus-rupture dispute is a
 *   category error: the components exhibit different degrees of change by
 *   different mechanisms, and the retained ambiguities are a structural
 *   feature of the package, not defects awaiting resolution. The packaging
 *   delivered a real coordination good — near-unanimous ratification of a
 *   reform that any explicit resolution might have shattered — while creating
 *   a standing asymmetry: factions downstream commit labor to implementations
 *   the texts never guarantee, and the center retains the discretionary right
 *   to ratify or condemn those implementations retroactively (the 1988
 *   excommunications and the 2007-to-2021 liturgical
 *   liberalization-then-reversal are the emblematic exercises). KEY AGENTS
 *   (by structural relationship): see key_agents. Family note: the sibling
 *   readings are separate constraint stories, not alternatives inside this
 *   one — continuity_reading, rupture_progressive_reading, and
 *   rupture_traditionalist_reading each instantiate a different constraint
 *   from the same kernel with its own epsilon, beneficiaries, and victims;
 *   this file links them via network.affects_constraints and documents the
 *   epsilon divergence in the dual-formulation note.
 *
 * KEY AGENTS:
 *   - papal_magisterium: Agenda-setting center (institutional/arbitrage) — holds interpretive discretion over the unsettled texts; the seat the arrangement's gains demonstrably accrue to
 *   - roman_curia: Administrative collector (institutional/arbitrage) — converts perpetual interpretive workload into jurisdiction; genuinely dual-positioned
 *   - conciliar_generation_hierarchy: Ratifying beneficiary (organized/identity_locked) — received a reform each bloc could read its own way
 *   - ecumenical_partner_churches: External beneficiary (organized/mobile) — sits outside the enforcement perimeter entirely
 *   - traditionalist_communities: Primary payer (organized/identity_locked) — bears the standing cost of irregular status and reversed concessions
 *   - progressive_theologians: Secondary payer (moderate/constrained) — bore the censures when favored strands were ruled excessive
 *   - ordinary_faithful: Diffuse payer (powerless/identity_locked) — absorbs liturgical and catechetical instability
 *   - council_minority_fathers: Excluded (organized/identity_locked) — the objections that produced the ambiguities, now outside the conversation
 *   - women_of_the_church: Excluded with payer costs (powerless/identity_locked) — implemented the reform, decided none of it
 *   - council_historians: Analytical observer (analytical/analytical) — the external check on every reading's claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.64).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.58).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "Vatican II Unified-Reform Packaging with Managed Doctrinal Ambiguity (Composite Overdetermination Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__composite_overdetermination_reading, '9a9aaa12-f9dd-440c-ae29-77d6c04842fa').
narrative_ontology:cs_kernel_codification('9a9aaa12-f9dd-440c-ae29-77d6c04842fa', fixed_text).
narrative_ontology:cs_authority_grounding('9a9aaa12-f9dd-440c-ae29-77d6c04842fa', lineage).
narrative_ontology:cs_interpretation_layer_present('9a9aaa12-f9dd-440c-ae29-77d6c04842fa').
narrative_ontology:cs_reading_relation('9a9aaa12-f9dd-440c-ae29-77d6c04842fa', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('9a9aaa12-f9dd-440c-ae29-77d6c04842fa', vatican_ii_doctrinal_authority__rupture_progressive_reading, forecloses).
narrative_ontology:cs_reading_relation('9a9aaa12-f9dd-440c-ae29-77d6c04842fa', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, forecloses).
narrative_ontology:cs_axiom('9a9aaa12-f9dd-440c-ae29-77d6c04842fa', foundational, no_unitary_council_verdict).
narrative_ontology:cs_axiom_status(no_unitary_council_verdict, holdable).
narrative_ontology:cs_axiom_grounding('9a9aaa12-f9dd-440c-ae29-77d6c04842fa', no_unitary_council_verdict, empirically_contingent).
narrative_ontology:cs_axiom('9a9aaa12-f9dd-440c-ae29-77d6c04842fa', foundational, ambiguity_is_structural_feature).
narrative_ontology:cs_axiom_status(ambiguity_is_structural_feature, holdable).
narrative_ontology:cs_axiom_grounding('9a9aaa12-f9dd-440c-ae29-77d6c04842fa', ambiguity_is_structural_feature, instrumental).
narrative_ontology:cs_reference_frame('9a9aaa12-f9dd-440c-ae29-77d6c04842fa', overdetermined_package_consensus).
narrative_ontology:cs_drift_state('9a9aaa12-f9dd-440c-ae29-77d6c04842fa', post_traditionis_custodes, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9a9aaa12-f9dd-440c-ae29-77d6c04842fa', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, papal_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, roman_curia).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, conciliar_generation_hierarchy).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_partner_churches).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_communities).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, progressive_theologians).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ordinary_faithful).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, women_of_the_church).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines which readings of the conciliar texts are authoritative, and when. Keeps the most contested terms officially unresolved, which preserves freedom to endorse one strand, tolerate another, and condemn a third as circumstances shift — as when the pre-conciliar liturgy went from suppressed, to universally permitted in 2007, to restricted again in 2021. Bears little cost from the unresolved state; the cost of any eventual settlement falls on whichever faction guessed wrong.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, papal_magisterium, agenda_setter,
    institutional, generational, arbitrage, global).

% Runs the offices that review doctrine, regulate worship, and process disciplinary cases arising from disputes over what the Council meant. The perpetual stream of interpretation questions sustains its jurisdiction, staffing, and centrality; it also executes the decisions that periodically penalize one faction or another. Its position improves the longer the texts stay unsettled.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, roman_curia, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__composite_overdetermination_reading, roman_curia, agenda_setter).

% The bishops who debated and voted on the documents. Each major bloc received wording it could live with — collegiality balanced by primacy footnotes, liturgical change balanced by rubrical continuity — and nearly all signed. Most lived long enough to see their preferred strand advanced or checked by later decisions, but their episcopal identity bound them to defend the Council they had approved regardless.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, conciliar_generation_hierarchy, beneficiary,
    organized, biographical, identity_locked, global).

% Protestant and Orthodox bodies that gained dialogue channels, mutual-recognition language, and a partner willing to say the boundaries of the Church extend beyond its visible institution. They sit outside the machinery that enforces the settlement's internal discipline and can deepen or suspend engagement at will; the opening cost them nothing binding.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_partner_churches, beneficiary,
    organized, generational, mobile, global).

% Communities organized around the pre-conciliar liturgy and the objections of the council's minority. They accepted irregular status rather than the settlement, accepted regularization when offered (1988, 2007), and absorbed the reversal when it came (2021). Their self-understanding as faithful Catholics preserving tradition makes both full acceptance of the settlement and departure from Catholicism unacceptable, so they persist in a standing irregular position whose terms others periodically revise.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_communities, payer,
    organized, generational, identity_locked, global).

% Scholars and pastors who read the Council's openings as mandates — for liturgical creativity, collegial governance, moral revision, engagement with liberation movements. When the center later ruled those readings excessive, individuals lost chairs, approvals, and standing; the censures of the late 1970s and 1980s are the emblematic cases. Their livelihoods depend on institutional recognition they forfeit by pressing the unsettled strands too far.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, progressive_theologians, payer,
    moderate, biographical, constrained, global).

% The mass of practicing Catholics, who experience the settlement as a sequence of parish-level changes: a liturgy transformed, then partially re-translated; catechesis rewritten by successive generations; parishes and religious orders consolidated or closed as implementation swung. Devout believers hold the sacramental life to be accessible only inside the institution, so staying is not optional even when the ground keeps moving.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ordinary_faithful, payer,
    powerless, generational, identity_locked, global).

% The bloc of council fathers — roughly a tenth to a seventh of the floor — who contested the drafts on collegiality, liturgy, and religious liberty. Their objections were handled with explanatory notes and footnote balances rather than answers, which is how several load-bearing ambiguities entered the final texts. Their institutional heirs now stand outside the formal conversation: consulted episodically, bound by decisions they never accepted.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, council_minority_fathers, excluded,
    organized, biographical, identity_locked, global).

% Women religious and laywomen carried much of the reform's implementation — teaching, liturgical preparation, administration — while holding no vote at the council and no seat in the offices that later decide what the texts permit. They absorb the results of those decisions: suppressed congregations, narrowed ministries, reversed liturgical norms, all determined elsewhere.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, women_of_the_church, excluded,
    powerless, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__composite_overdetermination_reading, women_of_the_church, payer).

% The scholarly community reconstructing the council from diaries, commission archives, and voting records. Across competing schools, its findings converge on facts every reading must face: the components moved by different mechanisms, the votes differed document by document, and key ambiguities trace to deliberate drafting choices. This seat checks all readings, including the one this story instantiates.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, council_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__composite_overdetermination_reading, papal_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved a real collective-action problem: ratifying heterogeneous reforms across a globally distributed hierarchical institution without fracturing it. Near-unanimous votes on contested documents were achieved by drafting texts each major constituency could read its own way, and by packaging liturgical, ecumenical, ecclesiological, and church-state changes as a single reform so that approval of one carried approval of all. The retained ambiguities kept every faction inside the coalition at ratification.
% TRANSFER_FUNCTION: Moves interpretive discretion and disciplinary initiative upward. Factions commit labor and credibility to implementing their favored strand of the unsettled texts; the center retains the standing right to authorize, delay, or condemn any implementation after the fact. Deference, compliance, and when invoked formal submission flow to Rome; legitimacy and license to operate flow outward to factions whose implementations the center currently tolerates.
% ABSENT_VOICES: The minority fathers' heirs (traditionalist communities) are formally outside the conversation — consulted irregularly, bound by outcomes they rejected. Women held no vote at the council and hold no seat in the interpretive bodies that decide implementation. Orthodox and Protestant observers shaped drafts but left with no continuing seat. The lay faithful at large are spoken for rather than consulted.
% DISAPPEARANCE_RATIONALE: If the packaging dissolved overnight — every contested term defined, every ambiguity resolved in one direction — the coalition it holds together splits along the resolved lines: a traditionally-resolved settlement drives mass traditionalist separation healed only at enormous cost, a progressively-resolved one drives the mirror-image break. Liturgical practice, ecumenical relationships, and the internal career structure of the clergy all reorganize around whichever resolution landed. The arrangement's absence is unimaginable without rearrangement because the arrangement IS the coalition.
% FOUNDING_PROBLEM: How a global, two-millennium-old hierarchical church could confront mid-century modernity — religious pluralism, secular constitutional states, a liturgy distant from the baptized, divided Christianity — without shattering. Renewal demanded change; the tradition's claims demanded continuity; the council had to deliver both at once.
% FOUNDING_PROBLEM_CORROBORATION: Council historians across schools corroborate that the founding problem was real and acute — the preparatory-commission records and floor debates document genuine crisis. Ecumenical partners corroborate the disunity problem the ecumenical component addressed; sociologists of religion corroborate the secularization pressure. What no one outside the hierarchy attests is that the packaging must persist as a standing governance instrument now that the transition is complete — that attestation comes almost exclusively from curial sources, which is itself signal.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.64 is authored for the packaging arrangement itself, by this reading's own lights: the option-value mechanism is real, recurring, and lands on identifiable factions (1988, 2021), but a large coordination dividend — ratification unity, a schism apparently avoided in 1965, a shared vocabulary all parties still invoke — keeps the figure below dominance. Suppression 0.58 is a raw structural property, unscaled by power or scope: selective canonical discipline, regulatory reversal capacity, and career gatekeeping, not pervasive surveillance. Theater 0.33 reflects an interpretive industry (anniversary commemorations, hermeneutics conferences, synod texts that reproduce the ambiguity they purport to resolve) layered over a disciplinary function that remains real. Accessibility_collapse 0.48: alternatives persist — parallel communities, academic dissent channels, plain exit for the non-devout — but within the believing identity, alternatives to accepting the center's arbitration timing collapse. Resistance 0.62: fifty-plus years of organized traditionalist resistance and recurrent progressive dissent. CYCLICAL PATTERN: the suppression_requirement series oscillates rather than drifting monotonically — tolerance (early 1970s), tightening (late 1970s-1980s), peak enforcement (1988), détente (2005-2012), renewed tightening (2021). The oscillation is partly the extraction mechanism itself, operating as intermittent reinforcement: concessions (1988 indult, 2007 universal permission) induce factions to invest in regularized presence; reversals (2021) reclaim the value of that investment back to the center. This is not noise around a trend and is documented accordingly. Base_properties are measured at interval end (2025), on the post-reversal plateau. All three tracked series share one time grid (ten points, 1962-2025) so no metric row is sampled against another metric's end-state. CLAIM/METRIC INDEPENDENCE: claimed_type tangled_rope is asserted from the structure (genuine coordination function + asymmetric extraction + active enforcement); the metrics are authored from the recorded operation; neither was tuned toward the other or toward a predicted engine output.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as stewardship of a living tradition whose meanings mature gradually; the payer seats experience the same structure as moving goalposts — labor committed to an implementation that a later ruling can condemn. The sharpest divergence is between same-power payer seats: traditionalist_communities and progressive_theologians are clawed by opposite teeth of the same option, each punished for over-reading a different strand, which is why they rarely recognize a common interest. Inter-institutionally, national episcopates sat inside the perimeter (the Dutch catechism episode shows a whole conference disciplined for implementation), while ecumenical partners sit outside it and collect the opening's benefits without exposure. Same-power lateral divergence is illustrated by conciliar_generation_hierarchy versus traditionalist_communities: identical power and exit atoms, opposite structural roles — differentiation the engine can only derive from the beneficiary/victim declarations, not from power alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: the papal_magisterium and roman_curia sit nearest the beneficiary pole (the arrangement subsidizes their discretion and jurisdiction); the conciliar_generation_hierarchy collected its benefit biographically and is identity-locked into defending it; ecumenical_partner_churches, as mobile beneficiaries outside the enforcement perimeter, sit nearest the subsidy end of any seat. Payers derive high directionality, amplified toward the full-target end by identity_lock: traditionalist_communities (trapped between unacceptable acceptance and unthinkable exit), progressive_theologians (constrained by career dependence), ordinary_faithful (sacramental dependency). No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already produce the correct spread, and the one candidate case (the curia's dual position) is handled structurally via secondary_role rather than by overriding the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The packaging's founding mandate was transitional: hold the coalition while a bounded reform was ratified and assimilated. That mandate was substantially spent by roughly 1980 — yet the arrangement persisted and acquired a standing function (coalition maintenance through managed ambiguity) at rising extraction cost. This is mandatrophy in the moderated sense: the original mandate died, but a successor function occupies the structure, so the constraint is not yet a piton. It is decisively NOT a piton because a concentrated capturer exists — the gains demonstrably accrue to the papal_magisterium seat (hence gain_flow names it, not diffuse), and the administrator bears little of the cost that fixing would impose on others; a piton requires that no seat profits enough to maintain the structure. It is NOT a snare because the coordination function is genuine and large: the packaging really did solve the ratification problem, and the avoided-fracture dividend is not cover story but documented history. It is NOT a rope because the asymmetry is structural rather than overhead: factions cannot secure their implementations against later reversal, and that insecurity is the product the arrangement continuously supplies to the center. The tangled_rope verdict holds both truths the mono-type labels erase. COALITION CHECK: the arrangement's extraction is protected by divide-by-ambiguity — the payer seats are structurally played against each other (each hopes the center will favor its strand), so the usual coalition route by which powerless victims escape snares is precisely what the packaging forecloses; this is noted as a structural feature, not an oversight.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story is one reading of the kernel vatican_ii_doctrinal_authority — the composite_overdetermination_reading. Would a sibling reading (continuity_reading, rupture_progressive_reading, rupture_traditionalist_reading) relocate the arrangement''s beneficiaries and victims so completely that the family''s classifications diverge irreconcilably?',
    'Author the three sibling stories and compare per-seat classifications: continuity_reading locates costs mainly in misreaders rather than the taught; rupture_traditionalist_reading locates victims among the faithful receiving defective teaching; rupture_progressive_reading locates victims among the unreformed. Cross-reading comparison of victim sets resolves whether the kernel supports one classification or four.',
    'If sibling victim sets diverge as predicted, the kernel is genuinely multi-constraint and this story''s epsilon (authored only for the packaging arrangement) is the correct granularity; if they converge, the composite reading over-decomposes and a single-story treatment regains warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the four readings of the Vatican II kernel instantiate distinct constraints or one.').

omega_variable(
    component_epsilon_independence,
    'The composite reading holds that the liturgical, ecumenical, ecclesiological, and religious-liberty components each carry independent extractiveness. Does this story''s bundle-level epsilon (0.64, authored for the packaging arrangement itself) misattribute extraction between components — borrowing legitimacy for high-extraction components from low-extraction ones?',
    'Decompose: author separate component stories (liturgical reform reception, religious-liberty teaching, ecumenical method, collegiality governance), each with its own epsilon and stakeholder surface, linked back to this story via network edges; compare per-component effective extraction against the bundle-level figure.',
    'If component-level extraction varies widely (as the reading predicts), the bundle figure understates extraction in the most extractive component and overstates it in the least; the packaging itself would then be the residual extractor, and its classification could drop toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(component_epsilon_independence, conceptual, 'Whether bundle-level epsilon properly represents a composite whose components warrant independent measurement.').

omega_variable(
    ambiguity_intentionality,
    'Were the load-bearing ambiguities (the Lumen Gentium chapter 3 footnotes, the nota explicativa on collegiality, the dual readability of the religious-liberty declaration) deliberate drafting design, or emergent residue of irreconcilable positions?',
    'Drafting-history scholarship: commission archives, relatio texts, diaries (Congar, Chenu), and the documented handling of minority amendments. Deliberateness is established where drafters explicitly chose balancing formulations to preserve votes.',
    'Deliberate ambiguity strengthens the optionality account — the center knowingly holds a permanent interpretive option — and supports the tangled_rope verdict; emergent ambiguity weakens intentional-design framing and shifts weight toward the coordination-cost side of the ledger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_intentionality, empirical, 'Whether the retained ambiguities were designed instruments or compromise debris.').

omega_variable(
    hermeneutic_stabilization,
    'Can an authoritative hermeneutic (the reform-in-continuity program announced in 2005) actually fix the texts'' meanings and dissolve the optionality mechanism, or does every stabilization attempt itself become a new exercise of the option?',
    'Track faction-discipline incidents and cross-faction acceptance following authoritative interpretive interventions: if a stabilized reading reduces disciplinary episodes and holds across pontificates, stabilization works; the 2021 reversal of the 2007 liberalization is evidence it does not.',
    'Successful stabilization would drain the extraction mechanism and drift the arrangement toward rope; repeated failure confirms the option as structural and entrenches the tangled_rope verdict.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hermeneutic_stabilization, empirical, 'Whether authoritative interpretation can settle what the packaging was built to leave open.').

omega_variable(
    schism_counterfactual,
    'Would explicit resolution of the contested questions at the council itself have fractured the church (making the packaging the lesser evil whose extraction is partly the price of avoided schism)?',
    'Counterfactual analysis grounded in the minority''s documented floor strength (roughly a tenth to a seventh on the decisive votes), the depth of the positions involved, and comparison with councils that did force resolution and their aftermaths.',
    'If fracture was probable, a measurable share of the authored extraction is coordination cost rather than rent, and the tangled_rope balance shifts toward its rope pole; if the minority would have acquiesced, the packaging''s extraction stands more nakedly as optionality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(schism_counterfactual, conceptual, 'Whether the packaging''s extraction is partly the price of a schism avoided in 1965.').

omega_variable(
    suppression_mechanism_split,
    'Is the measured suppression (0.58) predominantly structural (canonical penalties, regulatory reversal capacity, career gatekeeping) or internalized (formation-shaped deference to magisterial interpretation among clergy and laity)?',
    'Post-defection trajectories: clergy who leave ministry, communities that accept irregular status, and laity who exit retention of deference patterns after the enforcing structure no longer reaches them indicates internalized share.',
    'If internalized share is high, effective suppression exceeds the structural measure — targets carry the deference with them, and exit-option ratings overstate real mobility; if low, removing the disciplinary machinery would rapidly lower realized suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized composition of the arrangement''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 1962, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(vati_tr_t1970, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(vati_tr_t1978, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1978, 0.22).
narrative_ontology:measurement(vati_tr_t1988, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1988, 0.28).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1995, 0.32).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(vati_tr_t2012, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2012, 0.36).
narrative_ontology:measurement(vati_tr_t2021, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2021, 0.34).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2025, 0.33).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1962, 0.25).
narrative_ontology:measurement(vati_be_t1965, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1965, 0.3).
narrative_ontology:measurement(vati_be_t1970, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(vati_be_t1978, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1978, 0.48).
narrative_ontology:measurement(vati_be_t1988, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1988, 0.6).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(vati_be_t2012, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2012, 0.52).
narrative_ontology:measurement(vati_be_t2021, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2021, 0.66).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2025, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1962, 0.15).
narrative_ontology:measurement(vati_su_t1965, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1965, 0.2).
narrative_ontology:measurement(vati_su_t1970, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(vati_su_t1978, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1978, 0.42).
narrative_ontology:measurement(vati_su_t1988, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1988, 0.68).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement(vati_su_t2012, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2012, 0.44).
narrative_ontology:measurement(vati_su_t2021, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2021, 0.62).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, rupture_traditionalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'Vatican II' decomposes, per the epsilon-invariance principle, into four reading-stories of one kernel plus prospective component stories. This story instantiates composite_overdetermination_reading; its epsilon (0.64) is authored ONLY for the unified-packaging arrangement with managed ambiguity — never averaged across components or readings. Sibling readings author different epsilons over the same kernel because they locate the arrangement differently: continuity_reading treats the texts as explications of prior teaching (low extraction; costs fall on misreaders); rupture_traditionalist_reading treats them as error-bearing (high extraction; victims among the taught faithful); rupture_progressive_reading treats them as a beachhead for further reform (moderate extraction; victims among the unreformed). The composite reading's distinctive structural claim is that these unitary assessments are malformed for a bundle whose components warrant different verdicts — hence the natural next decomposition is per-component stories (liturgical reform reception, religious-liberty teaching, ecumenical method, collegiality governance), each with its own epsilon and stakeholder surface, linked back to this packaging story. Upstream/downstream: the documentary record (drafting histories, voting patterns) upstream-feeds all four readings; this reading downstream-influences the siblings by changing what they must argue against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
