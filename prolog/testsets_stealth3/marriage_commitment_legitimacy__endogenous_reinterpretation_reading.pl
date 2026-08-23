% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__endogenous_reinterpretation_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
 *   human_readable: The Manifesto as Genuine Revelation — Endogenous Reinterpretation Reading (1890 Reversal of Plural Marriage)
 *   domain: religious/political_theology/commitment_systems
 *
 * SUMMARY:
 *   In September 1890, Wilford Woodruff issued an official declaration ending
 *   the solemnization of new plural marriages in the Church of Jesus Christ
 *   of Latter-day Saints, a practice the tradition had taught for nearly half
 *   a century as divinely mandated. This story instantiates ONE reading of
 *   that event — the endogenous reinterpretation reading — under which the
 *   Manifesto is genuine prophetic revelation: God Himself released His
 *   people from the principle to preserve the institutional vehicle of
 *   salvation for higher purposes, and monogamy is received as the next
 *   covenant stage rather than as a concession. Within this reading's own
 *   lights, the standing arrangement under contest (and the ε referent) is
 *   the obligation of members to treat the Manifesto as binding revelation
 *   and conform marriage practice accordingly; obedience to God is not
 *   extraction from members, so ε is authored low, and the federal
 *   anti-polygamy campaign figures as catalyst of timing, not cause of the
 *   command. KEY AGENTS (by structural relationship): -
 *   wilford_woodruff_first_presidency: Agenda setter
 *   (institutional/identity_locked) — receives, announces, and frames the
 *   ruling as revelation - quorum_of_twelve_apostles: Beneficiary with
 *   secondary agenda-setting (institutional/identity_locked) — sustains and
 *   administers enforcement - compliant_membership_majority: Primary
 *   beneficiary with real cost-bearing (organized/constrained) — sustains and
 *   rebuilds - pre_manifesto_plural_families: Primary cost-bearers
 *   (moderate/trapped) — existing households absorb stigma and legal jeopardy
 *   - principled_plural_marriage_dissenters: Refusers and later schismatics
 *   (powerless/constrained) — disciplined, excommunicated, exit into
 *   separatist communion - future_convert_generations: Downstream
 *   beneficiaries (powerless/mobile) — inherit the preserved, legally clear
 *   institution - divine_authority: Non-agent seat retained for completeness
 *   (agent:false) — the party to whom all covenant obedience is directed in
 *   this frame. FAMILY NOTE: this file is one of three linked readings of the
 *   kernel marriage_commitment_legitimacy. The ε values differ sharply by
 *   design: this reading authors low extractiveness (~0.26) because the
 *   command's origin is divine; the exogenous_override sibling authors high
 *   extractiveness over the same referent because the command's origin is
 *   statutory coercion; the hybrid_pragmatic sibling authors intermediate
 *   values under a strategic-adaptation frame. The upstream reading in
 *   faith-community discourse is this one (it supplies the official account
 *   cited from the pulpit); the critical historiography downstream cites the
 *   exogenous file.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.26).
domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.42).
domain_priors:theater_ratio(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "The Manifesto as Genuine Revelation — Endogenous Reinterpretation Reading (1890 Reversal of Plural Marriage)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "religious/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'cb91f54c-9441-469c-b8e8-25f2905461ee').
narrative_ontology:cs_kernel_codification('cb91f54c-9441-469c-b8e8-25f2905461ee', fixed_text).
narrative_ontology:cs_authority_grounding('cb91f54c-9441-469c-b8e8-25f2905461ee', lineage).
narrative_ontology:cs_interpretation_layer_present('cb91f54c-9441-469c-b8e8-25f2905461ee').
narrative_ontology:cs_reading_relation('cb91f54c-9441-469c-b8e8-25f2905461ee', marriage_commitment_legitimacy__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('cb91f54c-9441-469c-b8e8-25f2905461ee', marriage_commitment_legitimacy__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('cb91f54c-9441-469c-b8e8-25f2905461ee', foundational, manifesto_is_genuine_divine_command).
narrative_ontology:cs_axiom_status(manifesto_is_genuine_divine_command, holdable).
narrative_ontology:cs_axiom_grounding('cb91f54c-9441-469c-b8e8-25f2905461ee', manifesto_is_genuine_divine_command, theological).
narrative_ontology:cs_axiom('cb91f54c-9441-469c-b8e8-25f2905461ee', secondary, covenant_requirements_progress_by_dispensation).
narrative_ontology:cs_axiom_status(covenant_requirements_progress_by_dispensation, holdable).
narrative_ontology:cs_axiom_grounding('cb91f54c-9441-469c-b8e8-25f2905461ee', covenant_requirements_progress_by_dispensation, theological).
narrative_ontology:cs_reference_frame('cb91f54c-9441-469c-b8e8-25f2905461ee', continuing_revelation_covenant_frame).
narrative_ontology:cs_drift_state('cb91f54c-9441-469c-b8e8-25f2905461ee', contemporary_post_1978_revelation_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cb91f54c-9441-469c-b8e8-25f2905461ee', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, quorum_of_twelve_apostles).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, compliant_membership_majority).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, future_convert_generations).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, pre_manifesto_plural_families).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, principled_plural_marriage_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, compliant_membership_majority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives and publicly announces what he presents as God's command ending new plural marriages, framing the reversal as divine mercy preserving the Church for higher purposes. Has taught, practiced, and staked decades of sacrifice on the principle being reversed, and now must present its suspension as revelation rather than negotiation. His authority consists in being the channel of such commands, so recasting the announcement as concession is not available to him from inside the office.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, wilford_woodruff_first_presidency, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Collectively sustains the ruling and, especially after 1904, administers its enforcement through disciplinary councils against continued plural marriage. The arrangement preserves the apostolic office's claim to speak for God across reversals — legitimacy capital the office collects each time a reversal is accepted as revelation. Several initially resisted before sustaining; exiting the arrangement would mean repudiating the ordination charge that constitutes their office.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, quorum_of_twelve_apostles, beneficiary,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, quorum_of_twelve_apostles, agenda_setter).

% Rank-and-file saints sustain the declaration in general conference and rebuild domestic life around monogamy. They receive legal safety, restored corporate property prospects, eventual statehood, and continued covenant standing. A large minority among them had sunk real investment into the revoked principle — plural households of their own, sealed relatives, decades of sacrificial giving — and they surrender that investment without compensation. Exit would mean abandoning the community that constitutes their social world and, in their own accounting, their eternal one.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, compliant_membership_majority, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, compliant_membership_majority, payer).

% Existing plural households — wives, husbands, and children of sealings contracted before the reversal — keep covenants their doctrine forbids dissolving. They absorb social stigma, continued legal jeopardy under post-Manifesto cohabitation enforcement, and the quiet demotion of the principle their family structure was built on. No exit exists short of dissolving sealed families, which their theology forbids; they carry the arrangement's costs involuntarily and permanently.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, pre_manifesto_plural_families, payer,
    moderate, generational, trapped, regional).

% Members who conclude the principle remains eternally binding and refuse the new standard. They face escalating ecclesiastical discipline culminating in excommunication after 1904, forfeiting fellowship, temple access, and communal belonging. Some relocate to colonies in Mexico or Canada; others later form separate fundamentalist communions. Exit into schism is physically available but severance of family, inheritance, and salvific community makes it ruinous rather than liberating.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, principled_plural_marriage_dissenters, payer,
    powerless, civilizational, constrained, regional).

% Converts joining after the transition inherit a monogamous church with unclouded legal status, enabling sustained growth in Europe, the Pacific, and Latin America. They bear none of the transition cost, receive the preserved institution entire, and their arrival retroactively supplies the concrete content of the 'higher purposes' the frame invokes. Their mobility is real: nothing binds them to the arrangement except conviction.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, future_convert_generations, beneficiary,
    powerless, generational, mobile, global).

% Non-agent seat retained for completeness of the frame: within this reading, God is the party whose command the Manifesto transmits, whose purposes the reversal serves, and to whom all covenant obedience rendered under the arrangement is directed. Listed so the reading's own account of where obedience flows is recorded; contributes nothing to the directional arithmetic.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, divine_authority, beneficiary,
    powerful, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, divine_authority).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, diffuse).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves, by a single binding ruling, the collective crisis of a covenant people whose marriage law had become legally untenable and institutionally fatal: it reorients the whole community's marriage practice simultaneously, ends the escalating federal conflict over the practice, secures the corporate and territorial future of the institution, and relocates the community's covenant identity from a revoked commandment to a new stage — without requiring each family to renegotiate separately.
% TRANSFER_FUNCTION: Moves covenant conformity from the membership upward: plural-marriage practice, the status attached to it, and prior sacrificial investment in the principle are surrendered to the divine will as mediated by the prophetic office. What flows back down is institutional survival, legal safety, statehood, and — on the frame's own accounting — continued access to the ordinances of salvation through a church whose authority to reverse course is thereby reaffirmed rather than diminished.
% ABSENT_VOICES: The plural wives of pre-existing sealings had the least voice: the ruling was announced from the head of the hierarchy and ratified by common consent after the fact, with no independent vote in which the households bearing its sharpest costs could register dissent. Conscientious objectors spoke only through the disciplinary process that adjudicated them. Post-hoc, the fundamentalist communities claim the reversal was never legitimately consented to; within this reading's frame their objection is apostasy rather than standing, which is precisely the move the absent-voices check exists to notice.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight — if members woke no longer bound to treat the Manifesto as revelation — the covenant framework would rearrange immediately: the apostolic office's claim to bind and loose across reversals would collapse or require reconstruction, the monogamous settlement would lose its theological ground and reopen as negotiable practice, the fundamentalist schisms would lose the apostasy-marker that defines them, and the international church built on the post-1890 settlement would inherit an unresolved doctrinal wound. The arrangement is load-bearing for the succession legitimacy this reading exists to maintain.
% FOUNDING_PROBLEM: How can a covenant people obey a revoking revelation — a God who once commanded plural marriage now releasing them from it — without destroying confidence that continuing revelation is genuine rather than improvised under pressure? The proximate trigger was existential: confiscatory federal legislation, imprisoned leadership, and the prospect of the church's legal dissolution; the deeper problem was making reversal compatible with the doctrine of an unchanging God working through dispensations.
% FOUNDING_PROBLEM_CORROBORATION: The founding crisis is corroborated strongly from outside the benefiting parties: federal statutes and Supreme Court litigation of the late 1880s, congressional debates, and contemporaneous secular press all attest the church faced legal dissolution. That the resolution was revelatory rather than coerced is attested almost exclusively from within the tradition — the president's own epistles and the sustaining votes of those the arrangement preserves — and this asymmetry is stated plainly rather than smoothed over. No source outside the beneficiary set confirms the divine origin claim; the reading accepts that burden as constitutive of faith rather than resolving it.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.26, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).
:- end_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (ending at 0.26) because, on this reading's lights, what the arrangement moves is covenant conformity owed to God, not rents owed to men; the residual ε reflects real, unevenly distributed transition costs — plural households demoted without compensation, refusers cut off — which even a sympathetic internal account concedes. Suppression (0.42) is structural: ecclesiastical councils, temple-recommend withdrawal, and post-1904 excommunication carried the enforcement load once federal prosecution ended, though this reading classifies that machinery as administration of a command rather than suppression of preference. Theater is authored very low (0.08): on this reading the revelatory framing is sincere, and the modest rise during 1890–1904 reflects the documented gap between public denial and privately tolerated new sealings, resolved by the 1904 crackdown. Resistance is substantial (0.55) — dissent was real, persistent, and produced lasting schismatic communities — and accessibility_collapse is partial (0.60) because exits into Mexico/Canada colonies and separatist sects remained open at high cost. All three tracked series run on one shared time grid (1890, 1895, 1900, 1904, 1910, 1920, 1935); extractiveness and suppression peak together at the 1904 Second Manifesto enforcement intensification, then decay as the monogamous generation replaces the plural-marriage cohort. The claimed type (rope) is stated from this reading's seat and is deliberately NOT reconciled to the metrics: seats occupied by the dissenters or the plural families may compute tangled-rope or worse from the same structural data, and that divergence is the measurement.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and apostolic seats experience the arrangement as covenant governance: the same announcement that reads as mercy and continuity from the First Presidency reads as dispossession from the plural-family seat and as betrayal from the dissenter seat. The compliant majority sits nearest the beneficiary pole — they traded a principle most had never practiced for legal safety and institutional permanence — while the trapped plural households bear costs they cannot exit and the dissenters convert conscience into excommunication. An analytical observer (historians inside and outside the tradition) sees the full structure: a genuine collective-action solution whose costs concentrate on a minority that did not consent to the reversal. The engine computes these per-seat divergences from the structural data; this file's rope claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: the apostolic quorum (d near 0.1) maintains the office's claim to bind and loose across reversals; the compliant majority collects legal safety, restored corporate standing, and covenant continuity; future converts inherit the preserved institution at zero transition cost. Targets derive high directionality: trapped plural families sit near the full-target pole (their costs are unavoidable and uncompensated), and dissenters near it (their refusal purchases total loss of fellowship). The compliant majority carries a deliberate override (d = 0.35 rather than the near-zero a pure beneficiary declaration yields) because the derivation cannot see that a large fraction of that 'majority' had sunk family, status, and sacrificial investment into the very principle being revoked — they are net beneficiaries with substantial unpaid cost-bearing, and the flat derivation would launder that asymmetry. Divine authority is authored as a non-agent seat (agent: false) so the frame's claim that obedience flows to God is recorded for completeness without feeding d -> chi arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate is live under this reading: preserving the institutional church through adaptive revelation is an ongoing function, not an outlived one, so no mandatrophy is declared and the founding-problem status is authored live. The classification discipline cuts both ways here. Against mislabeling-as-extraction: a purely external audit that sees only coercion and confiscated practice will score this a snare; authoring the reading's own structural data (sincere frame, diffuse gains, low theater) keeps the coordination function legible and lets the engine distinguish covenant governance from rent collection. Against laundering-as-coordination: the trap is the mirror image — a committed seat can dress concentrated, uncompensated costs on plural families and dissenters as universal sacrifice. The victim declarations and the directionality override on the majority exist precisely so the engine can price that asymmetry rather than accept the frame's own accounting. If the mandate ever atrophies — if the arrangement persists mainly as liturgical memory of 1890 rather than as operative covenant governance — the mismatch consumer (dead founding problem + world_rearranges verdict) flags it for the piton/theater path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (endogenous_reinterpretation_reading) of the contested kernel marriage_commitment_legitimacy — do the authored epsilon, victim set, and classification hold only under this reading?',
    'Cross-reading comparison against the sibling files (marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading): each reading authors its own epsilon over the same referent; convergence of computed types across readings would indicate reading-invariant structure, divergence confirms the classification is reading-indexed.',
    'Each sibling changes the structural picture materially: the exogenous reading expands the victim set to all members whose doctrine was overridden without consent and raises epsilon sharply; the hybrid reading relocates the agenda-setter seat to institutional strategy managers and introduces deliberate scope ambiguity as an operating feature. Folding the readings into one story would average away exactly the divergence the corpus exists to measure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this file instantiates one of three sibling readings; siblings alter victim sets, epsilon, and the agenda-setter seat.').

omega_variable(
    origin_of_authority_disagreement_locus,
    'Where exactly do the sibling readings disagree — the causal origin of the reversal command (divine mind versus federal statute), and therefore who is its author and what counts as obedience versus capitulation?',
    'Woodruff''s contemporaneous diaries and public epistles weighed against federal legislative and judicial records and the timing of enforcement escalation; internal consistency of the revelatory account with the sequence of coercive events.',
    'If the origin is divine, the arrangement is covenant adaptation and this file''s low-extraction profile stands; if statutory, the arrangement is capitulation, the victim set expands to the whole consenting-under-duress membership, and the classification migrates toward the exogenous sibling''s high-extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(origin_of_authority_disagreement_locus, empirical, 'The readings divide on the causal origin of the command — the single structural element that fixes epsilon and the victim set.').

omega_variable(
    post_manifesto_private_authorization_ambiguity,
    'Were the new plural marriages solemnized between 1890 and 1904 unauthorized local aberrations, or continuations tolerated or authorized at senior levels pending full implementation?',
    'Archival recovery: presidential authorization records, temple and sealing registers, and disciplinary case files from the 1904-1910 crackdown, cross-checked against apostolic journals.',
    'If authorized, the interim theater ratio is understated and the suppression buildup began earlier than the 1904 peak; if aberrant, the Second Manifesto was corrective housekeeping rather than a ratchet, which supports this reading''s sincerity frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_manifesto_private_authorization_ambiguity, empirical, 'Status of 1890-1904 new sealings determines whether the pre-1904 theater bump was hypocrisy or housekeeping.').

omega_variable(
    discipline_as_correction_or_alternative_suppression,
    'Is post-1904 ecclesiastical discipline against continued plural marriage intra-covenant correction (consistent with the rope claim) or suppression of a religious alternative (which would give the dissent seats snare-flavored structure)?',
    'Conceptual: compare disciplinary treatment of this abandoned practice with treatment of other superseded practices in the same tradition. Empirical supplement: track whether dissenters retained property, family ties, and speech rights after exit, and whether the schismatic communities were persecuted beyond formal excommunication.',
    'If the discipline functions as alternative-suppression, the dissenter seat computes nearer the full-target pole and the aggregate type shifts toward tangled_rope or snare despite the reading''s low authored extraction; if correction, the low-extraction profile holds and the rope claim is defensible at most seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discipline_as_correction_or_alternative_suppression, conceptual, 'Whether enforcement of the reversal is covenant correction or suppression of a live religious alternative.').

omega_variable(
    identity_lock_of_prophetic_office,
    'How much of the arrangement''s persistence rests on identity fusion of the presiding officers with the revelatory channel itself — such that no occupant could classify the 1890 episode as anything but revelation without unmaking the office?',
    'Counterfactual test: examine episodes where the tradition revised non-foundational teachings administratively without revelatory framing; if the same offices routinely reframe without invoking revelation, the lock is weaker than assumed and strategic reframing (the hybrid sibling''s terrain) becomes available from inside.',
    'If identity lock is strong, the agenda-setter and apostolic seats are structurally unable to author the exogenous reading regardless of evidence, stabilizing this constraint beyond what its coordination function alone would support; if weak, the arrangement''s persistence depends more on ordinary coordination benefits and less on office constitution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_of_prophetic_office, empirical, 'Strength of identity fusion between the prophetic office and the genuineness of its reversals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 1890, 1935).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement(marr_tr_t1895, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1895, 0.14).
narrative_ontology:measurement(marr_tr_t1900, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1900, 0.17).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1904, 0.11).
narrative_ontology:measurement(marr_tr_t1910, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1910, 0.1).
narrative_ontology:measurement(marr_tr_t1920, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1920, 0.09).
narrative_ontology:measurement(marr_tr_t1935, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1935, 0.08).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.34).
narrative_ontology:measurement(marr_be_t1895, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1895, 0.36).
narrative_ontology:measurement(marr_be_t1900, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1900, 0.37).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1904, 0.4).
narrative_ontology:measurement(marr_be_t1910, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1910, 0.35).
narrative_ontology:measurement(marr_be_t1920, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1920, 0.3).
narrative_ontology:measurement(marr_be_t1935, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1935, 0.26).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.45).
narrative_ontology:measurement(marr_su_t1895, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1895, 0.5).
narrative_ontology:measurement(marr_su_t1900, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1900, 0.52).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1904, 0.62).
narrative_ontology:measurement(marr_su_t1910, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1910, 0.58).
narrative_ontology:measurement(marr_su_t1920, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1920, 0.5).
narrative_ontology:measurement(marr_su_t1935, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1935, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Manifesto' decomposes into three structurally distinct constraints corresponding to three readings of one kernel (marriage_commitment_legitimacy), per the epsilon-invariance principle. The observable that varies across readings — the causal origin attributed to the reversal command — changes epsilon, the victim set, and the agenda-setter seat, so measuring one reading with another's assumptions would violate epsilon invariance. The readings are linked bidirectionally through network edges: this endogenous reading is upstream (the official account whose legitimacy the other two contest and whose evidentiary standards the hybrid reading reshapes); the exogenous reading is downstream in critical historiography. Each file carries this note and links its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
