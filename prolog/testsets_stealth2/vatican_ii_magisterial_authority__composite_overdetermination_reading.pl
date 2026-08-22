% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Assent Regime as Overdetermined Composite Corpus (Composite-Overdetermination Reading)
 *   domain: religious/ecclesiological/institutional-history
 *
 * SUMMARY:
 *   Under the composite-overdetermination reading, the operative arrangement
 *   is the post-conciliar assent regime: clergy, theologians, religious
 *   institutes, and the faithful are bound to treat the sixteen conciliar
 *   documents as binding magisterial teaching, while the documents themselves
 *   encode mutually incompatible ecclesiological visions in deliberately
 *   balanced formulations — the church 'subsisting in' the Catholic Church,
 *   collegiality qualified by primatial reservation, religious liberty
 *   affirmed without settling its pre-conciliar contradiction. Because the
 *   letter underdetermines doctrine, the regime's daily operation runs
 *   through hermeneutical control: whoever promulgates the authoritative
 *   reading collects the assent of the whole body. The arrangement solved a
 *   real coordination problem — it obtained supermajority votes in 1962-65
 *   precisely by letting opposed majorities each read their victory into the
 *   same text, and it has held a global communion together through six
 *   decades of internal dispute — while transferring the unresolved dispute's
 *   costs onto the seats least able to refuse: theologians censurable from
 *   either direction, traditionalist communities carrying standing canonical
 *   penalties, and successive generations of the faithful formed under
 *   swinging interpretations. The recurring 10-12 percent negative votes on
 *   contested schemata are read here as the measurable residue of
 *   incompatibility the final texts embedded rather than resolved. Claim and
 *   metrics are authored independently: the claimed type states the structure
 *   this reading finds (genuine coordination plus asymmetric extraction under
 *   active enforcement); the metrics describe observed operation across the
 *   interval.
 *
 * KEY AGENTS:
 *   - roman_papal_magisterium: Primary beneficiary and agenda-setter (institutional/arbitrage) — collects assent through control of authoritative interpretation; can redefine the corpus's meaning at will, spending legitimacy with one flank per reversal
 *   - congregation_for_doctrine: Enforcement arm (institutional/constrained) — administers investigations, reviews, and canonical processing that keep both flanks inside authorized readings
 *   - postconciliar_reform_establishment: Secondary beneficiary (institutional/constrained) — implemented the ambiguous mandates expansively during liberalization phases and holds positions predicated on the reform continuing
 *   - catholic_systematic_theologians: Primary target (moderate/identity_locked) — navigates the ambiguity professionally; careers have ended in both directions
 *   - traditionalist_canonical_communities: Primary target (organized/identity_locked) — carry suspensions, excommunications, and irregular status; refuse the exit their position implies
 *   - ordinary_parish_faithful: Diffuse target with secondary benefit (powerless/constrained) — absorb generational swings in catechesis and liturgy while receiving the renewal's concrete goods
 *   - ecumenical_dialogue_partners: Excluded party (organized/mobile) — bound by interpretive outputs they have no seat in producing
 *   - council_historians: Analytical observer (analytical/analytical) — see from the acta which ambiguities were inserted deliberately and which objections were accommodated rather than answered
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.66).
domain_priors:suppression_score(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.72).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__composite_overdetermination_reading, "Vatican II Assent Regime as Overdetermined Composite Corpus (Composite-Overdetermination Reading)").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__composite_overdetermination_reading, "religious/ecclesiological/institutional-history").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__composite_overdetermination_reading, '18e546c3-7be5-49c3-9931-54ca8ea618f7').
narrative_ontology:cs_kernel_codification('18e546c3-7be5-49c3-9931-54ca8ea618f7', fixed_text).
narrative_ontology:cs_authority_grounding('18e546c3-7be5-49c3-9931-54ca8ea618f7', extraction).
narrative_ontology:cs_interpretation_layer_present('18e546c3-7be5-49c3-9931-54ca8ea618f7').
narrative_ontology:cs_reading_relation('18e546c3-7be5-49c3-9931-54ca8ea618f7', vatican_ii_magisterial_authority__vatican_ii_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('18e546c3-7be5-49c3-9931-54ca8ea618f7', vatican_ii_magisterial_authority__vatican_ii_rupture_reading, coexists_with).
narrative_ontology:cs_axiom('18e546c3-7be5-49c3-9931-54ca8ea618f7', foundational, composite_encoding_via_deliberate_ambiguity).
narrative_ontology:cs_axiom_status(composite_encoding_via_deliberate_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding('18e546c3-7be5-49c3-9931-54ca8ea618f7', composite_encoding_via_deliberate_ambiguity, empirically_contingent).
narrative_ontology:cs_axiom('18e546c3-7be5-49c3-9931-54ca8ea618f7', foundational, hermeneutical_control_constitutes_authority).
narrative_ontology:cs_axiom_status(hermeneutical_control_constitutes_authority, holdable).
narrative_ontology:cs_axiom_grounding('18e546c3-7be5-49c3-9931-54ca8ea618f7', hermeneutical_control_constitutes_authority, conventional).
narrative_ontology:cs_reference_frame('18e546c3-7be5-49c3-9931-54ca8ea618f7', ambiguity_brokered_supermajority_corpus).
narrative_ontology:cs_drift_state('18e546c3-7be5-49c3-9931-54ca8ea618f7', contemporary_post_traditionis_custodes, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('18e546c3-7be5-49c3-9931-54ca8ea618f7', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, roman_papal_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, postconciliar_reform_establishment).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, catholic_systematic_theologians).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_canonical_communities).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, ordinary_parish_faithful).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, congregation_for_doctrine).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, ordinary_parish_faithful).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the authoritative interpretations that determine what the conciliar documents require — encyclicals, apostolic exhortations, curial instructions, and disciplinary acts settling contested passages case by case. Collects the assent of clergy and faithful to whatever reading it promulgates, and periodically reverses course (liberalizing access to the older liturgy in 2007, restricting it again in 2021) without conceding that the underlying texts were ever unclear. Its exit from the arrangement is effectively unlimited — it can redefine the corpus's meaning at will — though each reversal spends legitimacy with one flank or the other.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, roman_papal_magisterium, agenda_setter,
    institutional, generational, arbitrage, global).

% Administers the enforcement side: investigates theologians whose writings resolve the ambiguity in unauthorized directions, reviews religious institutes for conciliar fidelity, and processes the canonical cases of groups that reject the corpus outright. Its caseload rises whenever either flank presses the texts' plain implications too far, and its institutional standing depends on the contest remaining unresolved enough to require adjudication.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, congregation_for_doctrine, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__composite_overdetermination_reading, congregation_for_doctrine, beneficiary).

% The network of bishops' conference bureaucracies, liturgy commissions, and theological faculties that carried out the conciliar reforms. During liberalization phases it implemented the ambiguous mandates expansively — vernacular liturgies, collegial structures, interfaith dialogue — treating the letter as a floor rather than a ceiling. It retains institutional positions, faculties, and publishing infrastructure predicated on the reform continuing, and loses ground in each correction phase.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, postconciliar_reform_establishment, beneficiary,
    institutional, generational, constrained, global).

% Work professionally inside the ambiguity: every significant question — collegiality, religious liberty, the non-Christian religions, liturgical reform — admits readings the center will eventually punish in one direction or the other. Careers have ended both ways: censured for excessive novelty in the 1970s and 1980s, and more recently marginalized for excessive attachment to pre-conciliar formulations. Leaving the profession means losing ordination-linked employment, community, and vocation at once; most navigate instead, and their professional identity is fused with the institution whose penalties they risk.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, catholic_systematic_theologians, payer,
    moderate, biographical, identity_locked, global).

% Communities that read the corpus's discontinuous strand as disqualifying and refuse the reform's legitimacy in whole or in part. They carry suspensions, excommunications, and irregular canonical status; their sacramental life persists in a tolerated-or-suppressed gray zone that tightens or loosens with each pontificate. Their self-understanding as preserving the Church rather than leaving it prevents the exit their position otherwise implies, and their organizational discipline makes them the most resistant seat in the arrangement.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, traditionalist_canonical_communities, payer,
    organized, generational, identity_locked, global).

% Receive catechesis and liturgy that shifted generationally — Latin to vernacular, and in some places back again — with little voice in which reading governs their parish in a given decade. They bear the cumulative cost of contradictory formation across generations while also receiving the renewal's concrete goods: vernacular worship, lay roles in the liturgy, and softened relations with other Christians. Quiet disaffiliation is their main lever, exercised at the cost of family and cultural ties.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, ordinary_parish_faithful, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__composite_overdetermination_reading, ordinary_parish_faithful, beneficiary).

% Protestant and Orthodox partners whose agreements with Rome cite specific conciliar passages. When the center's interpretation swings, settled dialogues are reopened unilaterally; they have no seat in the interpretive process whose outputs bind them, and they calibrate their engagement accordingly, holding agreed texts loosely because the ground beneath them moves.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, ecumenical_dialogue_partners, excluded,
    organized, generational, mobile, global).

% Scholars working from the council's drafts, relatio, and voting records. They can see which ambiguities were inserted deliberately, which minority objections were accommodated rather than answered, and how the final texts were assembled for vote maximization. They publish findings that cut against every faction's preferred memory of the event, but hold no power over implementation.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__composite_overdetermination_reading, council_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__composite_overdetermination_reading, roman_papal_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Held a globally distributed communion of roughly a billion members together through a doctrinal transition by giving every faction a text it could sign: the supermajority votes of 1962-65, and the six decades of shared institutional life since, were coordinated by a corpus each major coalition could read as its own victory.
% TRANSFER_FUNCTION: Moves doctrinal assent and institutional obedience from clergy, theologians, religious institutes, and the faithful to the interpretive center — and, during liberalization phases, moves implementation discretion outward to episcopal conferences and liturgical commissions; the disputed substance itself is moved nowhere, being deferred indefinitely.
% ABSENT_VOICES: The conciliar minority's stated objections survive in the acta, but their heirs — the traditionalist communities — sit outside the interpretive conversation, consulted episodically and penalized when they insist on their reading. The progressive dissenters of the 1970s were likewise silenced once their readings outlived their usefulness to the center. Both flanks would object that the ambiguity is resolved unilaterally by the seat that collects from the resolution; the ecumenical partners would object that agreements citing the texts are reopened without their consent.
% DISAPPEARANCE_RATIONALE: If the assent regime vanished overnight, the communion would partition along the fault lines the corpus encodes: traditionalist communities would regularize or formalize separation, progressive jurisdictions would entrench divergent practice, liturgical usage would regionalize within a decade, and the papal office's doctrinal role would be renegotiated outright. The arrangements of every named seat depend on the regime's continued operation.
% FOUNDING_PROBLEM: The council was convened to renew the Church's engagement with the modern world while preserving internal unity; the immediate drafting-stage problem was achieving supermajority agreement among mutually incompatible ecclesiological visions — neo-scholastic centralizers against ressourcement and decentralizing majorities — without producing a schism.
% FOUNDING_PROBLEM_CORROBORATION: Council historians outside the benefiting parties — the Bologna school's history-of-the-councils project and comparable archival work — corroborate the vote-engineering genealogy from the draft histories, relatio, and roll-call records; the minority fathers' published objections corroborate the embedded incompatibility from outside the winning coalition. No attestation is fully independent of the parties: the archive itself is curated by the benefiting institution, and that residual dependence is itself signal.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.66 at interval end because the regime transfers real assent and obedience upward while deferring the substantive questions indefinitely — the transfer is substantial but bounded by the corpus's genuine teaching content and by the periodic liberalizations. Suppression is 0.72 because persistence depends on active enforcement machinery (doctrinal investigations, institute reviews, canonical penalties, liturgical mandates), not on voluntary preference. Theater is 0.38: anniversary narrations of unity, irreversibility affirmations, and unanimity commemorations are performed while implementation visibly diverges, but the underlying coordination is real, capping theatricality below the piton range. Accessibility_collapse is 0.58: once the regime is understood, open dissent and parallel magisteria collapse quickly, yet exit (schism, disaffiliation) remains available at existential cost, so alternatives are only partly closed. Resistance is 0.64: sustained flanking pressure from both directions for six decades, punctuated by the historical minority votes. The temporal series run on one shared seven-point grid (1965-2025) with all three metrics authored at every point. The series are cyclical, not monotonic: liberalization phases (early 1970s, late 1990s-2000s) let the reform establishment exploit the ambiguity expansively; correction phases (late 1970s-1980s censures and excommunications, post-2021 restrictions) reassert hermeneutical control; detente follows each correction until accumulation resumes. Roughly fifteen-to-twenty-year half-cycles. The oscillation is partly the extraction mechanism itself — intermittent reinforcement keeps both flanks invested in courting the center rather than exiting or resolving — and partly a side effect of pontificate turnover. End-state scalar values were sampled at a re-tightening phase (post-2021), so they sit near a suppression peak rather than a trough.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the papal seat the corpus is an instrument it wields: a coordination structure it built, maintains, and can redefine — the arrangement looks rope-like from there. From the theologian seat it is a censure lottery in which every significant question eventually punishes one resolution or the other, with exit priced as loss of vocation, community, and livelihood — identity fusion with conciliar obedience makes departure unthinkable for most even when penalties lapse. From the traditionalist seat it is a standing penalty regime whose severity tracks the pontifical calendar. From the faithful seat it is weather: generational swings in liturgy and catechesis arriving without a lever to pull. The engine computes these per-seat classifications from the structural data (power, exit options, role declarations); this story authors the data and the claim, not the verdicts.
 *
 * DIRECTIONALITY LOGIC:
 *   The papal magisterium and the reform establishment are declared beneficiaries and derive directionality near the beneficiary end: assent flows to them, and the reform establishment collected implementation discretion during liberalization phases. Theologians, traditionalist communities, and the faithful are declared victims and derive directionality near the target end, amplified for the identity-locked seats (theologians, traditionalists) whose exit is fused with vocation or self-understanding. One override is authored: ordinary_parish_faithful (powerless) is overridden to d=0.65 because the bare derivation from victim declaration plus constrained exit would place them near full target, overstating their net position — they simultaneously receive the renewal's concrete goods (vernacular worship, lay roles, softened ecumenical relations), making them moderately net-payers rather than pure targets. The institutional beneficiaries are left at their derived low directionality despite the center's real enforcement burden and legitimacy costs, because its net structural position remains decisively on the collecting side of the transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — obtaining supermajority agreement among incompatible ecclesiological visions without schism — was solved in 1965 and stayed solved: the vote was won. What persists is a successor function: communion maintenance plus collection of assent through interpretive control, with the original brokering ambiguity now the asset being administered rather than a means to an end. The founding_problem_status is authored 'contested' rather than 'dead' because the unity problem the texts brokered remains live — the composite deferred the incompatibility rather than resolving it, and each enforcement cycle re-litigates it. The classification guards against both mislabels: a pure-extraction reading would erase the real coordination (sixty years of institutional cohesion that neither flank destroyed), and a pure-coordination reading would erase the asymmetric transfer (the dispute's costs land on the seats least able to refuse while interpretive authority accrues to the center). If a future pass judges the founding problem dead while the world still rearranges around the arrangement, the mismatch flag fires — this story's contested status is the honest current reading, not a tuned one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the composite_overdetermination_reading of the vatican_ii_magisterial_authority kernel. Would the same corpus modeled under the continuity_reading (organic development, no encoded rupture) or the rupture_reading (decisive break with the pre-conciliar inheritance) yield a different constraint with a different epsilon and a different victim set?',
    'Not resolvable by data alone — the readings are alternative framings of one corpus. Partial resolution via archival vote-pattern and draft-provenance studies: documented substitution of clear formulations for votable ambiguous ones at identifiable drafting stages materially strengthens the composite framing against both siblings.',
    'Under the continuity_reading the arrangement computes nearer pure coordination (low epsilon, no encoded asymmetry); under the rupture_reading the victim set expands toward the entire pre-conciliar inheritance and epsilon rises. This reading locates extraction in hermeneutical control, invariant across which strand currently prevails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame omega: this file is one reading of a three-reading kernel; sibling readings are separate constraints, not hedges inside this one.').

omega_variable(
    deliberate_vs_structural_ambiguity,
    'Was the corpus''s ambiguity a deliberate vote-maximizing instrument serving identifiable beneficiaries, or the unavoidable residue of doctrinal language under irreducible pluralism?',
    'Draft-history forensics: compare rejected clear formulations against adopted ambiguous ones in the Theological Commission minutes and the relatio; documented replacement of clarity with votability at recorded drafting stages indicates deliberate construction.',
    'If deliberate, the ambiguity is a designed extraction instrument and the beneficiary declarations stand as written; if structural, part of measured extractiveness is the irreducible price of doctrinal speech under pluralism and effective extraction drops accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deliberate_vs_structural_ambiguity, empirical, 'Whether the encoding ambiguity was engineered for vote maximization or is an inherent property of doctrinal language under pluralism.').

omega_variable(
    hermeneutical_capture_durability,
    'Does the gain from interpretive control accrue durably to the papal center, or oscillate between factions (center, reform establishment, episodic toleration regimes) such that no seat durably captures?',
    'Track interpretive reversals across pontificates (the 2007 liberalization of the older liturgy versus the 2021 restriction) and measure which institutions retained positions, faculties, and publishing infrastructure across both phases.',
    'Durable center-capture confirms the named gain-flow seat; oscillating capture pushes the arrangement toward diffuse gains and raises the question of why it persists when no seat durably profits from maintaining it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutical_capture_durability, empirical, 'Durability of capture at the interpretive center versus oscillating factional capture.').

omega_variable(
    minority_vote_signal,
    'Do the recurring 10-12 percent negative votes on contested schemata measure transient coalition friction, or irreducible theological incompatibility that the final texts embed without resolving?',
    'Match each minority relatio objection to its treatment in the promulgated text: objections accommodated by ambiguity insertion versus objections overridden indicate which incompatibilities remain live inside the final corpus.',
    'Irreducible incompatibility means neither sibling reading can absorb the corpus without schism-scale cost, stabilizing this reading''s account; transient friction would permit eventual convergence and decay of the hermeneutical contest altogether.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_vote_signal, empirical, 'What the persistent minority votes signal about unresolved incompatibility embedded in the final texts.').

omega_variable(
    compliance_internalization,
    'Is observed compliance with the assent regime structural (canonical penalty exposure) or internalized (clerical identity fusion with conciliar obedience), and in what proportion?',
    'Post-penalty trajectory study: whether censured theologians and irregular-status communities restore conformity when enforcement relaxes (structural compliance) or persist in self-censorship after penalties lapse (internalized compliance).',
    'Internalized compliance means effective suppression exceeds the structural enforcement measure and persists across enforcement cycles; purely structural compliance predicts sharp relaxation during detente phases, matching the observed 2005-2015 trough.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_internalization, empirical, 'Structural versus internalized mechanisms sustaining compliance with the assent regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__composite_overdetermination_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 1995, 0.33).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2005, 0.36).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1965, 0.45).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1975, 0.58).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1985, 0.66).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 2025, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1975, 0.62).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1985, 0.74).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2005, 0.52).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2015, 0.63).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_rupture_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the authority of Vatican II' decomposes per the epsilon-invariance principle into three structurally distinct constraints sharing one kernel. This composite-overdetermination reading anchors epsilon in hermeneutical extraction over an underdetermined corpus. The continuity reading anchors epsilon near coordination-only and is upstream in citation order — it is the account the benefiting institution cites as the corpus's self-understanding, and this reading is downstream in the sense that the composite account explains why the continuity account remains citable despite implementation divergence. The rupture reading anchors epsilon in displacement of the pre-conciliar inheritance and mirrors the continuity reading from the minority flank. Each family member links the others via affects_constraints; no member averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_magisterial_authority__composite_overdetermination_reading, powerless, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
