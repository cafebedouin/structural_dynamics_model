% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__living_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__living_constitutionalism_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__living_constitutionalism_reading
 *   human_readable: Magna Carta as Inherited Due Process Restraint (Living Constitutionalism Reading)
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This story instantiates the living constitutionalism reading of the
 *   magna_carta_constraint_authority kernel: the claim that Magna Carta
 *   establishes an inherited due process and lawful restraint binding all
 *   subsequent rulers through juridical precedent and evolutionary
 *   interpretation. The standing arrangement under contest — and the sole
 *   referent of the extractiveness score — is that inherited-restraint
 *   arrangement as it actually operates: courts administering procedural
 *   guarantees descended from the 1215 settlement, executives bearing their
 *   limits, subjects holding them as an inheritance. From this reading's own
 *   lights the arrangement is predominantly coordination around inherited
 *   restraint, with identifiable but bounded asymmetries: executives
 *   surrender discretion, while the judiciary and legal profession accrue
 *   interpretive authority as the price of the machinery's maintenance. The
 *   sibling readings (feudal_obsolescence_reading,
 *   parliamentary_sovereignty_reading) are separate constraint stories linked
 *   through the network section; their verdicts are not averaged into this
 *   file, and this file's epsilon is indexed to this reading's assessment of
 *   the shared referent. KEY AGENTS (by structural relationship): -
 *   common_law_judiciary: agenda-setter and beneficiary
 *   (institutional/identity_locked) — administers the inherited restraint
 *   through precedent and accrues interpretive authority -
 *   charter_subjects_and_descendants: primary beneficiary
 *   (moderate/constrained) — hold the inherited due process shield across
 *   generations - royal_prerogative_bearers: primary target
 *   (powerful/constrained) — bear the restraint across reigns, from medieval
 *   monarchs to modern executives - legal_profession: secondary beneficiary
 *   (organized/identity_locked) — careers and authority structures
 *   constituted by the interpretive tradition - elected_parliamentarians:
 *   excluded voice (organized/constrained) — would contest unmandated
 *   judicial evolution of the restraint's meaning -
 *   constitutional_historians: analytical observer (analytical/analytical) —
 *   examines the charter's actual operative force across centuries
 *
 * KEY AGENTS:
 *   - common_law_judiciary: agenda-setter and beneficiary (institutional/identity_locked) — administers the inherited restraint through precedent and accrues interpretive authority
 *   - charter_subjects_and_descendants: primary beneficiary (moderate/constrained) — hold the inherited due process shield across generations
 *   - royal_prerogative_bearers: primary target (powerful/constrained) — bear the restraint across reigns, from medieval monarchs to modern executives
 *   - legal_profession: secondary beneficiary (organized/identity_locked) — careers and authority structures constituted by the interpretive tradition
 *   - elected_parliamentarians: excluded voice (organized/constrained) — would contest unmandated judicial evolution of the restraint's meaning
 *   - constitutional_historians: analytical observer (analytical/analytical) — examines the charter's actual operative force across centuries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__living_constitutionalism_reading, 0.4).
domain_priors:suppression_score(magna_carta_constraint_authority__living_constitutionalism_reading, 0.46).
domain_priors:theater_ratio(magna_carta_constraint_authority__living_constitutionalism_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0.46).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__living_constitutionalism_reading, rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__living_constitutionalism_reading, "Magna Carta as Inherited Due Process Restraint (Living Constitutionalism Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__living_constitutionalism_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__living_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__living_constitutionalism_reading, '56ac0d63-e955-4919-a087-d07f31c02871').
narrative_ontology:cs_kernel_codification('56ac0d63-e955-4919-a087-d07f31c02871', fixed_text).
narrative_ontology:cs_authority_grounding('56ac0d63-e955-4919-a087-d07f31c02871', lineage).
narrative_ontology:cs_interpretation_layer_present('56ac0d63-e955-4919-a087-d07f31c02871').
narrative_ontology:cs_reading_relation('56ac0d63-e955-4919-a087-d07f31c02871', magna_carta_constraint_authority__feudal_obsolescence_reading, forecloses).
narrative_ontology:cs_reading_relation('56ac0d63-e955-4919-a087-d07f31c02871', magna_carta_constraint_authority__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('56ac0d63-e955-4919-a087-d07f31c02871', foundational, precedent_binding_across_successions).
narrative_ontology:cs_axiom_status(precedent_binding_across_successions, holdable).
narrative_ontology:cs_axiom_grounding('56ac0d63-e955-4919-a087-d07f31c02871', precedent_binding_across_successions, conventional).
narrative_ontology:cs_axiom('56ac0d63-e955-4919-a087-d07f31c02871', foundational, evolutionary_interpretation_preserves_restraint).
narrative_ontology:cs_axiom_status(evolutionary_interpretation_preserves_restraint, holdable).
narrative_ontology:cs_axiom_grounding('56ac0d63-e955-4919-a087-d07f31c02871', evolutionary_interpretation_preserves_restraint, instrumental).
narrative_ontology:cs_reference_frame('56ac0d63-e955-4919-a087-d07f31c02871', perpetual_inherited_lawful_restraint).
narrative_ontology:cs_drift_state('56ac0d63-e955-4919-a087-d07f31c02871', contemporary_post_war_rights_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('56ac0d63-e955-4919-a087-d07f31c02871', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, charter_subjects_and_descendants).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, common_law_judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, legal_profession).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, royal_prerogative_bearers).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, juridical_precedent_binding_force).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, evolutionary_interpretation_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, rule_of_law_supremacy_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the inherited procedural guarantees: issues writs such as habeas corpus, tests exercises of executive power against old procedural standards, and decides what the ancient promises require of present cases. Each generation of judges inherits a body of precedent and adds to it; their adjudicative authority flows from being custodians of that inheritance. Leaving the role would mean abandoning the method that constitutes the office itself.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, common_law_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__living_constitutionalism_reading, common_law_judiciary, beneficiary).

% Hold enforceable procedural protections against the most powerful actor in the realm: protection from imprisonment without lawful judgment, from dispossession without process, from exaction without consent. The protections arrive already won — inherited rather than renegotiated at each succession. Individuals cannot exit the jurisdiction's legal order without moving to another one; within it, their recourse runs through the courts that administer the inheritance.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, charter_subjects_and_descendants, beneficiary,
    moderate, generational, constrained, national).

% Exercise the executive power the inherited guarantees fence in: medieval kings compelled to confirm the promises before levying taxes, Stuart monarchs confronted over forced loans and ship money, modern governments whose orders and prorogations can be tested against old procedural standards. Each holder commands the realm's largest resources yet cannot step outside the legal order; the available levers — appointing judges, persuading legislatures — work slowly and in public view.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, royal_prerogative_bearers, payer,
    powerful, biographical, constrained, national).

% Practices a craft organized around the inherited guarantees: drafting pleadings that invoke them, training each cohort in the interpretive method, staffing the offices through which the guarantees operate. Professional standing, education, and income attach to the tradition's continuation; exit would mean retraining into a different legal culture.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, legal_profession, beneficiary,
    organized, biographical, identity_locked, national).

% Represent electorates whose preferences the courts sometimes override when applying evolved procedural standards. They can legislate, but judicial constructions of the old guarantees constrain what statutes accomplish and how quickly. Their objection — that unelected interpreters set limits without electoral mandate — has no seat in the courtroom where the meaning is settled.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, elected_parliamentarians, excluded,
    organized, biographical, constrained, national).

% Study what the 1215 settlement actually did, what each reissue and reinterpretation changed, and how much of the inherited edifice remains operative law versus inherited symbolism. Positioned outside the practice, they can compare the tradition's self-description against the documentary record across eight centuries.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__living_constitutionalism_reading, common_law_judiciary).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__living_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Makes restraint on rulers durable across successions: one inherited standard of lawful procedure that each new holder of executive power receives already in force, so limits need not be re-won by force at every accession and subjects can know in advance the procedures that govern them.
% TRANSFER_FUNCTION: Moves discretionary power from executive holders to subjects as enforceable procedural rights, and moves interpretive authority over the inheritance to the judiciary and legal profession, who decide in each generation what the old guarantees require.
% ABSENT_VOICES: Elected legislators and the democratic majorities they answer to sit outside the interpretive seat: they would object that unelected judges evolve the restraint's meaning without electoral mandate. Historically, the defenders of unbounded prerogative — Stuart absolutists and divine-right theorists — were militarily and constitutionally defeated rather than persuaded, and neither group sits in the courtroom or the profession's self-governing bodies where the meaning is settled.
% DISAPPEARANCE_RATIONALE: If the inherited restraint vanished overnight, every exercise of state power would lose its oldest warrant: habeas corpus, due process protections, and judicial review of executive action descend from the charter's procedural guarantees. Successions would require fresh negotiation of limits each reign; property and liberty entitlements anchored in centuries of precedent would need wholesale re-legislation; the judiciary's adjudicative role would contract to whatever surviving statutes specify. The legal order would rearrange around either deliberate codification or unrestrained discretion.
% FOUNDING_PROBLEM: In 1215: a king levying arbitrary scutage and reliefs, seizing lands and heirs without judgment, imprisoning subjects without lawful process, and exploiting foreign favorites to extort revenue — the barons demanded written, inheritable limits on royal exaction and punishment.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Articles of the Barons and the charter text itself record the grievances contemporaneously; monastic chroniclers (Roger of Wendover, Matthew Paris) documented the exactions that provoked the revolt; and modern scholarship on administrative discretion attests the problem's persistence in transformed form. None of these sources collects anything from the arrangement's continued operation.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__living_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__living_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__living_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).
:- end_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.40 (matching the interval-end measurement): the arrangement extracts modestly — procedural burdens on executives, interpretive authority accrued by the profession — relative to its protective output; low-to-moderate, as this reading's structure predicts. Suppression 0.46: enforcement requires real coercive machinery (writs, contempt, invalidation of executive acts) but rests on broad legitimation rather than continuous force. Theater_ratio 0.45: a large ceremonial and rhetorical share (anniversary observances, political invocation, museum custody of the 1297 copy) coexists with genuinely operative function (the live habeas corpus statute, charter-citing precedent, due process doctrine). Accessibility_collapse 0.40: alternatives do not collapse — codified bills of rights, statutory guarantees, and treaty instruments layer atop the inheritance rather than being crowded out by it. Resistance 0.45: recurring executive pushback (prerogative assertions, emergency derogations, the prorogation litigation) keeps friction moderate and permanent. The three measurement series share one eight-point grid spanning 1215-2025; the suppression trajectory oscillates with regime type — armed baronial enforcement at origin, statutory consolidation, lapse under prerogative monarchy, revolutionary refounding in 1689, normalized modern enforcement — and the oscillation is documented as driven by external regime change, not as an intermittent-reinforcement mechanism. The claimed type (rope) and the metrics were authored independently: the claim states this reading's structural verdict; the metrics describe the arrangement's operation as the record shows it.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same precedent chain. From the executive seat the inherited guarantees are an external fetter: discretion surrendered to standards the executive did not set and cannot veto. From the beneficiary seat they are an inherited shield: protection already won, requiring no renewed exertion. From the agenda-setter seat the same chain is the source of the judiciary's own authority — custodianship reads as office, not burden. Nothing in the authored claim adjudicates between these; the engine derives each seat's classification from power, exit, and declared position.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation: charter_subjects_and_descendants (beneficiary, moderate, constrained) sit near the subsidized end; common_law_judiciary and legal_profession (beneficiaries, identity_locked) sit low, with the judiciary additionally holding the agenda-setter seat that administers enforcement; royal_prerogative_bearers (victim, powerful, constrained) derive near the full-target end. One override corrects the derivation for the powerful seat: rulers and executives receive real returns from governing under known law — legitimacy, credible commitment, succession stability — so their directionality sits at 0.88 rather than the ~1.0 a pure victim derivation yields. Suppression (0.46) is authored as a raw structural property — the enforcement intensity the arrangement requires — and is deliberately not scaled by power or scope; only extractiveness is scaled downstream.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim guards against the pure-extraction misread: a lens that saw only the judiciary's accrued authority and the executive's surrendered discretion would classify the arrangement as extraction dressed as guardianship, missing the genuine collective problem solved — durable limits without renegotiation at every succession. The reverse misread is equally guarded: the founding problem (capricious royal exaction) is dead in its original form, and the reading's live-status claim survives only because the problem has a demonstrable descendant in unaccountable administrative discretion. If that descendant link fails — if the restraint's maintenance becomes purely commemorative — the arrangement trends toward theatrical persistence, and the temporal theater_ratio series is the instrument that would catch the transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_index_of_classification,
    'This story is one reading (living_constitutionalism_reading) of the magna_carta_constraint_authority kernel; how would the classification change under the sibling readings?',
    'Classify the sibling stories (feudal_obsolescence_reading, parliamentary_sovereignty_reading) with identical structural probes and compare per-seat outputs across the family.',
    'Under feudal obsolescence the victim set empties (nothing binds, so nothing extracts and no seat bears costs); under parliamentary sovereignty the agenda-setter seat migrates from the judiciary to Parliament and the restraint becomes revisable statute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_index_of_classification, conceptual, 'Committer structure: classification is reading-indexed over a contested kernel.').

omega_variable(
    authority_locus_disagreement,
    'Where exactly do the readings disagree — on the empirical fact of institutional continuity from 1215, on the locus of interpretive authority (court versus Parliament), or on the normative claim that inherited restraint legitimately binds successors who never consented?',
    'Factor the kernel into separable claims (empirical continuity, institutional locus, normative bindingness) and test which axis each sibling reading rejects.',
    'If disagreement reduces to the normative axis, the readings are preference-class rivals and the kernel is stable; if the empirical continuity axis fractures, this reading loses its warrant entirely and collapses toward the feudal obsolescence position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_locus_disagreement, conceptual, 'Locating the structural element on which sibling readings diverge.').

omega_variable(
    operative_vs_symbolic_binding_share,
    'What share of the charter''s contemporary binding force is operative (justiciable provisions, live precedent citations, enforceable writs) versus symbolic (ceremony, rhetoric, commemoration)?',
    'Doctrinal census over a fixed window: count live judicial citations and enforceable charter-derived provisions against ceremonial and rhetorical invocations.',
    'A predominantly symbolic share raises theater_ratio and trends the arrangement toward theatrical persistence within this reading; a robust operative share confirms the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operative_vs_symbolic_binding_share, empirical, 'Operative versus symbolic composition of the inherited restraint''s present force.').

omega_variable(
    ruler_legitimacy_return,
    'Do rulers and executives derive net returns (legitimacy, stability, credible commitment) from governing under the inherited restraint sufficient to offset the discretion they surrender?',
    'Comparative institutional analysis of regimes with and without inherited-restraint traditions: succession stability, borrowing credibility, rebellion frequency.',
    'If returns exceed costs, the powerful seat''s directionality sits further from full-target than the victim declaration alone implies and effective extraction falls; if not, the restraint is closer to pure imposition on rulers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ruler_legitimacy_return, empirical, 'Whether the target seat receives offsetting returns from the arrangement.').

omega_variable(
    interpretive_authority_rent_boundary,
    'Is the judiciary''s accrued interpretive authority a necessary coordination cost of maintaining intergenerational restraint, or a rent captured by a self-perpetuating profession?',
    'Compare restraint administration across jurisdictions: court-administered (Anglophone), assembly-administered (codified), and absent; measure protective output per unit of interpretive authority.',
    'If rent, extractiveness rises and the arrangement drifts toward a hybrid coordination/extraction profile; if cost, the coordination reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_rent_boundary, conceptual, 'Boundary between coordination cost and captured rent in the interpretive machinery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__living_constitutionalism_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement_basis(magn_tr_t1215, observed).
narrative_ontology:measurement(magn_tr_t1350, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1350, 0.2).
narrative_ontology:measurement_basis(magn_tr_t1350, observed).
narrative_ontology:measurement(magn_tr_t1500, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1500, 0.3).
narrative_ontology:measurement_basis(magn_tr_t1500, observed).
narrative_ontology:measurement(magn_tr_t1689, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1689, 0.22).
narrative_ontology:measurement_basis(magn_tr_t1689, observed).
narrative_ontology:measurement(magn_tr_t1830, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1830, 0.3).
narrative_ontology:measurement_basis(magn_tr_t1830, observed).
narrative_ontology:measurement(magn_tr_t1918, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1918, 0.35).
narrative_ontology:measurement_basis(magn_tr_t1918, observed).
narrative_ontology:measurement(magn_tr_t1965, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1965, 0.4).
narrative_ontology:measurement_basis(magn_tr_t1965, observed).
narrative_ontology:measurement(magn_tr_t2025, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 2025, 0.45).
narrative_ontology:measurement_basis(magn_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1215, 0.28).
narrative_ontology:measurement_basis(magn_be_t1215, observed).
narrative_ontology:measurement(magn_be_t1350, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1350, 0.3).
narrative_ontology:measurement_basis(magn_be_t1350, observed).
narrative_ontology:measurement(magn_be_t1500, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1500, 0.26).
narrative_ontology:measurement_basis(magn_be_t1500, observed).
narrative_ontology:measurement(magn_be_t1689, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1689, 0.33).
narrative_ontology:measurement_basis(magn_be_t1689, observed).
narrative_ontology:measurement(magn_be_t1830, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1830, 0.34).
narrative_ontology:measurement_basis(magn_be_t1830, observed).
narrative_ontology:measurement(magn_be_t1918, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1918, 0.36).
narrative_ontology:measurement_basis(magn_be_t1918, observed).
narrative_ontology:measurement(magn_be_t1965, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1965, 0.38).
narrative_ontology:measurement_basis(magn_be_t1965, observed).
narrative_ontology:measurement(magn_be_t2025, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 2025, 0.4).
narrative_ontology:measurement_basis(magn_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1215, 0.55).
narrative_ontology:measurement_basis(magn_su_t1215, observed).
narrative_ontology:measurement(magn_su_t1350, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1350, 0.45).
narrative_ontology:measurement_basis(magn_su_t1350, observed).
narrative_ontology:measurement(magn_su_t1500, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1500, 0.35).
narrative_ontology:measurement_basis(magn_su_t1500, observed).
narrative_ontology:measurement(magn_su_t1689, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1689, 0.5).
narrative_ontology:measurement_basis(magn_su_t1689, observed).
narrative_ontology:measurement(magn_su_t1830, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1830, 0.4).
narrative_ontology:measurement_basis(magn_su_t1830, observed).
narrative_ontology:measurement(magn_su_t1918, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1918, 0.42).
narrative_ontology:measurement_basis(magn_su_t1918, observed).
narrative_ontology:measurement(magn_su_t1965, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1965, 0.44).
narrative_ontology:measurement_basis(magn_su_t1965, observed).
narrative_ontology:measurement(magn_su_t2025, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 2025, 0.46).
narrative_ontology:measurement_basis(magn_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__living_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority__feudal_obsolescence_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'Magna Carta's authority' covers three structurally distinct claims with different epsilon values, victim sets, and classifications. This file instantiates the living constitutionalism reading (inherited restraint binding all rulers through juridical precedent; subjects shielded, executives targeted, judiciary as agenda-setter; low-to-moderate extraction). The feudal obsolescence reading (no binding authority; no parties bound, no extraction) and the parliamentary sovereignty reading (restraint survives only as revisable statute; Parliament as agenda-setter) are separate stories linked here. The living reading is downstream of the documentary-historical record the feudal obsolescence reading interrogates, and upstream of the parliamentary sovereignty reading in legitimacy terms: the claim that precedent binds all rulers is precisely what the parliamentary reading must deny.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_constraint_authority__living_constitutionalism_reading, powerful, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
