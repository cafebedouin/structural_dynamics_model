% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__continuity_reading, []).

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
 *   constraint_id: correct_latin_kernel__continuity_reading
 *   human_readable: Continuity Doctrine of Correct Latin — Internal-Correction Regime
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   A learned-language arrangement spans Latin Christendom: cathedral and
 *   monastic schools teach the inherited tongue as the same language Rome
 *   wrote, matured by ordinary growth rather than broken; licensed masters
 *   train notaries, clerks, and scholars; scriptoria copy and mend books by
 *   house rules descended from the grammatical canon; chanceries issue
 *   instruments whose authority leans on the claim that nothing essential was
 *   ever interrupted. Repair is defined internally — copyist damage is
 *   corrected out of the tradition's own earlier layers and grammatical
 *   resources, and the result is held to remain the ancestors' speech.
 *   Endowments, posts, licensing fees, and documentary prestige flow to the
 *   apparatus administering this account; proposals to restore an older idiom
 *   from outside the stream are refused as meddling with healthy growth, and
 *   their advocates are shut out of posts and patronage. KEY AGENTS (by
 *   structural relationship): - cathedral_monastic_schools: agenda_setter and
 *   receipt seat (institutional/constrained) — administers curriculum,
 *   licensing, and correction protocol - papal_royal_chanceries: principal
 *   documentary beneficiary (institutional/arbitrage) — converts the
 *   unbroken-lineage claim into documentary authority -
 *   monastic_scriptorium_communities: operational beneficiary
 *   (organized/trapped) — performs copying and repair inside the tradition -
 *   grammar_masters: dual-positioned beneficiary-laborer
 *   (moderate/identity_locked) — teaches and defends the standard; identity
 *   fused with transmission - humanist_reform_advocates: primary target
 *   (moderate/mobile) — restoration program refused as meddling; barred from
 *   posts - antiquarian_textual_critics: secondary target
 *   (moderate/constrained) — collation labor refused where alterations are
 *   classed as growth - student_readers: diffuse cost bearer
 *   (powerless/trapped) — absorbs whatever survives correction -
 *   byzantine_greek_scholars: excluded witness (moderate/mobile) — holds
 *   comparative evidence, no seat in the conversation -
 *   comparative_philology_analysts: analytical observer
 *   (analytical/analytical) — views the full structure across the record. The
 *   claim/metric gap is deliberate: claimed_type states the structural
 *   reading, while the metric block reports operation as descriptively
 *   measured — the engine computes per-seat verdicts from the structural
 *   data, and the claim is not tuned to them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__continuity_reading, 0.42).
domain_priors:suppression_score(correct_latin_kernel__continuity_reading, 0.58).
domain_priors:theater_ratio(correct_latin_kernel__continuity_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__continuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__continuity_reading, "Continuity Doctrine of Correct Latin — Internal-Correction Regime").
narrative_ontology:topic_domain(correct_latin_kernel__continuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__continuity_reading, 'a54f3872-e1b3-4e58-be76-d475cf45b981').
narrative_ontology:cs_kernel_codification('a54f3872-e1b3-4e58-be76-d475cf45b981', fixed_text).
narrative_ontology:cs_authority_grounding('a54f3872-e1b3-4e58-be76-d475cf45b981', lineage).
narrative_ontology:cs_interpretation_layer_present('a54f3872-e1b3-4e58-be76-d475cf45b981').
narrative_ontology:cs_reading_relation('a54f3872-e1b3-4e58-be76-d475cf45b981', correct_latin_kernel__discontinuity_reading, forecloses).
narrative_ontology:cs_reading_relation('a54f3872-e1b3-4e58-be76-d475cf45b981', correct_latin_kernel__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('a54f3872-e1b3-4e58-be76-d475cf45b981', foundational, medieval_usage_is_organic_development).
narrative_ontology:cs_axiom_status(medieval_usage_is_organic_development, holdable).
narrative_ontology:cs_axiom_grounding('a54f3872-e1b3-4e58-be76-d475cf45b981', medieval_usage_is_organic_development, empirically_contingent).
narrative_ontology:cs_axiom('a54f3872-e1b3-4e58-be76-d475cf45b981', foundational, tradition_internal_repair_suffices).
narrative_ontology:cs_axiom_status(tradition_internal_repair_suffices, holdable).
narrative_ontology:cs_axiom_grounding('a54f3872-e1b3-4e58-be76-d475cf45b981', tradition_internal_repair_suffices, instrumental).
narrative_ontology:cs_reference_frame('a54f3872-e1b3-4e58-be76-d475cf45b981', unbroken_lineage_standard).
narrative_ontology:cs_drift_state('a54f3872-e1b3-4e58-be76-d475cf45b981', early_humanist_challenge, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('a54f3872-e1b3-4e58-be76-d475cf45b981', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__continuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, cathedral_monastic_schools).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, papal_royal_chanceries).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, monastic_scriptorium_communities).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, grammar_masters).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, humanist_reform_advocates).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, antiquarian_textual_critics).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, student_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, student_readers).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, grammar_masters).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, linguistic_continuity_doctrine).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, internal_correction_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the grammar curriculum across Latin Christendom, examine and license teachers, and keep the correction protocols by which copied books are repaired. Statutes, endowments, and office all presuppose custody of the tongue handed down from antiquity; abandoning the inherited account would dissolve the institution's reason for existing, so change happens slowly and from inside.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, cathedral_monastic_schools, agenda_setter,
    institutional, generational, constrained, continental).

% Issue bulls, diplomas, and legal instruments in the learned tongue. On the received account the written standard descends unbroken from Rome, so each instrument carries antique weight without further proof. They endow schools and prefer grammarians, and can move that patronage to whichever account of the language serves their documents' standing best.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, papal_royal_chanceries, beneficiary,
    institutional, generational, arbitrage, continental).

% Copy, gloss, and mend manuscripts under house rules drawn from the grammatical tradition. Libraries, liturgical books, and the round of daily offices rest on the received stream of texts; leaving the stream would orphan the collection and leave the mending work without warrant.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, monastic_scriptorium_communities, beneficiary,
    organized, generational, trapped, regional).

% Teach the inherited standard as the ancestors' own speech, train notaries and clerks, and carry out day-to-day correction of texts. Vocation, livelihood, and self-understanding are bound up with transmission — to cease being a master of the unbroken tongue would be to cease being who they are. They also shoulder the labor of answering challenges to the standard whenever novelties appear in the books.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, grammar_masters, beneficiary,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__continuity_reading, grammar_masters, payer).

% Hold that the learned tongue has slipped from its ancient form and should be brought back from early manuscripts and classical models. Teaching posts, chancery usage, and correction protocols stay closed to their program, which the schools dismiss as meddling with sound growth. They keep solvent by circulating among courts, patrons, and cities beyond any single school's reach.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, humanist_reform_advocates, payer,
    moderate, biographical, mobile, continental).

% Collate old manuscripts to strip out what copyists added or displaced. Where the received account classes many alterations as the language's natural growth, their proposed repairs are refused as needless or harmful, money for collation dries up, and results sit unpublished. Their craft binds them to the very libraries the schools administer, so the work cannot easily be taken elsewhere.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, antiquarian_textual_critics, payer,
    moderate, biographical, constrained, continental).

% Receive the corrected books as the whole of their schooling in letters, law, and theology. Whatever passes correction as legitimate growth stays in what they learn, and they hold no seat where corrections are decided. In return they gain a usable written language and access to the inherited shelf of books.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, student_readers, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__continuity_reading, student_readers, beneficiary).

% Keep direct acquaintance with ancient Greek and late-antique usage, and answer Italian correspondents who ask about antiquity. The western schools set correction norms without consulting them, though they could testify where received practice departs from late-antique precedent. Their vantage lies outside the conversation that decides what counts as correct.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, byzantine_greek_scholars, excluded,
    moderate, generational, mobile, continental).

% Examine the transmission record across centuries — charter formulae, manuscript families, Romance descendants, Greek parallels — to sort organic change from copyist error. They hold no office in any school and no stake in any endowment, and can view the whole structure at once.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, comparative_philology_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__continuity_reading, cathedral_monastic_schools).
narrative_ontology:fixing_cost_class(correct_latin_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps one written learned language intelligible from Ireland to Hungary: shared grammar, spelling, and morphology let a charter drafted in Paris be read in Krakow, let students move between schools, and let a book copied in one monastery be mended in another by the same rules.
% TRANSFER_FUNCTION: Moves legitimacy and income upward to the administering apparatus — endowments, teaching posts, licensing fees, and the antique authority attaching to chancery documents — moves correction labor downward onto masters and scribes, and leaves the cost of any damage that survives correction with students and downstream readers.
% ABSENT_VOICES: Byzantine Greek scholars who could compare received practice with late-antique precedent are not consulted; lay and vernacular communities who use the language without governing it have no seat; dissenting grammarians appear only as defendants when their proposals come up for judgment.
% DISAPPEARANCE_RATIONALE: Without the continuity account and its correction apparatus, the common written standard would fragment along regional lines far sooner; chancery instruments would lose their appeal to antique authority; schools would lose their charter logic; and the recovery of classical texts would take a different path, with the restoration turn arriving earlier or in another shape — every institution built on handing down one tongue would have to rebuild its justification.
% FOUNDING_PROBLEM: After the collapse of Roman administration, western Europe still needed a single supraregional written language for church governance, law, diplomacy, and learning, together with a workable procedure for keeping ancient texts alive through repeated copying.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: royal and imperial chanceries kept demanding trained notaries in a common legal idiom; notarial guild registers and mercantile contracts record the cost of cross-border drafting wherever the standard frayed; conciliar acta complain of delegates unable to understand one another. None of these attestations issues from the monastic or cathedral beneficiary set.
narrative_ontology:disappearance_verdict(correct_latin_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__continuity_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__continuity_reading_tests).
:- end_tests(correct_latin_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness sits mid-range (0.42 at interval end) because the arrangement does deliver the coordination good — one intelligible written language from Ireland to Hungary — while legitimacy rent and unflagged textual damage accrue asymmetrically to its administrators and away from readers. Suppression (0.58) is structural and unscaled: closure operates through withheld posts, licenses, and patronage rather than force, and only extractiveness is scaled by directionality and scope in the engine's computation. Theater (0.25) covers lineage invocations and ceremonial continuity claims that do little repair work beside a predominantly functional apparatus of grammar teaching, copying, and glossing. Accessibility collapse is moderate (0.40): Greek witnesses, pre-transmission exemplars, and courtly patronage markets keep alternatives visible though costly to pursue. Resistance (0.55) rises across the interval as restoration advocacy spreads. All three tracked series share one grid of seven points; suppression_requirement is tracked because enforcement capacity visibly hardened in response to the restoration challenge — a defensive ratchet, not mere drift in other metrics.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is stewardship: a living inheritance kept intact and handed on, with repair as caretaking. From the restoration advocate's seat the same structures are closure: posts never offered, protocols that refuse outside standards, and the label of meddler pinned to the rival program. Antiquarian collators experience refusal of their findings; students consume the output without seeing the selection that produced it. The engine computes these per-seat verdicts from the structural data; the divergence between seats is the measurement the corpus exists to take, not something the authored claim adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   Schools, chanceries, scriptoria, and masters declare as beneficiaries and sit near the subsidized end of directionality; masters carry a secondary payer position for their repair labor. Restoration advocates, antiquarian collators, and students declare as victims and sit near the target end; students also draw real coordination benefit, hence the secondary beneficiary marking. Exit structure sharpens the spread: chanceries hold arbitrage-grade exit and can re-patronize whichever account prevails; scriptoria and students are trapped; masters are identity-locked to transmission as vocation; restoration advocates stay mobile between courts; collators are constrained to the very archives the apparatus administers. Suppression is authored as a raw structural property and enters the computation unscaled; effective extractiveness is left entirely to the engine's arithmetic over directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetric mislabels. Reading the arrangement as pure confiscation erases the real supraregional good — charters readable abroad, students who can move between schools, books repairable in any house — and would mispredict the loyalty of participants who defend it as its users. Reading it as pure coordination erases the concentrated receipts (endowments, posts, documentary prestige accruing at the administrators) and the enforced closure of the rival program. The founding problem — a common written learned language plus a workable transmission procedure — remains live throughout the interval, so no mandate is declared obsolete; the contest is over which account of the language may carry the coordination and who bears the cost of maintaining it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story authors epsilon, the victim set, and the claimed type from the continuity_reading seat of correct_latin_kernel; how do those quantities shift under the sibling readings of the same transmission record?',
    'Author correct_latin_kernel__discontinuity_reading and correct_latin_kernel__hybrid_reading as separate stories over the same record and compare computed types, victim sets, and epsilon across the family.',
    'Under the discontinuity reading the medieval producers themselves become targets (every innovation counts as corruption) and epsilon rises sharply; under the hybrid reading extraction splits by linguistic layer, with lexical and syntactic recovery carrying the contested burden. Per-seat classifications reorder accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification over a shared transmission record.').

omega_variable(
    corruption_vs_development_share,
    'Of the divergences between transmitted usage and classical norms, what share is organic linguistic development and what share is accumulated copyist error that the internal-correction protocol fails to flag?',
    'Collate transmitted readings against surviving pre-transmission exemplars; compare with Romance reflexes and Greek parallel usage; model expected rates of organic morphosyntactic change against observed textual variance.',
    'A high error share means the regime launders damage as growth — extraction climbs toward confiscatory levels and downstream readers bear compounding costs; a low error share means the coordination function dominates and measured excess extraction shrinks toward coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corruption_vs_development_share, empirical, 'Indistinguishability of scribal error from legitimate evolution is the load-bearing uncertainty of the continuity claim.').

omega_variable(
    natural_change_vs_constructed_authority,
    'Is the operative regularity the natural fact that languages change (chosen by no party, collected from by none), or the constructed authority claim built on top of that fact (which concentrates legitimacy and income at the administering apparatus)?',
    'Separate the descriptive record of organic change from the normative doctrine that whatever the tradition carries is therefore legitimate; test whether the doctrine''s enforcement adds anything the bare descriptive fact does not already secure.',
    'If the authority superstructure is separable from natural change, the extraction attaches only to the superstructure and epsilon drops materially; if inseparable, part of the measured extraction is the unavoidable price of holding any standard steady over natural drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_change_vs_constructed_authority, conceptual, 'Natural-process ambiguity beneath a constructed authority claim.').

omega_variable(
    enforcement_material_vs_doctrinal,
    'Is the closure of alternative accounts maintained materially (withheld posts, patronage, and licenses) or doctrinally (persuasive dominance of the continuity account)?',
    'Trace the careers and publication record of grammarians proposing restoration or external-standard methods; compare appointment outcomes against doctrinal compliance.',
    'Material enforcement implies higher effective pressure on targets than the doctrinal picture alone and strengthens the extraction reading; purely doctrinal closure implies the scalar suppression overstates the structural force actually applied.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_material_vs_doctrinal, empirical, 'Mechanism ambiguity behind the measured suppression figure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__continuity_reading, 1150, 1450).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clk_continuity_tr_t1150, correct_latin_kernel__continuity_reading, theater_ratio, 1150, 0.16).
narrative_ontology:measurement(clk_continuity_tr_t1200, correct_latin_kernel__continuity_reading, theater_ratio, 1200, 0.17).
narrative_ontology:measurement(clk_continuity_tr_t1250, correct_latin_kernel__continuity_reading, theater_ratio, 1250, 0.19).
narrative_ontology:measurement(clk_continuity_tr_t1300, correct_latin_kernel__continuity_reading, theater_ratio, 1300, 0.2).
narrative_ontology:measurement(clk_continuity_tr_t1350, correct_latin_kernel__continuity_reading, theater_ratio, 1350, 0.22).
narrative_ontology:measurement(clk_continuity_tr_t1400, correct_latin_kernel__continuity_reading, theater_ratio, 1400, 0.24).
narrative_ontology:measurement(clk_continuity_tr_t1450, correct_latin_kernel__continuity_reading, theater_ratio, 1450, 0.25).

% Extraction over time
narrative_ontology:measurement(clk_continuity_be_t1150, correct_latin_kernel__continuity_reading, base_extractiveness, 1150, 0.34).
narrative_ontology:measurement(clk_continuity_be_t1200, correct_latin_kernel__continuity_reading, base_extractiveness, 1200, 0.35).
narrative_ontology:measurement(clk_continuity_be_t1250, correct_latin_kernel__continuity_reading, base_extractiveness, 1250, 0.37).
narrative_ontology:measurement(clk_continuity_be_t1300, correct_latin_kernel__continuity_reading, base_extractiveness, 1300, 0.38).
narrative_ontology:measurement(clk_continuity_be_t1350, correct_latin_kernel__continuity_reading, base_extractiveness, 1350, 0.39).
narrative_ontology:measurement(clk_continuity_be_t1400, correct_latin_kernel__continuity_reading, base_extractiveness, 1400, 0.41).
narrative_ontology:measurement(clk_continuity_be_t1450, correct_latin_kernel__continuity_reading, base_extractiveness, 1450, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(clk_continuity_su_t1150, correct_latin_kernel__continuity_reading, suppression_requirement, 1150, 0.44).
narrative_ontology:measurement(clk_continuity_su_t1200, correct_latin_kernel__continuity_reading, suppression_requirement, 1200, 0.46).
narrative_ontology:measurement(clk_continuity_su_t1250, correct_latin_kernel__continuity_reading, suppression_requirement, 1250, 0.49).
narrative_ontology:measurement(clk_continuity_su_t1300, correct_latin_kernel__continuity_reading, suppression_requirement, 1300, 0.51).
narrative_ontology:measurement(clk_continuity_su_t1350, correct_latin_kernel__continuity_reading, suppression_requirement, 1350, 0.54).
narrative_ontology:measurement(clk_continuity_su_t1400, correct_latin_kernel__continuity_reading, suppression_requirement, 1400, 0.56).
narrative_ontology:measurement(clk_continuity_su_t1450, correct_latin_kernel__continuity_reading, suppression_requirement, 1450, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__continuity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'correct Latin' decomposes, per the epsilon-invariance principle, into three reading-indexed constraints over one shared kernel — the transmitted corpus plus the grammatical canon. Each member carries a distinct epsilon, victim set, and classification: this file (continuity) treats medieval usage as organic growth and humanist restoration as purist meddling; the discontinuity sibling treats the two periods as distinct systems and reconstruction as reoccupation; the hybrid sibling splits continuity by linguistic layer. The files are linked, not merged: family edges above permit contamination propagation and cross-family comparison without averaging epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
