% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__revisable_translation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__revisable_translation_reading, []).

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
 *   constraint_id: kjv_text_1611__revisable_translation_reading
 *   human_readable: KJV Revisable-Translation Regime (Scholarly-Arbiter Reading)
 *   domain: religious/textual_criticism/publishing
 *
 * SUMMARY:
 *   Standing arrangement under contest: since the Revised Version of 1881,
 *   English Bible production has been organized around the premise that the
 *   1611 Authorized Version is a historically momentous but textually
 *   improvable document. Manuscript discoveries (Sinaiticus and Vaticanus in
 *   the nineteenth century, the Chester Beatty and Bodmer papyri
 *   mid-twentieth, the Dead Sea Scrolls from 1947) feed ongoing critical
 *   editions (Westcott-Hort, then Nestle-Aland / United Bible Societies);
 *   translation committees render the corrected text in contemporary
 *   language; commercial publishers package the result under copyright and
 *   sell it through successive editions and study-Bible lines. Selection
 *   among versions rests with consumers, authenticity with credentialed
 *   scholars, revenue with publishers. The arrangement genuinely repairs
 *   demonstrable defects and widens access - and it also taxes repeat
 *   purchasers, disrupts communities whose worship is keyed to a fixed
 *   wording, and concentrates licensing income. This file authors ONLY the
 *   revisable-translation reading of the kjv_text_1611 kernel; sibling
 *   readings are separate constraints with their own epsilon, and nothing
 *   here averages across them. Epsilon's referent is the standing
 *   revision-marketplace arrangement this reading institutes, assessed by
 *   this reading's own lights. Claim and metrics are authored independently:
 *   the type is stated from structural analysis; the metrics describe
 *   observed operation. KEY AGENTS (by structural relationship): -
 *   bible_publishers: Agenda setter and receipt-of-gain seat
 *   (institutional/arbitrage) - runs product cycles, enforces copyright
 *   licensing - academic_textual_critics: Arbiter-beneficiary
 *   (institutional/identity_locked) - adjudicates authentic readings;
 *   vocations constituted by the enterprise - bible_buying_public: Payer
 *   (powerless/mobile) - bears repeat-purchase costs; free alternatives one
 *   click away - traditional_worship_congregations: Payer of continuity costs
 *   (organized/constrained) - liturgy and memory disrupted by wording shifts
 *   - theological_seminaries: Secondary beneficiary
 *   (institutional/constrained) - kjv_only_advocates: Excluded voice
 *   (organized/identity_locked) - holds no seat in the arbiter conversation -
 *   cambridge_university_press: Heritage administrator-beneficiary
 *   (institutional/arbitrage) - letters-patent licensee on the 1611 text in
 *   England - literary_historians: Analytical observer
 *   (institutional/analytical)
 *
 * KEY AGENTS:
 *   - bible_publishers: agenda setter and primary gain-flow seat (institutional power, arbitrage exit) - commissions committees, sets release schedules, operates permissions desks
 *   - academic_textual_critics: arbiter-beneficiary (institutional power, identity_locked exit) - maintains critical editions and staffs translation committees; professional identity fused with the revision enterprise
 *   - bible_buying_public: payer (powerless, mobile exit) - individual readers bearing repeat-purchase costs with free substitutes available
 *   - traditional_worship_congregations: payer (organized, constrained exit) - communities absorbing liturgical and mnemonic disruption when wordings change
 *   - theological_seminaries: beneficiary (institutional, constrained exit) - curricula and faculty lines track each translation generation
 *   - kjv_only_advocates: excluded (organized, identity_locked exit) - contest the premise from outside every decision-making seat
 *   - cambridge_university_press: beneficiary (institutional, arbitrage exit, national scope) - administers Crown letters-patent rights on the 1611 text in England
 *   - literary_historians: observer (institutional, analytical exit) - study the 1611 prose and its cultural career with no stake in adoption outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__revisable_translation_reading, 0.62).
domain_priors:suppression_score(kjv_text_1611__revisable_translation_reading, 0.25).
domain_priors:theater_ratio(kjv_text_1611__revisable_translation_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__revisable_translation_reading, tangled_rope).
narrative_ontology:human_readable(kjv_text_1611__revisable_translation_reading, "KJV Revisable-Translation Regime (Scholarly-Arbiter Reading)").
narrative_ontology:topic_domain(kjv_text_1611__revisable_translation_reading, "religious/textual_criticism/publishing").

domain_priors:requires_active_enforcement(kjv_text_1611__revisable_translation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__revisable_translation_reading, '7476d1aa-26df-4066-a4e5-52d537039137').
narrative_ontology:cs_kernel_codification('7476d1aa-26df-4066-a4e5-52d537039137', fixed_text).
narrative_ontology:cs_authority_grounding('7476d1aa-26df-4066-a4e5-52d537039137', expertise).
narrative_ontology:cs_interpretation_layer_present('7476d1aa-26df-4066-a4e5-52d537039137').
narrative_ontology:cs_reading_relation('7476d1aa-26df-4066-a4e5-52d537039137', kjv_text_1611__exclusive_inspiration_reading, forecloses).
narrative_ontology:cs_reading_relation('7476d1aa-26df-4066-a4e5-52d537039137', kjv_text_1611__functional_equivalence_reading, coexists_with).
narrative_ontology:cs_axiom('7476d1aa-26df-4066-a4e5-52d537039137', foundational, manuscript_evidence_corrects_received_text).
narrative_ontology:cs_axiom_status(manuscript_evidence_corrects_received_text, holdable).
narrative_ontology:cs_axiom_grounding('7476d1aa-26df-4066-a4e5-52d537039137', manuscript_evidence_corrects_received_text, empirically_contingent).
narrative_ontology:cs_axiom('7476d1aa-26df-4066-a4e5-52d537039137', foundational, linguistic_drift_requires_translation_revision).
narrative_ontology:cs_axiom_status(linguistic_drift_requires_translation_revision, holdable).
narrative_ontology:cs_axiom_grounding('7476d1aa-26df-4066-a4e5-52d537039137', linguistic_drift_requires_translation_revision, instrumental).
narrative_ontology:cs_reference_frame('7476d1aa-26df-4066-a4e5-52d537039137', provisional_textual_authority).
narrative_ontology:cs_drift_state('7476d1aa-26df-4066-a4e5-52d537039137', contemporary_post_dss_papyri_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7476d1aa-26df-4066-a4e5-52d537039137', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__revisable_translation_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, academic_textual_critics).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, bible_publishers).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, theological_seminaries).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, cambridge_university_press).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, bible_buying_public).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, traditional_worship_congregations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and manage the major copyrighted modern translations. Commission translation committees, set release schedules for new editions and study-Bible lines, and operate permissions and licensing desks that charge for quotation, bulk use, and app integration. Revenue depends on continued demand for updated formats; exit would mean reorienting catalogs toward public-domain texts and forfeiting licensing income.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, bible_publishers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__revisable_translation_reading, bible_publishers, beneficiary).

% Professors and researchers who collate manuscripts, maintain critical editions, and staff translation committees. Their publications, chairs, and conference networks exist because the received text is treated as open to correction; the discipline of textual criticism is their professional identity, formed over decades of specialization. Leaving the field would mean abandoning the expertise that constitutes their careers.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, academic_textual_critics, beneficiary,
    institutional, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__revisable_translation_reading, academic_textual_critics, agenda_setter).

% Train clergy using current critical texts and teach the history of the English Bible; curriculum refresh cycles track each major new translation, and accreditation expectations assume engagement with the scholarly editions. Course materials depend on publisher desk-copy and licensing arrangements, giving the institutions a durable stake in each new version's arrival.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, theological_seminaries, beneficiary,
    institutional, generational, constrained, continental).

% Holds, under historic letters patent from the Crown, the right to print the 1611 text in England and administers licensing for its use there. Collects fees on authorized printings of the old translation while the same text circulates without payment everywhere else; the position is a legacy grant rather than a creation of the modern revision economy.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, cambridge_university_press, beneficiary,
    institutional, generational, arbitrage, national).

% Individual readers and families who purchase Bibles. Each new translation or edition resets the market: notes, formatting, and wording change, and households wanting current study materials buy again. Free and public-domain texts remain one click away, so stepping off the cycle is always possible - but following along costs money, and study-church cultures normalize the upgrades.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, bible_buying_public, payer,
    powerless, biographical, mobile, global).

% Communities whose liturgy, memorized verses, hymnody, and pulpit habits are keyed to a particular wording of scripture. When pew Bibles and lectionaries switch translations, services must be relearned and cross-references rechecked; adopting new wordings severs ties to inherited practice, while keeping the familiar wording means declining updates and absorbing the friction of being out of step with neighboring congregations and new materials.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, traditional_worship_congregations, payer,
    organized, biographical, constrained, continental).

% Movements, tract societies, and pastors who hold the 1611 translation uniquely authoritative. They publish critiques of critical-method conclusions and of modern versions, but hold no seat on translation committees, editorial boards, or scholarly guild offices; their objections reach the process only as outside correspondence, boycotts, and parallel institutions.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, kjv_only_advocates, excluded,
    organized, biographical, identity_locked, global).

% Scholars of literature and book history who study the 1611 translation's prose, its printers, and its cultural career without any stake in which text congregations adopt. They watch the revision economy from outside both the committees and the marketplace and contribute neither to nor against its enforcement.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, literary_historians, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kjv_text_1611__revisable_translation_reading, bible_publishers).
narrative_ontology:fixing_cost_class(kjv_text_1611__revisable_translation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates hundreds of dispersed manuscript witnesses into a single reliable text through collation and reasoned eclecticism, converts archaic diction into contemporary language, and distributes a standardized scripture at scale - problems no individual reader or congregation can solve alone.
% TRANSFER_FUNCTION: Moves money from Bible purchasers and church procurement budgets to commercial publishers (copyrighted copies, permissions fees) and onward to committee expenses and scholarly infrastructure; moves adjudicative authority to credentialed text-critical scholars; moves cultural attention through each edition-launch cycle.
% ABSENT_VOICES: KJV-only movements and pastors (kjv_only_advocates here) would contest the premise itself but hold no seat on committees, editorial boards, or guild offices; lay readers without academic credentials never occupy the arbiter chair; church leaders from the Global South, where most purchasing now happens, are thinly represented on translation rosters. Their objections arrive as outside correspondence and market behavior, not as votes.
% DISAPPEARANCE_RATIONALE: If the revisability premise vanished overnight and every party adopted a frozen-text stance, translation product cycles would stop, publisher catalogs would contract to heritage and study lines, seminary curricula would lose their organizing spine, the text-critical subfield would shrink toward museum curation, and licensing income would migrate to whatever stayed in print. Named parties demonstrably organize their operations around the premise, so the world rearranges.
% FOUNDING_PROBLEM: Accumulated manuscript evidence and linguistic change had rendered the 1611 translation demonstrably defective on two fronts: its underlying Greek text rested on late medieval copies, and its seventeenth-century diction was receding from common speech. The revision enterprise was built to restore earlier, better-attested readings and to re-render scripture in living language.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: papyrologists, museum-based manuscript researchers, and classical philologists attest that new witnesses keep surfacing and that comparative method measurably improves the text; historians of the English language attest the linguistic-drift premise. Publisher and scholarly-guild attestations agree but are inside the beneficiary set, so the external corroboration is the load-bearing one.
narrative_ontology:disappearance_verdict(kjv_text_1611__revisable_translation_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__revisable_translation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__revisable_translation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kjv_text_1611__revisable_translation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__revisable_translation_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__revisable_translation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kjv_text_1611__revisable_translation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kjv_text_1611__revisable_translation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects decoupling between textual advance and product cadence: manuscript evidence arrives irregularly, but copyrighted editions arrive on commercial schedules, so part of each purchase cycle pays for packaging and rights rather than corrected readings. Suppression is low (0.25) and structural-economic rather than coercive: nobody is compelled to buy any version and public-domain texts remain freely available; the residual force operates through copyright permissions and denominational procurement habit. Accessibility collapse is low (0.35): alternatives visibly survive - dozens of translations, free digital 1611 text - so the option space is not closed. Resistance (0.50) is persistent but institutional: KJV-only movements, liturgical conservatives, and whole denominations decline the revision cycles without frontal assault; buyer-side coalition potential exists and is partially realized (free and public-domain versions circulating online), which is exactly what keeps suppression low. Theater ratio (0.32) tracks the growing share of release activity that repackages an essentially unchanged critical text. All three series share one grid (T0 = 1881 publication of the Revised Version, unit = years, points every 24), each metric authored at every point; endpoints match the scalar properties. Suppression_requirement is tracked because enforcement capacity - permissions desks, licensing administration, digital-rights controls - was built up over the interval; that maturing machinery is the traced dynamic, and it rises gently rather than ratchets. The series drift monotonically; no oscillation is claimed and none is manufactured.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit four different arrangements wearing the same name. From the publisher seat the regime is legitimate rights-funded publishing: royalties finance committees, permissions desks protect textual integrity against mutilated editions, new editions serve readers. From the scholar seat it is faithful stewardship: each revision approaches the earliest recoverable text more closely, and continuation is fidelity, not franchise management. From the buying-public seat it is a treadmill: the Bible owned is declared dated every few years and replacement is marketed as devotion. From the traditional-congregation seat it is erosion of inherited practice: wording drift dissolves memorized verses and liturgical cadence. The engine computes these divergences from power, exit grade, and directional position; nothing in the authored claim adjudicates them. Inter-institutionally, publishers and scholarly guilds need each other while policing different boundaries - publishers guard text integrity through copyright, scholars guard method through peer review - and their cooperation is what converts manuscript discoveries into billable editions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. bible_publishers sit nearest the beneficiary pole: the fee stream lands on their ledgers and arbitrage-grade exit lets them reposition portfolios at will. academic_textual_critics and theological_seminaries derive strongly beneficiary-side positions, with the scholars' identity-locked exit binding them to the arrangement that constitutes their vocation. cambridge_university_press derives near-full beneficiary status on its narrow letters-patent rent. On the target side, traditional_worship_congregations bear high effective extraction: constrained exit (liturgy, memorized verses, cross-reference habits) pins them near the full-target end despite organized collective resources. bible_buying_public also registers as payer, but mobile exit damps effective extraction below what raw exposure alone would suggest - the derivation handles this correctly, so no directionality overrides are authored. Suppression enters the computation unscaled as a raw structural property; only extractiveness is scaled by directionality and spatial scope. Two same-power contrasts sharpen the picture: publishers (arbitrage) and scholars (identity_locked) hold comparable institutional standing yet compute differently because their exits differ; and the two payer classes carry equal nominal exposure but unequal exit grades (mobile versus constrained), yielding unequal effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: witnesses still surface, language still drifts, so the arrangement has not outlived its mandate and mandatrophy resolution is deliberately not declared. The classification prevents two symmetrical mislabels. Read as pure coordination, the arrangement would hide the copyright-refresh tax on buyers and the continuity costs borne by traditional congregations behind the real service textual criticism performs. Read as pure extraction, it would erase genuine corrections - the RV and later editions did repair demonstrable defects in the received text - and misdescribe a voluntary-purchase regime as coerced. Active enforcement is declared because the hybrid needs it: copyright machinery and committee gatekeeping hold the revenue layer in place atop a working coordination layer. The rising theater ratio is a forward-risk indicator, not yet a verdict: if releases keep decoupling from textual advance once the witness stream thins, the structure would begin migrating toward inertial performance, and a sunset question (declare the critical text provisionally settled) would go live. No sunset clause is authored because no participant claims the arrangement is transitional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story is the revisable_translation_reading of kernel kjv_text_1611; which structural facts would change if a sibling reading governed instead?',
    'Compile the sibling stories and diff seat surfaces: exclusive_inspiration_reading closes alternatives and raises suppression sharply; functional_equivalence_reading pluralizes supply and drives extraction toward coordination cost. The disagreement localizes to one element: whether the 1611 text admits defect and correction.',
    'Epsilon, beneficiary sets, and victim sets are NOT portable across readings. Under exclusivity the payer set expands to every non-KJV user; under functional equivalence the payer class nearly dissolves. Averaging epsilon across readings would violate invariance of the measured quantity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: one reading of a contested kernel, with sibling readings as separate constraints.').

omega_variable(
    publisher_capture_vs_textual_advance,
    'Is late-interval version proliferation driven by marginal textual advance or by copyright-refresh economics?',
    'Diff the critical-apparatus changes between successive copyrighted editions against release cadence and revenue data; use natural experiments where public-domain translations coexist with copyrighted peers serving the same readership.',
    'If commerce dominates, payer-seat effective extraction rises and the structure drifts snare-ward; if evidence dominates, the tangled_rope reading stands with extraction near genuine coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publisher_capture_vs_textual_advance, empirical, 'Whether the extraction layer rides genuine textual progress or manufactures its own demand.').

omega_variable(
    arbiter_identity_fusion,
    'Is the scholar-arbiter seat''s endorsement of permanent revisability professional identity fusion (careers constituted by the enterprise) or proportionate response to accumulating evidence?',
    'Probe expert willingness to declare the critical text provisionally settled in sub-corpora where attestation has genuinely stabilized; survey whether any constituency inside the guild argues for freezing.',
    'If fusion dominates, arbiter endorsement carries self-interest and part of the coordination credit assigned to scholarship is theatrical maintenance; classification is unchanged but theater_ratio attribution shifts upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arbiter_identity_fusion, conceptual, 'Professional identity-lock at the arbiter seat versus disinterested arbitration.').

omega_variable(
    crown_copyright_residual_rent,
    'Does residual Crown copyright on the 1611 text itself in England (held under letters patent and administered by university presses) constitute a rent layer inside this reading''s otherwise free baseline?',
    'Compare UK print and licensing terms for the 1611 text with unrestricted reproduction elsewhere; measure fee incidence on UK church and retail pricing.',
    'If material, the old text carries its own small extraction layer independent of modern-version churn, nested inside an arrangement the revisable framing usually treats as costless heritage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(crown_copyright_residual_rent, empirical, 'Residual letters-patent rents on the kernel text under this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__revisable_translation_reading, 0, 144).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t0, kjv_text_1611__revisable_translation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(kjv__tr_t24, kjv_text_1611__revisable_translation_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement(kjv__tr_t48, kjv_text_1611__revisable_translation_reading, theater_ratio, 48, 0.16).
narrative_ontology:measurement(kjv__tr_t72, kjv_text_1611__revisable_translation_reading, theater_ratio, 72, 0.19).
narrative_ontology:measurement(kjv__tr_t96, kjv_text_1611__revisable_translation_reading, theater_ratio, 96, 0.25).
narrative_ontology:measurement(kjv__tr_t120, kjv_text_1611__revisable_translation_reading, theater_ratio, 120, 0.29).
narrative_ontology:measurement(kjv__tr_t144, kjv_text_1611__revisable_translation_reading, theater_ratio, 144, 0.32).

% Extraction over time
narrative_ontology:measurement(kjv__be_t0, kjv_text_1611__revisable_translation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(kjv__be_t24, kjv_text_1611__revisable_translation_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(kjv__be_t48, kjv_text_1611__revisable_translation_reading, base_extractiveness, 48, 0.44).
narrative_ontology:measurement(kjv__be_t72, kjv_text_1611__revisable_translation_reading, base_extractiveness, 72, 0.5).
narrative_ontology:measurement(kjv__be_t96, kjv_text_1611__revisable_translation_reading, base_extractiveness, 96, 0.57).
narrative_ontology:measurement(kjv__be_t120, kjv_text_1611__revisable_translation_reading, base_extractiveness, 120, 0.6).
narrative_ontology:measurement(kjv__be_t144, kjv_text_1611__revisable_translation_reading, base_extractiveness, 144, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t0, kjv_text_1611__revisable_translation_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(kjv__su_t24, kjv_text_1611__revisable_translation_reading, suppression_requirement, 24, 0.12).
narrative_ontology:measurement(kjv__su_t48, kjv_text_1611__revisable_translation_reading, suppression_requirement, 48, 0.15).
narrative_ontology:measurement(kjv__su_t72, kjv_text_1611__revisable_translation_reading, suppression_requirement, 72, 0.18).
narrative_ontology:measurement(kjv__su_t96, kjv_text_1611__revisable_translation_reading, suppression_requirement, 96, 0.21).
narrative_ontology:measurement(kjv__su_t120, kjv_text_1611__revisable_translation_reading, suppression_requirement, 120, 0.23).
narrative_ontology:measurement(kjv__su_t144, kjv_text_1611__revisable_translation_reading, suppression_requirement, 144, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__revisable_translation_reading, resource_allocation).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, exclusive_inspiration_reading).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, functional_equivalence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the KJV's authority' decomposes into three structurally distinct constraints corresponding to the three readings of kernel kjv_text_1611. This file is the revisable-translation member: moderate extraction concentrated in publishing, low suppression, scholars as arbiters. The exclusive-inspiration sibling carries high suppression and a much larger payer set; the functional-equivalence sibling carries extraction near coordination cost. Each member links the others here; epsilon values differ by construction and are never averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
