% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__entangled_event_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__entangled_event_reading, []).

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
 *   constraint_id: statute_of_anne_ip_foundation__entangled_event_reading
 *   human_readable: Statute of Anne as Undecomposable Founding Act (Entangled Event Reading)
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the statute_of_anne_ip_foundation
 *   kernel: the entangled_event_reading, which holds that the Statute of Anne
 *   was a single act in which conceptual and institutional change occurred
 *   simultaneously and cannot be disentangled — copyright 'became thinkable'
 *   and was 'first held' in the same motion. On this reading the standing
 *   arrangement under contest is the 1710 settlement's fused dual character:
 *   authorial rhetoric welded to publisher-operated machinery, so that the
 *   beneficiary is structurally ambiguous (authors nominal, booksellers
 *   practical) and the enduring casualty is determinacy about what the
 *   arrangement actually is. Constraint-family note (epsilon-invariance
 *   decomposition): the colloquial label 'what the Statute of Anne did'
 *   covers three structurally distinct claims, authored as three linked
 *   stories. The conceptual_emergence_reading authors a LOW epsilon — a new
 *   conceptual space delivered to the learning public, minimal extraction,
 *   the Stationers' old order as the displaced party. The
 *   institutional_reallocation_reading authors a zero-sum transfer structure
 *   — authors as beneficiaries receiving rights, the Stationers' Company as
 *   victims losing them. THIS reading authors epsilon ≈ 0.60 because the
 *   fusion itself is the extractive engine: neither sibling's epsilon applies
 *   here, because neither preserves the fusion, and the fusion is what
 *   generates the nominal/practical beneficiary gap and the indeterminacy
 *   cost borne by everyone downstream. The upstream siblings typically get
 *   cited as evidence FOR this reading's synthesis; this reading exerts
 *   downstream pressure back on each sibling to answer the fusion objection.
 *
 * KEY AGENTS:
 *   - - london_bookseller_publishers: Practical beneficiary and administrator (organized/constrained) — holds assigned terms, runs registration and enforcement, collects the rents
 *   - - statutory_term_authors: Nominal beneficiary, practical payer (moderate/constrained) — receives statutory credit; economic value flows onward to assignees
 *   - - stationers_company_hall_officers: Registry administrator (organized/identity_locked) — custodians of the entry system, constituted by custody of it
 *   - - provincial_scottish_printers: Enforcement target (moderate/constrained) — the reprint trade made actionable by machinery they cannot wield
 *   - - book_buying_public: Diffuse payer (powerless/constrained) — bears the price effects; no seat anywhere in the scheme
 *   - - parliament_legislators: Founding agenda-setter (institutional/mobile) — enacted the settlement and retains amendment power
 *   - - common_law_judges: Analytical observer (institutional/analytical) — resolves the arrangement's scope at Millar v Taylor and Donaldson v Becket
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, 0.6).
domain_priors:suppression_score(statute_of_anne_ip_foundation__entangled_event_reading, 0.5).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__entangled_event_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__entangled_event_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__entangled_event_reading, "Statute of Anne as Undecomposable Founding Act (Entangled Event Reading)").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__entangled_event_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__entangled_event_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__entangled_event_reading, 'c5a5f130-39c3-45ec-a004-311fd87808f9').
narrative_ontology:cs_kernel_codification('c5a5f130-39c3-45ec-a004-311fd87808f9', fixed_text).
narrative_ontology:cs_authority_grounding('c5a5f130-39c3-45ec-a004-311fd87808f9', lineage).
narrative_ontology:cs_interpretation_layer_present('c5a5f130-39c3-45ec-a004-311fd87808f9').
narrative_ontology:cs_reading_relation('c5a5f130-39c3-45ec-a004-311fd87808f9', statute_of_anne_ip_foundation__conceptual_emergence_reading, influences).
narrative_ontology:cs_reading_relation('c5a5f130-39c3-45ec-a004-311fd87808f9', statute_of_anne_ip_foundation__institutional_reallocation_reading, influences).
narrative_ontology:cs_axiom('c5a5f130-39c3-45ec-a004-311fd87808f9', foundational, conceptual_and_institutional_change_inseparable).
narrative_ontology:cs_axiom_status(conceptual_and_institutional_change_inseparable, holdable).
narrative_ontology:cs_axiom_grounding('c5a5f130-39c3-45ec-a004-311fd87808f9', conceptual_and_institutional_change_inseparable, empirically_contingent).
narrative_ontology:cs_axiom('c5a5f130-39c3-45ec-a004-311fd87808f9', secondary, nominal_author_credit_practical_publisher_capture).
narrative_ontology:cs_axiom_status(nominal_author_credit_practical_publisher_capture, holdable).
narrative_ontology:cs_axiom_grounding('c5a5f130-39c3-45ec-a004-311fd87808f9', nominal_author_credit_practical_publisher_capture, empirically_contingent).
narrative_ontology:cs_reference_frame('c5a5f130-39c3-45ec-a004-311fd87808f9', single_entangled_founding_act).
narrative_ontology:cs_drift_state('c5a5f130-39c3-45ec-a004-311fd87808f9', contemporary_specialized_historiography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c5a5f130-39c3-45ec-a004-311fd87808f9', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, london_bookseller_publishers).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, statutory_term_authors).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, provincial_scottish_printers).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, book_buying_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, statutory_term_authors).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__entangled_event_reading, concept_institution_coconstitution_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wholesale bookselling houses in London that bought up author copyrights by assignment, registered entries at Stationers' Hall, financed editions, and sued unauthorized reprinters. They shaped the statutory scheme's operational details through parliamentary contacts, collected the bulk of the exclusive-right rents, and treated the fourteen-year terms as assets to aggregate, renew, and defend in court. Leaving the trade would mean liquidating their principal capital: the backlist of assigned titles.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, london_bookseller_publishers, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__entangled_event_reading, london_bookseller_publishers, beneficiary).

% Writers who received a statutory first term of fourteen years in their works, renewable for a second term if living. In practice most sold their rights outright to booksellers before or shortly after publication, so the statutory credit to 'authors' in the preamble traveled economically to the assignees. A few prominent authors negotiated directly and profited; the average working writer's income changed little. Exit meant not publishing through the trade, which for most meant not publishing.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, statutory_term_authors, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__entangled_event_reading, statutory_term_authors, payer).

% The governing body of the Stationers' Company, whose Hall served as the statutory registry: every title's protection began with an entry in their books, for fees, under their procedures. The Company had kept a register for a century before 1710; administering the new statutory registry preserved its central place in the trade after the old licensing regime lapsed. The officers' standing was constituted by custody of the register — dissolving that role would dissolve the office itself.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, stationers_company_hall_officers, agenda_setter,
    organized, generational, identity_locked, national).

% Printers in Scotland and the English provinces whose trade included reprinting London titles for local markets. The statute's import and enforcement provisions, operated by London houses with litigation budgets, made their principal line of business actionable. Some shifted to original works or agency for London editions; others litigated or risked seizure. Their access to the registry's protections was nominal — enforcement ran through the London courts and the London assignees' purses.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, provincial_scottish_printers, payer,
    moderate, biographical, constrained, regional).

% Readers who bought books at prices set by a trade whose costs included the exclusive-right premiums. The statute's stated end was their 'Encouragement of Learning,' but no seat in the scheme represented them; the five deposit copies went to specified libraries, not to the market. Gray-market options — Irish and Scottish reprints moving south — existed unevenly and carried legal risk chiefly for sellers.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, book_buying_public, payer,
    powerless, biographical, constrained, national).

% The Commons and Lords who enacted the statute in 1709–1710 after the Licensing Act lapsed in 1695, leaving the book trade without statutory footing. They set the terms, the registry requirement, and the import rules, and retained power to amend or repeal. Individual members moved between sessions; the arrangement's day-to-day maintenance after enactment passed to the trade and the courts.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, parliament_legislators, agenda_setter,
    institutional, biographical, mobile, national).

% The bench that decided what the statute left undecided: whether authors held a perpetual common-law right in published works alongside the statutory term (Millar v Taylor, 1769; Donaldson v Becket, 1774). Their rulings determined whether the statutory bargain was the whole of literary property or merely its floor. They collected nothing from the arrangement; their seat is interpretive.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, common_law_judges, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__entangled_event_reading, london_bookseller_publishers).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__entangled_event_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gave the book trade a finite, registrable exclusive printing right that made edition investment safe against unauthorized reprinting, with centralized entry at Stationers' Hall creating a public record of title after the lapse of the Licensing Act had left the trade without statutory footing.
% TRANSFER_FUNCTION: Moved enforceable exclusive rights in texts — and the rents they carry — to registered proprietors, practically the London houses holding author assignments; financed by higher book prices borne by readers and by prohibitions enforced against provincial and foreign reprinters.
% ABSENT_VOICES: Leverageless working authors had no seat in the drafting — the scheme was negotiated among parliamentary patrons and trade interests, and the authorial voice heard was that of a few prominent men of letters. Provincial and foreign reprinters were absent by design, as their trade was the object of the prohibitions. Ordinary readers and the non-deposit libraries were absent without representation: the 'Encouragement of Learning' spoke in their name but no mechanism carried their interests into the registry, the pricing, or the enforcement decisions.
% DISAPPEARANCE_RATIONALE: Without the 1710 settlement, the trade reverts to house-by-house common-law litigation over perpetual claims — the very outcome the booksellers pursued at law after 1710 and briefly won in 1769 — or fragments into piracy wars between London, Edinburgh, and Dublin. The concept of a finite statutory copyright loses its anchor text; the Stationers' register loses its statutory function; the eighteenth-century book trade reorganizes around whatever equilibrium the courts or private ordering produce instead.
% FOUNDING_PROBLEM: Post-1695 trade disorder: the Licensing Act's lapse removed the censorship-era controls that had incidentally stabilized the London book trade, leaving investment in editions exposed to unauthorized reprinting and the old Stationer practice of perpetual entry without statutory warrant.
% FOUNDING_PROBLEM_CORROBORATION: The narrow founding problem — securing the trade against post-1695 disorder — was substantially solved within two decades of enactment, and the arrangement then persisted and expanded for unrelated reasons. Corroboration comes from outside the benefiting parties: parliamentary debates accompanying the 1735 Engravers' Act concede the trade-stabilization goal was met and argue instead from analogy; counsel for Donaldson in 1774 argued the statutory bargain had long since done its work; and the modern historiography (Feather, Rose, Deazley) attests from the archival record that the stabilization function was achieved early while the rights machinery grew. No beneficiary-party source disputes that the original disorder ended; the dispute is over why the arrangement continued.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__entangled_event_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__entangled_event_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__entangled_event_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__entangled_event_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__entangled_event_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__entangled_event_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statute_of_anne_ip_foundation__entangled_event_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.60 at interval end: the settlement delivered a workable publishing regime (real coordination) while welding authorial credit to publisher control, so the arrangement's gains accrued to assignee houses under an 'Encouragement of Learning' preamble — the gap between credit and capture is the extraction. Suppression 0.50, authored as a RAW STRUCTURAL PROPERTY (unscaled; the engine scales only extractiveness, by directionality and scope): enforcement ran through Hall registration, import prohibitions, and Chancery suits — real coercive machinery, but alternatives (original works, agency, gray-market reprints) never fully closed. Theater_ratio 0.35: the learning rhetoric performed increasing work as cover while the function shifted to trade protection, peaking during the perpetuity litigation and partially re-coupling after 1774. Accessibility_collapse 0.40: accepting the entanglement thesis does not collapse the alternatives — the two sibling readings remain live and published, which is precisely why resistance is 0.60. Temporal arc on ONE SHARED GRID (every tracked metric authored at all seven points, 1710–1774): extraction climbs with assignment consolidation and peaks at 0.72 in 1769, when Millar v Taylor let the booksellers invoke BOTH the statute (as floor) and common-law perpetuity (as ceiling) — maximal exploitation of the fusion; the 1774 House of Lords decision narrows the ambiguity, dropping extraction to 0.60 and suppression to 0.50 as the enforcement apparatus stands down. The 1769 peak and 1774 correction are a litigation-driven cycle, not intermittent reinforcement: the oscillation tracks judicial rulings, not a deliberate tension-release mechanism. Measurement values are retrospective historiographical reconstructions from the archival record (registration volumes, suit counts, price evidence), not contemporaneous instrument readings — see the provenance omega.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the london_bookseller_publishers seat the arrangement is coordination they built, administer, and defend — a registry, a term structure, an enforcement practice; their d sits near the beneficiary end and the arrangement presents as infrastructure. From the statutory_term_authors seat the same structure presents as nominal recognition riding on practical subordination — near-symmetric d, per the authored override. From the provincial_scottish_printers and book_buying_public seats it presents as enforced exclusion and priced access — d near the target end. The common_law_judges seat sees the whole structure and decides its scope without collecting from it. Inter-institutionally, Parliament (mobile exit — can amend) faces the trade (constrained — capital locked in backlists) and the Company (identity-locked — the office IS the registry custody); same-level laterally, London houses and provincial printers share a nominal trade but differ in registry access, litigation budget, and court proximity, which is what differentiates their exits despite comparable trade standing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: london_bookseller_publishers (declared beneficiary, administers enforcement) derive low d; provincial_scottish_printers and book_buying_public (declared victims, constrained exit) derive high d. One override is authored: statutory_term_authors are DECLARED beneficiaries (the statute grants them a real first term they previously lacked), so the structural derivation would place them deep at the beneficiary end — but the reading's core finding is the nominal/practical gap: assignment practice routed the economic value to the assignees, leaving most authors near-symmetric (d ≈ 0.55). The override corrects the derivation for the fusion effect this reading exists to capture. The Company officers derive low-to-moderate d (fee income, gatekeeping value) consistent with their administrator role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — post-1695 trade disorder — was dead within roughly two decades of enactment, while the arrangement persisted and expanded: the status(dead) x verdict(world_rearranges) mismatch flags the capture dynamic honestly rather than laundering it. Classification as tangled_rope prevents mislabeling in both directions: a pure-extraction label would erase the genuine piracy-coordination function that made edition investment safe (the rope half is real — the trade DID stabilize); a pure-coordination label would erase the nominal-author/practical-publisher capture gap and the indeterminacy cost imposed on every downstream interpreter (the extraction half is equally real). The reading does NOT decay toward piton: theater peaks at 0.47 but the 1774 correction partially re-couples rhetoric to function, and the arrangement's administrators demonstrably profit, which disqualifies the no-concentrated-beneficiary piton profile. Mandatrophy resolution here is the entanglement thesis applied to itself: the coordination and the capture cannot be scored separately without falsifying the arrangement — which is exactly what this reading claims about 1710.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_entanglement,
    'This constraint is one reading of the statute_of_anne_ip_foundation kernel (entangled_event_reading). What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative classification across the three linked reading-stories: conceptual_emergence_reading relocates the beneficiary to the learning public and the victim to the displaced Stationer order, with low epsilon; institutional_reallocation_reading makes authors the beneficiaries and the Stationers'' Company the victims in a zero-sum transfer. The disagreement is located at separability — whether the statute''s conceptual and institutional dimensions admit separate description at all.',
    'If a sibling reading is adopted as the account of record, this story''s beneficiary/victim structure and epsilon are replaced wholesale: the ambiguity that drives this reading''s extraction profile dissolves into either a clean conceptual gift or a clean rights transfer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_entanglement, conceptual, 'Committer structure: which kernel, which reading, what siblings would change, where the dispute sits.').

omega_variable(
    nominal_vs_practical_capture,
    'Did authors or assignee booksellers actually capture the statutory gains — how much of the authorial first term survived contact with assignment practice?',
    'Archival quantification: assignment records, probate inventories of authors and booksellers, trade accounts, and the assignment-price series for the 1710–1774 period (the Deazley/Rose/Feather evidentiary base, extended).',
    'If authors captured substantively, the directionality override for statutory_term_authors collapses toward the derived beneficiary value and this reading drifts toward the institutional_reallocation sibling; if capture was near-total, the nominal credit is confirmed as cover and extraction sits at the high end of the authored band.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nominal_vs_practical_capture, empirical, 'Whether the nominal/practical beneficiary gap is a routing artifact or a structural feature.').

omega_variable(
    inseparability_fact_or_method,
    'Is the inseparability of the statute''s conceptual and institutional dimensions a fact about 1710, or a methodological commitment of this reading?',
    'Counterfactual and contemporaneous-analysis test: could Parliament have enacted the limited-term concept without the registry-and-enforcement machinery (or vice versa)? Did contemporaries distinguish the dimensions in petitions, drafts, and parliamentary debate?',
    'If the dimensions are separable in principle, this reading collapses toward whichever sibling carries the dominant dimension and epsilon splits into two stories per the epsilon-invariance rule; if inseparable, the sibling readings are each incomplete accounts and this story''s fused structure stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inseparability_fact_or_method, conceptual, 'Whether the reading''s core premise is an empirical finding or a framing choice.').

omega_variable(
    epistemic_victim_actor_mapping,
    'The reading''s structural delta names ''conceptual clarity'' as the victim — an abstract good, not an actor. Which real seats bear that harm, and should they register in the victim structure?',
    'Trace concrete bearers of the indeterminacy cost: doctrinal analysts denied a determinate origin, courts denied clean guidance in Millar/Donaldson-era argument, and later reform movements unable to cite 1710 as unambiguous precedent for either property or regulation.',
    'If a concrete bearer seat is identified and materially burdened, it belongs in victims[] and shifts derived directionality; if the cost stays irreducibly distributed, it remains commentary-grade and the current two-victim structure stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_victim_actor_mapping, conceptual, 'How an epistemic harm maps onto actor seats for directionality purposes.').

omega_variable(
    retrospective_measurement_provenance,
    'The temporal series are retrospective historiographical reconstructions, not contemporaneous instrument readings — how robust are the trajectory shapes (rise to 1769, correction at 1774) to archival revision?',
    'Systematic quantification of registration volumes at Stationers'' Hall, Chancery and common-law suit counts, and book-price series across the interval; sensitivity analysis of the trajectory endpoints to alternative codings of the litigation period.',
    'Trajectory shapes could flatten or steepen; the 1769 peak and 1774 correction are the load-bearing features for the cycle reading and the post-correction recoupling claim, so material revision there would alter the drift assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retrospective_measurement_provenance, empirical, 'Robustness of the authored temporal series to archival revision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__entangled_event_reading, 1710, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1710, 0.22).
narrative_ontology:measurement_basis(stat_tr_t1710, observed).
narrative_ontology:measurement(stat_tr_t1725, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1725, 0.28).
narrative_ontology:measurement_basis(stat_tr_t1725, observed).
narrative_ontology:measurement(stat_tr_t1740, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1740, 0.34).
narrative_ontology:measurement_basis(stat_tr_t1740, observed).
narrative_ontology:measurement(stat_tr_t1753, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1753, 0.4).
narrative_ontology:measurement_basis(stat_tr_t1753, observed).
narrative_ontology:measurement(stat_tr_t1760, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1760, 0.44).
narrative_ontology:measurement_basis(stat_tr_t1760, observed).
narrative_ontology:measurement(stat_tr_t1769, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1769, 0.47).
narrative_ontology:measurement_basis(stat_tr_t1769, observed).
narrative_ontology:measurement(stat_tr_t1774, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1774, 0.35).
narrative_ontology:measurement_basis(stat_tr_t1774, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1710, 0.52).
narrative_ontology:measurement_basis(stat_be_t1710, observed).
narrative_ontology:measurement(stat_be_t1725, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1725, 0.58).
narrative_ontology:measurement_basis(stat_be_t1725, observed).
narrative_ontology:measurement(stat_be_t1740, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1740, 0.63).
narrative_ontology:measurement_basis(stat_be_t1740, observed).
narrative_ontology:measurement(stat_be_t1753, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1753, 0.66).
narrative_ontology:measurement_basis(stat_be_t1753, observed).
narrative_ontology:measurement(stat_be_t1760, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1760, 0.68).
narrative_ontology:measurement_basis(stat_be_t1760, observed).
narrative_ontology:measurement(stat_be_t1769, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1769, 0.72).
narrative_ontology:measurement_basis(stat_be_t1769, observed).
narrative_ontology:measurement(stat_be_t1774, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1774, 0.6).
narrative_ontology:measurement_basis(stat_be_t1774, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1710, 0.38).
narrative_ontology:measurement_basis(stat_su_t1710, observed).
narrative_ontology:measurement(stat_su_t1725, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1725, 0.46).
narrative_ontology:measurement_basis(stat_su_t1725, observed).
narrative_ontology:measurement(stat_su_t1740, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1740, 0.54).
narrative_ontology:measurement_basis(stat_su_t1740, observed).
narrative_ontology:measurement(stat_su_t1753, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1753, 0.62).
narrative_ontology:measurement_basis(stat_su_t1753, observed).
narrative_ontology:measurement(stat_su_t1760, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1760, 0.66).
narrative_ontology:measurement_basis(stat_su_t1760, observed).
narrative_ontology:measurement(stat_su_t1769, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1769, 0.7).
narrative_ontology:measurement_basis(stat_su_t1769, observed).
narrative_ontology:measurement(stat_su_t1774, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1774, 0.5).
narrative_ontology:measurement_basis(stat_su_t1774, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__entangled_event_reading, resource_allocation).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, institutional_reallocation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what the Statute of Anne did' decomposes per the epsilon-invariance principle into three linked stories — conceptual_emergence_reading (low epsilon; concept as regulatory gift), institutional_reallocation_reading (zero-sum transfer; authors gain, Stationers lose), and this entangled_event_reading (fused act; epsilon ≈ 0.60 driven by the nominal/practical beneficiary gap and the indeterminacy cost). The upstream siblings are typically cited as evidence for this reading's synthesis; this story links back to both because adopting the fusion account exerts downstream pressure on each sibling to answer the fusion objection. Each story carries its own epsilon, beneficiaries, victims, and classification; none averages across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statute_of_anne_ip_foundation__entangled_event_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
